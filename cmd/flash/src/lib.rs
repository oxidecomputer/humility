// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility flash`
//!
//! Flashes the target with the image that is contained within the specified
//! archive (or dump).  As a precautionary measure, if the specified archive
//! already appears to be on the target, `humility flash` will fail unless the
//! `-F` (`--force`) flag is set.  Because this will only check the image
//! ID (and not the entire image), `humility flash` can be optionally told
//! to verify that all of the program text in the image is on the device
//! by specifying `-V` (`--verify`).  Similarly, if one wishes to *only*
//! check the image against the archive (and not flash at all), specify
//! `-C` (`--check`).
//!
//! If the specified archive includes auxiliary flash data and the new image
//! includes a task with the `AuxFlash` API, two slots of auxiliary flash
//! will be programmed after the image is written.  See RFD 311 for more
//! information about auxiliary flash management.

use anyhow::{Context, Result, anyhow, bail};
use clap::Parser;
use humility::{
    core::Core,
    hubris::*,
    log::{Logger, info},
};
use humility_cli::{ExecutionContext, humility_cmd};
use humility_flash::{get_image_state, program_auxflash};

#[derive(Parser, Debug)]
#[clap(name = "flash", about = env!("CARGO_PKG_DESCRIPTION"))]
pub struct FlashArgs {
    /// force re-flashing if archive matches
    #[clap(long, short = 'F')]
    force: bool,

    /// if using OpenOCD, do not actually flash, but show commands and retain
    /// any temporary files
    #[clap(long = "dry-run", short = 'n')]
    dryrun: bool,

    /// retain any temporary files
    #[clap(long = "retain-temporaries", short = 'R')]
    retain: bool,

    /// force usage of OpenOCD
    #[clap(long = "force-openocd", short = 'O')]
    force_openocd: bool,

    /// reset delay
    #[clap(
        long = "reset-delay", short = 'd',
        default_value_t = 100, value_name = "timeout_ms",
        value_parser = parse_int::parse::<u64>
    )]
    reset_delay: u64,

    /// if archive appears to already be flashed, verify contents
    #[clap(long, short = 'V', conflicts_with = "force")]
    verify: bool,

    /// do not flash, just check if archive has been flashed
    #[clap(long, short = 'C', conflicts_with_all = &["force", "verify"])]
    check: bool,

    /// print every mismatched byte when checking / verifying
    #[clap(long)]
    verbose: bool,
}

/// Validates the image and auxiliary flash against our subcommand
///
/// If `subargs.check` is true, returns `Ok(())` on a clean check and `Err(..)`
/// otherwise; the core is left running.
///
/// If `subargs.check` is false, returns `Ok(())` if the check _fails_ (meaning
/// we should reflash), and `Err(..)` if all checks pass (meaning we should
/// _not_ reflash).  The core is left halted if we should reflash and is running
/// otherwise.
fn validate(
    hubris: &HubrisArchive,
    core: &mut humility_probes_core::ProbeCore,
    subargs: &FlashArgs,
    log: &Logger,
) -> Result<()> {
    let check_type = if subargs.verify || subargs.check {
        humility_flash::ImageCheckType::ImageIdAndFlash {
            check_every_byte: subargs.verbose,
        }
    } else {
        humility_flash::ImageCheckType::ImageId
    };
    // Collapse both Humility-level errors and mismatch errors into `Err(..)`
    let r = match get_image_state(hubris, core, check_type, log) {
        Ok(humility_flash::ImageStateResult::Matches) => Ok(()),
        Ok(humility_flash::ImageStateResult::DoesNotMatch(e)) => Err(e.into()),
        Err(e) => Err(e.into()),
    };
    if subargs.check {
        core.run()?;
        return r;
    }

    match r {
        Ok(()) => {
            if subargs.force {
                info!(
                    log,
                    "archive appears to be already flashed; forcing re-flash"
                );
                Ok(())
            } else {
                core.run()?;
                Err(anyhow!(if subargs.verify {
                    "archive is already flashed on attached device; use -F \
                     (\"--force\") to force re-flash"
                } else {
                    "archive appears to be already flashed on attached \
                     device; use -F (\"--force\") to force re-flash or -V \
                     (\"--verify\") to verify contents"
                }))
            }
        }
        Err(e) => {
            info!(log, "{e}; reflashing");
            Ok(())
        }
    }
}

fn flashcmd(subargs: FlashArgs, context: &mut ExecutionContext) -> Result<()> {
    let hubris = &context.cli.archive()?;
    let log = context.log();

    let config = hubris.load_flash_config()?;

    if subargs.force_openocd {
        bail!("openocd no longer supported");
    }

    let probe = match &context.cli.probe {
        Some(p) => p,
        None => "auto",
    };

    let chip = match config.chip {
        Some(c) => c,
        None => bail!("Archive is very old and missing a chip"),
    };

    info!(log, "attaching with chip set to {chip:x?}");
    let core = &mut humility_probes_core::attach_for_flashing(
        probe,
        &chip,
        context.cli.speed,
        log,
    )?;

    validate(hubris, core, &subargs, log)?;
    if subargs.check {
        return Ok(());
    }

    //
    // Load the flash image.  If that fails, we're in a world of hurt:  we
    // really don't want to run the core for fear of masking the initial
    // error.  (It will hopefully be pretty clear to the user that a
    // half-flashed part is going to be in an ill-defined state!)
    //
    core.load(&config.elf)?;

    //
    // On Gimlet Rev B, the BOOT0 pin is unstrapped -- and during a flash,
    // it seems to float high enough to bounce the part onto the wrong
    // image (that is, the BOOT1 image -- which by default is the ST
    // bootloader).  This seems to only be true when resetting immediately
    // after flashing the part:  if there is a delay on the order of ~35
    // milliseconds or more, the BOOT0 pin is seen as low when the part
    // resets.  Because this delay is (more or less) harmless, we do it on
    // all platforms, and further make it tunable.
    //
    let delay = subargs.reset_delay;

    if delay != 0 {
        std::thread::sleep(std::time::Duration::from_millis(delay));
    }

    // Reset, using the handoff token if present in the archive
    core.reset_with_handoff(hubris, log)?;

    // At this point, we can attempt to program the auxiliary flash.  This has
    // to happen *after* the image is flashed and the core is reset, because it
    // uses hiffy calls to the `auxflash` task to actually do the programming;
    // because we have no knowledge of the archive previously flashed onto the
    // chip, we couldn't do hiffy calls before flashing.
    //
    // This is called out in RFD 311 as a weakness of our approach!
    try_program_auxflash(hubris, core, log)?;
    info!(log, "flashing done");
    Ok(())
}

fn try_program_auxflash(
    hubris: &HubrisArchive,
    core: &mut humility_probes_core::ProbeCore,
    log: &Logger,
) -> Result<()> {
    use humility_flash::ProgramAuxflashSuccess;
    match hubris.read_auxflash_data()? {
        Some(auxflash) => match program_auxflash(hubris, core, &auxflash, log) {
            Ok(r) => {
                match r {
                    ProgramAuxflashSuccess::AlreadyProgrammed { slot } => {
                        info!(
                            log,
                            "auxiliary flash data is already loaded in slot \
                             {slot}; skipping programming",
                        )
                    }
                    ProgramAuxflashSuccess::Success => {
                        info!(log, "successfully programmed auxiliary flash")
                    }
                }
                Ok(())
            }
            Err(e) => Err(e).context(
                "failed to program auxflash; your system may not be functional",
            ),
        },
        None => Ok(()),
    }
}

humility_cmd!(FlashArgs, flashcmd);
