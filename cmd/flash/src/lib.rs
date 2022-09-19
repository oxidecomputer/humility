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
//! This attempts to natively flash the part within Humility using probe-rs,
//! but for some parts or configurations, it may need to use OpenOCD as a
//! child process to flash it.  If OpenOCD is required but not installed (or
//! isn't in the path), this will fail.  If OpenOCD is used, temporary files
//! are created as part of this process; if they are to be retained, the `-R`
//! (`--retain-temporaries`) flag should be set.  To see what would be
//! executed without actually executing any commands, use the `-n`
//! (`--dry-run`) flag.  Should use of OpenOCD need to be forced (that is,
//! should probe-rs flashing fail), the `-O` (`--force-openocd`) flag can be
//! used.  That said, OpenOCD should generally be discouraged; the disposition
//! is to extend probe-rs to support any parts that must be flashed via
//! OpenOCD.
//!
//! If the specified archive includes auxiliary flash data and the new image
//! includes a task with the `AuxFlash` API, two slots of auxiliary flash
//! will be programmed after the image is written.  See RFD 311 for more
//! information about auxiliary flash management.

use anyhow::{bail, Context, Result};
use clap::{CommandFactory, Parser};
use humility::cli::{Cli, Subcommand};
use humility::{core::Core, hubris::*};
use humility_cmd::{Archive, Command, CommandKind};
use path_slash::PathExt;
use std::io::Write;
use std::process::ExitStatus;

#[derive(Parser, Debug)]
#[clap(name = "flash", about = env!("CARGO_PKG_DESCRIPTION"))]
struct FlashArgs {
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
        parse(try_from_str = parse_int::parse)
    )]
    reset_delay: u64,

    /// if archive appears to already be flashed, verify contents
    #[clap(long, short = 'V', conflicts_with = "force")]
    verify: bool,

    /// do not flash, just check if archive has been flashed
    #[clap(long, short = 'C', conflicts_with_all = &["force", "verify"])]
    check: bool,
}

fn force_openocd(
    hubris: &mut HubrisArchive,
    args: &Cli,
    subargs: &FlashArgs,
    config: &HubrisFlashMeta,
    elf: &[u8],
) -> Result<()> {
    // Images that include auxiliary flash data *must* be programmed through
    // probe-rs, because we use the resulting ProbeCore to program the
    // auxiliary flash (via hiffy)
    if hubris.read_auxflash_data()?.is_some() {
        bail!("cannot program an image with auxiliary flash through OpenOCD");
    }

    //
    // We need to attach to (1) confirm that we're plugged into something
    // and (2) extract serial information.
    //
    let probe = match &args.probe {
        Some(p) => p,
        None => "auto",
    };

    let serial = {
        let mut c = humility::core::attach(probe, hubris)?;
        let core = c.as_mut();

        validate(hubris, core, subargs)?;

        if subargs.check {
            return Ok(());
        }

        core.run()?;
        core.info().1
    };

    let dryrun = |cmd: &std::process::Command| {
        humility::msg!("would execute: {:?}", cmd);
    };

    let payload = match &config.program {
        FlashProgram::OpenOcd(payload) => payload,
        _ => {
            bail!(
                "cannot force OpenOCD for non-OpenOCD \
                flash configuration: {:?}",
                config.program
            );
        }
    };

    let mut flash = std::process::Command::new("openocd");

    //
    // We need to create a temporary file to hold our OpenOCD
    // configuration file and the SREC file that we're going to
    // actually program.
    //
    let mut conf = tempfile::NamedTempFile::new()?;
    let srec = tempfile::NamedTempFile::new()?;

    if let Some(serial) = serial {
        humility::msg!("specifying serial {}", serial);

        //
        // In OpenOCD 0.11 dev, hla_serial has been deprecated, and
        // using it results in this warning:
        //
        //   DEPRECATED! use 'adapter serial' not 'hla_serial'
        //
        // Unfortunately, the newer variant ("adapter serial") does
        // not exist prior to this interface being deprecated; in
        // order to allow execution on older OpenOCD variants, we
        // deliberately use the deprecated interface.  (And yes, it
        // would probably be convenient if OpenOCD just made the old
        // thing work instead of shouting about it and then doing it
        // anyway.)
        //
        writeln!(conf, "interface hla\nhla_serial {}", serial)?;
    }

    if let FlashProgramConfig::Payload(ref payload) = payload {
        write!(conf, "{}", payload)?;
    } else {
        bail!("unexpected OpenOCD payload: {:?}", payload);
    }

    std::fs::write(&srec, generate_srec_from_elf(elf)?)?;

    //
    // OpenOCD only deals with slash paths, not native paths
    // (regardless of platform), so we turn our paths into slash
    // paths.
    //
    let conf_path = conf.path().to_slash_lossy();
    let srec_path = srec.path().to_slash_lossy();

    if subargs.retain || subargs.dryrun {
        humility::msg!("retaining OpenOCD config as {:?}", conf.path());
        humility::msg!("retaining srec as {}", srec_path);
        conf.keep()?;
        srec.keep()?;
    }

    for arg in &config.args {
        match arg {
            FlashArgument::Direct(ref val) => {
                flash.arg(val);
            }
            FlashArgument::FormattedPayload(ref pre, ref post) => {
                flash.arg(format!("{} {} {}", pre, srec_path, post));
            }
            FlashArgument::Config => {
                flash.arg(&conf_path);
            }
            _ => {
                anyhow::bail!("unexpected OpenOCD argument {:?}", arg);
            }
        }
    }

    if subargs.dryrun {
        dryrun(&flash);
        return Ok(());
    }

    let status = nice_status(&mut flash)?;

    if !status.success() {
        anyhow::bail!("flash command ({:?}) failed; see output", flash);
    }

    Ok(())
}

fn validate(
    hubris: &mut HubrisArchive,
    core: &mut dyn humility::core::Core,
    subargs: &FlashArgs,
) -> Result<()> {
    core.halt()?;

    //
    // We want to actually try validating to determine if this archive
    // already matches; if it does, this command may well be in error,
    // and we want to force the user to force their intent.
    //
    match hubris.validate(core, HubrisValidate::ArchiveMatch) {
        Ok(_) => {
            if subargs.force {
                humility::msg!(
                    "archive appears to be already flashed; forcing re-flash"
                );
            } else if subargs.verify || subargs.check {
                if let Err(err) = hubris.verify(core) {
                    if subargs.check {
                        core.run()?;
                        bail!(
                            "image IDs match, but flash contents do not match \
                            archive contents: {}",
                            err
                        );
                    }

                    humility::msg!(
                        "image IDs match, but flash contents do not match \
                        archive contents: {}; reflashing",
                        err
                    );
                } else {
                    core.run()?;

                    if subargs.check {
                        humility::msg!("archive matches flash contents");
                        return Ok(());
                    }

                    bail!(
                        "archive is already flashed on attached device; \
                        use -F (\"--force\") to force re-flash"
                    );
                }
            } else {
                core.run()?;
                bail!(
                    "archive appears to be already flashed on attached \
                    device; use -F (\"--force\") to force re-flash or \
                    -V (\"--verify\") to verify contents"
                );
            }
        }
        Err(err) => {
            if subargs.check {
                core.run()?;
                bail!("flash/archive mismatch: {}", err);
            }
        }
    }

    Ok(())
}

fn flashcmd(context: &mut humility::ExecutionContext) -> Result<()> {
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let hubris = context.archive.as_mut().unwrap();
    let subargs = FlashArgs::try_parse_from(subargs)?;

    let config = hubris.load_flash_config()?;

    if subargs.force_openocd {
        humility::msg!("forcing flashing using OpenOCD");
        return force_openocd(
            hubris,
            &context.cli,
            &subargs,
            &config.metadata,
            &config.elf,
        );
    }

    let probe = match &context.cli.probe {
        Some(p) => p,
        None => "auto",
    };

    let chip = match config.chip {
        Some(c) => c,
        None => {
            return force_openocd(
                hubris,
                &context.cli,
                &subargs,
                &config.metadata,
                &config.elf,
            )
        }
    };

    humility::msg!("attaching with chip set to {:x?}", chip);
    let mut c = humility::core::attach_for_flashing(probe, hubris, &chip)?;
    let core = c.as_mut();

    validate(hubris, core, &subargs)?;

    if subargs.check {
        return Ok(());
    }

    let ihex = tempfile::NamedTempFile::new()?;
    std::fs::write(&ihex, generate_ihex_from_elf(&config.elf)?)?;
    let ihex_path = ihex.path();

    //
    // Load the flash image, and reset the part if that works.
    //
    if let Err(err) = core.load(ihex_path) {
        core.run()?;
        return Err(err);
    }

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

    core.reset()?;

    // At this point, we can attempt to program the auxiliary flash.  This has
    // to happen *after* the image is flashed and the core is reset, because it
    // uses hiffy calls to the `auxflash` task to actually do the programming;
    // because we have no knowledge of the archive previously flashed onto the
    // chip, we couldn't do hiffy calls before flashing.
    //
    // This is called out in RFD 311 as a weakness of our approach!
    try_program_auxflash(hubris, core)?;
    humility::msg!("flashing done");
    Ok(())
}

fn try_program_auxflash(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
) -> Result<()> {
    match hubris.read_auxflash_data()? {
        Some(auxflash) => match program_auxflash(hubris, core, &auxflash) {
            Ok(_) => {
                humility::msg!("done with auxiliary flash");
                Ok(())
            }
            Err(e) => bail!(
                "failed to program auxflash: {:?}; \
                 your system may not be functional!",
                e
            ),
        },
        None => Ok(()),
    }
}

fn program_auxflash(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    data: &[u8],
) -> Result<()> {
    let mut worker = cmd_auxflash::AuxFlashHandler::new(hubris, core, 15_000)?;

    // At this point, we've already rebooted into the new image.
    //
    // If the Hubris auxflash task has picked up an active slot, then we're all
    // set: our target image was already loaded (or was unchanged).
    match worker.active_slot() {
        Ok(Some(i)) => {
            humility::msg!(
                "auxiliary flash data is already loaded in slot {}; \
                 skipping programming",
                i
            );
            return Ok(());
        }
        Ok(None) => (),
        Err(e) => {
            humility::msg!("Got error while checking active slot: {:?}", e);
        }
    };

    // Otherwise, we need to pick a slot.  This is tricky, because we don't
    // actually know whether there's an image on the B partition that's using
    // auxiliary data.  We'll prioritize picking an empty (even) slot, and will
    // otherwise pick slot 0 arbitrarily.
    let slot_count = worker.slot_count()?;
    let mut target_slot = 0;
    for i in 0..slot_count {
        if i % 2 == 0 && matches!(worker.slot_status(i), Ok(None)) {
            target_slot = i;
            break;
        }
    }

    worker.auxflash_write(target_slot, data, false)?;

    // After a reset, two things will happen:
    // - The SP `auxflash` task will automatically mirror from the even slot to
    //   the odd slot, since the even slot will have valid data and the odd slot
    //   will not.
    // - The SP will recognize the programmed slot as valid and choose it as the
    //   active slot.
    worker.reset()?;

    // Give the SP plenty of time to do its mirroring operation
    humility::msg!("resetting the SP, please wait...");
    std::thread::sleep(std::time::Duration::from_secs(5));

    match worker.active_slot() {
        Ok(Some(..)) => Ok(()),
        Ok(None) => bail!("No active auxflash slot, even after programming"),
        Err(e) => {
            bail!("Could not check auxflash slot after programming: {:?}", e)
        }
    }
}

/// Executes `command` for its exit status, so that stdout/stderr are shared
/// with this process. This is equivalent to calling `Command::status` except
/// that, if the command fails to even _start,_ we provide a more detailed error
/// message than the default "no such file or directory."
fn nice_status(command: &mut std::process::Command) -> Result<ExitStatus> {
    command.status().with_context(|| {
        format!(
            "unable to execute {:?}, is it in your PATH and executable?",
            command.get_program(),
        )
    })
}

pub fn init() -> Command {
    Command {
        app: FlashArgs::command(),
        name: "flash",
        run: flashcmd,
        kind: CommandKind::Unattached { archive: Archive::Required },
    }
}

/// While it may sound like the impetus for an OSHA investigation at the North
/// Pole, this function is _actually_ designed to generate small (32-byte)
/// chunks describing the data in the PHDRs of an ELF file. Unless the file is
/// missing PHDRs, because objcopy sometimes does that for whatever reason, in
/// which case we do the section headers.
///
/// This is an implementation factor of both SREC and IHEX generation.
fn elf_chunks(elf_data: &[u8]) -> Result<Vec<(u32, &[u8])>> {
    let elf = goblin::elf::Elf::parse(elf_data)?;

    let mut addr_slices = vec![];

    if elf.program_headers.is_empty() {
        for sh in &elf.section_headers {
            if sh.sh_type != goblin::elf::section_header::SHT_PROGBITS {
                continue;
            }

            let addr = u32::try_from(sh.sh_addr)?;
            let offset = usize::try_from(sh.sh_offset)?;
            let size = usize::try_from(sh.sh_size)?;

            for (i, chunk) in
                elf_data[offset..offset + size].chunks(32).enumerate()
            {
                addr_slices.push((addr + i as u32 * 32, chunk));
            }
        }
    } else {
        for ph in &elf.program_headers {
            if ph.p_type != goblin::elf::program_header::PT_LOAD {
                continue;
            }

            let addr = u32::try_from(ph.p_vaddr)?;
            let offset = usize::try_from(ph.p_offset)?;
            let size = usize::try_from(ph.p_filesz)?;

            for (i, chunk) in
                elf_data[offset..offset + size].chunks(32).enumerate()
            {
                addr_slices.push((addr + i as u32 * 32, chunk));
            }
        }
    }

    Ok(addr_slices)
}

fn generate_srec_from_elf(data: &[u8]) -> Result<String> {
    let mut records = vec![srec::Record::S0("humility!".into())];

    for (addr, slice) in elf_chunks(data)? {
        records.push(srec::Record::S3(srec::Data {
            address: srec::Address32(addr),
            data: slice.to_vec(),
        }));
    }
    records.push(srec::Record::S7(srec::Address32(0))); // bogus entry point

    Ok(srec::writer::generate_srec_file(&records))
}

fn generate_ihex_from_elf(data: &[u8]) -> Result<String> {
    // Build up IHEX records from that information.
    let mut records = vec![];

    for (addr, slice) in elf_chunks(data)? {
        records.push(ihex::Record::ExtendedLinearAddress((addr >> 16) as u16));
        records.push(ihex::Record::Data {
            offset: addr as u16,
            value: slice.to_vec(),
        });
    }

    records.push(ihex::Record::EndOfFile);

    Ok(ihex::create_object_file_representation(&records)?)
}
