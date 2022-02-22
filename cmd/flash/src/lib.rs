// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility flash`
//!
//! Flashes the target with the image that is contained within the specified
//! archive (or dump).  This merely executes the underlying flashing
//! mechanism (either PyOCD or OpenOCD, depending on the target); if the
//! requisite software is not installed (or isn't in the path), this will
//! fail.  Temporary files are created as part of this process; if they are to
//! be retained, the `-R` (`--retain-temporaries`) flag should be set.
//! To see what would be executed without actually executing any commands,
//! use the `-n` (`--dry-run`) flag.  As a precautionary measure, if
//! the specified archive already appears to be on the target, `humility
//! flash` will fail unless the `-F` (`--force`) flag is set.
//!

use anyhow::{bail, Context, Result};
use clap::App;
use clap::IntoApp;
use clap::Parser;
use humility::hubris::*;
use humility_cmd::{Archive, Args, Command};
use path_slash::PathExt;
use std::io::Write;

use serde::Deserialize;

#[derive(Parser, Debug)]
#[clap(name = "flash", about = env!("CARGO_PKG_DESCRIPTION"))]
struct FlashArgs {
    /// force re-flashing if archive matches
    #[clap(long, short = 'F')]
    force: bool,

    /// do not actually flash, but show commands and retain any temporary files
    #[clap(long = "dry-run", short = 'n')]
    dryrun: bool,

    /// retain any temporary files
    #[clap(long = "retain-temporaries", short = 'R')]
    retain: bool,
}

//
// This is the Hubris definition
//
#[derive(Debug, Deserialize)]
enum FlashProgram {
    PyOcd(Vec<FlashArgument>),
    OpenOcd(FlashProgramConfig),
}

#[derive(Debug, Deserialize)]
enum FlashProgramConfig {
    Path(Vec<String>),
    Payload(String),
}

#[derive(Debug, Deserialize)]
enum FlashArgument {
    Direct(String),
    Payload,
    FormattedPayload(String, String),
    Config,
}

#[derive(Debug, Deserialize)]
struct FlashConfig {
    program: FlashProgram,
    args: Vec<FlashArgument>,
}

fn flashcmd(
    hubris: &mut HubrisArchive,
    args: &Args,
    subargs: &[String],
) -> Result<()> {
    let flash_config = hubris.load_flash_config()?;
    let subargs = FlashArgs::try_parse_from(subargs)?;

    let config: FlashConfig = ron::from_str(&flash_config.metadata)?;

    //
    // We need to attach to (1) confirm that we're plugged into something
    // and (2) extract serial information.
    //
    let probe = match &args.probe {
        Some(p) => p,
        None => "auto",
    };

    let serial = {
        let mut c = humility::core::attach(probe, &args.chip)?;
        let core = c.as_mut();

        //
        // We want to actually try validating to determine if this archive
        // already matches; if it does, this command may well be in error,
        // and we want to force the user to force their intent.
        //
        if hubris.validate(core, HubrisValidate::ArchiveMatch).is_ok() {
            if subargs.force {
                humility::msg!(
                    "archive appears to be already flashed; forcing re-flash"
                );
            } else {
                bail!("archive appears to be already flashed on attached device; \
                    use -F (\"--force\") to force re-flash");
            }
        }

        core.info().1
    };

    let dryrun = |cmd: &std::process::Command| {
        let args: Vec<_> =
            cmd.get_args().map(|o| o.to_string_lossy()).collect();
        humility::msg!(
            "would execute: \"{} {}\"",
            cmd.get_program().to_string_lossy(),
            args.join(" ")
        );
    };

    match config.program {
        FlashProgram::OpenOcd(payload) => {
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
                writeln!(conf, "adapter serial {}", serial)?;
            }

            if let FlashProgramConfig::Payload(ref payload) = payload {
                write!(conf, "{}", payload)?;
            } else {
                bail!("unexpected OpenOCD payload: {:?}", payload);
            }

            std::fs::write(&srec, flash_config.srec)?;

            //
            // OpenOCD only deals with slash paths, not native paths
            // (regardless of platform), so we turn our paths into slash
            // paths.
            //
            let conf_path = conf.path().to_slash_lossy();
            let srec_path = srec.path().to_slash_lossy();

            if subargs.retain {
                humility::msg!("retaining OpenOCD config as {:?}", conf.path());
                humility::msg!("retaining srec as {}", srec_path);
                conf.keep()?;
                srec.keep()?;
            }

            for arg in config.args {
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

            let status = flash
                .status()
                .with_context(|| format!("failed to flash ({:?})", flash))?;

            if !status.success() {
                anyhow::bail!("flash command ({:?}) failed; see output", flash);
            }
        }

        FlashProgram::PyOcd(ref reset_args) => {
            let mut flash = std::process::Command::new("pyocd");
            let mut reset = std::process::Command::new("pyocd");

            let ihex = tempfile::NamedTempFile::new()?;
            std::fs::write(&ihex, flash_config.ihex)?;
            let ihex_path = ihex.path();

            for arg in config.args {
                match arg {
                    FlashArgument::Direct(ref val) => {
                        flash.arg(val);
                    }
                    FlashArgument::Payload => {
                        flash.arg(ihex_path);
                    }
                    _ => {
                        anyhow::bail!("unexpected PyOCD argument {:?}", arg);
                    }
                }
            }

            for arg in reset_args {
                if let FlashArgument::Direct(ref val) = arg {
                    reset.arg(val);
                } else {
                    anyhow::bail!("unexpected PyOCD reset argument {:?}", arg);
                }
            }

            if let Some(serial) = serial {
                humility::msg!("specifying serial {}", serial);
                flash.arg("-u");
                flash.arg(&serial);
                reset.arg("-u");
                reset.arg(&serial);
            }

            if subargs.retain || subargs.dryrun {
                humility::msg!("retaining ihex as {}", ihex_path.display());
                ihex.keep()?;
            }

            if subargs.dryrun {
                dryrun(&flash);
                dryrun(&reset);

                return Ok(());
            }

            let status = flash
                .status()
                .with_context(|| format!("failed to flash ({:?})", flash))?;

            if !status.success() {
                anyhow::bail!("flash command ({:?}) failed; see output", flash);
            }

            let status = reset
                .status()
                .with_context(|| format!("failed to reset ({:?})", reset))?;

            if !status.success() {
                anyhow::bail!("reset command ({:?}) failed; see output", reset);
            }
        }
    };

    Ok(())
}

pub fn init() -> (Command, App<'static>) {
    (
        Command::Unattached {
            name: "flash",
            archive: Archive::Required,
            run: flashcmd,
        },
        FlashArgs::into_app(),
    )
}
