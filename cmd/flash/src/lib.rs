// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility flash`
//!
//! Flashes the target with the image that is contained within the specified
//! archive (or dump).  This merely executes the underlying flashing
//! mechanism (either pyOCD or OpenOCD, depending on the target); if the
//! requisite software is not installed (or isn't in the path), this will
//! fail.  Temporary files are created as part of this process; if they are to
//! be retained, the `-R` (`--retain-temporaries`) flag should be set.
//! To see what would be executed without actually executing any commands,
//! use the `-n` (`--dry-run`) flag.  As a precautionary measure, if
//! the specified archive already appears to be on the target, `humility
//! flash` will fail unless the `-F` (`--force`) flag is set.
//!

use anyhow::{bail, Context, Result};
use clap::Command as ClapCommand;
use clap::{CommandFactory, Parser};
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
        humility::msg!("would execute: {:?}", cmd);
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

            std::fs::write(&srec, generate_srec_from_elf(&flash_config.elf)?)?;

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

            if subargs.dryrun {
                dryrun(&flash);
                return Ok(());
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
            std::fs::write(&ihex, generate_ihex_from_elf(&flash_config.elf)?)?;
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
                        anyhow::bail!("unexpected pyOCD argument {:?}", arg);
                    }
                }
            }

            for arg in reset_args {
                if let FlashArgument::Direct(ref val) = arg {
                    reset.arg(val);
                } else {
                    anyhow::bail!("unexpected pyOCD reset argument {:?}", arg);
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

pub fn init() -> (Command, ClapCommand<'static>) {
    (
        Command::Unattached {
            name: "flash",
            archive: Archive::Required,
            run: flashcmd,
        },
        FlashArgs::command(),
    )
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
