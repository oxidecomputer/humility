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

use anyhow::{bail, Result};
use clap::Command as ClapCommand;
use clap::{CommandFactory, Parser};
use humility::hubris::*;
use humility_cmd::{Archive, Args, Command, RunUnattached};
use path_slash::PathExt;
use std::io::Write;
use std::process::ExitStatus;

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

    // This is incredibly ugly! It also gives us backwards compatibility!

    let chip = match config.program {
        FlashProgram::PyOcd(args) => {
            let s69 = regex::Regex::new(r"lpc55s69").unwrap();
            let s28 = regex::Regex::new(r"lpc55s28").unwrap();
            let mut c: Option<String> = None;
            for arg in args {
                c = match arg {
                    FlashArgument::Direct(s) => {
                        if s69.is_match(&s) {
                            Some("LPC55S69JBD100".to_string())
                        } else if s28.is_match(&s) {
                            Some("LPC55S28JBD64".to_string())
                        } else {
                            None
                        }
                    }
                    _ => None,
                };

                if c.is_some() {
                    break;
                }
            }

            if c.is_none() {
                bail!("Failed to find chip from pyOCD config");
            }

            c.unwrap()
        }
        FlashProgram::OpenOcd(a) => match a {
            FlashProgramConfig::Payload(d) => {
                let h7 = regex::Regex::new(r"find target/stm32h7").unwrap();
                let f3 = regex::Regex::new(r"find target/stm32f3").unwrap();
                let f4 = regex::Regex::new(r"find target/stm32f4").unwrap();
                let g0 = regex::Regex::new(r"find target/stm32g0").unwrap();

                let mut c: Option<String> = None;

                for s in d.split('\n') {
                    if h7.is_match(s) {
                        c = Some("STM32H753ZITx".to_string());
                        break;
                    }
                    if f3.is_match(s) {
                        c = Some("STM32F301C6Tx".to_string());
                        break;
                    }
                    if f4.is_match(s) {
                        c = Some("STM32F401CBUx".to_string());
                        break;
                    }
                    if g0.is_match(s) {
                        c = Some("STM32G030C6Tx".to_string());
                        break;
                    }
                }

                if c.is_none() {
                    bail!("Failed to get chip from OpenOCD config");
                }

                c.unwrap()
            }
            _ => bail!("Unexpected config?"),
        },
    };

    //
    // We need to attach to (1) confirm that we're plugged into something
    // and (2) extract serial information.
    //
    let probe = match &args.probe {
        Some(p) => p,
        None => "auto",
    };

    humility::msg!("attaching with chip set to {:x?}", chip);
    let mut c = humility::core::attach_for_flashing(probe, hubris, &chip)?;
    let core = c.as_mut();

    core.halt()?;

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
            core.run()?;
            bail!(
                "archive appears to be already flashed on attached device; \
                    use -F (\"--force\") to force re-flash"
            );
        }
    }

    let ihex = tempfile::NamedTempFile::new()?;
    std::fs::write(&ihex, flash_config.ihex)?;
    let ihex_path = ihex.path();

    //
    // This will reset the part if it works.
    //
    if let Err(err) = core.load(ihex_path) {
        core.run()?;
        Err(err)
    } else {
        humility::msg!("flashing done");
        Ok(())
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

pub fn init() -> (Command, ClapCommand<'static>) {
    (
        Command::Unattached {
            name: "flash",
            archive: Archive::Required,
            run: RunUnattached::Args(flashcmd),
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
