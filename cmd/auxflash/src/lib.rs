// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility auxflash`
//!
//! Tools to interact with the auxiliary flash, described in RFD 311.
//!
//! This subcommand should be rarely used; `humility flash` will automatically
//! program auxiliary flash when needed.

use anyhow::Result;
use clap::{CommandFactory, Parser};
use colored::Colorize;
use humility_cli::{ExecutionContext, Subcommand};
use humility_cmd::CommandKind;

use humility_auxflash::AuxFlashHandler;
use humility_cmd::{Archive, Attach, Command, Validate};

#[derive(Parser, Debug)]
#[clap(name = "auxflash", about = env!("CARGO_PKG_DESCRIPTION"))]
struct AuxFlashArgs {
    /// sets timeout
    #[clap(
        long, short = 'T', default_value_t = 15000, value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    #[clap(subcommand)]
    cmd: AuxFlashCommand,
}

#[derive(Parser, Debug)]
enum AuxFlashCommand {
    /// Prints the auxiliary flash status
    Status {
        #[clap(long, short)]
        verbose: bool,
    },
    Erase {
        #[clap(long, short)]
        slot: u32,
    },
    Read {
        #[clap(long, short)]
        slot: u32,

        /// Number of bytes to read (defaults to 1M)
        #[clap(long, short)]
        count: Option<usize>,
        output: String,
    },
    Write {
        #[clap(long, short)]
        slot: u32,
        #[clap(long, short = 'F')]
        force: bool,
        input: Option<String>,
    },
}

////////////////////////////////////////////////////////////////////////////////

fn auxflash_status(mut worker: AuxFlashHandler, verbose: bool) -> Result<()> {
    let slot_count = worker.slot_count()?;
    let active_slot = worker.active_slot()?;
    println!(" {} | {}", "slot".bold(), "status".bold());
    println!("------|----------------------------");
    for i in 0..slot_count {
        print!("  {:>3} | ", i);
        match worker.slot_status(i) {
            Err(e) => {
                println!("Error: {}", e.to_string().red());
            }
            Ok(None) => println!("{}", "Missing checksum".yellow()),
            Ok(Some(v)) => {
                if active_slot == Some(i) {
                    print!("{} (", "Active".green());
                } else {
                    print!("{} (", "Valid ".blue());
                }
                if verbose {
                    for byte in v {
                        print!("{:0>2x}", byte);
                    }
                } else {
                    for byte in &v[0..4] {
                        print!("{:0>2x}", byte);
                    }
                    print!("...");
                }
                println!(")");
            }
        }
    }
    Ok(())
}

fn auxflash(context: &mut ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let subargs = AuxFlashArgs::try_parse_from(subargs)?;
    let hubris = context.archive.as_ref().unwrap();
    let mut worker = AuxFlashHandler::new(hubris, core, subargs.timeout)?;

    match subargs.cmd {
        AuxFlashCommand::Status { verbose } => {
            auxflash_status(worker, verbose)?;
        }
        AuxFlashCommand::Erase { slot } => {
            worker.slot_erase(slot)?;
            humility::msg!("done erasing slot {slot}");
        }
        AuxFlashCommand::Read { slot, output, count } => {
            let data = worker.auxflash_read(slot, count)?;
            std::fs::write(output, data)?;
        }
        AuxFlashCommand::Write { slot, input, force } => match input {
            Some(input) => {
                let data = std::fs::read(input)?;
                worker.auxflash_write(slot, &data, force)?;
            }
            None => {
                // If the user didn't specify an image to flash on the
                // command line, then attempt to pull it from the image.
                worker.auxflash_write_from_archive(slot, force)?;
            }
        },
    }
    Ok(())
}

pub fn init() -> Command {
    Command {
        app: AuxFlashArgs::command(),
        name: "auxflash",
        run: auxflash,
        kind: CommandKind::Attached {
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
        },
    }
}
