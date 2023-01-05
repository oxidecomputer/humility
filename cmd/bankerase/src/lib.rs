// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility bankerase`
//!
//! Erase some memory please write some words
//!
//!

use anyhow::{bail, Result};
use clap::Command as ClapCommand;
use clap::{CommandFactory, Parser};
use humility::cli::{Subcommand};
use humility_cmd::{Archive, Command};

#[derive(Parser, Debug)]
#[clap(name = "bankerase", about = env!("CARGO_PKG_DESCRIPTION"))]
struct FlashArgs {
    /// force re-flashing if archive matches
    #[clap(long, short = 'F')]
    force: bool,

    /// reset delay
    #[clap(
        long = "reset-delay", short = 'd',
        default_value_t = 100, value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    reset_delay: u64,

    #[clap(long, parse(try_from_str = parse_int::parse))]
    address: u32,

    #[clap(long, parse(try_from_str = parse_int::parse))]
    len: u32
}

fn bankerasecmd(context: &mut humility::ExecutionContext) -> Result<()> {
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let hubris = context.archive.as_mut().unwrap();
    let subargs = FlashArgs::try_parse_from(subargs)?;

    let config = hubris.load_flash_config()?;

    let probe = match &context.cli.probe {
        Some(p) => p,
        None => "auto",
    };

    let chip = match config.chip {
        Some(c) => c,
        None => bail!("no"),
    };

    humility::msg!("attaching with chip set to {:x?}", chip);
    let mut c = humility::core::attach_for_flashing(probe, hubris, &chip)?;
    let core = c.as_mut();

    let ihex = tempfile::NamedTempFile::new()?;

    const ALIGN_MASK : u32 = 0x8000 - 1;

    let len = subargs.len + ALIGN_MASK & !ALIGN_MASK;

    std::fs::write(&ihex, generate_zeros(subargs.address, len)?)?;
    let ihex_path = ihex.path();

    //
    // Load the flash image, and reset the part if that works.
    //
    if let Err(err) = core.load(ihex_path) {
        core.run()?;
        return Err(err);
    }

    let delay = subargs.reset_delay;

    if delay != 0 {
        std::thread::sleep(std::time::Duration::from_millis(delay));
    }

    core.reset()?;

    humility::msg!("Erase done");
    Ok(())
}

pub fn init() -> (Command, ClapCommand<'static>) {
    (
        Command::Unattached {
            name: "bankerase",
            archive: Archive::Required,
            run: bankerasecmd,
        },
        FlashArgs::command(),
    )
}

fn generate_zeros(address: u32, len : u32) -> Result<String> {
    let mut records = vec![];

    // Generate a series of 32 byte zero entries
    for i in (0..len).step_by(32) {
        let a = address + i;
        records.push(ihex::Record::ExtendedLinearAddress((a >> 16) as u16));
        records.push(ihex::Record::Data {
            offset: a as u16,
            value: vec![0; 32],
        });
    }

    records.push(ihex::Record::EndOfFile);

    Ok(ihex::create_object_file_representation(&records)?)
}
