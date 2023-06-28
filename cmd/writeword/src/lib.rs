// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility writeword`
//!
//! To the specified word-aligned address, writes the specified 32-bit value.
//! If multiple values are specified, writes each in turn, incrementing
//! the address by the word size after each write.
//!

use anyhow::{bail, Result};
use clap::{CommandFactory, Parser};
use humility_cli::{ExecutionContext, Subcommand};
use humility_cmd::{Archive, Attach, Command, CommandKind, Validate};

#[derive(Parser, Debug)]
#[clap(name = "writeword", about = env!("CARGO_PKG_DESCRIPTION"))]
struct WritewordArgs {
    /// address to write to
    #[clap(parse(try_from_str = parse_int::parse))]
    address: u32,

    /// value(s) to write
    #[clap(parse(try_from_str = parse_int::parse))]
    value: Vec<u32>,
}

fn writeword(context: &mut ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();

    let subargs = WritewordArgs::try_parse_from(subargs)?;

    if subargs.address & 0b11 != 0 {
        bail!("address must be word aligned");
    }

    for (offs, v) in subargs.value.iter().enumerate() {
        let addr = subargs.address + (offs * 4) as u32;
        humility::msg!("writing {v:#x} to {addr:#x}");
        core.write_word_32(addr, *v)?;
    }

    Ok(())
}

pub fn init() -> Command {
    Command {
        app: WritewordArgs::command(),
        name: "writeword",
        run: writeword,
        kind: CommandKind::Attached {
            archive: Archive::Optional,
            attach: Attach::LiveOnly,
            validate: Validate::None,
        },
    }
}
