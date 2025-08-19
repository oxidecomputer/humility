// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility writeword`
//!
//! Given a word-aligned address, writes the specified 32-bit value.
//! If multiple values are specified, writes each in turn, incrementing
//! the address by the word size after each write.
//!
//! **It should go without saying that this should be only used with care.**
//! In particular, the target is **not** halted before any writes (but that
//! functionality can be affected by using `writeword`).
//!
//! For example, to write the value 0x11223344 to 0x24018900:
//!
//! ```console
//! $ humility writeword 0x24018900 0x11223344
//! humility: attached via ST-Link V3
//! humility: writing 0x11223344 to 0x24018900
//! ```
//!
//! Note that the word is written as single, 32-bit value -- and will therefore
//! be little-endian if reading memory:
//!
//! ```console
//! $ humility readmem 0x24018900 16
//! humility: attached via ST-Link V3
//!              \/  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f
//! 0x24018900 | 44 33 22 11 00 00 00 00 00 00 00 00 00 00 00 00 | D3".............
//! ```
//!
//! To write multiple values to multiple words starting at the specified
//! address, specify them as additional arguments, e.g.:
//!
//! ```console
//! $ humility writeword 0x24047800 0xaa 0xbb 0xcc 0xdd
//! humility: attached via ST-Link V3
//! humility: writing 0xaa to 0x24047800
//! humility: writing 0xbb to 0x24047804
//! humility: writing 0xcc to 0x24047808
//! humility: writing 0xdd to 0x2404780c
//! $ humility readmem 0x24047800 16
//! humility: attached via ST-Link V3
//!             \/  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f
//! 0x24047800 | aa 00 00 00 bb 00 00 00 cc 00 00 00 dd 00 00 00 | ................
//! ```
//!

use anyhow::{Result, bail};
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
