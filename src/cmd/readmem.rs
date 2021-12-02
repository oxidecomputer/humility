// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::cmd::*;
use crate::Args;
use anyhow::{bail, Result};
use humility::core::Core;
use humility::hubris::*;
use std::convert::TryInto;
use structopt::clap::App;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(name = "readmem", about = "read and display memory region")]
struct ReadmemArgs {
    /// print out as halfwords instead of as bytes
    #[structopt(long, short, conflicts_with_all = &["word", "symbol"])]
    halfword: bool,

    /// print out as words instead of as bytes
    #[structopt(long, short, conflicts_with_all = &["symbol"])]
    word: bool,

    /// print out as symbols
    #[structopt(long, short)]
    symbol: bool,

    /// address to read
    address: String,

    /// length to read
    #[structopt(parse(try_from_str = parse_int::parse))]
    length: Option<usize>,
}

fn readmem(
    hubris: &mut HubrisArchive,
    core: &mut dyn Core,
    _args: &Args,
    subargs: &Vec<String>,
) -> Result<()> {
    let subargs = ReadmemArgs::from_iter_safe(subargs)?;
    let max = humility::core::CORE_MAX_READSIZE;
    let size = if subargs.word || subargs.symbol {
        4
    } else if subargs.halfword {
        2
    } else {
        1
    };

    let length = subargs.length.unwrap_or(256);

    if length & (size - 1) != 0 {
        bail!("length must be {}-byte aligned", size);
    }

    if subargs.symbol {
        hubris.validate(core, HubrisValidate::ArchiveMatch)?;
    }

    let addr = match parse_int::parse::<u32>(&subargs.address) {
        Ok(addr) => addr,
        _ => {
            hubris.validate(core, HubrisValidate::ArchiveMatch)?;
            hubris.lookup_peripheral(&subargs.address)?
        }
    };

    if addr & (size - 1) as u32 != 0 {
        bail!("address must be {}-byte aligned", size);
    }

    if length > max {
        bail!("cannot read more than {} bytes", max);
    }

    let mut bytes = vec![0u8; length];

    let _info = core.halt()?;

    let rval = core.read_8(addr, &mut bytes);
    core.run()?;

    if rval.is_err() {
        return rval;
    }

    if subargs.symbol {
        for offs in (0..length).step_by(size) {
            let slice = &bytes[offs..offs + size];
            let val = u32::from_le_bytes(slice.try_into().unwrap());
            println!(
                "0x{:08x} | 0x{:08x}{}",
                addr + offs as u32,
                val,
                if let Some(sval) = hubris.instr_sym(val) {
                    format!(
                        " <- {}{}+0x{:x}",
                        match hubris.instr_mod(val) {
                            Some(module) if module != "kernel" => {
                                format!("{}:", module)
                            }
                            _ => "".to_string(),
                        },
                        sval.0,
                        val - sval.1
                    )
                } else {
                    "".to_string()
                }
            );
        }

        return Ok(());
    }

    printmem(&bytes, addr, size, 16);

    Ok(())
}

pub fn init<'a, 'b>() -> (Command, App<'a, 'b>) {
    (
        Command::Attached {
            name: "readmem",
            archive: Archive::Optional,
            attach: Attach::Any,
            validate: Validate::None,
            run: readmem,
        },
        ReadmemArgs::clap(),
    )
}
