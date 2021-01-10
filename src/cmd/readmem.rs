/*
 * Copyright 2020 Oxide Computer Company
 */

use crate::attach;
use crate::cmd::{Archive, HumilityCommand};
use crate::hubris::HubrisArchive;
use crate::Args;
use anyhow::{bail, Result};
use std::convert::TryInto;
use structopt::clap::App;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(name = "readmem", about = "read and display memory region")]
struct ReadmemArgs {
    /// print out as halfwords instead of as bytes
    #[structopt(long, short, conflicts_with_all = &["word"])]
    halfword: bool,

    /// print out as words instead of as bytes
    #[structopt(long, short)]
    word: bool,

    /// address to read
    address: String,

    /// length to read
    #[structopt(parse(try_from_str = parse_int::parse))]
    length: Option<usize>,
}

fn readmem(
    hubris: &HubrisArchive,
    args: &Args,
    subargs: &Vec<String>,
) -> Result<()> {
    let subargs = ReadmemArgs::from_iter_safe(subargs)?;
    let mut core = attach(&args)?;
    let max = crate::core::CORE_MAX_READSIZE;
    let width: usize = 16;
    let size = if subargs.word {
        4
    } else if subargs.halfword {
        2
    } else {
        1
    };

    let length = match subargs.length {
        Some(length) => length,
        None => 256,
    };

    if length & (size - 1) != 0 {
        bail!("length must be {}-byte aligned", size);
    }

    let mut addr = match parse_int::parse::<u32>(&subargs.address) {
        Ok(addr) => addr,
        _ => hubris.lookup_peripheral(&subargs.address)?,
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

    let print = |line: &[u8], addr, offs| {
        print!("0x{:08x} | ", addr);

        for i in (0..width).step_by(size) {
            if i < offs || i - offs >= line.len() {
                print!(" {:width$}", "", width = size * 2);
                continue;
            }

            let slice = &line[i - offs..i - offs + size];

            print!(
                "{:0width$x} ",
                match size {
                    1 => line[i - offs] as u32,
                    2 => u16::from_le_bytes(slice.try_into().unwrap()) as u32,
                    4 => u32::from_le_bytes(slice.try_into().unwrap()) as u32,
                    _ => {
                        panic!("invalid size");
                    }
                },
                width = size * 2
            );
        }

        print!("| ");

        for i in 0..width {
            if i < offs || i - offs >= line.len() {
                print!(" ");
            } else {
                let c = line[i - offs] as char;

                if c.is_ascii() && !c.is_ascii_control() {
                    print!("{}", c);
                } else {
                    print!(".");
                }
            }
        }

        println!("");
    };

    let offs = (addr & (width - 1) as u32) as usize;
    addr -= offs as u32;

    /*
     * Print out header line, OpenBoot PROM style
     */
    print!("  {:8}  ", "");

    for i in (0..width).step_by(size) {
        if i == offs {
            print!(" {:>width$}", "\\/", width = size * 2);
        } else {
            print!(" {:>width$x}", i, width = size * 2);
        }
    }

    println!("");

    /*
     * Print our first line.
     */
    let lim = std::cmp::min(width - offs, bytes.len());
    print(&bytes[0..lim], addr, offs);

    if lim < bytes.len() {
        let lines = bytes[lim..].chunks(width);

        for line in lines {
            addr += width as u32;
            print(line, addr, 0);
        }
    }

    Ok(())
}

pub fn init<'a, 'b>() -> (HumilityCommand, App<'a, 'b>) {
    (
        HumilityCommand {
            name: "readmem",
            archive: Archive::Optional,
            run: readmem,
        },
        ReadmemArgs::clap(),
    )
}
