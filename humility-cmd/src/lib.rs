// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

pub mod doppel;
pub mod hiffy;
pub mod i2c;
pub mod jefe;
pub mod reflect;
pub mod test;

use anyhow::{bail, Result};
use humility::core::Core;
use humility::hubris::*;
use structopt::StructOpt;

#[macro_use]
extern crate log;

#[derive(StructOpt)]
#[structopt(name = "humility", max_term_width = 80)]
pub struct Args {
    /// verbose messages
    #[structopt(long, short)]
    pub verbose: bool,

    /// specific chip on attached device
    #[structopt(
        long,
        short,
        env = "HUMILITY_CHIP",
        default_value = "STM32F407VGTx"
    )]
    pub chip: String,

    /// chip probe to use
    #[structopt(long, short, env = "HUMILITY_PROBE", conflicts_with = "dump")]
    pub probe: Option<String>,

    /// Hubris archive
    #[structopt(
        long,
        short,
        env = "HUMILITY_ARCHIVE",
        conflicts_with = "dump"
    )]
    pub archive: Option<String>,

    /// Hubris dump
    #[structopt(long, short, env = "HUMILITY_DUMP")]
    pub dump: Option<String>,

    #[structopt(subcommand)]
    pub cmd: Subcommand,
}

#[derive(StructOpt)]
pub enum Subcommand {
    #[structopt(external_subcommand)]
    Other(Vec<String>),
}

#[allow(dead_code)]
#[derive(Copy, Clone, Debug)]
pub enum Archive {
    Required,
    Optional,
    Prohibited,
}

#[allow(dead_code)]
#[derive(Copy, Clone, Debug)]
pub enum Attach {
    LiveOnly,
    DumpOnly,
    Any,
}

#[allow(dead_code)]
#[derive(Copy, Clone, Debug)]
pub enum Validate {
    Match,
    Booted,
    None,
}

pub enum Command {
    Attached {
        name: &'static str,
        archive: Archive,
        attach: Attach,
        validate: Validate,
        run: fn(
            &mut HubrisArchive,
            &mut dyn Core,
            &Args,
            &Vec<String>,
        ) -> Result<()>,
    },
    Unattached {
        name: &'static str,
        archive: Archive,
        run: fn(&mut HubrisArchive, &Args, &Vec<String>) -> Result<()>,
    },
}

pub fn attach_live(args: &Args) -> Result<Box<dyn Core>> {
    if args.dump.is_some() {
        bail!("must be run against a live system");
    } else {
        let probe = match &args.probe {
            Some(p) => p,
            None => "auto",
        };

        humility::core::attach(probe, &args.chip)
    }
}

pub fn attach_dump(
    args: &Args,
    hubris: &HubrisArchive,
) -> Result<Box<dyn Core>> {
    if let Some(dump) = &args.dump {
        humility::core::attach_dump(dump, hubris)
    } else {
        bail!("must be run against a dump");
    }
}

pub fn printmem(bytes: &[u8], addr: u32, size: usize, width: usize) {
    let mut addr = addr;

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

        println!();
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

    println!();

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
}
