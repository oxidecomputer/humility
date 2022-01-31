// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

pub mod doppel;
pub mod hiffy;
pub mod i2c;
pub mod idol;
pub mod jefe;
pub mod reflect;
pub mod test;

use anyhow::{bail, Result};
use clap::Parser;
use humility::core::Core;
use humility::hubris::*;

#[macro_use]
extern crate log;

#[derive(Parser)]
#[clap(name = "humility", max_term_width = 80)]
pub struct Args {
    /// verbose messages
    #[clap(long, short)]
    pub verbose: bool,

    /// specific chip on attached device
    #[clap(
        long,
        short,
        env = "HUMILITY_CHIP",
        default_value = "STM32F407VGTx"
    )]
    pub chip: String,

    /// chip probe to use
    #[clap(long, short, env = "HUMILITY_PROBE", conflicts_with = "dump")]
    pub probe: Option<String>,

    /// Hubris archive
    #[clap(long, short, env = "HUMILITY_ARCHIVE")]
    pub archive: Option<String>,

    /// Hubris dump
    #[clap(long, short, env = "HUMILITY_DUMP")]
    pub dump: Option<String>,

    #[clap(subcommand)]
    pub cmd: Subcommand,
}

#[derive(Parser)]
pub enum Subcommand {
    #[clap(external_subcommand)]
    Other(Vec<String>),
}

#[allow(dead_code)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Archive {
    /// Load a Hubris archive, failing if one is not present
    Required,
    /// Load a Hubris archive if available (as either a command-line flag or
    /// environmental variable); continue to run if no archive is present
    Optional,
    /// Do not load a Hubris archive, even if the command-line flag or
    /// environmental variable is set.  This saves a small amount of start-up
    /// time for subcommands which don't require a Hubris archive.
    Ignored,
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
        run: fn(&HubrisArchive, &mut dyn Core, &Args, &[String]) -> Result<()>,
    },
    Unattached {
        name: &'static str,
        archive: Archive,
        run: fn(&mut HubrisArchive, &Args, &[String]) -> Result<()>,
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

pub fn attach(
    hubris: &HubrisArchive,
    args: &Args,
    attach: Attach,
    validate: Validate,
    mut run: impl FnMut(&HubrisArchive, &mut dyn Core) -> Result<()>,
) -> Result<()> {
    let mut c = match attach {
        Attach::LiveOnly => attach_live(args),
        Attach::DumpOnly => attach_dump(args, hubris),
        Attach::Any => {
            if args.dump.is_some() {
                attach_dump(args, hubris)
            } else {
                attach_live(args)
            }
        }
    }?;

    let core = c.as_mut();

    match validate {
        Validate::Booted => {
            hubris.validate(core, HubrisValidate::Booted)?;
        }
        Validate::Match => {
            hubris.validate(core, HubrisValidate::ArchiveMatch)?;
        }
        Validate::None => {}
    }

    (run)(hubris, core)
}

pub struct Dumper {
    /// Word size, in bytes
    pub size: usize,

    /// Width of memory, in bytes
    pub width: usize,

    /// Address size, in nibbles
    pub addrsize: usize,

    /// Left indentation, in characters
    pub indent: usize,

    /// Left indent should be a hanging indent
    pub hanging: bool,

    /// Print the OpenBoot PROM-style header line
    pub header: bool,

    /// Print the ASCII translation of characters in the right margin
    pub ascii: bool,
}

impl Dumper {
    pub fn new() -> Self {
        Self {
            size: 1,
            width: 16,
            addrsize: 8,
            indent: 0,
            hanging: false,
            header: true,
            ascii: true,
        }
    }

    pub fn dump(&self, bytes: &[u8], addr: u32) {
        let size = self.size;
        let width = self.width;
        let mut addr = addr;
        let mut indent = if self.hanging { 0 } else { self.indent };

        let print = |line: &[u8], addr, offs, indent| {
            print!(
                "{:indent$}0x{:0width$x} | ",
                "",
                addr,
                indent = indent,
                width = self.addrsize
            );

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
                        2 =>
                            u16::from_le_bytes(slice.try_into().unwrap()) as u32,
                        4 =>
                            u32::from_le_bytes(slice.try_into().unwrap()) as u32,
                        _ => {
                            panic!("invalid size");
                        }
                    },
                    width = size * 2
                );
            }

            if self.ascii {
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
            }

            println!();
        };

        let offs = (addr & (width - 1) as u32) as usize;
        addr -= offs as u32;

        /*
         * Print out header line, OpenBoot PROM style
         */
        if self.header {
            print!("  {:width$}  ", "", width = indent + self.addrsize);

            for i in (0..width).step_by(size) {
                if i == offs {
                    print!(" {:>width$}", "\\/", width = size * 2);
                } else {
                    print!(" {:>width$x}", i, width = size * 2);
                }
            }

            println!();
            indent = self.indent;
        }

        /*
         * Print our first line.
         */
        let lim = std::cmp::min(width - offs, bytes.len());
        print(&bytes[0..lim], addr, offs, indent);
        indent = self.indent;

        if lim < bytes.len() {
            let lines = bytes[lim..].chunks(width);

            for line in lines {
                addr += width as u32;
                print(line, addr, 0, indent);
            }
        }
    }
}

impl Default for Dumper {
    fn default() -> Self {
        Self::new()
    }
}
