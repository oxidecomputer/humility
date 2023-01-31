// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

pub mod doppel;
pub mod hiffy;
pub mod i2c;
pub mod idol;
pub mod jefe;
pub mod stack;
pub mod test;

use anyhow::{bail, Result};
use clap::Command as ClapCommand;
use humility::cli::Cli;
use humility::core::Core;
use humility::hubris::*;
use std::time::Duration;

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

pub struct Command {
    pub app: ClapCommand<'static>,
    pub name: &'static str,
    pub kind: CommandKind,
    pub run: fn(&mut humility::ExecutionContext) -> Result<()>,
}

impl Command {
    pub fn short_description(&self) -> String {
        self.app.get_about().unwrap().to_string()
    }
}

pub enum CommandKind {
    /// Attached to a live system or dump
    Attached { archive: Archive, attach: Attach, validate: Validate },
    /// Not attached to a live system or dump
    Unattached { archive: Archive },
    /// Will never attach: any options attaching should be an error
    Detached { archive: Archive },
    /// Operate on a raw archive, from either the command line or a dump
    Raw,
}

pub fn attach_live(
    args: &Cli,
    hubris: &HubrisArchive,
) -> Result<Box<dyn Core>> {
    if args.dump.is_some() {
        bail!("must be run against a live system");
    } else if args.ip.is_some() {
        attach_net(args, hubris)
    } else {
        let probe = match &args.probe {
            Some(p) => p,
            None => "auto",
        };

        humility::core::attach(probe, hubris)
    }
}

pub fn attach_dump(
    args: &Cli,
    hubris: &HubrisArchive,
) -> Result<Box<dyn Core>> {
    if let Some(dump) = &args.dump {
        humility::core::attach_dump(dump, hubris)
    } else {
        bail!("must be run against a dump");
    }
}

pub fn attach_net(args: &Cli, hubris: &HubrisArchive) -> Result<Box<dyn Core>> {
    if let Some(ip) = &args.ip {
        let timeout = Duration::from_millis(args.timeout as u64);
        humility::core::attach_net(ip, hubris, timeout)
    } else {
        bail!("must be run against a specified IP address");
    }
}

pub fn attach(
    context: &mut humility::ExecutionContext,
    attach: Attach,
    validate: Validate,
    mut run: impl FnMut(&mut humility::ExecutionContext) -> Result<()>,
) -> Result<()> {
    let hubris = context.archive.as_ref().unwrap();

    if context.core.is_none() {
        context.core = Some(match attach {
            Attach::LiveOnly => attach_live(&context.cli, hubris),
            Attach::DumpOnly => attach_dump(&context.cli, hubris),
            Attach::Any => {
                //
                // Our Clap constraints prevent us from having more than one
                // of these set.
                //
                if context.cli.dump.is_some() {
                    attach_dump(&context.cli, hubris)
                } else {
                    attach_live(&context.cli, hubris)
                }
            }
        }?);
    }

    // we know from above we have set up a core if we hadn't previously
    let core = context.core.as_mut().unwrap();

    match validate {
        Validate::Booted => {
            hubris.validate(&mut **core, HubrisValidate::Booted)?;
        }
        Validate::Match => {
            hubris.validate(&mut **core, HubrisValidate::ArchiveMatch)?;
        }
        Validate::None => {}
    }

    (run)(context)
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

        //
        // Print out header line, OpenBoot PROM style
        //
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

        //
        // Print our first line.
        //
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
