/*
 * Copyright 2020 Oxide Computer Company
 */

mod apptable;
mod diagnose;
mod dump;
mod etm;
mod gpio;
mod hiffy;
mod i2c;
mod itm;
mod jefe;
mod manifest;
mod map;
mod pmbus;
mod probe;
mod qspi;
mod readmem;
mod readvar;
mod rencm;
mod ringbuf;
mod spd;
mod spi;
mod stackmargin;
mod stmsecure;
mod tasks;
mod test;
mod trace;

use crate::core::Core;
use crate::hubris::*;
use crate::Args;
use crate::{attach_dump, attach_live};
use anyhow::{bail, Result};
use std::collections::HashMap;
use std::convert::TryInto;
use structopt::clap::App;

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

pub fn init<'a, 'b>(
    app: App<'a, 'b>,
) -> (HashMap<&'static str, Command>, App<'a, 'b>) {
    let mut cmds = HashMap::new();
    let mut rval = app;

    let dcmds = [
        apptable::init,
        diagnose::init,
        dump::init,
        etm::init,
        gpio::init,
        hiffy::init,
        i2c::init,
        itm::init,
        jefe::init,
        manifest::init,
        map::init,
        pmbus::init,
        probe::init,
        qspi::init,
        readmem::init,
        readvar::init,
        rencm::init,
        ringbuf::init,
        spd::init,
        spi::init,
        stackmargin::init,
        tasks::init,
        test::init,
        trace::init,
        stmsecure::init,
    ];

    for dcmd in &dcmds {
        let (cmd, subcmd) = dcmd();

        let name = match cmd {
            Command::Attached { name, .. } => name,
            Command::Unattached { name, .. } => name,
        };

        cmds.insert(name, cmd);
        rval = rval.subcommand(subcmd);
    }

    (cmds, rval)
}

pub fn subcommand(
    commands: &HashMap<&'static str, Command>,
    hubris: &mut HubrisArchive,
    args: &Args,
    subargs: &Vec<String>,
) -> Result<()> {
    if let Some(command) = commands.get(&subargs[0].as_str()) {
        let archive = match command {
            Command::Attached { archive, .. } => archive,
            Command::Unattached { archive, .. } => archive,
        };

        match (archive, hubris.loaded()) {
            (Archive::Required, false) => {
                bail!("must provide a Hubris archive or dump");
            }

            (Archive::Prohibited, true) => {
                bail!("does not operate on a Hubris archive or dump");
            }

            (_, _) => {}
        }

        match command {
            Command::Attached { run, attach, validate, .. } => {
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

                (run)(hubris, core, args, subargs)
            }
            Command::Unattached { run, .. } => (run)(hubris, args, subargs),
        }
    } else {
        bail!("command {} not found", subargs[0]);
    }
}

fn printmem(bytes: &[u8], addr: u32, size: usize, width: usize) {
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
