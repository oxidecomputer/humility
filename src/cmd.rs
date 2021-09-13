/*
 * Copyright 2020 Oxide Computer Company
 */

mod apptable;
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
mod readmem;
mod readvar;
mod ringbuf;
mod spd;
mod spi;
mod stackmargin;
mod tasks;
mod test;
mod trace;

use crate::core::Core;
use crate::hubris::*;
use crate::Args;
use crate::{attach_dump, attach_live};
use anyhow::{bail, Result};
use std::collections::HashMap;
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
        readmem::init,
        readvar::init,
        ringbuf::init,
        spd::init,
        spi::init,
        stackmargin::init,
        tasks::init,
        test::init,
        trace::init,
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
                    Attach::LiveOnly => attach_live(&args),
                    Attach::DumpOnly => attach_dump(&args, hubris),
                    Attach::Any => {
                        if let Some(_) = &args.dump {
                            attach_dump(&args, hubris)
                        } else {
                            attach_live(&args)
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
