// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

// mod apptable;
/*
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
mod renbb;
mod rencm;
mod ringbuf;
mod spd;
mod spi;
mod stackmargin;
mod stmsecure;
mod tasks;
mod test;
mod trace;
*/

use humility_cmd::Args;
use crate::{attach_dump, attach_live};
use anyhow::{bail, Result};
use humility::core::Core;
use humility::hubris::*;
use humility_cmd::{Archive, Attach, Validate, Command};
use std::collections::HashMap;
use std::convert::TryInto;
use structopt::clap::App;

pub fn init<'a, 'b>(
    app: App<'a, 'b>,
) -> (HashMap<&'static str, Command>, App<'a, 'b>) {
    let mut cmds = HashMap::new();
    let mut rval = app;

    let dcmds = [
        cmd_apptable::init,
        /*
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
        renbb::init,
        rencm::init,
        ringbuf::init,
        spd::init,
        spi::init,
        stackmargin::init,
        tasks::init,
        test::init,
        trace::init,
        stmsecure::init,
        */
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
