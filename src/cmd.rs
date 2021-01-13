/*
 * Copyright 2020 Oxide Computer Company
 */

mod apptable;
mod dump;
mod etm;
mod i2c;
mod itm;
mod manifest;
mod map;
mod probe;
mod log;
mod readmem;
mod readvar;
mod ringbuf;
mod stackmargin;
mod tasks;
mod test;
mod trace;

use crate::hubris::HubrisArchive;
use crate::Args;
use anyhow::{bail, Result};
use std::collections::HashMap;
use structopt::clap::App;

#[allow(dead_code)]
#[derive(Copy, Clone, Debug)]
enum Archive {
    Required,
    Optional,
    Prohibited,
}

pub struct HumilityCommand {
    name: &'static str,
    archive: Archive,
    run: fn(&mut HubrisArchive, &Args, &Vec<String>) -> Result<()>,
}

pub fn init<'a, 'b>(
    app: App<'a, 'b>,
) -> (HashMap<&'static str, HumilityCommand>, App<'a, 'b>) {
    let mut cmds = HashMap::new();
    let mut rval = app;

    let dcmds = [
        apptable::init,
        dump::init,
        etm::init,
        i2c::init,
        itm::init,
        manifest::init,
        map::init,
        probe::init,
        log::init,
        readmem::init,
        readvar::init,
        ringbuf::init,
        stackmargin::init,
        tasks::init,
        test::init,
        trace::init,
    ];

    for dcmd in &dcmds {
        let (cmd, subcmd) = dcmd();
        cmds.insert(cmd.name, cmd);
        rval = rval.subcommand(subcmd);
    }

    (cmds, rval)
}

pub fn subcommand(
    commands: &HashMap<&'static str, HumilityCommand>,
    hubris: &mut HubrisArchive,
    args: &Args,
    subargs: &Vec<String>,
) -> Result<()> {
    if let Some(command) = commands.get(&subargs[0].as_str()) {
        match (command.archive, hubris.loaded()) {
            (Archive::Required, false) => {
                bail!("must provide a Hubris archive or dump");
            }

            (Archive::Prohibited, true) => {
                bail!("does not operate on a Hubris archive or dump");
            }

            (_, _) => {}
        }

        (command.run)(hubris, args, subargs)
    } else {
        bail!("command {} not found", subargs[0]);
    }
}
