/*
 * Copyright 2020 Oxide Computer Company
 */

mod readmem;

use crate::hubris::HubrisArchive;
use crate::Args;
use anyhow::{bail, Result};
use std::collections::HashMap;
use structopt::clap::App;

pub struct HumilityCommand {
    name: &'static str,
    run: fn(&HubrisArchive, &Args, &Vec<String>) -> Result<()>,
}

pub fn init<'a, 'b>(
    app: App<'a, 'b>,
) -> (HashMap<&'static str, HumilityCommand>, App<'a, 'b>) {
    let mut cmds = HashMap::new();
    let mut rval = app;

    let dcmds = [readmem::init];

    for dcmd in &dcmds {
        let (cmd, subcmd) = dcmd();
        cmds.insert(cmd.name, cmd);
        rval = rval.subcommand(subcmd);
    }

    (cmds, rval)
}

pub fn subcommand(
    commands: &HashMap<&'static str, HumilityCommand>,
    hubris: &HubrisArchive,
    args: &Args,
    subargs: &Vec<String>,
) -> Result<()> {
    if let Some(command) = commands.get(&subargs[0].as_str()) {
        (command.run)(hubris, args, subargs)
    } else {
        bail!("command {} not found", subargs[0]);
    }
}
