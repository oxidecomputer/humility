// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::{Context, Result};
use clap::Command as ClapCommand;
use humility_cli::ExecutionContext;
use humility_cmd::Command;
use std::collections::HashMap;

//
// Our build.rs creates cmds.rs, which looks at our workspace to assemble
// the commands, and creates a function (`dcmds`) that we call to get
// a vector of them.
//
include!(concat!(env!("OUT_DIR"), "/cmds.rs"));

pub fn init(
    command: ClapCommand,
) -> (HashMap<&'static str, Command>, ClapCommand) {
    let mut cmds = HashMap::new();
    let mut rval = command;

    let dcmds = dcmds();

    for dcmd in dcmds {
        let cmd = (dcmd.init)();
        let app = cmd.app.clone();
        let name = cmd.name;

        cmds.insert(name, cmd);

        rval = rval.subcommand(app.after_help(dcmd.docmsg));
    }

    (cmds, rval)
}

pub fn subcommand(
    context: &mut ExecutionContext,
    commands: &HashMap<&'static str, Command>,
) -> Result<()> {
    let cmd = context.cli.cmd[0].as_str();

    let command = commands
        .get(cmd)
        .with_context(|| format!("command {} not found", cmd))?;

    (command.run)(context)
}
