// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::{bail, Context, Result};
use clap::Command as ClapCommand;
use humility::cli::Subcommand;
use humility::hubris::*;
use humility_cmd::{Archive, Command, CommandKind};
use std::collections::HashMap;

//
// Our build.rs creates cmds.rs, which looks at our workspace to assemble
// the commands, and creates a function (`dcmds`) that we call to get
// a vector of them.
//
include!(concat!(env!("OUT_DIR"), "/cmds.rs"));

use crate::cmd_repl;

pub fn init(
    command: ClapCommand<'static>,
) -> (HashMap<&'static str, Command>, ClapCommand<'static>) {
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
    context: &mut humility::ExecutionContext,
    commands: &HashMap<&'static str, Command>,
) -> Result<()> {
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let cmd = subargs[0].as_str();

    let command = commands
        .get(cmd)
        .with_context(|| format!("command {} not found", cmd))?;

    let mut hubris = HubrisArchive::new().context("failed to initialize")?;

    let (archive, doneness) = match &command.kind {
        CommandKind::Attached { archive, .. } => {
            (*archive, HubrisArchiveDoneness::Cook)
        }
        CommandKind::Unattached { archive, .. } => {
            (*archive, HubrisArchiveDoneness::Cook)
        }
        CommandKind::Detached { archive, .. } => {
            (*archive, HubrisArchiveDoneness::Cook)
        }
        CommandKind::Raw { .. } => {
            (Archive::Required, HubrisArchiveDoneness::Raw)
        }
    };

    if archive != Archive::Ignored {
        if let Some(archive) = &context.cli.archive {
            hubris.load(archive, doneness).with_context(|| {
                format!("failed to load archive \"{}\"", archive)
            })?;
        } else if let Some(dump) = &context.cli.dump {
            hubris
                .load_dump(dump, doneness)
                .with_context(|| format!("failed to load dump \"{}\"", dump))?;
        }
    }

    if archive == Archive::Required
        && doneness == HubrisArchiveDoneness::Cook
        && !hubris.loaded()
    {
        if context.cli.environment.is_some() {
            bail!("must provide a Hubris archive, dump, or name");
        }

        bail!("must provide a Hubris archive or dump");
    }

    context.archive = Some(hubris);

    let run = command.run;

    match &command.kind {
        CommandKind::Attached { attach, validate, .. } => {
            humility_cmd::attach(context, *attach, *validate, |context| {
                (run)(context)
            })
        }
        CommandKind::Unattached { .. } => (run)(context),
        CommandKind::Detached { .. } => {
            if context.cli.dump.is_some() {
                bail!("cannot specify a dump for {} command", cmd);
            }

            if context.cli.ip.is_some() {
                bail!("cannot specify IP address for {} command", cmd);
            }

            if context.cli.probe.is_some() {
                bail!("cannot specify probe for {} command", cmd);
            }

            (run)(context)
        }
        CommandKind::Raw { .. } => (run)(context),
    }
}
