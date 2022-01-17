// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::{bail, Context, Result};
use humility::hubris::*;
use humility_cmd::Args;
use humility_cmd::{Archive, Command};
use std::collections::HashMap;
use structopt::clap::App;

pub fn init<'a, 'b>(
    app: App<'a, 'b>,
) -> (HashMap<&'static str, Command>, App<'a, 'b>) {
    let mut cmds = HashMap::new();
    let mut rval = app;

    let dcmds = [
        cmd_apptable::init,
        cmd_etm::init,
        cmd_dashboard::init,
        cmd_diagnose::init,
        cmd_dump::init,
        cmd_etm::init,
        cmd_gpio::init,
        cmd_hiffy::init,
        cmd_i2c::init,
        cmd_itm::init,
        cmd_jefe::init,
        cmd_lpc55gpio::init,
        cmd_manifest::init,
        cmd_map::init,
        cmd_pmbus::init,
        cmd_probe::init,
        cmd_qspi::init,
        cmd_readmem::init,
        cmd_readvar::init,
        cmd_renbb::init,
        cmd_rencm::init,
        cmd_ringbuf::init,
        cmd_sensors::init,
        cmd_spd::init,
        cmd_spi::init,
        cmd_stackmargin::init,
        cmd_tasks::init,
        cmd_test::init,
        cmd_trace::init,
        cmd_stmsecure::init,
        cmd_vsc7448::init,
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
    args: &Args,
    subargs: &[String],
) -> Result<()> {
    if let Some(command) = commands.get(&subargs[0].as_str()) {
        let archive = match command {
            Command::Attached { archive, .. } => archive,
            Command::Unattached { archive, .. } => archive,
        };

        let mut hubris =
            HubrisArchive::new().context("failed to initialize")?;

        if *archive != Archive::Ignored {
            if let Some(archive) = &args.archive {
                hubris.load(archive).context("failed to load archive")?;
            } else if let Some(dump) = &args.dump {
                hubris.load_dump(dump).context("failed to load dump")?;
            }
        }

        if *archive == Archive::Required && !hubris.loaded() {
            bail!("must provide a Hubris archive or dump");
        }

        match command {
            Command::Attached { run, attach, validate, .. } => {
                humility_cmd::attach(
                    &hubris,
                    args,
                    *attach,
                    *validate,
                    |h, core| (run)(h, core, args, subargs),
                )
            }
            Command::Unattached { run, .. } => {
                (run)(&mut hubris, args, subargs)
            }
        }
    } else {
        bail!("command {} not found", subargs[0]);
    }
}
