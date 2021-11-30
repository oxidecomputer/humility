// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::cmd::{Archive, Command};
use crate::hubris::HubrisArchive;
use crate::Args;
use anyhow::Result;
use structopt::clap::App;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(name = "manifest", about = "print archive manifest")]
struct ManifestArgs {}

fn manifestcmd(
    hubris: &mut HubrisArchive,
    _args: &Args,
    _subargs: &Vec<String>,
) -> Result<()> {
    hubris.manifest()?;
    Ok(())
}

pub fn init<'a, 'b>() -> (Command, App<'a, 'b>) {
    (
        Command::Unattached {
            name: "manifest",
            archive: Archive::Required,
            run: manifestcmd,
        },
        ManifestArgs::clap(),
    )
}
