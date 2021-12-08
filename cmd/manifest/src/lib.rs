// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::Result;
use humility::hubris::HubrisArchive;
use humility_cmd::{Archive, Args, Command};
use structopt::clap::App;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(name = "manifest", about = "print archive manifest")]
struct ManifestArgs {}

fn manifestcmd(
    hubris: &mut HubrisArchive,
    _args: &Args,
    _subargs: &[String],
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
