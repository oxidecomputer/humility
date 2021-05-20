/*
 * Copyright 2020 Oxide Computer Company
 */

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
