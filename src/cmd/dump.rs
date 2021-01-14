/*
 * Copyright 2020 Oxide Computer Company
 */

use crate::attach_live;
use crate::cmd::{Archive, HumilityCommand};
use crate::hubris::*;
use crate::Args;
use anyhow::Result;
use structopt::clap::App;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(name = "dump", about = "generate Hubris dump")]
struct DumpArgs {
    dumpfile: Option<String>,
}

fn dumpcmd(
    hubris: &mut HubrisArchive,
    args: &Args,
    subargs: &Vec<String>,
) -> Result<()> {
    let subargs = DumpArgs::from_iter_safe(subargs)?;

    let mut core = attach_live(&args)?;
    hubris.validate(core.as_mut(), HubrisValidate::Booted)?;
    let _info = core.halt()?;
    info!("core halted");

    let rval = hubris.dump(core.as_mut(), subargs.dumpfile.as_deref());

    core.run()?;
    info!("core resumed");

    rval
}

pub fn init<'a, 'b>() -> (HumilityCommand, App<'a, 'b>) {
    (
        HumilityCommand {
            name: "dump",
            archive: Archive::Required,
            run: dumpcmd,
        },
        DumpArgs::clap(),
    )
}
