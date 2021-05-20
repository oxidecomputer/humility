/*
 * Copyright 2020 Oxide Computer Company
 */

use crate::cmd::*;
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
    core: &mut dyn Core,
    _args: &Args,
    subargs: &Vec<String>,
) -> Result<()> {
    let subargs = DumpArgs::from_iter_safe(subargs)?;

    let _info = core.halt()?;
    info!("core halted");

    let rval = hubris.dump(core, subargs.dumpfile.as_deref());

    core.run()?;
    info!("core resumed");

    rval
}

pub fn init<'a, 'b>() -> (Command, App<'a, 'b>) {
    (
        Command::Attached {
            name: "dump",
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
            run: dumpcmd,
        },
        DumpArgs::clap(),
    )
}
