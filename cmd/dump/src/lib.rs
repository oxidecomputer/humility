// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

#[macro_use]
extern crate log;

use anyhow::Result;
use humility::core::Core;
use humility::hubris::*;
use humility_cmd::{Archive, Args, Attach, Command, Validate};
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
    subargs: &[String],
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
