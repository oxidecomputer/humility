// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::{bail, Result};
use humility::core::Core;
use humility::hubris::*;
use humility_cmd::{Archive, Args, Attach, Command, Validate};
use structopt::clap::App;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(
    name = "readvar",
    about = "read and display a specified Hubris variable"
)]
struct ReadvarArgs {
    /// values in decimal instead of hex
    #[structopt(long, short)]
    decimal: bool,
    /// list variables
    #[structopt(long, short)]
    list: bool,
    #[structopt(conflicts_with = "list")]
    variable: Option<String>,
}

fn readvar_dump(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    variable: &HubrisVariable,
    subargs: &ReadvarArgs,
) -> Result<()> {
    let mut buf: Vec<u8> = vec![];
    buf.resize_with(variable.size, Default::default);

    let _info = core.halt()?;
    core.read_8(variable.addr, buf.as_mut_slice())?;
    core.run()?;

    let hex = !subargs.decimal;

    let fmt = HubrisPrintFormat {
        newline: true,
        hex,
        ..HubrisPrintFormat::default()
    };
    let name = subargs.variable.as_ref().unwrap();
    let dumped = hubris.printfmt(&buf, variable.goff, &fmt)?;

    println!("{} (0x{:08x}) = {}", name, variable.addr, dumped);

    Ok(())
}

fn readvar(
    hubris: &mut HubrisArchive,
    core: &mut dyn Core,
    _args: &Args,
    subargs: &Vec<String>,
) -> Result<()> {
    let subargs = ReadvarArgs::from_iter_safe(subargs)?;

    if subargs.list {
        return hubris.list_variables();
    }

    let variables = match subargs.variable {
        Some(ref variable) => hubris.lookup_variables(variable)?,
        None => bail!("expected variable (use \"-l\" to list)"),
    };

    for v in variables {
        readvar_dump(hubris, core, v, &subargs)?;
    }

    Ok(())
}

pub fn init<'a, 'b>() -> (Command, App<'a, 'b>) {
    (
        Command::Attached {
            name: "readvar",
            archive: Archive::Required,
            attach: Attach::Any,
            validate: Validate::Match,
            run: readvar,
        },
        ReadvarArgs::clap(),
    )
}
