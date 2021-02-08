/*
 * Copyright 2020 Oxide Computer Company
 */

use crate::attach;
use crate::cmd::{Archive, HumilityCommand};
use crate::core::Core;
use crate::hubris::*;
use crate::Args;
use anyhow::{bail, Result};
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
    core.read_8(variable.addr, buf.as_mut_slice())?;
    let hex = !subargs.decimal;

    let fmt = HubrisPrintFormat { indent: 0, newline: true, hex: hex };
    let name = subargs.variable.as_ref().unwrap();
    let dumped = hubris.printfmt(&buf, variable.goff, &fmt)?;

    println!("{} (0x{:08x}) = {}", name, variable.addr, dumped);

    Ok(())
}

fn readvar(
    hubris: &mut HubrisArchive,
    args: &Args,
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

    let mut core = attach(&args)?;
    hubris.validate(core.as_mut(), HubrisValidate::ArchiveMatch)?;

    for v in variables {
        readvar_dump(hubris, core.as_mut(), v, &subargs)?;
    }

    Ok(())
}

pub fn init<'a, 'b>() -> (HumilityCommand, App<'a, 'b>) {
    (
        HumilityCommand {
            name: "readvar",
            archive: Archive::Required,
            run: readvar,
        },
        ReadvarArgs::clap(),
    )
}
