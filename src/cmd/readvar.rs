/*
 * Copyright 2020 Oxide Computer Company
 */

use crate::attach;
use crate::cmd::{Archive, HumilityCommand};
use crate::hubris::{HubrisArchive, HubrisPrintFormat};
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
    /// list variables
    #[structopt(long, short)]
    list: bool,
    #[structopt(conflicts_with = "list")]
    variable: Option<String>,
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

    let v = match subargs.variable {
        Some(ref variable) => hubris.lookup_variable(variable)?,
        None => bail!("expected variable (use \"-l\" to list)"),
    };

    let mut core = attach(&args)?;
    hubris.validate(core.as_mut())?;

    let mut buf: Vec<u8> = vec![];
    buf.resize_with(v.size, Default::default);
    core.read_8(v.addr, buf.as_mut_slice())?;

    let fmt = HubrisPrintFormat { indent: 0, newline: true, hex: true };
    let name = subargs.variable.as_ref().unwrap();
    let dumped = hubris.printfmt(&buf, v.goff, &fmt)?;

    println!("{} (0x{:08x}) = {}", name, v.addr, dumped);

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
