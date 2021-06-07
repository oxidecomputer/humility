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
#[structopt(
    name = "map",
    about = "print memory map, with association of regions to tasks"
)]
struct MapArgs {}

fn mapcmd(
    hubris: &mut HubrisArchive,
    core: &mut dyn crate::core::Core,
    _args: &Args,
    _subargs: &Vec<String>,
) -> Result<()> {
    let regions = hubris.regions(core)?;

    println!(
        "{:10} {:10}   {:10} {:>7} {:5} {:2} {}",
        "DESC", "LOW", "HIGH", "SIZE", "ATTR", "ID", "TASK"
    );

    for (_, region) in regions.iter() {
        println!(
            "{:10} 0x{:08x} - 0x{:08x} {:>7} {}{}{}{}{} {:2} {}",
            match region.daddr {
                Some(daddr) => format!("0x{:08x}", daddr),
                None => "-".to_owned(),
            },
            region.base,
            region.base + region.mapsize - 1,
            if region.mapsize >= 1024 {
                format!("{}KiB", region.mapsize >> 10)
            } else {
                format!("{}", region.mapsize)
            },
            if region.attr.read { "r" } else { "-" },
            if region.attr.write { "w" } else { "-" },
            if region.attr.execute { "x" } else { "-" },
            if region.attr.device { "d" } else { "-" },
            if region.attr.dma { "m" } else { "-" },
            region.task.id(),
            hubris.lookup_module(region.task)?.name
        );
    }

    Ok(())
}

pub fn init<'a, 'b>() -> (Command, App<'a, 'b>) {
    (
        Command::Attached {
            name: "map",
            archive: Archive::Required,
            attach: Attach::Any,
            validate: Validate::Booted,
            run: mapcmd,
        },
        MapArgs::clap(),
    )
}
