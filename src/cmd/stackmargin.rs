/*
 * Copyright 2020 Oxide Computer Company
 */

use crate::cmd::*;
use crate::core::Core;
use crate::hubris::*;
use crate::Args;
use anyhow::{bail, Result};
use std::convert::TryInto;
use structopt::clap::App;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(
    name = "stackmargin",
    about = "calculate and print stack margins by task"
)]
struct StackmarginArgs {}

#[rustfmt::skip::macros(println, bail)]
fn stackmargin(
    hubris: &mut HubrisArchive,
    core: &mut dyn Core,
    _args: &Args,
    _subargs: &Vec<String>,
) -> Result<()> {
    let regions = hubris.regions(core)?;

    let base = core.read_word_32(hubris.lookup_symword("TASK_TABLE_BASE")?)?;
    let size = core.read_word_32(hubris.lookup_symword("TASK_TABLE_SIZE")?)?;

    let task = hubris.lookup_struct_byname("Task")?;
    let taskdesc = hubris.lookup_struct_byname("TaskDesc")?;

    let mut taskblock: Vec<u8> = vec![];
    taskblock.resize_with(task.size * size as usize, Default::default);
    core.read_8(base, taskblock.as_mut_slice())?;

    let descriptor = task.lookup_member("descriptor")?.offset as u32;
    let initial_stack = taskdesc.lookup_member("initial_stack")?.offset as u32;

    println!("{:2} {:18} {:>10} {:>10} {:>10} {:>10}",
        "ID", "TASK", "STACKBASE", "STACKSIZE", "MAXDEPTH", "MARGIN");

    let taskblock32 =
        |o| u32::from_le_bytes(taskblock[o..o + 4].try_into().unwrap());

    let find = |addr| {
        for (_, region) in regions.iter() {
            if addr > region.base && addr <= region.base + region.mapsize {
                return Ok(region);
            }
        }

        bail!(format!("could not find region for address {:x}", addr));
    };

    for i in 0..size {
        let offs = i as usize * task.size;
        let daddr = taskblock32(offs + descriptor as usize);
        let initial = core.read_word_32(daddr + initial_stack)?;

        let module = hubris.lookup_task(HubrisTask::Task(i))?;
        let region = find(initial)?;

        if region.task != module.task {
            bail!(format!(
                "mismatched task on 0x{:x}: expected {:?}, found {:?}",
                initial, module.task, region.task)
            )
        }

        let size = (initial - region.base) as usize;
        let mut stack: Vec<u8> = vec![];
        stack.resize_with(size, Default::default);
        core.read_8(region.base, stack.as_mut_slice())?;

        let mut o = 0;

        let depth = loop {
            let c = u32::from_le_bytes(stack[o..o + 4].try_into().unwrap());

            if c != 0xbaddcafe || o + 4 >= size {
                break size - o;
            }

            o += 4;
        };

        println!("{:2} {:18} 0x{:<8x} {:10} {:10} {:10}",
            i, module.name, region.base,
            size, depth, size - depth);
    }

    Ok(())
}

pub fn init<'a, 'b>() -> (Command, App<'a, 'b>) {
    (
        Command::Attached {
            name: "stackmargin",
            archive: Archive::Required,
            attach: Attach::Any,
            validate: Validate::Booted,
            run: stackmargin,
        },
        StackmarginArgs::clap(),
    )
}
