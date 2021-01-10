/*
 * Copyright 2020 Oxide Computer Company
 */

use crate::attach;
use crate::cmd::{Archive, HumilityCommand};
use crate::hubris::{HubrisArchive, HubrisPrintFormat};
use crate::Args;
use anyhow::Result;
use std::convert::TryInto;
use structopt::clap::App;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(name = "tasks", about = "list Hubris tasks")]
struct TasksArgs {
    /// spin pulling tasks
    #[structopt(long, short)]
    spin: bool,
    /// verbose task output
    #[structopt(long, short)]
    verbose: bool,
}

#[rustfmt::skip::macros(println)]

fn tasks(
    hubris: &HubrisArchive,
    args: &Args,
    subargs: &Vec<String>,
) -> Result<()> {
    let subargs = TasksArgs::from_iter_safe(subargs)?;
    let mut core = attach(&args)?;

    hubris.validate(core.as_mut())?;

    let base = core.read_word_32(hubris.lookup_symword("TASK_TABLE_BASE")?)?;
    let size = core.read_word_32(hubris.lookup_symword("TASK_TABLE_SIZE")?)?;

    let task = hubris.lookup_struct_byname("Task")?;
    let taskdesc = hubris.lookup_struct_byname("TaskDesc")?;

    let state = task.lookup_member("state")?;
    let state_enum = hubris.lookup_enum(state.goff)?;

    loop {
        core.halt()?;

        let cur =
            core.read_word_32(hubris.lookup_symword("CURRENT_TASK_PTR")?)?;

        /*
         * We read the entire task table at a go to get as consistent a
         * snapshot as possible.
         */
        let mut taskblock: Vec<u8> = vec![];
        taskblock.resize_with(task.size * size as usize, Default::default);
        core.read_8(base, taskblock.as_mut_slice())?;

        core.run()?;

        let descriptor = task.lookup_member("descriptor")?.offset as u32;
        let generation = task.lookup_member("generation")?.offset as u32;
        let priority = task.lookup_member("priority")?.offset as u32;
        let state = task.lookup_member("state")?.offset as u32;

        let entry_point = taskdesc.lookup_member("entry_point")?.offset as u32;

        println!("{:2} {:8} {:18} {:3} {:3} {:9}",
            "ID", "ADDR", "TASK", "GEN", "PRI", "STATE");

        let taskblock32 =
            |o| u32::from_le_bytes(taskblock[o..o + 4].try_into().unwrap());

        for i in 0..size {
            let addr = base + i * task.size as u32;
            let offs = i as usize * task.size;
            let soffs = offs + state as usize;

            let gen = taskblock[offs + generation as usize];
            let pri = taskblock[offs + priority as usize];
            let daddr = taskblock32(offs + descriptor as usize);
            let entry = core.read_word_32(daddr + entry_point)?;
            let module = hubris.instr_mod(entry).unwrap_or("<unknown>");

            println!("{:2} {:08x} {:18} {:3} {:3} {:25} {}", i, addr, module,
                gen, pri, hubris.print(&taskblock[soffs..], state_enum.goff)?,
                if addr == cur { " <-" } else { "" });

            if subargs.verbose {
                let fmt =
                    HubrisPrintFormat { indent: 16, newline: true, hex: true };

                println!(
                    "          |\n          +----> {}\n",
                    hubris.printfmt(&taskblock[offs..], task.goff, &fmt)?
                );
            }
        }

        if !subargs.spin {
            break;
        }
    }

    Ok(())
}

pub fn init<'a, 'b>() -> (HumilityCommand, App<'a, 'b>) {
    (
        HumilityCommand {
            name: "tasks",
            archive: Archive::Required,
            run: tasks,
        },
        TasksArgs::clap(),
    )
}
