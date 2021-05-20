/*
 * Copyright 2020 Oxide Computer Company
 */

use crate::cmd::*;
use crate::core::Core;
use crate::debug::*;
use crate::hubris::*;
use crate::Args;
use anyhow::{bail, Result};
use num_traits::FromPrimitive;
use std::collections::HashMap;
use std::convert::TryInto;
use structopt::clap::App;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(name = "tasks", about = "list Hubris tasks")]
struct TasksArgs {
    /// show registers
    #[structopt(long, short)]
    registers: bool,

    /// show stack backtrace
    #[structopt(long, short)]
    stack: bool,

    /// show line number information with stack backtrace
    #[structopt(long, short, requires = "stack")]
    line: bool,

    /// spin pulling tasks
    #[structopt(long, short = "S")]
    spin: bool,

    /// verbose task output
    #[structopt(long, short)]
    verbose: bool,

    /// single task to display
    task: Option<String>,
}

fn print_stack(
    hubris: &HubrisArchive,
    stack: &Vec<HubrisStackFrame>,
    subargs: &TasksArgs,
) {
    let additional = subargs.registers || subargs.verbose;
    let bar = if additional { "|" } else { " " };

    print!("   |\n   +--->  ");

    for i in 0..stack.len() {
        let frame = &stack[i];
        let pc = frame.registers.get(&ARMRegister::PC).unwrap();

        if let Some(ref inlined) = frame.inlined {
            for inline in inlined {
                println!(
                    "0x{:08x} 0x{:08x} {}()",
                    frame.cfa, inline.addr, inline.name
                );
                print!("   {}      ", bar);

                if subargs.line {
                    if let Some(src) = hubris.lookup_src(inline.origin) {
                        println!("{:11}@ {}:{}", "", src.fullpath(), src.line);
                        print!("   {}      ", bar);
                    }
                }
            }
        }

        if let Some(sym) = frame.sym {
            println!("0x{:08x} 0x{:08x} {}()", frame.cfa, *pc, sym.name);

            if subargs.line {
                if let Some(src) = hubris.lookup_src(sym.goff) {
                    print!("   {}      ", bar);
                    println!("{:11}@ {}:{}", "", src.fullpath(), src.line);
                }
            }
        } else {
            println!("0x{:08x} 0x{:08x}", frame.cfa, *pc);
        }

        if i + 1 < stack.len() {
            print!("   {}      ", bar);
        }
    }

    if additional {
        println!("   {}", bar);
    } else {
        println!("");
    }
}

fn print_regs(regs: &HashMap<ARMRegister, u32>, additional: bool) {
    let bar = if additional { "|" } else { " " };

    print!("   |\n   +--->");

    for r in 0..16 {
        let reg = ARMRegister::from_usize(r).unwrap();

        if r != 0 && r % 4 == 0 {
            print!("   {}    ", bar);
        }

        print!("  {:>3} = 0x{:08x}", reg, regs.get(&reg).unwrap());

        if r % 4 == 3 {
            println!("");
        }
    }
}

#[rustfmt::skip::macros(println)]

fn tasks(
    hubris: &mut HubrisArchive,
    core: &mut dyn Core,
    _args: &Args,
    subargs: &Vec<String>,
) -> Result<()> {
    let subargs = TasksArgs::from_iter_safe(subargs)?;

    let base = core.read_word_32(hubris.lookup_symword("TASK_TABLE_BASE")?)?;
    let size = core.read_word_32(hubris.lookup_symword("TASK_TABLE_SIZE")?)?;

    let task = hubris.lookup_struct_byname("Task")?;
    let taskdesc = hubris.lookup_struct_byname("TaskDesc")?;

    let entry_point = taskdesc.lookup_member("entry_point")?.offset as u32;
    let initial_stack = taskdesc.lookup_member("initial_stack")?.offset as u32;

    let state = task.lookup_member("state")?;
    let state_enum = hubris.lookup_enum(state.goff)?;

    let mut found = false;

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

        if !subargs.stack {
            core.run()?;
        }

        let descriptor = task.lookup_member("descriptor")?.offset as u32;
        let generation = task.lookup_member("generation")?.offset as u32;
        let priority = task.lookup_member("priority")?.offset as u32;
        let state = task.lookup_member("state")?.offset as u32;

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
            let limit = core.read_word_32(daddr + initial_stack)?;
            let module = hubris.instr_mod(entry).unwrap_or("<unknown>");

            if let Some(ref task) = subargs.task {
                if task != module {
                    continue;
                }

                found = true;
            }

            println!("{:2} {:08x} {:18} {:3} {:3} {:25} {}", i, addr, module,
                gen, pri, hubris.print(&taskblock[soffs..], state_enum.goff)?,
                if addr == cur { " <-" } else { "" });

            if subargs.stack || subargs.registers {
                let t = HubrisTask::Task(i);
                let regs = hubris.registers(core, t)?;

                if subargs.stack {
                    let stack = hubris.stack(core, t, limit, &regs)?;
                    print_stack(hubris, &stack, &subargs);
                }

                if subargs.registers {
                    print_regs(&regs, subargs.verbose);
                }
            }

            if subargs.verbose {
                let fmt =
                    HubrisPrintFormat { indent: 16, newline: true, hex: true };

                println!(
                    "   |\n   +-----------> {}\n",
                    hubris.printfmt(&taskblock[offs..], task.goff, &fmt)?
                );
            }

            if subargs.registers && !subargs.verbose {
                println!("");
            }
        }

        if subargs.stack {
            core.run()?;
        }

        if subargs.task.is_some() && !found {
            bail!("\"{}\" is not a valid task", subargs.task.unwrap());
        }

        if !subargs.spin {
            break;
        }
    }

    Ok(())
}

pub fn init<'a, 'b>() -> (Command, App<'a, 'b>) {
    (
        Command::Attached {
            name: "tasks",
            archive: Archive::Required,
            attach: Attach::Any,
            validate: Validate::Booted,
            run: tasks,
        },
        TasksArgs::clap(),
    )
}
