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
    stack: &[HubrisStackFrame],
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
        println!();
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
            println!();
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
    let task_count =
        core.read_word_32(hubris.lookup_symword("TASK_TABLE_SIZE")?)?;
    let ticks = core.read_word_64(hubris.lookup_variable("TICKS")?.addr)?;

    let task = hubris.lookup_struct_byname("Task")?;
    let taskdesc = hubris.lookup_struct_byname("TaskDesc")?;

    let entry_point = taskdesc.lookup_member("entry_point")?.offset as u32;
    let initial_stack = taskdesc.lookup_member("initial_stack")?.offset as u32;

    let state = task.lookup_member("state")?;
    let state_enum = hubris.lookup_enum(state.goff)?;

    let timer_state = task.lookup_member("timer")?;
    let timer_state_t = hubris.lookup_struct(timer_state.goff)?;
    let timer_state_deadline_m = timer_state_t.lookup_member("deadline")?;
    let timer_state_to_post_m = timer_state_t.lookup_member("to_post")?;
    let timer_state_deadline_opt_t =
        hubris.lookup_enum(timer_state_deadline_m.goff)?;

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
        taskblock
            .resize_with(task.size * task_count as usize, Default::default);
        core.read_8(base, taskblock.as_mut_slice())?;

        if !subargs.stack {
            core.run()?;
        }

        let descriptor = task.lookup_member("descriptor")?.offset as u32;
        let generation = task.lookup_member("generation")?.offset as u32;
        let priority = task.lookup_member("priority")?.offset as u32;
        let state = task.lookup_member("state")?.offset as u32;

        let taskblock32 =
            |o| u32::from_le_bytes(taskblock[o..o + 4].try_into().unwrap());

        println!("system time = {}", ticks);

        println!("{:2} {:15} {:3} {:3} {:9}",
            "ID", "TASK", "GEN", "PRI", "STATE");

        let mut any_names_truncated = false;
        for i in 0..task_count {
            let addr = base + i * task.size as u32;
            let offs = i as usize * task.size;
            let soffs = offs + state as usize;
            let toffs = offs + timer_state.offset as usize;

            let gen = taskblock[offs + generation as usize];
            let pri = taskblock[offs + priority as usize];
            let daddr = taskblock32(offs + descriptor as usize);
            let entry = core.read_word_32(daddr + entry_point)?;
            let limit = core.read_word_32(daddr + initial_stack)?;
            let module = hubris.instr_mod(entry).unwrap_or("<unknown>");

            let irqs = hubris.manifest.task_irqs.get(module);

            if let Some(ref task) = subargs.task {
                if task != module {
                    continue;
                }

                found = true;
            }

            let timer = {
                let timerblock = &taskblock[toffs..];
                let timerblock = &timerblock[..timer_state_t.size];
                let tstpm = timer_state_to_post_m.offset as usize;
                let to_post = u32::from_le_bytes(
                    timerblock[tstpm..tstpm + 4].try_into().unwrap(),
                );

                let tsdm = timer_state_deadline_m.offset as usize;
                let dv = timer_state_deadline_opt_t
                    .determine_variant(hubris, &timerblock[tsdm..])?;
                match &*dv.name {
                    "Some" => {
                        let anon_struct_t =
                            hubris.lookup_struct(dv.goff.unwrap())?;
                        let memb = anon_struct_t.lookup_member("__0")?;
                        let dlbytes = &timerblock[tsdm..];
                        let dlbytes = &dlbytes[memb.offset..];
                        let dl = u64::from_le_bytes(
                            dlbytes[..8].try_into().unwrap(),
                        );
                        Some((dl as i64 - ticks as i64, to_post))
                    }
                    _ => None,
                }
            };

            {
                let mut modname = module.to_string();
                if modname.len() > 14 {
                    modname.truncate(14);
                    modname.push('…');
                    any_names_truncated = true;
                }
                print!("{:2} {:15} {:3} {:3} ", i, modname, gen, pri);
            }
            explain_state(
                hubris,
                core,
                i,
                &taskblock[soffs..],
                state_enum,
                addr == cur,
                irqs,
                timer,
            )?;
            println!();

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
                println!();
            }
        }

        if any_names_truncated {
            println!("Note: task names were truncated to fit. Use \
                humility manifest to see them.");
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

fn explain_state(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    task_index: u32,
    buf: &[u8],
    state_enum: &HubrisEnum,
    current: bool,
    irqs: Option<&Vec<(u32, u32)>>,
    timer: Option<(i64, u32)>,
) -> Result<()> {
    let e = crate::reflect::load_enum(hubris, buf, state_enum, 0)?;
    match e.disc() {
        "Healthy" => {
            explain_sched_state(
                hubris,
                core,
                task_index,
                current,
                irqs,
                timer,
                &e.contents().unwrap().as_struct().unwrap()["__0"]
                    .as_enum()
                    .unwrap(),
            )?;
        }
        "Faulted" => {
            let contents = e.contents().unwrap().as_struct().unwrap();
            let fault = contents["fault"].as_enum().unwrap();
            let orig = contents["original_state"].as_enum().unwrap();

            explain_fault_info(hubris, core, task_index, fault)?;
            print!(" (was: ");
            explain_sched_state(
                hubris, core, task_index, current, irqs, timer, orig,
            )?;
            print!(")");
        }
        _ => print!("???"),
    }
    Ok(())
}

fn explain_sched_state(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    task_index: u32,
    current: bool,
    irqs: Option<&Vec<(u32, u32)>>,
    timer: Option<(i64, u32)>,
    e: &crate::reflect::Enum,
) -> Result<()> {
    match e.disc() {
        "Stopped" => print!("not started"),
        "Runnable" => {
            if current {
                print!("RUNNING")
            } else {
                print!("ready")
            }
        }
        "InSend" => {
            let task_id = &e.contents().unwrap().as_struct().unwrap()["__0"];
            let tid = task_id.as_struct().unwrap()["__0"]
                .as_base()
                .unwrap()
                .as_u16()
                .unwrap();
            if tid == 0xFFFF {
                print!("HALT: send to kernel");
            } else {
                print!("wait: send to 0x{:04x}", tid);
            }
        }
        "InReply" => {
            let task_id = &e.contents().unwrap().as_struct().unwrap()["__0"];
            let tid = task_id.as_struct().unwrap()["__0"]
                .as_base()
                .unwrap()
                .as_u16()
                .unwrap();
            print!("wait: reply from 0x{:04x}", tid);
        }
        "InRecv" => {
            let option_task_id =
                &e.contents().unwrap().as_struct().unwrap()["__0"];
            let tid =
                option_task_id.as_enum().unwrap().as_option().map(|task_id| {
                    task_id.as_struct().unwrap()["__0"]
                        .as_base()
                        .unwrap()
                        .as_u16()
                        .unwrap()
                });

            let r = hubris.registers(core, HubrisTask::Task(task_index))?;
            let notmask = *r.get(&ARMRegister::R6).unwrap();

            explain_recv(tid, notmask, irqs, timer);
        }
        _ => print!("???"),
    }
    Ok(())
}

fn explain_fault_info(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    task_index: u32,
    e: &crate::reflect::Enum,
) -> Result<()> {
    print!("FAULT: ");
    match e.disc() {
        "DivideByZero" => print!("divide by zero"),
        "IllegalText" => print!("jump to non-executable mem"),
        "IllegalInstruction" => print!("illegal instruction"),
        "InvalidOperation" => {
            let info = e.contents().unwrap().as_struct().unwrap()["__0"]
                .as_base()
                .unwrap()
                .as_u32()
                .unwrap();

            print!("general fault, cfsr=0x{:x}", info);
        }
        "StackOverflow" => {
            let addr = e.contents().unwrap().as_struct().unwrap()["address"]
                .as_base()
                .unwrap()
                .as_u32()
                .unwrap();

            print!("stack overflow; sp=0x{:x}", addr);
        }
        "Injected" => {
            let tid = e.contents().unwrap().as_struct().unwrap()["__0"]
                .as_struct()
                .unwrap()["__0"]
                .as_base()
                .unwrap()
                .as_u16()
                .unwrap();

            print!("killed by task 0x{:04x}", tid);
        }
        "MemoryAccess" => {
            let contents = e.contents().unwrap().as_struct().unwrap();
            let addr = contents["address"]
                .as_enum()
                .unwrap()
                .as_option()
                .map(|v| v.as_base().unwrap().as_u32().unwrap());

            print!("mem fault (");
            if let Some(addr) = addr {
                print!("precise: 0x{:x}", addr);
            } else {
                print!("imprecise");
            }
            print!(")");

            explain_fault_source(contents["source"].as_enum().unwrap())?;
        }
        "BusError" => {
            let contents = e.contents().unwrap().as_struct().unwrap();
            let addr = contents["address"]
                .as_enum()
                .unwrap()
                .as_option()
                .map(|v| v.as_base().unwrap().as_u32().unwrap());

            print!("bus fault (");
            if let Some(addr) = addr {
                print!("precise: 0x{:x}", addr);
            } else {
                print!("imprecise");
            }
            print!(")");

            explain_fault_source(contents["source"].as_enum().unwrap())?;
        }
        "SyscallUsage" => {
            print!("in syscall: ");
            explain_usage_error(
                e.contents().unwrap().as_struct().unwrap()["__0"]
                    .as_enum()
                    .unwrap(),
            )?;
        }
        "Panic" => {
            let r = hubris.registers(core, HubrisTask::Task(task_index))?;
            let msg_base = *r.get(&ARMRegister::R4).unwrap();
            let msg_len = *r.get(&ARMRegister::R5).unwrap();
            let msg_len = msg_len.min(255) as usize;
            let mut buf = vec![0; msg_len];
            core.read_8(msg_base, &mut buf)?;
            match std::str::from_utf8(&buf) {
                Ok(msg) => print!("{}", msg),
                Err(_) => print!("panic with invalid message"),
            }
        }
        _ => print!("???"),
    }
    Ok(())
}

fn explain_usage_error(e: &crate::reflect::Enum) -> Result<()> {
    match e.disc() {
        "BadSyscallNumber" => print!("undefined syscall number"),
        "InvalidSlice" => print!("sent malformed slice to kernel"),
        "TaskOutOfRange" => print!("used bogus task index"),
        "IllegalTask" => print!("illegal task operation"),
        "LeaseOutOfRange" => print!("bad caller lease index"),
        "OffsetOutOfRange" => print!("bad caller lease offset"),
        "NoIrq" => print!("referred to undefined interrupt"),
        "BadKernelMessage" => print!("sent nonsense IPC to kernel"),
        _ => print!("???"),
    }
    Ok(())
}

fn explain_fault_source(e: &crate::reflect::Enum) -> Result<()> {
    match e.disc() {
        "User" => print!(" in task code"),
        "Kernel" => print!(" in syscall"),
        _ => print!(" ???"),
    }
    Ok(())
}

/// Heuristic recognition of receive states used by normal programs.
///
/// We can print any receive state as a bunch of raw names and bits, but it's
/// often easier to read if common patterns are summarized.
///
/// Goals here include:
/// - Don't hide information - we should be able to exactly predict the state
///   representation from what's printed, even if it's pretty-printed.
///
/// - Make unusual cases obvious.
///
/// - Make common cases unobtrusive and easy to scan.
fn explain_recv(
    src: Option<u16>,
    notmask: u32,
    irqs: Option<&Vec<(u32, u32)>>,
    timer: Option<(i64, u32)>,
) {
    // Come up with a description for each notification bit.
    struct NoteInfo {
        irqs: Vec<u32>,
        timer: Option<i64>,
    }
    let mut note_types = vec![];
    for i in 0..32 {
        let bitmask = 1 << i;
        if notmask & bitmask == 0 {
            continue;
        }

        // Collect the IRQs that correspond to this enabled notification mask
        // bit.
        let irqnums = if let Some(irqs) = irqs {
            irqs.iter()
                .filter(|&&(m, _)| m == bitmask)
                .map(|&(_, n)| n)
                .collect::<Vec<_>>()
        } else {
            vec![]
        };
        let timer_assoc =
            timer.and_then(
                |(ts, mask)| if mask & bitmask != 0 { Some(ts) } else { None },
            );
        note_types.push(NoteInfo { irqs: irqnums, timer: timer_assoc });
    }

    // Display kernel receives as "wait" and others as "recv", noting the
    // explicit source for a closed receive.
    let mut outer_first = false;
    match src {
        Some(0xFFFF) => {
            outer_first = true;
        }
        Some(other) => {
            print!("recv(0x{:04x} only)", other);
        }
        None => {
            print!("recv");
        }
    }

    // Display notification bits, along with meaning where we can.
    if notmask != 0 {
        print!("{}notif:", if outer_first { "" } else { ", " });
        for (i, nt) in note_types.into_iter().enumerate() {
            print!(" bit{}", i);
            if !nt.irqs.is_empty() || nt.timer.is_some() {
                print!("(");
                let mut first = true;
                if let Some(ts) = nt.timer {
                    print!("T{:+}", ts);
                    first = false;
                }
                for irq in &nt.irqs {
                    print!("{}irq{}", if !first { "/" } else { "" }, irq);
                    first = false;
                }
                print!(")");
            }
        }
    }

    // Flag things that are probably bugs
    if src == Some(0xFFFF) && notmask == 0 {
        print!("(DEAD)");
    }
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
