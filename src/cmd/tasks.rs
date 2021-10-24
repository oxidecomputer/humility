/*
 * Copyright 2020 Oxide Computer Company
 */

use crate::cmd::*;
use crate::core::Core;
use crate::debug::*;
use crate::doppel::{self, Task, TaskDesc, TaskId, TaskState};
use crate::hubris::*;
use crate::reflect::{self, Format, Load};
use crate::Args;
use anyhow::{bail, Result};
use num_traits::FromPrimitive;
use std::collections::HashMap;
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
                    "0x{:08x} 0x{:08x} {}",
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
            println!(
                "0x{:08x} 0x{:08x} {}",
                frame.cfa, *pc, sym.demangled_name
            );

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

    let task_t = hubris.lookup_struct_byname("Task")?;

    let mut found = false;

    loop {
        core.halt()?;

        let cur =
            core.read_word_32(hubris.lookup_symword("CURRENT_TASK_PTR")?)?;

        /*
         * We read the entire task table at a go to get as consistent a
         * snapshot as possible.
         */
        let mut taskblock = vec![0; task_t.size * task_count as usize];
        core.read_8(base, &mut taskblock)?;

        if !subargs.stack {
            core.run()?;
        }

        println!("system time = {}", ticks);

        println!("{:2} {:15} {:3} {:3} {:9}",
            "ID", "TASK", "GEN", "PRI", "STATE");

        let mut any_names_truncated = false;
        for i in 0..task_count {
            let addr = base + i * task_t.size as u32;
            let offs = i as usize * task_t.size;

            let task_value: reflect::Value =
                reflect::load(hubris, &taskblock, task_t, offs)?;
            let task: Task = Task::from_value(&task_value)?;
            let desc: TaskDesc = task.descriptor.load_from(hubris, core)?;
            let module =
                hubris.instr_mod(desc.entry_point).unwrap_or("<unknown>");

            let irqs = hubris.manifest.task_irqs.get(module);

            if let Some(ref task) = subargs.task {
                if task != module {
                    continue;
                }

                found = true;
            }

            let timer = if let Some(deadline) = task.timer.deadline {
                Some((deadline.0 as i64 - ticks as i64, task.timer.to_post.0))
            } else {
                None
            };

            {
                let mut modname = module.to_string();
                if modname.len() > 14 {
                    modname.truncate(14);
                    modname.push('…');
                    any_names_truncated = true;
                }
                print!(
                    "{:2} {:15} {:3} {:3} ",
                    i, modname, task.generation.0, task.priority.0
                );
            }
            explain_state(
                hubris,
                core,
                i,
                task.state,
                addr == cur,
                irqs,
                timer,
            )?;
            println!();

            if subargs.stack || subargs.registers {
                let t = HubrisTask::Task(i);
                let regs = hubris.registers(core, t)?;

                if subargs.stack {
                    match hubris.stack(core, t, desc.initial_stack, &regs) {
                        Ok(stack) => print_stack(hubris, &stack, &subargs),
                        Err(e) => {
                            println!("   stack unwind failed: {:?} ", e);
                        }
                    }
                }

                if subargs.registers {
                    print_regs(&regs, subargs.verbose);
                }
            }

            if subargs.verbose {
                let fmt = HubrisPrintFormat {
                    indent: 16,
                    newline: true,
                    hex: true,
                    ..HubrisPrintFormat::default()
                };

                print!("   |\n   +-----------> ");
                task_value.format(hubris, fmt, &mut std::io::stdout())?;
                println!("\n");
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
    ts: TaskState,
    current: bool,
    irqs: Option<&Vec<(u32, u32)>>,
    timer: Option<(i64, u32)>,
) -> Result<()> {
    match ts {
        TaskState::Healthy(ss) => {
            explain_sched_state(
                hubris, core, task_index, current, irqs, timer, ss,
            )?;
        }
        TaskState::Faulted { fault, original_state } => {
            explain_fault_info(hubris, core, task_index, fault)?;
            print!(" (was: ");
            explain_sched_state(
                hubris,
                core,
                task_index,
                current,
                irqs,
                timer,
                original_state,
            )?;
            print!(")");
        }
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
    e: doppel::SchedState,
) -> Result<()> {
    use crate::doppel::SchedState;

    match e {
        SchedState::Stopped => print!("not started"),
        SchedState::Runnable => {
            if current {
                print!("RUNNING")
            } else {
                print!("ready")
            }
        }
        SchedState::InSend(tid) => {
            if tid == TaskId::KERNEL {
                print!("HALT: send to kernel");
            } else {
                print!("wait: send to ");
                print_task_id(hubris, tid);
            }
        }
        SchedState::InReply(tid) => {
            print!("wait: reply from ");
            print_task_id(hubris, tid);
        }
        SchedState::InRecv(tid) => {
            let r = hubris.registers(core, HubrisTask::Task(task_index))?;
            let notmask = *r.get(&ARMRegister::R6).unwrap();

            explain_recv(hubris, tid, notmask, irqs, timer);
        }
    }
    Ok(())
}

fn print_task_id(hubris: &HubrisArchive, task_id: TaskId) {
    if let Some(n) = hubris.task_name(task_id.index()) {
        print!("{}/gen{}", n, task_id.generation());
    } else {
        print!("unknown#{}/gen{}", task_id.index(), task_id.generation());
    }
}

fn explain_fault_info(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    task_index: u32,
    fi: crate::doppel::FaultInfo,
) -> Result<()> {
    use crate::doppel::FaultInfo;

    print!("FAULT: ");
    match fi {
        FaultInfo::DivideByZero => print!("divide by zero"),
        FaultInfo::IllegalText => print!("jump to non-executable mem"),
        FaultInfo::IllegalInstruction => print!("illegal instruction"),
        FaultInfo::InvalidOperation(bits) => {
            print!("general fault, cfsr=0x{:x}", bits);
        }
        FaultInfo::StackOverflow { address } => {
            print!("stack overflow; sp=0x{:x}", address);
        }
        FaultInfo::Injected(task) => {
            print!("killed by ");
            print_task_id(hubris, task);
        }
        FaultInfo::MemoryAccess { address, source } => {
            print!("mem fault (");
            if let Some(addr) = address {
                print!("precise: 0x{:x}", addr);
            } else {
                print!("imprecise");
            }
            print!(")");

            explain_fault_source(source);
        }
        FaultInfo::BusError { address, source } => {
            print!("bus fault (");
            if let Some(addr) = address {
                print!("precise: 0x{:x}", addr);
            } else {
                print!("imprecise");
            }
            print!(")");

            explain_fault_source(source);
        }
        FaultInfo::SyscallUsage(ue) => {
            print!("in syscall: ");
            explain_usage_error(ue);
        }
        FaultInfo::Panic => {
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
    }
    Ok(())
}

fn explain_usage_error(e: doppel::UsageError) {
    use doppel::UsageError::*;
    match e {
        BadSyscallNumber => print!("undefined syscall number"),
        InvalidSlice => print!("sent malformed slice to kernel"),
        TaskOutOfRange => print!("used bogus task index"),
        IllegalTask => print!("illegal task operation"),
        LeaseOutOfRange => print!("bad caller lease index"),
        OffsetOutOfRange => print!("bad caller lease offset"),
        NoIrq => print!("referred to undefined interrupt"),
        BadKernelMessage => print!("sent nonsense IPC to kernel"),
    }
}

fn explain_fault_source(e: doppel::FaultSource) {
    match e {
        doppel::FaultSource::User => print!(" in task code"),
        doppel::FaultSource::Kernel => print!(" in syscall"),
    }
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
    hubris: &HubrisArchive,
    src: Option<TaskId>,
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
        Some(TaskId::KERNEL) => {
            outer_first = true;
        }
        Some(other) => {
            print!("recv(");
            print_task_id(hubris, other);
            print!(" only)");
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
    if src == Some(TaskId::KERNEL) && notmask == 0 {
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
