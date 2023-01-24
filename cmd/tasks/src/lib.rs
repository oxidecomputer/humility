// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility tasks`
//!
//! `humility tasks` offers a ps-like view of a system, e.g.:
//!
//! ```console
//! % humility tasks
//! humility: attached via ST-Link
//! system time = 1764993
//! ID TASK                 GEN PRI STATE    
//!  0 jefe                   0   0 recv, notif: bit0 bit1(T+7)
//!  1 rcc_driver             0   1 recv
//!  2 gpio_driver            0   2 recv
//!  3 usart_driver           0   2 recv, notif: bit0(irq39)
//!  4 i2c_driver             0   2 recv
//!  5 spi_driver             0   2 recv
//!  6 user_leds              0   2 recv
//!  7 pong                   0   3 FAULT: killed by jefe/gen0 (was: recv, notif: bit0)
//!  8 ping               14190   4 wait: send to pong/gen0
//!  9 hiffy                  0   3 notif: bit0(T+7)
//! 10 hf                     0   3 notif: bit0(T+18)
//! 11 idle                   0   5 RUNNING
//! ```
//!
//! To see every field in each task, you can use the `-v` flag:
//!
//! ```console
//! % humility -d hubris.core.4 tasks -v
//! humility: attached to dump
//! system time = 1791860
//! ID TASK                 GEN PRI STATE    
//! ...
//!  7 pong                   0   3 FAULT: killed by jefe/gen0 (was: recv, notif: bit0)
//!    |
//!    +-----------> Task {
//!                     save: SavedState {
//!                         r4: 0x200063c4,
//!                         r5: 0x10,
//!                         r6: 0x1,
//!                         r7: 0x0,
//!                         r8: 0x60003,
//!                         r9: 0x4,
//!                         r10: 0x200063d4,
//!                         r11: 0x1,
//!                         psp: 0x20006330,
//!                         exc_return: 0xffffffed,
//!                         ...
//!                     },
//!                     priority: Priority(0x3),
//!                     state: Faulted {
//!                         fault: Injected(TaskId(0x0)),
//!                         original_state: InRecv(None)
//!                     },
//!                     ...
//! ...
//! ```
//!
//! To see a task's registers, use the `-r` flag:
//!
//! ```console
//! % humility tasks -r user_leds
//! humility: attached via ST-Link
//! system time = 1990498
//! ID TASK                 GEN PRI STATE    
//!  6 user_leds              0   2 recv
//!    |
//!    +--->   R0 = 0x20005fc8   R1 = 0x0000000c   R2 = 0x00000000   R3 = 0x20005fd8
//!            R4 = 0x20005fc8   R5 = 0x0000000c   R6 = 0x00000000   R7 = 0x00000000
//!            R8 = 0x08027154   R9 = 0x00000000  R10 = 0xfffffe00  R11 = 0x00000001
//!           R12 = 0x00000000   SP = 0x20005fa0   LR = 0x08026137   PC = 0x08026e42
//! ```
//!
//! To see a task's stack backtrace, use the `-s` flag:
//!
//! ```console
//! % humility tasks -s user_leds
//! humility: attached via ST-Link
//! system time = 2021382
//! ID TASK                 GEN PRI STATE    
//!  6 user_leds              0   2 recv
//!    |
//!    +--->  0x20005fc0 0x08026e42 userlib::sys_recv_stub
//!           0x20006000 0x08026128 userlib::sys_recv
//!           0x20006000 0x08026128 idol_runtime::dispatch
//!           0x20006000 0x08026136 main
//! ```
//!
//! To additionally see line number information on a stack backtrace, also provide
//! `-l` flag:
//!
//! ```console
//! % humility tasks -sl user_leds
//! humility: attached via ST-Link
//! system time = 2049587
//! ID TASK                 GEN PRI STATE    
//!  6 user_leds              0   2 recv
//!    |
//!    +--->  0x20005fc0 0x08026e42 userlib::sys_recv_stub
//!                      @ /home/bmc/hubris/sys/userlib/src/lib.rs:288
//!           0x20006000 0x08026128 userlib::sys_recv
//!                      @ /home/bmc/hubris/sys/userlib/src/lib.rs:236
//!           0x20006000 0x08026128 idol_runtime::dispatch
//!                      @ /home/bmc/.cargo/git/checkouts/idolatry-1ebf1c2fd2f30300/6d18e14/runtime/src/lib.rs:137
//!           0x20006000 0x08026136 main
//!                      @ /home/bmc/hubris/drv/user-leds/src/main.rs:110
//! ```
//!
//! These options can naturally be combined, e.g. `humility tasks -slvr`.
//!

use anyhow::{bail, Context, Result};
use clap::{CommandFactory, Parser};
use humility::arch::ARMRegister;
use humility::cli::Subcommand;
use humility::core::Core;
use humility::hubris::*;
use humility::reflect::{self, Format, Load};
use humility_cmd::doppel::{self, Task, TaskDesc, TaskId, TaskState};
use humility_cmd::{Archive, Attach, Command, CommandKind, Validate};
use num_traits::FromPrimitive;
use std::collections::{BTreeMap, HashMap};

#[derive(Parser, Debug)]
#[clap(name = "tasks", about = env!("CARGO_PKG_DESCRIPTION"))]
struct TasksArgs {
    /// show registers
    #[clap(long, short)]
    registers: bool,

    /// show stack backtrace
    #[clap(long, short)]
    stack: bool,

    /// show line number information with stack backtrace
    #[clap(long, short, requires = "stack")]
    line: bool,

    /// spin pulling tasks
    #[clap(long, short = 'S')]
    spin: bool,

    /// verbose task output
    #[clap(long, short)]
    verbose: bool,

    /// single task to display
    task: Option<String>,
}

fn print_regs(regs: &BTreeMap<ARMRegister, u32>, additional: bool) {
    let bar = if additional { "|" } else { " " };

    print!("   |\n   +--->");

    for r in 0..=16 {
        let reg = ARMRegister::from_usize(r).unwrap();

        if r != 0 && r % 4 == 0 {
            print!("   {}    ", bar);
        }

        print!("  {:>3} = 0x{:08x}", reg, regs.get(&reg).unwrap());

        if r % 4 == 3 {
            println!();
        }
    }

    println!();
}

#[rustfmt::skip::macros(println)]
fn tasks(context: &mut humility::ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let hubris = context.archive.as_ref().unwrap();

    let subargs = TasksArgs::try_parse_from(subargs)?;

    let (base, task_count) = hubris.task_table(core)?;
    log::debug!("task table: {:#x?}, count: {}", base, task_count);
    let ticks = core.read_word_64(hubris.lookup_variable("TICKS")?.addr)?;

    let task_t = hubris.lookup_struct_byname("Task")?;
    let save = task_t.lookup_member("save")?.offset;
    let state = hubris.lookup_struct_byname("SavedState")?;
    let r4 = save + state.lookup_member("r4")?.offset;

    let mut found = false;

    let printer = humility_cmd::stack::StackPrinter {
        indent: 3,
        line: subargs.line,
        additional: subargs.registers || subargs.verbose,
    };

    loop {
        core.halt()?;

        let cur =
            core.read_word_32(hubris.lookup_symword("CURRENT_TASK_PTR")?)?;

        //
        // We read the entire task table at a go to get as consistent a
        // snapshot as possible.
        //
        let mut taskblock = vec![0; task_t.size * task_count as usize];
        core.read_8(base, &mut taskblock)?;

        if let Some(epitaph) = hubris.epitaph(core)? {
            humility::warn!("kernel has panicked: {}", epitaph);
        }

        let mut tasks = vec![];
        let mut panicked = false;
        let mut regs = HashMap::new();

        for i in 0..task_count {
            let addr = base + i * task_t.size as u32;
            let offs = i as usize * task_t.size;

            let task_value: reflect::Value =
                reflect::load(hubris, &taskblock, task_t, offs).with_context(
                    || format!("loading task control block for task {}", i),
                )?;
            let task: Task = Task::from_value(&task_value)?;

            //
            // Always load R4, R5 and R6, which are in the saved state in our
            // task structure (and are needed to print state).
            //
            for r in 4..=6 {
                let o = offs + r4 + (r - 4) * 4;
                let v =
                    u32::from_le_bytes(taskblock[o..o + 4].try_into().unwrap());
                regs.insert((i, ARMRegister::from_usize(r).unwrap()), v);
            }

            tasks.push((addr, task_value, task));

            if let TaskState::Faulted { fault, .. } = task.state {
                if fault == doppel::FaultInfo::Panic {
                    panicked = true;
                }
            }
        }

        let keep_halted = subargs.stack || subargs.registers || panicked;

        if !keep_halted {
            core.run()?;
        }

        println!("system time = {}", ticks);

        println!("{:2} {:21} {:>8} {:3} {:9}",
            "ID", "TASK", "GEN", "PRI", "STATE");

        let mut any_names_truncated = false;

        for (i, (addr, task_value, task)) in tasks.iter().enumerate() {
            let i = i as u32;
            let desc: TaskDesc = task.descriptor.load_from(hubris, core)?;

            let module = match hubris.lookup_module(HubrisTask::Task(i)) {
                Ok(m) => &m.name,
                _ => "<unknown>",
            };

            let irqs = hubris.manifest.task_irqs.get(module);

            if let Some(ref task) = subargs.task {
                if *task != module {
                    if let Ok(task_addr) = parse_int::parse::<u32>(task) {
                        if *addr != task_addr {
                            continue;
                        }
                    } else {
                        continue;
                    }
                }

                found = true;
            }

            let timer = task.timer.deadline.map(|deadline| {
                (deadline.0 as i64 - ticks as i64, task.timer.to_post.0)
            });

            {
                let mut modname = module.to_string();
                if modname.len() > 20 {
                    modname.truncate(20);
                    modname.push('…');
                    any_names_truncated = true;
                }
                print!(
                    "{:2} {:21} {:>8} {:3} ",
                    i,
                    modname,
                    u32::from(task.generation),
                    task.priority.0
                );
            }
            explain_state(
                hubris,
                core,
                i,
                &regs,
                task.state,
                *addr == cur,
                irqs,
                timer,
            )?;
            println!();

            if subargs.stack || subargs.registers {
                let t = HubrisTask::Task(i);
                let regs = hubris.registers(core, t)?;

                if subargs.stack {
                    match hubris.stack(core, t, desc.initial_stack, &regs) {
                        Ok(stack) => printer.print(hubris, &stack),
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

                print!("   |\n   +-----------> {:#08x} ", addr);
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

        if keep_halted {
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

#[allow(clippy::too_many_arguments)]
fn explain_state(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    task_index: u32,
    regs: &HashMap<(u32, ARMRegister), u32>,
    ts: TaskState,
    current: bool,
    irqs: Option<&Vec<(u32, u32)>>,
    timer: Option<(i64, u32)>,
) -> Result<()> {
    match ts {
        TaskState::Healthy(ss) => {
            explain_sched_state(
                hubris, task_index, regs, current, irqs, timer, ss,
            )?;
        }
        TaskState::Faulted { fault, original_state } => {
            explain_fault_info(hubris, core, task_index, regs, fault)?;
            print!(" (was: ");
            explain_sched_state(
                hubris,
                task_index,
                regs,
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
    task_index: u32,
    regs: &HashMap<(u32, ARMRegister), u32>,
    current: bool,
    irqs: Option<&Vec<(u32, u32)>>,
    timer: Option<(i64, u32)>,
    e: doppel::SchedState,
) -> Result<()> {
    use doppel::SchedState;

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
            let notmask = *regs.get(&(task_index, ARMRegister::R6)).unwrap();
            explain_recv(hubris, task_index, tid, notmask, irqs, timer);
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
    regs: &HashMap<(u32, ARMRegister), u32>,
    fi: doppel::FaultInfo,
) -> Result<()> {
    use doppel::FaultInfo;

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
            let msg_base = *regs.get(&(task_index, ARMRegister::R4)).unwrap();
            let msg_len = *regs.get(&(task_index, ARMRegister::R5)).unwrap();
            let msg_len = msg_len.min(255) as usize;
            let mut buf = vec![0; msg_len];
            core.read_8(msg_base, &mut buf)?;
            match std::str::from_utf8(&buf) {
                Ok(msg) => print!("{}", msg),
                Err(_) => print!("panic with invalid message"),
            }
        }
        FaultInfo::FromServer(task_id, reason) => {
            print!("reply fault: task id {}, reason {:?}", task_id, reason);
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
    task_index: u32,
    src: Option<TaskId>,
    notmask: u32,
    irqs: Option<&Vec<(u32, u32)>>,
    timer: Option<(i64, u32)>,
) {
    // Come up with a description for each notification bit.
    struct NoteInfo {
        irqs: Vec<u32>,
        timer: Option<i64>,
        bit: usize,
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
        note_types.push(NoteInfo { irqs: irqnums, timer: timer_assoc, bit: i });
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

    let task_mod = hubris.lookup_module(HubrisTask::Task(task_index));
    let notification_names = if let Ok(task_mod) = task_mod {
        hubris.manifest.task_notifications.get(&task_mod.name)
    } else {
        None
    };
    let notification_name = |s: usize| -> Option<&str> {
        notification_names.and_then(|n| n.get(s)).map(|s| s.as_str())
    };

    // Display notification bits, along with meaning where we can.
    if notmask != 0 {
        print!("{}notif:", if outer_first { "" } else { ", " });
        for nt in note_types {
            let name = notification_name(nt.bit);
            if let Some(name) = name {
                print!(" {name}");
            } else {
                print!(" bit{}", nt.bit);
            }
            if !nt.irqs.is_empty() || nt.timer.is_some() {
                print!("(");
                let mut first = true;
                if let Some(ts) = nt.timer {
                    print!("{}T{:+}", if !first { "/" } else { "" }, ts);
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

pub fn init() -> Command {
    Command {
        app: TasksArgs::command(),
        name: "tasks",
        run: tasks,
        kind: CommandKind::Attached {
            archive: Archive::Required,
            attach: Attach::Any,
            validate: Validate::Booted,
        },
    }
}
