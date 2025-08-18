// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility tasks`
//!
//! `humility tasks` offers a ps-like view of a system, e.g.:
//!
//! ```console
//! $ humility tasks
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
//! $ humility -d hubris.core.4 tasks -v
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
//! $ humility tasks -r user_leds
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
//! $ humility tasks -s user_leds
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
//! $ humility tasks -sl user_leds
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

use anyhow::{Context, Result, bail};
use clap::{CommandFactory, Parser};
use humility::core::Core;
use humility::hubris::*;
use humility::reflect::{self, Format, Load};
use humility_arch_arm::ARMRegister;
use humility_cli::{ExecutionContext, Subcommand};
use humility_cmd::{Archive, Attach, Command, CommandKind, Validate};
use humility_doppel::{self as doppel, Task, TaskDesc, TaskId, TaskState};
use num_traits::FromPrimitive;
use std::collections::{BTreeMap, HashMap};
use std::io::Write;

#[derive(Parser, Debug)]
#[clap(name = "tasks", about = env!("CARGO_PKG_DESCRIPTION"))]
struct TasksArgs {
    /// show registers
    #[clap(long, short)]
    registers: bool,

    /// show stack backtrace
    #[clap(long, short)]
    stack: bool,

    /// use saved frame pointer to guess at stack trace
    #[clap(long, requires = "stack")]
    guess: bool,

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

fn print_regs(
    w: &mut dyn Write,
    regs: &BTreeMap<ARMRegister, u32>,
    additional: bool,
) -> Result<()> {
    let bar = if additional { "|" } else { " " };

    write!(w, "   |\n   +--->")?;

    for r in 0..=16 {
        let reg = ARMRegister::from_usize(r).unwrap();

        if r != 0 && r % 4 == 0 {
            write!(w, "   {}    ", bar)?;
        }

        write!(w, "  {:>3} = 0x{:08x}", reg, regs.get(&reg).unwrap())?;

        if r % 4 == 3 {
            writeln!(w)?;
        }
    }

    writeln!(w)?;

    Ok(())
}

fn tasks(context: &mut ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let hubris = context.archive.as_ref().unwrap();

    let subargs = TasksArgs::try_parse_from(subargs)?;

    print_tasks(
        &mut std::io::stdout(),
        core,
        hubris,
        subargs.registers,
        subargs.stack,
        subargs.line,
        subargs.guess,
        subargs.spin,
        subargs.verbose,
        subargs.task,
    )
}
#[rustfmt::skip::macros(println)]
#[allow(clippy::too_many_arguments)]
pub fn print_tasks(
    w: &mut dyn Write,
    core: &mut dyn Core,
    hubris: &HubrisArchive,
    registers: bool,
    stack: bool,
    line: bool,
    guess: bool,
    spin: bool,
    verbose: bool,
    task_arg: Option<String>,
) -> Result<()> {
    let (base, task_count) = hubris.task_table(core)?;
    log::debug!("task table: {:#x?}, count: {}", base, task_count);

    let task_t = hubris.lookup_struct_byname("Task")?;
    let save = task_t.lookup_member("save")?.offset;
    let state = hubris.lookup_struct_byname("SavedState")?;
    let r4 = save + state.lookup_member("r4")?.offset;

    let mut found = false;

    let printer = humility_stack::StackPrinter {
        indent: 3,
        line,
        additional: registers || verbose,
    };

    loop {
        core.halt()?;

        let ticks =
            if core.is_net() { None } else { Some(hubris.ticks(core)?) };

        let cur = hubris.current_task(core)?;
        let task_dump = hubris.task_dump();

        //
        // We read the entire task table at a go to get as consistent a
        // snapshot as possible.
        //
        let mut taskblock = vec![0; task_t.size * task_count as usize];

        if let Some(HubrisTask::Task(i)) = task_dump {
            let offs = i as usize * task_t.size;
            let addr = base + offs as u32;
            core.read_8(addr, &mut taskblock[offs..offs + task_t.size])?;
        } else if core.is_net() {
            // We cannot remotely read supervisor or non-TCB memory, so we skip
            // the supervisor and the kernel epitaph if this is a remote core.
            core.read_8(
                base + task_t.size as u32,
                &mut taskblock[task_t.size..],
            )?;
            humility::msg!(
                "reading tasks remotely; state may not be consistent"
            );
        } else {
            core.read_8(base, &mut taskblock)?;

            if let Some(epitaph) = hubris.epitaph(core)? {
                humility::warn!("kernel has panicked: {}", epitaph);
            }
        }

        let mut tasks = vec![];
        let mut regs = HashMap::new();

        for i in 0..task_count {
            let addr = base + i * task_t.size as u32;
            let offs = i as usize * task_t.size;

            if let Some(HubrisTask::Task(ndx)) = task_dump {
                if ndx != i {
                    continue;
                }
            }

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

            tasks.push((i, addr, task_value, task));
        }

        writeln!(
            w,
            "system time = {}",
            ticks
                .map(|t| t.to_string())
                .unwrap_or_else(|| "unavailable-via-net".to_owned())
        )?;
        writeln!(
            w,
            "{:2} {:21} {:>8} {:3} {:9}",
            "ID", "TASK", "GEN", "PRI", "STATE"
        )?;

        let mut any_names_truncated = false;

        for (i, addr, task_value, task) in tasks.iter() {
            let i = *i;

            let module = match hubris.lookup_module(HubrisTask::Task(i)) {
                Ok(m) => &m.name,
                _ => "<unknown>",
            };

            let irqs = hubris.manifest.task_irqs.get(module);

            if let Some(ref task) = task_arg {
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

            let timer = match (task.timer.deadline, ticks) {
                (Some(deadline), Some(ticks)) => Some(Deadline::Relative {
                    dt: deadline.0 as i64 - ticks as i64,
                    notif: task.timer.to_post.0,
                }),
                (Some(deadline), None) => Some(Deadline::Absolute {
                    t: deadline.0,
                    notif: task.timer.to_post.0,
                }),
                (None, _) => None,
            };

            let modname = {
                let mut modname = module.to_string();
                if modname.len() > 20 {
                    modname.truncate(20);
                    modname.push('…');
                    any_names_truncated = true;
                }
                modname
            };

            // Special case for the supervisor on a net core, which we can't
            // meaningfully process.
            if i == 0 && core.is_net() {
                writeln!(
                    w,
                    "{:2} {:21} {:>8} {:3} [cannot read supervisor memory]",
                    i, modname, "?", task.priority.0
                )?;
                continue;
            } else {
                write!(
                    w,
                    "{:2} {:21} {:>8} {:3} ",
                    i,
                    modname,
                    u32::from(task.generation),
                    task.priority.0
                )?;
            }
            explain_state(
                w,
                hubris,
                core,
                i,
                &regs,
                task.state,
                cur == Some(HubrisTask::Task(i)),
                irqs,
                timer,
            )?;
            writeln!(w)?;

            let desc: TaskDesc = task.descriptor.load_from(hubris, core)?;
            if stack || registers {
                let t = HubrisTask::Task(i);
                match hubris.registers(core, t) {
                    Ok(regs) => {
                        if stack {
                            let initial = desc.initial_stack;

                            match hubris.stack(core, t, initial, &regs) {
                                Ok(stack) => printer.print(hubris, &stack),
                                Err(e) => {
                                    writeln!(
                                        w,
                                        "   stack unwind failed: {e:?}"
                                    )?;
                                }
                            }
                        }

                        if registers {
                            print_regs(w, &regs, verbose)?;
                        }
                    }
                    Err(e) => {
                        writeln!(w, "   could not read registers: {e:?}")?;
                        if stack {
                            if guess {
                                match stack_guess(
                                    w, core, hubris, t, task_value, &desc,
                                ) {
                                    Ok(stack) => printer.print(hubris, &stack),
                                    Err(e) => writeln!(
                                        w,
                                        "   stack unwind failed: {e:?}"
                                    )?,
                                }
                            } else {
                                writeln!(
                                    w,
                                    "   use `--guess` to guess at stack trace"
                                )?;
                            }
                        }
                    }
                }
            }

            if verbose {
                let fmt = HubrisPrintFormat {
                    indent: 16,
                    newline: true,
                    hex: true,
                    ..HubrisPrintFormat::default()
                };

                write!(w, "   |\n   +-----------> {:#08x} ", addr)?;
                task_value.format(hubris, fmt, &mut std::io::stdout())?;
                writeln!(w, "\n")?;
            }

            if registers && !verbose {
                writeln!(w)?;
            }
        }

        if any_names_truncated {
            writeln!(
                w,
                "Note: task names were truncated to fit. Use \
                humility manifest to see them."
            )?;
        }

        core.run()?;

        if task_arg.is_some() && !found {
            bail!("\"{}\" is not a valid task", task_arg.unwrap());
        }

        if !spin {
            break;
        }
    }

    Ok(())
}

fn stack_guess<'a>(
    w: &'a mut dyn Write,
    core: &'a mut dyn Core,
    hubris: &'a HubrisArchive,
    t: HubrisTask,
    task_value: &'a reflect::Value,
    desc: &'a TaskDesc,
) -> Result<Vec<HubrisStackFrame<'a>>> {
    writeln!(w, "   guessing at stack trace using saved frame pointer")?;
    let reflect::Value::Struct(s) = &task_value else {
        bail!("invalid type for task_value")
    };
    let Some(reflect::Value::Struct(save)) = s.get("save") else {
        bail!("invalid type for save")
    };
    let Some(reflect::Value::Base(reflect::Base::U32(addr))) = save.get("r7")
    else {
        bail!("invalid type for r7")
    };
    let pc = core.read_word_32(*addr + 4)? & !1;

    let mut regs = BTreeMap::new();
    regs.insert(ARMRegister::R7, *addr);
    regs.insert(ARMRegister::LR, *addr);

    // Provide a dummy stack value to pick the
    // correct memory region
    regs.insert(ARMRegister::SP, desc.initial_stack);

    // See if the previous instruction was a branch;
    // if so, use that as a fake PC
    regs.insert(
        ARMRegister::PC,
        if let Some(HubrisTarget::Call(t)) =
            pc.checked_sub(4).and_then(|pc| hubris.instr_target(pc))
        {
            t + 4
        } else {
            pc
        },
    );

    let initial = desc.initial_stack;

    hubris.stack(core, t, initial, &regs)
}

#[derive(Copy, Clone, Debug)]
enum Deadline {
    Absolute { t: u64, notif: u32 },
    Relative { dt: i64, notif: u32 },
}

#[allow(clippy::too_many_arguments)]
fn explain_state(
    w: &mut dyn Write,
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    task_index: u32,
    regs: &HashMap<(u32, ARMRegister), u32>,
    ts: TaskState,
    current: bool,
    irqs: Option<&Vec<(u32, u32)>>,
    timer: Option<Deadline>,
) -> Result<()> {
    match ts {
        TaskState::Healthy(ss) => {
            explain_sched_state(
                w, hubris, task_index, regs, current, irqs, timer, ss,
            )?;
        }
        TaskState::Faulted { fault, original_state } => {
            explain_fault_info(w, hubris, core, task_index, regs, fault)?;
            write!(w, " (was: ")?;
            explain_sched_state(
                w,
                hubris,
                task_index,
                regs,
                current,
                irqs,
                timer,
                original_state,
            )?;
            write!(w, ")")?;
        }
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn explain_sched_state(
    w: &mut dyn Write,
    hubris: &HubrisArchive,
    task_index: u32,
    regs: &HashMap<(u32, ARMRegister), u32>,
    current: bool,
    irqs: Option<&Vec<(u32, u32)>>,
    timer: Option<Deadline>,
    e: doppel::SchedState,
) -> Result<()> {
    use doppel::SchedState;

    match e {
        SchedState::Stopped => {
            write!(w, "not started")?;
        }
        SchedState::Runnable => {
            if current {
                write!(w, "RUNNING")?;
            } else {
                write!(w, "ready")?;
            }
        }
        SchedState::InSend(tid) => {
            if tid == TaskId::KERNEL {
                write!(w, "HALT: send to kernel")?;
            } else {
                write!(w, "wait: send to ")?;
                print_task_id(w, hubris, tid)?;
            }
        }
        SchedState::InReply(tid) => {
            write!(w, "wait: reply from ")?;
            print_task_id(w, hubris, tid)?;
        }
        SchedState::InRecv(tid) => {
            let notmask = *regs.get(&(task_index, ARMRegister::R6)).unwrap();
            explain_recv(w, hubris, task_index, tid, notmask, irqs, timer)?;
        }
    }
    Ok(())
}

fn print_task_id(
    w: &mut dyn Write,
    hubris: &HubrisArchive,
    task_id: TaskId,
) -> Result<()> {
    if let Some(n) = hubris.task_name(task_id.index()) {
        write!(w, "{}/gen{}", n, task_id.generation())?;
    } else {
        write!(w, "unknown#{}/gen{}", task_id.index(), task_id.generation())?;
    }

    Ok(())
}

fn explain_fault_info(
    w: &mut dyn Write,
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    task_index: u32,
    regs: &HashMap<(u32, ARMRegister), u32>,
    fi: doppel::FaultInfo,
) -> Result<()> {
    use doppel::FaultInfo;

    write!(w, "FAULT: ")?;
    match fi {
        FaultInfo::DivideByZero => {
            write!(w, "divide by zero")?;
        }
        FaultInfo::IllegalText => {
            write!(w, "jump to non-executable mem")?;
        }
        FaultInfo::IllegalInstruction => {
            write!(w, "illegal instruction")?;
        }
        FaultInfo::InvalidOperation(bits) => {
            write!(w, "general fault, cfsr=0x{:x}", bits)?;
        }
        FaultInfo::StackOverflow { address } => {
            write!(w, "stack overflow; sp=0x{:x}", address)?;
        }
        FaultInfo::Injected(task) => {
            write!(w, "killed by ")?;
            print_task_id(w, hubris, task)?;
        }
        FaultInfo::MemoryAccess { address, source } => {
            write!(w, "mem fault (")?;
            if let Some(addr) = address {
                write!(w, "precise: 0x{:x}", addr)?;
            } else {
                write!(w, "imprecise")?;
            }
            write!(w, ")")?;

            explain_fault_source(w, source)?;
        }
        FaultInfo::BusError { address, source } => {
            write!(w, "bus fault (")?;
            if let Some(addr) = address {
                write!(w, "precise: 0x{:x}", addr)?;
            } else {
                write!(w, "imprecise")?;
            }
            write!(w, ")")?;

            explain_fault_source(w, source)?;
        }
        FaultInfo::SyscallUsage(ue) => {
            write!(w, "in syscall: ")?;
            explain_usage_error(w, ue)?;
        }
        FaultInfo::Panic => {
            let msg_base = *regs.get(&(task_index, ARMRegister::R4)).unwrap();
            let msg_len = *regs.get(&(task_index, ARMRegister::R5)).unwrap();
            let msg_len = msg_len.min(255) as usize;
            let mut buf = vec![0; msg_len];
            core.read_8(msg_base, &mut buf)?;
            match std::str::from_utf8(&buf) {
                Ok(msg) => {
                    write!(w, "{}", msg)?;
                }
                Err(_) => {
                    write!(w, "panic with invalid message")?;
                }
            }
        }
        FaultInfo::FromServer(task_id, reason) => {
            write!(w, "reply fault: task id {}, reason {:?}", task_id, reason)?;
        }
    }
    Ok(())
}

fn explain_usage_error(w: &mut dyn Write, e: doppel::UsageError) -> Result<()> {
    use doppel::UsageError::*;
    match e {
        BadSyscallNumber => write!(w, "undefined syscall number")?,
        InvalidSlice => write!(w, "sent malformed slice to kernel")?,
        TaskOutOfRange => write!(w, "used bogus task index")?,
        IllegalTask => write!(w, "illegal task operation")?,
        LeaseOutOfRange => write!(w, "bad caller lease index")?,
        OffsetOutOfRange => write!(w, "bad caller lease offset")?,
        NoIrq => write!(w, "referred to undefined interrupt")?,
        BadKernelMessage => write!(w, "sent nonsense IPC to kernel")?,
    };

    Ok(())
}

fn explain_fault_source(
    w: &mut dyn Write,
    e: doppel::FaultSource,
) -> Result<()> {
    match e {
        doppel::FaultSource::User => write!(w, " in task code")?,
        doppel::FaultSource::Kernel => write!(w, " in syscall")?,
    };

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
    w: &mut dyn Write,
    hubris: &HubrisArchive,
    task_index: u32,
    src: Option<TaskId>,
    notmask: u32,
    irqs: Option<&Vec<(u32, u32)>>,
    timer: Option<Deadline>,
) -> Result<()> {
    // Come up with a description for each notification bit.
    struct NoteInfo {
        irqs: Vec<u32>,
        timer: Option<Deadline>,
        bit: u32,
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
        let timer_assoc = timer.filter(|t| match t {
            Deadline::Absolute { notif, .. }
            | Deadline::Relative { notif, .. } => notif & bitmask != 0,
        });
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
            write!(w, "recv(")?;
            print_task_id(w, hubris, other)?;
            write!(w, " only)")?;
        }
        None => {
            write!(w, "recv")?;
        }
    }

    let task_mod = hubris.lookup_module(HubrisTask::Task(task_index));
    let notification_names = if let Ok(task_mod) = task_mod {
        hubris.manifest.task_notifications.get(&task_mod.name)
    } else {
        None
    };
    let notification_name = |s: u32| -> Option<&str> {
        notification_names.and_then(|n| n.get(s as usize)).map(|s| s.as_str())
    };

    // Display notification bits, along with meaning where we can.
    if notmask != 0 {
        write!(w, "{}notif:", if outer_first { "" } else { ", " })?;
        for nt in note_types {
            let name = notification_name(nt.bit);
            if let Some(name) = name {
                write!(w, " {name}")?;
            } else {
                write!(w, " bit{}", nt.bit)?;
            }
            if !nt.irqs.is_empty() || nt.timer.is_some() {
                write!(w, "(")?;
                let mut first = true;
                if let Some(ts) = nt.timer {
                    write!(w, "{}T", if !first { "/" } else { "" })?;
                    match ts {
                        Deadline::Relative { dt, .. } => {
                            write!(w, "{dt:+}")?;
                        }
                        Deadline::Absolute { t, .. } => {
                            write!(w, "={t:}")?;
                        }
                    }
                    first = false;
                }
                for irq in &nt.irqs {
                    write!(w, "{}irq{}", if !first { "/" } else { "" }, irq)?;
                    first = false;
                }
                write!(w, ")")?;
            }
        }
    }

    // Flag things that are probably bugs
    if src == Some(TaskId::KERNEL) && notmask == 0 {
        write!(w, "(DEAD)")?;
    }

    Ok(())
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
