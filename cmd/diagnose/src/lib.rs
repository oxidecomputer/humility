// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility diagnose`
//!
//! This subcommand provides a "firmware engineer in a can" for scanning and
//! reporting issues in a running system. It's mostly concerned with
//!
//! - Noticing things that seem fishy, and
//! - Taking action to get good snapshots of those things.
//!
//! This is application-independent logic, so it doesn't have any visibility
//! into things the _application_ may think are fishy -- only general behaviors
//! at the OS level, like faults.

use anyhow::{bail, Result};
use clap::{CommandFactory, Parser};
use humility::cli::Subcommand;
use humility::core::Core;
use humility::hubris::*;
use humility::reflect;
use humility_cmd::doppel::{GenOrRestartCount, Task, TaskDesc, TaskState};
use humility_cmd::{jefe, CommandKind};
use humility_cmd::{Archive, Attach, Command, Validate};
use std::num::NonZeroU32;
use std::time::Duration;

/// Command registration.
pub fn init() -> Command {
    Command {
        app: DiagnoseArgs::command(),
        name: "diagnose",
        run: diagnose,
        kind: CommandKind::Attached {
            archive: Archive::Required,
            attach: Attach::Any,
            validate: Validate::Booted,
        },
    }
}

#[derive(Parser, Debug)]
#[clap(name = "diagnose", about = env!("CARGO_PKG_DESCRIPTION"))]
struct DiagnoseArgs {
    /// timeout to wait for interactions with the supervisor task to complete
    #[clap(
        long, short, default_value_t = 5000, value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// number of milliseconds to sleep trying to catch crashes
    #[clap(long, short, default_value_t = 15, value_name = "ms")]
    sleep_ms: u64,

    /// suppresses automatic coredump generation
    #[clap(long, short)]
    no_dump: bool,
}

/// Error conditions that we can report, giving the user a code they can cite
/// unambiguously when raising the issue. This is intended to work like the
/// error numbers from some compilers.
#[derive(Copy, Clone, Debug)]
enum Condition {
    TaskFaulted,
    TaskHasBeenRestarted,
    TimeWentBackwards,
    TaskFlapping,
}

/// Displays the condition name on a new line, because that's typically what we
/// need.
impl std::fmt::Display for Condition {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "\n[{:?}]", self)
    }
}

/// Generates a report section header.
fn section(title: &str) {
    println!("\n--- {} ---\n", title);
}

fn diagnose(context: &mut humility::ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let hubris = context.archive.as_ref().unwrap();

    let subargs = DiagnoseArgs::try_parse_from(subargs)?;

    section("Initial Inspection");

    println!("Taking initial snapshot of task status...");

    // Mise en place:
    let base = core.read_word_32(hubris.lookup_symword("TASK_TABLE_BASE")?)?;
    let task_count =
        core.read_word_32(hubris.lookup_symword("TASK_TABLE_SIZE")?)? as usize;
    let task_t = hubris.lookup_struct_byname("Task")?.clone();

    // Park the core so that we don't have stuff changing out from under us.
    // Until we expect that, anyway.
    core.halt()?;
    let ticks_0 = core.read_word_64(hubris.lookup_variable("TICKS")?.addr)?;
    println!(
        "System last rebooted {} ticks ago; \
         assuming tick=millisecond, {:?} ago",
        ticks_0,
        Duration::from_millis(ticks_0),
    );

    // Read the initial task table snapshot.
    let tasks_0 = load_tcbs(hubris, core, base, task_count, &task_t)?;
    // Load the descriptors; these are not expected to change.
    let descs = load_task_descs(hubris, core, &tasks_0)?;
    // Find the names for each descriptor; these too are not expected to change.
    let task_names = find_task_names(hubris, &descs)?;

    println!("Snapshot taken, {} tasks in application.", tasks_0.len());

    let narrow_generations =
        matches!(tasks_0[0].generation, GenOrRestartCount::Gen(_));

    if narrow_generations {
        println!(
            "Firmware using older 6-bit kernel generations, restart counts \n\
             will be less precise."
        );
    }

    section("Tasks: First Pass");
    println!("Checking for any suspicious task attributes...");
    for (i, task) in tasks_0.iter().enumerate() {
        if let TaskState::Faulted { fault, original_state } = task.state {
            println!(
                "{} task {} (#{}) is in faulted state\n\
                 - Fault: {:?}\n\
                 - State before fault: {:?}",
                Condition::TaskFaulted,
                task_names[i],
                i,
                fault,
                original_state,
            );
            if u32::from(task.generation) == 0 {
                println!("- Task may be held in this state (gen=0)");
            }
        }

        if u32::from(task.generation) > 0 {
            let g = u32::from(task.generation);
            println!(
                "{} task {} (#{}) has been restarted\n\
                 - Generation: {}",
                Condition::TaskHasBeenRestarted,
                task_names[i],
                i,
                g,
            );
            if !narrow_generations {
                let per_tick = g as f64 / ticks_0 as f64;
                println!(
                    "- rate: {:.03} restarts/tick, or {:.03}/s",
                    per_tick,
                    per_tick * 1000.,
                );
            }
        }
    }

    // This is a bit of a hack, but, we can detect core dumps by going...
    if core.info().0 == "core dump" {
        section("This Is A Core Dump");
        println!("You're running this on a dump; that's all we can do.");
        println!("Connect to a live system for more output.");
        section("End Of Report");
        return Ok(());
    }

    section("Advancing Time");

    println!("\nResuming core for a bit...");
    core.run()?;
    if narrow_generations {
        // Give the core a bit to make forward progress. In practice, this will
        // be at least 2ms longer than whatever we sleep here, because probe-rs
        // is teh slow.
        std::thread::sleep(Duration::from_millis(subargs.sleep_ms));
    } else {
        // With wider generations we allow more time because wraparound is
        // unlikely.
        std::thread::sleep(Duration::from_millis(1000));
    }
    core.halt()?;

    // Take a new snapshot.
    let ticks_1 = core.read_word_64(hubris.lookup_variable("TICKS")?.addr)?;
    let tasks_1 = load_tcbs(hubris, core, base, task_count, &task_t)?;

    // Note: we are leaving core halted here.

    // Check for any time reversal.
    let delta_ticks = ticks_1.checked_sub(ticks_0);
    if let Some(t) = delta_ticks {
        println!("Core halted after {} more ticks.", t);
    } else {
        println!(
            "{}: The kernel timestamp has rewound.\n\
             It was last observed as {}, and is now {}.\n\
             This probably indicates one of the following conditions:\n\
             1. The processor is rebooting due to a failure in the kernel or\n\
                supervisor,\n\
             2. Kernel memory is being corrupted.\n\
             3. There is a bug in Humility and this message is misguided.\n\
             \n\
             Output past this point may be less useful as a result.",
            Condition::TimeWentBackwards,
            ticks_0,
            ticks_1,
        );

        // Heuristically guess if this is a plausible reboot rather than
        // corruption.
        if ticks_1 < 100 {
            println!(
                "With a ticks value of {} the reboot is most likely.",
                ticks_1
            );
        }
    }

    if delta_ticks == Some(0) {
        println!(
            "The kernel does not appear to be making forward progress.\n\
             This implies that either\n\
             1. The SysTick ISR is being starved (more likely), or\n\
             2. The system is rebooting itself in less than 1ms.\n\
             \n\
             In practice the system is probably stuck at a kernel fault."
        );
    }

    // Make a second pass over the TCBs looking for evidence of new restarts,
    // and particularly, tasks that are restarting but we haven't seen why.
    section("Tasks: Second Pass");

    // This will accumulate the (name, index) of tasks that we need to have the
    // supervisor halt.
    let mut tasks_worth_holding = vec![];

    for (i, (before, after)) in tasks_0.iter().zip(&tasks_1).enumerate() {
        if before.generation != after.generation {
            // Estimate restart count. Since kernel generation bits are
            // currently limited, this is best-effort.
            let count = match (before.generation, after.generation) {
                (
                    GenOrRestartCount::RestartCount(b),
                    GenOrRestartCount::RestartCount(a),
                ) => a.wrapping_sub(b),
                (GenOrRestartCount::Gen(b), GenOrRestartCount::Gen(a)) => {
                    u32::from(a.0.wrapping_sub(b.0) & 0x3F)
                }
                _ => bail!("generation changed shape between reads?"),
            };
            println!(
                "{}: Task {} (#{}) has restarted {}{} more times.",
                Condition::TaskFlapping,
                task_names[i],
                i,
                if narrow_generations { "at least " } else { "" },
                count,
            );
            // Estimate restart rate.
            if let Some(dt) = delta_ticks {
                let f = f64::from(count) / dt as f64;
                println!(
                    "- (this is {:.03} restarts/tick, or {:.03}/s)",
                    f,
                    1000. * f
                );
            }

            // Report previous or current faults.
            if let TaskState::Faulted { fault, .. } = after.state {
                println!("- current fault is {:?}", fault);
            } else if let TaskState::Faulted { fault, .. } = before.state {
                println!("- note: fault from earlier was {:?}", fault);
            } else {
                println!("- fault cause undetermined so far");
                // Add it to our list. Unless it's the supervisor. In that case,
                // just complain.
                if let Some(idx) = NonZeroU32::new(i as u32) {
                    tasks_worth_holding.push((&task_names[i], idx));
                } else {
                    println!("- THIS IS THE SUPERVISOR. Fault bad.");
                }
            }
        }
    }

    if !tasks_worth_holding.is_empty() {
        section("Halting Errant Tasks");
        println!(
            "{} tasks are restarting and we don't know why.",
            tasks_worth_holding.len()
        );
        println!("Requesting that the supervisor stop restarting:");
        core.run()?;
        for &(name, i) in &tasks_worth_holding {
            println!("- {}", name);
            jefe::send_request(
                hubris,
                core,
                jefe::JefeRequest::Hold,
                i,
                subargs.timeout,
            )?;
        }

        println!("Tasks no longer restarting, giving them time to crash...");
        std::thread::sleep(Duration::from_millis(5));
        core.halt()?;

        println!("Results for these tasks::");
        let tasks_2 = load_tcbs(hubris, core, base, task_count, &task_t)?;
        for (name, i) in tasks_worth_holding {
            let i = u32::from(i) as usize;
            if let TaskState::Faulted { fault, original_state } =
                tasks_2[i].state
            {
                println!(
                    "{}: task {} (#{}) is in the faulted state",
                    Condition::TaskFaulted,
                    name,
                    i
                );
                println!("- Fault: {:?}", fault);
                println!("- State before fault: {:?}", original_state);
            } else {
                println!("Failed to catch task {} (#{}) in the act.", name, i);
            }
        }
    }

    if !subargs.no_dump {
        section("Generating Coredump");
        let regions = hubris.regions(core)?;
        let rval = hubris.dump(core, &regions, None, None);
        if let Err(e) = rval {
            println!("Coredump failed: {}", e);
        }
    }

    println!("Leaving CPU halted in case you want to inspect.");

    section("End Of Report");

    Ok(())
}

/// Reads a snapshot of the task table (the Task Control Blocks) from target
/// memory.
///
/// Tasks are returned in index order.
fn load_tcbs(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    task_table_base: u32,
    task_count: usize,
    task_t: &HubrisStruct,
) -> Result<Vec<Task>> {
    let taskblock = {
        let mut taskblock = vec![0; task_t.size * task_count];
        core.read_8(task_table_base, &mut taskblock)?;
        taskblock
    };

    let mut tasks = Vec::with_capacity(task_count);
    for i in 0..task_count {
        let offs = i * task_t.size;

        let task: Task = reflect::load(hubris, &taskblock, task_t, offs)?;
        tasks.push(task);
    }

    Ok(tasks)
}

/// Reads a snapshot of the Task Descriptors portion of the app table from
/// target memory. The Task Descriptors are returned in the same order as the
/// given set of tasks.
fn load_task_descs<'a>(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    tasks: impl IntoIterator<Item = &'a Task>,
) -> Result<Vec<TaskDesc>> {
    tasks
        .into_iter()
        .map(|task| {
            let desc: TaskDesc = task.descriptor.load_from(hubris, core)?;
            Ok(desc)
        })
        .collect()
}

/// Finds the task/module names corresponding to a sequence of `TaskDesc`s.
///
/// This is basically a very expensive way of loading the task name array from
/// the toml, but, this is how the data structures are currently arranged.
fn find_task_names<'a>(
    hubris: &HubrisArchive,
    descs: impl IntoIterator<Item = &'a TaskDesc>,
) -> Result<Vec<String>> {
    Ok(descs
        .into_iter()
        .map(|desc| {
            hubris
                .instr_mod(desc.entry_point)
                .unwrap_or("<unknown>")
                .to_string()
        })
        .collect())
}
