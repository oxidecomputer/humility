// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility counters`
//!
//! `humility counters` reads and displays any Hubris counters (as created via
//! the `counters` crate and either the `counters!` or `counted_ringbuf!`
//! macros).
//!
//! For example:
//! ```console
//! $ humility -d ./hubris.core.0 counters
//! humility: attached to dump
//! gimlet_seq
//! |
//! +---> drv_gimlet_seq_server::__RINGBUF:
//!       TOTAL VARIANT
//!           1 Ice40Rails
//!           1 IdentValid
//!           1 ChecksumValid
//!           1 Reprogram
//!           1 Programmed
//!           1 Programming
//!           1 Ice40PowerGoodV1P2
//!           1 Ice40PowerGoodV3P3
//!           1 RailsOff
//!           1 Ident
//!           1 A2Status
//!           1 A2
//!         102 A1Status
//!           1 CPUPresent
//!           1 Coretype
//!         292 A0Status
//!          15 A0Power
//! ...
//! ```
//!
//! If an argument is provided, only counters that have a name that
//! contains the argument as a substring, or are in a task that contains
//! the argument as a substring will be displayed.  For example,
//! to display every counter that has `thermal` in the name or the
//! containing task:
//!
//! ```console
//! $ humility -d ./hubris.core.0 counters thermal
//! humility: attached to dump
//! thermal
//!  |
//!  +---> task_thermal::__RINGBUF:
//!        TOTAL VARIANT
//!            1 Start
//!            1 ThermalMode(Auto)
//!            3 AutoState(Boot)
//!            2 AutoState(Running)
//!           78 ControlPwm
//!            2 PowerModeChanged
//!            6 FanAdded
//! ```
//!
//!
//! The `--ipc` argument shows IPC client counters generated automatically by
//! `idol`, showing the total request count for a given IPC and per-client-task
//! breakdowns. For example:`
//!
//! ```console
//! $ humility -d ./hubris.core.0 counters --ipc`
//! humility: attached to dump
//! drv_gimlet_hf_api::__HOSTFLASH_CLIENT_COUNTERS
//!        6 HostFlash::get_mux()
//!        6 +---> Ok <---+ [host_sp_comms]
//!
//!        2 HostFlash::set_mux()
//!        2 +---> Ok
//!        1 |     <---+ [gimlet_seq]
//!        1 |     <---+ [host_sp_comms]
//!
//!        1 HostFlash::get_dev()
//!        1 +---> Ok <---+ [host_sp_comms]
//!
//!
//! drv_gimlet_seq_api::__SEQUENCER_CLIENT_COUNTERS
//!     2017 Sequencer::get_state()
//!     2017 +---> Ok
//!     1386 |     <---+ [thermal]
//!      626 |     <---+ [power]
//!        5 |     <---+ [host_sp_comms]
//!
//!
//! drv_spi_api::__SPI_CLIENT_COUNTERS
//!    67589 Spi::exchange()
//!    67589 +---> Ok
//!    67580 |     <---+ [gimlet_seq]
//!        8 |     <---+ [net]
//!        1 |     <---+ [host_sp_comms]
//!
//!      592 Spi::write()
//!      592 +---> Ok
//!      530 |     <---+ [gimlet_seq]
//!       62 |     <---+ [net]
//!
//!        4 Spi::lock()
//!        4 +---> Ok <---+ [gimlet_seq]
//!
//!        1 Spi::release()
//!        1 +---> Ok <---+ [gimlet_seq]
//! ...
//! ```
//!
//! When displaying counters by IPC, substring filtering is performed on the
//! counters variable, but *not* on the client task name.

use anyhow::{bail, Result};
use clap::{CommandFactory, Parser, ValueEnum};
use colored::Colorize;
use core::num;
use humility::core::Core;
use humility::hubris::*;
use humility::reflect::{self, Load, Value};
use humility_cli::{ExecutionContext, Subcommand};
use humility_cmd::{Archive, Attach, Command, CommandKind, Validate};
use humility_doppel::{
    CountedRingbuf, CounterVariant, Counters, GenOrRestartCount,
};
use indexmap::IndexMap;
use std::collections::BTreeMap;
use std::fmt;

#[derive(Parser, Debug)]
#[clap(name = "counters", about = env!("CARGO_PKG_DESCRIPTION"))]
struct CountersArgs {
    /// list all counters without displaying their values
    #[clap(long, short)]
    list: bool,

    /// print full errors
    #[clap(long, short)]
    verbose: bool,

    /// print only a single counter by substring of name
    #[clap(conflicts_with = "list")]
    name: Option<String>,

    /// show counters with zero values
    #[clap(long, short, conflicts_with = "list")]
    full: bool,

    /// show IPC counters, grouped by IPC interface rather than by counter.
    #[clap(long, short, conflicts_with = "list")]
    ipc: bool,

    /// sort counters using the provided ordering.
    ///
    /// [default: `decl` if `--full` is set, `alpha` otherwise]
    #[clap(long, short, conflicts_with = "list", value_enum)]
    sort: Option<Order>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, ValueEnum)]
#[clap(rename_all = "kebab-case")]
enum Order {
    /// Sort by declaration order in the source struct.
    Decl,
    /// Sort by counter value, descending (highest to lowest).
    Value,
    /// Sort by counter name, ascending (alphabetical).
    Alpha,
}

fn counters(context: &mut ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let hubris = context.archive.as_ref().unwrap();

    let subargs = CountersArgs::try_parse_from(subargs)?;

    if subargs.ipc {
        return ipc_counter_dump(hubris, core, &subargs);
    }

    // map of counters by task, sorted by task name.
    let mut counters: BTreeMap<&str, Vec<_>> = BTreeMap::new();

    for (varname, var) in hubris.qualified_variables() {
        let def = match hubris.lookup_struct(var.goff) {
            Ok(s) => {
                // Skip variables whose type does not indicate they contain a
                // counters struct; this check is imprecise but probably good enough
                if s.name.contains("Counts")
                    || s.name.contains("CountedRingbuf")
                {
                    Some(s)
                } else {
                    continue;
                }
            }
            Err(_) => {
                // Type lookup failed, so fall back to the variable name
                if varname.ends_with("COUNTERS") {
                    None
                } else {
                    continue;
                }
            }
        };
        let t = taskname(hubris, var)?;
        if let Some(ref name) = subargs.name {
            if varname.contains(name) || t.contains(name) {
                counters.entry(t).or_default().push(((varname, var), def));
            }
        } else {
            counters.entry(t).or_default().push(((varname, var), def));
        }
    }

    if counters.is_empty() {
        if let Some(name) = subargs.name {
            bail!(
                "no counters found with names containing \"{}\" (-l to list)",
                name
            );
        } else {
            bail!("no counters found");
        }
    }

    for vars in counters.values_mut() {
        vars.sort_by_key(|&(v, _)| v);
    }

    if subargs.list {
        for (t, ctrs) in counters {
            println!("{t}:");
            println!("    {:<10} {:>5} VARIABLE", "ADDR", "SIZE",);
            for ((name, var), _) in ctrs {
                println!("    {:<#010x} {:>5} {name}", var.addr, var.size)
            }
        }

        return Ok(());
    }

    for (t, ctrs) in counters {
        // Try not to use `?` here, because it causes one bad counter to make
        // them all unavailable.
        println!("{t}\n |");
        let mut ctrs = ctrs.iter().peekable();
        while let Some(((varname, var), def)) = ctrs.next() {
            if let Some(def) = def {
                println!(" +---> {varname}:");
                let pad = if ctrs.peek().is_some() { " |  " } else { "    " };
                if let Err(e) =
                    counter_dump(hubris, core, def, var, &subargs, pad)
                {
                    if subargs.verbose {
                        humility::msg!("counter dump failed: {e:?}");
                    } else {
                        humility::msg!("counter dump failed: {e}");
                    }
                }
            } else {
                humility::msg!("could not look up type: {:?}", var.goff);
            }
        }
    }
    Ok(())
}

fn counter_dump(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    def: &HubrisStruct,
    var: &HubrisVariable,
    args: &CountersArgs,
    pad: &str,
) -> Result<()> {
    // Counters may either be a standalone counters variable or a counted
    // ringbuf. We'll try to interpret the var as either one.
    let mut counters = load_counters(hubris, core, def, var)?;

    // Sort the counters.
    match (args.sort, args.full) {
        // If `--full` is set, zero valued counters are displayed, so it's nice
        // to always display them in the same order across dumps/Humility
        // processes, so that the output is easily comparable. Therefore, sort
        // by declaration order, if no sorting was requested.
        (Some(Order::Decl), _) | (None, true) => {
            // Counters are already sorted by declaration order.
        }
        // If `--full` is not set, the output is not comparable across
        // dumps/Hubris processes, because which counters are present varies
        // based on the state of the system.
        //
        // Therefore, sort by value by default, so the highest-valued counters
        // are shown first.
        (Some(Order::Value), _) | (None, false) => counters.sort_unstable_by(
            &mut |_, a: &CounterVariant, _, b: &CounterVariant| {
                a.total().cmp(&b.total()).reverse()
            },
        ),
        (Some(Order::Alpha), _) => {
            counters.sort_unstable_by(&mut |a, _, b, _| a.cmp(b))
        }
    }

    let disp = counters.display_padded(pad);
    if args.full {
        println!("{disp:#}");
    } else if counters.total() > 0 {
        println!("{disp}");
    } else {
        println!("{pad}   <no counts recorded>")
    }
    Ok(())
}

fn ipc_counter_dump(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    subargs: &CountersArgs,
) -> Result<()> {
    // In order to display task generations accurately, we must find and load
    // the task table:
    let task_table = {
        let (base, task_count) = hubris.task_table(core)?;
        let task_t = hubris.lookup_struct_byname("Task")?.clone();
        humility_doppel::Task::load_tcbs(
            hubris,
            core,
            base,
            task_count as usize,
            &task_t,
        )?
    };

    let mut ipcs = BTreeMap::new();

    for (varname, var) in hubris.qualified_variables() {
        if varname.ends_with("_CLIENT_COUNTERS") {
            let task = HubrisTask::from(var.goff);
            let taskname = taskname(hubris, var)?;
            let gen = task_table[task.task() as usize].generation;
            if let Some(ref name) = subargs.name {
                if !varname.contains(name) {
                    continue;
                }
            }

            let def = hubris.lookup_struct(var.goff)?;
            let ctrs = load_counters(hubris, core, def, var)?;
            let iface = ipcs.entry(varname).or_insert_with(|| {
                let name = def
                    .name
                    .split("::")
                    .last()
                    .map(|s| {
                        s.trim_end_matches("Counts").trim_end_matches("Event")
                    })
                    .unwrap_or("");
                IpcIface { name, methods: Default::default() }
            });
            for (method, count) in ctrs.counts {
                iface
                    .methods
                    .entry(method)
                    .or_default()
                    .0
                    .insert((taskname, gen), count);
            }
        }
    }

    if ipcs.is_empty() {
        if let Some(ref name) = subargs.name {
            bail!(
                "no IPC counters found with names containing \"{}\" (-l to list)",
                name
            );
        } else {
            bail!("no IPC counters found");
        }
    }

    for (varname, mut iface) in ipcs {
        if let Some(order) = subargs.sort {
            iface.sort(order);
        } else if !subargs.full {
            iface.sort(Order::Value)
        }
        if subargs.full {
            println!("{varname}\n{iface:#}");
        } else if iface.total() > 0 {
            println!("{varname}\n{iface}");
        }
    }
    Ok(())
}

fn load_counters(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    def: &HubrisStruct,
    var: &HubrisVariable,
) -> Result<Counters> {
    let mut buf = vec![0u8; var.size];

    core.halt()?;
    core.read_8(var.addr, buf.as_mut_slice())?;
    core.run()?;

    let val: Value = Value::Struct(reflect::load_struct(hubris, &buf, def, 0)?);

    // Counters may either be a standalone counters variable or a counted
    // ringbuf. We'll try to interpret the var as either one.
    CountedRingbuf::from_value(&val)
        .map(|r| r.counters)
        .or_else(|_| Counters::from_value(&val))
}

struct IpcIface<'a> {
    name: &'a str,
    methods: IndexMap<String, Ipc<'a>>,
}

#[derive(Default)]
struct Ipc<'taskname>(
    IndexMap<(&'taskname str, GenOrRestartCount), CounterVariant>,
);

impl IpcIface<'_> {
    fn total(&self) -> usize {
        self.methods.values().map(Ipc::total).sum()
    }

    fn sort(&mut self, order: Order) {
        for (_, method) in &mut self.methods {
            method.sort(order);
        }
        match order {
            Order::Decl => {}
            Order::Value => {
                self.methods.sort_unstable_by(|_, a, _, b| {
                    a.total().cmp(&b.total()).reverse()
                });
            }
            Order::Alpha => {
                self.methods.sort_unstable_by(|a, _, b, _| a.cmp(b));
            }
        }
    }
}

impl fmt::Display for IpcIface<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let total_len = f.width().unwrap_or(10);
        let dim_space = " ".dimmed();
        for (method_name, ctrs) in &self.methods {
            let total = ctrs.total();
            if total == 0 && !f.alternate() {
                continue;
            }
            let total_str = total.to_string();
            let errors = ctrs.total_errors();
            let ok = total - errors;
            let ok_str = ok.to_string();
            let err_str = errors.to_string();
            writeln!(
                f,
                " fn {}::{}() {dim_space:.>pad$}{} calls\n    clients:",
                self.name.bold(),
                method_name.bold(),
                total_str.bold(),
                pad = 80usize
                    .saturating_sub(" fn ".len())
                    .saturating_sub("() ".len())
                    .saturating_sub("::".len())
                    .saturating_sub(" calls".len())
                    .saturating_sub(method_name.len())
                    .saturating_sub(self.name.len())
                    .saturating_sub(total_str.len())
                    .saturating_sub(1)
            )?;
            let mut formatted_tasks = 0;
            let mut formatted_errors = 0;
            let num_important_tasks = ctrs
                .0
                .iter()
                .filter(|(_, c)| c.total() > 0 || f.alternate())
                .count();
            for ((task, gen), ctrs) in &ctrs.0 {
                let total = ctrs.total();
                if total == 0 && !f.alternate() {
                    continue;
                } else {
                    formatted_tasks += 1;
                }

                let errors = ipc_error_count(ctrs);
                let ok = total - errors;
                let ok_str = format!(
                    "{} {ok}",
                    if num_important_tasks > 1 { "+" } else { "=" }
                );
                let restarts = match gen {
                    GenOrRestartCount::Gen(gen) => {
                        format!(" (gen {gen:?})")
                    }
                    GenOrRestartCount::RestartCount(restarts) => {
                        format!(" ({restarts} restarts)")
                    }
                };
                write!(f, "    task {}{restarts} ", task.italic())?;
                if errors == 0 && !f.alternate() {
                    let err_str =
                        if num_important_tasks > 1 { "+ 0" } else { "= 0" };
                    let pad1 = 80usize
                        .saturating_sub(err_str.len())
                        .saturating_sub(task.len())
                        .saturating_sub(restarts.len())
                        .saturating_sub("    task  ".len())
                        .saturating_sub(total_len * 2);
                    writeln!(
                        f,
                        "{dim_space:.>pad1$}{} {dim_space:.>pad2$}{} ok",
                        err_str.dimmed(),
                        if ok > 0 { ok_str.green() } else { ok_str.dimmed() },
                        pad2 = 80usize
                            .saturating_sub(pad1)
                            .saturating_sub(ok_str.len())
                            .saturating_sub(" ok".len())
                            .saturating_sub(err_str.len())
                            .saturating_sub(task.len())
                            .saturating_sub(restarts.len())
                            .saturating_sub("    task  ".len())
                            .saturating_sub(2)
                    )?;
                } else {
                    writeln!(
                        f,
                        "{dim_space:.>pad$}{} ok",
                        if ok > 0 { ok_str.green() } else { ok_str.dimmed() },
                        pad = 80usize
                            .saturating_sub(ok_str.len())
                            .saturating_sub(task.len())
                            .saturating_sub(restarts.len())
                            .saturating_sub("    task ".len())
                            .saturating_sub(2)
                            .saturating_sub(" ok".len())
                    )?;
                }

                fn fmt_err_variant(
                    ctr: &CounterVariant,
                    mut pfx: String,
                    formatted: &mut usize,
                    num_important: usize,
                    only_important_task: bool,
                    f: &mut fmt::Formatter<'_>,
                ) -> fmt::Result {
                    match ctr {
                        &CounterVariant::Single(value)
                            if value > 0 || f.alternate() =>
                        {
                            *formatted += 1;
                            let total_len = f.width().unwrap_or(10);
                            let value_str = format!("+ {value}",);
                            for _ in 0..pfx.matches('(').count() {
                                pfx.push(')');
                            }
                            let pad = 80usize
                                .saturating_sub(total_len * 2)
                                .saturating_sub(
                                    if num_important > 1 && !only_important_task
                                    {
                                        total_len + 3
                                    } else {
                                        0
                                    },
                                )
                                .saturating_sub(pfx.len())
                                .saturating_sub(value_str.len())
                                .saturating_sub("    - Err() ".len());
                            writeln!(
                                f,
                                "    - Err({}) {:.>pad$}{}{:.<rem$}",
                                pfx.red(),
                                " ".dimmed(),
                                if value > 0 {
                                    value_str.red()
                                } else {
                                    value_str.dimmed()
                                },
                                " ".dimmed(),
                                rem = 80usize
                                    .saturating_sub(pad)
                                    .saturating_sub(value_str.len())
                                    .saturating_sub(pfx.len())
                                    .saturating_sub("    - Err() ".len())
                                    .saturating_sub(1),
                            )?;
                        }
                        CounterVariant::Nested(map) => {
                            let num_important = map
                                .counts
                                .iter()
                                .filter(|(_, c)| c.total() > 0 || f.alternate())
                                .count();
                            for (name, ctr) in &map.counts {
                                fmt_err_variant(
                                    ctr,
                                    if !pfx.is_empty() {
                                        format!("{pfx}({name}")
                                    } else {
                                        name.clone()
                                    },
                                    formatted,
                                    num_important,
                                    only_important_task,
                                    f,
                                )?;
                            }
                        }
                        _ => {}
                    };

                    Ok(())
                }

                let mut formatted_errs_this_task = 0;
                let errs = if let CounterVariant::Nested(map) = ctrs {
                    map.counts.get("Err")
                } else {
                    None
                };
                if let Some(errs) = errs {
                    let err_total = errs.total();
                    if err_total > 0 || f.alternate() {
                        let total_str = format!("{err_total}");
                        if let CounterVariant::Nested(_) = errs {
                            let only_important_task = num_important_tasks == 1;
                            fmt_err_variant(
                                errs,
                                String::new(),
                                &mut formatted_errs_this_task,
                                0,
                                only_important_task,
                                f,
                            )?;
                            if formatted_errs_this_task > 1
                                && !only_important_task
                            {
                                const INDENT: &str = "      ";
                                let colored_total = if err_total > 0 {
                                    total_str.red()
                                } else {
                                    total_str.dimmed()
                                };
                                let pad = 80usize
                                    .saturating_sub(total_len * 3)
                                    .saturating_sub(total_str.len())
                                    .saturating_sub(INDENT.len())
                                    .saturating_sub(5);
                                write!(
                                    f,
                                    "{INDENT}{:>pad$}{:->underline$}\n{INDENT}{:>pad$}= {colored_total}",
                                    "",
                                    "",
                                    "",
                                    underline = total_str.len() + 2,
                                )?;
                                if num_important_tasks > 1 {
                                    writeln!(
                                        f,
                                        " {:->arrow$} {} {colored_total}",
                                        ">".dimmed(),
                                        if err_total > 0 {
                                            "+".red()
                                        } else {
                                            "+".dimmed()
                                        },
                                        arrow = (total_len + 3)
                                            .saturating_sub(4) // spaces and +
                                            .saturating_sub(total_str.len())
                                    )?;
                                } else {
                                    writeln!(f, " err")?;
                                }
                            }
                        }
                    }
                }
                formatted_errors += formatted_errs_this_task;
            }

            if formatted_tasks > 1 || formatted_errors > 1 {
                let err_underline = "-".repeat(err_str.len() + 2);
                let ok_underline = "-".repeat(ok_str.len() + 2);
                writeln!(
                    f,
                    "    {:>pad1$}{err_underline}    {:>pad2$}{ok_underline}",
                    "",
                    " ",
                    pad1 = 80usize
                        .saturating_sub(total_len * 2)
                        .saturating_sub(err_underline.len())
                        .saturating_sub(4),
                    pad2 = (total_len + 2) - ok_underline.len(),
                )?;
                writeln!(
                    f,
                    "    totals:{:>pad1$}= {} err {:>pad2$}= {} ok",
                    " ",
                    if errors > 0 { err_str.red() } else { err_str.dimmed() },
                    " ",
                    if ok > 0 { ok_str.green() } else { ok_str.dimmed() },
                    pad1 = 80usize
                        .saturating_sub(total_len * 2)
                        .saturating_sub(err_str.len() + 2)
                        .saturating_sub(4)
                        .saturating_sub("totals:".len()),
                    pad2 = (total_len) - (ok_str.len() + 1),
                )?;
            }
            f.write_str("\n")?;
        }

        Ok(())
    }
}

impl Ipc<'_> {
    fn total(&self) -> usize {
        self.0.values().map(CounterVariant::total).sum()
    }

    fn total_errors(&self) -> usize {
        self.0.values().map(ipc_error_count).sum()
    }

    fn sort(&mut self, order: Order) {
        match order {
            Order::Decl => {}
            Order::Value => {
                self.0.sort_unstable_by(|_, a, _, b| {
                    a.total().cmp(&b.total()).reverse()
                });
            }
            Order::Alpha => {
                self.0.sort_unstable_by(|(a, _), _, (b, _), _| a.cmp(b));
            }
        }
    }
}

fn ipc_error_count(ctr: &CounterVariant) -> usize {
    match ctr {
        CounterVariant::Single(_) => 0,
        CounterVariant::Nested(ref ctrs) => {
            ctrs.counts.get("Err").map(|c| c.total()).unwrap_or(0)
        }
    }
}

fn taskname<'a>(
    hubris: &'a HubrisArchive,
    variable: &'a HubrisVariable,
) -> Result<&'a str> {
    Ok(&hubris.lookup_module(HubrisTask::from(variable.goff))?.name)
}

pub fn init() -> Command {
    Command {
        app: CountersArgs::command(),
        name: "counters",
        run: counters,
        kind: CommandKind::Attached {
            archive: Archive::Required,
            attach: Attach::Any,
            validate: Validate::Match,
        },
    }
}
