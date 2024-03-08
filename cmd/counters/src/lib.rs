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
//!        6 +---> Ok [host_sp_comms]
//!
//!        2 HostFlash::set_mux()
//!        2 +---> Ok
//!        1 |      +---> [gimlet_seq]
//!        1 |      +---> [host_sp_comms]
//!
//!        1 HostFlash::get_dev()
//!        1 +---> Ok [host_sp_comms]
//!
//!
//! drv_gimlet_seq_api::__SEQUENCER_CLIENT_COUNTERS
//!     2017 Sequencer::get_state()
//!     2017 +---> Ok
//!     1386 |      +---> [thermal]
//!      626 |      +---> [power]
//!        5 |      +---> [host_sp_comms]
//! ...
//! ```
//!
//! When displaying counters by IPC, substring filtering is performed on the
//! counters variable, but *not* on the client task name.

use anyhow::{bail, Result};
use clap::{CommandFactory, Parser, ValueEnum};
use colored::Colorize;
use humility::core::Core;
use humility::hubris::*;
use humility::reflect::{self, Load, Value};
use humility_cli::{ExecutionContext, Subcommand};
use humility_cmd::{Archive, Attach, Command, CommandKind, Validate};
use humility_doppel::{CountedRingbuf, CounterVariant, Counters};
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
    let mut ipcs = BTreeMap::new();
    for (varname, var) in hubris.qualified_variables() {
        if varname.ends_with("_CLIENT_COUNTERS") {
            let task = taskname(hubris, var)?;
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
                IpcIface { name, counters: Default::default() }
            });
            IpcCounters::populate(
                &mut iface.counters,
                task,
                ctrs,
                subargs.full,
            );
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

    for (ipc_name, mut ctrs) in ipcs {
        if let Some(order) = subargs.sort {
            ctrs.sort(order);
        } else if !subargs.full {
            ctrs.sort(Order::Value)
        }
        if !ctrs.counters.is_empty() {
            println!("{ipc_name}\n{ctrs}");
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
    counters: IndexMap<String, IpcCounters<'a>>,
}

#[derive(Debug)]
enum IpcCounters<'taskname> {
    Single(IndexMap<&'taskname str, u32>),
    Nested(IndexMap<String, IpcCounters<'taskname>>),
}

impl<'taskname> IpcCounters<'taskname> {
    fn empty() -> Self {
        Self::Nested(Default::default())
    }

    fn sort(&mut self, order: Order) {
        match self {
            IpcCounters::Single(ref mut tasks) => match order {
                Order::Decl => {}
                Order::Value => {
                    tasks.sort_unstable_by(|_, a, _, b| a.cmp(b).reverse());
                }
                Order::Alpha => {
                    tasks.sort_unstable_by(|a, _, b, _| a.cmp(b));
                }
            },
            IpcCounters::Nested(ref mut map) => {
                for v in map.values_mut() {
                    v.sort(order);
                }
                match order {
                    Order::Decl => {}
                    Order::Value => {
                        map.sort_unstable_by(|_, a, _, b| {
                            a.total().cmp(&b.total()).reverse()
                        });
                    }
                    Order::Alpha => {
                        map.sort_unstable_by(|a, _, b, _| a.cmp(b));
                    }
                }
            }
        }
    }

    fn populate(
        map: &mut IndexMap<String, IpcCounters<'taskname>>,
        taskname: &'taskname str,
        ctrs: Counters,
        full: bool,
    ) {
        for (name, count) in ctrs.counts {
            if !full && count.total() == 0 {
                continue;
            }
            match count {
                CounterVariant::Single(val) => {
                    match map.entry(name).or_insert_with(|| {
                        IpcCounters::Single(Default::default())
                    }) {
                        IpcCounters::Single(ref mut tasks) => {
                            tasks.insert(taskname, val);
                        }
                        _ => panic!("expected single IPC counters"),
                    }
                }
                CounterVariant::Nested(vals) => {
                    if let IpcCounters::Nested(ref mut map) =
                        map.entry(name).or_insert_with(Self::empty)
                    {
                        Self::populate(map, taskname, vals, full)
                    } else {
                        unreachable!()
                    }
                }
            }
        }
    }

    fn total(&self) -> u32 {
        match self {
            IpcCounters::Single(ref tasks) => tasks.values().sum(),
            IpcCounters::Nested(ref map) => map.values().map(Self::total).sum(),
        }
    }

    fn fmt_counters(
        &self,
        prefix: &str,
        indent: usize,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        let total_len = f.width().unwrap_or(8);
        let mut has_written_any = false;
        let print_arrow = |f: &mut fmt::Formatter<'_>, indent: usize| {
            for _ in 1..indent {
                f.write_str(" |     ")?;
            }
            f.write_str(" +---> ")?;
            Ok(())
        };
        let total = self.total();

        let print_parens = |f: &mut fmt::Formatter<'_>| {
            for _ in 0..prefix.matches('(').count() {
                write!(
                    f,
                    "{}",
                    if prefix.contains("Err") {
                        ")".red()
                    } else {
                        ")".green()
                    }
                )?;
            }
            Ok(())
        };

        let print_single_prefix = |f: &mut fmt::Formatter<'_>| {
            if !prefix.is_empty() {
                write!(
                    f,
                    "{}",
                    if prefix.contains("Err") {
                        prefix.red()
                    } else {
                        prefix.green()
                    }
                )?;
                print_parens(f)?;
                f.write_str(" ")?;
            }
            Ok::<_, fmt::Error>(())
        };
        match self {
            IpcCounters::Single(tasks) if tasks.len() == 1 => {
                let (task, counter) = tasks.iter().next().unwrap();
                write!(f, "{counter:>total_len$}")?;
                print_arrow(f, indent)?;
                print_single_prefix(f)?;
                writeln!(f, "[{task}]")?;
            }

            IpcCounters::Single(tasks) => {
                if !prefix.is_empty() {
                    write!(f, "{total:>total_len$}")?;
                    print_arrow(f, indent)?;
                    print_single_prefix(f)?;
                    f.write_str("\n")?;
                }

                for (&task, &count) in tasks {
                    if has_written_any {
                        f.write_str("\n")?;
                    } else {
                        has_written_any = true
                    };
                    write!(f, "{count:>total_len$}",)?;
                    print_arrow(f, indent + 1)?;
                    write!(f, "[{task}]",)?;
                }

                if has_written_any {
                    f.write_str("\n")?;
                }
            }
            IpcCounters::Nested(ref counts) if counts.len() == 1 => {
                let (name, counter) = counts.iter().next().unwrap();
                let indent = if matches!(counter, IpcCounters::Single(counts) if counts.len() > 1)
                {
                    indent + 1
                } else {
                    indent
                };
                if prefix.is_empty() {
                    counter.fmt_counters(name, indent, f)?;
                } else {
                    counter.fmt_counters(
                        &format!("{prefix}({name}"),
                        indent,
                        f,
                    )?;
                }
            }

            IpcCounters::Nested(ref counts) => {
                if !prefix.is_empty() {
                    write!(f, "{total:>total_len$}")?;
                    print_arrow(f, indent)?;
                    if prefix.contains("Err") {
                        write!(f, "{}{}", prefix.red(), "(_)".red())?;
                    } else {
                        write!(f, "{}{}", prefix.green(), "(_)".green())?;
                    }
                    print_parens(f)?;
                    writeln!(f)?;
                }
                for (name, counter) in counts {
                    if prefix.is_empty() {
                        counter.fmt_counters(name, indent + 1, f)?;
                    } else {
                        counter.fmt_counters(
                            &format!("{prefix}({name}"),
                            indent + 1,
                            f,
                        )?;
                    }
                }
            }
        };

        Ok(())
    }
}

impl IpcIface<'_> {
    fn sort(&mut self, order: Order) {
        for ctrs in self.counters.values_mut() {
            ctrs.sort(order);
        }
        match order {
            Order::Decl => {}
            Order::Value => {
                self.counters.sort_unstable_by(|_, a, _, b| {
                    a.total().cmp(&b.total()).reverse()
                });
            }
            Order::Alpha => {
                self.counters.sort_unstable_by(|a, _, b, _| a.cmp(b));
            }
        }
    }
}

impl fmt::Display for IpcIface<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { name, counters } = self;
        for (ipc, ctrs) in counters {
            let total = ctrs.total();
            if total == 0 && !f.alternate() {
                continue;
            }
            writeln!(f, "{total:>8} {}::{}()", name.bold(), ipc.bold(),)?;
            ctrs.fmt_counters("", 0, f)?;
            writeln!(f)?;
        }
        Ok(())
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
