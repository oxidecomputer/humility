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
        let total_len = f.width().unwrap_or(8);
        for (method_name, ctrs) in &self.methods {
            let total = ctrs.total();
            if total == 0 && !f.alternate() {
                continue;
            }
            let errors = ctrs.total_errors();
            let ok = total - errors;
            let ok_str = ok.to_string();
            let err_str = errors.to_string();
            writeln!(
                f,
                "{total:>total_len$} {}::{}() {:.<pad$} {} ok {:.>pad2$}{} err",
                self.name.bold(),
                method_name.bold(),
                "".dimmed(),
                ok_str.green(),
                " ".dimmed(),
                err_str.red(),
                pad = 80
                    - (total_len * 3)
                    - ok_str.len()
                    - self.name.len()
                    - method_name.len()
                    - 8,
                pad2 = total_len - err_str.len(),
            )?;
            for ((task, gen), ctrs) in &ctrs.0 {
                let total = ctrs.total();
                if total == 0 && !f.alternate() {
                    continue;
                }

                let errors = ipc_error_count(ctrs);
                let ok = total - errors;
                let ok_str = ok.to_string();
                let err_str = errors.to_string();
                let restarts = match gen {
                    GenOrRestartCount::Gen(gen) => {
                        format!(" (gen {gen:?})")
                    }
                    GenOrRestartCount::RestartCount(restarts) => {
                        format!(" ({restarts} restarts)")
                    }
                };
                writeln!(
                    f,
                    "{:>total_len$} task {}{restarts}{:.<pad$} {} ok {:.>pad2$}{} err",
                    "",
                    task.italic(),
                    " ".dimmed(),
                    ok_str.green(),
                    " ".dimmed(),
                    err_str.red(),
                    pad = 80
                        - (total_len * 3)
                        - ok_str.len()
                        - task.len()
                        - restarts.len()
                        - 8,
                    pad2 = total_len - err_str.len(),
                )?;

                match ctrs {
                    CounterVariant::Nested(map) if f.alternate() => {
                        writeln!(f, "{:#}", map.display_padded("         - "))?
                    }
                    CounterVariant::Nested(map)
                        if map
                            .counts
                            .iter()
                            .filter(|(_, c)| c.total() > 1)
                            .count()
                            > 1 =>
                    {
                        writeln!(f, "{}", ctrs.display_padded("         - "))?
                    }
                    _ => {}
                }
            }
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

// enum IpcCounters<'taskname> {
//     Single(IndexMap<&'taskname str, u32>),
//     Nested(IpcCounterMap<'taskname>),
// }

// const REQ_ARROW: &str = "<---+";
// const RSP_ARROW: &str = "+--->";

// impl<'taskname> IpcCounters<'taskname> {
//     fn sort(&mut self, order: Order) {
//         match self {
//             Self::Single(ref mut tasks) => match order {
//                 Order::Decl => {}
//                 Order::Value => {
//                     tasks.sort_unstable_by(|_, a, _, b| a.cmp(b).reverse());
//                 }
//                 Order::Alpha => {
//                     tasks.sort_unstable_by(|a, _, b, _| a.cmp(b));
//                 }
//             },
//             Self::Nested(ref mut map) => map.sort(order),
//         }
//     }

//     fn total(&self) -> u32 {
//         match self {
//             Self::Single(ref tasks) => tasks.values().sum(),
//             Self::Nested(ref ctrs) => ctrs.total(),
//         }
//     }

//     fn fmt_counters(
//         &self,
//         prefix: &str,
//         indent: usize,
//         f: &mut fmt::Formatter<'_>,
//     ) -> fmt::Result {
//         let total_len = f.width().unwrap_or(8);
//         let total = self.total();
//         let print_single_prefix = |f: &mut fmt::Formatter<'_>| {
//             if !prefix.is_empty() {
//                 write!(
//                     f,
//                     "{}",
//                     if prefix.contains("Err") {
//                         prefix.red()
//                     } else {
//                         prefix.green()
//                     }
//                 )?;
//                 print_parens(f, prefix)?;
//                 f.write_str(" ")?;
//             }
//             Ok::<_, fmt::Error>(())
//         };
//         match self {
//             IpcCounters::Single(tasks) if tasks.len() == 1 => {
//                 let (task, counter) = tasks.iter().next().unwrap();
//                 write!(f, "{counter:>total_len$} ")?;
//                 print_indent(f, indent)?;
//                 if !prefix.is_empty() {
//                     write!(f, "{RSP_ARROW} ")?;
//                     print_single_prefix(f)?;
//                 }
//                 writeln!(f, "{REQ_ARROW} [{task}]",)?;
//             }

//             IpcCounters::Single(tasks) => {
//                 if !prefix.is_empty() {
//                     write!(f, "{total:>total_len$} ")?;
//                     print_indent(f, indent)?;
//                     write!(f, "{RSP_ARROW} ")?;
//                     print_single_prefix(f)?;
//                     f.write_str("\n")?;
//                 }

//                 let mut has_written_any = false;
//                 for (&task, &count) in tasks {
//                     if has_written_any {
//                         f.write_str("\n")?;
//                     } else {
//                         has_written_any = true
//                     };
//                     write!(f, "{count:>total_len$} ",)?;
//                     print_indent(f, indent + 1)?;
//                     write!(f, "{REQ_ARROW} [{task}]",)?;
//                 }

//                 if has_written_any {
//                     f.write_str("\n")?;
//                 }
//             }
//             IpcCounters::Nested(ref counts) => {
//                 counts.fmt_counters(prefix, indent, f)?
//             }
//         };

//         Ok(())
//     }
// }

// impl fmt::Display for IpcIface<'_> {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         let Self { name, counters: IpcCounterMap(counters) } = self;
//         for (ipc, ctrs) in counters {
//             let total = ctrs.total();
//             writeln!(f, "{total:>8} {}::{}()", name.bold(), ipc.bold(),)?;
//             ctrs.fmt_counters("", 0, f)?;
//             writeln!(f)?;
//         }
//         Ok(())
//     }
// }

// impl<'taskname> IpcCounterMap<'taskname> {
//     fn total(&self) -> u32 {
//         self.0.values().map(IpcCounters::total).sum()
//     }

//     fn sort(&mut self, order: Order) {
//         for v in self.0.values_mut() {
//             v.sort(order);
//         }
//         match order {
//             Order::Decl => {}
//             Order::Value => {
//                 self.0.sort_unstable_by(|_, a, _, b| {
//                     a.total().cmp(&b.total()).reverse()
//                 });
//             }
//             Order::Alpha => {
//                 self.0.sort_unstable_by(|a, _, b, _| a.cmp(b));
//             }
//         }
//     }

//     fn populate(
//         &mut self,
//         taskname: &'taskname str,
//         ctrs: Counters,
//         full: bool,
//     ) {
//         for (name, count) in ctrs.counts {
//             if !full && count.total() == 0 {
//                 continue;
//             }
//             match count {
//                 CounterVariant::Single(val) => {
//                     match self.0.entry(name).or_insert_with(|| {
//                         IpcCounters::Single(Default::default())
//                     }) {
//                         IpcCounters::Single(ref mut tasks) => {
//                             tasks.insert(taskname, val);
//                         }
//                         _ => panic!("expected single IPC counters"),
//                     }
//                 }
//                 CounterVariant::Nested(vals) => {
//                     if let IpcCounters::Nested(ref mut map) =
//                         self.0.entry(name).or_insert_with(|| {
//                             IpcCounters::Nested(Default::default())
//                         })
//                     {
//                         map.populate(taskname, vals, full);
//                     } else {
//                         unreachable!()
//                     }
//                 }
//             }
//         }
//     }

//     fn fmt_counters(
//         &self,
//         prefix: &str,
//         indent: usize,
//         f: &mut fmt::Formatter<'_>,
//     ) -> fmt::Result {
//         let Self(counts) = self;
//         if counts.len() == 1 {
//             let (name, counter) = counts.iter().next().unwrap();
//             let indent = if matches!(counter, IpcCounters::Single(counts) if counts.len() > 1)
//             {
//                 indent + 1
//             } else {
//                 indent
//             };
//             if prefix.is_empty() {
//                 counter.fmt_counters(name, indent, f)?;
//             } else {
//                 counter.fmt_counters(&format!("{prefix}({name}"), indent, f)?;
//             }

//             return Ok(());
//         }

//         let total_len = f.width().unwrap_or(8);

//         if !prefix.is_empty() {
//             write!(f, "{:>total_len$} ", self.total())?;
//             print_indent(f, indent)?;
//             f.write_str(RSP_ARROW)?;
//             if prefix.contains("Err") {
//                 write!(f, " {}{}", prefix.red(), "(_)".red())?;
//             } else {
//                 write!(f, " {}{}", prefix.green(), "(_)".green())?;
//             }
//             print_parens(f, prefix)?;
//             writeln!(f)?;
//         }
//         for (name, counter) in counts {
//             if prefix.is_empty() {
//                 counter.fmt_counters(name, indent + 1, f)?;
//             } else {
//                 counter.fmt_counters(
//                     &format!("{prefix}({name}"),
//                     indent + 1,
//                     f,
//                 )?;
//             }
//         }
//         Ok(())
//     }
// }

// fn print_indent(f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
//     for _ in 1..indent {
//         f.write_str("|     ")?
//     }
//     Ok(())
// }

// fn print_parens(f: &mut fmt::Formatter<'_>, prefix: &str) -> fmt::Result {
//     for _ in 0..prefix.matches('(').count() {
//         write!(
//             f,
//             "{}",
//             if prefix.contains("Err") { ")".red() } else { ")".green() }
//         )?;
//     }
//     Ok(())
// }

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
