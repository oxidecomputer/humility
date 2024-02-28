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

use anyhow::{bail, Result};
use clap::{CommandFactory, Parser, ValueEnum};
use humility::core::Core;
use humility::hubris::*;
use humility::reflect::{self, Load, Value};
use humility_cli::{ExecutionContext, Subcommand};
use humility_cmd::{Archive, Attach, Command, CommandKind, Validate};
use humility_doppel::{CountedRingbuf, CounterVariant, Counters};
use std::collections::BTreeMap;

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
    let mut buf: Vec<u8> = vec![];
    buf.resize_with(var.size, Default::default);

    core.halt()?;
    core.read_8(var.addr, buf.as_mut_slice())?;
    core.run()?;

    let val: Value = Value::Struct(reflect::load_struct(hubris, &buf, def, 0)?);

    // Counters may either be a standalone counters variable or a counted
    // ringbuf. We'll try to interpret the var as either one.
    let mut counters = CountedRingbuf::from_value(&val)
        .map(|r| r.counters)
        .or_else(|_| Counters::from_value(&val))?;

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
