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
//! #### IPC counters
//!
//! The `ipc` subcommand shows IPC client counters generated automatically by
//! `idol`, showing the total request count for a given IPC and per-client-task
//! breakdowns. For example:`
//!
//! ```console
//! $ humility -d ./hubris.core.0 counters ipc`
//! humility: attached to dump
//! drv_gimlet_hf_api::__HOSTFLASH_CLIENT_COUNTERS
//!  fn HostFlash::get_mux() .............................................. 6 calls
//!     clients:
//!     task host_sp_comms (0 restarts) .................... = 0 ........... = 6 ok
//!
//!  fn HostFlash::set_mux() .............................................. 2 calls
//!     clients:
//!     task gimlet_seq (0 restarts) ....................... + 0 ........... + 1 ok
//!     task host_sp_comms (0 restarts) .................... + 0 ........... + 1 ok
//!                                                          ---             ---
//!     totals:                                              = 0 err         = 2 ok
//!
//!  fn HostFlash::get_dev() .............................................. 1 calls
//!     clients:
//!     task host_sp_comms (0 restarts) .................... = 0 ........... = 1 ok
//!
//!
//! drv_gimlet_seq_api::__SEQUENCER_CLIENT_COUNTERS
//!  fn Sequencer::get_state() ......................................... 2017 calls
//!     clients:
//!     task thermal (0 restarts) .......................... + 0 ........ + 1386 ok
//!     task power (0 restarts) ............................ + 0 ......... + 626 ok
//!     task host_sp_comms (0 restarts) .................... + 0 ........... + 5 ok
//!                                                          ---          ------
//!     totals:                                              = 0 err      = 2017 ok
//! ...
//! ```
//!
//! When displaying counters by IPC, substring filtering is performed on the
//! counters variable, but *not* on the client task name. This allows filtering
//! the output based on the IPC interface. For example:
//!
//! ```console
//! $ humility -d ./hubris.core.0 counters ipc sensors
//! humility: attached to dump
//! task_sensor_api::__SENSOR_CLIENT_COUNTERS
//! fn Sensor::post() ................................................ 76717 calls
//!    clients:
//!    task power (0 restarts) ............................ + 0 ....... + 50300 ok
//!    task thermal (0 restarts) .......................... + 0 ....... + 26417 ok
//!                                                         ---         -------
//!    totals:                                              = 0 err     = 76717 ok
//!
//! fn Sensor::get_reading() ......................................... 19804 calls
//!    clients:
//!    task thermal (0 restarts) ...................................... = 18101 ok
//!    - Err(NotPresent) ............................... + 1701 ..................
//!    - Err(DeviceError) ................................. + 2 ..................
//!                                                      ------         -------
//!    totals:                                           = 1703 err     = 18101 ok
//!
//! fn Sensor::nodata() ............................................... 6225 calls
//!    clients:
//!    task power (0 restarts) ............................ + 0 ........ + 3536 ok
//!    task thermal (0 restarts) .......................... + 0 ........ + 2689 ok
//!                                                         ---          ------
//!    totals:                                              = 0 err      = 6225 ok
//!
//! ```
//!
//! Instead, to show only the IPC counters _recorded_ by specific client tasks,
//! use the `--client` argument, which will filter the output counters to those
//! recorded in tasks whose names match the provided strings. For example, to
//! show only IPC counters recorded by the `gimlet_seq` task, use:
//!
//!```console
//! $ humility -d ./hubris.core.0 counters ipc --client gimlet_seq
//! humility: attached to dump
//! drv_gimlet_hf_api::__HOSTFLASH_CLIENT_COUNTERS
//!  fn HostFlash::set_mux() .............................................. 1 calls
//!     clients:
//!     task gimlet_seq (0 restarts) ....................... = 0 ........... = 1 ok
//!
//!
//! drv_spi_api::__SPI_CLIENT_COUNTERS
//!  fn Spi::exchange() ............................................... 67580 calls
//!     clients:
//!     task gimlet_seq (0 restarts) ....................... = 0 ....... = 67580 ok
//!
//!  fn Spi::write() .................................................... 530 calls
//!     clients:
//!     task gimlet_seq (0 restarts) ....................... = 0 ......... = 530 ok
//!
//!  fn Spi::lock() ....................................................... 4 calls
//!     clients:
//!     task gimlet_seq (0 restarts) ....................... = 0 ........... = 4 ok
//!
//!  fn Spi::release() .................................................... 1 calls
//!     clients:
//!     task gimlet_seq (0 restarts) ....................... = 0 ........... = 1 ok
//!
//!
//! drv_stm32xx_sys_api::__SYS_CLIENT_COUNTERS
//!  fn Sys::gpio_read_input() ........................................ 16796 calls
//!     clients:
//!     task gimlet_seq (0 restarts) ....................... = 0 ....... = 16796 ok
//!
//!  fn Sys::gpio_set_reset() ............................................ 15 calls
//!     clients:
//!     task gimlet_seq (0 restarts) ....................... = 0 .......... = 15 ok
//!
//!  fn Sys::gpio_configure_raw() ........................................ 14 calls
//!     clients:
//!     task gimlet_seq (0 restarts) ....................... = 0 .......... = 14 ok
//!
//!
//! task_jefe_api::__JEFE_CLIENT_COUNTERS
//!  fn Jefe::set_state() ................................................. 5 calls
//!     clients:
//!     task gimlet_seq (0 restarts) ....................... = 0 ........... = 5 ok
//!
//!
//! task_packrat_api::__PACKRAT_CLIENT_COUNTERS
//!  fn Packrat::set_spd_eeprom() ........................................ 32 calls
//!     clients:
//!     task gimlet_seq (0 restarts) ....................... = 0 .......... = 32 ok
//!
//!  fn Packrat::set_mac_address_block() .................................. 1 calls
//!     clients:
//!     task gimlet_seq (0 restarts) ....................... = 0 ........... = 1 ok
//!
//!  fn Packrat::set_identity() ........................................... 1 calls
//!     clients:
//!     task gimlet_seq (0 restarts) ....................... = 0 ........... = 1 ok
//! ```
//!
//! Multiple `--client` arguments may be provided, to show IPCs from any of a
//! set of client tasks.
//!
//! `--client` may be combined with a filter matching counter variable names, to
//! show only the calls to specific IPC interfaces from specific tasks.

use anyhow::{Result, bail};
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

mod ipc;

#[derive(Parser, Debug)]
#[clap(name = "counters", about = env!("CARGO_PKG_DESCRIPTION"))]
// This attribute means that any the args defined in `Options` will conflict
// with the `list` subcommand.
#[clap(args_conflicts_with_subcommands = true)]
struct CountersArgs {
    #[clap(subcommand)]
    command: Option<Subcmd>,

    #[clap(flatten)]
    opts: Options,

    /// select the format to output counters.
    ///
    /// * "text": format counters in a human-readable ASCII text format (the
    ///   default)
    ///
    /// * "csv": outputs counters in comma-separated values (CSV) format,
    ///   suitable for use with other tools.
    ///
    /// * "json": outputs counters in JSON format, suitable for use with
    ///   other tools.
    ///
    /// [conflicts with: ipc]
    #[clap(long, short, value_enum, default_value_t = Output::Text)]
    output: Output,
}

/// Arguments that conflict with the `list` subcommand, but apply to both the
/// default and to the `ipc` subcommand.
#[derive(Parser, Debug)]
struct Options {
    /// show only counters whose names include the provided substring.
    name: Option<String>,

    /// print full errors
    #[clap(long, short)]
    verbose: bool,

    /// show counters with zero values
    #[clap(long, short)]
    full: bool,

    /// sort counters using the provided ordering.
    ///
    /// [default: `decl` if `--full` is set, `alpha` otherwise]
    #[clap(long, short, value_enum)]
    sort: Option<Order>,
}

#[derive(clap::Subcommand, Debug)]
enum Subcmd {
    /// list all counters without displaying their values
    List {
        /// show only counters whose names include the provided substring.
        name: Option<String>,

        /// select the format to output counters.
        ///
        /// * "text": format counters in a human-readable ASCII text format (the
        ///   default)
        ///
        /// * "csv": outputs counters in comma-separated values (CSV) format,
        ///   suitable for use with other tools.
        ///
        /// * "json": outputs counters in JSON format, suitable for use with
        ///   other tools.
        #[clap(long, short, value_enum, default_value_t = Output::Text)]
        output: Output,
    },

    /// show IPC counters, grouped by IPC interface rather than by counter.
    Ipc(ipc::Args),
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

impl CountersArgs {
    fn name(&self) -> Option<&str> {
        match self.command {
            Some(Subcmd::List { ref name, .. }) => name.as_deref(),
            Some(Subcmd::Ipc(ref ipc)) => ipc.opts.name.as_deref(),
            _ => self.opts.name.as_deref(),
        }
    }
}
#[derive(Copy, Clone, Debug, PartialEq, Eq, ValueEnum)]
#[clap(rename_all = "kebab-case")]
enum Output {
    /// Output human-readable ASCII text (the default).
    Text,
    /// Output comma-separated values (CSV).
    Csv,
    /// Output JSON.
    Json,
}

// Help message printed out when no counters match a filter.
const LIST_HINT: &str = "use `humility counters list` to list all \
    available counters";

fn counters(context: &mut ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let hubris = context.archive.as_ref().unwrap();

    let subargs = CountersArgs::try_parse_from(subargs)?;

    if let Some(Subcmd::Ipc(ipc)) = subargs.command {
        return ipc.ipc_counter_dump(hubris, core);
    }
    let name = subargs.name();

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
        if let Some(ref name) = name {
            if varname.contains(name) || t.contains(name) {
                counters.entry(t).or_default().push(((varname, var), def));
            }
        } else {
            counters.entry(t).or_default().push(((varname, var), def));
        }
    }

    if counters.is_empty() {
        if let Some(name) = name {
            bail!(
                "no counters found with names containing \"{name}\"\n\
                {} {LIST_HINT}",
                hint(),
            );
        } else {
            bail!("no counters found");
        }
    }

    for vars in counters.values_mut() {
        vars.sort_by_key(|&(v, _)| v);
    }

    if let Some(Subcmd::List { output, .. }) = subargs.command {
        match output {
            Output::Text => {
                for (t, ctrs) in &counters {
                    println!("{t}:");
                    println!("    {:<10} {:>5} VARIABLE", "ADDR", "SIZE",);
                    for ((name, HubrisVariable { addr, size, .. }), _) in ctrs {
                        println!("    {addr:<#010x} {size:>5} {name}",)
                    }
                }
            }
            Output::Csv => {
                println!("task,variable,addr,size");
                for (t, ctrs) in &counters {
                    for ((name, HubrisVariable { addr, size, .. }), _) in ctrs {
                        println!("{t},{name},{addr:#x},{size}");
                    }
                }
            }
            Output::Json => {
                let mut values = Vec::new();
                for (t, ctrs) in counters {
                    for ((name, HubrisVariable { addr, size, .. }), _) in ctrs {
                        values.push(serde_json::json!({
                            "task": t,
                            "name": name,
                            "addr": addr,
                            "size": size,
                        }));
                    }
                }

                serde_json::to_writer_pretty(std::io::stdout(), &values)?;
            }
        }
        return Ok(());
    }

    if subargs.output == Output::Csv {
        println!("task,variable,variant,count");
    }

    let mut json: IndexMap<&str, IndexMap<_, _>> = IndexMap::new();
    for (t, ctrs) in counters {
        // Try not to use `?` here, because it causes one bad counter to make
        // them all unavailable. Instead, construct an iterator of
        // `Result<Counters, Error>`.
        let resolved_counters = ctrs.iter().map(|((varname, var), def)| {
            let ctr = def
                .ok_or_else(|| {
                    anyhow::anyhow!("could not look up type: {:?}", var.goff)
                })
                .and_then(|def| load_counters(hubris, core, def, var));
            (varname, ctr)
        });
        match subargs.output {
            Output::Csv => {
                for (varname, ctr) in resolved_counters {
                    match ctr {
                        Err(e) if subargs.opts.verbose => {
                            humility::warn!("counter dump failed: {e:?}")
                        }
                        Err(e) => humility::warn!("counter dump failed: {e}"),
                        Ok(mut ctr) => {
                            counters_dump_csv(
                                &mut ctr,
                                varname,
                                t,
                                &subargs.opts,
                            );
                        }
                    }
                }
            }
            Output::Json => {
                for (varname, ctr) in resolved_counters {
                    match ctr {
                        Err(e) if subargs.opts.verbose => {
                            humility::warn!("counter dump failed: {e:?}")
                        }
                        Err(e) => humility::warn!("counter dump failed: {e}"),
                        Ok(ctr) => {
                            json.entry(t)
                                .or_default()
                                .insert(varname.to_owned(), ctr);
                        }
                    }
                }
            }
            Output::Text => {
                println!("{t}\n |");
                let mut ctrs = resolved_counters.peekable();
                while let Some((varname, ctr)) = ctrs.next() {
                    println!(" +---> {varname}:");
                    let pad =
                        if ctrs.peek().is_some() { " |  " } else { "    " };
                    match ctr {
                        Err(e) if subargs.opts.verbose => {
                            humility::warn!("counter dump failed: {e:?}")
                        }
                        Err(e) => humility::warn!("counter dump failed: {e}"),
                        Ok(mut ctr) => {
                            counter_dump(&mut ctr, &subargs.opts, pad)
                        }
                    }
                }
            }
        }
    }

    if !json.is_empty() {
        assert_eq!(subargs.output, Output::Json);
        serde_json::to_writer_pretty(std::io::stdout(), &json)?;
    }
    Ok(())
}

fn counters_dump_csv(
    counters: &mut Counters,
    varname: &str,
    taskname: &str,
    opts: &Options,
) {
    // Sort the counters.
    match opts.sort {
        Some(Order::Value) => counters.sort_by(
            &mut |_, a: &CounterVariant, _, b: &CounterVariant| {
                a.total().cmp(&b.total()).reverse()
            },
        ),
        Some(Order::Alpha) => counters.sort_by(&mut |a, _, b, _| a.cmp(b)),
        _ => {}
    }

    fn csv_dump(
        taskname: &str,
        varname: &str,
        prefix: &mut String,
        counters: &Counters,
        opts: &Options,
    ) {
        for (name, ctr) in &counters.counts {
            if ctr.total() == 0 && !opts.full {
                continue;
            }
            match ctr {
                CounterVariant::Single(count) => {
                    print!("{taskname},{varname},{prefix}{name}");
                    for _ in 0..prefix.matches('(').count() {
                        print!(")");
                    }
                    println!(",{count}");
                }
                CounterVariant::Nested(counts) => {
                    let pfxlen = prefix.len();
                    prefix.push_str(name);
                    prefix.push('(');
                    csv_dump(taskname, varname, prefix, counts, opts);
                    prefix.truncate(pfxlen);
                }
            }
        }
    }

    csv_dump(taskname, varname, &mut String::new(), counters, opts);
}

fn counter_dump(counters: &mut Counters, opts: &Options, pad: &str) {
    // Sort the counters.
    match opts {
        // If `--full` is set, zero valued counters are displayed, so it's nice
        // to always display them in the same order across dumps/Humility
        // processes, so that the output is easily comparable. Therefore, sort
        // by declaration order, if no sorting was requested.
        Options { sort: Some(Order::Decl), .. }
        | Options { sort: None, full: true, .. } => {
            // Counters are already sorted by declaration order.
        }
        // If `--full` is not set, the output is not comparable across
        // dumps/Hubris processes, because which counters are present varies
        // based on the state of the system.
        //
        // Therefore, sort by value by default, so the highest-valued counters
        // are shown first.
        Options { sort: Some(Order::Value), .. }
        | Options { sort: None, full: false, .. } => counters.sort_by(
            &mut |_, a: &CounterVariant, _, b: &CounterVariant| {
                a.total().cmp(&b.total()).reverse()
            },
        ),
        Options { sort: Some(Order::Alpha), .. } => {
            counters.sort_by(&mut |a, _, b, _| a.cmp(b))
        }
    }

    let disp = counters.display_padded(pad);
    if opts.full {
        println!("{disp:#}");
    } else if counters.total() > 0 {
        println!("{disp}");
    } else {
        println!("{pad}   <no counts recorded>")
    }
}

fn taskname<'a>(
    hubris: &'a HubrisArchive,
    variable: &'a HubrisVariable,
) -> Result<&'a str> {
    Ok(&hubris.lookup_module(HubrisTask::from(variable.goff))?.name)
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

fn hint() -> impl std::fmt::Display {
    "hint:".bold()
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
