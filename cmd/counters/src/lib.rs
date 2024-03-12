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
//! The `--ipc` argument shows IPC client counters generated automatically by
//! `idol`, showing the total request count for a given IPC and per-client-task
//! breakdowns. For example:`
//!
//! ```console
//! $ humility -d ./hubris.core.0 counters --ipc`
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
//! $ humility -d ./hubris.core.0 counters --ipc sensors
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
//! $ humility -d ./hubris.core.0 counters --ipc --client gimlet_seq
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

use anyhow::{bail, Result};
use clap::{CommandFactory, Parser, ValueEnum};
use humility::core::Core;
use humility::hubris::*;
use humility::reflect::{self, Load, Value};
use humility_cli::{ExecutionContext, Subcommand};
use humility_cmd::{Archive, Attach, Command, CommandKind, Validate};
use humility_doppel::{CountedRingbuf, CounterVariant, Counters};
use std::collections::BTreeMap;

mod ipc;

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

    /// when used with `--ipc`, show only IPC counters originating from tasks
    /// whose name contain the given substring.
    ///
    /// multiple values may be provided to select more than one client task.
    #[clap(long, short, conflicts_with = "list", requires = "ipc")]
    client: Vec<String>,
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
        return ipc::ipc_counter_dump(hubris, core, &subargs);
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
