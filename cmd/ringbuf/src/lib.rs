// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility ringbuf`
//!
//! `humility ringbuf` reads and displays any Hubris ring buffers (as created
//! via the `ringbuf!` macro in the Hubris `ringbuf` crate).  e.g.:
//!
//! ```console
//! $ humility -d ./hubris.core.5 ringbuf
//! ADDR        NDX LINE  GEN    COUNT PAYLOAD
//! 0x2000a288  552   92    1        5 (21.5, 70.69999694824219)
//! 0x2000a298  553   92    1        1 (21.4375, 70.58749389648438)
//! 0x2000a2a8  554   92    1        1 (21.5, 70.69999694824219)
//! 0x2000a2b8  555   92    1        1 (21.4375, 70.58749389648438)
//! 0x2000a2c8  556   92    1        5 (21.5, 70.69999694824219)
//! 0x2000a2d8  557   92    1        1 (21.5625, 70.8125)
//! 0x2000a2e8  558   92    1       15 (21.5, 70.69999694824219)
//! 0x2000a2f8  559   92    1        1 (21.4375, 70.58749389648438)
//! 0x2000a308  560   92    1       10 (21.5, 70.69999694824219)
//! 0x2000a318  561   92    1        2 (21.4375, 70.58749389648438)
//! 0x2000a328  562   92    1        2 (21.5, 70.69999694824219)
//! 0x2000a338  563   92    1        1 (21.4375, 70.58749389648438)
//! 0x2000a348  564   92    1        9 (21.5, 70.69999694824219)
//! 0x2000a358  565   92    1        3 (21.4375, 70.58749389648438)
//! 0x2000a368  566   92    1        4 (21.5, 70.69999694824219)
//! 0x2000a378  567   92    1        1 (21.4375, 70.58749389648438)
//! ...
//! ```
//!
//! If an argument is provided, only ring buffers that have a name that
//! contains the argument as a substring, or are in a task that contains
//! the argument as a substring will be displayed.  For example,
//! to display every ring buffer that has `i2c` in the name or the
//! containing task:
//!
//! ```console
//! $ humility -d ./hubris.core.76 ringbuf ksz
//! humility: attached to dump
//! humility: ring buffer ksz8463::__RINGBUF in net:
//!  NDX LINE      GEN    COUNT PAYLOAD
//!    2  134       89        1 Read(IADR5, 0x4000)
//!    3  134       89        1 Read(IADR4, 0x0)
//!    4  134       89        1 Read(P1MBSR, 0x780c)
//!    5  148       89        1 Write(IACR, 0x1c00)
//!    6  134       89        1 Read(IADR5, 0x4000)
//!    7  134       89        1 Read(IADR4, 0x0)
//!    8  148       89        1 Write(IACR, 0x1c14)
//! ...
//! ```
//!
//! See the [`ringbuf`
//! documentation](https://github.com/oxidecomputer/hubris/blob/master/lib/ringbuf/src/lib.rs) for more details.

use anyhow::{bail, Context, Result};
use clap::{CommandFactory, Parser};
use humility::core::Core;
use humility::hubris::*;
use humility::reflect::{self, Format, Load, Value};
use humility_cli::{ExecutionContext, Subcommand};
use humility_cmd::{Archive, Attach, Command, CommandKind, Validate};
use humility_doppel::{CountedRingbuf, Ringbuf, StaticCell};

#[derive(Parser, Debug)]
#[clap(name = "ringbuf", about = env!("CARGO_PKG_DESCRIPTION"))]
struct RingbufArgs {
    /// list variables
    #[clap(long, short)]
    list: bool,
    /// print full errors
    #[clap(long, short)]
    verbose: bool,
    /// print only a single ringbuffer by substring of name
    #[clap(conflicts_with = "list")]
    name: Option<String>,
    #[clap(flatten)]
    totals: TotalsOptions,
}

#[derive(Parser, Debug, Clone, Copy)]
#[clap(next_help_heading = "DISPLAYING ENTRY TOTALS")]
struct TotalsOptions {
    /// when displaying totals, don't skip entry variants which have
    /// not been recorded
    #[clap(long, short, conflicts_with = "list")]
    full_totals: bool,
    /// only display entry variant totals, and not ringbuf contents
    #[clap(long, short, conflicts_with = "list")]
    totals_only: bool,
    /// skip displaying total counts for ringbuf entry variants
    #[clap(
        long,
        short,
        conflicts_with_all = &[
            "full-totals",
            "totals-only",
            "list"
        ],
    )]
    no_totals: bool,
}

fn ringbuf_dump(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    definition: &HubrisStruct,
    ringbuf_var: &HubrisVariable,
    TotalsOptions { full_totals, totals_only, no_totals }: TotalsOptions,
) -> Result<()> {
    let mut buf: Vec<u8> = vec![];
    buf.resize_with(ringbuf_var.size, Default::default);

    core.halt()?;
    core.read_8(ringbuf_var.addr, buf.as_mut_slice())?;
    core.run()?;

    // There are three possible shapes of ringbufs, depending on the age of the
    // firmware.
    // - Raw Ringbuf that is not wrapped by anything.
    // - Safe Ringbuf that is inside a StaticCell.
    // - CountedRingbuf, which consists of a `StaticCell<Ringbuf>` and a set of
    //   entry counters.
    //
    // Here we will attempt to handle all three -- first the counted ringbuf,
    // then raw, then fallback.
    let ringbuf_val: Value =
        Value::Struct(reflect::load_struct(hubris, &buf, definition, 0)?);

    let (ringbuf, counters) = {
        let maybe_counted = CountedRingbuf::from_value(&ringbuf_val).map(
            |CountedRingbuf { ringbuf, counters }| (ringbuf, Some(counters)),
        );
        // If we are only displaying totals, bail early with the error returned
        // by `CountedRingbuf::from_value`, since this ringbuf doesn't have totals.
        if totals_only {
            maybe_counted.context("no entry total counts found")?
        } else {
            maybe_counted
                .or_else(|_e| {
                    Ringbuf::from_value(&ringbuf_val).map(|r| (Some(r), None))
                })
                .or_else(|_e| {
                    let cell: StaticCell =
                        StaticCell::from_value(&ringbuf_val)?;
                    let ringbuf = Ringbuf::from_value(&cell.cell.value)?;
                    Ok::<_, anyhow::Error>((Some(ringbuf), None))
                })?
        }
    };

    if let (Some(counters), false) = (counters, no_totals) {
        if full_totals {
            println!("{counters:#}");
        } else {
            println!("{counters}");
        }
    }

    if let (Some(ringbuf), false) = (ringbuf, totals_only) {
        let ndx = if let Some(x) = ringbuf.last {
            x as usize
        } else {
            return Ok(());
        };

        let fmt =
            HubrisPrintFormat { hex: true, ..HubrisPrintFormat::default() };

        println!(
            "{:>4} {:>4} {:>8} {:>8} PAYLOAD",
            "NDX", "LINE", "GEN", "COUNT",
        );

        for i in 0..ringbuf.buffer.len() {
            let slot = (ndx + i + 1) % ringbuf.buffer.len();
            let entry = &ringbuf.buffer[slot];

            if entry.generation == 0 {
                continue;
            }

            let mut dumped = vec![];
            entry.payload.format(hubris, fmt, &mut dumped)?;
            let dumped = String::from_utf8(dumped)?;

            println!(
                "{:4} {:4} {:8} {:8} {}",
                slot, entry.line, entry.generation, entry.count, dumped
            );
        }
    }

    Ok(())
}

fn taskname<'a>(
    hubris: &'a HubrisArchive,
    variable: &'a HubrisVariable,
) -> Result<&'a str> {
    Ok(&hubris.lookup_module(HubrisTask::from(variable.goff))?.name)
}

// this allow is meant for the header println! in the body but you cannot apply
// an attribute to a macro invoction, so we have to put it here instead.
#[allow(clippy::print_literal)]
fn ringbuf(context: &mut ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let hubris = context.archive.as_ref().unwrap();

    let subargs = RingbufArgs::try_parse_from(subargs)?;

    let mut ringbufs = vec![];

    for v in hubris.qualified_variables() {
        let def = match hubris.lookup_struct(v.1.goff) {
            Ok(s) => {
                // Skip variables whose type does not indicate they contain a
                // ringbuf; this check is imprecise but probably good enough
                if s.name.contains("Ringbuf") {
                    Some(s)
                } else {
                    continue;
                }
            }
            Err(_) => {
                // Type lookup failed, so fall back to the variable name
                if v.0.ends_with("RINGBUF") {
                    None
                } else {
                    continue;
                }
            }
        };

        if let Some(ref name) = subargs.name {
            if v.0.contains(name) || taskname(hubris, v.1)?.contains(name) {
                ringbufs.push((v, def));
            }
        } else {
            ringbufs.push((v, def));
        }
    }

    if ringbufs.is_empty() {
        if let Some(name) = subargs.name {
            bail!("no ring buffer name contains \"{}\" (-l to list)", name);
        } else {
            bail!("no ring buffers found");
        }
    }

    ringbufs.sort_by_key(|&(v, _def)| v);

    if subargs.list {
        println!("{:18} {:<30} {:<10} {}", "MODULE", "BUFFER", "ADDR", "SIZE");

        for (v, _def) in ringbufs {
            let t = taskname(hubris, v.1)?;

            println!("{:18} {:<30} 0x{:08x} {:<}", t, v.0, v.1.addr, v.1.size);
        }

        return Ok(());
    }

    for (v, def) in ringbufs {
        // Try not to use `?` here, because it causes one bad ringbuf to make
        // them all unavailable.
        println!(
            "humility: ring buffer {} in {}:",
            v.0,
            taskname(hubris, v.1).unwrap_or("???")
        );
        if let Some(def) = def {
            if let Err(e) = ringbuf_dump(hubris, core, def, v.1, subargs.totals)
            {
                // If we're only displaying total counts, don't print the
                // error for ringbufs that don't have counters, unless
                // verbose was requested or the user specifically requested
                // a ringbuf by name (suggesting that they expected it to have
                // counters) .
                let display_error =
                    !subargs.totals.totals_only || subargs.name.is_some();

                if subargs.verbose {
                    humility::msg!("ringbuf dump failed: {e:?}");
                } else if display_error {
                    humility::msg!("ringbuf dump failed: {e}");
                }
            }
        } else {
            humility::msg!("could not look up type: {:?}", v.1.goff);
        }
    }

    Ok(())
}

pub fn init() -> Command {
    Command {
        app: RingbufArgs::command(),
        name: "ringbuf",
        run: ringbuf,
        kind: CommandKind::Attached {
            archive: Archive::Required,
            attach: Attach::Any,
            validate: Validate::Match,
        },
    }
}
