// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility ringbuf`
//!
//! `humility ringbuf` reads and displays any Hubris ring buffers (as created
//! via the `ringbuf!` macro in the Hubris `ringbuf` crate).  e.g.:
//!
//! ```console
//! % humility -d ./hubris.core.5 ringbuf
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
//! See the [`ringbuf`
//! documentation](https://github.com/oxidecomputer/hubris/blob/master/lib/ringbuf/src/lib.rs) for more details.

use anyhow::{bail, Result};
use humility::core::Core;
use humility::hubris::*;
use humility_cmd::doppel::{Ringbuf, StaticCell};
use humility_cmd::reflect::{self, Format, Load, Value};
use humility_cmd::{Archive, Args, Attach, Command, Validate};
use structopt::clap::App;
use structopt::StructOpt;

#[macro_use]
extern crate log;

#[derive(StructOpt, Debug)]
#[structopt(name = "ringbuf", about = env!("CARGO_PKG_DESCRIPTION"))]
struct RingbufArgs {
    /// list variables
    #[structopt(long, short)]
    list: bool,
    /// print only a single ringbuffer by name
    #[structopt(conflicts_with = "list")]
    variable: Option<String>,
}

fn ringbuf_dump(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    definition: &HubrisStruct,
    ringbuf_var: &HubrisVariable,
) -> Result<()> {
    let mut buf: Vec<u8> = vec![];
    buf.resize_with(ringbuf_var.size, Default::default);

    let _info = core.halt()?;
    core.read_8(ringbuf_var.addr, buf.as_mut_slice())?;
    core.run()?;

    // There are two possible shapes of ringbufs, depending on the age of the
    // firmware.
    // - Raw Ringbuf that is not wrapped by anything.
    // - Safe Ringbuf that is inside a StaticCell.
    //
    // Here we will attempt to handle them both -- first raw, then fallback.
    let ringbuf_val: Value =
        Value::Struct(reflect::load_struct(hubris, &buf, definition, 0)?);

    let ringbuf: Ringbuf = Ringbuf::from_value(&ringbuf_val).or_else(|_e| {
        let cell: StaticCell = StaticCell::from_value(&ringbuf_val)?;
        Ringbuf::from_value(&cell.cell.value)
    })?;

    let ndx = if let Some(x) = ringbuf.last {
        x as usize
    } else {
        return Ok(());
    };

    let fmt = HubrisPrintFormat { hex: true, ..HubrisPrintFormat::default() };

    println!("{:>4} {:>4} {:>8} {:>8} PAYLOAD", "NDX", "LINE", "GEN", "COUNT",);

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

    Ok(())
}

fn taskname<'a>(
    hubris: &'a HubrisArchive,
    variable: &'a HubrisVariable,
) -> Result<&'a str> {
    Ok(&hubris.lookup_module(HubrisTask::from(variable.goff))?.name)
}

fn ringbuf(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    _args: &Args,
    subargs: &[String],
) -> Result<()> {
    let subargs = RingbufArgs::from_iter_safe(subargs)?;

    let mut ringbufs = vec![];

    for v in hubris.qualified_variables() {
        if let Some(ref variable) = subargs.variable {
            if v.0.eq(variable) {
                ringbufs.push(v);
            }
        } else if v.0.ends_with("RINGBUF") {
            ringbufs.push(v);
        }
    }

    if ringbufs.is_empty() {
        if let Some(variable) = subargs.variable {
            bail!("ring buffer \"{}\" not found (-l to list)", variable);
        } else {
            bail!("no ring buffers found");
        }
    }

    ringbufs.sort();

    if subargs.list {
        info!("{:18} {:<30} {:<10} {}", "MODULE", "BUFFER", "ADDR", "SIZE");

        for v in ringbufs {
            let t = taskname(hubris, v.1)?;
            info!("{:18} {:<30} 0x{:08x} {:<}", t, v.0, v.1.addr, v.1.size);
        }

        return Ok(());
    }

    for v in ringbufs {
        // Try not to use `?` here, because it causes one bad ringbuf to make
        // them all unavailable.
        info!(
            "ring buffer {} in {}:",
            v.0,
            taskname(hubris, v.1).unwrap_or("???")
        );
        if let Ok(def) = hubris.lookup_struct(v.1.goff) {
            if let Err(e) = ringbuf_dump(hubris, core, def, v.1) {
                info!("ringbuf dump failed: {}", e);
            }
        } else {
            info!("could not look up type: {:?}", v.1.goff);
        }
    }

    Ok(())
}

pub fn init<'a, 'b>() -> (Command, App<'a, 'b>) {
    (
        Command::Attached {
            name: "ringbuf",
            archive: Archive::Required,
            attach: Attach::Any,
            validate: Validate::Match,
            run: ringbuf,
        },
        RingbufArgs::clap(),
    )
}
