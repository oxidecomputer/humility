// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::cmd::*;
use crate::doppel::{Ringbuf, StaticCell};
use crate::reflect::{self, Format, Load, Value};
use crate::Args;
use anyhow::{bail, Result};
use humility::core::Core;
use humility::hubris::*;
use structopt::clap::App;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(
    name = "ringbuf",
    about = "read and display a specified ring buffer"
)]
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
    hubris: &mut HubrisArchive,
    core: &mut dyn Core,
    _args: &Args,
    subargs: &Vec<String>,
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
