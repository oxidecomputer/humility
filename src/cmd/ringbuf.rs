/*
 * Copyright 2020 Oxide Computer Company
 */

use crate::cmd::*;
use crate::core::Core;
use crate::hubris::*;
use crate::Args;
use anyhow::{bail, Context, Result};
use std::convert::TryInto;
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
    #[structopt(conflicts_with = "list")]
    variable: Option<String>,
}

fn ringbuf_dump(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    definition: &HubrisStruct,
    ringbuf: &HubrisVariable,
) -> Result<()> {
    let variant_basetype = |variant: &HubrisEnumVariant| {
        let t = match variant.goff {
            None => bail!("expected tuple"),
            Some(goff) => hubris.lookup_struct(goff)?.lookup_member("__0")?,
        };

        Ok((hubris.lookup_basetype(t.goff)?, t.offset))
    };

    let member_basetype =
        |elem: &HubrisStruct, name| -> Result<(&HubrisBasetype, usize)> {
            let member = elem.lookup_member(name)?;
            let t = hubris.lookup_basetype(member.goff)?;

            Ok((t, member.offset))
        };

    let read_basetype = |buf: &Vec<u8>, t: &HubrisBasetype, offset| {
        let slice = &buf[offset..offset + t.size];

        match t.size {
            1 => slice[0] as usize,
            2 => u16::from_le_bytes(slice.try_into().unwrap()) as usize,
            4 => u32::from_le_bytes(slice.try_into().unwrap()) as usize,
            _ => {
                panic!("invalid size");
            }
        }
    };

    let mut buf: Vec<u8> = vec![];
    buf.resize_with(ringbuf.size, Default::default);
    core.read_8(ringbuf.addr, buf.as_mut_slice())?;

    let last = definition.lookup_member("last")?;
    let buffer = definition.lookup_member("buffer")?;

    let option = hubris
        .lookup_enum(last.goff)
        .context("expected 'last' to be an Option")?;

    let some = option.lookup_variant_byname("Some")?;
    let none = option.lookup_variant_byname("None")?;

    let variant = option.determine_variant(hubris, &buf[last.offset..])?;

    if variant.goff == none.goff {
        return Ok(());
    }

    let (last, offset) = variant_basetype(some)?;
    let ndx = read_basetype(&buf, last, offset);

    let array = hubris
        .lookup_array(buffer.goff)
        .context("expected 'buffer' to be an array")?;

    let entry = hubris
        .lookup_struct(array.goff)
        .context("expected 'buffer' to be an array of structs")?;

    let payload = entry.lookup_member("payload")?;
    let line = member_basetype(entry, "line")?;
    let gen = member_basetype(entry, "generation")?;
    let count = member_basetype(entry, "count")?;
    let fmt = HubrisPrintFormat { indent: 0, newline: false, hex: true };

    println!(
        "{:10} {:>4} {:>4} {:>8} {:>8} {}",
        "ADDR", "NDX", "LINE", "GEN", "COUNT", "PAYLOAD"
    );

    for i in 0..array.count {
        let slot = (ndx + i + 1) % array.count;
        let offset = buffer.offset + (slot * entry.size);
        let genval = read_basetype(&buf, gen.0, offset + gen.1);

        if genval == 0 {
            continue;
        }

        let dumped = hubris.printfmt(
            &buf[offset + payload.offset..],
            payload.goff,
            &fmt,
        )?;

        println!(
            "0x{:08x} {:4} {:4} {:8} {:8} {}",
            ringbuf.addr as usize + offset,
            slot,
            read_basetype(&buf, line.0, offset + line.1),
            genval,
            read_basetype(&buf, count.0, offset + count.1),
            dumped
        );
    }

    Ok(())
}

fn taskname<'a>(
    hubris: &'a HubrisArchive,
    variable: &'a HubrisVariable,
) -> Result<&'a String> {
    Ok(&hubris.lookup_task(HubrisTask::from(variable.goff))?.name)
}

fn ringbuf(
    hubris: &mut HubrisArchive,
    core: &mut dyn Core,
    _args: &Args,
    subargs: &Vec<String>,
) -> Result<()> {
    let subargs = RingbufArgs::from_iter_safe(subargs)?;

    let vars = hubris.variables();
    let mut ringbufs = vec![];

    for v in vars {
        if let Some(ref variable) = subargs.variable {
            if v.0.eq(variable) {
                ringbufs.push(v);
            }
        } else {
            if v.0.ends_with("RINGBUF") {
                ringbufs.push(v);
            }
        }
    }

    if ringbufs.len() == 0 {
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
            let t = taskname(hubris, &v.1)?;
            info!("{:18} {:<30} 0x{:08x} {:<}", t, v.0, v.1.addr, v.1.size);
        }

        return Ok(());
    }

    for v in ringbufs {
        info!("ring buffer {} in {}:", v.0, taskname(hubris, &v.1)?);
        let def = hubris.lookup_struct(v.1.goff)?;
        ringbuf_dump(hubris, core, def, &v.1)?;
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
