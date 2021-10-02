/*
 * Copyright 2021 Oxide Computer Company
 */

use crate::cmd::{Archive, Attach, Validate};
use crate::core::Core;
use crate::hiffy::*;
use crate::hubris::*;
use crate::Args;
use std::convert::TryFrom;
use std::str;
use std::thread;

use anyhow::{anyhow, bail, Context, Result};
use hif::*;
use std::time::Duration;
use structopt::clap::App;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(name = "spd", about = "scan for and read SPD devices")]
struct SpdArgs {
    /// sets timeout
    #[structopt(
        long, short, default_value = "5000", value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// verbose output
    #[structopt(long, short)]
    verbose: bool,

    /// show errors
    #[structopt(long, short)]
    errors: bool,

    /// specifies an I2C controller
    #[structopt(long, short, value_name = "controller",
        parse(try_from_str = parse_int::parse),
    )]
    controller: u8,

    /// specifies an I2C controller port
    #[structopt(long, short, value_name = "port")]
    port: Option<String>,

    /// specifies I2C multiplexer and segment
    #[structopt(long, short, value_name = "mux:segment")]
    mux: Option<String>,
}

fn from_bcd(val: u8) -> u8 {
    (val >> 4) * 10 + (val & 0xf)
}

fn dump_spd(
    subargs: &SpdArgs,
    addr: u8,
    buf: &[u8],
    header: bool,
) -> Result<()> {
    use spd::Offset;

    let jep_cc = Offset::ModuleManufacturerIDCodeLSB.within(buf) & 0x7f;
    let jep_id = Offset::ModuleManufacturerIDCodeMSB.within(buf) & 0x7f;

    let year = from_bcd(Offset::ModuleManufacturingDateYear.within(buf));
    let week = from_bcd(Offset::ModuleManufacturingDateWeek.within(buf));

    let width: usize = 16;

    let manufacturer = jep106::JEP106Code::new(jep_cc, jep_id);

    let part = str::from_utf8(
        &buf[Offset::PartNumberBase.to_usize()
            ..=Offset::PartNumberLimit.to_usize()],
    );

    if header || subargs.verbose {
        println!(
            "{:4} {:25} {:20} {:4} {:4}",
            "ADDR", "MANUFACTURER", "PART", "WEEK", "YEAR"
        )
    }

    println!(
        "{:4} {:25} {:20} {:4} {:4}",
        addr,
        match manufacturer.get() {
            Some(ref m) => m,
            _ => "<unknown>",
        },
        match part {
            Ok(part) => part,
            _ => "<unknown>",
        },
        week,
        2000 + (year as u16),
    );

    if !subargs.verbose {
        return Ok(());
    }

    println!("   |");
    print!("   +---->   ");

    for i in 0..width {
        print!(" {:02x}", i);
    }

    println!("");

    for offs in (0..512).step_by(width) {
        print!("    0x{:03x} | ", offs);

        for i in 0..width {
            print!(" {:02x}", buf[offs + i]);
        }

        print!(" | ");

        for i in 0..width {
            let c = buf[offs + i] as char;

            if c.is_ascii() && !c.is_ascii_control() {
                print!("{}", c);
            } else {
                print!(".");
            }
        }

        println!("");
    }

    Ok(())
}

// Assumes that we already have pushed on the stack our controller/port/mux
fn set_page(ops: &mut Vec<Op>, i2c_write: &HiffyFunction, page: u8) {
    let dev = spd::Function::PageAddress(page).to_code().unwrap();
    ops.push(Op::Push(dev)); // Device
    ops.push(Op::PushNone); // Register
    ops.push(Op::Push(0)); // Buffer
    ops.push(Op::Push(1)); // Length
    ops.push(Op::Call(i2c_write.id));
    ops.push(Op::DropN(4));
}

fn spd(
    hubris: &mut HubrisArchive,
    core: &mut dyn Core,
    _args: &Args,
    subargs: &Vec<String>,
) -> Result<()> {
    let subargs = SpdArgs::from_iter_safe(subargs)?;
    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;
    let funcs = context.functions()?;

    let i2c_read = funcs
        .get("I2cRead")
        .ok_or_else(|| anyhow!("did not find I2cRead function"))?;

    let i2c_write = funcs
        .get("I2cWrite")
        .ok_or_else(|| anyhow!("did not find I2cWrite function"))?;

    if i2c_read.args.len() != 7 {
        bail!("mismatched function signature on I2cRead");
    }

    let mut port = None;

    if let Some(ref portarg) = subargs.port {
        let p = hubris
            .lookup_enum(i2c_read.args[1])
            .context("expected port to be an enum")?;

        if p.size != 1 {
            bail!("expected port to be a 1-byte enum");
        }

        for variant in &p.variants {
            if variant.name.eq_ignore_ascii_case(&portarg) {
                port = Some(u8::try_from(variant.tag.unwrap())?);
                break;
            }
        }

        if port.is_none() {
            let mut vals: Vec<String> = vec![];

            for variant in &p.variants {
                vals.push(variant.name.to_string());
            }

            bail!(
                "invalid port \"{}\" (must be one of: {})",
                portarg,
                vals.join(", ")
            );
        }
    }

    let mux = if let Some(mux) = &subargs.mux {
        let s = mux
            .split(":")
            .map(|v| parse_int::parse::<u8>(v))
            .collect::<Result<Vec<_>, _>>()
            .context("expected multiplexer and segment to be integers")?;

        if s.len() == 2 {
            Some((s[0], s[1]))
        } else if s.len() == 1 {
            Some((0, s[0]))
        } else {
            bail!("expected only multiplexer and segment identifiers");
        }
    } else {
        None
    };

    let mut ops = vec![];

    ops.push(Op::Push(subargs.controller));

    if let Some(port) = port {
        ops.push(Op::Push(port));
    } else {
        ops.push(Op::PushNone);
    }

    if let Some(mux) = mux {
        ops.push(Op::Push(mux.0));
        ops.push(Op::Push(mux.1));
    } else {
        ops.push(Op::PushNone);
        ops.push(Op::PushNone);
    }

    let base = ops.clone();

    //
    // First, we want to have all SPDs on the specified bus flip to
    // their 0 page
    //
    set_page(&mut ops, &i2c_write, 0);

    //
    // Now issue single byte reads to determine where our devices are.
    //
    for addr in 0..spd::MAX_DEVICES {
        ops.push(Op::Push(spd::Function::Memory(addr).to_code().unwrap()));
        ops.push(Op::PushNone);
        ops.push(Op::Push(1));
        ops.push(Op::Call(i2c_read.id));
        ops.push(Op::DropN(3));
    }

    ops.push(Op::Done);

    context.execute(core, ops.as_slice(), None)?;

    loop {
        if context.done(core)? {
            break;
        }

        thread::sleep(Duration::from_millis(100));
    }

    let results = context.results(core)?;
    let mut header = true;

    if let Err(err) = results[0] {
        bail!("failed to set page to 0: {}", i2c_write.strerror(err));
    }

    for addr in 0..spd::MAX_DEVICES {
        if let Ok(_) = &results[addr as usize + 1] {
            let mut ops = base.clone();

            //
            // Issue the read for the bottom 128 bytes from the 0 page
            //
            let dev = spd::Function::Memory(addr).to_code().unwrap();
            ops.push(Op::Push(dev));
            ops.push(Op::Push(0));
            ops.push(Op::Push(128));
            ops.push(Op::Call(i2c_read.id));

            //
            // And now read the top 128 bytes from the 0 page...
            //
            ops.push(Op::DropN(2));
            ops.push(Op::Push(128));
            ops.push(Op::Push(128));
            ops.push(Op::Call(i2c_read.id));

            //
            // Switch to the 1 page
            //
            ops.push(Op::DropN(3));
            set_page(&mut ops, &i2c_write, 1);

            //
            // Issue an identical read for the bottom 128 bytes...
            //
            ops.push(Op::Push(dev));
            ops.push(Op::Push(0));
            ops.push(Op::Push(128));
            ops.push(Op::Call(i2c_read.id));

            //
            // ...and the top 128 bytes
            //
            ops.push(Op::DropN(2));
            ops.push(Op::Push(128));
            ops.push(Op::Push(128));
            ops.push(Op::Call(i2c_read.id));
            ops.push(Op::DropN(3));

            //
            // Finally, set ourselves back to the 0 page
            //
            set_page(&mut ops, &i2c_write, 0);

            ops.push(Op::Done);

            context.execute(core, ops.as_slice(), None)?;

            loop {
                if context.done(core)? {
                    break;
                }

                thread::sleep(Duration::from_millis(100));
            }

            let results = context.results(core)?;

            //
            // If that succeeded, we'll have four buffers that should add up
            // to 512 bytes.
            //
            let mut buf = vec![];

            for result in &results {
                match result {
                    Ok(val) => {
                        buf.extend_from_slice(val);
                    }
                    Err(_) => {
                        bail!("failed to read SPD: {:?}", results);
                    }
                }
            }

            if buf.len() != 512 {
                bail!("bad SPD length ({} bytes): {:?}", buf.len(), results);
            }

            dump_spd(&subargs, addr, &buf, header)?;
            header = false;
        }
    }

    Ok(())
}

pub fn init<'a, 'b>() -> (crate::cmd::Command, App<'a, 'b>) {
    (
        crate::cmd::Command::Attached {
            name: "spd",
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
            run: spd,
        },
        SpdArgs::clap(),
    )
}
