// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::cmd::{Archive, Attach, Validate};
use crate::core::Core;
use crate::hiffy::*;
use crate::hubris::*;
use crate::Args;
use std::str;
use std::thread;

use anyhow::{anyhow, bail, Result};
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

    /// specifies an I2C controller
    #[structopt(long, short, value_name = "controller",
        parse(try_from_str = parse_int::parse),
    )]
    controller: Option<u8>,

    /// specifies an I2C bus by name
    #[structopt(long, short, value_name = "bus",
        conflicts_with_all = &["port", "controller"]
    )]
    bus: Option<String>,

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
        manufacturer.get().unwrap_or("<unknown>"),
        part.unwrap_or("<unknown>"),
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

    println!();

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

        println!();
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

    let hargs = crate::i2c::I2cArgs::parse(
        hubris,
        &subargs.bus,
        subargs.controller,
        &subargs.port,
        &subargs.mux,
        &None,
    )?;

    let mut ops = vec![Op::Push(hargs.controller)];

    ops.push(Op::Push(hargs.port.index));

    if let Some(mux) = hargs.mux {
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
    set_page(&mut ops, i2c_write, 0);

    //
    // Now issue single byte register reads to determine where our devices are.
    //
    for addr in 0..spd::MAX_DEVICES {
        ops.push(Op::Push(spd::Function::Memory(addr).to_code().unwrap()));
        ops.push(Op::Push(0));
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
        if results[addr as usize + 1].is_ok() {
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
            set_page(&mut ops, i2c_write, 1);

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
            set_page(&mut ops, i2c_write, 0);

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
