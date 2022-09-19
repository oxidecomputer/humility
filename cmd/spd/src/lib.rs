// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use humility::cli::Subcommand;
use humility_cmd::i2c::I2cArgs;
use humility_cmd::{hiffy::*, CommandKind};
use humility_cmd::{Archive, Attach, Command, Validate};
use std::str;

use anyhow::{bail, Result};
use clap::{CommandFactory, Parser};
use hif::*;

#[derive(Parser, Debug)]
#[clap(name = "spd", about = env!("CARGO_PKG_DESCRIPTION"))]
struct SpdArgs {
    /// sets timeout
    #[clap(
        long, short, default_value_t = 5000, value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// verbose output
    #[clap(long, short)]
    verbose: bool,

    /// specifies an I2C controller
    #[clap(long, short, value_name = "controller",
        parse(try_from_str = parse_int::parse),
    )]
    controller: Option<u8>,

    /// specifies an I2C bus by name
    #[clap(long, short, value_name = "bus",
        conflicts_with_all = &["port", "controller"]
    )]
    bus: Option<String>,

    /// specifies an I2C controller port
    #[clap(long, short, value_name = "port")]
    port: Option<String>,

    /// specifies I2C multiplexer and segment
    #[clap(long, short, value_name = "mux:segment")]
    mux: Option<String>,
}

const SPD_SIZE: usize = 512;

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

    for offs in (0..SPD_SIZE).step_by(width) {
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
    let dev =
        spd::Function::PageAddress(spd::Page(page)).to_device_code().unwrap();
    ops.push(Op::Push(dev)); // Device
    ops.push(Op::PushNone); // Register
    ops.push(Op::Push(0)); // Buffer
    ops.push(Op::Push(1)); // Length
    ops.push(Op::Call(i2c_write.id));
    ops.push(Op::DropN(4));
}

fn spd(context: &mut humility::ExecutionContext) -> Result<()> {
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let hubris = context.archive.as_ref().unwrap();
    let core = &mut **context.core.as_mut().unwrap();

    let subargs = SpdArgs::try_parse_from(subargs)?;

    //
    // If we have been given no device-related arguments, we will attempt
    // to find the `SPD_DATA` variable.
    //
    if subargs.bus.is_none() && subargs.controller.is_none() {
        let spd_data = match hubris.lookup_variables("SPD_DATA") {
            Ok(variables) => {
                if variables.len() > 1 {
                    bail!("more than one SPD_DATA?");
                }

                variables[0]
            }
            Err(_) => {
                bail!("no bus specified and no SPD_DATA found");
            }
        };

        if spd_data.size % SPD_SIZE != 0 {
            bail!(
                "SPD_DATA is {} bytes; expected even multiple of {}",
                spd_data.size,
                SPD_SIZE
            );
        }

        let nspd = spd_data.size / SPD_SIZE;
        let mut bytes = vec![0u8; spd_data.size];

        core.halt()?;
        let rval = core.read_8(spd_data.addr, &mut bytes);
        core.run()?;

        rval?;

        let mut header = true;

        for addr in 0..nspd {
            let offs = addr * SPD_SIZE;
            let data = &bytes[offs..offs + SPD_SIZE];

            if !data.iter().any(|&datum| datum != 0) {
                continue;
            }

            dump_spd(&subargs, addr as u8, data, header)?;
            header = false;
        }

        if header {
            humility::msg!("all SPD data is empty");
        }

        return Ok(());
    }

    if core.is_dump() {
        bail!("cannot specify bus/controller on a dump");
    }

    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;
    let funcs = context.functions()?;

    let i2c_read = funcs.get("I2cRead", 7)?;
    let i2c_write = funcs.get("I2cWrite", 8)?;

    let hargs = I2cArgs::parse(
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
        ops.push(Op::Push(
            spd::Function::Memory(addr).to_device_code().unwrap(),
        ));
        ops.push(Op::Push(0));
        ops.push(Op::Push(1));
        ops.push(Op::Call(i2c_read.id));
        ops.push(Op::DropN(3));
    }

    ops.push(Op::Done);

    let results = context.run(core, ops.as_slice(), None)?;
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
            let dev = spd::Function::Memory(addr).to_device_code().unwrap();
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

            let results = context.run(core, ops.as_slice(), None)?;

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

            if buf.len() != SPD_SIZE {
                bail!("bad SPD length ({} bytes): {:?}", buf.len(), results);
            }

            dump_spd(&subargs, addr, &buf, header)?;
            header = false;
        }
    }

    Ok(())
}

pub fn init() -> Command {
    Command {
        app: SpdArgs::command(),
        name: "spd",
        run: spd,
        kind: CommandKind::Attached {
            archive: Archive::Required,
            attach: Attach::Any,
            validate: Validate::Booted,
        },
    }
}
