// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility spd`
//!
//! Scan for and read devices implementing Serial Presence Detect (SPD).
//! When run without arguments, `humility spd` will display the SPD data
//! as gathered and cached by the system:
//!
//! ```console
//! $ humility spd
//! humility: attached via ST-Link V3
//! ADDR MANUFACTURER              PART                 WEEK YEAR
//!    0 Micron Technology         36ASF8G72PZ-3G2E1      44 2021
//!    1 Micron Technology         36ASF8G72PZ-3G2E1      44 2021
//!    2 Micron Technology         36ASF8G72PZ-3G2E1      44 2021
//!    3 Micron Technology         36ASF8G72PZ-3G2E1      44 2021
//!    4 Micron Technology         36ASF8G72PZ-3G2E1      44 2021
//!    5 Micron Technology         36ASF8G72PZ-3G2E1      44 2021
//!    6 Micron Technology         36ASF8G72PZ-3G2E1      44 2021
//!    7 Micron Technology         36ASF8G72PZ-3G2E1      44 2021
//!    8 Micron Technology         36ASF8G72PZ-3G2E1      44 2021
//!    9 Micron Technology         36ASF8G72PZ-3G2E1      44 2021
//!   10 Micron Technology         36ASF8G72PZ-3G2E1      44 2021
//!   11 Micron Technology         36ASF8G72PZ-3G2E1      44 2021
//!   12 Micron Technology         36ASF8G72PZ-3G2E1      44 2021
//!   13 Micron Technology         36ASF8G72PZ-3G2E1      44 2021
//!   14 Micron Technology         36ASF8G72PZ-3G2E1      44 2021
//!   15 Micron Technology         36ASF8G72PZ-3G2E1      44 2021
//! ```
//!
//! This performs no I2C reads, and because it operates on cached data,
//! can be run postmortem.
//!
//! To force an I2C read of a given SPD device, specify the desired bus
//! in terms of either a named bus or controller/port/mux:
//!
//! ```console
//! % humility spd -b mid
//! humility: attached via ST-Link V3
//! ADDR MANUFACTURER              PART                 WEEK YEAR
//!    0 Micron Technology         36ASF8G72PZ-3G2E1      44 2021
//!    1 Micron Technology         36ASF8G72PZ-3G2E1      44 2021
//!    2 Micron Technology         36ASF8G72PZ-3G2E1      44 2021
//!    3 Micron Technology         36ASF8G72PZ-3G2E1      44 2021
//!    4 Micron Technology         36ASF8G72PZ-3G2E1      44 2021
//!    5 Micron Technology         36ASF8G72PZ-3G2E1      44 2021
//!    6 Micron Technology         36ASF8G72PZ-3G2E1      44 2021
//!    7 Micron Technology         36ASF8G72PZ-3G2E1      44 2021
//! ```
//!
//! Note that a given bus can have up to 8 DIMMs on it.
//!
//! To dump the entire contents of one more SPDs, use the `--verbose` (`-v`)
//! option:
//!
//! ```console
//! % humility spd --bus mid --address 5 --verbose
//! humility: attached via ST-Link V3
//! ADDR MANUFACTURER              PART                 WEEK YEAR
//!    5 Micron Technology         36ASF8G72PZ-3G2E1      44 2021
//!    |
//!    +---->    00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f
//!     0x000 |  23 12 0c 01 86 31 00 08 00 60 00 03 08 0b 80 00 | #....1...`......
//!     0x010 |  00 00 05 0d f8 ff 02 00 6e 6e 6e 11 00 6e f0 0a | ........nnn..n..
//!     0x020 |  20 08 00 05 00 50 14 28 28 00 78 00 14 3c 00 00 |  ....P.((.x..<..
//!     0x030 |  00 00 00 00 00 00 00 00 00 00 00 00 16 16 15 16 | ................
//!     0x040 |  03 16 03 16 03 16 03 16 0d 16 16 16 16 16 00 00 | ................
//!     0x050 |  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 | ................
//!     0x060 |  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 | ................
//!     0x070 |  00 00 00 00 00 00 9c 00 00 00 00 00 e7 00 fd a3 | ................
//!     0x080 |  31 11 61 19 00 86 9d 22 01 65 45 00 00 00 00 00 | 1.a....".eE.....
//!     0x090 |  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 | ................
//!     0x0a0 |  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 | ................
//!     0x0b0 |  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 | ................
//!     0x0c0 |  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 | ................
//!     0x0d0 |  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 | ................
//!     0x0e0 |  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 | ................
//!     0x0f0 |  00 00 00 00 00 00 00 00 00 00 00 00 00 00 b8 ce | ................
//!     0x100 |  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 | ................
//!     0x110 |  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 | ................
//!     0x120 |  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 | ................
//!     0x130 |  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 | ................
//!     0x140 |  80 2c 06 21 44 32 52 c5 7e 33 36 41 53 46 38 47 | .,.!D2R.~36ASF8G
//!     0x150 |  37 32 50 5a 2d 33 47 32 45 31 20 20 20 31 80 2c | 72PZ-3G2E1   1.,
//!     0x160 |  45 4a 41 41 42 4a 35 50 30 30 31 00 00 00 00 00 | EJAABJ5P001.....
//!     0x170 |  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 | ................
//!     0x180 |  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 | ................
//!     0x190 |  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 | ................
//!     0x1a0 |  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 | ................
//!     0x1b0 |  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 | ................
//!     0x1c0 |  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 | ................
//!     0x1d0 |  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 | ................
//!     0x1e0 |  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 | ................
//!     0x1f0 |  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 | ................
//! ```
//!
//! To dump a given SPD to a file, additionally provide the `--output` (`-o`)
//! option and specify a desired output file:
//!
//! ```console
//! % humility spd --bus mid --address 5 --output spd.5.out
//! humility: attached via ST-Link V3
//! humility: wrote SPD data for address 5 as binary to spd.5.out
//! ```
//!

use humility::hubris::*;
use humility::{reflect, reflect::Load};
use humility_cli::{ExecutionContext, Subcommand};
use humility_cmd::{Archive, Attach, Command, CommandKind, Validate};
use humility_doppel as doppel;
use humility_hiffy::*;
use humility_i2c::I2cArgs;
use humility_log::msg;
use std::fs::File;
use std::io::Write;
use std::str;

use anyhow::{anyhow, bail, Result};
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

    /// verbose output (including raw 512-byte SPD)
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

    /// dump only the specified address
    #[clap(long, short, value_name = "address",
        parse(try_from_str = parse_int::parse)
    )]
    address: Option<u8>,

    /// output SPD for specified address as binary to the specified file
    #[clap(
        long,
        short,
        value_name = "file",
        requires = "address",
        conflicts_with = "verbose"
    )]
    output: Option<String>,
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

    if let Some(address) = subargs.address {
        if address != addr {
            return Ok(());
        }
    }

    if let Some(filename) = &subargs.output {
        let mut output = File::create(filename)?;
        output.write_all(buf)?;
        msg!("wrote SPD data for address {addr} as binary to {filename}");
        return Ok(());
    }

    let part = str::from_utf8(
        &buf[Offset::PartNumberBase.to_usize()
            ..=Offset::PartNumberLimit.to_usize()],
    );

    if header || subargs.address.is_some() || subargs.verbose {
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
                print!("{c}");
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

//
// Our SPD data is sitting in packrat, so we'll specify just enough of the
// embedded structures to get at it.
//
static PACKRAT_BUF_NAME: &str = "task_packrat::main::BUFS";

#[derive(Load, Debug)]
struct GimletStaticBufs {
    spd_data: Vec<u8>,
}

#[derive(Load, Debug)]
struct PackratStaticBufs {
    gimlet_bufs: GimletStaticBufs,
}

fn spd_lookup(
    hubris: &HubrisArchive,
    core: &mut dyn humility::core::Core,
) -> Result<Option<Vec<u8>>> {
    if let Ok(variables) = hubris.lookup_variables("SPD_DATA") {
        if variables.len() > 1 {
            bail!("more than one SPD_DATA?");
        }

        let var = variables[0];
        let mut buf: Vec<u8> = vec![0u8; var.size];

        core.halt()?;
        core.read_8(var.addr, &mut buf)?;
        core.run()?;

        Ok(Some(buf))
    } else if let Ok(var) = hubris.lookup_qualified_variable(PACKRAT_BUF_NAME) {
        let var_ty = hubris.lookup_type(var.goff)?;
        let mut buf: Vec<u8> = vec![0u8; var.size];

        core.halt()?;
        core.read_8(var.addr, &mut buf)?;
        core.run()?;

        let v = reflect::load_value(hubris, &buf, var_ty, 0)?;
        let as_static_cell = doppel::ClaimOnceCell::from_value(&v)?;

        let p = PackratStaticBufs::from_value(&as_static_cell.cell.value)?;
        Ok(Some(p.gimlet_bufs.spd_data))
    } else {
        Ok(None)
    }
}

pub fn spd_any(
    hubris: &HubrisArchive,
    core: &mut dyn humility::core::Core,
) -> Result<bool> {
    match spd_lookup(hubris, core)? {
        Some(spd_data) => Ok(spd_data.iter().any(|&datum| datum != 0)),
        None => Ok(false),
    }
}

fn spd(context: &mut ExecutionContext) -> Result<()> {
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let hubris = context.archive.as_ref().unwrap();
    let core = &mut **context.core.as_mut().unwrap();

    let subargs = SpdArgs::try_parse_from(subargs)?;

    //
    // If we have been given no device-related arguments, we will attempt
    // to find the `SPD_DATA` variable.
    //
    if subargs.bus.is_none() && subargs.controller.is_none() {
        let spd_data = spd_lookup(hubris, core)?
            .ok_or_else(|| anyhow!("no bus specified and no SPD_DATA found"))?;

        if spd_data.len() % SPD_SIZE != 0 {
            bail!(
                "SPD_DATA is {} bytes; expected even multiple of {SPD_SIZE}",
                spd_data.len(),
            );
        }

        let nspd = spd_data.len() / SPD_SIZE;

        let mut header = true;

        for addr in 0..nspd {
            let offs = addr * SPD_SIZE;
            let data = &spd_data[offs..offs + SPD_SIZE];

            if !data.iter().any(|&datum| datum != 0) {
                continue;
            }

            dump_spd(&subargs, addr as u8, data, header)?;
            header = false;
        }

        if header {
            msg!("all SPD data is empty");
        }

        return Ok(());
    }

    if core.is_dump() {
        bail!("cannot specify bus/controller on a dump");
    }

    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;

    let i2c_read = context.get_function("I2cRead", 7)?;
    let i2c_write = context.get_function("I2cWrite", 8)?;

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
    set_page(&mut ops, &i2c_write, 0);

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
                        bail!("failed to read SPD: {results:?}");
                    }
                }
            }

            if buf.len() != SPD_SIZE {
                bail!("bad SPD length ({} bytes): {results:?}", buf.len());
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
