// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility i2c`
//!
//! On platforms that have I<sup>2</sup>C support, `humility i2c` can be used
//! to scan a bus, scan a device, read a register, or write a register.  Its
//! usage will be specific to the board being examined; to specify a
//! controller use the `-c`, to specify a port (if necessary), use `-p`; to
//! specify a mux and segment (if necessary), use `-m`.
//!
//! For example, on a Gimletlet, here is a scan of controller I2C3, revealing
//! one device at address `0x48`:
//!
//! ```console
//! % humility i2c -s -c 3
//! humility: attached via ST-Link
//!
//! Device scan on controller I2C3:
//!
//!     R = Reserved   - = No device   \o/ = Device found   X = Timed out
//!
//! ADDR     0x0 0x1 0x2 0x3 0x4 0x5 0x6 0x7 0x8 0x9 0xa 0xb 0xc 0xd 0xe 0xf
//! 0x00       R   R   R   R   R   R   R   R   -   -   -   -   -   -   -   -
//! 0x10       -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
//! 0x20       -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
//! 0x30       -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
//! 0x40       -   -   -   -   -   -   -   - \o/   -   -   -   -   -   -   -
//! 0x50       -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
//! 0x60       -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
//! 0x70       -   -   -   -   -   -   -   -   -   -   -   -   R   R   R   R
//! ```
//!
//! To scan that device, specify its address via `-d`:
//!
//! ```console
//! % humility i2c -s -c 3 -d 0x48
//! humility: attached via ST-Link
//!
//! Register scan for device 0x48 on I2C3:
//!
//!       - = No register        ! = No device        X = Timed out
//!
//! ADDR  0x0 0x1 0x2 0x3 0x4 0x5 0x6 0x7 0x8 0x9 0xa 0xb 0xc 0xd 0xe 0xf
//! 0x00   0b  c8  80  00  20  00  05  00  49  80  05  cb  00  00  00  00
//! 0x10   00  00  00  00  00  00  00  00  6c  00  06  00  00  00  00  00
//! 0x20   0b  c8  80  00  20  00  05  00  49  80  05  cb  00  00  00  00
//! 0x30   00  00  00  00  00  00  00  00  6c  00  06  00  00  00  00  00
//! 0x40   00  00  80  00  20  00  05  00  49  80  05  cb  00  00  00  00
//! 0x50   00  00  00  00  00  00  00  00  6c  00  06  00  00  00  00  00
//! 0x60   00  00  80  00  20  00  05  00  49  80  05  cb  00  00  00  00
//! 0x70   00  00  00  00  00  00  00  00  6c  00  06  00  00  00  00  00
//! 0x80   00  00  80  00  20  00  05  00  49  80  05  cb  00  00  00  00
//! 0x90   00  00  00  00  00  00  00  00  6c  00  06  00  00  00  00  00
//! 0xa0   00  00  80  00  20  00  05  00  49  80  05  cb  00  00  00  00
//! 0xb0   00  00  00  00  00  00  00  00  6c  00  06  00  00  00  00  00
//! 0xc0   00  00  80  00  20  00  05  00  49  80  05  cb  00  00  00  00
//! 0xd0   00  00  00  00  00  00  00  00  6c  00  06  00  00  00  00  00
//! 0xe0   00  00  80  00  20  00  05  00  49  80  05  cb  00  00  00  00
//! 0xf0   00  00  00  00  00  00  00  00  6c  00  06  00  00  00  00  00
//! ```
//!
//! (This device is an ADT7420 temp sensor.)  To look at a particular
//! register, elide `-s` and specify the register of interest via `-r`:
//!
//! ```console
//! % humility i2c -c 3 -d 0x48 -r 0xb
//! humility: attached via ST-Link
//! Controller I2C3, device 0x48, register 0xb = 0xcb
//! ```
//!
//! To write a value to a register, specify the `-w` flag, along with the
//! value to write, e.g. (for the ADT7420), the MSB of the T<sub>HIGH</sub>
//! register:
//!
//! ```console
//! % humility i2c -c 3 -d 0x48 -r 0x4 -w 0x1f
//! humility: attached via ST-Link
//! Controller I2C3, device 0x48, register 0x4 = 0x1f
//! ```
//!
//! Note that if registers are not writable, the write will (generally) be
//! silently discarded by the device; it can be useful to read the register
//! after writing it to confirm that the value is as expected:
//!
//! ```console
//! % humility i2c -c 3 -d 0x48 -r 0x4
//! humility: attached via ST-Link
//! Controller I2C3, device 0x48, register 0x4 = 0x1f
//! ```
//!
//! To determine the last mux and segment to be enabled on a particular
//! controller/port, use `--lastmux` (`-l`):
//!
//! ```console
//! % humility i2c -b front --lastmux
//! humility: attached via ST-Link V3
//! last selected mux/segment for I2C2, port F: mux 3, segment 2
//! ```
//!

use anyhow::{bail, Result};
use clap::{CommandFactory, Parser};
use hif::*;
use humility::cli::Subcommand;
use humility_cmd::{hiffy::*, CommandKind};
use humility_cmd::{Archive, Attach, Command, Dumper, Validate};

use std::collections::HashMap;
use std::fs;
use std::fs::File;
use std::io::Read;
use std::time::Instant;

use indicatif::{HumanBytes, HumanDuration};
use indicatif::{ProgressBar, ProgressStyle};

#[derive(Parser, Debug, Default)]
#[clap(name = "i2c", about = env!("CARGO_PKG_DESCRIPTION"))]
pub struct I2cArgs {
    /// sets timeout
    #[clap(
        long, short, default_value_t = 5000, value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// scan a controller for devices (by performing a raw read) or a device
    /// for registers (by doing a write followed by a read)
    #[clap(long, short, conflicts_with = "register")]
    scan: bool,

    /// scan a controller for devices at a particular register, which may
    /// have side-effects on unsporting devices
    #[clap(long, short = 'S', value_name = "register",
        conflicts_with_all = &["scan", "register", "device"],
        parse(try_from_str = parse_int::parse),
    )]
    scanreg: Option<u8>,

    /// specifies an I2C bus by name
    #[clap(long, short, value_name = "bus",
        conflicts_with_all = &["port", "controller"]
    )]
    bus: Option<String>,

    /// specifies an I2C controller
    #[clap(long, short, value_name = "controller")]
    controller: Option<u8>,

    /// specifies an I2C controller port
    #[clap(long, short, value_name = "port")]
    port: Option<String>,

    /// specifies I2C multiplexer and segment
    #[clap(long, short, value_name = "mux:segment")]
    mux: Option<String>,

    /// specifies an I2C device address
    #[clap(long, short, value_name = "address")]
    device: Option<String>,

    /// specifies register
    #[clap(long, short, value_name = "register",
        parse(try_from_str = parse_int::parse),
    )]
    register: Option<u8>,

    /// indicates a raw operation
    #[clap(long, short = 'R', conflicts_with = "register")]
    raw: bool,

    /// read block
    #[clap(long, short = 'B', conflicts_with_all = &["write", "nbytes"])]
    block: bool,

    /// specifies write value
    #[clap(long, short, value_name = "bytes")]
    write: Option<String>,

    /// perform a zero-byte write to the specified register
    #[clap(
        long,
        short = 'W',
        conflicts_with_all = &["write", "raw", "nbytes"],
        requires = "register"
    )]
    writeraw: bool,

    /// number of bytes to read from (or write to) register
    #[clap(long, short, value_name = "nbytes",
        conflicts_with = "write",
        parse(try_from_str = parse_int::parse),
    )]
    nbytes: Option<u8>,

    /// flash the specified file, assuming two byte addressing
    #[clap(long, short,
        conflicts_with_all = &[
            "write", "raw", "nbytes", "register", "scan",
            "writeraw"
        ],
        value_name = "filename",
        requires = "device",
    )]
    flash: Option<String>,

    /// indicate last selected mux/segment
    #[clap(long, short,
        conflicts_with_all = &[
            "write", "raw", "nbytes", "register", "scan",
            "writeraw", "flash", "mux", "device",
        ],
    )]
    lastmux: bool,
}

fn i2c_done(
    subargs: &I2cArgs,
    hargs: &humility_cmd::i2c::I2cArgs,
    results: &[Result<Vec<u8>, u32>],
    func: &HiffyFunction,
) -> Result<()> {
    let errmap = &func.errmap;
    let mut errs: HashMap<u32, u32> = HashMap::new();

    if (subargs.scan || subargs.scanreg.is_some()) && subargs.device.is_none() {
        println!("\nDevice scan on controller I2C{}:\n", hargs.controller);

        if subargs.scan {
            println!(
                "    R = Reserved   - = No device   \
                \\o/ = Device found   X = Timed out\n"
            );
        } else {
            println!(
                "     R = Reserved   - = No device   \
                ! = No register   X = Timed out\n"
            );
        }

        print!("{:<8}", "ADDR");

        for i in 0..16 {
            print!(" 0x{:x}", i);
        }

        println!();

        for i in 0..128 {
            if i % 16 == 0 {
                print!("0x{:02x}    ", i);
            }

            if subargs.scanreg.is_some() && i < results.len() {
                if let Ok(val) = &results[i] {
                    print!("  {:02x}", val[0]);

                    if i % 16 == 15 {
                        println!();
                    }

                    continue;
                }
            }

            print!(
                "{:>4}",
                if i >= results.len() {
                    "X"
                } else {
                    match &results[i] {
                        Ok(_) => "\\o/",
                        Err(err) => {
                            if let Some(name) = errmap.get(err) {
                                if name == "NoDevice" {
                                    "-"
                                } else if name == "NoRegister" {
                                    "!"
                                } else if name == "ReservedAddress" {
                                    "R"
                                } else {
                                    *errs.entry(*err).or_insert(0) += 1;
                                    "Err"
                                }
                            } else {
                                *errs.entry(*err).or_insert(0) += 1;
                                "???"
                            }
                        }
                    }
                }
            );

            if i % 16 == 15 {
                println!();
            }
        }
    } else if subargs.scan && subargs.device.is_some() {
        println!(
            "\nRegister scan for device 0x{:x} on I2C{}:\n",
            hargs.address.unwrap(),
            hargs.controller
        );

        println!(
            "      - = No register        ! = No device        X = Timed out\n"
        );

        print!("{:<5}", "ADDR");

        for i in 0..16 {
            print!(" 0x{:x}", i);
        }

        println!();

        for i in 0..256 {
            if i % 16 == 0 {
                print!("0x{:02x} ", i);
            }

            if i >= results.len() {
                print!("{:>4}", "X");
            } else {
                match &results[i] {
                    Ok(val) => {
                        print!("  {:02x}", val[0]);
                    }
                    Err(err) => {
                        print!(
                            "{:>4}",
                            if let Some(name) = errmap.get(err) {
                                if name == "NoRegister" {
                                    "-"
                                } else if name == "NoDevice" {
                                    "!"
                                } else {
                                    *errs.entry(*err).or_insert(0) += 1;
                                    "Err"
                                }
                            } else {
                                *errs.entry(*err).or_insert(0) += 1;
                                "???"
                            }
                        );
                    }
                }
            }

            if i % 16 == 15 {
                println!();
            }
        }
    } else if subargs.raw {
        print!(
            "Controller I2C{}, device 0x{:x}, raw {} = ",
            hargs.controller,
            hargs.address.unwrap(),
            if subargs.write.is_some() { "write" } else { "read" },
        );

        if results.is_empty() {
            println!("Timed out");
        } else {
            match &results[0] {
                Err(err) => {
                    println!("Err({})", func.strerror(*err));
                }
                Ok(val) => match subargs.nbytes {
                    Some(n) if n > 2 => {
                        println!();
                        Dumper::new().dump(val, 0);
                    }
                    Some(2) => {
                        println!("0x{:02x} 0x{:02x}", val[0], val[1]);
                    }
                    Some(1) => {
                        println!("0x{:02x}", val[0]);
                    }
                    _ => {
                        println!("Success");
                    }
                },
            }
        }
    } else {
        print!(
            "Controller I2C{}, device 0x{:x}, {}register 0x{:x} = ",
            hargs.controller,
            hargs.address.unwrap(),
            if subargs.writeraw { "raw write to " } else { "" },
            subargs.register.unwrap()
        );

        if results.is_empty() {
            println!("Timed out");
        } else {
            match &results[0] {
                Err(err) => {
                    println!("Err({})", func.strerror(*err))
                }
                Ok(val) if subargs.block => {
                    println!();
                    Dumper::new().dump(val, 0);
                }

                Ok(val) => match subargs.nbytes {
                    Some(n) if n > 2 => {
                        println!();
                        Dumper::new().dump(val, 0);
                    }
                    Some(2) => {
                        println!("0x{:02x} 0x{:02x}", val[0], val[1])
                    }
                    Some(1) => {
                        println!("0x{:02x}", val[0])
                    }
                    _ => {
                        println!("Success")
                    }
                },
            }
        }
    }

    if !errs.is_empty() {
        println!("\nError summary:\n\n  COUNT ERROR");

        for (err, count) in errs {
            println!("  {:>5} {}", count, func.strerror(err));
        }

        println!();
    }

    Ok(())
}

fn i2c(context: &mut humility::ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let hubris = context.archive.as_ref().unwrap();

    let subargs = I2cArgs::try_parse_from(subargs)?;

    if !subargs.scan
        && subargs.scanreg.is_none()
        && subargs.register.is_none()
        && !subargs.raw
        && subargs.flash.is_none()
        && !subargs.lastmux
    {
        bail!(
            "must indicate a scan (-s/-S), specify a register (-r), \
            indicate raw (-R), flash (-f), or last selected mux/segment (-l)"
        );
    }

    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;

    let (fname, args) = if subargs.flash.is_some() {
        ("I2cBulkWrite", 8)
    } else if subargs.lastmux {
        ("I2cSelectedMuxSegment", 2)
    } else {
        match (subargs.write.is_some(), subargs.writeraw) {
            (true, _) | (false, true) => ("I2cWrite", 8),
            (false, false) => ("I2cRead", 7),
        }
    };

    let funcs = context.functions()?;
    let func = funcs.get(fname, args)?;

    let hargs = humility_cmd::i2c::I2cArgs::parse(
        hubris,
        &subargs.bus,
        subargs.controller,
        &subargs.port,
        &subargs.mux,
        &subargs.device,
    )?;

    let mut ops = vec![Op::Push(hargs.controller)];

    ops.push(Op::Push(hargs.port.index));

    if subargs.lastmux {
        ops.push(Op::Call(func.id));
        ops.push(Op::Done);

        let results = context.run(core, ops.as_slice(), None)?;

        print!("last selected mux/segment for {}: ", hargs);

        match &results[0] {
            Ok(val) => {
                if val.is_empty() {
                    println!("none");
                } else {
                    if val.len() != 2 {
                        bail!("unexpected mux/segment: {:?}", val);
                    }

                    println!("mux {}, segment {}", val[0], val[1]);
                }
            }
            Err(err) => {
                println!("Err({})", func.errmap.get(err).unwrap());
            }
        }

        return Ok(());
    }

    if let Some(mux) = hargs.mux {
        ops.push(Op::Push(mux.0));
        ops.push(Op::Push(mux.1));
    } else {
        ops.push(Op::PushNone);
        ops.push(Op::PushNone);
    }

    if let Some(filename) = subargs.flash {
        ops.push(Op::Push(hargs.address.unwrap()));
        ops.push(Op::PushNone);

        let filelen = {
            let len = fs::metadata(filename.clone())?.len() as u32;

            if len > u16::MAX.into() {
                humility::msg!("file will be clamped at {}", u16::MAX);
                u16::MAX
            } else {
                len as u16
            }
        };

        //
        // We set our block size to be a conservatively small page size:
        // even though many EEPROMs can support a larger size, we want to
        // support as many variantss as we can despite its substantial
        // effect on performance.
        //
        let addr_size = 2;
        let block_size = 16u16;
        let nibble_size = block_size + addr_size;

        let data_size = context.data_size();
        let mut chunk: usize = data_size - (data_size % nibble_size as usize);
        let mut offset = 0u16;

        let mut buf = vec![0u8; chunk];
        let mut file = File::open(filename)?;
        let mut last = false;

        let sleep = funcs.get("Sleep", 1)?;

        let started = Instant::now();
        let bar = ProgressBar::new(filelen as u64);
        bar.set_style(
            ProgressStyle::default_bar().template(
                "humility: flashing [{bar:30}] {bytes}/{total_bytes}",
            ),
        );

        let base = ops;

        loop {
            let mut noffs = 0usize;

            loop {
                assert!(offset < filelen);

                if noffs + nibble_size as usize > chunk {
                    //
                    // No more room; we have filled up our chunk.
                    //
                    break;
                }

                let len = if offset + block_size > filelen {
                    filelen - offset
                } else {
                    block_size
                };

                // Plop in our address, 16-bit big endian
                let b = offset.to_be_bytes();
                buf[noffs] = b[0];
                buf[noffs + 1] = b[1];
                noffs += 2;

                // Read the file contents
                file.read_exact(&mut buf[noffs..noffs + len as usize])?;
                noffs += len as usize;

                if offset as u32 + len as u32 >= filelen as u32 {
                    //
                    // We are at the end of our file; clamp our chunk size at
                    // the amount of nibbles we actually took to prevent any
                    // spurious writes
                    //
                    chunk = noffs;
                    last = true;
                    break;
                }

                offset += len;
            }

            //
            // We have our chunk; now a HIF loop to write our chunk in
            // block_size nibbles -- making for a nibble_size write.
            //
            let mut ops = base.clone();

            ops.push(Op::Push32(0));
            ops.push(Op::PushNone);
            ops.push(Op::Label(Target(0)));
            ops.push(Op::Drop);
            ops.push(Op::Push16(nibble_size));
            ops.push(Op::Call(func.id));
            ops.push(Op::Push(5));
            ops.push(Op::Call(sleep.id));
            ops.push(Op::Drop);
            ops.push(Op::Add);
            ops.push(Op::Push32(chunk as u32));
            ops.push(Op::BranchGreaterThan(Target(0)));
            ops.push(Op::Done);

            let results = context.run(core, ops.as_slice(), Some(&buf))?;

            bar.set_position(offset.into());

            for (i, item) in results.into_iter().enumerate() {
                if let Err(err) = item {
                    bail!(
                        "failed to write block {} at offset {}: {}",
                        i,
                        offset,
                        func.strerror(err)
                    );
                }
            }

            if offset >= filelen || last {
                break;
            }
        }

        bar.finish_and_clear();

        humility::msg!(
            "flashed {} in {}",
            HumanBytes(filelen as u64),
            HumanDuration(started.elapsed())
        );

        return Ok(());
    }

    if !subargs.scan && subargs.scanreg.is_none() {
        if let Some(address) = hargs.address {
            ops.push(Op::Push(address));
        } else {
            bail!("expected device");
        }

        if let Some(ref write) = subargs.write {
            if let Some(register) = subargs.register {
                ops.push(Op::Push(register));
            } else {
                ops.push(Op::PushNone);
            }

            let bytes: Vec<&str> = write.split(',').collect();
            let mut arr = vec![];

            for byte in &bytes {
                if let Ok(val) = parse_int::parse::<u8>(byte) {
                    arr.push(val);
                } else {
                    bail!("invalid byte {}", byte)
                }
            }

            for item in &arr {
                ops.push(Op::Push(*item));
            }

            ops.push(Op::Push32(arr.len() as u32));
        } else if subargs.writeraw {
            //
            // We know that we have a register when -W has been specified; use
            // this as our 1-byte payload and set our register to None
            //
            ops.push(Op::PushNone);
            ops.push(Op::Push(subargs.register.unwrap()));
            ops.push(Op::Push(1));
        } else {
            if let Some(register) = subargs.register {
                ops.push(Op::Push(register));
            } else {
                ops.push(Op::PushNone);
            }

            if let Some(nbytes) = subargs.nbytes {
                ops.push(Op::Push(nbytes));
            } else if subargs.block {
                ops.push(Op::PushNone);
            } else {
                ops.push(Op::Push(1));
            }
        }

        ops.push(Op::Call(func.id));
    } else if let Some(address) = hargs.address {
        ops.push(Op::Push(address));
        ops.push(Op::Push(0));
        ops.push(Op::PushNone);
        ops.push(Op::Label(Target(0)));
        ops.push(Op::Drop);
        ops.push(Op::Push(1));
        ops.push(Op::Call(func.id));
        ops.push(Op::Add);
        ops.push(Op::Push(0xff));
        ops.push(Op::BranchGreaterThanOrEqualTo(Target(0)));
    } else {
        match subargs.scanreg {
            Some(reg) => ops.push(Op::Push(reg)),
            None => ops.push(Op::PushNone),
        }

        ops.push(Op::Push(0));
        ops.push(Op::PushNone);
        ops.push(Op::Label(Target(0)));
        ops.push(Op::Drop);
        ops.push(Op::Swap);
        ops.push(Op::Push(1));
        ops.push(Op::Call(func.id));
        ops.push(Op::Drop);
        ops.push(Op::Swap);
        ops.push(Op::Push(1));
        ops.push(Op::Add);
        ops.push(Op::Push(128));
        ops.push(Op::BranchGreaterThanOrEqualTo(Target(0)));
    }

    ops.push(Op::Done);

    let results = context.run(core, ops.as_slice(), None)?;

    i2c_done(&subargs, &hargs, &results, func)?;

    Ok(())
}

pub fn init() -> Command {
    Command {
        app: I2cArgs::command(),
        name: "i2c",
        run: i2c,
        kind: CommandKind::Attached {
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
        },
    }
}
