/*
 * Copyright 2020 Oxide Computer Company
 */

use crate::cmd::*;
use crate::core::Core;
use crate::hiffy::*;
use crate::hubris::*;
use crate::Args;
use anyhow::{anyhow, bail, Result};
use hif::*;
use std::thread;
use std::time::Duration;
use structopt::clap::App;
use structopt::StructOpt;

use std::fs;
use std::fs::File;
use std::io::Read;
use std::time::Instant;

use indicatif::{HumanBytes, HumanDuration};
use indicatif::{ProgressBar, ProgressStyle};

#[derive(StructOpt, Debug, Default)]
#[structopt(name = "i2c", about = "scan for and read I2C devices")]
pub struct I2cArgs {
    /// sets timeout
    #[structopt(
        long, short, default_value = "5000", value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// scan a controller for devices (by performing a raw read) or a device
    /// for registers (by doing a write followed by a read)
    #[structopt(long, short, conflicts_with = "register")]
    scan: bool,

    /// scan a controller for devices at a particular register, which may
    /// have side-effects on unsporting devices
    #[structopt(long, short = "S", value_name = "register",
        conflicts_with_all = &["scan", "register", "device"],
        parse(try_from_str = parse_int::parse),
    )]
    scanreg: Option<u8>,

    /// specifies an I2C bus by name
    #[structopt(long, short, value_name = "bus",
        conflicts_with_all = &["port", "controller"]
    )]
    bus: Option<String>,

    /// specifies an I2C controller
    #[structopt(long, short, value_name = "controller")]
    controller: Option<u8>,

    /// specifies an I2C controller port
    #[structopt(long, short, value_name = "port")]
    port: Option<String>,

    /// specifies I2C multiplexer and segment
    #[structopt(long, short, value_name = "mux:segment")]
    mux: Option<String>,

    /// specifies an I2C device address
    #[structopt(long, short, value_name = "address")]
    device: Option<String>,

    /// specifies register
    #[structopt(long, short, value_name = "register",
        parse(try_from_str = parse_int::parse),
    )]
    register: Option<u8>,

    /// indicates a raw operation
    #[structopt(long, short = "R", conflicts_with = "register")]
    raw: bool,

    /// read block
    #[structopt(long, short = "B", conflicts_with_all = &["write", "nbytes"])]
    block: bool,

    /// specifies write value
    #[structopt(long, short, value_name = "bytes")]
    write: Option<String>,

    /// perform a zero-byte write to the specified register
    #[structopt(
        long,
        short = "W",
        conflicts_with_all = &["write", "raw", "nbytes"],
        requires = "register"
    )]
    writeraw: bool,

    /// number of bytes to read from (or write to) register
    #[structopt(long, short, value_name = "nbytes",
        conflicts_with = "write",
        parse(try_from_str = parse_int::parse),
    )]
    nbytes: Option<u8>,

    /// flash the specified file, assuming two byte addressing
    #[structopt(long, short,
        conflicts_with_all = &[
            "write", "raw", "nbytes", "read", "writeall", "register", "scan",
            "writeraw"
        ],
        value_name = "filename",
        requires = "device",
    )]
    flash: Option<String>,
}

fn i2c_done(
    subargs: &I2cArgs,
    hargs: &HiffyI2cArgs,
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
            hargs.device.unwrap(),
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
            hargs.device.unwrap(),
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
                        crate::cmd::printmem(val, 0, 1, 16);
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
            hargs.device.unwrap(),
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
                    for i in 0..val.len() {
                        print!(
                            "0x{:02x}{}",
                            val[i],
                            if i < val.len() - 1 { " " } else { "" }
                        )
                    }

                    println!()
                }

                Ok(val) => match subargs.nbytes {
                    Some(n) if n > 2 => {
                        println!();
                        crate::cmd::printmem(val, 0, 1, 16);
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

fn i2c(
    hubris: &mut HubrisArchive,
    core: &mut dyn Core,
    _args: &Args,
    subargs: &Vec<String>,
) -> Result<()> {
    let subargs = I2cArgs::from_iter_safe(subargs)?;

    if !subargs.scan
        && subargs.scanreg.is_none()
        && subargs.register.is_none()
        && !subargs.raw
        && subargs.flash.is_none()
    {
        bail!(
            "must indicate a scan (-s/-S), specify a register (-r), \
            indicate raw (-R) or flash (-f)"
        );
    }

    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;

    let (fname, args) = if subargs.flash.is_some() {
        ("I2cBulkWrite", 8)
    } else {
        match (subargs.write.is_some(), subargs.writeraw) {
            (true, _) | (false, true) => ("I2cWrite", 8),
            (false, false) => ("I2cRead", 7),
        }
    };

    let funcs = context.functions()?;
    let func = funcs
        .get(fname)
        .ok_or_else(|| anyhow!("did not find {} function", fname))?;

    if func.args.len() != args {
        bail!("mismatched function signature on {}", fname);
    }

    let hargs = hiffy_i2c_args(
        hubris,
        func.args[1],
        &subargs.bus,
        subargs.controller,
        &subargs.port,
        &subargs.mux,
        &subargs.device,
    )?;

    let mut ops = vec![Op::Push(hargs.controller)];

    ops.push(Op::Push(hargs.port));

    if let Some(mux) = hargs.mux {
        ops.push(Op::Push(mux.0));
        ops.push(Op::Push(mux.1));
    } else {
        ops.push(Op::PushNone);
        ops.push(Op::PushNone);
    }

    if let Some(filename) = subargs.flash {
        ops.push(Op::Push(hargs.device.unwrap()));
        ops.push(Op::PushNone);

        let filelen = {
            let len = fs::metadata(filename.clone())?.len() as u32;

            if len > u16::MAX.into() {
                info!("file will be clamped at {}", u16::MAX);
                u16::MAX
            } else {
                len as u16
            }
        };

        let addr_size = 2;
        let block_size = 128u16;
        let nibble_size = block_size + addr_size;

        let data_size = context.data_size();
        let mut chunk: usize = data_size - (data_size % nibble_size as usize);
        let mut offset = 0u16;

        let mut buf = vec![0u8; chunk];
        let mut file = File::open(filename)?;
        let mut last = false;

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
                file.read(&mut buf[noffs..noffs + len as usize])?;
                noffs += len as usize;

                if offset as u32 + len as u32 > filelen as u32 {
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
            ops.push(Op::Add);
            ops.push(Op::Push32(chunk as u32));
            ops.push(Op::BranchGreaterThan(Target(0)));
            ops.push(Op::Done);

            context.execute(core, ops.as_slice(), Some(&buf))?;

            loop {
                if context.done(core)? {
                    break;
                }

                thread::sleep(Duration::from_millis(100));
            }

            let results = context.results(core)?;

            bar.set_position(offset.into());

            for i in 0..results.len() {
                if let Err(err) = results[i] {
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

        info!(
            "flashed {} in {}",
            HumanBytes(filelen as u64),
            HumanDuration(started.elapsed())
        );

        return Ok(());
    }

    if !subargs.scan && subargs.scanreg.is_none() {
        if let Some(device) = hargs.device {
            ops.push(Op::Push(device));
        } else {
            bail!("expected device");
        }

        if let Some(ref write) = subargs.write {
            if let Some(register) = subargs.register {
                ops.push(Op::Push(register));
            } else {
                ops.push(Op::PushNone);
            }

            let bytes: Vec<&str> = write.split(",").collect();
            let mut arr = vec![];

            for byte in &bytes {
                if let Ok(val) = parse_int::parse::<u8>(byte) {
                    arr.push(val);
                } else {
                    bail!("invalid byte {}", byte)
                }
            }

            for i in 0..arr.len() {
                ops.push(Op::Push(arr[i]));
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
    } else if let Some(device) = hargs.device {
        ops.push(Op::Push(device));
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

    context.execute(core, ops.as_slice(), None)?;

    loop {
        if context.done(core)? {
            break;
        }

        thread::sleep(Duration::from_millis(100));
    }

    let results = context.results(core)?;

    i2c_done(&subargs, &hargs, &results, func)?;

    Ok(())
}

pub fn init<'a, 'b>() -> (Command, App<'a, 'b>) {
    (
        Command::Attached {
            name: "i2c",
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
            run: i2c,
        },
        I2cArgs::clap(),
    )
}
