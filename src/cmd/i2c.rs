/*
 * Copyright 2020 Oxide Computer Company
 */

use crate::cmd::*;
use crate::core::Core;
use crate::hiffy::*;
use crate::hubris::*;
use crate::Args;
use anyhow::{anyhow, bail, Context, Result};
use hif::*;
use std::convert::TryFrom;
use std::io::{Cursor, Write};
use std::thread;
use std::time::Duration;
use structopt::clap::App;
use structopt::StructOpt;

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

    /// specifies an I2C bus
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

    /// specifies an I2C device address
    #[structopt(long, short, value_name = "address",
        parse(try_from_str = parse_int::parse),
    )]
    device: Option<u8>,

    /// specifies register
    #[structopt(long, short, value_name = "register",
        parse(try_from_str = parse_int::parse),
    )]
    register: Option<u8>,

    /// indicates a raw operation
    #[structopt(long, short = "R", conflicts_with = "register")]
    raw: bool,

    /// read block
    #[structopt(long, short, conflicts_with_all = &["write", "nbytes"])]
    block: bool,

    /// specifies write value
    #[structopt(long, short, value_name = "value",
        parse(try_from_str = parse_int::parse),
    )]
    write: Option<u8>,

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
}

fn i2c_done(
    subargs: &I2cArgs,
    results: &[Result<Vec<u8>, u32>],
    func: &HiffyFunction,
) -> Result<()> {
    let errmap = &func.errmap;
    let mut errs: HashMap<u32, u32> = HashMap::new();

    if (subargs.scan || subargs.scanreg.is_some()) && subargs.device.is_none() {
        println!("\nDevice scan on controller I2C{}:\n", subargs.controller);

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
            subargs.device.unwrap(),
            subargs.controller
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
        println!(
            "Controller I2C{}, device 0x{:x}, raw {} = {}",
            subargs.controller,
            subargs.device.unwrap(),
            if subargs.write.is_some() { "write" } else { "read" },
            if results.is_empty() {
                "Timed out".to_string()
            } else {
                match &results[0] {
                    Err(err) => {
                        format!("Err({})", func.strerror(*err))
                    }
                    Ok(val) => match subargs.nbytes {
                        Some(2) => {
                            format!("0x{:02x} 0x{:02x}", val[0], val[1])
                        }
                        Some(1) => {
                            format!("0x{:02x}", val[0])
                        }
                        _ => "Success".to_string(),
                    },
                }
            }
        );
    } else {
        println!(
            "Controller I2C{}, device 0x{:x}, {}register 0x{:x} = {}",
            subargs.controller,
            subargs.device.unwrap(),
            if subargs.writeraw { "raw write to " } else { "" },
            subargs.register.unwrap(),
            if results.is_empty() {
                "Timed out".to_string()
            } else {
                match &results[0] {
                    Err(err) => {
                        format!("Err({})", func.strerror(*err))
                    }
                    Ok(val) if subargs.block => {
                        let mut buf = [0u8; 1024];

                        let mut cursor = Cursor::new(&mut buf[..]);
                        for i in 0..val.len() {
                            write!(
                                cursor,
                                "0x{:02x}{}",
                                val[i],
                                if i < val.len() - 1 { " " } else { "" }
                            )
                            .unwrap();
                        }

                        let len = cursor.position() as usize;
                        String::from_utf8(buf[..len].to_vec()).unwrap()
                    }

                    Ok(val) => match subargs.nbytes {
                        Some(2) => {
                            format!("0x{:02x} 0x{:02x}", val[0], val[1])
                        }
                        Some(1) => {
                            format!("0x{:02x}", val[0])
                        }
                        _ => "Success".to_string(),
                    },
                }
            }
        );
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
    {
        bail!(
            "must indicate a scan (-s/-S), specify a register (-r), \
            or indicate raw (-R)"
        );
    }

    let (fname, args) = match (subargs.write, subargs.writeraw) {
        (Some(_), _) | (None, true) => ("I2cWrite", 8),
        (None, false) => ("I2cRead", 7),
    };

    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;
    let funcs = context.functions()?;
    let func = funcs
        .get(fname)
        .ok_or_else(|| anyhow!("did not find {} function", fname))?;

    if func.args.len() != args {
        bail!("mismatched function signature on {}", fname);
    }

    let mut port = None;

    if let Some(ref portarg) = subargs.port {
        let p = hubris
            .lookup_enum(func.args[1])
            .context("expected port to be an enum")?;

        if p.size != 1 {
            bail!("expected port to be a 1-byte enum");
        }

        for variant in &p.variants {
            if variant.name.eq_ignore_ascii_case(portarg) {
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
            .split(':')
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

    let mut ops = vec![Op::Push(subargs.controller)];

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

    if !subargs.scan && subargs.scanreg.is_none() {
        if let Some(device) = subargs.device {
            ops.push(Op::Push(device));
        } else {
            bail!("expected device");
        }

        if let Some(write) = subargs.write {
            if let Some(register) = subargs.register {
                ops.push(Op::Push(register));
            } else {
                ops.push(Op::PushNone);
            }

            ops.push(Op::Push(write));
            ops.push(Op::Push(1));
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
                if nbytes != 1 && nbytes != 2 {
                    bail!("nbytes must be 1 or 2");
                }

                ops.push(Op::Push(nbytes));
            } else if subargs.block {
                ops.push(Op::PushNone);
            } else {
                ops.push(Op::Push(1));
            }
        }

        ops.push(Op::Call(func.id));
    } else if let Some(device) = subargs.device {
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

    i2c_done(&subargs, &results, func)?;

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
