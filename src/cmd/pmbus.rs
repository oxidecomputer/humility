/*
 * Copyright 2020 Oxide Computer Company
 */

use crate::cmd::{Archive, Attach, Validate};
use crate::core::Core;
use crate::hiffy::*;
use crate::hubris::*;
use crate::Args;
use multimap::MultiMap;
use std::thread;

use anyhow::{anyhow, bail, Result};
use hif::*;
use pmbus::commands::*;
use pmbus::*;
use std::collections::HashMap;
use std::fmt::Write;
use std::time::Duration;
use structopt::clap::App;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(name = "pmbus", about = "scan for and read PMBus devices")]
struct PmbusArgs {
    /// sets timeout
    #[structopt(
        long, short, default_value = "5000", value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// list PMBus components
    #[structopt(
        long, short, conflicts_with_all = &[
            "driver", "controller", "port", "bus", "summarize"
        ]
    )]
    list: bool,

    /// summarize PMBus components
    #[structopt(
        long, short, conflicts_with_all = &["driver", "controller", "port", "bus"]
    )]
    summarize: bool,

    /// command-specific help
    #[structopt(long, short = "H", value_name = "command")]
    commandhelp: Option<Vec<String>>,

    /// verbose output
    #[structopt(long, short)]
    verbose: bool,

    /// show errors
    #[structopt(long, short)]
    errors: bool,

    /// dry-run; show commands instead of running them
    #[structopt(long = "dry-run", short = "n")]
    dryrun: bool,

    /// force unrecognized PMBus device
    #[structopt(long, short = "F")]
    force: bool,

    /// specifies a PMBus driver
    #[structopt(long, short = "D")]
    driver: Option<String>,

    /// specifies commands to run
    #[structopt(
        long,
        short = "C",
        conflicts_with = "writes",
        value_name = "command"
    )]
    commands: Option<Vec<String>>,

    /// specifies writes to perform
    #[structopt(long, short = "w")]
    writes: Option<Vec<String>>,

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

    /// specifies an I2C device address
    #[structopt(long, short = "d", value_name = "address")]
    device: Option<String>,

    /// specifies a rail within the specified device
    #[structopt(long, short = "r", value_name = "rail")]
    rail: Option<String>,
}

fn all_commands(
    device: pmbus::Device,
) -> (HashMap<String, u8>, HashMap<u8, String>) {
    let mut all: HashMap<String, u8> = HashMap::new();
    let mut bycode: HashMap<u8, String> = HashMap::new();

    for i in 0..=255u8 {
        device.command(i, |cmd| {
            all.insert(cmd.name().to_string(), i);
            bycode.insert(i, cmd.name().to_string());
        });
    }

    (all, bycode)
}

fn print_command(
    device: pmbus::Device,
    code: u8,
    command: &dyn pmbus::Command,
) {
    use std::*;

    println!("0x{:02x} {}", code, command.name());

    let mut bitfields = false;

    let fields = |field: &dyn Field| {
        let bits = field.bits();
        let nbits = bits.1 .0 as usize;

        let b = if nbits == 1 {
            format!("b{}", bits.0 .0)
        } else {
            format!("b{}:{}", bits.0 .0 + bits.1 .0 - 1, bits.0 .0)
        };

        if field.bitfield() {
            bitfields = true;

            println!("     | {:6} {:30} <= {}", b, field.name(), field.desc());

            let mut last = None;

            let sentinels = |val: &dyn Value| {
                let v =
                    format!("0b{:0w$b} = {}", val.raw(), val.name(), w = nbits);

                println!("     | {:6} {:30} <- {}", "", v, val.desc());

                if let Some(last) = last {
                    if last >= val.raw() {
                        panic!("values are out of order");
                    }
                }

                last = Some(val.raw());
            };

            device.sentinels(code, field.bits().0, sentinels).unwrap();
        }
    };

    device.fields(code, fields).unwrap();

    if bitfields {
        println!(
            "     +------------------------------------------\
            -----------------------------\n"
        );
    }
}

#[rustfmt::skip::macros(println)]
fn print_result(
    subargs: &PmbusArgs,
    device: pmbus::Device,
    code: u8,
    mode: impl Fn() -> VOutModeCommandData,
    command: &dyn pmbus::Command,
    result: &Result<Vec<u8>, u32>,
    errmap: &HashMap<u32, String>,
) -> Result<()> {
    let nbytes = match command.read_op() {
        pmbus::Operation::ReadByte => Some(1),
        pmbus::Operation::ReadWord => Some(2),
        pmbus::Operation::ReadWord32 => Some(4),
        pmbus::Operation::ReadBlock => None,
        _ => {
            unreachable!();
        }
    };

    let name = command.name();
    let cmdstr = format!("0x{:02x} {:<25}", code, name);

    fn printchar(val: u8) {
        let c = val as char;

        if c.is_ascii() && !c.is_ascii_control() {
            print!("{}", c);
        } else {
            print!(".");
        }
    }

    match result {
        Err(err) => {
            if subargs.errors {
                println!("{} Err({})", cmdstr, errmap.get(err).unwrap());
            }
        }

        Ok(val) => {
            if val.is_empty() && subargs.errors {
                println!("{} Timed out", cmdstr);
                return Ok(());
            }

            if let Some(nbytes) = nbytes {
                if val.len() != nbytes {
                    println!("{} Short read: {:x?}", cmdstr, val);
                    return Ok(());
                }
            }

            let mut printed = false;
            let mut interpreted = false;

            let printraw = |interpret: bool| {
                print!("{}", cmdstr);

                if nbytes.is_none() {
                    let w = 8;

                    for i in 0..val.len() {
                        if i > 0 && i % w == 0 {
                            print!(" |");

                            for &j in val[(i - w)..i].iter() {
                                printchar(j);
                            }

                            if !interpret {
                                print!("\n{:30}", "");
                            } else {
                                print!("\n     | {:22} ", "");
                            }
                        }

                        print!(" 0x{:02x}", val[i]);
                    }

                    let rem = val.len() % w;

                    if rem != 0 {
                        print!("{:width$} |", "", width = (w - rem) * 5);
                        let base = val.len() - rem;

                        for i in 0..rem {
                            printchar(val[base + i]);
                        }
                    }
                } else {
                    print!(" 0x");
                    for i in (0..val.len()).rev() {
                        print!("{:02x}", val[i]);
                    }
                }

                println!();
            };

            let err = device.interpret(code, val, mode, |field, value| {
                if !field.bitfield() {
                    let width = (field.bits().1 .0 / 4) as usize;

                    println!(
                       "{} 0x{:0width$x} = {}",
                       cmdstr, value.raw(), value, width = width
                    );

                    interpreted = true;
                    return;
                }

                if !subargs.verbose {
                    return;
                }

                if !interpreted {
                    printraw(true);
                    println!("     |");
                    interpreted = true;
                }

                let (pos, width) = field.bits();

                let bits = if width.0 == 1 {
                    format!("b{}", pos.0)
                } else {
                    format!("b{}:{}", pos.0 + width.0 - 1, pos.0)
                };

                let value = format!("{}", value);

                println!("     | {:6} {:<30} <= {}", bits, value, field.name());
                printed = true;
            });

            if err.is_err() && subargs.errors {
                println!("{} {:?}", cmdstr, err);
            }

            if !interpreted {
                printraw(false);
            }

            if printed {
                println!(
                    "     +------------------------------------------\
                    -----------------------------\n"
                );
            }
        }
    }

    Ok(())
}

fn prepare_write(
    device: pmbus::Device,
    code: u8,
    mode: impl Fn() -> VOutModeCommandData,
    command: &dyn pmbus::Command,
    payload: &[u8],
    writes: &Vec<(Bitpos, Replacement)>,
) -> Result<Vec<u8>> {
    let name = command.name();
    let mut rval = payload.to_vec();
    let mut replaced = vec![false; writes.len()];

    let err = device.mutate(code, &mut rval, mode, |field, _| {
        let pos = field.bits().0;

        for i in 0..writes.len() {
            if writes[i].0 == pos {
                replaced[i] = true;
                return Some(writes[i].1);
            }
        }

        None
    });

    if err.is_err() {
        bail!("failed to mutate {}: {:?}", name, err);
    }

    for i in 0..writes.len() {
        if !replaced[i] {
            bail!("failed to replace {} at position {}", name, writes[i].0 .0);
        }
    }

    Ok(rval.to_vec())
}

fn validate_write(
    device: pmbus::Device,
    cmd: &str,
    code: u8,
    field: Option<&str>,
    value: &str,
) -> Result<(Bitpos, Replacement)> {
    if let Some(field) = field {
        //
        // Iterate over the fields for this command to make sure we have
        // the specified field
        //
        let mut all = vec![];
        let mut found = None;

        device
            .fields(code, |f| {
                if f.name() == field {
                    found = Some(f.bits());
                }

                all.push(f.name());
            })
            .unwrap();

        match found {
            None => {
                bail!(
                    "field {} not found in {}; expected one of: {}",
                    field,
                    cmd,
                    all.join(", ")
                );
            }
            Some(bits) => {
                let mut replacement: Option<Replacement> = None;
                let mut all = vec![];

                device
                    .sentinels(code, bits.0, |s| {
                        if s.name() == value {
                            replacement = Some(Replacement::Integer(s.raw()));
                        }

                        all.push(s.name());
                    })
                    .unwrap();

                match replacement {
                    Some(replacement) => Ok((bits.0, replacement)),

                    None => {
                        bail!(
                            "field {} of {} cannot be set to {}; \
                            expected one of: {}",
                            field,
                            cmd,
                            value,
                            all.join(", ")
                        )
                    }
                }
            }
        }
    } else {
        let mut bits = None;
        let mut bitfields = false;

        device
            .fields(code, |f| {
                if !f.bitfield() {
                    bits = Some(f.bits());
                } else {
                    bitfields = true;
                }
            })
            .unwrap();

        if let Some(bits) = bits {
            if let Ok(val) = parse_int::parse::<u32>(value) {
                Ok((bits.0, Replacement::Integer(val)))
            } else if let Ok(val) = value.parse::<f32>() {
                Ok((bits.0, Replacement::Float(val)))
            } else {
                bail!("illegal value: {}", value);
            }
        } else if bitfields {
            bail!("{} has bitfields which must be set explicitly", cmd);
        } else {
            bail!("can't write to {}: data has unknown type", cmd);
        }
    }
}

fn split_write(write: &str) -> Result<(&str, Option<&str>, Option<&str>)> {
    let expr: Vec<&str> = write.split('=').collect();

    if expr.len() > 2 {
        bail!("write \"{}\" has an ambiguous value", write);
    }

    if expr.len() < 2 {
        Ok((expr[0], None, None))
    } else {
        let field: Vec<&str> = expr[0].split('.').collect();

        if field.len() > 1 {
            if field.len() > 2 {
                bail!("write \"{}\" has too many field delimiters", write);
            }

            Ok((field[0], Some(field[1]), Some(expr[1])))
        } else {
            Ok((expr[0], None, Some(expr[1])))
        }
    }
}

fn summarize_rail(
    device: &HubrisI2cDevice,
    driver: &pmbus::Device,
    rail: &str,
    calls: &Vec<u8>,
    results: &[Result<Vec<u8>, u32>],
    func: &HiffyFunction,
    width: usize,
) -> Result<()> {
    let mut base = 0;

    let mux = match (device.mux, device.segment) {
        (Some(m), Some(s)) => format!("{}:{}", m, s),
        (None, None) => "-".to_string(),
        (_, _) => "?:?".to_string(),
    };

    print!(
        "{} {:2} {:3} 0x{:02x} {:13} {:13}",
        device.controller,
        device.port.name,
        mux,
        device.address,
        device.device,
        rail
    );

    if calls[base] == CommandCode::PAGE as u8 {
        //
        // This is a selected rail -- we just want to be sure that it worked
        //
        if let Err(code) = results[base] {
            bail!("rail selection failed: {}", func.strerror(code));
        }

        base += 1;
    }

    let mode = if calls[base] == CommandCode::VOUT_MODE as u8 {
        match results[base] {
            Err(code) => {
                bail!("can't read VOUT_MODE: {}", func.strerror(code));
            }
            Ok(ref val) => {
                base += 1;
                Some(VOUT_MODE::CommandData::from_slice(val).unwrap())
            }
        }
    } else {
        None
    };

    let getmode = || match mode {
        Some(mode) => mode,
        None => {
            panic!("unexpected call to VOutMode");
        }
    };

    for i in base..calls.len() {
        let code = calls[i];
        match results[i] {
            Err(_) => {
                print!(" {:>width$}", "-", width = width);
            }
            Ok(ref val) => {
                let mut interpreted = false;
                let mut str = String::new();

                let err =
                    driver.interpret(code, val, getmode, |field, value| {
                        if !field.bitfield() {
                            write!(&mut str, "{}", value).unwrap();
                            interpreted = true;
                        }
                    });

                if err.is_err() {
                    print!(" {:>width$?}", err, width = width);
                    continue;
                }

                if !interpreted {
                    write!(&mut str, "0x").unwrap();
                    for i in (0..val.len()).rev() {
                        write!(&mut str, "{:02x}", val[i]).unwrap();
                    }
                }

                print!(" {:>width$}", str, width = width);
            }
        }
    }

    println!();

    Ok(())
}

fn summarize(
    subargs: &PmbusArgs,
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
    func: &HiffyFunction,
    write_func: &HiffyFunction,
) -> Result<()> {
    let page = CommandCode::PAGE as u8;
    let (all, bycode) = all_commands(pmbus::Device::Common);

    let mut width = 9;
    let mut commands = vec![(CommandCode::VOUT_MODE as u8, None)];

    if let Some(ref cmds) = subargs.commands {
        for cmd in cmds {
            if let Some(code) = all.get(cmd) {
                commands.push((*code, Some(bycode.get(code).unwrap().as_str())))
            } else {
                bail!("unrecognized command {}", cmd);
            }
        }

        width = 15;
    } else {
        commands.extend_from_slice(&[
            (CommandCode::READ_VIN as u8, Some("VIN")),
            (CommandCode::READ_VOUT as u8, Some("VOUT")),
            (CommandCode::READ_IOUT as u8, Some("IOUT")),
            (CommandCode::READ_TEMPERATURE_1 as u8, Some("TEMP_1")),
        ]);
    }

    let mut ops = vec![];
    let mut work = vec![];

    for device in &hubris.manifest.i2c_devices {
        if let HubrisI2cDeviceClass::Pmbus { rails } = &device.class {
            let driver = match pmbus::Device::from_str(&device.device) {
                Some(device) => device,
                None => pmbus::Device::Common,
            };

            let harg = crate::i2c::I2cArgs::from_device(device);

            ops.push(Op::Push(harg.controller));
            ops.push(Op::Push(harg.port.index));

            if let Some(mux) = harg.mux {
                ops.push(Op::Push(mux.0));
                ops.push(Op::Push(mux.1));
            } else {
                ops.push(Op::PushNone);
                ops.push(Op::PushNone);
            }

            ops.push(Op::Push(harg.address.unwrap()));

            //
            // We have the arguments for our device pushed.  Now iterate over
            // each rail, selecting it as needed...
            //
            for (rnum, rail) in rails.iter().enumerate() {
                let mut calls = vec![];

                if rails.len() > 1 {
                    ops.push(Op::Push(page));
                    ops.push(Op::Push(rnum as u8));
                    ops.push(Op::Push(1));
                    ops.push(Op::Call(write_func.id));
                    ops.push(Op::DropN(3));
                    calls.push(page);
                }

                //
                // For each of the commands that we need to run, add a call
                // for it
                //
                for (code, _) in &commands {
                    driver.command(*code, |cmd| {
                        let op = match cmd.read_op() {
                            pmbus::Operation::ReadByte => Op::Push(1),
                            pmbus::Operation::ReadWord => Op::Push(2),
                            pmbus::Operation::ReadWord32 => Op::Push(4),
                            pmbus::Operation::ReadBlock => Op::PushNone,
                            _ => {
                                return;
                            }
                        };

                        ops.push(Op::Push(*code));
                        ops.push(op);
                        ops.push(Op::Call(func.id));
                        ops.push(Op::DropN(2));
                        calls.push(*code as u8);
                    });
                }

                work.push((device, driver, rail, calls));
            }

            ops.push(Op::DropN(5));
        }
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
    let mut base = 0;

    print!(
        "{} {:2} {} {} {:13} {:13}",
        "C", "P", "MUX", "ADDR", "DEVICE", "RAIL"
    );

    for (_, header) in commands.iter() {
        if let Some(header) = header {
            print!(" {:>width$}", header, width = width);
        }
    }

    println!();

    for (device, driver, rail, calls) in &work {
        summarize_rail(
            device,
            driver,
            rail,
            calls,
            &results[base..base + calls.len()],
            func,
            width,
        )?;

        base += calls.len();
    }

    Ok(())
}

fn pmbus(
    hubris: &mut HubrisArchive,
    core: &mut dyn Core,
    _args: &Args,
    subargs: &Vec<String>,
) -> Result<()> {
    let subargs = PmbusArgs::from_iter_safe(subargs)?;

    if subargs.list {
        println!(
            "{} {:2} {} {} {:13} {}",
            "C", "P", "MUX", "ADDR", "DEVICE", "RAILS"
        );

        for device in &hubris.manifest.i2c_devices {
            if let HubrisI2cDeviceClass::Pmbus { rails } = &device.class {
                let mux = match (device.mux, device.segment) {
                    (Some(m), Some(s)) => format!("{}:{}", m, s),
                    (None, None) => "-".to_string(),
                    (_, _) => "?:?".to_string(),
                };

                println!(
                    "{} {:2} {:3} 0x{:02x} {:13} {}",
                    device.controller,
                    device.port.name,
                    mux,
                    device.address,
                    device.device,
                    rails.join(", "),
                )
            }
        }

        return Ok(());
    }

    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;
    let funcs = context.functions()?;
    let func = funcs
        .get("I2cRead")
        .ok_or_else(|| anyhow!("did not find I2cRead function"))?;

    if func.args.len() != 7 {
        bail!("mismatched function signature on I2cRead");
    }

    let write_func = funcs
        .get("I2cWrite")
        .ok_or_else(|| anyhow!("did not find I2cWrite function"))?;

    if write_func.args.len() != 8 {
        bail!("mismatched function signature on I2cWrite");
    }

    if subargs.summarize {
        summarize(&subargs, hubris, core, &mut context, func, write_func)?;
        return Ok(());
    }

    let hargs = match (&subargs.rail, &subargs.device) {
        (Some(rail), None) => {
            let mut found = None;

            for device in &hubris.manifest.i2c_devices {
                if let HubrisI2cDeviceClass::Pmbus { rails } = &device.class {
                    for r in rails {
                        if rail == r {
                            found = match found {
                                Some(_) => {
                                    bail!("multiple devices match {}", rail);
                                }
                                None => Some(device),
                            }
                        }
                    }
                }
            }

            match found {
                None => {
                    bail!("rail {} not found", rail);
                }
                Some(device) => crate::i2c::I2cArgs::from_device(device),
            }
        }

        (_, _) => crate::i2c::I2cArgs::parse(
            hubris,
            &subargs.bus,
            subargs.controller,
            &subargs.port,
            &subargs.mux,
            &subargs.device,
        )?,
    };

    let device = if let Some(driver) = &subargs.driver {
        match pmbus::Device::from_str(driver) {
            Some(device) => device,
            None => {
                bail!("unknown device \"{}\"", driver);
            }
        }
    } else {
        if let Some(driver) = hargs.device {
            match pmbus::Device::from_str(&driver) {
                Some(device) => device,
                None => pmbus::Device::Common,
            }
        } else {
            pmbus::Device::Common
        }
    };

    let (all, bycode) = all_commands(device);

    if let Some(ref commands) = subargs.commandhelp {
        if commands.is_empty() || commands[0] == "all" {
            for code in 0..0xffu8 {
                device.command(code, |cmd| {
                    print_command(device, code, cmd);
                });
            }

            return Ok(());
        }

        for cmd in commands {
            let code = match all.get(cmd) {
                Some(code) => *code,
                None => match parse_int::parse::<u8>(cmd) {
                    Ok(code) => code,
                    Err(_) => {
                        bail!(
                            "unrecognized PMBus command {}; \
                              use 'all' for all commands",
                            cmd
                        );
                    }
                },
            };

            device.command(code, |cmd| {
                print_command(device, code, cmd);
            });
        }

        return Ok(());
    }

    let mut ops = vec![];
    let mut cmds = vec![];

    ops.push(Op::Push(hargs.controller));
    ops.push(Op::Push(hargs.port.index));

    if let Some(mux) = hargs.mux {
        ops.push(Op::Push(mux.0));
        ops.push(Op::Push(mux.1));
    } else {
        ops.push(Op::PushNone);
        ops.push(Op::PushNone);
    }

    if let Some(address) = hargs.address {
        ops.push(Op::Push(address));
    } else {
        bail!("no device specified");
    }

    let rails = match hargs.class {
        HubrisI2cDeviceClass::Pmbus { rails } => Some(rails),
        _ => {
            if !subargs.force {
                bail!("not a recognized PMBus device; -F to force");
            } else {
                None
            }
        }
    };

    let mut run = [true; 256];

    if let Some(ref commands) = subargs.commands {
        if commands.is_empty() {
            bail!("expected a command");
        }

        run.fill(false);

        for cmd in commands {
            if let Some(code) = all.get(cmd) {
                run[*code as usize] = true;
            } else if let Ok(code) = parse_int::parse::<u8>(cmd) {
                run[code as usize] = true;
            } else {
                bail!(
                    "unrecognized PMBus command {}; \
                     use -H for command help",
                    cmd
                );
            }
        }
    }

    let mut writes = MultiMap::new();
    let mut write_ops = vec![];
    let mut send_bytes = vec![];

    if let Some(ref writecmds) = subargs.writes {
        run.fill(false);

        for write in writecmds {
            let (cmd, field, value) = split_write(write)?;

            match all.get(cmd) {
                Some(code) => {
                    let mut send_byte = false;

                    device.command(*code, |cmd| {
                        if cmd.write_op() == pmbus::Operation::SendByte {
                            send_byte = true;
                        }
                    });

                    if send_byte {
                        if value.is_some() {
                            bail!("write of \"{}\" cannot take a value", cmd);
                        }

                        send_bytes.push((*code, cmd));
                    } else {
                        run[*code as usize] = true;

                        let value = match value {
                            None => {
                                bail!(
                                    "write \"{}\" needs a value, \
                                    e.g. COMMAND=value",
                                    cmd
                                );
                            }
                            Some(value) => value,
                        };

                        writes.insert(
                            *code,
                            validate_write(device, cmd, *code, field, value)?,
                        );
                    }
                }
                None => {
                    bail!("unrecognized PMBus command {}", cmd);
                }
            }
        }
    }

    //
    // If we have a rail specified, we want to set that first.
    //
    if let Some(rail) = &subargs.rail {
        let rails = match rails {
            Some(rails) => rails,
            None => bail!("rail specified, but device has unknown rails"),
        };

        if rails.len() == 0 {
            bail!("rail specified, but device has no defined rails");
        }

        if rails.len() == 1 {
            bail!("rail specified, but device only has one rail");
        }

        //
        // We want to allow our rail to be specified by number or by name.
        //
        let rnum = match parse_int::parse::<u8>(&rail) {
            Ok(rnum) => {
                if rnum as usize >= rails.len() {
                    bail!("invalid rail number {}", rnum);
                }
                rnum as usize
            }
            _ => match rails.iter().position(|r| r == rail) {
                Some(rnum) => rnum,
                None => {
                    bail!("invalid rail; expected one of: {}", rails.join(", "))
                }
            },
        };

        let page = pmbus::commands::CommandCode::PAGE as u8;

        ops.push(Op::Push(page));
        ops.push(Op::Push(rnum as u8));
        ops.push(Op::Push(1));
        ops.push(Op::Call(write_func.id));
        ops.push(Op::DropN(3));
        cmds.push(page);
    }

    if !writes.is_empty() {
        write_ops = ops.clone();
    }

    let mut addcmd = |cmd: &dyn Command, code| {
        let op = match cmd.read_op() {
            pmbus::Operation::ReadByte => Op::Push(1),
            pmbus::Operation::ReadWord => Op::Push(2),
            pmbus::Operation::ReadWord32 => Op::Push(4),
            pmbus::Operation::ReadBlock => Op::PushNone,
            _ => {
                return;
            }
        };

        if subargs.dryrun {
            println!("0x{:02x} {:?}", code, cmd);
        }

        ops.push(Op::Push(code));
        ops.push(op);
        ops.push(Op::Call(func.id));
        ops.push(Op::Drop);
        ops.push(Op::Drop);
        cmds.push(code);
    };

    let vout = pmbus::commands::CommandCode::VOUT_MODE as u8;
    device.command(vout, |cmd| addcmd(cmd, vout));

    for i in 0..=255u8 {
        if run[i as usize] {
            device.command(i, |cmd| addcmd(cmd, i));
        }
    }

    //
    // Now add any SendByte commands that we might have.
    //
    for (code, cmd) in &send_bytes {
        //
        // For SendByte operations, we issue a 1-byte raw write that is the
        // command by indicating the register to be None.
        //
        ops.push(Op::PushNone);
        ops.push(Op::Push(*code));
        ops.push(Op::Push(1));
        ops.push(Op::Call(write_func.id));
        ops.push(Op::DropN(3));

        if subargs.dryrun {
            println!("0x{:02x} {} SendByte", code, cmd);
        }
    }

    if subargs.dryrun {
        return Ok(());
    }

    if cmds.is_empty() {
        bail!("no command to run");
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

    let base = match &subargs.rail {
        Some(_) => match results[0] {
            Err(code) => {
                bail!("couldn't set rail: {}", write_func.strerror(code));
            }
            Ok(_) => 1,
        },
        None => 0,
    };

    let (mode, ndx) = if cmds[base] == vout {
        let mode = match results[base] {
            Err(code) => {
                bail!("can't read VOUT_MODE: {}", func.strerror(code));
            }
            Ok(ref val) => VOUT_MODE::CommandData::from_slice(val).unwrap(),
        };

        (Some(mode), base + 1 + send_bytes.len())
    } else {
        (None, base + send_bytes.len())
    };

    let getmode = || match mode {
        Some(mode) => mode,
        None => {
            panic!("unexpected call to get VOutMode");
        }
    };

    if !send_bytes.is_empty() {
        let offs = ndx - send_bytes.len();

        for i in 0..send_bytes.len() {
            let (_, cmd) = &send_bytes[i];

            match results[i + offs] {
                Err(code) => {
                    bail!(
                        "failed to write {}: Err({})",
                        cmd,
                        write_func.strerror(code)
                    );
                }
                Ok(_) => {
                    info!("successfully wrote {}", cmd);
                }
            }
        }
    }

    if !writes.is_empty() {
        for i in ndx..results.len() {
            let payload = match results[i] {
                Err(code) => {
                    bail!(
                        "failed to read {:x}: Err({})",
                        cmds[i],
                        func.strerror(code)
                    );
                }
                Ok(ref val) => val,
            };

            if let Some(write) = writes.get_vec(&cmds[i]) {
                let mut r = None;

                device.command(cmds[i], |cmd| {
                    r = Some(prepare_write(
                        device, cmds[i], getmode, cmd, payload, write,
                    ));
                });

                let v = r.unwrap()?;

                write_ops.push(Op::Push(cmds[i]));

                for &item in &v {
                    write_ops.push(Op::Push(item));
                }

                write_ops.push(Op::Push(v.len() as u8));
                write_ops.push(Op::Call(write_func.id));
                write_ops.push(Op::DropN(v.len() as u8 + 2));
            }
        }

        write_ops.push(Op::Done);

        context.execute(core, write_ops.as_slice(), None)?;

        loop {
            if context.done(core)? {
                break;
            }

            thread::sleep(Duration::from_millis(100));
        }

        let wresults = context.results(core)?;

        for i in base..wresults.len() {
            let name = bycode.get(&cmds[i + ndx - base]).unwrap();

            match wresults[i] {
                Err(code) => {
                    bail!(
                        "failed to write {}: Err({})",
                        name,
                        write_func.strerror(code)
                    );
                }
                Ok(_) => {
                    info!("successfully wrote {}", name);
                }
            }
        }
    } else {
        for i in ndx..results.len() {
            let mut r = Ok(());

            device.command(cmds[i], |cmd| {
                r = print_result(
                    &subargs,
                    device,
                    cmds[i],
                    getmode,
                    cmd,
                    &results[i],
                    &func.errmap,
                );
            });

            r?;
        }
    }

    Ok(())
}

pub fn init<'a, 'b>() -> (crate::cmd::Command, App<'a, 'b>) {
    (
        crate::cmd::Command::Attached {
            name: "pmbus",
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
            run: pmbus,
        },
        PmbusArgs::clap(),
    )
}
