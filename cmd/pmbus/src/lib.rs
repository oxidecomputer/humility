// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use colored::Colorize;
use humility::cli::Subcommand;
use humility::core::Core;
use humility::hubris::*;
use humility_cmd::i2c::I2cArgs;
use humility_cmd::{hiffy::*, CommandKind};
use humility_cmd::{Archive, Attach, Command, Validate};

use anyhow::{bail, Result};
use clap::{CommandFactory, Parser};
use hif::*;
use indexmap::IndexMap;
use pmbus::commands::*;
use pmbus::*;
use std::collections::HashMap;
use std::fmt::Write;

#[derive(Parser, Debug)]
#[clap(name = "pmbus", about = env!("CARGO_PKG_DESCRIPTION"))]
struct PmbusArgs {
    /// sets timeout
    #[clap(
        long, short, default_value_t = 5000, value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// list PMBus components
    #[clap(
        long, short, conflicts_with_all = &[
            "driver", "controller", "port", "bus", "summarize"
        ]
    )]
    list: bool,

    /// summarize PMBus components
    #[clap(
        long, short, conflicts_with_all = &["driver", "controller", "port", "bus"]
    )]
    summarize: bool,

    /// command-specific help
    #[clap(long, short = 'H', value_name = "command")]
    commandhelp: Option<Vec<String>>,

    /// verbose output
    #[clap(long, short)]
    verbose: bool,

    /// show errors
    #[clap(long, short)]
    errors: bool,

    /// dry-run; show commands instead of running them
    #[clap(long = "dry-run", short = 'n')]
    dryrun: bool,

    /// force unrecognized PMBus device
    #[clap(long, short = 'F')]
    force: bool,

    /// specifies a PMBus driver
    #[clap(long, short = 'D')]
    driver: Option<String>,

    /// specifies commands to run
    #[clap(
        long,
        short = 'C',
        conflicts_with = "writes",
        value_name = "command"
    )]
    commands: Option<Vec<String>>,

    /// specifies writes to perform
    #[clap(long, short = 'w', use_value_delimiter = false)]
    writes: Option<Vec<String>>,

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

    /// specifies an I2C device address
    #[clap(long, short = 'd', value_name = "address")]
    device: Option<String>,

    /// specifies a rail within the specified device
    #[clap(long, short = 'r', value_name = "rail", use_value_delimiter = true)]
    rail: Option<Vec<String>>,
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
    writes: &[(Bitpos, Replacement)],
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
    value: Option<&str>,
) -> Result<(Bitpos, Replacement)> {
    let value = match value {
        None => {
            bail!("write \"{}\" needs a value, e.g. COMMAND=value", cmd);
        }
        Some(value) => value,
    };

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

#[allow(clippy::too_many_arguments)]
fn summarize_rail(
    subargs: &PmbusArgs,
    device: &HubrisI2cDevice,
    driver: &pmbus::Device,
    rail: &str,
    calls: &[u8],
    results: &[Result<Vec<u8>, u32>],
    func: &HiffyFunction,
    width: usize,
) -> Result<()> {
    let mut base = 0;

    print!("{:13} {:16}", device.device, rail);

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

    assert_eq!(calls[base], CommandCode::STATUS_WORD as u8);

    let status = match results[base] {
        Err(_) => None,
        Ok(ref val) => Some(STATUS_WORD::CommandData::from_slice(val).unwrap()),
    };

    print!(
        " {:>3}",
        match status {
            Some(status) => match status.get_power_good_status() {
                Some(STATUS_WORD::PowerGoodStatus::PowerGood) => "Y".green(),
                Some(STATUS_WORD::PowerGoodStatus::NoPowerGood) => "N".red(),
                None => "X".red(),
            },
            None => "-".yellow(),
        }
    );

    let mut faults = vec![];

    print!(
        " {:>4}",
        match status {
            Some(status) => {
                let _ = status.interpret(getmode, |field, value| {
                    if field.name().contains("Fault") && value.raw() != 0 {
                        faults.push(field.desc());
                    }
                });

                let str = format!("{}", faults.len());

                if !faults.is_empty() {
                    str.red()
                } else {
                    str.green()
                }
            }
            None => "-".yellow(),
        }
    );

    base += 1;

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

    if subargs.verbose && !faults.is_empty() {
        println!("{:38}|", "");
        println!("{:38}+--- {}", "", faults[0]);

        for item in faults.iter().skip(1) {
            println!("{:38}     {}", "", item);
        }

        println!();
    }

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
    let mut commands = vec![
        (CommandCode::VOUT_MODE as u8, None),
        (CommandCode::STATUS_WORD as u8, None),
    ];

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

            let harg = I2cArgs::from_device(device);

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

    let results = context.run(core, ops.as_slice(), None)?;
    let mut base = 0;

    print!(
        "{:13} {:16} {:3} {:4}",
        "DEVICE".bold(),
        "RAIL".bold(),
        "PG?".bold(),
        "#FLT".bold()
    );

    for (_, header) in commands.iter() {
        if let Some(header) = header {
            print!(" {:>width$}", header.bold(), width = width);
        }
    }

    println!();

    for (device, driver, rail, calls) in &work {
        if let Err(e) = summarize_rail(
            subargs,
            device,
            driver,
            rail,
            calls,
            &results[base..base + calls.len()],
            func,
            width,
        ) {
            println!(
                " {0}  {1} {2}  {0} ",
                "--".dimmed(),
                "error:".yellow(),
                e,
            );
        }

        base += calls.len();
    }

    Ok(())
}

fn find_rail<'a>(
    hubris: &'a HubrisArchive,
    rail: &str,
) -> Result<(I2cArgs<'a>, Option<u8>)> {
    let mut found = None;

    for device in &hubris.manifest.i2c_devices {
        if let HubrisI2cDeviceClass::Pmbus { rails } = &device.class {
            for (rnum, r) in rails.iter().enumerate() {
                if rail == r {
                    found = match found {
                        Some(_) => {
                            bail!("multiple devices match {}", rail);
                        }
                        None => Some((
                            device,
                            if rails.len() > 1 {
                                Some(rnum as u8)
                            } else {
                                None
                            },
                        )),
                    }
                }
            }
        }
    }

    match found {
        None => {
            bail!("rail {} not found", rail);
        }
        Some((device, rail)) => Ok((I2cArgs::from_device(device), rail)),
    }
}

#[derive(Debug)]
enum WriteOp {
    Modify(usize, Vec<(Bitpos, Replacement)>),
    SetBlock(Vec<u8>),
    Set,
}

impl WriteOp {
    fn from_op(
        device: pmbus::Device,
        op: pmbus::Operation,
        cmd: &str,
        code: u8,
        field: Option<&str>,
        value: Option<&str>,
    ) -> Result<Self> {
        match op {
            pmbus::Operation::SendByte => {
                if value.is_some() {
                    bail!("write of {} cannot take a value", cmd);
                }

                Ok(WriteOp::Set)
            }

            pmbus::Operation::WriteBlock => {
                if field.is_some() {
                    bail!("write {} can only take raw bytes", cmd);
                }

                let bytes: Vec<&str> = match value {
                    None => {
                        bail!(
                            "write {} needs a byte stream, e.g. {}=0x1,0xde",
                            cmd,
                            cmd
                        );
                    }
                    Some(value) => value.split(',').collect(),
                };

                let mut payload = vec![];

                for byte in &bytes {
                    if let Ok(val) = parse_int::parse::<u8>(byte) {
                        payload.push(val);
                    } else {
                        bail!("invalid byte {}", byte)
                    }
                }

                Ok(WriteOp::SetBlock(payload))
            }

            pmbus::Operation::WriteByte
            | pmbus::Operation::WriteWord
            | pmbus::Operation::WriteWord32 => {
                let count = match op {
                    pmbus::Operation::WriteByte => 1,
                    pmbus::Operation::WriteWord => 2,
                    pmbus::Operation::WriteWord32 => 4,
                    _ => {
                        panic!("unexpected operation {:?}", op);
                    }
                };

                Ok(WriteOp::Modify(
                    count,
                    vec![validate_write(device, cmd, code, field, value)?],
                ))
            }

            _ => {
                bail!("{} cannot be written", cmd);
            }
        }
    }
}

fn validate_writes(
    writecmds: &[String],
    device: pmbus::Device,
) -> Result<IndexMap<u8, (String, WriteOp)>> {
    let mut rval = IndexMap::new();
    let mut all = HashMap::new();

    for i in 0..=255u8 {
        device.command(i, |cmd| {
            all.insert(cmd.name().to_string(), (i, cmd.write_op()));
        });
    }

    for write in writecmds {
        let (cmd, field, value) = split_write(write)?;

        if let Some((code, op)) = all.get(cmd) {
            match rval.get_mut(code) {
                Some((_, WriteOp::Modify(_, ref mut writes))) => {
                    assert!(*op != pmbus::Operation::SendByte);
                    writes.push(validate_write(
                        device, cmd, *code, field, value,
                    )?);
                }
                None => {
                    rval.insert(
                        *code,
                        (
                            cmd.to_string(),
                            WriteOp::from_op(
                                device, *op, cmd, *code, field, value,
                            )?,
                        ),
                    );
                }
                _ => {
                    bail!("{} cannot be written more than once", cmd);
                }
            }
        } else {
            bail!("unrecognized PMBus command {}", cmd);
        }
    }

    Ok(rval)
}

#[rustfmt::skip::macros(bail)]
fn writes(
    subargs: &PmbusArgs,
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
    func: &HiffyFunction,
    write_func: &HiffyFunction,
) -> Result<()> {
    let hargs = match (&subargs.rail, &subargs.device) {
        (Some(rails), None) => rails
            .iter()
            .map(|rail| find_rail(hubris, rail))
            .collect::<Result<Vec<(I2cArgs, Option<u8>)>>>(),

        (_, _) => Ok(vec![(
            I2cArgs::parse(
                hubris,
                &subargs.bus,
                subargs.controller,
                &subargs.port,
                &subargs.mux,
                &subargs.device,
            )?,
            None,
        )]),
    }?;

    //
    // Determine if we have a common device.
    //
    let device = hargs
        .iter()
        .fold(None, |dev, (harg, _)| {
            Some(match dev {
                None => match &harg.device {
                    Some(device) => match pmbus::Device::from_str(device) {
                        Some(device) => device,
                        None => pmbus::Device::Common,
                    },
                    None => pmbus::Device::Common,
                },
                Some(pmbus::Device::Common) => pmbus::Device::Common,
                Some(found) => match &harg.device {
                    Some(device) => match pmbus::Device::from_str(device) {
                        Some(device) if device == found => device,
                        _ => pmbus::Device::Common,
                    },
                    None => pmbus::Device::Common,
                },
            })
        })
        .unwrap();

    //
    // Now determine what we're actually going to write.
    //
    let writecmds = subargs.writes.as_ref().unwrap();
    let writes = validate_writes(writecmds, device)?;

    let mut ops = vec![];

    //
    // First up, we are going to do any reads that we need to perform, along
    // with any operations to set a command (SendByte) as well as set an
    // entire block (WriteBlock).
    //
    for (harg, rail) in &hargs {
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
        // If we have a rail, select it.
        //
        if let Some(rnum) = rail {
            ops.push(Op::Push(CommandCode::PAGE as u8));
            ops.push(Op::Push(*rnum));
            ops.push(Op::Push(1));
            ops.push(Op::Call(write_func.id));
            ops.push(Op::DropN(3));
        }

        //
        // Now our VOUT_MODE
        //
        ops.push(Op::Push(CommandCode::VOUT_MODE as u8));
        ops.push(Op::Push(1));
        ops.push(Op::Call(func.id));
        ops.push(Op::DropN(2));

        for (&code, (_cmd, op)) in &writes {
            match op {
                WriteOp::Modify(size, _) => {
                    //
                    // For any modification, we need to first read the command.
                    //
                    ops.push(Op::Push(code));
                    ops.push(Op::Push(*size as u8));
                    ops.push(Op::Call(func.id));
                    ops.push(Op::DropN(2));
                }
                WriteOp::SetBlock(payload) => {
                    //
                    // For WriteBlock operations, we must write the size
                    // followed by the payload.
                    //
                    ops.push(Op::Push(code));
                    ops.push(Op::Push(payload.len() as u8));

                    for &byte in payload {
                        ops.push(Op::Push(byte));
                    }

                    ops.push(Op::Push(payload.len() as u8 + 1));
                    ops.push(Op::Call(write_func.id));
                    ops.push(Op::DropN(payload.len() as u8 + 3));
                }

                WriteOp::Set => {
                    //
                    // For SendByte operations, we issue a 1-byte raw write
                    // that is the command by indicating the register to be
                    // None.
                    //
                    ops.push(Op::PushNone);
                    ops.push(Op::Push(code));
                    ops.push(Op::Push(1));
                    ops.push(Op::Call(write_func.id));
                    ops.push(Op::DropN(3));
                }
            }
        }

        ops.push(Op::DropN(5));
    }

    ops.push(Op::Done);

    //
    // Now go back through our devices checking results -- and creating our
    // next batch of work, if any.
    //
    let results = context.run(core, ops.as_slice(), None)?;
    let mut ndx = 0;
    let mut additional = false;

    let success = |harg, rail: &Option<u8>, cmd| {
        if let Some(rnum) = *rail {
            humility::msg!(
                "{}, rail {}: successfully wrote {}",
                harg,
                rnum,
                cmd
            );
        } else {
            humility::msg!("{}: successfully wrote {}", harg, cmd);
        }
    };

    for (harg, rail) in &hargs {
        if let Some(rnum) = rail {
            if let Err(code) = results[ndx] {
                bail!("{}: failed to set rail {}: {}", harg, rnum, code);
            }

            ndx += 1;
        }

        //
        // Step over the mode.
        //
        ndx += 1;

        for (&_code, (cmd, op)) in &writes {
            match op {
                WriteOp::Modify(_, _) => {
                    additional = true;
                }
                WriteOp::Set | WriteOp::SetBlock(_) => match results[ndx] {
                    Err(code) => {
                        bail!(
                                "{}: failed to set {}: {}",
                                harg, cmd, write_func.strerror(code)
                            )
                    }
                    Ok(_) => {
                        success(harg, rail, cmd);
                    }
                },
            }
            ndx += 1;
        }
    }

    if !additional {
        return Ok(());
    }

    //
    // If we're here, we have addtional work to do.
    //
    let mut ops = vec![];
    let mut ndx = 0;

    for (harg, rail) in &hargs {
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
        // If we have a rail, select it.
        //
        if let Some(rnum) = rail {
            ops.push(Op::Push(CommandCode::PAGE as u8));
            ops.push(Op::Push(*rnum));
            ops.push(Op::Push(1));
            ops.push(Op::Call(write_func.id));
            ops.push(Op::DropN(3));
            ndx += 1;
        }

        let mode = match results[ndx] {
            Err(code) => {
                bail!("bad VOUT_MODE on {}: {}", harg, func.strerror(code));
            }
            Ok(ref val) => VOUT_MODE::CommandData::from_slice(val).unwrap(),
        };

        ndx += 1;

        let getmode = || mode;

        for (&code, (cmd, op)) in &writes {
            if let WriteOp::Modify(size, set) = op {
                let payload = match results[ndx] {
                    Err(code) => {
                        bail!(
                            "failed to read {}: {}",
                            cmd, func.strerror(code)
                        );
                    }
                    Ok(ref val) => val,
                };

                let mut r = None;

                if payload.len() != *size {
                    bail!(
                        "mismatch on {}: expected {}, found {}",
                        cmd, size, payload.len()
                    );
                }

                device.command(code, |cmd| {
                    r = Some(prepare_write(
                        device, code, getmode, cmd, payload, set,
                    ));
                });

                let v = r.unwrap()?;

                ops.push(Op::Push(code));

                for byte in v {
                    ops.push(Op::Push(byte));
                }

                ops.push(Op::Push(*size as u8));
                ops.push(Op::Call(write_func.id));
                ops.push(Op::DropN(*size as u8 + 2));
            }

            ndx += 1;
        }

        ops.push(Op::DropN(5));
    }

    ops.push(Op::Done);

    let results = context.run(core, ops.as_slice(), None)?;
    let mut ndx = 0;

    //
    // Now take one final lap through our results, reporting any errors
    // that we find.
    //
    for (harg, rail) in &hargs {
        if let Some(rnum) = rail {
            if let Err(code) = results[ndx] {
                bail!("failed to set rail {} on {}: Err({})", rnum, harg, code);
            }

            ndx += 1;
        }

        for (&_code, (cmd, op)) in &writes {
            if let WriteOp::Modify(_, _) = op {
                if let Err(code) = results[ndx] {
                    bail!(
                        "{}: failed to write {}: {}",
                        harg, cmd, write_func.strerror(code)
                    );
                } else {
                    success(harg, rail, cmd);
                }

                ndx += 1;
            }
        }
    }

    Ok(())
}

#[allow(clippy::print_literal)]
fn pmbus(context: &mut humility::ExecutionContext) -> Result<()> {
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let hubris = context.archive.as_ref().unwrap();
    let subargs = PmbusArgs::try_parse_from(subargs)?;

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

    let core = &mut **context.core.as_mut().unwrap();

    if core.is_dump() {
        bail!("can only list PMBus devices on a dump");
    }

    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;
    let funcs = context.functions()?;
    let func = funcs.get("I2cRead", 7)?;
    let write_func = funcs.get("I2cWrite", 8)?;

    if subargs.summarize {
        summarize(&subargs, hubris, core, &mut context, func, write_func)?;
        return Ok(());
    }

    if subargs.writes.is_some() {
        writes(&subargs, hubris, core, &mut context, func, write_func)?;
        return Ok(());
    }

    let hargs = match (&subargs.rail, &subargs.device) {
        (Some(rails), None) => {
            if rails.len() > 1 {
                bail!("cannot specify more than one rail");
            }

            find_rail(hubris, &rails[0])?.0
        }

        (_, _) => I2cArgs::parse(
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
    } else if let Some(driver) = hargs.device {
        match pmbus::Device::from_str(&driver) {
            Some(device) => device,
            None => pmbus::Device::Common,
        }
    } else {
        pmbus::Device::Common
    };

    let (all, _) = all_commands(device);

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

    let mut setrail = false;

    //
    // If we have a rail specified, we want to set that first.
    //
    if let Some(railargs) = &subargs.rail {
        if railargs.len() != 1 {
            bail!("rails length?!");
        }

        let rail = &railargs[0];

        let rails = match rails {
            Some(rails) => rails,
            None => bail!("rail specified, but device has unknown rails"),
        };

        if rails.is_empty() {
            bail!("rail specified, but device has no defined rails");
        }

        //
        // We want to allow our rail to be specified by number or by name.
        //
        let rnum = match parse_int::parse::<u8>(rail) {
            Ok(rnum) => {
                if rails.len() == 1 {
                    bail!("rail specified, but device only has one rail");
                }

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

        if rails.len() > 1 {
            ops.push(Op::Push(page));
            ops.push(Op::Push(rnum as u8));
            ops.push(Op::Push(1));
            ops.push(Op::Call(write_func.id));
            ops.push(Op::DropN(3));
            cmds.push(page);
            setrail = true;
        }
    }

    let mut addcmd = |cmd: &dyn pmbus::Command, code| {
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

    if subargs.dryrun {
        return Ok(());
    }

    if cmds.is_empty() {
        bail!("no command to run");
    }

    ops.push(Op::Done);

    let results = context.run(core, ops.as_slice(), None)?;

    let base = if setrail {
        match results[0] {
            Err(code) => {
                bail!("couldn't set rail: {}", write_func.strerror(code));
            }
            Ok(_) => 1,
        }
    } else {
        0
    };

    let (mode, ndx) = if cmds[base] == vout {
        let mode = match results[base] {
            Err(code) => {
                bail!("can't read VOUT_MODE: {}", func.strerror(code));
            }
            Ok(ref val) => VOUT_MODE::CommandData::from_slice(val).unwrap(),
        };

        (Some(mode), base + 1)
    } else {
        (None, base)
    };

    let getmode = || match mode {
        Some(mode) => mode,
        None => {
            panic!("unexpected call to get VOutMode");
        }
    };

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

    Ok(())
}

pub fn init() -> Command {
    Command {
        app: PmbusArgs::command(),
        name: "pmbus",
        run: pmbus,
        kind: CommandKind::Attached {
            archive: Archive::Required,
            attach: Attach::Any,
            validate: Validate::Booted,
        },
    }
}
