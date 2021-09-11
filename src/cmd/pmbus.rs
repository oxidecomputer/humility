/*
 * Copyright 2020 Oxide Computer Company
 */

use crate::cmd::{Archive, Attach, Validate};
use crate::core::Core;
use crate::hiffy::*;
use crate::hubris::*;
use crate::Args;
use multimap::MultiMap;
use std::convert::TryFrom;
use std::thread;

use anyhow::{anyhow, bail, Context, Result};
use hif::*;
use pmbus::commands::*;
use pmbus::*;
use std::collections::HashMap;
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

    /// verbose output
    #[structopt(long, short)]
    verbose: bool,

    /// show errors
    #[structopt(long, short)]
    errors: bool,

    /// dry-run; show commands instead of running them
    #[structopt(long = "dry-run", short = "n")]
    dryrun: bool,

    /// specifies a PMBus driver
    #[structopt(long, short = "D")]
    driver: Option<String>,

    /// specifies commands to run
    #[structopt(long, short = "C", conflicts_with = "writes")]
    commands: Option<Vec<String>>,

    /// specifies writes to perform
    #[structopt(long, short = "w")]
    writes: Option<Vec<String>>,

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

    /// specifies an I2C device address
    #[structopt(long, short = "d", value_name = "address",
        parse(try_from_str = parse_int::parse),
    )]
    device: u8,
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

#[rustfmt::skip::macros(println)]
fn print_result(
    subargs: &PmbusArgs,
    device: pmbus::Device,
    code: u8,
    mode: impl Fn() -> VOutMode,
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
            if val.len() == 0 {
                if subargs.errors {
                    println!("{} Timed out", cmdstr);
                    return Ok(());
                }
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

                            for j in (i - w)..i {
                                printchar(val[j]);
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

            if !err.is_ok() && subargs.errors {
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
    mode: impl Fn() -> VOutMode,
    command: &dyn pmbus::Command,
    payload: &Vec<u8>,
    writes: &Vec<(Bitpos, Replacement)>,
) -> Result<Vec<u8>> {
    let name = command.name();
    let mut rval = payload.clone();
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

    if !err.is_ok() {
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
        } else {
            if bitfields {
                bail!("{} has bitfields which must be set explicitly", cmd);
            } else {
                bail!("can't write to {}: data has unknown type", cmd);
            }
        }
    }
}

fn split_write(write: &str) -> Result<(&str, Option<&str>, &str)> {
    let expr: Vec<&str> = write.split('=').collect();

    if expr.len() < 2 {
        bail!("write \"{}\" needs a value, e.g. COMMAND=value", write);
    }

    if expr.len() > 2 {
        bail!("write \"{}\" has an ambiguous value", write);
    }

    let field: Vec<&str> = expr[0].split('.').collect();

    if field.len() > 1 {
        if field.len() > 2 {
            bail!("write \"{}\" has too many field delimiters", write);
        }

        Ok((field[0], Some(field[1]), expr[1]))
    } else {
        Ok((expr[0], None, expr[1]))
    }
}

fn pmbus(
    hubris: &mut HubrisArchive,
    core: &mut dyn Core,
    _args: &Args,
    subargs: &Vec<String>,
) -> Result<()> {
    let subargs = PmbusArgs::from_iter_safe(subargs)?;

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

    let mut port = None;

    if let Some(ref portarg) = subargs.port {
        let p = hubris
            .lookup_enum(func.args[1])
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

    let device = if let Some(driver) = &subargs.driver {
        match pmbus::Device::from_str(driver) {
            Some(device) => device,
            None => {
                bail!("unknown device \"{}\"", driver);
            }
        }
    } else {
        pmbus::Device::Common
    };

    let mut ops = vec![];
    let mut cmds = vec![];

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

    ops.push(Op::Push(subargs.device));

    let mut run = [true; 256];
    let (all, bycode) = all_commands(device);

    if let Some(ref commands) = subargs.commands {
        for i in 0..run.len() {
            run[i] = false;
        }

        for cmd in commands {
            if let Some(code) = all.get(cmd) {
                run[*code as usize] = true;
            } else {
                if let Ok(code) = parse_int::parse::<u8>(cmd) {
                    run[code as usize] = true;
                } else {
                    bail!("unrecognized PMBus command {}", cmd);
                }
            }
        }
    }

    let mut writes = MultiMap::new();
    let mut write_ops = vec![];

    if let Some(ref writecmds) = subargs.writes {
        for i in 0..run.len() {
            run[i] = false;
        }

        for write in writecmds {
            let (cmd, field, value) = split_write(write)?;

            match all.get(cmd) {
                Some(code) => {
                    run[*code as usize] = true;

                    writes.insert(
                        *code,
                        validate_write(device, cmd, *code, field, value)?,
                    );
                }
                None => {
                    bail!("unrecognized PMBus command {}", cmd);
                }
            }
        }
    }

    if writes.len() > 0 {
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

    if subargs.dryrun {
        return Ok(());
    }

    if cmds.len() == 0 {
        bail!("no command to run");
    }

    let mndx = if cmds[0] == vout { Some(0) } else { None };

    ops.push(Op::Done);

    context.execute(core, ops.as_slice())?;

    loop {
        if context.done(core)? {
            break;
        }

        thread::sleep(Duration::from_millis(100));
    }

    let results = context.results(core)?;

    let (mode, ndx) = match mndx {
        Some(mndx) => {
            let mode = match results[mndx] {
                Err(code) => {
                    bail!("can't read VOUT_MODE: {}", func.strerror(code));
                }
                Ok(ref val) => VOUT_MODE::CommandData::from_slice(val).unwrap(),
            };

            (Some(mode), 1)
        }
        None => (None, 0),
    };

    let getmode = || match mode {
        Some(mode) => mode,
        None => {
            panic!("unexpected call to get VOutMode");
        }
    };

    if writes.len() > 0 {
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

                for i in 0..v.len() {
                    write_ops.push(Op::Push(v[i]));
                }

                write_ops.push(Op::Push(v.len() as u8));
                write_ops.push(Op::Call(write_func.id));
                write_ops.push(Op::DropN(v.len() as u8 + 2));
            }
        }

        write_ops.push(Op::Done);

        context.execute(core, write_ops.as_slice())?;

        loop {
            if context.done(core)? {
                break;
            }

            thread::sleep(Duration::from_millis(100));
        }

        let wresults = context.results(core)?;

        for i in 0..wresults.len() {
            let name = bycode.get(&cmds[i + ndx]).unwrap();

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
