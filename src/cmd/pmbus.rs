/*
 * Copyright 2020 Oxide Computer Company
 */

use crate::cmd::i2c::*;
use crate::cmd::{Archive, Attach, Validate};
use crate::core::Core;
use crate::hubris::*;
use crate::Args;

use anyhow::Result;
use std::time::Instant;
use structopt::clap::App;
use structopt::StructOpt;

use num_traits::FromPrimitive;

#[derive(StructOpt, Debug)]
#[structopt(name = "pmbus", about = "scan for and read PMBus devices")]
struct PmbusArgs {
    /// verbose output
    #[structopt(long, short)]
    verbose: bool,

    /// scan a device
    #[structopt(long, short, conflicts_with = "register")]
    scan: bool,

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
    #[structopt(long, short, value_name = "address",
        parse(try_from_str = parse_int::parse),
    )]
    device: u8,
}

fn pmbus_read(
    core: &mut dyn Core,
    subargs: &PmbusArgs,
    i2c: &mut I2cArgs,
    vars: &mut I2cVariables,
    command: pmbus::Command,
) -> Result<()> {
    let (nbytes, block) = match command.read_op() {
        pmbus::Operation::ReadByte => (Some(1), false),
        pmbus::Operation::ReadWord => (Some(2), false),
        pmbus::Operation::ReadBlock => (None, true),
        _ => {
            return Ok(());
        }
    };

    i2c.set_nbytes(nbytes);
    i2c.set_block(block);
    i2c.set_register(command as u8);

    let mut ts = vec![];
    ts.push(Instant::now());

    vars.kickit(core, &i2c)?;
    ts.push(Instant::now());

    loop {
        if vars.done(core)? {
            break;
        }
    }

    ts.push(Instant::now());

    let results = vars.results(core, nbytes)?;
    ts.push(Instant::now());

    let name = format!("{:?}", command);
    let cmdstr = format!("0x{:02x} {:<25}", command as u8, name);

    match results[0] {
        Some(Err(err)) => {
            if subargs.verbose {
                println!("{} Err({})", cmdstr, err.name);
            }
        }

        None => {
            if subargs.verbose {
                println!("{} Timed out", cmdstr);
            }
        }

        Some(Ok(val)) => match nbytes {
            Some(1) => {
                println!("{} 0x{:02x}", cmdstr, val);
            }

            Some(2) => {
                if results.len() > 1 {
                    if let Some(Ok(msb)) = results[1] {
                        let word = ((msb as u16) << 8) | (val as u16);
                        println!("{} 0x{:04x}", cmdstr, word);
                    }
                } else {
                    println!("{} Short: {:?}", cmdstr, results);
                }
            }
            None => {
                print!("{}", cmdstr);
                for i in 0..results.len() {
                    if let Some(Ok(val)) = results[i] {
                        print!(" 0x{:02x}", val);
                    } else {
                        break;
                    }
                }
                println!();
            }
            _ => {
                unreachable!();
            }
        },
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

    let mux = match subargs.mux {
        Some(ref str) => Some(str.clone()),
        None => None,
    };

    let port = match subargs.port {
        Some(ref str) => Some(str.clone()),
        None => None,
    };

    let mut i2c =
        I2cArgs::from_caller(subargs.controller, subargs.device, port, mux);

    let mut vars = I2cVariables::new(hubris, &i2c)?;

    for i in 0..=255 {
        match pmbus::Command::from_u8(i) {
            Some(cmd) => {
                pmbus_read(core, &subargs, &mut i2c, &mut vars, cmd)?;
            }
            None => {}
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
