/*
 * Copyright 2020 Oxide Computer Company
 */

use crate::cmd::{Archive, Attach, Validate};
use crate::core::Core;
use crate::hiffy::*;
use crate::hubris::*;
use crate::Args;
use std::convert::TryFrom;
use std::thread;

use anyhow::{anyhow, bail, Context, Result};
use hif::*;
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

    /// specifies a PMBus driver
    #[structopt(long, short = "D")]
    driver: Option<String>,

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

fn pmbus(
    hubris: &mut HubrisArchive,
    core: &mut dyn Core,
    _args: &Args,
    subargs: &Vec<String>,
) -> Result<()> {
    let subargs = PmbusArgs::from_iter_safe(subargs)?;

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

    ops.push(Op::Push(subargs.device));

    for j in 0..=255u8 {
        let i = j as u8;
        println!("i is {}", i);
        pmbus::command(device, i, |cmd| {
            let op = match cmd.read_op() {
                pmbus::Operation::ReadByte => Op::Push(1),
                pmbus::Operation::ReadWord => Op::Push(2),
                pmbus::Operation::ReadBlock => Op::PushNone,
                _ => {
                    return;
                }
            };

            ops.push(Op::Push(i));
            ops.push(op);
            ops.push(Op::Drop);
            ops.push(Op::Drop);
            cmds.push(i);
        });
    }

    ops.push(Op::Done);

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
