// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility power`
//!
//! `humility power` displays the values associated with devices that
//! can measure voltage, displaying voltage, current (if measured) and
//! temperature (if measured).

use anyhow::{bail, Context, Result};
use clap::Command as ClapCommand;
use clap::{CommandFactory, Parser};
use hif::*;
use humility::cli::Subcommand;
use humility::hubris::*;
use humility_cmd::hiffy::*;
use humility_cmd::idol;
use humility_cmd::{Archive, Attach, Command, Validate};
use std::collections::BTreeMap;

#[derive(Parser, Debug)]
#[clap(name = "power", about = env!("CARGO_PKG_DESCRIPTION"))]
struct PowerArgs {
    /// sets timeout
    #[clap(
        long, short = 'T', default_value = "5000", value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,
}

struct Device<'a> {
    name: &'a str,
    voltage: Option<usize>,
    current: Option<usize>,
    temperature: Option<usize>,
}

fn power(context: &mut humility::ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let hubris = context.archive.as_ref().unwrap();

    let subargs = PowerArgs::try_parse_from(subargs)?;

    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;
    let mut ops = vec![];
    let funcs = context.functions()?;
    let op = idol::IdolOperation::new(hubris, "Sensor", "get", None)
        .context("is the 'sensor' task present?")?;

    let ok = hubris.lookup_basetype(op.ok)?;

    if ok.encoding != HubrisEncoding::Float {
        bail!("expected return value of read_sensors() to be a float");
    }

    if ok.size != 4 {
        bail!("expected return value of read_sensors() to be an f32");
    }

    if hubris.manifest.sensors.is_empty() {
        bail!("no sensors found");
    }

    let mut devices = BTreeMap::new();

    //
    // First, take a pass looking for devices that can measure voltage.
    //
    for s in hubris.manifest.sensors.iter() {
        if s.kind == HubrisSensorKind::Voltage {
            devices.insert(
                (&s.name, s.device),
                Device {
                    name: &s.name,
                    voltage: None,
                    current: None,
                    temperature: None,
                },
            );
        }
    }

    let mut ndx = 0;

    for (i, s) in hubris.manifest.sensors.iter().enumerate() {
        if let Some(device) = devices.get_mut(&(&s.name, s.device)) {
            match s.kind {
                HubrisSensorKind::Current => {
                    device.current = Some(ndx);
                }
                HubrisSensorKind::Voltage => {
                    device.voltage = Some(ndx);
                }
                HubrisSensorKind::Temperature => {
                    device.temperature = Some(ndx);
                }
                _ => {
                    continue;
                }
            }

            let payload =
                op.payload(&[("id", idol::IdolArgument::Scalar(i as u64))])?;
            context.idol_call_ops(&funcs, &op, &payload, &mut ops)?;
            ndx += 1;
        }
    }

    ops.push(Op::Done);

    let results = context.run(core, ops.as_slice(), None)?;

    let mut rval = vec![];

    for r in &results {
        if let Ok(val) = r {
            rval.push(Some(f32::from_le_bytes(val[0..4].try_into()?)));
        } else {
            rval.push(None);
        }
    }

    println!("{:25} {:>8} {:>8} {:>8}", "RAIL", "VOUT", "IOUT", "TEMP");

    let p = |what| {
        print!(" ");

        match what {
            Some(ndx) => {
                if let Some(value) = rval[ndx] {
                    print!("{:>8.2}", value);
                } else {
                    print!("{:>8}", "-");
                }
            }
            None => {
                print!("{:8}", "");
            }
        }
    };

    for d in devices.values() {
        print!("{:25}", d.name);

        p(d.voltage);
        p(d.current);
        p(d.temperature);

        println!();
    }

    Ok(())
}

pub fn init() -> (Command, ClapCommand<'static>) {
    (
        Command::Attached {
            name: "power",
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
            run: power,
        },
        PowerArgs::command(),
    )
}
