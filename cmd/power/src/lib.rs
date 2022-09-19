// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility power`
//!
//! `humility power` displays the values associated with devices that
//! can measure voltage, displaying voltage, current (if measured) and
//! temperature (if measured).

use anyhow::{bail, Result};
use clap::{CommandFactory, Parser};
use hif::*;
use humility::cli::Subcommand;
use humility::hubris::*;
use humility_cmd::idol::{self, HubrisIdol};
use humility_cmd::{hiffy::*, CommandKind};
use humility_cmd::{Archive, Attach, Command, Validate};
use std::collections::BTreeMap;

#[derive(Parser, Debug)]
#[clap(name = "power", about = env!("CARGO_PKG_DESCRIPTION"))]
struct PowerArgs {
    /// sets timeout
    #[clap(
        long, short = 'T', default_value_t = 5000, value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,
}

struct Device<'a> {
    name: &'a str,
    voltage: Option<usize>,
    current: Option<usize>,
    input_voltage: Option<usize>,
    input_current: Option<usize>,
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
    let op = hubris.get_idol_command("Sensor.get")?;

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
            let key = (&s.name, s.device.clone());
            if devices
                .insert(
                    key,
                    Device {
                        name: &s.name,
                        voltage: None,
                        current: None,
                        input_voltage: None,
                        input_current: None,
                        temperature: None,
                    },
                )
                .is_some()
            {
                bail!("Duplicate voltage sensor: {s:?}");
            }
        }
    }

    let mut ndx = 0;

    for (i, s) in hubris.manifest.sensors.iter().enumerate() {
        if let Some(device) = devices.get_mut(&(&s.name, s.device.clone())) {
            match s.kind {
                HubrisSensorKind::Current => {
                    device.current = Some(ndx);
                }
                HubrisSensorKind::Voltage => {
                    device.voltage = Some(ndx);
                }
                HubrisSensorKind::InputCurrent => {
                    device.input_current = Some(ndx);
                }
                HubrisSensorKind::InputVoltage => {
                    device.input_voltage = Some(ndx);
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

    println!(
        "{:25} {:>8} {:>8} {:>8} {:>8} {:>8}",
        "RAIL", "VOUT", "IOUT", "VIN", "IIN", "TEMP"
    );

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
        p(d.input_voltage);
        p(d.input_current);
        p(d.temperature);

        println!();
    }

    Ok(())
}

pub fn init() -> Command {
    Command {
        app: PowerArgs::command(),
        name: "power",
        run: power,
        kind: CommandKind::Attached {
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
        },
    }
}
