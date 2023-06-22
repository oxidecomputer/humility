// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility power`
//!
//! `humility power` displays the values associated with power rails,
//! displaying output voltage, output current, input voltate, input current,
//! and temperature.  Not all measurements are available for all rails; if a
//! measurement is not provided for a given rail, "-" is printed.  To specify
//! which rails are displayed, use the `--rails` option.  If a rail can
//! provide a given measurement, but that measurement is unavailable (say, due
//! to being in a power state whereby the rail is not powered), "x" is
//! displayed.  Some rails can determine current by output phase; to display
//! these, use the `--phase-current` option.
//!

use anyhow::{bail, Result};
use clap::{CommandFactory, Parser};
use hif::*;
use humility::core::Core;
use humility::hubris::*;
use humility_cli::{ExecutionContext, Subcommand};
use humility_cmd::CommandKind;
use humility_cmd::{Archive, Attach, Command, Validate};
use humility_hiffy::*;
use humility_idol::{self as idol, HubrisIdol};
use std::collections::{BTreeMap, HashSet};

#[derive(Parser, Debug)]
#[clap(name = "power", about = env!("CARGO_PKG_DESCRIPTION"))]
struct PowerArgs {
    /// sets timeout
    #[clap(
        long, short = 'T', default_value_t = 5000, value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// only show values associated with the specified rail(s)
    #[clap(short, long, use_value_delimiter = true)]
    rails: Option<Vec<String>>,

    /// get phase current where available
    #[clap(long)]
    phase_current: bool,
}

struct Device<'a> {
    name: &'a str,
    rail: Option<usize>,
    voltage: Option<usize>,
    current: Option<usize>,
    input_voltage: Option<usize>,
    input_current: Option<usize>,
    temperature: Option<usize>,
    phases: Option<&'a Vec<u8>>,
    phase_currents: Option<Vec<Option<f32>>>,
}

fn phase_currents(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
    devices: &mut BTreeMap<(&String, HubrisSensorDevice), Device>,
) -> Result<()> {
    let op = hubris.get_idol_command("Power.phase_current")?;
    let ok = hubris.lookup_basetype(op.ok)?;
    let mut ops = vec![];

    if (ok.encoding, ok.size) != (HubrisEncoding::Float, 4) {
        bail!("expected return value of phase_current() to be a float");
    }

    for device in devices.values() {
        if let Some(phases) = device.phases {
            let id = match device.rail {
                Some(id) => id,
                None => {
                    bail!("{} does not have a voltage sensor?", device.name);
                }
            };

            for (phase, _) in phases.iter().enumerate() {
                let payload = op.payload(&[
                    ("rail", idol::IdolArgument::Scalar(id as u64)),
                    ("phase", idol::IdolArgument::Scalar(phase as u64)),
                ])?;
                context.idol_call_ops(&op, &payload, &mut ops)?;
            }
        }
    }

    ops.push(Op::Done);
    let results = context.run(core, ops.as_slice(), None)?;

    let mut ndx = 0;

    for device in devices.values_mut() {
        if let Some(phases) = device.phases {
            let mut phase_currents = vec![];

            for _ in phases.iter() {
                phase_currents.push(match &results[ndx] {
                    Ok(val) => Some(f32::from_le_bytes(val[0..4].try_into()?)),
                    _ => None,
                });

                ndx += 1;
            }

            device.phase_currents = Some(phase_currents);
        }
    }

    Ok(())
}

fn power(context: &mut ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let hubris = context.archive.as_ref().unwrap();

    let subargs = PowerArgs::try_parse_from(subargs)?;

    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;
    let mut ops = vec![];
    let op = hubris.get_idol_command("Sensor.get")?;

    let ok = hubris.lookup_basetype(op.ok)?;

    if (ok.encoding, ok.size) != (HubrisEncoding::Float, 4) {
        bail!("expected return value of Sensor.get() to be a float");
    }

    if hubris.manifest.sensors.is_empty() {
        bail!("no sensors found");
    }

    let mut devices = BTreeMap::new();

    let mut rails: Option<HashSet<String>> =
        subargs.rails.map(|r| HashSet::from_iter(r.iter().cloned()));

    //
    // First, take a pass looking for devices that can measure voltage.
    //
    for s in hubris.manifest.sensors.iter() {
        if s.kind == HubrisSensorKind::Voltage {
            let mut phases = None;

            if let Some(ref mut r) = rails {
                if !r.remove(&s.name) {
                    continue;
                }
            }

            if let HubrisSensorDevice::I2c(i) = &s.device {
                let d = &hubris.manifest.i2c_devices[*i];

                if let HubrisI2cDeviceClass::Pmbus { rails } = &d.class {
                    if let Some(r) = rails.iter().find(|&r| r.name == s.name) {
                        if let Some(p) = &r.phases {
                            phases = Some(p);
                        }
                    }
                }
            }

            let key = (&s.name, s.device.clone());
            if devices
                .insert(
                    key,
                    Device {
                        name: &s.name,
                        rail: None,
                        voltage: None,
                        current: None,
                        input_voltage: None,
                        input_current: None,
                        temperature: None,
                        phases,
                        phase_currents: None,
                    },
                )
                .is_some()
            {
                bail!("Duplicate voltage sensor: {s:?}");
            }
        }
    }

    if let Some(ref r) = rails {
        if !r.is_empty() {
            bail!("unknown rail(s): {r:?}");
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
                    device.rail = Some(i);
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
            context.idol_call_ops(&op, &payload, &mut ops)?;
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

    //
    // If we have been asked for phase currents, we'll get all of those now.
    //
    if subargs.phase_current {
        phase_currents(hubris, core, &mut context, &mut devices)?;
    }

    println!(
        "{:30} {:>8} {:>8} {:>8} {:>8} {:>8}",
        "RAIL", "VOUT", "IOUT", "VIN", "IIN", "TEMP"
    );

    let no = "-";
    let err = "x";

    let p = |what| {
        print!(" ");

        match what {
            Some(ndx) => {
                if let Some(value) = rval[ndx] {
                    print!("{value:>8.3}");
                } else {
                    print!("{err:>8}");
                }
            }
            None => {
                print!("{no:>8}");
            }
        }
    };

    for d in devices.values() {
        print!("{:30}", d.name);

        p(d.voltage);
        p(d.current);

        p(d.input_voltage);
        p(d.input_current);
        p(d.temperature);

        println!();

        if let Some(phase_currents) = &d.phase_currents {
            if phase_currents.len() > 1 {
                for (index, value) in phase_currents.iter().enumerate() {
                    let name = format!("{}.phase-{index}", d.name);

                    if let Some(value) = value {
                        print!("{name:30} {no:>8} {value:>8.3}");
                    } else {
                        print!("{name:30} {no:>8} {err:>8}");
                    }

                    for _ in 0..3 {
                        print!(" {no:>8}");
                    }

                    println!();
                }
            }
        }
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
