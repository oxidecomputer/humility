// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::{bail, Result};
use hif::*;
use humility::core::Core;
use humility::hubris::*;
use humility_cmd::hiffy::*;
use humility_cmd::idol;
use humility_cmd::{Archive, Args, Attach, Command, Validate};
use std::thread;
use std::time::Duration;
use structopt::clap::App;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(name = "sensor", about = env!("CARGO_PKG_DESCRIPTION"))]
struct SensorArgs {
    /// sets timeout
    #[structopt(
        long, short = "T", default_value = "5000", value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// list all sensors
    #[structopt(long, short)]
    list: bool,

    /// summarize all sensors
    #[structopt(long, short)]
    summarize: bool,
}

fn sensor_list(hubris: &mut HubrisArchive) -> Result<()> {
    println!(
        "{:2} {:<5} {:2} {:2} {} {} {:13} {}",
        "ID", "KIND", "C", "P", "MUX", "ADDR", "DEVICE", "DESCRIPTION"
    );

    for (ndx, s) in hubris.manifest.sensors.iter().enumerate() {
        let device = &hubris.manifest.i2c_devices[s.device];

        let mux = match (device.mux, device.segment) {
            (Some(m), Some(s)) => format!("{}:{}", m, s),
            (None, None) => "-".to_string(),
            (_, _) => "?:?".to_string(),
        };

        let kind = match s.kind {
            HubrisSensorKind::Temperature => "temp",
            HubrisSensorKind::Power => "power",
            HubrisSensorKind::Current => "crrnt",
            HubrisSensorKind::Voltage => "vltge",
            HubrisSensorKind::Speed => "speed",
        };

        println!(
            "{:2} {:5} {:2} {:2} {:3} 0x{:02x} {:13} {}",
            ndx,
            kind,
            device.controller,
            device.port.name,
            mux,
            device.address,
            device.device,
            device.description
        );
    }

    Ok(())
}

fn sensor_summarize(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
) -> Result<()> {
    let mut ops = vec![];
    let funcs = context.functions()?;
    let op = idol::IdolOperation::new(hubris, "Sensor", "get", None)?;

    let ok = hubris.lookup_basetype(op.ok)?;

    if ok.encoding != HubrisEncoding::Float {
        bail!("expected return value of read_sensor() to be a float");
    }

    if ok.size != 4 {
        bail!("expected return value of read_sensor() to be an f32");
    }

    if hubris.manifest.sensors.len() == 0 {
        bail!("no sensors found");
    }

    for i in 0..hubris.manifest.sensors.len() {
        let payload =
            op.payload(&[("id", idol::IdolArgument::Scalar(i as u64))])?;
        context.idol_call_ops(&funcs, &op, &payload, &mut ops)?;
    }

    ops.push(Op::Done);
    println!("{:#?}", ops);

    loop {
        let results = context.run(core, ops.as_slice(), None)?;

        let mut rval = vec![];

        for r in results {
            if let Ok(val) = r {
                rval.push(Some(f32::from_le_bytes(val[0..4].try_into()?)));
            } else {
                rval.push(None);
            }
        }

        for val in rval {
            if let Some(val) = val {
                print!(" {:>6.2}", val);
            } else {
                print!(" {:>6}", "-");
            }
        }

        println!();

        thread::sleep(Duration::from_millis(1000));
    }
}

fn sensor(
    hubris: &mut HubrisArchive,
    core: &mut dyn Core,
    _args: &Args,
    subargs: &[String],
) -> Result<()> {
    let subargs = SensorArgs::from_iter_safe(subargs)?;

    if subargs.list {
        sensor_list(hubris)?;
        return Ok(());
    }

    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;

    if subargs.summarize {
        sensor_summarize(hubris, core, &mut context)?;
        return Ok(());
    }

    Ok(())
}

pub fn init<'a, 'b>() -> (Command, App<'a, 'b>) {
    (
        Command::Attached {
            name: "sensor",
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
            run: sensor,
        },
        SensorArgs::clap(),
    )
}
