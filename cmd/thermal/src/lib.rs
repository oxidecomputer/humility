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
#[structopt(name = "thermal", about = env!("CARGO_PKG_DESCRIPTION"))]
struct ThermalArgs {
    /// sets timeout
    #[structopt(
        long, short = "T", default_value = "5000", value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// summarize all sensors
    #[structopt(long, short)]
    summarize: bool,
}

fn thermal_summarize(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
) -> Result<()> {
    let mut ops = vec![];
    let funcs = context.functions()?;
    let op = idol::IdolOperation::new(hubris, "Thermal", "read_sensor", None)?;

    let ok = hubris.lookup_basetype(op.ok)?;

    if ok.encoding != HubrisEncoding::Float {
        bail!("expected return value of read_sensor() to be a float");
    }

    if ok.size != 4 {
        bail!("expected return value of read_sensor() to be an f32");
    }

    for i in 0..7 {
        let payload =
            op.payload(&[("index", idol::IdolArgument::Scalar(i))])?;
        context.idol_call_ops(&funcs, &op, &payload, &mut ops)?;
    }

    ops.push(Op::Done);

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

fn thermal(
    hubris: &mut HubrisArchive,
    core: &mut dyn Core,
    _args: &Args,
    subargs: &[String],
) -> Result<()> {
    let subargs = ThermalArgs::from_iter_safe(subargs)?;

    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;

    if subargs.summarize {
        thermal_summarize(hubris, core, &mut context)?;
        return Ok(());
    }

    Ok(())
}

pub fn init<'a, 'b>() -> (Command, App<'a, 'b>) {
    (
        Command::Attached {
            name: "thermal",
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
            run: thermal,
        },
        ThermalArgs::clap(),
    )
}
