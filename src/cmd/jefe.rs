/*
 * Copyright 2021 Oxide Computer Company
 */

use crate::cmd::*;
use crate::core::Core;
use crate::hiffy::*;
use crate::hubris::*;
use crate::Args;
use anyhow::{anyhow, bail, Result};
use hif::*;
use std::thread;
use std::time::Duration;
use structopt::clap::App;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(name = "jefe", about = "influence jefe externally")]
struct JefeArgs {
    /// sets timeout
    #[structopt(
        long, short, default_value = "5000", value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// fault the specified task
    #[structopt(long, short, conflicts_with_all = &["start", "release", "hold"])]
    fault: bool,

    /// start the specified task
    #[structopt(long, short, conflicts_with_all = &["release", "hold"])]
    start: bool,

    /// hold the specified task
    #[structopt(long, short, conflicts_with = "release")]
    hold: bool,

    /// release the specified task
    #[structopt(long, short)]
    release: bool,

    task: String,
}

fn jefe(
    hubris: &mut HubrisArchive,
    core: &mut dyn Core,
    _args: &Args,
    subargs: &Vec<String>,
) -> Result<()> {
    let subargs = JefeArgs::from_iter_safe(subargs)?;

    let mut context = HiffyContext::new(hubris, subargs.timeout)?;
    let funcs = context.functions()?;
    let func = funcs
        .get("JefeSetDisposition")
        .ok_or_else(|| anyhow!("did not find JefeSetDisposition function"))?;

    let request = if subargs.fault {
        "Fault"
    } else if subargs.start {
        "Start"
    } else if subargs.hold {
        "Hold"
    } else if subargs.release {
        "Restart"
    } else {
        bail!("one of fault, start, hold, or release must be specified");
    };

    let val = func.lookup_argument(hubris, 1, request)?;

    let task = hubris
        .lookup_task(&subargs.task)
        .ok_or_else(|| anyhow!("couldn't find task {}", subargs.task))?;

    let id = match task {
        HubrisTask::Kernel | HubrisTask::Task(0) => {
            bail!("cannot change disposition of {}", subargs.task);
        }
        HubrisTask::Task(id) => *id,
    };

    let mut ops = vec![];

    ops.push(Op::Push16(id as u16));
    ops.push(Op::Push16(val));
    ops.push(Op::Call(func.id));
    ops.push(Op::Done);

    context.execute(core, ops.as_slice())?;

    loop {
        if context.done(core)? {
            break;
        }

        thread::sleep(Duration::from_millis(100));
    }

    let results = context.results(core)?;

    if results.len() == 0 {
        bail!("command timed out");
    }

    match results[0] {
        Ok(_) => {
            info!("successfully changed disposition for {}", subargs.task);
        }
        Err(err) => {
            bail!("command failed: {}", func.strerror(err))
        }
    }

    Ok(())
}

pub fn init<'a, 'b>() -> (Command, App<'a, 'b>) {
    (
        Command::Attached {
            name: "jefe",
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
            run: jefe,
        },
        JefeArgs::clap(),
    )
}
