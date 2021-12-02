// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::{anyhow, bail, Result};
use humility::core::Core;
use humility::hubris::*;
use humility_cmd::jefe::{send_request, JefeRequest};
use humility_cmd::{Archive, Args, Attach, Command, Validate};
use std::num::NonZeroU32;
use structopt::clap::App;
use structopt::StructOpt;

#[macro_use]
extern crate log;

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

    let request = if subargs.fault {
        JefeRequest::Fault
    } else if subargs.start {
        JefeRequest::Start
    } else if subargs.hold {
        JefeRequest::Hold
    } else if subargs.release {
        JefeRequest::Release
    } else {
        bail!("one of fault, start, hold, or release must be specified");
    };

    let task = hubris
        .lookup_task(&subargs.task)
        .ok_or_else(|| anyhow!("couldn't find task {}", subargs.task))?;

    let id = match task {
        HubrisTask::Kernel => {
            bail!("cannot change disposition of kernel");
        }
        HubrisTask::Task(id) => {
            if let Some(id) = NonZeroU32::new(*id) {
                id
            } else {
                bail!("cannot change disposition of supervisor task");
            }
        }
    };

    send_request(hubris, core, request, id, subargs.timeout)?;

    info!("successfully changed disposition for {}", subargs.task);

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
