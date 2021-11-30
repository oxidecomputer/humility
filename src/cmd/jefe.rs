// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::cmd::*;
use crate::core::Core;
use crate::hubris::*;
use crate::Args;
use anyhow::{anyhow, bail, Context, Result};
use std::num::NonZeroU32;
use std::thread;
use std::time::Duration;
use std::time::Instant;
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

#[derive(Debug)]
#[allow(dead_code)]
pub struct JefeVariables<'a> {
    hubris: &'a HubrisArchive,
    ready: &'a HubrisVariable,
    kick: &'a HubrisVariable,
    request: &'a HubrisVariable,
    requests: &'a HubrisVariable,
    errors: &'a HubrisVariable,
    task: &'a HubrisVariable,
    cached: Option<(u32, u32)>,
    kicked: Option<Instant>,
    timeout: u32,
    timedout: bool,
}

#[derive(Copy, Clone, Debug)]
#[repr(u32)]
pub enum JefeRequest {
    Start = 1,
    Hold = 2,
    Release = 3,
    Fault = 4,
}

impl<'a> JefeVariables<'a> {
    fn variable(
        hubris: &'a HubrisArchive,
        name: &str,
        wordsize: bool,
    ) -> Result<&'a HubrisVariable> {
        let v = hubris
            .lookup_variable(name)
            .context("expected jefe external interface not found")?;

        if wordsize && v.size != 4 {
            bail!("expected {} to be size 4, found {}", name, v.size);
        }

        Ok(v)
    }

    pub fn new(
        hubris: &'a HubrisArchive,
        timeout: u32,
    ) -> Result<JefeVariables> {
        Ok(Self {
            hubris,
            ready: Self::variable(hubris, "JEFE_EXTERNAL_READY", true)?,
            kick: Self::variable(hubris, "JEFE_EXTERNAL_KICK", true)?,
            request: Self::variable(hubris, "JEFE_EXTERNAL_REQUEST", true)?,
            requests: Self::variable(hubris, "JEFE_EXTERNAL_REQUESTS", true)?,
            errors: Self::variable(hubris, "JEFE_EXTERNAL_ERRORS", true)?,
            task: Self::variable(hubris, "JEFE_EXTERNAL_TASKINDEX", true)?,
            cached: None,
            kicked: None,
            timeout,
            timedout: false,
        })
    }

    fn kickit(
        &mut self,
        core: &mut dyn Core,
        request: JefeRequest,
        taskid: NonZeroU32,
    ) -> Result<()> {
        if core.read_word_32(self.ready.addr)? != 1 {
            bail!("jefe external control facility unavailable");
        }

        core.halt()?;

        core.write_word_32(self.request.addr, request as u32)?;
        core.write_word_32(self.task.addr, taskid.into())?;

        core.write_word_32(self.kick.addr, 1)?;

        self.cached = Some((
            core.read_word_32(self.requests.addr)?,
            core.read_word_32(self.errors.addr)?,
        ));

        self.kicked = Some(Instant::now());

        core.run()?;
        Ok(())
    }

    fn done(&mut self, core: &mut dyn Core) -> Result<bool> {
        core.halt()?;

        let vars = (
            core.read_word_32(self.requests.addr)?,
            core.read_word_32(self.errors.addr)?,
        );

        core.run()?;

        if let Some(kicked) = self.kicked {
            if kicked.elapsed().as_millis() > self.timeout.into() {
                bail!("operation timed out");
            }
        }

        if let Some(cached) = self.cached {
            if vars.0 != cached.0 {
                Ok(true)
            } else if vars.1 != cached.1 {
                bail!("request failed");
            } else {
                Ok(false)
            }
        } else {
            Ok(false)
        }
    }
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

pub fn send_request(
    hubris: &mut HubrisArchive,
    core: &mut dyn Core,
    request: JefeRequest,
    id: NonZeroU32,
    timeout: u32,
) -> Result<()> {
    let mut vars = JefeVariables::new(hubris, timeout)?;

    vars.kickit(core, request, id)?;

    loop {
        thread::sleep(Duration::from_millis(100));
        if vars.done(core)? {
            break;
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
