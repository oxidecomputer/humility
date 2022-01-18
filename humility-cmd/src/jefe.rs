// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::{bail, Context, Result};
use humility::core::Core;
use humility::hubris::*;
use std::num::NonZeroU32;
use std::thread;
use std::time::Duration;
use std::time::Instant;

#[derive(Debug)]
pub struct JefeVariables<'a> {
    #[allow(unused)]
    hubris: &'a HubrisArchive,
    ready: &'a HubrisVariable,
    kick: &'a HubrisVariable,
    request: &'a HubrisVariable,
    requests: &'a HubrisVariable,
    errors: &'a HubrisVariable,
    task: &'a HubrisVariable,
    cached: Option<(u32, u32)>,
    kicked: Option<Instant>,
    #[allow(unused)]
    timeout: u32,
    #[allow(unused)]
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

pub fn send_request(
    hubris: &HubrisArchive,
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
