/*
 * Copyright 2021 Oxide Computer Company
 */

use crate::core::Core;
use crate::hubris::*;
use anyhow::{anyhow, bail, Context, Result};
use hif::*;
use postcard::{take_from_bytes, to_slice};
use std::collections::HashMap;
use std::convert::TryFrom;
use std::time::Instant;

#[derive(Debug)]
pub struct HiffyContext<'a> {
    hubris: &'a HubrisArchive,
    ready: &'a HubrisVariable,
    kick: &'a HubrisVariable,
    text: &'a HubrisVariable,
    rstack: &'a HubrisVariable,
    requests: &'a HubrisVariable,
    errors: &'a HubrisVariable,
    functions: HubrisGoff,
    cached: Option<(u32, u32)>,
    kicked: Option<Instant>,
    timeout: u32,
    timedout: bool,
}

#[derive(Debug)]
pub struct HiffyFunction {
    pub id: TargetFunction,
    pub args: Vec<HubrisGoff>,
    pub errmap: HashMap<u32, String>,
}

impl<'a> HiffyContext<'a> {
    fn variable(
        hubris: &'a HubrisArchive,
        name: &str,
        wordsize: bool,
    ) -> Result<&'a HubrisVariable> {
        let v = hubris
            .lookup_variable(name)
            .context("expected hiffy interface not found")?;

        if wordsize && v.size != 4 {
            bail!("expected {} to be size 4, found {}", name, v.size);
        }

        Ok(v)
    }

    fn definition(hubris: &'a HubrisArchive, name: &str) -> Result<HubrisGoff> {
        let goff = hubris
            .lookup_definition(name)
            .context("expected hiffy definition not found")?;

        Ok(*goff)
    }

    pub fn new(
        hubris: &'a HubrisArchive,
        timeout: u32,
    ) -> Result<HiffyContext> {
        Ok(Self {
            hubris: hubris,
            ready: Self::variable(hubris, "HIFFY_READY", true)?,
            kick: Self::variable(hubris, "HIFFY_KICK", true)?,
            text: Self::variable(hubris, "HIFFY_TEXT", false)?,
            rstack: Self::variable(hubris, "HIFFY_RSTACK", false)?,
            requests: Self::variable(hubris, "HIFFY_REQUESTS", true)?,
            errors: Self::variable(hubris, "HIFFY_ERRORS", true)?,
            functions: Self::definition(hubris, "HIFFY_FUNCTIONS")?,
            cached: None,
            kicked: None,
            timeout: timeout,
            timedout: false,
        })
    }

    pub fn functions(&mut self) -> Result<HashMap<String, HiffyFunction>> {
        let hubris = self.hubris;

        let goff = hubris
            .lookup_enum(self.functions)?
            .lookup_variant_byname("Some")?
            .goff
            .ok_or_else(|| anyhow!("malconstructed functions"))?;

        let ptr = hubris.lookup_struct(goff)?.lookup_member("__0")?.goff;
        let goff = hubris.lookup_ptrtype(ptr)?;
        let functions = hubris.lookup_enum(goff)?;
        let mut rval = HashMap::new();

        for f in &functions.variants {
            let tag = f.tag.ok_or_else(|| {
                anyhow!("function {}: missing an identifier", f.name)
            })?;

            let goff = f.goff.ok_or_else(|| {
                anyhow!("function {}: missing a type", f.name)
            })?;

            let mut func = HiffyFunction {
                id: TargetFunction(u8::try_from(tag)?),
                args: Vec::new(),
                errmap: HashMap::new(),
            };

            //
            // We expect a 2-tuple that is our arguments and our error type
            //
            let sig = hubris.lookup_struct(goff)?;
            let args = sig.lookup_member("__0")?.goff;

            if let Ok(args) = hubris.lookup_struct(args) {
                for arg in &args.members {
                    func.args.push(arg.goff);
                }
            } else {
                func.args.push(args);
            }

            let err = sig.lookup_member("__1")?.goff;

            //
            // We expect our error type to be 4-byte base type or an enum.
            //
            if let Ok(err) = hubris.lookup_enum(err) {
                for e in &err.variants {
                    let tag = e.tag.ok_or_else(|| {
                        anyhow!("function {}: malformed errors", f.name)
                    })?;

                    let val = u32::try_from(tag)?;
                    func.errmap.insert(val, e.name.to_string());
                }
            }

            rval.insert(f.name.to_string(), func);
        }

        Ok(rval)
    }

    pub fn execute(&mut self, core: &mut dyn Core, ops: &[Op]) -> Result<()> {
        let mut text: Vec<u8> = vec![];
        text.resize_with(self.text.size, Default::default);

        if core.read_word_32(self.ready.addr)? != 1 {
            bail!("HIF execution facility unavailable");
        }

        let buf = &mut text.as_mut_slice();
        let mut current = 0;

        for op in ops {
            let serialized = to_slice(op, &mut buf[current..])?;
            current += serialized.len();
        }

        core.halt()?;
        core.write_8(self.text.addr, &buf[0..])?;
        core.write_word_32(self.kick.addr, 1)?;

        self.cached = Some((
            core.read_word_32(self.requests.addr)?,
            core.read_word_32(self.errors.addr)?,
        ));

        self.kicked = Some(Instant::now());

        core.run()?;

        Ok(())
    }

    pub fn done(&mut self, core: &mut dyn Core) -> Result<bool> {
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

    pub fn results(
        &mut self,
        core: &mut dyn Core,
    ) -> Result<Vec<Result<Vec<u8>, u32>>> {
        let mut rstack: Vec<u8> = vec![];
        rstack.resize_with(self.rstack.size, Default::default);

        core.halt()?;

        let mut rvec = vec![];
        core.read_8(self.rstack.addr, rstack.as_mut_slice())?;

        core.run()?;

        let mut result = &rstack[0..];

        loop {
            let (rval, next) = take_from_bytes::<FunctionResult>(result)?;

            match rval {
                FunctionResult::Done => {
                    break;
                }

                FunctionResult::Success(ref payload) => {
                    rvec.push(Ok(payload.to_vec()))
                }

                FunctionResult::Failure(code) => rvec.push(Err(code)),
            }

            result = next;
        }

        Ok(rvec)
    }
}
