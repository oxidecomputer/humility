// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::{anyhow, bail, Context, Result};
use hif::*;
use humility::core::Core;
use humility::hubris::*;
use postcard::{take_from_bytes, to_slice};
use std::collections::HashMap;
use std::convert::TryFrom;
use std::time::Instant;

#[derive(Debug, PartialEq)]
enum State {
    Initialized,
    Kicked,
    ResultsReady,
    ResultsConsumed,
}

#[derive(Debug)]
pub struct HiffyContext<'a> {
    hubris: &'a HubrisArchive,
    ready: &'a HubrisVariable,
    kick: &'a HubrisVariable,
    text: &'a HubrisVariable,
    data: &'a HubrisVariable,
    rstack: &'a HubrisVariable,
    requests: &'a HubrisVariable,
    errors: &'a HubrisVariable,
    failure: &'a HubrisVariable,
    functions: HubrisGoff,
    cached: Option<(u32, u32)>,
    kicked: Option<Instant>,
    timeout: u32,
    state: State,
}

#[derive(Debug)]
pub struct HiffyFunction {
    pub id: TargetFunction,
    pub name: String,
    pub args: Vec<HubrisGoff>,
    pub errmap: HashMap<u32, String>,
}

impl HiffyFunction {
    pub fn strerror(&self, code: u32) -> String {
        match self.errmap.get(&code) {
            Some(name) => name.clone(),
            None => format!("<Unknown {} error: {}>", self.name, code),
        }
    }

    pub fn argument_variants(
        &self,
        hubris: &HubrisArchive,
        ndx: usize,
    ) -> Result<Vec<(String, u16)>> {
        let arg = hubris
            .lookup_enum(self.args[ndx])
            .context(format!("expected enum for arg #{}", ndx))?;

        let mut variants = vec![];

        for v in &arg.variants {
            let tag = v.tag.ok_or_else(|| {
                anyhow!("{}: malformed variant in arg #{}", ndx, self.args[ndx])
            })?;

            variants.push((v.name.to_string(), u16::try_from(tag)?));
        }

        Ok(variants)
    }

    pub fn lookup_argument(
        &self,
        hubris: &HubrisArchive,
        what: &str,
        ndx: usize,
        name: &str,
    ) -> Result<u16> {
        let arg = hubris
            .lookup_enum(self.args[ndx])
            .context(format!("expected enum for {}", what))?;

        for v in &arg.variants {
            let tag = v.tag.ok_or_else(|| {
                anyhow!("{}: malformed variant in {}", what, self.args[ndx])
            })?;

            if v.name == name {
                return Ok(u16::try_from(tag)?);
            }
        }

        let mut vals: Vec<String> = vec![];

        for variant in &arg.variants {
            vals.push(variant.name.to_string());
        }

        let list = vals.join(", ");
        bail!("invalid {} \"{}\" (must be one of: {})", what, name, list);
    }
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

    fn read_word(
        hubris: &'a HubrisArchive,
        core: &mut dyn Core,
        name: &str,
    ) -> Result<u32> {
        let v = Self::variable(hubris, name, true)?;

        let result = core
            .read_word_32(v.addr)
            .context(format!("couldn't read {}", name))?;

        Ok(result)
    }

    fn definition(hubris: &'a HubrisArchive, name: &str) -> Result<HubrisGoff> {
        let goff = hubris
            .lookup_definition(name)
            .context("expected hiffy definition not found")?;

        Ok(*goff)
    }

    pub fn new(
        hubris: &'a HubrisArchive,
        core: &mut dyn Core,
        timeout: u32,
    ) -> Result<HiffyContext<'a>> {
        core.halt()?;

        let (major, minor) = (
            Self::read_word(hubris, core, "HIFFY_VERSION_MAJOR"),
            Self::read_word(hubris, core, "HIFFY_VERSION_MINOR"),
        );

        core.run()?;

        let target = (major?, minor?);
        let ours = (HIF_VERSION_MAJOR, HIF_VERSION_MINOR);

        //
        // For now, we insist on an exact version match between Humility
        // and Hubris.
        //
        if ours != target {
            #[rustfmt::skip]
            bail!(
                "HIF version mismatch: target has {}.{}; ours is {}.{}",
                target.0, target.1, ours.0, ours.1
            );
        }

        Ok(Self {
            hubris,
            ready: Self::variable(hubris, "HIFFY_READY", true)?,
            kick: Self::variable(hubris, "HIFFY_KICK", true)?,
            text: Self::variable(hubris, "HIFFY_TEXT", false)?,
            data: Self::variable(hubris, "HIFFY_DATA", false)?,
            rstack: Self::variable(hubris, "HIFFY_RSTACK", false)?,
            requests: Self::variable(hubris, "HIFFY_REQUESTS", true)?,
            errors: Self::variable(hubris, "HIFFY_ERRORS", true)?,
            failure: Self::variable(hubris, "HIFFY_FAILURE", false)?,
            functions: Self::definition(hubris, "HIFFY_FUNCTIONS")?,
            cached: None,
            kicked: None,
            timeout,
            state: State::Initialized,
        })
    }

    pub fn data_size(&self) -> usize {
        self.data.size
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
            //
            // We expect every function to have a tag (unless there is only
            // one function present in which case we know that the index is 0).
            //
            let tag = match f.tag {
                Some(tag) => tag,
                None if functions.variants.len() == 1 => 0,
                _ => {
                    bail!("function {} in {}: missing tag", f.name, goff);
                }
            };

            let goff = f.goff.ok_or_else(|| {
                anyhow!("function {} in {}: missing a type", f.name, goff)
            })?;

            let mut func = HiffyFunction {
                id: TargetFunction(u8::try_from(tag)?),
                name: f.name.to_string(),
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
                //
                // This isn't a structure argument; if it's not an empty
                // tuple (denoting no argument), push our single argument.
                //
                match hubris.lookup_basetype(args) {
                    Ok(basetype) if basetype.size == 0 => {}
                    _ => {
                        func.args.push(args);
                    }
                }
            }

            let err = sig.lookup_member("__1")?.goff;

            //
            // We expect our error type to be 4-byte base type or an enum.
            //
            if let Ok(err) = hubris.lookup_enum(err) {
                for e in &err.variants {
                    let tag = e.tag.ok_or_else(|| {
                        anyhow!(
                            "function {}: malformed error type {}",
                            f.name,
                            err.goff,
                        )
                    })?;

                    let val = u32::try_from(tag)?;
                    func.errmap.insert(val, e.name.to_string());
                }
            }

            rval.insert(func.name.clone(), func);
        }

        Ok(rval)
    }

    pub fn execute(
        &mut self,
        core: &mut dyn Core,
        ops: &[Op],
        data: Option<&[u8]>,
    ) -> Result<()> {
        match self.state {
            State::Initialized | State::ResultsConsumed => {}
            _ => {
                bail!("invalid state for execution: {:?}", self.state);
            }
        }

        if let Some(data) = data {
            if data.len() > self.data.size {
                bail!(
                    "data size ({}) exceeds maximum data size ({})",
                    data.len(),
                    self.data.size
                );
            }
        }

        let mut text: Vec<u8> = vec![];
        text.resize_with(self.text.size, Default::default);

        core.halt()?;

        if core.read_word_32(self.ready.addr)? != 1 {
            core.run()?;
            bail!("HIF execution facility unavailable");
        }

        let buf = &mut text.as_mut_slice();
        let mut current = 0;

        for op in ops {
            let serialized = to_slice(op, &mut buf[current..])?;
            current += serialized.len();
        }

        core.write_8(self.text.addr, &buf[0..])?;

        if let Some(data) = data {
            core.write_8(self.data.addr, data)?;
        }

        core.write_word_32(self.kick.addr, 1)?;

        self.cached = Some((
            core.read_word_32(self.requests.addr)?,
            core.read_word_32(self.errors.addr)?,
        ));

        self.kicked = Some(Instant::now());

        self.state = State::Kicked;

        core.run()?;

        Ok(())
    }

    pub fn done(&mut self, core: &mut dyn Core) -> Result<bool> {
        if self.state != State::Kicked {
            bail!("invalid state for waiting: {:?}", self.state);
        }

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
                self.state = State::ResultsReady;
                Ok(true)
            } else if vars.1 != cached.1 {
                //
                // If we have died because our HIF failed fatally (which we
                // only expect due to programmer error), we want to print
                // HIFFY_FAILURE to provide some additional context.
                //
                let mut buf: Vec<u8> = vec![];
                buf.resize_with(self.failure.size, Default::default);

                core.halt()?;
                let r = core.read_8(self.failure.addr, buf.as_mut_slice());
                core.run()?;

                match r {
                    Ok(_) => {
                        let fmt = HubrisPrintFormat {
                            hex: true,
                            ..HubrisPrintFormat::default()
                        };

                        let hubris = self.hubris;
                        let f =
                            hubris.printfmt(&buf, self.failure.goff, &fmt)?;
                        bail!("request failed: {}", f);
                    }
                    _ => {
                        bail!("request failed");
                    }
                }
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
        if self.state != State::ResultsReady {
            bail!("invalid state for consuming results: {:?}", self.state);
        }

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

                FunctionResult::Success(payload) => {
                    rvec.push(Ok(payload.to_vec()))
                }

                FunctionResult::Failure(code) => rvec.push(Err(code)),
            }

            result = next;
        }

        self.state = State::ResultsConsumed;

        Ok(rvec)
    }
}
