// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::{
    doppel::StaticCell,
    idol,
    reflect::{self, Load, Value},
};
use anyhow::{anyhow, bail, Context, Result};
use hif::*;
use humility::core::Core;
use humility::hubris::*;
use postcard::{take_from_bytes, to_slice};
use std::collections::HashMap;
use std::convert::TryFrom;
use std::thread;
use std::time::{Duration, Instant};

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
    scratch: Option<&'a HubrisVariable>,
    scratch_size: usize,
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

/// Simple wrapper `struct` that exposes a checked `get(name, nargs)`
#[derive(Debug)]
pub struct HiffyFunctions(pub HashMap<String, HiffyFunction>);

impl HiffyFunctions {
    pub fn get(&self, name: &str, nargs: usize) -> Result<&HiffyFunction> {
        let f = self
            .0
            .get(name)
            .ok_or_else(|| anyhow!("did not find {} function", name))?;
        if f.args.len() != nargs {
            bail!(
                "mismatched function signature on {} \
                (target has {} args, but we expected {}); \
                missing Hubris/Humility flag day?",
                name,
                f.args.len(),
                nargs
            );
        }
        Ok(f)
    }
    pub fn len(&self) -> usize {
        self.0.len()
    }
    pub fn is_empty(&self) -> bool {
        self.0.len() == 0
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

        // Backwards compatibility
        // Previous versions stored a 256 byte array on the stack
        let (scratch, scratch_size) = if let Ok(scratch) =
            Self::variable(hubris, "HIFFY_SCRATCH", false)
        {
            let mut buf: Vec<u8> = vec![];
            buf.resize_with(scratch.size, Default::default);

            let _info = core.halt()?;
            core.read_8(scratch.addr, buf.as_mut_slice())?;
            core.run()?;

            let def = hubris.lookup_struct(scratch.goff)?;
            let val: Value =
                Value::Struct(reflect::load_struct(hubris, &buf, def, 0)?);
            let scratch_cell: StaticCell = StaticCell::from_value(&val)?;

            let scratch_size = scratch_cell.cell.value.as_array()?.len();

            println!("scratch size = {}", scratch_size);

            (Some(scratch), scratch_size)
        } else {
            (None, 256)
        };

        Ok(Self {
            hubris,
            ready: Self::variable(hubris, "HIFFY_READY", true)?,
            kick: Self::variable(hubris, "HIFFY_KICK", true)?,
            text: Self::variable(hubris, "HIFFY_TEXT", false)?,
            data: Self::variable(hubris, "HIFFY_DATA", false)?,
            rstack: Self::variable(hubris, "HIFFY_RSTACK", false)?,
            requests: Self::variable(hubris, "HIFFY_REQUESTS", true)?,
            scratch,
            scratch_size,
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

    pub fn functions(&mut self) -> Result<HiffyFunctions> {
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

        Ok(HiffyFunctions(rval))
    }

    /// Convenience routine to translate an Idol call into HIF operations
    pub fn idol_call_ops(
        &self,
        funcs: &HiffyFunctions,
        op: &idol::IdolOperation,
        payload: &[u8],
        ops: &mut Vec<Op>,
    ) -> Result<()> {
        let send = funcs.get("Send", 4)?;

        let push = |val: u32| {
            if val <= u8::MAX as u32 {
                Op::Push(val as u8)
            } else if val <= u16::MAX as u32 {
                Op::Push16(val as u16)
            } else {
                Op::Push32(val as u32)
            }
        };

        if let HubrisTask::Task(id) = op.task {
            ops.push(push(id));
        } else {
            bail!("interface matches invalid task {:?}", op.task);
        }

        let size = u8::try_from(4 + payload.len())
            .map_err(|_| anyhow!("payload size exceeds maximum size"))?;

        ops.push(push(op.code as u32));

        for byte in payload {
            ops.push(Op::Push(*byte));
        }

        ops.push(push(payload.len() as u32));
        ops.push(push(self.hubris.typesize(op.ok)? as u32));
        ops.push(Op::Call(send.id));
        ops.push(Op::DropN(size));

        Ok(())
    }

    /// Begins HIF execution.  This is non-blocking with respect to the HIF
    /// program, so you will need to poll [Self::done] to check for completion.
    pub fn start(
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

    /// Blocking execution of a program, returning the results
    pub fn run(
        &mut self,
        core: &mut dyn Core,
        ops: &[Op],
        data: Option<&[u8]>,
    ) -> Result<Vec<Result<Vec<u8>, u32>>> {
        self.start(core, ops, data)?;
        while !self.done(core)? {
            thread::sleep(Duration::from_millis(100));
        }
        self.results(core)
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

    pub fn rstack_size(&self) -> usize {
        println!("rStack size = {}", self.rstack.size);
        self.rstack.size
    }

    pub fn scratch_size(&self) -> usize {
        self.scratch_size
    }
}
