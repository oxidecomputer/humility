// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::{doppel::RpcHeader, doppel::StaticCell, idol};
use anyhow::{anyhow, bail, Context, Result};
use hif::*;
use humility::core::Core;
use humility::hubris::*;
use humility::reflect::{self, Load, Value};
use postcard::{take_from_bytes, to_slice};
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::thread;
use std::time::{Duration, Instant};
use zerocopy::{AsBytes, U16, U64};

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
    funcs: HubrisGoff,
    scratch_size: usize,
    cached: Option<(u32, u32)>,
    kicked: Option<Instant>,
    timeout: u32,
    state: State,
    functions: HashMap<String, TargetFunction>,
    rpc_results: Vec<Result<Vec<u8>, u32>>,
    rpc_reply_type: Option<&'a HubrisEnum>,
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
        if !core.is_net() {
            core.op_start()?;

            let (major, minor) = (
                Self::read_word(hubris, core, "HIFFY_VERSION_MAJOR"),
                Self::read_word(hubris, core, "HIFFY_VERSION_MINOR"),
            );

            core.op_done()?;

            let target = (major?, minor?);
            let ours = (HIF_VERSION_MAJOR, HIF_VERSION_MINOR);

            //
            // For now, we insist on an exact version match between Humility
            // and Hubris.
            //
            if ours != target {
                //
                // If the version in core appears wildly wrong (i.e, anything
                // greater than a byte), it may be because Hiffy is getting
                // starved; generate a messaage pointing in that direction.
                //
                if target.0 > 255 || target.1 > 255 {
                    bail!(
                        "HIF versions appear uninitialized; \
                    has the hiffy task not yet run?"
                    );
                }

                #[rustfmt::skip]
                bail!(
                    "HIF version mismatch: target has {}.{}; ours is {}.{}",
                    target.0, target.1, ours.0, ours.1
                );
            }
        }

        let scratch_size = match (
            core.is_net(),
            Self::variable(hubris, "HIFFY_SCRATCH", false),
        ) {
            (false, Ok(scratch)) => {
                let mut buf: Vec<u8> = vec![];
                buf.resize_with(scratch.size, Default::default);

                core.op_start()?;
                core.read_8(scratch.addr, buf.as_mut_slice())?;
                core.op_done()?;

                let def = hubris.lookup_struct(scratch.goff)?;
                let val: Value =
                    Value::Struct(reflect::load_struct(hubris, &buf, def, 0)?);
                let scratch_cell: StaticCell = StaticCell::from_value(&val)?;
                scratch_cell.cell.value.as_array()?.len()
            }
            _ => {
                // Backwards/network compatibility
                // Previous versions stored a 256 byte array on the stack
                256
            }
        };

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
            funcs: Self::definition(hubris, "HIFFY_FUNCTIONS")?,
            scratch_size,
            cached: None,
            kicked: None,
            timeout,
            state: State::Initialized,
            functions: HashMap::new(),
            rpc_reply_type: if core.is_net() {
                //
                // This should have been checked when we initially attached.
                //
                let rpc_task = hubris.lookup_task("udprpc").unwrap();

                Some(
                    hubris
                        .lookup_module(*rpc_task)?
                        .lookup_enum_byname(hubris, "RpcReply")?,
                )
            } else {
                None
            },
            rpc_results: Vec::new(),
        })
    }

    pub fn data_size(&self) -> usize {
        self.data.size
    }

    pub fn text_size(&self) -> usize {
        self.text.size
    }

    pub fn rdata_size(&self) -> usize {
        self.rstack.size
    }

    ///
    /// Convenience routine to indicate the size of a HIF snippet
    ///
    pub fn ops_size(&self, ops: &[Op]) -> Result<usize> {
        let mut text: Vec<u8> = vec![];
        text.resize_with(self.text.size, Default::default);
        let mut total = 0;

        for op in ops {
            match to_slice(op, text.as_mut_slice()) {
                Ok(serialized) => {
                    total += serialized.len();
                }
                Err(postcard::Error::SerializeBufferFull) => {
                    bail!(
                        "HIF snippet ({} ops) cannot fit in \
                        target program text ({} bytes)",
                        ops.len(),
                        text.len(),
                    );
                }
                Err(err) => {
                    bail!("HIF snippet serialization failed: {}", err);
                }
            }
        }

        Ok(total)
    }

    pub fn functions(&mut self) -> Result<HiffyFunctions> {
        let hubris = self.hubris;

        let goff = hubris
            .lookup_enum(self.funcs)?
            .lookup_variant_byname("Some")?
            .goff
            .ok_or_else(|| anyhow!("malconstructed functions"))?;

        let ptr = hubris.lookup_struct(goff)?.lookup_member("__0")?.goff;
        let goff = hubris.lookup_ptrtype(ptr)?;
        let functions = hubris.lookup_enum(goff)?;
        let mut rval = HashMap::new();

        //
        // Iterate over our functions.  Note that we very much expect these to
        // be encoded in the DWARF in program order!
        //
        for (id, f) in functions.variants.iter().enumerate() {
            let goff = f.goff.ok_or_else(|| {
                anyhow!("function {} in {}: missing a type", f.name, goff)
            })?;

            let mut func = HiffyFunction {
                id: TargetFunction(u8::try_from(id)?),
                name: f.name.to_string(),
                args: Vec::new(),
                errmap: HashMap::new(),
            };

            self.functions.insert(func.name.clone(), func.id);

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

    fn perform_rpc(&mut self, core: &mut dyn Core, ops: &[Op]) -> Result<()> {
        let send = self
            .functions
            .get("Send")
            .ok_or_else(|| anyhow!("illegal network operations: {:?}", ops))?;

        // Bail out immediately if the program makes a call other than Send
        if ops.iter().any(|op| matches!(*op, Op::Call(id) if id != *send)) {
            bail!("can't make non-Idol calls over RPC");
        }

        // Pick values that are much larger than we'd ever see on a machine
        const HIFFY_TEXT_SIZE: usize = 65536;
        const HIFFY_RSTACK_SIZE: usize = 65536;
        const HIFFY_SCRATCH_SIZE: usize = 65536;

        // hard-coded values in task/hiffy/src/main.rs
        const NLABELS: usize = 4;
        let mut stack = [None; 32];

        let mut rstack = vec![0u8; HIFFY_RSTACK_SIZE];
        let mut scratch = vec![0u8; HIFFY_SCRATCH_SIZE];
        let mut text = vec![0u8; HIFFY_TEXT_SIZE];

        // Serialize opcodes into `text`
        let buf = &mut text.as_mut_slice();
        let mut current = 0;
        for op in ops {
            let serialized = to_slice(op, &mut buf[current..]).unwrap();
            current += serialized.len();
        }

        ////////////////////////////////////////////////////////////////////////
        // HIF requires its functions to have a bare `fn` signature, i.e. it
        // doesn't support closures.  This is tricky, because we need to pass
        // some extra parameters in order to replace local calls with networked
        // RPC.
        //
        // To work around this, we use a static workspace, which is defined as a
        // thread-local `RefCell<HiffySendWorkspace>`.  We'll smuggle our local
        // variables as pointers into this workspace, then clean them up when
        // we're done.
        struct HiffySendWorkspace {
            hubris: Option<std::ptr::NonNull<HubrisArchive>>,
            core: Option<std::ptr::NonNull<dyn Core>>,

            /// If we receive an RPC result, then record the buffer here
            results: Vec<Vec<u8>>,

            errors: Vec<anyhow::Error>,
        }

        thread_local! {
            static HIFFY_SEND_WORKSPACE: RefCell<HiffySendWorkspace> =
                RefCell::new(
                    HiffySendWorkspace {
                        hubris: None,
                        core: None,
                        results: vec![],
                        errors: vec![],
                    });
        }

        /// Reimplementation of `fn send` in `task/hiffy/src/common.rs`.
        ///
        /// We extract a handful of parameters, then pack them into a packet and
        /// send them across the network to the `udprpc` task.
        fn hiffy_send_fn(
            stack: &[Option<u32>],
            _data: &[u8],
            rval: &mut [u8],
        ) -> Result<usize, Failure> {
            let mut payload = [0u8; 32];

            let sp = stack.len();
            if sp < 4 {
                return Err(Failure::Fault(Fault::MissingParameters));
            }

            let nreply = stack[sp - 1]
                .ok_or(Failure::Fault(Fault::EmptyParameter(4)))?;
            let nbytes: u16 = stack[sp - 2]
                .ok_or(Failure::Fault(Fault::EmptyParameter(3)))?
                .try_into()
                .map_err(|_| Failure::Fault(Fault::BadParameter(1)))?;

            let fp = sp - (nbytes as usize + 4);

            let task: u16 = stack[fp]
                .ok_or(Failure::Fault(Fault::EmptyParameter(0)))?
                .try_into()
                .map_err(|_| Failure::Fault(Fault::BadParameter(1)))?;

            let op: u16 = stack[fp + 1]
                .ok_or(Failure::Fault(Fault::EmptyParameter(1)))?
                .try_into()
                .map_err(|_| Failure::Fault(Fault::BadParameter(1)))?;

            let base = fp + 2;

            for i in base..base + nbytes as usize {
                payload[i - base] = stack[i]
                    .ok_or(Failure::Fault(Fault::EmptyParameter(2)))?
                    .try_into()
                    .map_err(|_| Failure::Fault(Fault::BadParameter(2)))?;
            }

            let mut buf = [0u8; 1024]; // matches buffer size in `task-udprpc`
            HIFFY_SEND_WORKSPACE.with(|workspace| {
                let mut workspace = workspace.borrow_mut();
                let (hubris, core) = {
                    // SAFETY: we only ever call this function when the pointers
                    // are populated, and reset them to null / None afterwards.
                    // This means we should fail at this unwrap() if someone
                    // violates the rules.
                    unsafe {
                        (
                            workspace.hubris.unwrap().as_ref(),
                            workspace.core.unwrap().as_mut(),
                        )
                    }
                };
                let image_id = hubris.image_id().unwrap();

                let header = RpcHeader {
                    image_id: U64::from_bytes(image_id.try_into().unwrap()),
                    task: U16::new(task),
                    op: U16::new(op),
                    nreply: U16::new(nreply as u16),
                    nbytes: U16::new(nbytes),
                };

                let mut packet = header.as_bytes().to_vec();
                packet.extend(&payload[0..nbytes as usize]);

                // Send the packet out
                if let Err(e) = core.send(&packet) {
                    workspace.errors.push(e);
                    return Err(Failure::FunctionError(0));
                }

                // Try to receive a reply
                match core.recv(buf.as_mut_slice()) {
                    Ok(n) => {
                        workspace.results.push(buf[0..n].to_vec());
                        Ok(())
                    }
                    Err(e) => {
                        workspace.errors.push(e);
                        Err(Failure::FunctionError(0))
                    }
                }
            })?;

            //
            // Now check the return code of the Idol call that we made, and
            // spoof up a HIF function result.  Note that this implicitly
            // depends on the fact that Idol does not use 0 as an error
            // condition.
            //
            let code = u32::from_be_bytes(buf[1..5].try_into().unwrap());

            if code != 0 {
                return Err(Failure::FunctionError(code));
            }
            rval[0..nreply as usize]
                .copy_from_slice(&buf[5..(5 + nreply as usize)]);

            Ok(nreply.try_into().unwrap())
        }
        ////////////////////////////////////////////////////////////////////////
        // Back to normal code!
        //
        // We know from the `Call` check that this program only ever calls
        // `Send`, at a particular index.  We'll build a function array that
        // contains our local `hiffy_send_fn`, repeated just times so that the
        // call operation works; i.e. at index `send.0`, it will find a function
        // pointer to `hiffy_send_fn`.
        let functions: Vec<Function> = vec![hiffy_send_fn; send.0 as usize + 1];

        // Okay, this is a _little_ cursed: HIF functions use a C calling
        // convention without any place to stash a context pointer, so we're
        // going to put raw pointers to the HubrisArchive and Core into a global
        // variable.
        //
        // This is made trickier by lifetimes: these are references, rather than
        // value types, so we need to cast away the lifetime for the Core using
        // std::mem::transmute (!)
        //
        // This is a thread-local structure, so no one should be able to mess
        // with it except us.  As belt-and-suspenders, we also use a `struct
        // WorkspaceCleanup` to make sure that we clear those pointers when
        // we're done (so that no one tries to use them after the references
        // expire).
        //
        // This cleanup is not **strictly necessary**, since there's no harm in
        // leaking pointers (as long as you don't dereference them).  Still, it
        // produces more pleasant invariants: the pointers are only valid
        // between here and the drop of `_cleanup` (either explicitly after
        // `execute` is called, or during a panic).
        struct WorkspaceCleanup;
        impl Drop for WorkspaceCleanup {
            fn drop(&mut self) {
                HIFFY_SEND_WORKSPACE.with(|workspace| {
                    let mut workspace = workspace.borrow_mut();
                    workspace.hubris = None;
                    workspace.core = None;
                });
            }
        }
        let cleanup = WorkspaceCleanup; // cleans up on drop

        HIFFY_SEND_WORKSPACE.with(|workspace| {
            let mut workspace = workspace.borrow_mut();
            *workspace = HiffySendWorkspace {
                hubris: Some(std::ptr::NonNull::from(self.hubris)),

                // SAFETY: We are transmuting to strip the lifetime from this
                // object, but guarantee through the use of WorkspaceCleanup
                // that it won't be possible for it to outlive the original
                // reference.
                core: Some(
                    std::ptr::NonNull::new(unsafe {
                        std::mem::transmute::<
                            *mut (dyn Core + '_),
                            *mut (dyn Core + 'static),
                        >(core as *mut dyn Core)
                    })
                    .unwrap(),
                ),

                results: vec![],
                errors: vec![],
            };
        });

        let v = execute::<_, NLABELS>(
            &text,
            &functions,
            &[], // no data
            &mut stack,
            &mut rstack,
            &mut scratch,
            |_offset, _op| Ok(()),
        );

        // Explicitly do cleanup here, to minimize the danger zone when raw
        // pointers are living in `HIFFY_SEND_WORKSPACE`
        drop(cleanup);

        if let Err(e) = v {
            bail!("Hiffy execution error: {e:?}");
        }
        assert_eq!(self.rpc_results.len(), 0);

        self.state = State::Kicked;
        HIFFY_SEND_WORKSPACE.with(|workspace| {
            let workspace = workspace.borrow();
            if let Some(e) = workspace.errors.first() {
                // We have to translate from Error -> String to work around
                // ownership
                return Err(anyhow!(e.to_string()));
            }
            for buf in &workspace.results {
                //
                // If udprpc gave us an error, it's because something was
                // malformed or (most likely) we have an image mismatch.  We don't
                // want to continue processing in this case; toss our error.
                //
                if buf[0] != 0 {
                    let rpc_reply_type = self.rpc_reply_type.unwrap();
                    match rpc_reply_type.lookup_variant_by_tag(buf[0] as u64) {
                        Some(e) => {
                            let image_id = self.hubris.image_id().unwrap();
                            let msg = format!("RPC error: {}", e.name);
                            if e.name == "BadImageId" {
                                bail!(
                                "{msg}: {:02x?} (Humility) {:02x?} (Hubris)",
                                image_id,
                                &buf[1..9]
                            );
                            } else {
                                bail!("{msg}");
                            }
                        }
                        None => {
                            bail!("Got unknown error from `udprpc`: {}", buf[0])
                        }
                    }
                }

                assert_eq!(buf[0], 0);

                //
                // Now check the return code of the Idol call that we made, and
                // spoof up a HIF function result.  Note that this implicitly
                // depends on the fact that Idol does not use 0 as an error
                // condition.
                //
                let rval = u32::from_be_bytes(buf[1..5].try_into().unwrap());

                if rval == 0 {
                    self.rpc_results.push(Ok(buf[5..].to_vec()));
                } else {
                    self.rpc_results.push(Err(rval));
                }
            }
            self.state = State::Kicked;
            Ok(())
        })
    }

    /// Convenience routine to translate an Idol call into HIF operations,
    /// generic across `Send/SendLeaseRead/SendLeaseWrite`
    fn idol_call_ops_inner(
        &self,
        funcs: &HiffyFunctions,
        op: &idol::IdolOperation,
        payload: &[u8],
        ops: &mut Vec<Op>,
        lease_size: Option<u32>,
        func_name: &str,
    ) -> Result<()> {
        let arg_count = if lease_size.is_some() { 5 } else { 4 };
        let send = funcs.get(func_name, arg_count)?;

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

        let size = u8::try_from(arg_count + payload.len())
            .map_err(|_| anyhow!("payload size exceeds maximum size"))?;

        ops.push(push(op.code as u32));

        for byte in payload {
            ops.push(Op::Push(*byte));
        }

        ops.push(push(payload.len() as u32));
        let reply_size = match op.operation.encoding {
            ::idol::syntax::Encoding::Zerocopy => {
                self.hubris.typesize(op.ok)?
            }
            ::idol::syntax::Encoding::Ssmarshal
            | ::idol::syntax::Encoding::Hubpack => {
                self.hubris.hubpack_serialized_maxsize(op.ok)?
            }
        };
        ops.push(push(reply_size as u32));
        if let Some(lease_size) = lease_size {
            ops.push(push(lease_size));
        }
        ops.push(Op::Call(send.id));
        ops.push(Op::DropN(size));

        Ok(())
    }

    /// Convenience routine to translate an Idol call into HIF operations
    pub fn idol_call_ops(
        &self,
        funcs: &HiffyFunctions,
        op: &idol::IdolOperation,
        payload: &[u8],
        ops: &mut Vec<Op>,
    ) -> Result<()> {
        self.idol_call_ops_inner(funcs, op, payload, ops, None, "Send")
    }

    /// Convenience routine to translate an Idol call (which reads data from the
    /// device back to the host) into HIF operations
    pub fn idol_call_ops_read(
        &self,
        funcs: &HiffyFunctions,
        op: &idol::IdolOperation,
        payload: &[u8],
        ops: &mut Vec<Op>,
        lease_size: u32,
    ) -> Result<()> {
        self.idol_call_ops_inner(
            funcs,
            op,
            payload,
            ops,
            Some(lease_size),
            "SendLeaseRead",
        )
    }

    /// Convenience routine to translate an Idol call (which writes data from
    /// the host to the device) into HIF operations
    pub fn idol_call_ops_write(
        &self,
        funcs: &HiffyFunctions,
        op: &idol::IdolOperation,
        payload: &[u8],
        ops: &mut Vec<Op>,
        lease_size: u32,
    ) -> Result<()> {
        self.idol_call_ops_inner(
            funcs,
            op,
            payload,
            ops,
            Some(lease_size),
            "SendLeaseWrite",
        )
    }

    /// Convenience routine to pull out the result of an Idol call
    pub fn idol_result(
        &mut self,
        op: &idol::IdolOperation,
        result: &Result<Vec<u8>, u32>,
    ) -> Result<humility::reflect::Value> {
        use humility::reflect::{deserialize_value, load_value};

        match result {
            Ok(val) => {
                let ty = self.hubris.lookup_type(op.ok).unwrap();
                Ok(match op.operation.encoding {
                    ::idol::syntax::Encoding::Zerocopy => {
                        load_value(self.hubris, val, ty, 0)?
                    }
                    ::idol::syntax::Encoding::Ssmarshal
                    | ::idol::syntax::Encoding::Hubpack => {
                        deserialize_value(self.hubris, val, ty)?.0
                    }
                })
            }
            Err(e) => {
                let variant = if let Some(error) = op.error {
                    error.lookup_variant_by_tag(*e as u64)
                } else {
                    None
                };

                if let Some(variant) = variant {
                    bail!(variant.name.to_string())
                } else {
                    bail!(format!("{:x?}", e))
                }
            }
        }
    }

    /// Begins HIF execution.  This is potentially non-blocking with respect to
    /// the HIF program, so you will need to poll [Self::done] to check for
    /// completion.
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

        if core.is_net() {
            if data.is_some() {
                bail!("cannot execute -based operations over the network");
            }

            return self.perform_rpc(core, ops);
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

        core.op_start()?;

        //
        // We now want to loop until we know that the HIF facility is
        // available.  Generally, if we see that HIFFY_READY is 0, it is
        // because our in situ cohort either never ran, or has not checked
        // back in (i.e., it is wedged on an earlier call).  But it's also
        // conceivable that we have caught it in the very small window when it
        // is awake, but only to check if it has been kicked; to differentiate
        // these cases, we back off for a linearly increasing number of
        // milliseconds until we conclude that the facility is alive, or that
        // we have tried too many times.
        //
        let mut lap = 0;
        const MAX_LAPS: u64 = 10;

        let ready = loop {
            if core.read_word_32(self.ready.addr)? == 1 {
                break true;
            }

            core.op_done()?;

            lap += 1;

            if lap >= MAX_LAPS {
                break false;
            }

            thread::sleep(Duration::from_millis(lap));
            core.op_start()?;
        };

        if !ready {
            bail!("HIF execution facility unavailable");
        }

        let buf = &mut text.as_mut_slice();
        let mut current = 0;

        for (n, op) in ops.iter().enumerate() {
            match to_slice(op, &mut buf[current..]) {
                Ok(serialized) => {
                    current += serialized.len();
                }
                Err(postcard::Error::SerializeBufferFull) => {
                    //
                    // This error that can be induced by HIF programmer error,
                    // so we want to fail in a way that provides some detail
                    // as to what is happening.
                    //
                    bail!(
                        "HIF program cannot be serialized: program length \
                        ({} operations) cannot fit in \
                        target program text ({} bytes); \
                        failed after serializing {} operations",
                        ops.len(),
                        buf.len(),
                        n
                    );
                }
                Err(err) => {
                    bail!("HIF program serialization failed: {}", err);
                }
            }
        }

        core.write_8(self.text.addr, &buf[0..current])?;

        if let Some(data) = data {
            core.write_8(self.data.addr, data)?;
        }

        self.cached = Some((
            core.read_word_32(self.requests.addr)?,
            core.read_word_32(self.errors.addr)?,
        ));

        core.write_word_32(self.kick.addr, 1)?;

        self.kicked = Some(Instant::now());

        self.state = State::Kicked;

        core.op_done()?;

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

        //
        // If this is over the network, our calls are already done by the
        // time we're here; immediately transition to `ResultsReady`.
        //
        if core.is_net() {
            self.state = State::ResultsReady;
            return Ok(true);
        }

        core.op_start()?;

        let vars = (
            core.read_word_32(self.requests.addr)?,
            core.read_word_32(self.errors.addr)?,
        );

        core.op_done()?;

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

                core.op_start()?;
                let r = core.read_8(self.failure.addr, buf.as_mut_slice());
                core.op_done()?;

                match r {
                    Ok(_) => {
                        let fmt = HubrisPrintFormat {
                            hex: true,
                            ..HubrisPrintFormat::default()
                        };

                        let hubris = self.hubris;
                        let f =
                            hubris.printfmt(&buf, self.failure.goff, fmt)?;

                        // If Hiffy reports `Invalid`, this could be due to a
                        // patch version mismatch, i.e. Humility trying to use
                        // HIF operations that the target does not know about.
                        if f == "Some(Invalid)" {
                            let patch = Self::read_word(
                                hubris,
                                core,
                                "HIFFY_VERSION_PATCH",
                            );
                            match patch {
                                Ok(patch) => {
                                    if patch != HIF_VERSION_PATCH {
                                        bail!(
                                            "request failed: {0}. Perhaps due \
                                             to HIF version mismatch? \
                                             ({1}.{2}.{3} on host, \
                                              {1}.{2}.{4} on device)",
                                            f,
                                            HIF_VERSION_MAJOR,
                                            HIF_VERSION_MINOR,
                                            HIF_VERSION_PATCH,
                                            patch
                                        );
                                    }
                                }
                                Err(e) => bail!(
                                    "request failed: {}; failed to read HIF \
                                     patch version: {:?}",
                                    f,
                                    e
                                ),
                            }
                        }
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

        if core.is_net() {
            let results = self.rpc_results.clone();
            self.rpc_results = Vec::new();
            self.state = State::ResultsConsumed;
            return Ok(results);
        }

        let mut rstack: Vec<u8> = vec![];
        rstack.resize_with(self.rstack.size, Default::default);

        core.op_start()?;

        let mut rvec = vec![];
        core.read_8(self.rstack.addr, rstack.as_mut_slice())?;

        core.op_done()?;

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
        self.rstack.size
    }

    pub fn scratch_size(&self) -> usize {
        self.scratch_size
    }
}
