// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
use crate::DumpAgent;
use anyhow::{bail, Result};
use core::mem::size_of;
use hif::*;
use humility::{core::Core, hubris::HubrisArchive};
use humility_hiffy::{HiffyContext, IpcError};
use humility_idol::{self as idol, HubrisIdol};
use humpty::{DumpAreaHeader, DumpSegment, DumpSegmentHeader};

/// Represents a dump agent that communicates through the `hiffy` task
///
/// The agent is either local (connected via a debugger) or remote; in the
/// latter case, HIF programs are sent to the `udprpc` task under the hood by
/// the `HiffyContext`, which sees that its core is a `NetCore`.
pub struct HiffyDumpAgent<'a> {
    hubris: &'a HubrisArchive,
    core: &'a mut dyn Core,
    context: HiffyContext<'a>,
}

impl<'a> HiffyDumpAgent<'a> {
    pub fn new(
        hubris: &'a HubrisArchive,
        core: &'a mut dyn Core,
        timeout: u32,
    ) -> Result<Self> {
        let context = HiffyContext::new(hubris, core, timeout)?;

        //
        // Do some sanity checks on the number of bytes returned by read_dump().
        // If these checks fail, something is really wrong.
        //
        let op = hubris.get_idol_command("DumpAgent.read_dump")?;
        let rsize = hubris.lookup_type(op.ok)?.size(hubris)?;
        let min = size_of::<DumpAreaHeader>() + size_of::<DumpSegment>();

        if rsize < min {
            bail!(
                "read_dump size is {rsize}, but expected minimum size of {min}"
            );
        }

        let max_nsegments = (rsize - min) / size_of::<DumpSegmentHeader>();

        if max_nsegments < 16 {
            bail!(
                "read_dump size of {rsize} is unexpectedly small (can only \
                 hold {max_nsegments} dumpable segments)"
            );
        }

        Ok(Self { hubris, core, context })
    }
    fn run(&mut self, ops: &[Op]) -> Result<Vec<Result<Vec<u8>, IpcError>>> {
        self.context.run(self.core, ops, None)
    }
}

impl<'a> DumpAgent for HiffyDumpAgent<'a> {
    fn core(&mut self) -> &mut dyn Core {
        self.core
    }

    fn initialize_dump(&mut self) -> Result<()> {
        let op = self.hubris.get_idol_command("DumpAgent.initialize_dump")?;
        let mut ops = vec![];
        self.context.idol_call_ops(&op, &[], &mut ops)?;
        ops.push(Op::Done);

        match self.run(ops.as_slice())?[0] {
            Err(IpcError::Error(err)) => {
                bail!("failed to initialize dump: {}", op.strerror(err))
            }
            // TODO(eliza): perhaps we can retry?
            Err(IpcError::ServerDied(_)) => {
                bail!("failed to initialize dump: dump agent died")
            }
            Ok(_) => {}
        }

        Ok(())
    }

    fn initialize_segments(&mut self, segments: &[(u32, u32)]) -> Result<()> {
        let op = self.hubris.get_idol_command("DumpAgent.add_dump_segment")?;
        let mut ops = vec![];

        for (base, size) in segments {
            let payload = op.payload(&[
                ("address", idol::IdolArgument::Scalar(*base as u64)),
                ("length", idol::IdolArgument::Scalar(*size as u64)),
            ])?;

            self.context.idol_call_ops(&op, &payload, &mut ops)?;
        }

        ops.push(Op::Done);

        let results = self.run(ops.as_slice())?;

        for (result, (base, size)) in results.iter().zip(segments.iter()) {
            match result {
                Err(IpcError::Error(err)) => {
                    bail!("failed to add segment at address {base:#x} for length {size}: {}", op.strerror(*err))
                }
                // TODO(eliza): perhaps we can retry?
                Err(IpcError::ServerDied(_)) => {
                    bail!("failed to add segment at address {base:#x} for length {size}: dump agent died")
                }
                Ok(_) => {}
            }
        }

        Ok(())
    }

    fn take_dump(&mut self) -> Result<()> {
        let op = self.hubris.get_idol_command("DumpAgent.take_dump")?;
        let mut ops = vec![];

        let rindex = if !self.core.is_net() {
            //
            // If we are connected via a dongle, we will need to be unplugged
            // in order for the dump to operate.  Emit a message to this
            // effect, and then send a HIF payload that will wait for 10
            // seconds (100 iterations of 100ms apiece) and then start the dump;
            // if the dongle has been pulled, the dump will start -- and if
            // not the dump will fail.  However, because determining the
            // presence of the dongle necessitates activating the pins on the
            // RoT, we will lose our connection either way -- and unless the
            // dump fails for an earlier reason, it will look like we lost
            // our SWD connection no matter what.
            //
            humility::msg!(
                "dump will start in 10 seconds; unplug probe now, and \
                 reset RoT via SWD after dump is complete to re-attach"
            );

            let sleep = self.context.get_function("Sleep", 1)?;
            let ms = 100;
            let iter = 100;

            ops.extend([
                Op::Push(0),                      // Iterations completed
                Op::Push(0),                      // Dummy comparison value
                Op::Label(Target(0)),             // Start of loop
                Op::Drop,                         // Drop comparison
                Op::Push(ms),                     // Push arg for 100ms
                Op::Call(sleep.id),               // Sleep for 100ms
                Op::Drop,                         // Drop arg
                Op::Push(1),                      // Push increment value
                Op::Add,                          // Add to iterations
                Op::Push(iter),                   // Push limit
                Op::BranchGreaterThan(Target(0)), // Continue if not at limit
            ]);

            iter as usize
        } else {
            humility::msg!(
                "taking dump; target will be stopped for ~20 seconds"
            );
            0
        };

        self.context.idol_call_ops(&op, &[], &mut ops)?;
        ops.push(Op::Done);

        let results = self.run(ops.as_slice())?;

        match results[rindex] {
            Err(IpcError::Error(err)) => {
                bail!("failed to take dump: {}", op.strerror(err))
            }
            Err(e) => bail!("failed to take dump: {e}"),
            Ok(_) => {}
        }

        Ok(())
    }

    /// Reads dump areas from the target
    ///
    /// Under the hood, we combine multiple calls to `DumpAgent.read_dump` into
    /// a single HIF program for efficiency; this should be transparent to the
    /// caller.
    fn read_generic(
        &mut self,
        areas: &mut dyn Iterator<Item = (u8, u32)>,
        cont: &mut dyn FnMut(u8, u32, &[u8]) -> Result<bool>,
    ) -> Result<Vec<Vec<u8>>> {
        // Because HIF has overhead, we're going to process a chunk of multiple
        // `read_dump` calls in a single HIF program.  The number depends on our
        // returned data size and the Hiffy context's `rdata` array size.
        let op = self.hubris.get_idol_command("DumpAgent.read_dump")?;
        let rsize = self.hubris.lookup_type(op.ok)?.size(self.hubris)?;
        let chunksize = (self.context.rdata_size() / rsize) - 1;

        let mut rval = vec![];
        loop {
            // Prepare a program to dump a handful of sections
            let mut ops = vec![];
            let mut pos = vec![];
            for _ in 0..chunksize {
                if let Some((index, offset)) = areas.next() {
                    let payload = op.payload(&[
                        ("index", idol::IdolArgument::Scalar(index as u64)),
                        ("offset", idol::IdolArgument::Scalar(offset as u64)),
                    ])?;

                    self.context.idol_call_ops(&op, &payload, &mut ops)?;
                    pos.push((index, offset))
                } else {
                    break;
                }
            }
            if ops.is_empty() {
                break;
            }
            ops.push(Op::Done);

            // Check the results
            let results = self.run(&ops)?;
            for (r, (index, offset)) in results.iter().zip(pos.into_iter()) {
                match r {
                    Ok(val) => {
                        if !cont(index, offset, val)? {
                            return Ok(rval);
                        }
                        rval.push(val.to_vec());
                    }
                    Err(IpcError::Error(err)) => {
                        let s = op.strerror(*err);
                        if s == "InvalidArea" {
                            return Ok(rval);
                        } else {
                            bail!(
                                "failed to read index {index}, offset: {offset}:
                                {s}",
                            );
                        }
                    }
                    Err(e) => bail!(
                        "failed to read index {index}, offset: {offset}: {e}"
                    ),
                }
            }
        }

        Ok(rval)
    }

    fn dump_task(&mut self, task_index: u32) -> Result<u8> {
        let op = self.hubris.get_idol_command("DumpAgent.dump_task")?;
        let mut ops = vec![];
        let payload = op.payload(&[(
            "task_index",
            idol::IdolArgument::Scalar(task_index as u64),
        )])?;
        self.context.idol_call_ops(&op, &payload, &mut ops)?;
        ops.push(Op::Done);

        let out = self.run(ops.as_slice())?;
        assert_eq!(out.len(), 1);
        match &out[0] {
            Ok(v) => {
                assert_eq!(v.len(), 1);
                Ok(v[0])
            }
            Err(IpcError::Error(err)) => {
                bail!("failed to dump task: {}", op.strerror(*err))
            }
            Err(e) => {
                bail!("failed to dump task: {e}")
            }
        }
    }

    fn dump_task_region(
        &mut self,
        task_index: u32,
        start: u32,
        length: u32,
    ) -> Result<u8> {
        let op = self.hubris.get_idol_command("DumpAgent.dump_task_region")?;
        let mut ops = vec![];
        let payload = op.payload(&[
            ("task_index", idol::IdolArgument::Scalar(task_index as u64)),
            ("start", idol::IdolArgument::Scalar(start as u64)),
            ("length", idol::IdolArgument::Scalar(length as u64)),
        ])?;
        self.context.idol_call_ops(&op, &payload, &mut ops)?;
        ops.push(Op::Done);

        let out = self.run(ops.as_slice())?;
        assert_eq!(out.len(), 1);
        match &out[0] {
            Ok(v) => {
                assert_eq!(v.len(), 1);
                Ok(v[0])
            }
            Err(IpcError::Error(err)) => {
                bail!("failed to dump task region: {}", op.strerror(*err))
            }
            Err(e) => {
                bail!("failed to dump task region: {e}")
            }
        }
    }

    fn reinitialize_dump_from(&mut self, index: u8) -> Result<()> {
        let op =
            self.hubris.get_idol_command("DumpAgent.reinitialize_dump_from")?;
        let mut ops = vec![];
        let payload =
            op.payload(&[("index", idol::IdolArgument::Scalar(index as u64))])?;
        self.context.idol_call_ops(&op, &payload, &mut ops)?;
        ops.push(Op::Done);

        let out = self.run(ops.as_slice())?;
        assert_eq!(out.len(), 1);
        match &out[0] {
            Ok(_) => Ok(()),
            Err(IpcError::Error(err)) => {
                bail!("failed to dump task region: {}", op.strerror(*err))
            }
            Err(e) => {
                bail!("failed to dump task region: {e}")
            }
        }
    }
}
