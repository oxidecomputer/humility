// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility dump`
//!
//! `humility dump` takes a dump of the attached system, writing out an ELF
//! core file:
//!
//! ```console
//! % humility dump
//! humility: attached via ST-Link
//! humility: core halted
//! humility: dumping to hubris.core.0
//! humility: dumped 1.12MB in 24 seconds
//! humility: core resumed
//! ```
//!
//! A dump file name may also be specified:
//!
//! ```console
//! % humility dump hubris.core.`date +%s`
//! humility: attached via ST-Link
//! humility: core halted
//! humility: dumping to hubris.core.1600718079
//! humility: dumped 1.12MB in 24 seconds
//! humility: core resumed
//! ```
//!
//! The resulting dump can be used with many commands (including `manifest`,
//! `map`, `readvar`, and `tasks`) -- and need not be run on the same machine
//! as the debugged MCU, e.g.:
//!
//! ```console
//! % humility -d hubris.core.0 tasks
//! humility: attached to dump
//! system time = 94529
//! ID TASK                       GEN PRI STATE    
//!  0 jefe                         0   0 recv, notif: bit0 bit1(T+71)
//!  1 net                          1   5 recv, notif: bit0(irq61) bit2(T+213)
//!  2 sys                          0   1 recv
//!  3 spi4_driver                  0   3 recv
//!  4 spi2_driver                  0   3 recv
//!  5 i2c_driver                   0   3 recv
//!  6 spd                          0   2 notif: bit0(irq31/irq32)
//!  7 thermal                      0   5 recv, notif: bit0(T+673)
//!  8 power                        0   6 recv, notif: bit0(T+351)
//!  9 hiffy                        0   5 wait: reply from dump_agent/gen0
//! 10 gimlet_seq                   0   4 recv, notif: bit0
//! 11 hash_driver                  0   2 recv
//! 12 hf                           0   3 recv
//! 13 update_server                0   3 recv
//! 14 sensor                       0   4 recv, notif: bit0(T+472)
//! 15 host_sp_comms                0   7 recv, notif: bit0(irq82) bit1
//! 16 udpecho                      0   6 notif: bit0
//! 17 udpbroadcast                 0   6 notif: bit31(T+86)
//! 18 udprpc                       0   6 notif: bit0
//! 19 control_plane_agent          0   6 recv, notif: bit0 bit1(irq37) bit2
//! 20 sprot                        0   4 notif: bit31(T+2)
//! 21 validate                     0   5 recv
//! 22 vpd                          0   4 recv
//! 23 user_leds                    0   2 recv
//! 24 dump_agent                   0   4 wait: reply from sprot/gen0
//! 25 idle                         0   8 RUNNING
//! ```
//!

use anyhow::{anyhow, bail, Context, Result};
use clap::{ArgGroup, CommandFactory, Parser};
use core::mem::size_of;
use hif::*;
use humility::arch::ARMRegister;
use humility::cli::Subcommand;
use humility::core::{Core, NetAgent};
use humility::hubris::*;
use humility_cmd::hiffy::*;
use humility_cmd::idol::{self, HubrisIdol};
use humility_cmd::{Archive, Attach, Command, CommandKind, Validate};
use humpty::*;
use indexmap::IndexMap;
use indicatif::{HumanBytes, HumanDuration};
use indicatif::{ProgressBar, ProgressStyle};
use num_traits::FromPrimitive;
use rand::Rng;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::path::Path;
use std::time::{Duration, Instant};
use zerocopy::FromBytes;

#[derive(Parser, Debug)]
#[clap(
    name = "dump", about = env!("CARGO_PKG_DESCRIPTION"),
    group = ArgGroup::new("simulation").multiple(false)
        .required(false).requires("force-dump-agent")
)]

struct DumpArgs {
    /// sets timeout
    #[clap(
        long, short = 'T', default_value_t = 20000, value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// show dump agent status
    #[clap(long, conflicts_with = "simulation")]
    dump_agent_status: bool,

    /// force use of the dump agent when directly attached with a debug probe
    #[clap(long)]
    force_dump_agent: bool,

    /// force manual initiation, leaving target halted
    #[clap(long, requires = "force-dump-agent")]
    force_manual_initiation: bool,

    /// force existing in situ dump to be read
    #[clap(long, conflicts_with = "simulation")]
    force_read: bool,

    /// initialize dump state, clearing any dump at the dump agent
    #[clap(long, conflicts_with = "simulation")]
    initialize_dump_agent: bool,

    /// retain dump state after a system dump
    #[clap(long, conflicts_with_all = &["task", "list", "area"])]
    retain_state: bool,

    /// overwrite any dump state as part of taking dump
    #[clap(
        long, short = 'F',
        conflicts_with_all = &["initialize-dump-agent", "simulate-dumper"],
    )]
    force_overwrite: bool,

    /// simulate dumper by reading directly from target (skipping agent)
    #[clap(long, group = "simulation")]
    simulate_dumper: bool,

    /// in addition to simulating the dumper, generate a stock dump
    #[clap(long, requires = "simulation")]
    stock_dumpfile: Option<String>,

    /// emulate in situ dumper by reading directly from target and writing
    /// compressed memory back to agent's dump region
    #[clap(long, group = "simulation")]
    emulate_dumper: bool,

    #[clap(long, requires = "simulation", conflicts_with = "stock-dumpfile")]
    task: Option<String>,

    #[clap(short, long, conflicts_with_all = &[
        "task", "simulation", "list"
    ])]
    area: Option<usize>,

    /// leave the target halted
    #[clap(long, conflicts_with = "simulation")]
    leave_halted: bool,

    #[clap(long, short, conflicts_with_all = &[
        "task", "simulation", "area"
    ])]
    list: bool,

    dumpfile: Option<String>,
}

////////////////////////////////////////////////////////////////////////////////

//
// When using the dump agent, we create our own ersatz Core
//
struct AgentCore {
    flash: HubrisFlashMap,
    ram_regions: BTreeMap<u32, Vec<u8>>,
    registers: HashMap<ARMRegister, u32>,
}

impl AgentCore {
    fn new(hubris: &HubrisArchive) -> Result<AgentCore> {
        Ok(Self {
            flash: HubrisFlashMap::new(hubris)?,
            ram_regions: Default::default(),
            registers: Default::default(),
        })
    }

    fn add_ram_region(&mut self, addr: u32, contents: Vec<u8>) {
        self.ram_regions.insert(addr, contents);
    }

    fn add_register(&mut self, reg: ARMRegister, val: u32) {
        self.registers.insert(reg, val);
    }

    fn read_flash(&self, addr: u32, data: &mut [u8]) -> Result<()> {
        if let Some((&base, &(size, offset))) =
            self.flash.regions.range(..=addr).rev().next()
        {
            if base > addr || base + size <= addr {
                //
                // It's not here, and we have already tried RAM...
                //
                bail!("address 0x{:08x} not found", addr);
            }

            let start = (addr - base) as usize;
            let roffs = offset + start;

            if start + data.len() <= size as usize {
                //
                // This flash region wholly contains our desired region; copy
                // it and leave.
                //
                data.copy_from_slice(
                    &self.flash.contents[roffs..roffs + data.len()],
                );

                return Ok(());
            }

            let len = (size as usize) - start;
            data[..len]
                .copy_from_slice(&self.flash.contents[roffs..roffs + len]);

            self.read_flash(addr + len as u32, &mut data[len..])
        } else {
            bail!("address 0x{:08x} not found", addr);
        }
    }

    fn read(&mut self, addr: u32, data: &mut [u8]) -> Result<()> {
        if let Some((&base, contents)) =
            self.ram_regions.range(..=addr).rev().next()
        {
            if base > addr || base + (contents.len() as u32) <= addr {
                //
                // We don't have this in RAM -- pull it out of flash.
                //
                return self.read_flash(addr, data);
            }

            let start = (addr - base) as usize;

            if start + data.len() <= contents.len() {
                //
                // This region -- and only this region -- contains our RAM.
                // Copy it and leave.
                //
                data.copy_from_slice(&contents[start..start + data.len()]);
                return Ok(());
            }

            //
            // This region contains our RAM, but there is more.  Copy the bit
            // that we want and recurse.
            //
            let len = contents.len() - start;
            data[..len].copy_from_slice(&contents[start..contents.len()]);
            self.read(addr + len as u32, &mut data[len..])
        } else {
            self.read_flash(addr, data)
        }
    }
}

impl Core for AgentCore {
    fn info(&self) -> (String, Option<String>) {
        panic!("unexpected call to AgentCore info");
    }

    fn read_8(&mut self, addr: u32, data: &mut [u8]) -> Result<()> {
        self.read(addr, data)
    }

    fn read_reg(&mut self, reg: ARMRegister) -> Result<u32> {
        match self.registers.get(&reg) {
            Some(val) => Ok(*val),
            None => bail!("unexpected read from register {reg:?}"),
        }
    }

    fn write_reg(&mut self, reg: ARMRegister, _value: u32) -> Result<()> {
        bail!("cannot write register {} over dump agent", reg);
    }

    fn write_word_32(&mut self, _addr: u32, _data: u32) -> Result<()> {
        bail!("cannot write a word over dump agent");
    }

    fn write_8(&mut self, _addr: u32, _data: &[u8]) -> Result<()> {
        bail!("cannot write a byte over dump agent");
    }

    fn halt(&mut self) -> Result<()> {
        bail!("unexpected call to halt");
    }

    fn run(&mut self) -> Result<()> {
        bail!("unexpected call to run");
    }

    fn step(&mut self) -> Result<()> {
        bail!("can't step over dump agent");
    }

    fn init_swv(&mut self) -> Result<()> {
        bail!("cannot enable SWV over dump agent");
    }

    fn read_swv(&mut self) -> Result<Vec<u8>> {
        bail!("cannot read SWV over dump agent");
    }

    fn load(&mut self, _path: &Path) -> Result<()> {
        bail!("cannot load flash over dump agent");
    }

    fn reset(&mut self) -> Result<()> {
        bail!("cannot reset over dump agent");
    }

    fn reset_and_halt(&mut self, _dur: std::time::Duration) -> Result<()> {
        bail!("cannot reset over dump agent");
    }

    fn wait_for_halt(&mut self, _dur: std::time::Duration) -> Result<()> {
        bail!("cannot wait for halt over dump agent");
    }
}

////////////////////////////////////////////////////////////////////////////////

/// Trait which abstracts away dump agent interfaces
trait DumpAgent {
    fn core(&mut self) -> &mut dyn Core;

    /// Initializes the dump regions
    fn initialize_dump(&mut self) -> Result<()>;

    /// Initializes a set of segments defined as `(address, length)` tuples
    fn initialize_segments(&mut self, segments: &[(u32, u32)]) -> Result<()>;

    /// Kicks off a dump
    fn take_dump(&mut self) -> Result<()>;

    /// General-purpose dump reading abstraction
    ///
    /// This function is given an iterator over areas, which are represented as
    /// `index, offset` tuples.  It reads from those areas, chunking internally
    /// as needed for efficiency.
    ///
    /// Reading terminates when either the iterator runs out _or_ the `cont`
    /// callback returns `false`; the data for which it returned `false` is not
    /// returned.
    fn read_generic(
        &mut self,
        areas: &mut dyn Iterator<Item = (u8, u32)>,
        cont: &mut dyn FnMut(u8, u32, &[u8]) -> Result<bool>,
    ) -> Result<Vec<Vec<u8>>>;

    //////////////////////////////////////////////////////////////////////////
    // Everything below here is implemented in terms of the functions above, but
    // lives in the trait for convenience.

    fn read_dump_header(&mut self) -> Result<DumpAreaHeader> {
        self.read_dump_header_at(0)
    }

    fn read_dump_header_at(&mut self, i: u8) -> Result<DumpAreaHeader> {
        let val = self
            .read_generic(&mut std::iter::once((i, 0)), &mut |_, _, _| {
                Ok(false)
            })?;
        assert_eq!(val.len(), 0);
        let (header, _task) = parse_dump_header(i as usize, &val[0])?;
        Ok(header)
    }

    fn read_dump_area(
        &mut self,
        index: u8,
        progress: &mut dyn FnMut(usize),
    ) -> Result<(DumpAreaHeader, Vec<u8>)> {
        // Read the header of this dump area
        let val = &self.read_generic(
            &mut std::iter::once((index, 0)),
            &mut |_, _, _| Ok(false),
        )?[0];

        let header = DumpAreaHeader::read_from_prefix(val.as_slice())
            .ok_or_else(|| anyhow!("failed to read parse header"))?;

        if header.magic != humpty::DUMP_MAGIC {
            bail!("bad magic at dump offset {:#x}: {:x?}", index, header);
        }

        let size = size_of::<DumpAreaHeader>();

        let mut out = vec![];
        if header.written > size as u32 {
            let max = val.len().min(header.written as usize);
            let data = val[size..max].to_owned();
            progress(data.len());
            out.push(data);
        }

        let chunk_size = val.len();
        assert_eq!(chunk_size, 256);

        let mut chunks = vec![];
        let mut offset = chunk_size;
        while offset < header.written as usize {
            let len = chunk_size.min(header.written as usize - offset);
            chunks.push((offset.try_into().unwrap(), len.try_into().unwrap()));
            offset += len;
        }

        out.extend(self.read_generic(
            &mut chunks.into_iter(),
            &mut |_index, _offset, result| {
                progress(result.len());
                Ok(true)
            },
        )?);

        Ok((header, out.into_iter().flatten().collect()))
    }

    /// Reads dump headers from the target
    ///
    /// If `raw` is set, includes headers configured as `DUMPER_NONE`;
    /// otherwise, stops once the first `DUMPER_NONE` header is encountered.
    fn read_dump_headers(
        &mut self,
        raw: bool,
    ) -> Result<Vec<(DumpAreaHeader, Option<DumpTask>)>> {
        //
        // Iterate over dump area headers.  Once we've hit an area that hasn't
        // been dumped to, or an invalid area (denoting that all areas are
        // full), we're done...
        //
        let results = self.read_generic(
            &mut (0..).map(|index| (index, 0)),
            &mut |index, _offset, val| {
                let (header, _task) = parse_dump_header(index as usize, val)?;
                if !raw && header.dumper == humpty::DUMPER_NONE {
                    Ok(false)
                } else {
                    Ok(true)
                }
            },
        )?;

        results
            .into_iter()
            .enumerate()
            .map(|(i, r)| parse_dump_header(i, &r))
            .collect::<Result<Vec<_>>>()
    }

    fn read_dump(
        &mut self,
        area: Option<DumpArea>,
        out: &mut AgentCore,
    ) -> Result<Option<DumpTask>> {
        let (base, headers, task) = {
            let all = self.read_dump_headers(false)?;

            let area = match area {
                None => None,
                Some(DumpArea::ByIndex(ndx)) => Some(ndx),
                Some(DumpArea::ByAddress(address)) => all
                    .iter()
                    .enumerate()
                    .filter(|&(_, (header, _))| header.address == address)
                    .map(|(ndx, _)| ndx)
                    .next(),
            };

            match area {
                None | Some(0) if all[0].1.is_none() => (
                    0usize,
                    all.iter()
                        .map(|(h, _t)| *h)
                        .collect::<Vec<DumpAreaHeader>>(),
                    None,
                ),

                Some(ndx) => {
                    let areas = task_areas(&all);
                    match areas.get(&ndx) {
                        None => {
                            bail!("area {ndx} is invalid (--list to list)");
                        }
                        Some((task, headers)) => {
                            (ndx, headers.clone(), Some(*task))
                        }
                    }
                }

                _ => {
                    bail!("area must be explicitly specified (--list to list)");
                }
            }
        };

        let total = headers.iter().fold(0, |sum, header| sum + header.written);

        let started = Instant::now();
        let bar = ProgressBar::new(total as u64);
        bar.set_style(
            ProgressStyle::default_bar()
                .template("humility: pulling [{bar:30}] {bytes}/{total_bytes}"),
        );

        let mut count = 0;
        let mut contents = vec![];
        for (ndx, _) in headers.iter().enumerate() {
            let index = (ndx + base).try_into().unwrap();
            let (_header, data) = self.read_dump_area(index, &mut |size| {
                count += size;
                bar.set_position(size as u64);
            })?;
            contents.extend(data.into_iter());
        }

        bar.finish_and_clear();

        humility::msg!(
            "pulled {} in {}",
            HumanBytes(total as u64),
            HumanDuration(started.elapsed())
        );

        //
        // We have a dump!  On to processing...
        //
        process_dump(&headers[0], &contents, out, task)?;

        Ok(task)
    }
}

////////////////////////////////////////////////////////////////////////////////

/// Represents a dump agent that communicates through the `hiffy` task
///
/// The agent is either local (connected via a debugger) or remote; in the
/// latter case, HIF programs are sent to the `udprpc` task under the hood by
/// the `HiffyContext`, which sees that its core is a `NetCore`.
struct HiffyDumpAgent<'a> {
    hubris: &'a HubrisArchive,
    core: &'a mut dyn Core,
    context: HiffyContext<'a>,
}

impl<'a> HiffyDumpAgent<'a> {
    fn new(
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
    fn run(&mut self, ops: &[Op]) -> Result<Vec<Result<Vec<u8>, u32>>> {
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

        if let Err(err) = &self.run(ops.as_slice())?[0] {
            bail!("failed to initialize dump: {}", op.strerror(*err));
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
            if let Err(err) = result {
                bail!(
                    "failed to add segment at address {:#x} for length {}: {}",
                    *base,
                    *size,
                    op.strerror(*err)
                );
            }
        }

        Ok(())
    }

    fn take_dump(&mut self) -> Result<()> {
        let op = self.hubris.get_idol_command("DumpAgent.take_dump")?;
        let mut ops = vec![];

        //
        // We are about to disappear for -- as the kids say -- a minute.  Set
        // our timeout to be a literal minute so we don't prematurely give up.
        //
        self.core.set_timeout(Duration::new(60, 0))?;

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

        if let Err(err) = results[rindex] {
            bail!("failed to take dump: {}", op.strerror(err));
        }

        Ok(())
    }

    /// This is the tricky one
    fn read_generic(
        &mut self,
        areas: &mut dyn Iterator<Item = (u8, u32)>,
        cont: &mut dyn FnMut(u8, u32, &[u8]) -> Result<bool>,
    ) -> Result<Vec<Vec<u8>>> {
        // Because HIF has overhead, we're going to process a chunk of multiple
        // read_dump calls in a single HIF program.  The size depends on our
        // return data size.
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

            // Check the results.
            //
            // In some cases, this is used to process entire
            let results = self.run(&ops)?;
            for (r, (index, offset)) in results.iter().zip(pos.into_iter()) {
                match r {
                    Ok(val) => {
                        if !cont(index, offset, val)? {
                            return Ok(rval);
                        }
                        rval.push(val.to_vec());
                    }
                    Err(err) => {
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
                }
            }
        }

        Ok(rval)
    }
}

////////////////////////////////////////////////////////////////////////////////

struct UdpDumpAgent<'a> {
    core: &'a mut dyn Core,
}

impl<'a> UdpDumpAgent<'a> {
    /// Sends a remote dump command over the network
    fn dump_remote_action(
        &mut self,
        msg: udp::Request,
    ) -> Result<Result<udp::Response, udp::Error>> {
        use udp::{Header, Request, Response};
        let mut rng = rand::thread_rng();
        let header = Header { version: udp::VERSION, message_id: rng.gen() };
        let mut buf = vec![0u8; std::mem::size_of::<(Header, Request)>()];
        let size = hubpack::serialize(&mut buf, &(header, msg))
            .context("failed to serialize message")?;

        // Send the packet out
        self.core
            .send(&buf[..size], NetAgent::DumpAgent)
            .context("failed to send packet")?;

        // Try to receive a reply
        let size = self
            .core
            .recv(buf.as_mut_slice(), NetAgent::DumpAgent)
            .context("failed to receive packet")?;

        let ((rheader, reply), _): ((Header, Result<Response, udp::Error>), _) =
            hubpack::deserialize(&buf[..size])
                .map_err(|_| anyhow!("deserialization failed"))?;
        if rheader.message_id != header.message_id {
            bail!(
                "message ID mismatch: {} != {}",
                rheader.message_id,
                header.message_id
            );
        }
        Ok(reply)
    }
}

impl<'a> DumpAgent for UdpDumpAgent<'a> {
    fn read_generic(
        &mut self,
        areas: &mut dyn Iterator<Item = (u8, u32)>,
        cont: &mut dyn FnMut(u8, u32, &[u8]) -> Result<bool>,
    ) -> Result<Vec<Vec<u8>>> {
        let mut out = vec![];
        for (index, offset) in areas {
            let r = self
                .dump_remote_action(udp::Request::ReadDump { index, offset })?;
            match r {
                Ok(udp::Response::ReadDump(d)) => {
                    if !cont(index, offset, &d)? {
                        break;
                    } else {
                        out.push(d.to_vec())
                    }
                }
                Err(udp::Error::InvalidArea) => break,
                _ => bail!("invalid reply: {r:?}"),
            }
        }
        Ok(out)
    }

    fn core(&mut self) -> &mut dyn Core {
        self.core
    }

    fn initialize_dump(&mut self) -> Result<()> {
        let r = self.dump_remote_action(udp::Request::InitializeDump)?;
        match r {
            Ok(udp::Response::InitializeDump) => Ok(()),
            _ => bail!("invalid response: {r:?}"),
        }
    }

    fn initialize_segments(&mut self, segments: &[(u32, u32)]) -> Result<()> {
        for &(address, length) in segments {
            let r = self.dump_remote_action(udp::Request::AddDumpSegment {
                address,
                length,
            })?;
            match r {
                Ok(udp::Response::AddDumpSegment) => (),
                _ => bail!("invalid response: {r:?}"),
            }
        }
        Ok(())
    }

    fn take_dump(&mut self) -> Result<()> {
        let r = self.dump_remote_action(udp::Request::TakeDump)?;
        match r {
            Ok(udp::Response::TakeDump) => Ok(()),
            _ => bail!("invalid response: {r:?}"),
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

fn emulate_dump(
    core: &mut dyn Core,
    task: Option<DumpTask>,
    base: u32,
    total: u32,
) -> Result<()> {
    let shared = RefCell::new(core);
    let started = Instant::now();
    let bar = ProgressBar::new(total as u64);
    let mut nread = 0;
    let mut nwritten = 0;

    bar.set_style(ProgressStyle::default_bar().template(
        "humility: dumping in situ [{bar:30}] {bytes}/{total_bytes}",
    ));

    let mut rnum = 0;

    let r = humpty::dump::<anyhow::Error, 2048, { humpty::DUMPER_EMULATED }>(
        base,
        task,
        || {
            let start = rnum;

            for i in start..=31 {
                if let Some(reg) = ARMRegister::from_u16(i) {
                    let val = shared.borrow_mut().read_reg(reg)?;
                    rnum = i + 1;
                    return Ok(Some(humpty::RegisterRead(i, val)));
                }
            }
            Ok(None)
        },
        |addr, buf, _meta| {
            nread += buf.len();
            bar.set_position(nread as u64);
            shared.borrow_mut().read_8(addr, buf)
        },
        |addr, buf| {
            nwritten += buf.len();
            shared.borrow_mut().write_8(addr, buf)
        },
    );

    bar.finish_and_clear();

    humility::msg!(
        "dumped {} in situ (compressed to {}) in {}",
        HumanBytes(total as u64),
        HumanBytes(nwritten as u64),
        HumanDuration(started.elapsed())
    );

    if let Err(e) = r {
        bail!("dump failed: {:x?}", e);
    }

    Ok(())
}

fn emulate_task_dump_prep(
    core: &mut dyn Core,
    segments: &Vec<(u32, u32)>,
    base: u32,
) -> Result<u32> {
    let shared = RefCell::new(core);

    let area = match humpty::claim_dump_area::<anyhow::Error>(
        base,
        humpty::DumpContents::SingleTask,
        |addr, buf, _meta| shared.borrow_mut().read_8(addr, buf),
        |addr, buf| shared.borrow_mut().write_8(addr, buf),
    ) {
        Ok(area) => area,
        Err(e) => {
            bail!("dump area allocation failed: {:x?}", e);
        }
    };

    let area = match area {
        Some(area) => area,
        None => {
            bail!("no dump area is available");
        }
    };

    let mut total = 0;

    for (base, size) in segments {
        total += size;

        if let Err(e) = humpty::add_dump_segment_header::<anyhow::Error>(
            area.address,
            *base,
            *size,
            |addr, buf, _meta| shared.borrow_mut().read_8(addr, buf),
            |addr, buf| shared.borrow_mut().write_8(addr, buf),
        ) {
            bail!("adding segment at {base:#x} (length {size}) failed: {e:x?}");
        }
    }

    Ok(area.address)
}

fn parse_dump_header(
    area: usize,
    buf: &[u8],
) -> Result<(DumpAreaHeader, Option<DumpTask>)> {
    let header = DumpAreaHeader::read_from_prefix(buf)
        .ok_or_else(|| anyhow!("failed to parse dump area {area}"))?;

    if header.magic != humpty::DUMP_MAGIC {
        bail!("bad magic at in dump area {area}: {header:x?}");
    }

    //
    // This is a little sleazy (or maybe even a lot sleazy?):  we know that if
    // we have task dump here, the number of segments is sufficiently low to
    // assure that we will have also slurped our task information -- and we
    // know that our task information will immediately follow our segment
    // headers.  (We take some of the sting off of this by at least checking
    // the sizes when we read the dump headers -- but we still rely on the
    // fact that any task information is dumped first.)  Gone fishin'...
    //
    let size = size_of::<DumpAreaHeader>();
    let toffs =
        size + header.nsegments as usize * size_of::<DumpSegmentHeader>();

    let task = if toffs < buf.len() {
        if let Some(DumpSegment::Task(t)) = DumpSegment::from(&buf[toffs..]) {
            Some(t)
        } else {
            None
        }
    } else {
        None
    };

    Ok((header, task))
}

//
// Because single task dumps can spread over adjacent areas, they can be a
// little tedious to process; iterate over all dump areas and return a map of
// area indices to a task/vector of headers tuple.
//
fn task_areas(
    headers: &[(DumpAreaHeader, Option<DumpTask>)],
) -> IndexMap<usize, (DumpTask, Vec<DumpAreaHeader>)> {
    let mut rval = IndexMap::new();

    if headers[0].1.is_none() {
        return rval;
    }

    let mut current = 0;
    let mut areas = vec![headers[0].0];

    for (ndx, (header, task)) in headers.iter().enumerate().skip(1) {
        match task {
            None => {
                //
                // If the header indicates that it's been written, it's
                // a continuation of our previous header -- otherwise, we're
                // done processing entirely.
                //
                if header.dumper != humpty::DUMPER_NONE {
                    areas.push(*header);
                } else {
                    break;
                }
            }
            Some(_task) => {
                //
                // We have hit a new task dump, so record what we have seen
                // for our current task dump, and start a new one.
                //
                rval.insert(current, (headers[current].1.unwrap(), areas));
                current = ndx;
                areas = vec![*header];
            }
        }
    }

    rval.insert(current, (headers[current].1.unwrap(), areas));
    rval
}

fn process_dump(
    header: &DumpAreaHeader,
    dump: &Vec<u8>,
    agent: &mut AgentCore,
    task: Option<DumpTask>,
) -> Result<()> {
    let nsegments = header.nsegments;
    let mut offset = nsegments as usize * size_of::<DumpSegmentHeader>();

    if offset > dump.len() {
        bail!("in situ dump is short; missing {nsegments} segments");
    }

    if offset == dump.len() {
        bail!("in situ dump is empty");
    }

    while offset < dump.len() {
        let segment = match DumpSegment::from(&dump[offset..]) {
            Some(segment) => segment,
            None => {
                bail!("short read at offset {offset}");
            }
        };

        match segment {
            DumpSegment::Task(t) => {
                match task {
                    None => {
                        bail!("found unexpected task {t:?}");
                    }
                    Some(task) if task != t => {
                        bail!("task mismatch: found {t:?}, expected {task:?}");
                    }
                    _ => {}
                }

                offset += size_of::<DumpTask>();
                continue;
            }

            DumpSegment::Register(reg) => {
                //
                // These are register values; slurp them and continue.
                //
                if let Some(register) = ARMRegister::from_u16(reg.register) {
                    agent.add_register(register, reg.value);
                } else {
                    let r = reg.register;
                    bail!("unrecognized register {r:#x} at offset {offset}");
                }

                offset += size_of::<DumpRegister>();
                continue;
            }

            DumpSegment::Data(data) => {
                offset += size_of::<DumpSegmentData>();

                let len = data.uncompressed_length as usize;

                let mut contents = vec![0; len];
                let limit = offset + data.compressed_length as usize;

                humpty::DumpLzss::decompress(
                    lzss::SliceReader::new(&dump[offset..limit]),
                    lzss::SliceWriter::new(&mut contents),
                )?;

                agent.add_ram_region(data.address, contents[0..len].to_vec());
                offset = limit;

                while offset < dump.len()
                    && dump[offset] == humpty::DUMP_SEGMENT_PAD
                {
                    offset += 1;
                }
            }

            DumpSegment::Unknown(signature) => {
                bail!(
                    "unrecognized data with signature \
                    {signature:x?} at offset {offset}"
                );
            }
        }
    }

    Ok(())
}

enum DumpArea {
    ByIndex(usize),
    ByAddress(u32),
}

fn get_dump_agent<'a>(
    hubris: &'a HubrisArchive,
    core: &'a mut dyn Core,
    subargs: &DumpArgs,
) -> Result<Box<dyn DumpAgent + 'a>> {
    if core.is_net()
        && hubris
            .manifest
            .task_features
            .get("dump-agent")
            .map(|f| f.contains(&"net".to_string()))
            .unwrap_or(false)
    {
        humility::msg!("making UDP dump agent");
        Ok(Box::new(UdpDumpAgent { core }))
    } else {
        humility::msg!("making hiffy dump agent");
        Ok(Box::new(HiffyDumpAgent::new(hubris, core, subargs.timeout)?))
    }
}

fn dump_via_agent(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    subargs: &DumpArgs,
) -> Result<()> {
    let mut out = AgentCore::new(hubris)?;
    let started = Some(Instant::now());
    let mut area = subargs.area.map(DumpArea::ByIndex);

    //
    // Our task can come from a couple of different spots:  we can either
    // be explicitly told our task (in which case we are simulated or
    // emulated) or we are pulling it out of an area.  If the latter, we
    // will discover the task when we actually read our dump headers, so
    // leave it as None for now...
    //
    let mut task = match &subargs.task {
        Some(task) => {
            let ndx = match hubris.lookup_task(task) {
                Some(HubrisTask::Task(ndx)) => ndx,
                _ => {
                    bail!("invalid task \"{task}\"");
                }
            };

            Some(DumpTask::new(*ndx as u16, hubris.ticks(core)?))
        }
        None => None,
    };

    if subargs.simulate_dumper {
        //
        // We are being asked to simulate our dumper:  we are going to pull
        // our dynamic memory directly from our target -- and determine what
        // our compression ratio would be along the way.
        //
        core.halt()?;
        humility::msg!("core halted");

        if let Some(ref stock) = subargs.stock_dumpfile {
            hubris.dump(core, task, Some(stock), None)?;
        }

        match task {
            Some(task) => {
                if hubris.current_task(core)?
                    == Some(HubrisTask::Task(task.id as u32))
                {
                    core.run()?;
                    bail!("cannot dump a task while it is running");
                }
            }
            None => {
                for i in 0..=ARMRegister::max() {
                    if let Some(reg) = ARMRegister::from_u16(i) {
                        let val = core.read_reg(reg)?;
                        out.add_register(reg, val);
                    }
                }
            }
        }

        let segments = hubris.dump_segments(core, task, false)?;
        let total = segments.iter().fold(0, |ttl, (_, size)| ttl + size);

        let started = Instant::now();

        let bar = ProgressBar::new(total as u64);
        bar.set_style(
            ProgressStyle::default_bar()
                .template("humility: reading [{bar:30}] {bytes}/{total_bytes}"),
        );

        let mut nread = 0;
        let mut ncompressed = 0;

        for (base, size) in &segments {
            let mut remain = *size as usize;
            let mut bytes = vec![0; 1024];
            let input_len = (bytes.len() / 2) - (bytes.len() / 8);

            let mut addr = *base;

            while remain > 0 {
                let nbytes = core::cmp::min(remain, input_len);
                let offs = bytes.len() - nbytes;
                let len = bytes.len();

                core.read_8(addr, &mut bytes[offs..len])?;

                let mut output = vec![0; 2048];
                let mut compare: Vec<u8> = vec![];

                compare.extend(&bytes[offs..len]);

                let (compressed, rval) =
                    humpty::DumpLzss::compress_in_place(&mut bytes, offs);

                if let Some(overflow) = rval {
                    bail!(
                        "compresion overflow at address {:#x} by {} bytes",
                        addr,
                        overflow,
                    );
                }

                humpty::DumpLzss::decompress(
                    lzss::SliceReader::new(&bytes[0..compressed]),
                    lzss::SliceWriter::new(&mut output),
                )?;

                for i in 0..nbytes {
                    if compare[i] != output[i] {
                        bail!("compression/decompression mismatch!");
                    }
                }

                ncompressed += compressed;

                out.add_ram_region(addr, compare);

                remain -= nbytes;
                nread += nbytes;
                addr += nbytes as u32;
                bar.set_position(nread as u64);
            }
        }

        bar.finish_and_clear();

        humility::msg!(
            "read {} (compressing to {}) in {}",
            HumanBytes(nread as u64),
            HumanBytes(ncompressed as u64),
            HumanDuration(started.elapsed())
        );

        core.run()?;
        humility::msg!("core resumed");
    } else {
        let segments = hubris.dump_segments(core, None, false)?;
        let mut agent = get_dump_agent(hubris, core, subargs)?;
        let header = agent.read_dump_header()?;

        if !subargs.force_read && subargs.area.is_none() {
            if header.dumper != humpty::DUMPER_NONE
                && !subargs.initialize_dump_agent
                && !subargs.force_overwrite
                && task.is_none()
            {
                bail!(
                    "there appears to already be one or more dumps in situ; \
                    list them with --list, clear them with \
                    --initialize-dump-agent, or force them to be overwritten \
                    with --force-overwrite"
                )
            }

            if task.is_none() || subargs.initialize_dump_agent {
                humility::msg!("initializing dump agent state");
                agent.initialize_dump()?;
            }

            if subargs.initialize_dump_agent {
                return Ok(());
            }

            if task.is_none() {
                humility::msg!("initializing segments");
                agent.initialize_segments(&segments)?;
            }
        }

        if subargs.emulate_dumper {
            agent.core().halt()?;
            humility::msg!("core halted");

            if let Some(ref stock) = subargs.stock_dumpfile {
                hubris.dump(agent.core(), task, Some(stock), None)?;
            }

            let base = header.address;
            let total = segments.iter().fold(0, |ttl, (_, size)| ttl + size);

            let address = if task.is_some() {
                match emulate_task_dump_prep(agent.core(), &segments, base) {
                    Err(e) => {
                        agent.core().run()?;
                        humility::msg!("core resumed after failure");
                        return Err(e);
                    }
                    Ok(address) => {
                        assert!(area.is_none());
                        area = Some(DumpArea::ByAddress(address));
                        address
                    }
                }
            } else {
                base
            };

            emulate_dump(agent.core(), task, address, total)?;
            agent.core().run()?;
            humility::msg!("core resumed");
        } else if !subargs.force_read && subargs.area.is_none() {
            if subargs.force_manual_initiation {
                agent.core().halt()?;
                humility::msg!("leaving core halted");
                let base = header.address;
                humility::msg!(
                    "unplug probe and manually \
                    initiate dump from address {:#x}",
                    base
                );
                humility::msg!(
                    "e.g., \"humility hiffy --call \
                    Dumper.dump -a address={:#x}\"",
                    base
                );
                return Ok(());
            }

            //
            // Tell the thing to take a dump
            //
            agent.take_dump()?;
        }

        //
        // If we're here, we have a dump in situ -- time to pull it.
        //
        task = agent.read_dump(area, &mut out)?;

        //
        // If this was a whole-system dump, we will leave our state initialized
        // to assure that it will be ready to take subsequent task dumps (unless
        // explicitly asked not to).
        //
        if task.is_none() {
            if !subargs.retain_state {
                humility::msg!("resetting dump agent state");
                agent.initialize_dump()?;
            } else {
                humility::msg!("retaining dump agent state");
            }
        }
    }

    hubris.dump(&mut out, task, subargs.dumpfile.as_deref(), started)?;

    Ok(())
}

fn dump_list(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    subargs: &DumpArgs,
) -> Result<()> {
    println!("{:4} {:21} {:10} SIZE", "AREA", "TASK", "TIME");

    let mut agent = get_dump_agent(hubris, core, subargs)?;
    let headers = agent.read_dump_headers(false)?;

    if headers.is_empty() || headers[0].0.dumper == humpty::DUMPER_NONE {
        return Ok(());
    }

    if headers[0].1.is_none() {
        let size = headers
            .iter()
            .filter(|&(h, _)| h.dumper != humpty::DUMPER_NONE)
            .fold(0, |ttl, (h, _)| ttl + h.written);

        println!("{:>4} {:21} {:<10} {size}", 0, "<system>", "-");
        return Ok(());
    }

    let areas = task_areas(&headers);

    for (area, (task, headers)) in &areas {
        let size = headers.iter().fold(0, |ttl, h| ttl + h.written);

        println!(
            "{area:>4} {:21} {:<10} {size}",
            match hubris.lookup_module(HubrisTask::Task(task.id.into())) {
                Ok(module) => &module.name,
                _ => "<unknown>",
            },
            task.time,
        );
    }

    Ok(())
}

fn dump_agent_status(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    subargs: &DumpArgs,
) -> Result<()> {
    let mut agent = get_dump_agent(hubris, core, subargs)?;
    let headers = agent.read_dump_headers(true)?;
    humility::msg!("{:#x?}", headers);

    Ok(())
}

fn dumpcmd(context: &mut humility::ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let hubris = context.archive.as_ref().unwrap();

    let subargs = DumpArgs::try_parse_from(subargs)?;

    if subargs.force_dump_agent && core.is_net() {
        bail!("can only force the dump agent when attached via debug probe");
    }

    if subargs.list {
        dump_list(hubris, core, &subargs)
    } else if subargs.dump_agent_status {
        dump_agent_status(hubris, core, &subargs)
    } else if core.is_net()
        || subargs.force_dump_agent
        || subargs.force_read
        || subargs.area.is_some()
    {
        dump_via_agent(hubris, core, &subargs)
    } else {
        if subargs.initialize_dump_agent {
            bail!("must also use --force-dump-agent to initialize dump agent");
        }

        core.halt()?;
        humility::msg!("core halted");

        let rval = hubris.dump(core, None, subargs.dumpfile.as_deref(), None);

        if !subargs.leave_halted {
            core.run()?;
            humility::msg!("core resumed");
        } else {
            humility::msg!("core left halted");
        }

        rval
    }
}

pub fn init() -> Command {
    Command {
        app: DumpArgs::command(),
        name: "dump",
        run: dumpcmd,
        kind: CommandKind::Attached {
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Match,
        },
    }
}
