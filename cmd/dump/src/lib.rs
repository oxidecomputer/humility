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

use anyhow::{anyhow, bail, Result};
use clap::{ArgGroup, CommandFactory, Parser};
use core::mem::size_of;
use hif::*;
use humility::arch::ARMRegister;
use humility::cli::Subcommand;
use humility::core::Core;
use humility::hubris::*;
use humility_cmd::hiffy::*;
use humility_cmd::idol::{self, HubrisIdol};
use humility_cmd::{Archive, Attach, Command, CommandKind, Validate};
use humpty::*;
use indicatif::{HumanBytes, HumanDuration};
use indicatif::{ProgressBar, ProgressStyle};
use num_traits::FromPrimitive;
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

    #[clap(
        long,
        requires = "emulate-dumper",
        conflicts_with = "stock-dumpfile"
    )]
    task: Option<String>,

    #[clap(short, long, conflicts_with_all = &[
        "task", "simulation", "list"
    ])]
    area: Option<usize>,

    /// leave the target halted
    #[clap(long, conflicts_with = "simulation")]
    leave_halted: bool,

    #[clap(long, short, conflicts_with = "task")]
    list: bool,

    dumpfile: Option<String>,
}

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

    fn read_word_32(&mut self, addr: u32) -> Result<u32> {
        bail!("unexpected call to read 32-bit value at 0x{:x}", addr);
    }

    fn read_8(&mut self, addr: u32, data: &mut [u8]) -> Result<()> {
        self.read(addr, data)
    }

    fn read_reg(&mut self, reg: ARMRegister) -> Result<u32> {
        match self.registers.get(&reg) {
            Some(val) => Ok(*val),
            None => bail!("unknown register {}", reg),
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

fn initialize_dump(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
    funcs: &HiffyFunctions,
) -> Result<()> {
    let op = hubris.get_idol_command("DumpAgent.initialize_dump")?;
    let mut ops = vec![];
    context.idol_call_ops(funcs, &op, &[], &mut ops)?;
    ops.push(Op::Done);

    if let Err(err) = &context.run(core, ops.as_slice(), None)?[0] {
        bail!("failed to initialize dump: {}", op.strerror(*err));
    }

    Ok(())
}

fn initialize_segments(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
    funcs: &HiffyFunctions,
    segments: &Vec<(u32, u32)>,
) -> Result<()> {
    let op = hubris.get_idol_command("DumpAgent.add_dump_segment")?;
    let mut ops = vec![];

    for (base, size) in segments {
        let payload = op.payload(&[
            ("address", idol::IdolArgument::Scalar(*base as u64)),
            ("length", idol::IdolArgument::Scalar(*size as u64)),
        ])?;

        context.idol_call_ops(funcs, &op, &payload, &mut ops)?;
    }

    ops.push(Op::Done);

    let results = context.run(core, ops.as_slice(), None)?;

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

fn emulate_dump(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    task: Option<u16>,
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

    let task = match task {
        Some(t) => Some(DumpTask::new(
            t,
            shared
                .borrow_mut()
                .read_word_64(hubris.lookup_variable("TICKS")?.addr)?,
        )),
        None => None,
    };

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
        |addr, buf| {
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

fn task_dump_segments(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    regions: &BTreeMap<u32, HubrisRegion>,
    task: &str,
) -> Result<(u16, Vec<(u32, u32)>)> {
    let mut rval = vec![];
    let (base, _) = hubris.task_table(core)?;

    let t = match hubris.lookup_task(task) {
        Some(t) => t,
        None => {
            bail!("unknown task \"{}\"", task);
        }
    };

    let task_t = hubris.lookup_struct_byname("Task")?;

    let ndx = match t {
        HubrisTask::Task(ndx) => ndx,
        _ => bail!("invalid task \"{:?}\"", t),
    };

    rval.push((base + (*ndx * task_t.size as u32), task_t.size as u32));

    for v in regions.values() {
        if v.tasks.contains(t) {
            rval.push((v.base, v.size));
        }
    }

    Ok((*ndx as u16, rval))
}

fn emulate_task_dump(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    segments: &Vec<(u32, u32)>,
    base: u32,
) -> Result<u32> {
    let shared = RefCell::new(core);

    let area = match humpty::claim_dump_area::<anyhow::Error>(
        base,
        humpty::DumpAgent::Jefe,
        false,
        |addr, buf| shared.borrow_mut().read_8(addr, buf),
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

        if let Err(e) = humpty::add_dump_segment::<anyhow::Error>(
            area.address,
            *base,
            *size,
            |addr, buf| shared.borrow_mut().read_8(addr, buf),
            |addr, buf| shared.borrow_mut().write_8(addr, buf),
        ) {
            bail!("adding segment at {base:#x} (length {size}) failed: {e:x?}");
        }
    }

    Ok(area.address)
}

fn take_dump(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
    funcs: &HiffyFunctions,
) -> Result<()> {
    let op = hubris.get_idol_command("DumpAgent.take_dump")?;
    let mut ops = vec![];

    //
    // We are about to disappear for -- as the kids say -- a minute.  Set
    // our timeout to be a literal minute so we don't prematurely give up.
    //
    core.set_timeout(Duration::new(60, 0))?;

    let rindex = if !core.is_net() {
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
        humility::msg!("dump will start in 10 seconds; unplug probe now");

        let sleep = funcs.get("Sleep", 1)?;
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
        humility::msg!("taking dump; target will be stopped for ~20 seconds");
        0
    };

    context.idol_call_ops(funcs, &op, &[], &mut ops)?;
    ops.push(Op::Done);

    let results = context.run(core, ops.as_slice(), None)?;

    if let Err(err) = results[rindex] {
        bail!("failed to take dump: {}", op.strerror(err));
    }

    Ok(())
}

fn read_dump_at(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
    funcs: &HiffyFunctions,
    index: usize,
    mut progress: impl FnMut(&[u8]) -> bool,
) -> Result<(DumpAreaHeader, Option<DumpTask>)> {
    //
    // We expect a DumpAreaHeader to be at the specified offset.
    //
    let op = hubris.get_idol_command("DumpAgent.read_dump")?;
    let mut ops = vec![];
    let mut offset = 0;

    let payload = op.payload(&[
        ("index", idol::IdolArgument::Scalar(index as u64)),
        ("offset", idol::IdolArgument::Scalar(offset as u64)),
    ])?;

    context.idol_call_ops(funcs, &op, &payload, &mut ops)?;
    ops.push(Op::Done);

    match &context.run(core, ops.as_slice(), None)?[0] {
        Ok(val) => {
            let header = DumpAreaHeader::read_from_prefix(val.as_slice())
                .ok_or_else(|| {
                    anyhow!("failed to read dump at offset {:#x}", offset)
                })?;

            if header.magic != humpty::DUMP_MAGIC {
                bail!("bad magic at dump offset {:#x}: {:x?}", offset, header);
            }

            //
            // This is a little sleazy:  we know that if we have task dump
            // here, the number of segments is sufficiently low to assure that
            // we will have also slurped our task information.
            //
            let size = size_of::<DumpAreaHeader>();
            let toffs = size
                + header.nsegments as usize * size_of::<DumpSegmentHeader>();

            let task = if toffs < val.len() {
                if let Some(DumpSegment::Task(task)) =
                    DumpSegment::from(&val[toffs..])
                {
                    Some(task)
                } else {
                    None
                }
            } else {
                None
            };

            if header.written > size as u32 {
                let max = std::cmp::min(header.written as usize, val.len());

                if !progress(&val[size..max]) {
                    return Ok((header, task));
                }
            }

            let mut all_ops = vec![];
            let mut offsets = vec![];
            offset = val.len() as u32;

            while offset < header.written {
                let mut ops = vec![];

                let payload = op.payload(&[
                    ("index", idol::IdolArgument::Scalar(index as u64)),
                    ("offset", idol::IdolArgument::Scalar(offset as u64)),
                ])?;

                let len = std::cmp::min(
                    val.len(),
                    (header.written - offset) as usize,
                );

                offsets.push((offset, len));

                context.idol_call_ops(funcs, &op, &payload, &mut ops)?;
                offset += val.len() as u32;
                all_ops.push(ops);
            }

            //
            // We have all of our operations.  We're going to chunk it out
            // in batches to make this somewhat practical when emulated.
            //
            let chunksize = (context.rdata_size() / val.len()) - 1;

            for (ndx, o) in all_ops.chunks(chunksize).enumerate() {
                let mut chunk: Vec<Op> = vec![];

                for ops in o {
                    chunk.extend(ops)
                }

                chunk.push(Op::Done);

                let results = context.run(core, chunk.as_slice(), None)?;

                for (rndx, r) in results.iter().enumerate() {
                    let (offset, size) = offsets[ndx * chunksize + rndx];

                    match r {
                        Ok(val) => {
                            if !progress(&val[..size]) {
                                return Ok((header, task));
                            }
                        }

                        Err(err) => {
                            bail!(
                                "failed to read dump at offset {}: {}",
                                offset,
                                op.strerror(*err)
                            );
                        }
                    }
                }
            }

            Ok((header, task))
        }

        Err(err) => {
            bail!("{:?}", err);
        }
    }
}

fn read_dump_header(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
    funcs: &HiffyFunctions,
) -> Result<(DumpAreaHeader, Option<DumpTask>)> {
    read_dump_at(hubris, core, context, funcs, 0, |_| false)
}

fn read_dump_headers(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
    funcs: &HiffyFunctions,
) -> Result<Vec<(DumpAreaHeader, Option<DumpTask>)>> {
    let mut rval = vec![];
    let mut done = false;
    let mut ndx = 0;

    while !done {
        let header =
            read_dump_at(hubris, core, context, funcs, ndx, |_| false)?;

        if header.0.next != 0 {
            ndx += 1;
        } else {
            done = true;
        }

        rval.push(header);
    }

    Ok(rval)
}

//
// Because single task dumps can spread over adjacent areas, they can be a
// little tedious to process; iterate over all dump areas and return a map of
// area indices to a task/vector of headers tuple.
//
fn task_areas(
    headers: &Vec<(DumpAreaHeader, Option<DumpTask>)>,
) -> HashMap<usize, (DumpTask, Vec<DumpAreaHeader>)> {
    let mut rval = HashMap::new();

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
            Some(t) => {
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
            DumpSegment::Task(task) => {
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

fn read_dump(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
    funcs: &HiffyFunctions,
    agent: &mut AgentCore,
    area: Option<DumpArea>,
) -> Result<()> {
    let headers = read_dump_headers(hubris, core, context, funcs)?;
    let mut contents: Vec<u8> = vec![];

    let (base, headers, task) = {
        let all = read_dump_headers(hubris, core, context, funcs)?;

        let area = match area {
            None => None,
            Some(DumpArea::ByIndex(ndx)) => Some(ndx),
            Some(DumpArea::ByAddress(address)) => all
                .iter()
                .enumerate()
                .filter(|&(ndx, (header, _))| header.address == address)
                .map(|(ndx, _)| ndx)
                .next(),
        };

        match area {
            None => {
                if all[0].1.is_some() {
                    bail!("area must be explicitly specified (--list to list)");
                }
                (
                    0usize,
                    all.iter()
                        .map(|(h, _t)| *h)
                        .collect::<Vec<DumpAreaHeader>>(),
                    None,
                )
            }

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
        }
    };

    let total = headers.iter().fold(0, |sum, header| sum + header.written);

    let started = Instant::now();
    let bar = ProgressBar::new(total as u64);
    bar.set_style(
        ProgressStyle::default_bar()
            .template("humility: pulling [{bar:30}] {bytes}/{total_bytes}"),
    );

    for (ndx, _) in headers.iter().enumerate() {
        read_dump_at(hubris, core, context, funcs, ndx + base, |rval| {
            contents.extend(rval);
            bar.set_position(contents.len() as u64);
            true
        })?;
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
    process_dump(&headers[0], &contents, agent)
}

fn dump_via_agent(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    subargs: &DumpArgs,
) -> Result<()> {
    let mut agent = AgentCore::new(hubris)?;
    let regions = hubris.regions(core)?;
    let started = Some(Instant::now());
    let mut area = match subargs.area {
        Some(ndx) => Some(DumpArea::ByIndex(ndx)),
        None => None,
    };

    let (task, mut segments) = if let Some(task) = &subargs.task {
        let r = task_dump_segments(hubris, core, &regions, task)?;
        (Some(r.0), r.1)
    } else {
        (
            None,
            regions
                .values()
                .filter(|&r| !r.attr.device && r.attr.write)
                .map(|r| (r.base, r.size))
                .collect::<Vec<_>>(),
        )
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
            hubris.dump(core, &segments, Some(stock), None)?;
        }

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

                agent.add_ram_region(addr, bytes[0..nbytes].to_vec().clone());

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
        let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;
        let funcs = context.functions()?;
        let header = read_dump_header(hubris, core, &mut context, &funcs)?.0;

        if !subargs.force_read {
            if header.dumper != humpty::DUMPER_NONE
                && !subargs.initialize_dump_agent
                && !subargs.force_overwrite
                && task.is_none()
            {
                bail!(
                    "there appears to already be a dump in situ; \
                        clear with --initialize-dump-agent or force dump \
                        to be overwritten with --force-overwrite"
                )
            }

            if task.is_none() || subargs.initialize_dump_agent {
                humility::msg!("initializing dump agent state");
                initialize_dump(hubris, core, &mut context, &funcs)?;
            }

            if subargs.initialize_dump_agent {
                return Ok(());
            }

            if task.is_none() {
                humility::msg!("initializing segments");
                initialize_segments(
                    hubris,
                    core,
                    &mut context,
                    &funcs,
                    &segments,
                )?;
            }
        }

        if subargs.emulate_dumper {
            core.halt()?;
            humility::msg!("core halted");

            if let Some(ref stock) = subargs.stock_dumpfile {
                hubris.dump(core, &segments, Some(stock), None)?;
            }

            let base = header.address;

            let total = segments.iter().fold(0, |ttl, (_, size)| ttl + size);

            if let Some(task) = task {
                match emulate_task_dump(hubris, core, &segments, base) {
                    Err(e) => {
                        core.run()?;
                        humility::msg!("core resumed after failure");
                        return Err(e);
                    }
                    Ok(address) => {
                        emulate_dump(hubris, core, Some(task), address, total)?;
                        assert!(area.is_none());
                        area = Some(DumpArea::ByAddress(address));
                    }
                }
            } else {
                emulate_dump(hubris, core, None, base, total)?;
            }

            core.run()?;
            humility::msg!("core resumed");
        } else if !subargs.force_read {
            if subargs.force_manual_initiation {
                core.halt()?;
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
            take_dump(hubris, core, &mut context, &funcs)?;
        }

        //
        // If we're here, we have a dump in situ -- time to pull it.
        //
        read_dump(hubris, core, &mut context, &funcs, &mut agent, area)?;
    }

    //
    // Now add all non-writable regions.  Even if we are dumping a single
    // task, we will dump everything we can.
    //
    for r in regions.values() {
        if !r.attr.device && !r.attr.write {
            segments.push((r.base, r.size));
        }
    }

    hubris.dump(&mut agent, &segments, subargs.dumpfile.as_deref(), started)
}

fn dump_list(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    subargs: &DumpArgs,
) -> Result<()> {
    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;
    let funcs = context.functions()?;

    let headers = read_dump_headers(hubris, core, &mut context, &funcs)?;

    println!("{:4} {:21} {}", "AREA", "TASK", "TIME");

    for (area, (header, task)) in headers.iter().enumerate() {
        match task {
            None => {
                if header.dumper != humpty::DUMPER_NONE {
                    println!("{:>4} {:21}", area, "<system>");
                    return Ok(());
                }
            }
            Some(t) => {
                println!(
                    "{:>4} {:21} {}",
                    area,
                    match hubris.lookup_module(HubrisTask::Task(t.id.into())) {
                        Ok(module) => &module.name,
                        _ => "<unknown>",
                    },
                    t.time,
                );
            }
        }
    }

    Ok(())
}

fn dump_agent_status(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    subargs: &DumpArgs,
) -> Result<()> {
    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;
    let funcs = context.functions()?;

    let headers = read_dump_headers(hubris, core, &mut context, &funcs)?;
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
    } else if core.is_net() || subargs.force_dump_agent || subargs.force_read {
        dump_via_agent(hubris, core, &subargs)
    } else {
        if subargs.initialize_dump_agent {
            bail!("must also use --force-dump-agent to initialize dump agent");
        }

        core.halt()?;
        humility::msg!("core halted");

        let regions = hubris
            .regions(core)?
            .values()
            .filter(|&r| !r.attr.device)
            .map(|r| (r.base, r.size))
            .collect::<Vec<_>>();

        let rval =
            hubris.dump(core, &regions, subargs.dumpfile.as_deref(), None);

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
