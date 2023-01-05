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
//! ID ADDR     TASK               GEN STATE
//!  0 20000168 jefe                 0 Healthy(InRecv(None))
//!  1 200001d8 rcc_driver           0 Healthy(InRecv(None))
//!  2 20000248 gpio_driver          0 Healthy(InRecv(None))
//!  3 200002b8 usart_driver         0 Healthy(InRecv(None))
//!  4 20000328 i2c_driver           0 Healthy(InRecv(None))
//!  5 20000398 user_leds            0 Healthy(InRecv(None))
//!  6 20000408 pong                 0 Healthy(InRecv(None))
//!  7 20000478 ping                40 Healthy(InReply(TaskId(0x3)))
//!  8 200004e8 adt7420              0 Healthy(InRecv(Some(TaskId(0xffff))))
//!  9 20000558 idle                 0 Healthy(Runnable)          <-
//! ```
//!

use anyhow::{anyhow, bail, Result};
use clap::Command as ClapCommand;
use clap::{CommandFactory, Parser};
use core::mem::size_of;
use goblin::elf::Elf;
use hif::*;
use humility::arch::ARMRegister;
use humility::cli::Subcommand;
use humility::core::Core;
use humility::hubris::*;
use humility_cmd::hiffy::*;
use humility_cmd::idol::{self, HubrisIdol};
use humility_cmd::{Archive, Attach, Command, Validate};
use humpty::{
    DumpAreaHeader, DumpRegister, DumpSegmentData, DumpSegmentHeader,
};
use indicatif::{HumanBytes, HumanDuration};
use indicatif::{ProgressBar, ProgressStyle};
use num_traits::FromPrimitive;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::io::Cursor;
use std::io::Read;
use std::path::Path;
use std::time::Instant;
use zerocopy::FromBytes;

#[derive(Parser, Debug)]
#[clap(name = "dump", about = env!("CARGO_PKG_DESCRIPTION"))]
struct DumpArgs {
    /// sets timeout
    #[clap(
        long, short = 'T', default_value_t = 5000, value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// show dump agent status
    #[clap(long)]
    dump_agent_status: bool,

    /// force use of the dump agent
    #[clap(long)]
    force_dump_agent: bool,

    /// force dump to be read
    #[clap(
        long, conflicts_with_all = &["clear_dump_agent", "stimulate_dumper",
        "emulate_dumper"]
    )]
    #[clap(long)]
    force_read: bool,

    /// initialize dump, clearing any dump at the dump agent
    #[clap(long)]
    initialize_dump_agent: bool,

    /// simulate dumper by reading directly from target (skipping agent)
    #[clap(
        long,
        requires = "force_dump_agent",
        conflicts_with = "emulate_dumper"
    )]
    simulate_dumper: bool,

    /// in addition to simulating the dumper, generate a stock dump
    #[clap(long, requires = "simulate_dumper")]
    stock_dumpfile: Option<String>,

    /// emulate in situ dumper by reading directly from target and writing
    /// compressed memory back to agent's dump region
    #[clap(
        long,
        requires = "force_dump_agent",
        conflicts_with = "simulate_dumper"
    )]
    emulate_dumper: bool,

    dumpfile: Option<String>,
}

//
// When using the dump agent, we create our own ersatz Core
//
#[derive(Default)]
struct AgentCore {
    flash_contents: Vec<u8>,
    flash_regions: BTreeMap<u32, (u32, usize)>,
    ram_regions: BTreeMap<u32, Vec<u8>>,
    registers: HashMap<ARMRegister, u32>,
}

impl AgentCore {
    fn new(hubris: &HubrisArchive) -> Result<AgentCore> {
        //
        // We want to read in the "final.elf" from our archive and use that
        // to populate our flash memory to dump.
        //
        let cursor = Cursor::new(hubris.archive());
        let mut archive = zip::ZipArchive::new(cursor)?;
        let mut file = archive
            .by_name("img/final.elf")
            .map_err(|e| anyhow!("failed to find final.elf: {}", e))?;

        let mut flash_contents = Vec::new();
        file.read_to_end(&mut flash_contents)?;

        let elf = Elf::parse(&flash_contents).map_err(|e| {
            anyhow!("failed to parse final.elf as an ELF file: {}", e)
        })?;

        let mut flash_regions = BTreeMap::new();

        for shdr in elf.section_headers.iter() {
            if shdr.sh_type != goblin::elf::section_header::SHT_PROGBITS {
                continue;
            }

            flash_regions.insert(
                shdr.sh_addr as u32,
                (shdr.sh_size as u32, shdr.sh_offset as usize),
            );
        }

        Ok(Self { flash_contents, flash_regions, ..Default::default() })
    }

    fn add_ram_region(&mut self, addr: u32, contents: Vec<u8>) {
        self.ram_regions.insert(addr, contents);
    }

    fn add_register(&mut self, reg: ARMRegister, val: u32) {
        self.registers.insert(reg, val);
    }

    fn read_flash(&self, addr: u32, data: &mut [u8]) -> Result<()> {
        if let Some((&base, &(size, offset))) =
            self.flash_regions.range(..=addr).rev().next()
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
                    &self.flash_contents[roffs..roffs + data.len()],
                );

                return Ok(());
            }

            let len = (size as usize) - start;
            data[..len]
                .copy_from_slice(&self.flash_contents[roffs..roffs + len]);

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
    segments: &Vec<&HubrisRegion>,
) -> Result<()> {
    let op = hubris.get_idol_command("DumpAgent.add_dump_segment")?;
    let mut ops = vec![];

    for r in segments {
        let payload = op.payload(&[
            ("address", idol::IdolArgument::Scalar(r.base as u64)),
            ("length", idol::IdolArgument::Scalar(r.size as u64)),
        ])?;

        context.idol_call_ops(funcs, &op, &payload, &mut ops)?;
    }

    ops.push(Op::Done);

    let results = context.run(core, ops.as_slice(), None)?;

    for (result, region) in results.iter().zip(segments.iter()) {
        if let Err(err) = result {
            bail!(
                "failed to add segment at address {:#x} for length {}: {}",
                region.base,
                region.size,
                op.strerror(*err)
            );
        }
    }

    Ok(())
}

fn emulate_dump(core: &mut dyn Core, base: u32, total: u32) -> Result<()> {
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

fn take_dump(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
    funcs: &HiffyFunctions,
) -> Result<()> {
    let op = hubris.get_idol_command("DumpAgent.take_dump")?;
    let mut ops = vec![];

    humility::msg!("taking dump; target will be stopped for ~20-30 seconds");

    context.idol_call_ops(funcs, &op, &[], &mut ops)?;
    ops.push(Op::Done);

    let results = context.run(core, ops.as_slice(), None)?;

    if let Err(err) = results[0] {
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
) -> Result<DumpAreaHeader> {
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

            let size = size_of::<DumpAreaHeader>();

            if header.written > size as u32 {
                let max = std::cmp::min(header.written as usize, val.len());

                if !progress(&val[size..max]) {
                    return Ok(header);
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
                                return Ok(header);
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

            Ok(header)
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
) -> Result<DumpAreaHeader> {
    read_dump_at(hubris, core, context, funcs, 0, |_| false)
}

fn read_dump_headers(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
    funcs: &HiffyFunctions,
) -> Result<Vec<DumpAreaHeader>> {
    let mut rval = vec![];
    let mut done = false;
    let mut ndx = 0;

    while !done {
        let header =
            read_dump_at(hubris, core, context, funcs, ndx, |_| false)?;

        if header.next != 0 {
            ndx += 1;
        } else {
            done = true;
        }

        rval.push(header);
    }

    Ok(rval)
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
        if dump[offset..offset + 2] == humpty::DUMP_REGISTER_MAGIC {
            //
            // These are register values; slurp them and continue.
            //
            let reg = match DumpRegister::read_from_prefix(&dump[offset..]) {
                Some(reg) => reg,
                None => {
                    bail!("derailed on registers at offset {offset}");
                }
            };

            if let Some(register) = ARMRegister::from_u16(reg.register) {
                agent.add_register(register, reg.value);
            } else {
                let r = reg.register;
                bail!("unrecognized register {:#x} at offset {offset}", r);
            }

            offset += size_of::<DumpRegister>();
            continue;
        };

        let data = match DumpSegmentData::read_from_prefix(&dump[offset..]) {
            Some(data) => data,
            None => {
                bail!("derailed at offset {offset}");
            }
        };

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

        while offset < dump.len() && dump[offset] == humpty::DUMP_SEGMENT_PAD {
            offset += 1;
        }
    }

    Ok(())
}

fn read_dump(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
    funcs: &HiffyFunctions,
    agent: &mut AgentCore,
) -> Result<()> {
    let headers = read_dump_headers(hubris, core, context, funcs)?;
    let mut contents: Vec<u8> = vec![];

    let total = headers.iter().fold(0, |sum, header| sum + header.written);

    let started = Instant::now();
    let bar = ProgressBar::new(total as u64);
    bar.set_style(
        ProgressStyle::default_bar()
            .template("humility: pulling [{bar:30}] {bytes}/{total_bytes}"),
    );

    for (ndx, _) in headers.iter().enumerate() {
        read_dump_at(hubris, core, context, funcs, ndx, |rval| {
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

    if subargs.simulate_dumper {
        //
        // We are being asked to simulate our dumper:  we are going to pull
        // our dynamic memory directly from our target -- and determine what
        // our compression ratio would be along the way.
        //
        core.halt()?;
        humility::msg!("core halted");

        if let Some(ref stock) = subargs.stock_dumpfile {
            hubris.dump(core, &regions, Some(stock))?;
        }

        let total = regions
            .values()
            .filter(|&r| !r.attr.device && r.attr.write)
            .fold(0, |ttl, r| ttl + r.size);

        let started = Instant::now();

        let bar = ProgressBar::new(total as u64);
        bar.set_style(
            ProgressStyle::default_bar()
                .template("humility: reading [{bar:30}] {bytes}/{total_bytes}"),
        );

        let mut nread = 0;
        let mut ncompressed = 0;

        for (_, region) in regions.iter() {
            if region.attr.device || !region.attr.write {
                continue;
            }

            let mut remain = region.size as usize;
            let mut bytes = vec![0; 1024];
            let input_len = (bytes.len() / 2) - (bytes.len() / 8);

            let mut addr = region.base;

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

        let segments = regions
            .values()
            .filter(|&r| !r.attr.device && r.attr.write)
            .collect::<Vec<_>>();

        let header = read_dump_header(hubris, core, &mut context, &funcs)?;

        if !subargs.force_read {
            if header.dumper_version != humpty::DUMPER_NONE
                && !subargs.initialize_dump_agent
            {
                bail!(
                    "there appears to already be a dump in situ; \
                        clear with --initialize-dump-agent"
                )
            }

            humility::msg!("initializing dump agent state");
            initialize_dump(hubris, core, &mut context, &funcs)?;

            humility::msg!("initializing segments");
            initialize_segments(hubris, core, &mut context, &funcs, &segments)?;

            if subargs.initialize_dump_agent {
                return Ok(());
            }
        }

        if subargs.emulate_dumper {
            core.halt()?;
            humility::msg!("core halted");

            if let Some(ref stock) = subargs.stock_dumpfile {
                hubris.dump(core, &regions, Some(stock))?;
            }

            let total = segments.iter().fold(0, |ttl, &r| ttl + r.size);
            let base = header.address;

            emulate_dump(core, base, total)?;
            core.run()?;
            humility::msg!("core resumed");
        } else if !subargs.force_read {
            //
            // Tell the thing to take a dump
            //
            take_dump(hubris, core, &mut context, &funcs)?;
        }

        //
        // If we're here, we have a dump in situ -- time to pull it.
        //
        read_dump(hubris, core, &mut context, &funcs, &mut agent)?;
    }

    hubris.dump(&mut agent, &regions, subargs.dumpfile.as_deref())
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

    if subargs.dump_agent_status {
        dump_agent_status(hubris, core, &subargs)
    } else if core.is_net() || subargs.force_dump_agent {
        dump_via_agent(hubris, core, &subargs)
    } else {
        if subargs.initialize_dump_agent {
            bail!("must also use --force-dump-agent to initialize dump agent");
        }

        core.halt()?;
        humility::msg!("core halted");

        let regions = hubris.regions(core)?;
        let rval = hubris.dump(core, &regions, subargs.dumpfile.as_deref());

        core.run()?;
        humility::msg!("core resumed");

        rval
    }
}

pub fn init() -> (Command, ClapCommand<'static>) {
    (
        Command::Attached {
            name: "dump",
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Match,
            run: dumpcmd,
        },
        DumpArgs::command(),
    )
}
