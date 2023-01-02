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
use goblin::elf::Elf;
use hif::*;
use humility::arch::ARMRegister;
use humility::cli::Subcommand;
use humility::core::Core;
use humility::hubris::*;
use humility_cmd::doppel::DumpAreaHeader;
use humility_cmd::hiffy::*;
use humility_cmd::idol::{self, HubrisIdol};
use humility_cmd::{Archive, Attach, Command, Validate};
use std::collections::BTreeMap;
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

    fn read_reg(&mut self, _reg: ARMRegister) -> Result<u32> {
        //
        // XXX: for now!
        //
        Ok(0x1de)
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

type DumpLzss = lzss::Lzss<10, 4, 0x20, { 1 << 10 }, { 2 << 10 }>;

fn read_dump_at(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
    funcs: &HiffyFunctions,
    offset: u32,
) -> Result<(DumpAreaHeader, Vec<u8>)> {
    //
    // We expect a DumpAreaHeader to be here.
    //
    let op = hubris.get_idol_command("DumpAgent.read_dump")?;
    let mut ops = vec![];

    let payload =
        op.payload(&[("offset", idol::IdolArgument::Scalar(offset as u64))])?;

    context.idol_call_ops(&funcs, &op, &payload, &mut ops)?;
    ops.push(Op::Done);

    //
    // Call that.
    //
    let results = context.run(core, ops.as_slice(), None)?;

    // let mut rval = vec![];

    //
    for r in &results {
        if let Ok(val) = r {
            //
            // We should be able to turn that into
            let h = DumpAreaHeader::read_from_prefix(val.as_slice());

            //
            // XXX from here, we need to
            println!("{:x?}", h);
        } else {
            bail!("{:?}", r);
        }
    }

    bail!("no");
}

fn read_dump(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
    funcs: &HiffyFunctions,
) -> Result<()> {
    _ = read_dump_at(hubris, core, context, funcs, 0)?;

    Ok(())
}

fn dump_via_agent(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    subargs: &DumpArgs,
) -> Result<()> {
    let mut agent = AgentCore::new(hubris)?;
    let regions = hubris.regions(core)?;

    if subargs.simulate_dumper {
        use indicatif::{HumanBytes, HumanDuration};
        use indicatif::{ProgressBar, ProgressStyle};

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
            let mut addr = region.base;

            while remain > 0 {
                let nbytes =
                    if remain > bytes.len() { bytes.len() } else { remain };

                core.read_8(addr, &mut bytes[0..nbytes])?;

                let mut output = vec![0; 2048];

                let compressed = DumpLzss::compress(
                    lzss::SliceReader::new(&bytes[0..nbytes]),
                    lzss::SliceWriter::new(&mut output),
                )?;

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

    read_dump(hubris, core, &mut context, &funcs)?;

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
