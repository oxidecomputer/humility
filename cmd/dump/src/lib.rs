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

use anyhow::{bail, Result};
use clap::{ArgGroup, CommandFactory, Parser};
use humility::arch::ARMRegister;
use humility::cli::Subcommand;
use humility::core::Core;
use humility::hubris::*;
use humility_cmd::{Archive, Attach, Command, CommandKind, Validate};
use humility_dump_agent::{
    task_areas, DumpAgent, DumpAgentCore, DumpAgentExt, DumpArea,
    HiffyDumpAgent, UdpDumpAgent,
};
use humpty::DumpTask;
use indicatif::{HumanBytes, HumanDuration, ProgressBar, ProgressStyle};
use num_traits::FromPrimitive;
use std::cell::RefCell;
use std::time::Instant;

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

    /// force use of hiffy, even if the UDP dump agent is available
    #[clap(long)]
    force_hiffy_agent: bool,

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

    /// simulates a single-task dump
    #[clap(
        long,
        requires = "simulate_dumper",
        conflicts_with = "stock-dumpfile"
    )]
    simulate_task_dump: Option<String>,

    /// dumps a single task
    #[clap(
        long,
        conflicts_with_all = &[
            "stock_dumpfile", "simulation", "list", "area",
            "force_read", "force_manual_initialization",
            "dump_agent_status"
        ]
    )]
    task: Option<String>,

    #[clap(short, long, conflicts_with_all = &[
        "simulate_task_dump", "simulation", "list"
    ])]
    area: Option<usize>,

    /// leave the target halted
    #[clap(long, conflicts_with = "simulation")]
    leave_halted: bool,

    #[clap(long, short, conflicts_with_all = &[
        "simulate_task_dump", "simulation", "area"
    ])]
    list: bool,

    dumpfile: Option<String>,
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

fn get_dump_agent<'a>(
    hubris: &'a HubrisArchive,
    core: &'a mut dyn Core,
    subargs: &DumpArgs,
) -> Result<Box<dyn DumpAgent + 'a>> {
    // Find the dump agent task name.  This is usually `dump_agent`, but that's
    // not guaranteed; what *is* guaranteed is that it implements the DumpAgent
    // interface.
    let dump_agent_task_name = (0..hubris.ntasks())
        .map(|t| hubris.lookup_module(HubrisTask::Task(t as u32)).unwrap())
        .filter(|t| {
            t.iface.as_ref().map(|i| i.name == "DumpAgent").unwrap_or(false)
        })
        .map(|t| t.name.as_str())
        .next();

    if core.is_net()
        && !subargs.force_hiffy_agent
        && hubris
            .manifest
            .task_features
            .get(dump_agent_task_name.unwrap_or(""))
            .map(|f| f.contains(&"net".to_string()))
            .unwrap_or(false)
    {
        humility::msg!("using UDP dump agent");
        Ok(Box::new(UdpDumpAgent::new(core)))
    } else {
        humility::msg!("using hiffy dump agent");
        Ok(Box::new(HiffyDumpAgent::new(hubris, core, subargs.timeout)?))
    }
}

fn dump_via_agent(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    subargs: &DumpArgs,
) -> Result<()> {
    let mut out = DumpAgentCore::new(HubrisFlashMap::new(hubris)?);
    let started = Some(Instant::now());
    let mut area = subargs.area.map(DumpArea::ByIndex);

    //
    // Our task can come from a couple of different spots:  we can either
    // be explicitly told our task (in which case we are simulated or
    // emulated) or we are pulling it out of an area.  If the latter, we
    // will discover the task when we actually read our dump headers, so
    // leave it as None for now...
    //
    let mut task = match &subargs.simulate_task_dump {
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
        task = agent.read_dump(area, &mut out, true)?;

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

fn dump_task_via_agent(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    subargs: &DumpArgs,
) -> Result<()> {
    let mut out = DumpAgentCore::new(HubrisFlashMap::new(hubris)?);
    let started = Some(Instant::now());

    let mut agent = get_dump_agent(hubris, core, subargs)?;

    todo!()
}

fn dump_list(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    subargs: &DumpArgs,
) -> Result<()> {
    let mut agent = get_dump_agent(hubris, core, subargs)?;

    println!("{:4} {:21} {:10} SIZE", "AREA", "TASK", "TIME");
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
                Ok(module) => match headers[0].contents {
                    humpty::DUMP_CONTENTS_SINGLETASK => module.name.to_owned(),
                    humpty::DUMP_CONTENTS_TASKREGION =>
                        format!("{} [region]", module.name.to_owned()),
                    c => bail!("unknown contents type: {c}"),
                },
                _ => "<unknown>".to_owned(),
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
    println!("{:#x?}", headers);

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
    } else if subargs.task.is_some() {
        if subargs.force_dump_agent {
            humility::msg!("--force-dump-agent is implied by --task");
        }
        dump_task_via_agent(hubris, core, &subargs)
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
