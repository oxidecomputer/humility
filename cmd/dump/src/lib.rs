// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility dump`
//!
//! `humility dump` takes a dump of the attached system, writing out an ELF
//! core file:
//!
//! ```console
//! $ humility dump
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
//! $ humility dump hubris.core.`date +%s`
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
//! $ humility -d hubris.core.0 tasks
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
use humility::core::Core;
use humility::hubris::*;
use humility_arch_arm::ARMRegister;
use humility_cli::{ExecutionContext, Subcommand};
use humility_cmd::{Archive, Attach, Command, CommandKind, Validate};
use humility_dump_agent::{
    task_areas, DumpAgent, DumpAgentCore, DumpAgentExt, DumpArea,
    DumpBreakdown, HiffyDumpAgent, UdpDumpAgent,
};
use humpty::DumpTask;
use indicatif::{HumanBytes, HumanDuration, ProgressBar, ProgressStyle};
use num_traits::FromPrimitive;
use std::cell::RefCell;
use std::path::PathBuf;
use std::time::Instant;

#[derive(Clone, Parser, Debug)]
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
    #[clap(long, conflicts_with_all = &["simulation", "task", "extract-all"])]
    dump_agent_status: bool,

    /// force use of the dump agent when directly attached with a debug probe
    #[clap(long)]
    force_dump_agent: bool,

    /// force use of hiffy, even if the UDP dump agent is available
    #[clap(long)]
    force_hiffy_agent: bool,

    /// force manual initiation, leaving target halted
    #[clap(
        long, requires = "force-dump-agent",
        conflicts_with_all = &["extract-all", "task"]
    )]
    force_manual_initiation: bool,

    /// force existing in situ dump to be read
    #[clap(long, conflicts_with_all = &["simulation", "task", "extract-all"])]
    force_read: bool,

    /// initialize dump state, clearing any dump at the dump agent
    #[clap(long, conflicts_with = "simulation")]
    initialize_dump_agent: bool,

    /// retain dump state after a system dump
    #[clap(long, conflicts_with_all = &["task", "list"])]
    retain_state: bool,

    /// overwrite any dump state as part of taking dump
    #[clap(
        long,
        conflicts_with_all = &["initialize-dump-agent", "simulate-dumper"],
    )]
    force_overwrite: bool,

    /// simulate dumper by reading directly from target
    #[clap(long, group = "simulation")]
    simulate_dumper: bool,

    /// in addition to simulating the dumper, generate a stock dump
    #[clap(
        long, requires = "simulation",
        conflicts_with_all = &["task", "extract-all"],
        value_name = "filename",
    )]
    stock_dumpfile: Option<PathBuf>,

    /// emulate in situ dumper by reading directly from target and writing
    /// compressed memory back to agent's dump region
    #[clap(long, group = "simulation")]
    emulate_dumper: bool,

    /// simulates a single-task dump
    #[clap(
        long,
        conflicts_with_all = &["extract", "stock-dumpfile", "list"],
        requires = "simulate-dumper",
        value_name = "task",
    )]
    simulate_task_dump: Option<String>,

    /// dumps a single task
    #[clap(
        long,
        conflicts_with_all = &["simulation", "list", "extract"],
        value_name = "task",
    )]
    task: Option<String>,

    /// extracts the dump in the specified area
    #[clap(
        short = 'a', alias = "area", long, value_name = "area",
        conflicts_with_all = &["simulation", "list"]
    )]
    extract: Option<usize>,

    /// extracts every available dump
    #[clap(
        long,
        alias = "all",
        conflicts_with_all = &["simulation", "list", "extract", "task"]
    )]
    extract_all: bool,

    /// leave the target halted
    #[clap(long, conflicts_with = "simulation")]
    leave_halted: bool,

    /// list all dump areas
    #[clap(long, short, conflicts_with_all = &["simulation", "extract"])]
    list: bool,

    /// print dump breakdown
    #[clap(long)]
    print_dump_breakdown: bool,

    dumpfile: Option<PathBuf>,
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

    let r = humpty::dump::<anyhow::Error, 1024, { humpty::DUMPER_EMULATED }>(
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
            area.region.address,
            *base,
            *size,
            |addr, buf, _meta| shared.borrow_mut().read_8(addr, buf),
            |addr, buf| shared.borrow_mut().write_8(addr, buf),
        ) {
            bail!("adding segment at {base:#x} (length {size}) failed: {e:x?}");
        }
    }

    Ok(area.region.address)
}

fn get_dump_agent<'a>(
    hubris: &'a HubrisArchive,
    core: &'a mut dyn Core,
    subargs: &DumpArgs,
) -> Result<Box<dyn DumpAgent + 'a>> {
    // Find the dump agent task name.  This is usually `dump_agent`, but that's
    // not guaranteed; what *is* guaranteed is that it implements the DumpAgent
    // interface.
    let dump_agent_task =
        hubris.lookup_module_by_iface("DumpAgent").map(|t| t.task);

    if core.is_net()
        && !subargs.force_hiffy_agent
        && dump_agent_task
            .map(|t| hubris.does_task_have_feature(t, "net").unwrap())
            .unwrap_or(false)
    {
        humility::msg!("using UDP dump agent");

        let imageid = &hubris
            .imageid
            .as_ref()
            .ok_or_else(|| anyhow!("missing image ID"))?
            .1;

        Ok(Box::new(UdpDumpAgent::new(core, imageid)?))
    } else {
        humility::msg!("using hiffy dump agent");
        Ok(Box::new(HiffyDumpAgent::new(hubris, core, subargs.timeout)?))
    }
}

fn read_dump<'a>(
    agent: &mut Box<dyn DumpAgent + 'a>,
    area: Option<DumpArea>,
    out: &mut DumpAgentCore,
    subargs: &DumpArgs,
) -> Result<Option<DumpTask>> {
    let (task, breakdown) = agent.read_dump(area, out, true)?;

    if subargs.print_dump_breakdown {
        print_dump_breakdown(&breakdown);
    }

    Ok(task)
}

fn print_dump_breakdown(breakdown: &DumpBreakdown) {
    let w = 30;
    let w2 = 10;

    let mut total = 0;

    humility::msg!("Dump breakdown:");

    let print_val = |str, val| {
        humility::msg!("  {str:<w$} => {val}");
    };

    let print_signed_val = |str, val| {
        humility::msg!("  {str:<w$} => {val}");
    };

    let mut print_perc = |str, val, acct| {
        let (label, perc) = if acct {
            total += val;
            ("consumed", (val as f32 / breakdown.used as f32) * 100.0)
        } else {
            ("total", (val as f32 / breakdown.total as f32) * 100.0)
        };

        humility::msg!("  {str:<w$} => {val:<w2$} {perc:>6.2}% of {label}");
    };

    print_val("total dump area", breakdown.total);
    print_perc("dump area consumed", breakdown.used, false);
    print_perc("dump headers", breakdown.headers, true);
    print_perc("segment headers", breakdown.segment_headers, true);
    print_perc("register headers + data", breakdown.registers, true);
    print_perc("data headers", breakdown.data_headers, true);
    print_perc("data", breakdown.compressed, true);
    print_perc("data padding", breakdown.segment_padding, true);
    print_perc("orphaned", breakdown.orphaned, true);

    humility::msg!(
        "  -------------------------------------\
        --------------------------------"
    );

    let unaccounted = breakdown.used as i32 - total as i32;
    print_val("total accounted", total);
    print_signed_val("unaccounted dump area", unaccounted);

    humility::msg!("");

    print_val("uncompressed size", breakdown.uncompressed);
    print_val("compressed size", breakdown.compressed);
    print_val("data expansion", breakdown.inverted);
}

fn dump_via_agent(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    subargs: &DumpArgs,
) -> Result<()> {
    let mut out = DumpAgentCore::new(HubrisFlashMap::new(hubris)?);
    let started = Some(Instant::now());
    let mut area = subargs.extract.map(DumpArea::ByIndex);

    //
    // Our task can come from a couple of different spots:  we can either
    // be explicitly told our task (in which case we are simulated or
    // emulated) or we are pulling it out of an area.  If the latter, we
    // will discover the task when we actually read our dump headers, so
    // leave it as None for now...
    //
    let mut task = match &subargs.simulate_task_dump {
        Some(task) => {
            if !subargs.simulate_dumper {
                bail!("--simulate-task-dump requires --simulate-dumper");
            }
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
                        "compresion overflow at address {addr:#x} \
                        by {overflow} bytes",
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

        if !subargs.force_read && subargs.extract.is_none() {
            if header.dumper != humpty::DUMPER_NONE
                && !subargs.initialize_dump_agent
                && !subargs.force_overwrite
                && task.is_none()
            {
                bail!(
                    "there appears to already be one or more dumps in situ; \
                    list them with --list and extract them with --extract-all"
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
        } else if !subargs.force_read && subargs.extract.is_none() {
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
            // We are about to disappear for -- as the kids say -- a minute.
            // Set our timeout to be a literal minute so we don't prematurely
            // give up.
            //
            agent.core().set_timeout(std::time::Duration::new(60, 0))?;

            //
            // Tell the thing to take a dump.
            //
            if let Err(err) = agent.take_dump() {
                //
                // If that fails, it may be because we ran out of space.  Check
                // our dump headers; if all of them are consumed, assume
                // that we ran out of space -- and if any of them are consumed,
                // process whatever we find (some dump is better than none!) and
                // warn accordingly.
                //
                if let Ok(all) = agent.read_dump_headers(true) {
                    let c = all
                        .iter()
                        .filter(|&&(h, _)| h.dumper != humpty::DUMPER_NONE)
                        .count();

                    if c == all.len() {
                        humility::warn!(
                            "dump has indicated failure ({err:?}), but this is \
                            likely due to space exhaustion; \
                            dump will be extracted but may be incomplete!"
                        );
                    } else if c != 0 {
                        humility::warn!(
                            "dump has indicated failure ({err:?}), but some dump \
                            contents appear to have been written; \
                            dump will be extracted but may be incomplete!"
                        );
                    } else {
                        return Err(err);
                    }
                } else {
                    return Err(err);
                }
            }
        }

        //
        // If we're here, we have a dump in situ -- time to pull it.
        //
        task = read_dump(&mut agent, area, &mut out, subargs)?;

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

    let task = subargs.task.as_ref().unwrap();
    let ndx = match hubris.lookup_task(task) {
        Some(HubrisTask::Task(ndx)) => *ndx,
        _ => {
            bail!("invalid task \"{task}\"");
        }
    };
    if ndx == 0 {
        bail!("cannot dump supervisor");
    }
    let area = agent.dump_task(ndx)?;
    let task = read_dump(
        &mut agent,
        Some(DumpArea::ByIndex(area as usize)),
        &mut out,
        subargs,
    )?;
    assert!(task.is_some());
    hubris.dump(&mut out, task, subargs.dumpfile.as_deref(), started)?;

    Ok(())
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

fn dump_all(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    subargs: &DumpArgs,
) -> Result<()> {
    let mut agent = get_dump_agent(hubris, core, subargs)?;
    let headers = agent.read_dump_headers(false)?;
    if headers.is_empty() || headers[0].0.dumper == humpty::DUMPER_NONE {
        return Ok(());
    }

    // We have a full-system dump
    if headers[0].1.is_none() {
        // Extract the full-system dump via the usual method
        drop(agent);
        let mut subargs = subargs.clone();
        subargs.force_read = true;
        dump_via_agent(hubris, core, &subargs)
    } else {
        let areas = task_areas(&headers);
        for (area, (task, headers)) in &areas {
            let task_name = match hubris
                .lookup_module(HubrisTask::Task(task.id.into()))
            {
                Ok(module) => match headers[0].contents {
                    humpty::DUMP_CONTENTS_SINGLETASK => module.name.to_owned(),
                    humpty::DUMP_CONTENTS_TASKREGION => {
                        format!("{}.region", module.name.to_owned())
                    }
                    c => bail!("unknown contents type: {c}"),
                },
                _ => "<unknown>".to_owned(),
            };

            let dumpfile = (0..)
                .map(|i| PathBuf::from(format!("hubris.core.{task_name}.{i}")))
                .find(|f| !f.exists())
                .unwrap();
            humility::msg!("dumping {task_name} (area {area})");

            let mut out = DumpAgentCore::new(HubrisFlashMap::new(hubris)?);
            let started = Some(Instant::now());
            let task = read_dump(
                &mut agent,
                Some(DumpArea::ByIndex(*area)),
                &mut out,
                subargs,
            )?;

            assert!(task.is_some());
            hubris.dump(&mut out, task, Some(&dumpfile), started)?;
        }

        if !subargs.retain_state {
            humility::msg!("resetting dump agent state");
            agent.initialize_dump()?;
        } else {
            humility::msg!("retaining dump agent state");
        }
        Ok(())
    }
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

fn dumpcmd(context: &mut ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let hubris = context.archive.as_ref().unwrap();

    let subargs = DumpArgs::try_parse_from(subargs)?;

    if subargs.force_dump_agent && core.is_net() {
        bail!("can only force the dump agent when attached via debug probe");
    }

    if subargs.extract_all {
        dump_all(hubris, core, &subargs)
    } else if subargs.list {
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
        || subargs.extract.is_some()
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
            attach: Attach::Any,
            validate: Validate::Match,
        },
    }
}
