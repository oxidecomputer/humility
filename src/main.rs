/*
 * Copyright 2020 Oxide Computer Company
 */

#[macro_use]
extern crate log;

#[macro_use]
extern crate num_derive;

use anyhow::{bail, Result};

use structopt::StructOpt;

mod debug;
use debug::*;

mod etm;
use etm::*;

mod tpiu;
use tpiu::*;

mod hubris;
use hubris::*;

mod scs;
use scs::*;

mod cmd;
mod core;
mod dwt;
mod itm;
mod swo;
mod test;

use std::convert::TryInto;
use std::fs::File;
use std::time::Instant;

macro_rules! fatal {
    ($fmt:expr) => ({
        eprint!(concat!("humility: ", $fmt, "\n"));
        ::std::process::exit(1);
    });
    ($fmt:expr, $($arg:tt)*) => ({
        eprint!(concat!("humility: ", $fmt, "\n"), $($arg)*);
        ::std::process::exit(1);
    });
}

#[derive(Debug, Clone, Copy)]
pub struct HumilityLog {
    level: log::LevelFilter,
}

fn is_humility(metadata: &log::Metadata) -> bool {
    if let Some(metadata) = metadata.target().split("::").next() {
        metadata == "humility"
    } else {
        false
    }
}

impl log::Log for HumilityLog {
    fn enabled(&self, metadata: &log::Metadata) -> bool {
        metadata.level() <= self.level
    }

    fn log(&self, record: &log::Record) {
        if !self.enabled(record.metadata()) {
            return;
        }

        if is_humility(record.metadata()) {
            eprintln!("humility: {}", record.args())
        } else {
            eprintln!(
                "humility: {} ({}): {}",
                record.level(),
                record.metadata().target(),
                record.args()
            );
        }
    }

    fn flush(&self) {}
}

impl HumilityLog {
    pub fn enable(&mut self) {
        match log::set_boxed_logger(Box::new(*self)) {
            Err(e) => {
                fatal!("unable to enable logging: {}", e);
            }
            Ok(_l) => {
                log::set_max_level(self.level);
            }
        };
    }
}

struct TraceInstruction {
    nsecs: u64,
    addr: u32,
    _len: u32,
    target: HubrisTarget,
    skipped: bool,
}

struct TraceException {
    nsecs: u64,
    exception: ETM3Exception,
}

#[derive(Debug)]
struct TraceConfig<'a> {
    hubris: &'a HubrisArchive,
    flowindent: bool,
    traceid: u8,
}

#[derive(Debug, Default)]
struct TraceState {
    indent: usize,
    target: Option<HubrisTarget>,
    inlined: Vec<HubrisGoff>,
    stack: Vec<(usize, Vec<HubrisGoff>, u32)>,
}

fn attach(args: &Args) -> Result<Box<dyn core::Core>> {
    if let Some(dump) = &args.dump {
        crate::core::attach_dump(dump)
    } else {
        let probe = match &args.probe {
            Some(p) => p,
            None => "auto",
        };

        crate::core::attach(probe, &args.chip)
    }
}

fn attach_live(args: &Args) -> Result<Box<dyn core::Core>> {
    if let Some(_) = &args.dump {
        bail!("must be run against a live system");
    } else {
        attach(args)
    }
}

const HUMILITY_ETM_SWOSCALER: u16 = 7;
const HUMILITY_ETM_TRACEID_MAX: u8 = 0x7f;
const HUMILITY_ETM_ALWAYSTRUE: u32 = 0b110_1111;

fn etmcmd_probe(core: &mut dyn core::Core) -> Result<()> {
    let coreinfo = CoreInfo::read(core)?;

    let etm = match coreinfo.address(CoreSightComponent::ETM) {
        None => {
            fatal!("ETM is not available on this CPU");
        }
        Some(etm) => etm,
    };

    if etm != ETMCR::ADDRESS {
        fatal!(
            "ETM base address (0x{:x}) is not at expected location ({:x})",
            etm,
            ETMCR::ADDRESS
        );
    }

    let etmccr = ETMCCR::read(core)?;
    info!("{:#x?}", etmccr);

    if !etmccr.has_etmidr() {
        warn!("ETMv1.3 and earlier not supported");
        return Ok(());
    }

    let etmidr = ETMIDR::read(core)?;
    info!("{:#x?}", etmidr);

    let etmccer = ETMCCER::read(core)?;
    info!("{:#x?}", etmccer);

    Ok(())
}

fn etmcmd_enable(
    core: &mut dyn core::Core,
    clockscaler: Option<u16>,
    traceid: u8,
) -> Result<()> {
    let etmccr = ETMCCR::read(core)?;

    if !etmccr.has_etmidr() {
        warn!("ETMv1.3 and earlier not supported");
        return Ok(());
    }

    let etmidr = ETMIDR::read(core)?;
    trace!("{:?}", etmidr);

    let major = etmidr.etm_major() + 1;
    let minor = etmidr.etm_minor();

    if (major, minor) != (3, 5) {
        warn!("only ETMv3.5 supported");
        return Ok(());
    }

    if !etmidr.has_branch_encoding() {
        warn!("only alternative branch encoding supported");
        return Ok(());
    }

    /*
     * First, enable TRCENA in the DEMCR.
     */
    let mut val = DEMCR::read(core)?;
    val.set_trcena(true);
    val.write(core)?;

    /*
     * Now unlock the ETM.
     */
    ETMLAR::unlock(core)?;

    /*
     * STM32F407-specific: enable TRACE_IOEN in the DBGMCU_CR, and set the
     * trace mode to be asynchronous.
     */
    let mut val = STM32F4_DBGMCU_CR::read(core)?;
    val.set_trace_ioen(true);
    val.set_trace_mode(0);
    val.write(core)?;

    /*
     * Now setup the TPIU.
     */
    let mut val = TPIU_SPPR::read(core)?;
    val.set_txmode(TPIUMode::NRZ);
    val.write(core)?;

    let mut val = TPIU_FFCR::read(core)?;
    val.set_continuous_formatting(true);
    val.write(core)?;

    let mut acpr = TPIU_ACPR::read(core)?;
    acpr.set_swoscaler(clockscaler.unwrap_or(HUMILITY_ETM_SWOSCALER).into());
    acpr.write(core)?;
    trace!("{:#x?}", TPIU_ACPR::read(core)?);

    /*
     * We are now ready to enable ETM.  There are a bunch of steps involved in
     * this, but we need to first write to the ETMCR to indicate that we are
     * programming it.  Once done writing to the ETM control registers, we
     * need to write to ETMCR again to indicate that we are done programming
     * it.
     */
    trace!("{:#x?}", ETMCR::read(core)?);
    let mut etmcr = ETMCR::read(core)?;
    etmcr.set_branch_output(true);
    etmcr.set_stall_processor(true);
    etmcr.set_port_size(1);
    etmcr.set_port_select(true);
    etmcr.set_programming(true);
    etmcr.set_power_down(false);
    trace!("will write {:#x?}", etmcr);
    etmcr.write(core)?;

    /*
     * Set to the hard-wired always-true event
     */
    let mut teevr = ETMTEEVR::read(core)?;
    teevr.set_resource_a(HUMILITY_ETM_ALWAYSTRUE);
    teevr.write(core)?;
    trace!("{:#x?}", ETMTEEVR::read(core)?);

    let mut tecr1 = ETMTECR1::read(core)?;
    tecr1.set_map_decode_select(0);
    tecr1.set_comparator_select(0);
    tecr1.set_exclude(true);
    tecr1.write(core)?;

    let mut ffrr = ETMFFRR::read(core)?;
    ffrr.set_map_decode_select(0);
    ffrr.set_comparator_select(0);
    ffrr.set_exclude(true);
    ffrr.write(core)?;

    let mut fflr = ETMFFLR::read(core)?;
    fflr.set_fifo_full_level(24);
    fflr.write(core)?;

    trace!("{:#x?}", ETMFFLR::read(core)?);

    trace!("{:#x?}", ETMTRACEIDR::read(core)?);
    let mut val = ETMTRACEIDR::read(core)?;
    val.set_traceid(traceid.into());
    val.write(core)?;
    trace!("{:#x?}", ETMTRACEIDR::read(core)?);

    /*
     * Finally, indicate that we are done programming!
     */
    etmcr.set_programming(false);
    etmcr.write(core)?;

    info!("ETM enabled");

    Ok(())
}

fn etmcmd_disable(core: &mut dyn core::Core) -> Result<()> {
    let mut etmcr = ETMCR::read(core)?;

    if etmcr.power_down() {
        info!("ETM not enabled");
        return Ok(());
    }

    etmcr.set_programming(true);
    etmcr.write(core)?;

    etmcr.set_power_down(true);
    etmcr.write(core)?;

    etmcr.set_programming(false);
    etmcr.write(core)?;

    info!("ETM disabled");

    Ok(())
}

#[rustfmt::skip::macros(println)]

fn etmcmd_trace(
    config: &TraceConfig,
    instr: &TraceInstruction,
    state: &mut TraceState,
) -> Result<()> {
    let hubris = config.hubris;
    let addr = instr.addr;
    let c = if !instr.skipped { 'E' } else { 'N' };
    let module = hubris.instr_mod(addr).unwrap_or("<unknown>");
    let sym = hubris.instr_sym(addr).unwrap_or(("<unknown>", addr));
    let sigil = 2;

    if !config.flowindent {
        println!("{:-10} {:08x} {} {}:{}+{:x} {:x?}",
            instr.nsecs, addr, c, module, sym.0, addr - sym.1, instr.target);
        return Ok(());
    }

    let inlined = hubris.instr_inlined(addr, sym.1);

    match state.target {
        Some(HubrisTarget::Call(_)) | Some(HubrisTarget::IndirectCall) => {
            state.indent += 2;
            println!("{:-10} {:width$}-> {}:{}", instr.nsecs, "", module, sym.0,
                width = state.indent);
        }
        None => {
            println!("{:-10} {:width$} ? {}:{}", instr.nsecs, "", module, sym.0,
                width = state.indent);
        }
        _ => {}
    }

    for (i, element) in inlined.iter().enumerate() {
        if i < state.inlined.len() && element.id == state.inlined[i] {
            continue;
        }

        println!("{:-10} {:width$} | {}:{} {}", instr.nsecs, "", module,
            element.name, element.id,
            width = state.indent + (i * 2) + sigil);
    }

    while let Some(_) = state.inlined.pop() {
        continue;
    }

    state.target = Some(instr.target);

    match instr.target {
        HubrisTarget::Call(_) | HubrisTarget::IndirectCall => {
            let mut nindent = state.indent;

            if !inlined.is_empty() {
                nindent += (inlined.len() * 2) + 1;
            }

            state.stack.push((
                state.indent,
                inlined.iter().map(|i| i.id).collect(),
                instr.addr,
            ));

            state.indent = nindent;

            return Ok(());
        }

        HubrisTarget::Return => {
            println!("{:-10} {:width$}<- {}:{}", instr.nsecs, "", module, sym.0,
                width = state.indent);

            if !state.stack.is_empty() {
                let top = state.stack.pop().unwrap();

                state.inlined = top.1;
                state.indent = top.0;
            } else {
                state.indent = 0;
            }
        }
        _ => {
            state.inlined = inlined.iter().map(|i| i.id).collect();
        }
    }

    Ok(())
}

fn etmcmd_trace_exception(
    _config: &TraceConfig,
    exception: &TraceException,
    _state: &mut TraceState,
) -> Result<()> {
    println!("{:-10} {:8} X {:?}", exception.nsecs, "-", exception.exception);

    Ok(())
}

fn etmcmd_ingest(config: &TraceConfig, filename: &str) -> Result<()> {
    let file = File::open(filename)?;
    let mut rdr = csv::Reader::from_reader(file);
    let mut curaddr: Option<u32> = None;
    let mut lastaddr: Option<u32> = None;
    let hubris = config.hubris;

    let econfig = &ETM3Config {
        alternative_encoding: true,
        context_id: 0,
        data_access: false,
        traceid: config.traceid,
    };

    type SaleaeTraceRecord = (f64, u8, Option<String>, Option<String>);

    let mut iter = rdr.deserialize();
    let mut broken = false;
    let mut target: (Option<u32>, HubrisTarget) = (None, HubrisTarget::None);

    let mut state = TraceState::default();

    etm_ingest(
        &econfig,
        || {
            if let Some(line) = iter.next() {
                let record: SaleaeTraceRecord = line?;
                Ok(Some((record.1, record.0)))
            } else {
                Ok(None)
            }
        },
        |packet| {
            let nsecs = (packet.time * 1_000_000_000_f64) as u64;

            match (lastaddr, packet.header) {
                (None, ETM3Header::ISync) | (Some(_), _) => {}
                (None, _) => {
                    if broken {
                        return Ok(());
                    }

                    fatal!("non-ISync packet at time {}", nsecs);
                }
            }

            let mut instr = |skipped| {
                if broken {
                    return Ok(());
                }

                let addr = curaddr.unwrap();
                let mut l = 0;

                curaddr = match hubris.instr_len(addr) {
                    Some(len) => {
                        l = len;
                        Some(addr + len)
                    }
                    None => {
                        warn!("unknown instruction length at {:x}!", addr);
                        broken = true;
                        None
                    }
                };

                target = (Some(addr), hubris.instr_target(addr));
                etmcmd_trace(
                    config,
                    &TraceInstruction {
                        nsecs,
                        addr,
                        target: target.1,
                        _len: l,
                        skipped,
                    },
                    &mut state,
                )
            };

            println!("{:#x?}", packet);

            match packet.header {
                ETM3Header::PHeaderFormat1 { e, n } => {
                    for _i in 0..e {
                        instr(false)?;
                    }

                    for _i in 0..n {
                        instr(true)?;
                    }
                }
                ETM3Header::PHeaderFormat2 { e0, e1 } => {
                    instr(e0)?;
                    instr(e1)?;
                }
                ETM3Header::ExceptionExit
                | ETM3Header::ASync
                | ETM3Header::ISync
                | ETM3Header::BranchAddress { .. } => {}
                _ => {
                    fatal!("unhandled packet: {:#x?}", packet);
                }
            }

            match packet.payload {
                ETM3Payload::ISync { address, .. } => {
                    if broken {
                        warn!("re-railing at offset {}", packet.offset);
                        broken = false;
                        target = (None, HubrisTarget::None);
                    }

                    curaddr = Some(address);
                    lastaddr = curaddr;
                }
                ETM3Payload::BranchAddress { addr, mask, exception } => {
                    curaddr = Some((lastaddr.unwrap() & mask) | addr);
                    lastaddr = curaddr;

                    match (target.0, target.1) {
                        (Some(origin), HubrisTarget::Direct(expected))
                        | (Some(origin), HubrisTarget::Call(expected)) => {
                            if curaddr.unwrap() != expected {
                                warn!(
                                    "detected bad branch: at 0x{:x} expected \
                                branch to 0x{:x}, found 0x{:x}; packet: {:x?}",
                                    origin,
                                    expected,
                                    curaddr.unwrap(),
                                    packet
                                );
                            }
                        }

                        (Some(origin), HubrisTarget::None) => {
                            if exception.is_none() {
                                warn!(
                                    "detected bad branch: did not expect any \
                                branch from 0x{:x}, but control transferred \
                                to 0x{:x}; packet: {:x?}",
                                    origin,
                                    curaddr.unwrap(),
                                    packet
                                );
                            }
                        }

                        (_, _) => {}
                    }

                    if let Some(exception) = exception {
                        etmcmd_trace_exception(
                            config,
                            &TraceException { nsecs, exception },
                            &mut state,
                        )?;
                    }
                }
                ETM3Payload::None => {}
            }

            Ok(())
        },
    )?;

    Ok(())
}

fn etmcmd_output(core: &mut dyn core::Core) -> Result<()> {
    let start = Instant::now();

    println!("Time [s],Value,Parity Error,Framing Error");

    loop {
        let bytes = core.read_swv().unwrap();

        for b in bytes {
            println!("{:.15},0x{:02X},,", start.elapsed().as_secs_f64(), b);
        }
    }
}

#[derive(StructOpt)]
struct EtmArgs {
    /// probe for ETM capability on attached device
    #[structopt(
        long, short, conflicts_with_all = &["enable", "disable", "ingest"]
    )]
    probe: bool,
    /// enable ETM on attached device
    #[structopt(long, short, conflicts_with_all = &["disable", "ingest"])]
    enable: bool,
    /// disable ETM on attached device
    #[structopt(long, short)]
    disable: bool,
    /// sets ETM trace identifier
    #[structopt(
        long, short, value_name = "identifier",
        default_value = "0x54", parse(try_from_str = parse_int::parse),
    )]
    traceid: u8,
    /// ingest ETM data as CSV
    #[structopt(long, short, value_name = "filename")]
    ingest: Option<String>,
    /// flowindent ingested data
    #[structopt(long, short = "F")]
    flowindent: bool,
    /// sets the value of SWOSCALER
    #[structopt(
        long, short, value_name = "scaler", requires = "enable",
        parse(try_from_str = parse_int::parse)
    )]
    clockscaler: Option<u16>,
    /// output ETM data as CSV
    #[structopt(long, short, conflicts_with = "ingest")]
    output: bool,
}

fn etmcmd(
    hubris: &HubrisArchive,
    args: &Args,
    subargs: &EtmArgs,
) -> Result<()> {
    let mut rval = Ok(());

    let traceid = subargs.traceid;

    if traceid >= HUMILITY_ETM_TRACEID_MAX {
        fatal!("traceid has a maximum value of {:x}", HUMILITY_ETM_TRACEID_MAX);
    }

    if let Some(ingest) = &subargs.ingest {
        let config = TraceConfig {
            hubris,
            flowindent: subargs.flowindent,
            traceid: subargs.traceid,
        };

        match etmcmd_ingest(&config, ingest) {
            Err(e) => {
                fatal!("failed to ingest {}: {}", ingest, e);
            }
            _ => {
                return Ok(());
            }
        }
    }

    /*
     * For all of the other commands, we need to actually attach to the chip.
     */
    let mut core = attach(args)?;
    let _info = core.halt()?;

    info!("core halted");

    if subargs.probe {
        rval = etmcmd_probe(core.as_mut());
    }

    if subargs.enable {
        rval = etmcmd_enable(core.as_mut(), subargs.clockscaler, traceid);
    }

    if subargs.disable {
        rval = etmcmd_disable(core.as_mut());
    }

    core.run()?;
    info!("core resumed");

    if subargs.output {
        match etmcmd_output(core.as_mut()) {
            Err(e) => {
                fatal!("failed to output from attached device: {}", e);
            }
            _ => {
                return Ok(());
            }
        }
    }

    rval
}

fn probe(hubris: &HubrisArchive, args: &Args) -> Result<()> {
    let mut core = attach(args)?;

    hubris.validate(core.as_mut())?;
    cpuinfo(hubris, core.as_mut())?;

    Ok(())
}

fn map(hubris: &HubrisArchive, args: &Args) -> Result<()> {
    let mut core = attach(&args)?;
    hubris.validate(core.as_mut())?;

    let regions = hubris.regions(core.as_mut())?;

    println!(
        "{:10} {:10}   {:10} {:>7} {:5} {:2} {}",
        "DESC", "LOW", "HIGH", "SIZE", "ATTR", "ID", "TASK"
    );

    for (_, region) in regions.iter() {
        println!(
            "{:10} 0x{:08x} - 0x{:08x} {:>7} {}{}{}{}{} {:2} {}",
            match region.daddr {
                Some(daddr) => format!("0x{:08x}", daddr),
                None => "-".to_owned(),
            },
            region.base,
            region.base + region.mapsize - 1,
            if region.mapsize >= 1024 {
                format!("{}KiB", region.mapsize >> 10)
            } else {
                format!("{}", region.mapsize)
            },
            if region.attr.read { "r" } else { "-" },
            if region.attr.write { "w" } else { "-" },
            if region.attr.execute { "x" } else { "-" },
            if region.attr.device { "d" } else { "-" },
            if region.attr.dma { "m" } else { "-" },
            region.task.id(),
            hubris.lookup_task(region.task)?.name
        );
    }

    Ok(())
}

#[derive(StructOpt, Debug)]
struct DumpArgs {
    dumpfile: Option<String>,
}

fn dump(hubris: &HubrisArchive, args: &Args, subargs: &DumpArgs) -> Result<()> {
    let mut core = attach(&args)?;
    hubris.validate(core.as_mut())?;
    let _info = core.halt()?;
    info!("core halted");

    let rval = hubris.dump(core.as_mut(), subargs.dumpfile.as_deref());

    core.run()?;
    info!("core resumed");

    rval
}

fn manifest(hubris: &HubrisArchive) -> Result<()> {
    hubris.manifest()?;
    Ok(())
}

#[derive(StructOpt, Debug)]
struct ApptableArgs {
    #[structopt(
        help = "path to kernel ELF object (in lieu of Hubris archive)"
    )]
    kernel: Option<String>,
}

#[rustfmt::skip::macros(println, fatal)]
fn apptable(hubris: &HubrisArchive) -> Result<()> {
    let app = hubris.lookup_struct_byname("App")?;
    let task = hubris.lookup_struct_byname("TaskDesc")?;
    let region = hubris.lookup_struct_byname("RegionDesc")?;
    let interrupt = hubris.lookup_struct_byname("Interrupt")?;
    let fmt = HubrisPrintFormat { indent: 4, newline: true, hex: true };
    let apptable = hubris.apptable();

    let fatal = |msg, expected| -> ! {
        fatal!("short app table on {}: found {} bytes, expected at least {}",
            msg, apptable.len(), expected);
    };

    if app.size > apptable.len() {
        fatal("App header", app.size);
    }

    let lookup = |m| -> Result<u32> {
        let o = app.lookup_member(m)?.offset;
        Ok(u32::from_le_bytes(apptable[o..o + 4].try_into().unwrap()))
    };

    let task_count = lookup("task_count")?;
    let region_count = lookup("region_count")?;
    let irq_count = lookup("irq_count")?;

    println!(
        "App ={}\n",
        hubris.printfmt(&apptable[0..app.size], app.goff, &fmt)?
    );

    let mut offs = app.size;

    for i in 0..region_count {
        let str = format!("RegionDesc[0x{:x}]", i);

        if offs + region.size > apptable.len() {
            fatal(&str, offs + region.size);
        }

        println!("{} ={}\n", str, hubris.printfmt(
            &apptable[offs..offs + region.size],
            region.goff,
            &fmt
        )?);

        offs += region.size;
    }

    for i in 0..task_count {
        let str = format!("TaskDesc[0x{:x}]", i);

        if offs + task.size > apptable.len() {
            fatal(&str, offs + task.size);
        }

        println!("{} ={}\n", str, hubris.printfmt(
            &apptable[offs..offs + task.size],
            task.goff,
            &fmt)?
        );

        offs += task.size;
    }

    for i in 0..irq_count {
        let str = format!("Interrupt[0x{:x}]", i);

        if offs + interrupt.size > apptable.len() {
            fatal(&str, offs + interrupt.size);
        }

        println!("{} ={}\n", str, hubris.printfmt(
            &apptable[offs..offs + interrupt.size],
            interrupt.goff,
            &fmt)?
        );

        offs += interrupt.size;
    }

    Ok(())
}

#[derive(StructOpt)]
#[structopt(name = "humility", max_term_width = 80)]
pub struct Args {
    /// verbose messages
    #[structopt(long, short)]
    verbose: bool,

    /// specific chip on attached device
    #[structopt(
        long,
        short,
        env = "HUMILITY_CHIP",
        default_value = "STM32F407VGTx"
    )]
    chip: String,

    /// chip probe to use
    #[structopt(long, short, env = "HUMILITY_PROBE", conflicts_with = "dump")]
    probe: Option<String>,

    /// Hubris archive
    #[structopt(
        long,
        short,
        env = "HUMILITY_ARCHIVE",
        conflicts_with = "dump"
    )]
    archive: Option<String>,

    /// Hubris dump
    #[structopt(long, short, env = "HUMILITY_DUMP")]
    dump: Option<String>,

    #[structopt(subcommand)]
    cmd: Subcommand,
}

#[derive(StructOpt)]
enum Subcommand {
    /// print Hubris apptable
    Apptable(ApptableArgs),
    /// generate Hubris dump
    Dump(DumpArgs),
    /// commands for ARM's Embedded Trace Macrocell (ETM)
    Etm(EtmArgs),
    /// print memory map, with association of regions to tasks
    Map,
    /// print archive manifest
    Manifest,
    /// probe for any attached devices
    Probe,

    #[structopt(external_subcommand)]
    Other(Vec<String>),
}

fn main() {
    /*
     * This isn't hugely efficient, but we actually parse our arguments twice:
     * the first is with our subcommands grafted into our arguments to get us
     * a unified help and error message in the event of any parsing value or
     * request for a help message; if that works, we parse our arguments again
     * but relying on the external_subcommand to directive to allow our
     * subcommand to do any parsing on its own.
     */
    let (commands, clap) = cmd::init(Args::clap());
    let _args = Args::from_clap(&clap.get_matches());

    /*
     * If we're here, we know that our arguments pass muster from the Structopt/
     * Clap perspective.
     */
    let args = Args::from_args();

    if args.verbose {
        HumilityLog { level: log::LevelFilter::Trace }.enable();
    } else {
        HumilityLog { level: log::LevelFilter::Info }.enable();
    }

    let mut hubris = HubrisArchive::new()
        .map_err(|err| {
            fatal!("failed to initialize: {}", err);
        })
        .unwrap();

    if let Some(archive) = &args.archive {
        if let Err(err) = hubris.load(&archive) {
            fatal!("failed to load archive: {}", err);
        }
    } else if let Some(dump) = &args.dump {
        if let Err(err) = hubris.load_dump(&dump) {
            fatal!("failed to load dump: {}", err);
        }
    } else {
        match &args.cmd {
            Subcommand::Apptable(subargs) => {
                if subargs.kernel.is_none() {
                    fatal!("must provide a Hubris archive or kernel");
                }
            }

            Subcommand::Dump(..) | Subcommand::Manifest | Subcommand::Map => {
                fatal!("must provide a Hubris archive");
            }
            _ => {}
        }
    }

    match &args.cmd {
        Subcommand::Apptable(subargs) => {
            if let Some(ref kernel) = subargs.kernel {
                match hubris.load_kernel(kernel) {
                    Err(err) => fatal!("can't load {}: {:?}", kernel, err),
                    _ => {}
                }
            }

            match apptable(&hubris) {
                Err(err) => fatal!("apptable failed: {:?}", err),
                _ => std::process::exit(0),
            }
        }

        Subcommand::Probe => match probe(&hubris, &args) {
            Err(err) => fatal!("probe failed: {:?}", err),
            _ => std::process::exit(0),
        },

        Subcommand::Etm(subargs) => match etmcmd(&hubris, &args, subargs) {
            Err(err) => fatal!("etm failed: {:?}", err),
            _ => std::process::exit(0),
        },

        Subcommand::Dump(subargs) => match dump(&hubris, &args, subargs) {
            Err(err) => fatal!("dump failed: {:?}", err),
            _ => std::process::exit(0),
        },

        Subcommand::Manifest => match manifest(&hubris) {
            Err(err) => fatal!("manifest failed: {:?}", err),
            _ => std::process::exit(0),
        },

        Subcommand::Map => match map(&hubris, &args) {
            Err(err) => fatal!("map failed: {:?}", err),
            _ => std::process::exit(0),
        },

        Subcommand::Other(ref subargs) => {
            match cmd::subcommand(&commands, &hubris, &args, subargs) {
                Err(err) => fatal!("{} failed: {:?}", subargs[0], err),
                _ => std::process::exit(0),
            }
        }
    }
}
