/*
 * Copyright 2020 Oxide Computer Company
 */

use probe_rs::{Core, Probe};

#[macro_use]
extern crate log;

use structopt::StructOpt;

mod debug;
use debug::*;

mod etm;
use etm::*;

mod itm;
use itm::*;

mod dwt;
use dwt::*;

mod tpiu;
use tpiu::*;

mod hubris;
use hubris::*;

use std::error::Error;
use std::fs::File;
use std::convert::TryInto;
use std::time::Instant;
use std::time::SystemTime;
use std::collections::HashMap;

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
            eprintln!("humility: {} ({}): {}",
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
    exception: ETM3Exception
}

#[derive(Debug)]
struct TraceConfig<'a> {
    hubris: &'a HubrisPackage,
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

const HUMILITY_ETM_SWOSCALER: u16 = 7;
const HUMILITY_ETM_TRACEID_MAX: u8 = 0x7f;
const HUMILITY_ETM_ALWAYSTRUE: u32 = 0b110_1111;

fn etmcmd_probe(
    core: &probe_rs::Core,
) -> Result<(), probe_rs::Error> {
    let tab = read_debug_rom_table(&core)?;

    info!("ROM debug table: {:#x?}", tab);

    let etm = match tab.ETM {
        None => { 
            fatal!("ETM is not available on this CPU");
        }
        Some(etm) => { etm }
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

    let etmidr = ETMIDR::read(&core)?;
    info!("{:#x?}", etmidr);

    let etmccer = ETMCCER::read(&core)?;
    info!("{:#x?}", etmccer);

    Ok(())
}

fn etmcmd_enable(
    core: &probe_rs::Core,
    clockscaler: Option<u16>,
    traceid: u8,
) -> Result<(), probe_rs::Error> {
    let etmccr = ETMCCR::read(&core)?;

    if !etmccr.has_etmidr() {
        warn!("ETMv1.3 and earlier not supported");
        return Ok(());
    }

    let etmidr = ETMIDR::read(&core)?;
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
    let mut val = DEMCR::read(&core)?;
    val.set_trcena(true);
    val.write(&core)?;

    /*
     * Now unlock the ETM.
     */
    ETMLAR::unlock(&core)?;

    /*
     * STM32F407-specific: enable TRACE_IOEN in the DBGMCU_CR, and set the
     * trace mode to be asynchronous.
     */
    let mut val = DBGMCU_CR::read(&core)?;
    val.set_trace_ioen(true);
    val.set_trace_mode(0);
    val.write(&core)?;

    /*
     * Now setup the TPIU.
     */
    let mut val = TPIU_SPPR::read(&core)?;
    val.set_txmode(TPIUMode::NRZ);
    val.write(&core)?;

    let mut val = TPIU_FFCR::read(&core)?;
    val.set_continuous_formatting(true);
    val.write(&core)?;

    let mut acpr = TPIU_ACPR::read(&core)?;
    acpr.set_swoscaler(clockscaler.unwrap_or(HUMILITY_ETM_SWOSCALER).into());
    acpr.write(&core)?;
    trace!("{:#x?}", TPIU_ACPR::read(&core)?);

    /*
     * We are now ready to enable ETM.  There are a bunch of steps involved in
     * this, but we need to first write to the ETMCR to indicate that we are
     * programming it.  Once done writing to the ETM control registers, we
     * need to write to ETMCR again to indicate that we are done programming
     * it.
     */
    trace!("{:#x?}", ETMCR::read(&core)?);
    let mut etmcr = ETMCR::read(&core)?;
    etmcr.set_branch_output(true);
    etmcr.set_stall_processor(true);
    etmcr.set_port_size(1);
    etmcr.set_port_select(true);
    etmcr.set_programming(true);
    etmcr.set_power_down(false);
    trace!("will write {:#x?}", etmcr);
    etmcr.write(&core)?;

    /*
     * Set to the hard-wired always-true event
     */
    let mut teevr = ETMTEEVR::read(&core)?;
    teevr.set_resource_a(HUMILITY_ETM_ALWAYSTRUE);
    teevr.write(&core)?;
    trace!("{:#x?}", ETMTEEVR::read(&core)?);

    let mut tecr1 = ETMTECR1::read(&core)?;
    tecr1.set_map_decode_select(0);
    tecr1.set_comparator_select(0);
    tecr1.set_exclude(true);
    tecr1.write(&core)?;

    let mut ffrr = ETMFFRR::read(&core)?;
    ffrr.set_map_decode_select(0);
    ffrr.set_comparator_select(0);
    ffrr.set_exclude(true);
    ffrr.write(&core)?;

    let mut fflr = ETMFFLR::read(&core)?;
    fflr.set_fifo_full_level(24);
    fflr.write(&core)?;

    trace!("{:#x?}", ETMFFLR::read(&core)?);

    trace!("{:#x?}", ETMTRACEIDR::read(&core)?);
    let mut val = ETMTRACEIDR::read(&core)?;
    val.set_traceid(traceid.into());
    val.write(&core)?;
    trace!("{:#x?}", ETMTRACEIDR::read(&core)?);

    /*
     * Finally, indicate that we are done programming!
     */
    etmcr.set_programming(false);
    etmcr.write(&core)?;

    info!("ETM enabled");

    Ok(())
}

fn etmcmd_disable(
    core: &probe_rs::Core
) -> Result<(), probe_rs::Error> {
    let mut etmcr = ETMCR::read(&core)?;

    if etmcr.power_down() {
        info!("ETM not enabled");
        return Ok(());
    }

    etmcr.set_programming(true);
    etmcr.write(&core)?;

    etmcr.set_power_down(true);
    etmcr.write(&core)?;

    etmcr.set_programming(false);
    etmcr.write(&core)?;

    info!("ETM disabled");

    Ok(())
}

fn etmcmd_attach(args: &Args,
    _subargs: &EtmArgs,
) -> Result<probe_rs::Core, probe_rs::Error> {
    info!("attaching as chip {} ...", args.chip);
    let core = Core::auto_attach(&args.chip)?;
    info!("attached");

    Ok(core)
}

fn etmcmd_trace(
    config: &TraceConfig,
    instr: &TraceInstruction,
    state: &mut TraceState,
) -> Result<(), Box<dyn Error>> {
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
        Some(HubrisTarget::Call(_)) |
        Some(HubrisTarget::IndirectCall) => {
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
        HubrisTarget::Call(_) |
        HubrisTarget::IndirectCall => {
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
) -> Result<(), Box<dyn Error>> {
    println!("{:-10} {:8} X {:?}", exception.nsecs, "-", exception.exception);

    Ok(())
}

fn etmcmd_ingest(
    config: &TraceConfig,
    filename: &str,
) -> Result<(), Box<dyn Error>> {
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

    etm_ingest(&econfig, || {
        if let Some(line) = iter.next() {
            let record: SaleaeTraceRecord = line?;
            Ok(Some((record.1, record.0)))
        } else {
            Ok(None)
        }
    }, |packet| {
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
                &mut state
            )
        };

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
            ETM3Header::ExceptionExit |
            ETM3Header::ASync |
            ETM3Header::ISync |
            ETM3Header::BranchAddress { .. } => {}
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
                    (Some(origin), HubrisTarget::Direct(expected)) | 
                    (Some(origin), HubrisTarget::Call(expected)) => {
                        if curaddr.unwrap() != expected {
                            warn!(
                                concat!(
                                    "detected bad branch: ",
                                    "at 0x{:x} expected branch to 0x{:x}, ",
                                    "found 0x{:x}; packet: {:x?}"
                                ), origin, expected, curaddr.unwrap(), packet
                            );
                        }
                    }

                    (Some(origin), HubrisTarget::None) => {
                        if exception.is_none() {
                            warn!(
                                concat!(
                                    "detected bad branch: did not expect any ",
                                    "branch from 0x{:x}, but control ",
                                    "transferred to 0x{:x}; packet: {:x?}"
                                ), origin, curaddr.unwrap(), packet
                            );
                        }
                    }

                    (_, _) => {}
                }

                if let Some(exception) = exception {
                    etmcmd_trace_exception(
                        config,
                        &TraceException {
                            nsecs,
                            exception,
                        },
                        &mut state
                    )?;
                }
            }
            ETM3Payload::None => {}
        }

        Ok(())
    })?;

    Ok(())
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
        long, short, value_name = "identifier", conflicts_with = "disable",
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
}

fn etmcmd(
    hubris: &HubrisPackage,
    args: &Args,
    subargs: &EtmArgs,
) -> Result<(), probe_rs::Error> {
    let mut rval = Ok(());

    if subargs.traceid >= HUMILITY_ETM_TRACEID_MAX {
        fatal!(
            "traceid has a maximum value of {:x}",
            HUMILITY_ETM_TRACEID_MAX
        );
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
    let core = etmcmd_attach(args, subargs)?;
    let _info = core.halt();

    info!("core halted");

    if subargs.probe {
        rval = etmcmd_probe(&core);
    }

    if subargs.enable {
        rval = etmcmd_enable(&core, subargs.clockscaler, subargs.traceid);
    }

    if subargs.disable {
        rval = etmcmd_disable(&core);
    }

    core.run()?;
    info!("core resumed");

    rval
}

fn itmcmd_attach(args: &Args,
    _subargs: &ItmArgs,
) -> Result<(probe_rs::Session, probe_rs::Core), probe_rs::Error> {

    let probes = Probe::list_all();
    let probe = probes[0].open()?;

    info!("attaching as chip {} ...", args.chip);
    let session = probe.attach(&args.chip)?;

    let core = session.attach_to_core(0)?;
    info!("attached");

    Ok((session, core))
}

fn itmcmd_probe(
    core: &probe_rs::Core,
) -> Result<(), probe_rs::Error> {
    let tab = read_debug_rom_table(&core)?;

    info!("ROM debug table: {:#x?}", tab);

    info!("{:#x?}", DEMCR::read(&core)?);
    info!("{:#x?}", ITM_LSR::read(&core)?);
    info!("{:#x?}", ITM_TCR::read(&core)?);
    info!("{:#x?}", ITM_TER::read(&core)?);
    info!("{:#x?}", DBGMCU_CR::read(&core)?);
    info!("{:#x?}", TPIU_FFCR::read(&core)?);
    info!("{:#x?}", DWT_CTRL::read(&core)?);
    info!("{:#x?}", TPIU_SPPR::read(&core)?);

    Ok(())
}

fn itmcmd_enable(
    core: &probe_rs::Core,
    clockscaler: Option<u16>,
    traceid: u8,
) -> Result<(), probe_rs::Error> {
    /*
     * First, enable TRCENA in the DEMCR.
     */
    let mut val = DEMCR::read(&core)?;
    val.set_trcena(true);
    val.write(&core)?;

    /*
     * STM32F407-specific: enable TRACE_IOEN in the DBGMCU_CR, and set the
     * trace mode to be asynchronous.
     */
    let mut val = DBGMCU_CR::read(&core)?;
    val.set_trace_ioen(true);
    val.set_trace_mode(0);
    val.write(&core)?;

    /*
     * Now setup the TPIU.
     */
    let mut val = TPIU_SPPR::read(&core)?;
    val.set_txmode(TPIUMode::NRZ);
    val.write(&core)?;

    let mut val = TPIU_FFCR::read(&core)?;
    val.set_continuous_formatting(true);
    val.write(&core)?;

    let mut acpr = TPIU_ACPR::read(&core)?;
    acpr.set_swoscaler(clockscaler.unwrap_or(HUMILITY_ETM_SWOSCALER).into());
    acpr.write(&core)?;
    trace!("{:#x?}", TPIU_ACPR::read(&core)?);

    /*
     * Unlock the ITM.
     */
    ITM_LAR::unlock(&core)?;

    /*
     * Disable the ITM.
     */
    let mut tcr = ITM_TCR::read(&core)?;
    tcr.set_itm_enable(false);
    tcr.write(&core)?;

    /*
     * Spin until the ITM is not busy
     */
    while ITM_TCR::read(&core)?.itm_busy() {
        continue;
    }

    /*
     * Enable the DWT to generate a synchronization packet every 8M cycles.
     */
    let mut dwt = DWT_CTRL::read(&core)?;
    dwt.set_synctap(DWTSyncTapFrequency::CycCnt8M);
    dwt.set_cyccnt_enabled(true);
    dwt.write(&core)?;

    /*
     * Enable all stimuli
     */
    let mut ter = ITM_TER::read(&core)?;
    ter.set_enabled(0xffff_ffff);
    ter.write(&core)?;

    /*
     * Set the trace ID
     */
    tcr = ITM_TCR::read(&core)?;
    tcr.set_traceid(traceid.into());
    tcr.set_timestamp_enable(true);
    tcr.set_sync_enable(true);
    tcr.set_itm_enable(true);
    tcr.write(&core)?;

    Ok(())
}

fn itmcmd_disable(
    core: &probe_rs::Core
) -> Result<(), probe_rs::Error> {
    /*
     * Unlock the ITM.
     */
    ITM_LAR::unlock(&core)?;

    /*
     * Disable the ITM.
     */
    let mut tcr = ITM_TCR::read(&core)?;
    tcr.set_itm_enable(false);
    tcr.write(&core)?;

    info!("ITM disabled");

    Ok(())
}

fn itmcmd_ingest(
    traceid: u8,
    filename: &str,
) -> Result<(), Box<dyn Error>> {
    let file = File::open(filename)?;
    let mut rdr = csv::Reader::from_reader(file);

    type SaleaeTraceRecord = (f64, u8, Option<String>, Option<String>);

    let mut iter = rdr.deserialize();

    itm_ingest(traceid, || {
        if let Some(line) = iter.next() {
            let record: SaleaeTraceRecord = line?;
            Ok(Some((record.1, record.0)))
        } else {
            Ok(None)
        }
    }, |packet| {
        if let ITMPayload::Instrumentation { payload, .. } = &packet.payload {
            for p in payload {
                print!("{}", *p as char);
            }
        }

        Ok(())
    })
}

fn itmcmd_ingest_attached(
    session: &mut probe_rs::Session,
    _core: &mut probe_rs::Core,
    traceid: u8,
) -> Result<(), Box<dyn Error>> {
    let mut bytes: Vec<u8> = vec![];
    let mut ndx = 0;

    let start = Instant::now();

    itm_ingest(traceid, || {
        while ndx == bytes.len() {
            bytes = session.read_swv().unwrap();
            ndx = 0;
        }
        ndx += 1;
        Ok(Some((bytes[ndx - 1], start.elapsed().as_secs_f64())))
    }, |packet| {
        match &packet.payload {
            ITMPayload::Instrumentation { payload, port } => {
                if *port > 1 {
                    println!("{:x?}", payload);
                    return Ok(());
                }

                for p in payload {
                    print!("{}", *p as char);
                }
            }
            _ => {}
        }

        Ok(())
    })
}

#[derive(StructOpt)]
struct ItmArgs {
    /// probe for ITM capability on attached device
    #[structopt(
        long, short, conflicts_with_all = &["enable", "disable", "ingest"]
    )]
    probe: bool,
    /// enable ITM on attached device
    #[structopt(long, short, conflicts_with_all = &["disable", "ingest"])]
    enable: bool,
    /// disable ITM on attached device
    #[structopt(long, short)]
    disable: bool,
    /// sets ITM trace identifier
    #[structopt(
        long, short, default_value = "0x3a", value_name = "identifier",
        parse(try_from_str = parse_int::parse), conflicts_with = "disable"
    )]
    traceid: u8,
    /// ingest ITM data as CSV
    #[structopt(long, short, value_name = "filename")]
    ingest: Option<String>,
    /// ingest directly from attached device
    #[structopt(long, short, conflicts_with_all = &["disable", "ingest"])]
    attach: bool,
    /// sets the value of SWOSCALER
    #[structopt(long, short, value_name = "scaler", requires = "enable",
        parse(try_from_str = parse_int::parse),
    )]
    clockscaler: Option<u16>,
}

fn itmcmd(
    _hubris: &HubrisPackage,
    args: &Args,
    subargs: &ItmArgs,
) -> Result<(), probe_rs::Error> {
    let mut rval = Ok(());

    if subargs.traceid >= HUMILITY_ETM_TRACEID_MAX {
        fatal!(
            "traceid has a maximum value of {:x}",
            HUMILITY_ETM_TRACEID_MAX
        );
    }

    if let Some(ingest) = &subargs.ingest {
        match itmcmd_ingest(subargs.traceid, ingest) {
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
    let (mut session, mut core) = itmcmd_attach(args, subargs)?;
    let _info = core.halt();

    info!("core halted");

    if subargs.probe {
        rval = itmcmd_probe(&core);
    }

    if subargs.enable {
        rval = itmcmd_enable(&core, subargs.clockscaler, subargs.traceid);
    }

    if subargs.disable {
        rval = itmcmd_disable(&core);
    }

    core.run()?;
    info!("core resumed");

    if subargs.attach {
        match itmcmd_ingest_attached(&mut session, &mut core, subargs.traceid) {
            Err(e) => {
                fatal!("failed to ingest from attached device: {}", e);
            }
            _ => {
                return Ok(());
            }
        }
    }

    rval
}

fn probe(
    args: &Args,
) -> Result<(), probe_rs::Error> {
    let probe_list = Probe::list_all();
    info!("probes: {:?}", probe_list);

    info!("attaching as chip {} ...", &args.chip);
    let _core = Core::auto_attach(&args.chip)?;
    info!("attached");

    Ok(())
}

#[derive(StructOpt)]
struct TasksArgs {
    /// spin pulling tasks
    #[structopt(
        long, short, 
    )]
    spin: bool,
}

fn taskscmd(
    hubris: &HubrisPackage,
    args: &Args,
    subargs: &TasksArgs,
) -> Result<(), Box<dyn Error>> {
    let core = Core::auto_attach(&args.chip)?;

    let base = core.read_word_32(hubris.lookup_symword("TASK_TABLE_BASE")?)?;
    let size = core.read_word_32(hubris.lookup_symword("TASK_TABLE_SIZE")?)?;

    let task = hubris.lookup_struct_byname("Task")?;
    let taskdesc = hubris.lookup_struct_byname("TaskDesc")?;

    let state = task.lookup_member("state")?;
    let state_enum = hubris.lookup_enum(state.goff)?;

    loop {
        let cur = core.read_word_32(
            hubris.lookup_symword("CURRENT_TASK_PTR")?
        )?;

        /*
         * We read the entire task table at a go to get as consistent a
         * snapshot as possible.
         */
        let mut taskblock: Vec<u8> = vec![];
        taskblock.resize_with(task.size * size as usize, Default::default);
        core.read_8(base, taskblock.as_mut_slice())?;

        let descriptor = task.lookup_member("descriptor")?.offset as u32;
        let generation = task.lookup_member("generation")?.offset as u32;
        let state = task.lookup_member("state")?.offset as u32;

        let entry_point = taskdesc.lookup_member("entry_point")?.offset as u32;

        println!("{:2} {:8} {:18} {:3} {:9}",
            "ID", "ADDR", "TASK", "GEN", "STATE");

        let taskblock32 = |o| {
            u32::from_le_bytes(taskblock[o..o + 4].try_into().unwrap())
        };

        for i in 0..size {
            let addr = base + i * task.size as u32;
            let offs = i as usize * task.size;
            let soffs = offs + state as usize;

            let gen = taskblock[offs + generation as usize];
            let daddr = taskblock32(offs + descriptor as usize);
            let entry = core.read_word_32(daddr + entry_point)?;
            let module = hubris.instr_mod(entry).unwrap_or("<unknown>");

            println!("{:2} {:08x} {:18} {:3} {:25} {}", i, addr, module, gen,
                hubris.dump(&taskblock[soffs..], state_enum.goff)?,
                if addr == cur { " <-" } else { "" });

            println!("{:?}", &taskblock[soffs..soffs + state_enum.size]);
        }

        if !subargs.spin {
            break;
        }
    }

    Ok(())
}

#[derive(StructOpt)]
struct TraceArgs {
    /// sets ITM trace identifier
    #[structopt(
        long, short, default_value = "0x3a", value_name = "identifier",
        parse(try_from_str = parse_int::parse)
    )]
    traceid: u8,
    /// sets the value of SWOSCALER
    #[structopt(long, short, value_name = "scaler", requires = "enable",
        parse(try_from_str = parse_int::parse)
    )]
    clockscaler: Option<u16>,
    /// provide statemap-ready output
    #[structopt(
        long, short
    )]
    statemap: bool,
}

fn tracecmd_ingest(
    hubris: &HubrisPackage,
    subargs: &TraceArgs,
    session: &mut probe_rs::Session,
    tasks: &HashMap<u32, String>,
) -> Result<(), Box<dyn Error>> {
    let mut bytes: Vec<u8> = vec![];
    let mut ndx = 0;

    let start = Instant::now();
    let mut ts: f64 = 0.0;
    let mut time = 0;

    let mut states: HashMap<String, i32> = HashMap::new();

    if subargs.statemap {
        let t = SystemTime::now().duration_since(SystemTime::UNIX_EPOCH)?;

        let colors = [
            "#ed441f", "#ef5b3b", "#f27357", "#f48a73",
            "#f6a28f", "#f8b9ab", "#fad0c7", "#fde8e3",
        ];

        println!("{{");
        println!("\t\"start\": [ {}, {} ],", t.as_secs(), t.subsec_nanos());
        println!("\t\"title\": \"Hubris tasks\",");
        println!("\t\"entityKind\": \"Task\",");

        println!("\t\"states\": {{");
        println!("\t\t\"Running\": {{ \"value\": 0, \"color\": \"#DAF7A6\" }},");
        println!("\t\t\"Runnable\": {{ \"value\": 1, \"color\": \"#9BC362\" }},");
        println!("\t\t\"InRecv\": {{ \"value\": 2, \"color\": \"#e0e0e0\" }},");

        states.insert("Runnable".to_string(), 1);
        states.insert("InRecv(None)".to_string(), 2);

        for i in 0..tasks.len() {
            let name = tasks.get(&(i as u32)).unwrap();
            let s = format!("InReply(({}))", i);
            let state = 3 + i;

            states.insert(s, state as i32);
            println!(concat!("\t\t\"InReply({})\": {{ \"value\": {}, ",
                "\"color\": \"{}\" }}{}"), name, state, colors[i],
                if i < tasks.len() - 1 { "," } else { "" });
        }

        println!("\t}}");
        println!("}}");

        for i in 0..tasks.len() {
            let name = tasks.get(&(i as u32)).unwrap();
            println!("{{ \"entity\": \"{}\", \"description\": \"{}\" }}",
                i, name);
        }
    }

    let tstruct = hubris.lookup_struct_byname("Task")?;
    let state = tstruct.lookup_member("state")?;
    let state_enum = hubris.lookup_enum(state.goff)?;
    let healthy = state_enum.lookup_variant_byname("Healthy")?;
    let hh = hubris.lookup_struct(healthy.goff)?;

    let schedstate = hubris.lookup_enum(hh.lookup_member("__0")?.goff)?;
    let mut spayload = Vec::with_capacity(schedstate.size);
    let mut task = 0;
    let mut newtask = None;

    itm_ingest(subargs.traceid, || {
        while ndx == bytes.len() {
            bytes = session.read_swv().unwrap();
            ts = start.elapsed().as_secs_f64();
            ndx = 0;
        }
        ndx += 1;
        Ok(Some((bytes[ndx - 1], ts)))
    }, |packet| {
        match &packet.payload {
            ITMPayload::Instrumentation { payload, port } => {
                if *port == 30 {
                    newtask = Some(payload[0] as u32);
                    return Ok(());
                }

                if *port == 31 {
                    if payload.len() == 1 {
                        task = payload[0] as u32;
                        spayload.truncate(0);
                    } else {
                        for p in payload {
                            spayload.push(*p);
                        }
                    }

                    if spayload.len() < schedstate.size {
                        return Ok(());
                    }

                    if !subargs.statemap {
                        println!(
                            "{:.9} {} ({}): {}",
                            time as f64 / 16_000_000 as f64,
                            task,
                            tasks.get(&task).unwrap_or(&"<invalid>".to_string()),
                            hubris.dump(&spayload[..], schedstate.goff)?,
                        );
                        return Ok(());
                    }

                    let state = hubris.dump(&spayload[..], schedstate.goff)?;

                    println!(
                        concat!(
                            "{{ \"time\": \"{}\", \"entity\": \"{}\", ",
                            "\"state\": {} }}"
                        ),
                        ((time as f64 / 16_000_000 as f64) *
                        1_000_000_000 as f64) as u64,
                        task, states.get(&state).unwrap_or(&-1)
                    );

                    return Ok(());
                }

                if !subargs.statemap {
                    for p in payload {
                        print!("{}", *p as char);
                    }
                }
            }

            ITMPayload::LocalTimestamp { timedelta, delayed, early } => {
                time += timedelta;

                if let Some(task) = newtask {
                    if subargs.statemap {
                        println!(
                            concat!(
                                "{{ \"time\": \"{}\", \"entity\": \"{}\", ",
                                "\"state\": 0 }}"
                            ),
                            ((time as f64 / 16_000_000 as f64) *
                            1_000_000_000 as f64) as u64,
                            task
                        );
                    } else {
                        println!(
                            "{:.9} {} ({}): Running",
                            time as f64 / 16_000_000 as f64,
                            task,
                            tasks.get(&task).unwrap_or(&"<invalid>".to_string())
                        );
                    }

                    newtask = None;
                }
            }
            _ => {}
        }

        Ok(())
    })
}

fn tracecmd(
    hubris: &HubrisPackage,
    args: &Args,
    subargs: &TraceArgs,
) -> Result<(), Box<dyn Error>> {
    let mut tasks: HashMap<u32, String> = HashMap::new();

    let probes = Probe::list_all();
    let probe = probes[0].open()?;
    let mut session = probe.attach(&args.chip)?;
    let core = session.attach_to_core(0)?;

    /*
     * First, read the task block to get a mapping of IDs to names.
     */
    let base = core.read_word_32(hubris.lookup_symword("TASK_TABLE_BASE")?)?;
    let size = core.read_word_32(hubris.lookup_symword("TASK_TABLE_SIZE")?)?;

    let task = hubris.lookup_struct_byname("Task")?;
    let descriptor = task.lookup_member("descriptor")?.offset as u32;

    let taskdesc = hubris.lookup_struct_byname("TaskDesc")?;
    let entry_point = taskdesc.lookup_member("entry_point")?.offset as u32;

    let mut taskblock: Vec<u8> = vec![];
    taskblock.resize_with(task.size * size as usize, Default::default);
    core.read_8(base, taskblock.as_mut_slice())?;

    let taskblock32 = |o| {
        u32::from_le_bytes(taskblock[o..o + 4].try_into().unwrap())
    };

    for i in 0..size {
        let addr = base + i * task.size as u32;
        let offs = i as usize * task.size;
        let daddr = taskblock32(offs + descriptor as usize);
        let entry = core.read_word_32(daddr + entry_point)?;
        let module = hubris.instr_mod(entry).unwrap_or("<unknown>");

        trace!("task {} is {}", i, module);
        tasks.insert(i, module.to_string());
    }

    /*
     * Now enable ITM and ingest.
     */
    itmcmd_enable(&core, subargs.clockscaler, subargs.traceid)?;
    tracecmd_ingest(&hubris, subargs, &mut session, &tasks)?;

    Ok(())
}

#[derive(StructOpt)]
#[structopt(name = "humility", max_term_width = 80)]
struct Args {
    /// verbose messages
    #[structopt(long, short)]
    verbose: bool,

    /// specific chip on attached device
    #[structopt(long, short, env = "HUMILITY_CHIP", default_value = "STM32F407VGTx")]
    chip: String,

    /// directory containing Hubris package
    #[structopt(long, short, env = "HUMILITY_PACKAGE")]
    package: Option<String>,

    #[structopt(subcommand)]
    cmd: Subcommand,
}

#[derive(StructOpt)]
enum Subcommand {
    /// probe for attached devices
    Probe,
    /// commands for ARM's Embedded Trace Macrocell (ETM) facility
    Etm(EtmArgs),
    /// commands for ARM's Instrumentation Trace Macrocell (ITM) facility
    Itm(ItmArgs),
    /// list tasks
    Tasks(TasksArgs),
    /// trace Hubris operations
    Trace(TraceArgs),
}

fn main() {
    let args = Args::from_args();

    if args.verbose {
        HumilityLog { level: log::LevelFilter::Trace }.enable();
    } else {
        HumilityLog { level: log::LevelFilter::Info }.enable();
    }

    let mut hubris = HubrisPackage::new().map_err(|err| {
        fatal!("failed to initialize: {}", err);
    }).unwrap();

    if let Some(dir) = &args.package {
        if let Err(err) = hubris.load(&dir) {
            fatal!("failed to load package: {}", err);
        }
    } else {
        match &args.cmd {
            Subcommand::Tasks(..) | Subcommand::Trace(..) => {
                fatal!("must provide a package");
            }
            _ => {}
        }
    }

    match &args.cmd {
        Subcommand::Probe => match probe(&args) {
            Err(err) => fatal!("probe failed: {} (raw: \"{:?})\"", err, err),
            _ => std::process::exit(0),
        }

        Subcommand::Etm(subargs) => match etmcmd(&hubris, &args, subargs) {
            Err(err) => fatal!("etm failed: {} (raw: \"{:?})\"", err, err),
            _ => std::process::exit(0),
        }

        Subcommand::Itm(subargs) => match itmcmd(&hubris, &args, subargs) {
            Err(err) => fatal!("itm failed: {} (raw: \"{:?})\"", err, err),
            _ => std::process::exit(0),
        }

        Subcommand::Tasks(subargs) => match taskscmd(&hubris, &args, subargs) {
            Err(err) => fatal!("tasks failed: {} (raw: \"{:?})\"", err, err),
            _ => std::process::exit(0),
        }

        Subcommand::Trace(subargs) => match tracecmd(&hubris, &args, subargs) {
            Err(err) => fatal!("trace failed: {} (raw: \"{:?})\"", err, err),
            _ => std::process::exit(0),
        }
    }
}
