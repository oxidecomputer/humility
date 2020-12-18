/*
 * Copyright 2020 Oxide Computer Company
 */

#[macro_use]
extern crate log;

#[macro_use]
extern crate num_derive;

use anyhow::{anyhow, bail, Context, Result};

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

mod swo;
use swo::*;

mod hubris;
use hubris::*;

mod scs;
use scs::*;

mod test;
use test::*;

mod core;

use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::convert::TryInto;
use std::fs::File;
use std::io::Read;
use std::time::Instant;
use std::time::SystemTime;

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

fn clock_scaler(
    hubris: &HubrisArchive,
    core: &mut dyn core::Core,
    arg: Option<u16>,
) -> Result<u16> {
    let debug_clock_mhz = 2_000_000;

    match arg {
        Some(val) => Ok(val),
        None => match hubris.clock(core)? {
            None => Err(anyhow!(
                "clock couldn't be determined; set clock scaler explicitly"
            )),

            Some(clock) => Ok(((clock * 1000) / debug_clock_mhz) as u16 - 1),
        },
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

fn itmcmd_probe(core: &mut dyn core::Core, coreinfo: &CoreInfo) -> Result<()> {
    info!("{:#x?}", TPIU_ACPR::read(core)?);
    info!("{:#x?}", TPIU_SPPR::read(core)?);
    info!("{:#x?}", TPIU_FFCR::read(core)?);

    info!("{:#x?}", ITM_LSR::read(core)?);
    info!("{:#x?}", ITM_TCR::read(core)?);
    info!("{:#x?}", ITM_TER::read(core)?);
    info!("{:#x?}", ITM_TPR::read(core)?);

    info!("{:#x?}", DWT_CTRL::read(core)?);

    info!("{:#x?}", DEMCR::read(core)?);

    match (coreinfo.vendor, coreinfo.part) {
        (Vendor::ST, ARMCore::CortexM4) => {
            info!("{:#x?}", STM32F4_DBGMCU_CR::read(core)?);
        }
        (Vendor::ST, ARMCore::CortexM7) => {
            info!("{:#x?}", STM32H7_DBGMCU_CR::read(core)?);
        }
        _ => {}
    }

    Ok(())
}

fn itmcmd_enable(
    core: &mut dyn core::Core,
    coreinfo: &CoreInfo,
    clockscaler: u16,
    traceid: u8,
    stimuli: u32,
) -> Result<()> {
    /*
     * First, enable TRCENA in the DEMCR.
     */
    let mut val = DEMCR::read(core)?;
    val.set_trcena(true);
    val.write(core)?;

    match (coreinfo.vendor, coreinfo.part) {
        (Vendor::ST, ARMCore::CortexM4) => {
            /*
             * STM32F4xx-specific: enable TRACE_IOEN in the DBGMCU_CR, and set
             * the trace mode to be asynchronous.
             */
            let mut val = STM32F4_DBGMCU_CR::read(core)?;
            val.set_trace_ioen(true);
            val.set_trace_mode(0);
            val.write(core)?;
        }

        (Vendor::ST, ARMCore::CortexM7) => {
            /*
             * STM32H7xx-specific: enable D3 and D1 clock domain + traceclk
             */
            let mut cr = STM32H7_DBGMCU_CR::read(core)?;
            cr.set_srdbgcken(true);
            cr.set_cddbgcken(true);
            cr.set_traceclken(true);
            cr.write(core)?;

            let components = &coreinfo.components;

            if let Some(cstf) = components.get_vec(&CoreSightComponent::CSTF) {
                /*
                 * If we have two funnels, the first is in D3 -- and it needs
                 * to be unlocked and enabled.
                 */
                if cstf.len() > 1 {
                    trace!("SWTF found at {:x}", cstf[0]);
                    SWO_LAR::unlock(core, cstf[0])?;
                    let mut swtf = SWTF_CTRL::read(core, cstf[0])?;
                    swtf.register.set_es0(true);
                    swtf.write(core)?;
                }
            }
        }

        _ => {}
    }

    let swoscaler = clockscaler as u32;

    if let Some(swo) = coreinfo.address(CoreSightComponent::SWO) {
        /*
         * If we have a SWO unit, configure it instead of the TPIU
         */
        trace!("SWO found at {:x}", swo);

        SWO_LAR::unlock(core, swo)?;

        let mut codr = SWO_CODR::read(core, swo)?;
        codr.register.set_prescaler(swoscaler);
        codr.write(core)?;

        let mut sppr = SWO_SPPR::read(core, swo)?;
        sppr.register.set_pprot(SWOMode::NRZ.into());
        sppr.write(core)?;
    } else {
        /*
         * Otherwise setup the TPIU.
         */
        let mut val = TPIU_SPPR::read(core)?;
        val.set_txmode(TPIUMode::NRZ);
        val.write(core)?;

        let mut val = TPIU_FFCR::read(core)?;
        val.set_continuous_formatting(true);
        val.write(core)?;

        let mut acpr = TPIU_ACPR::read(core)?;
        acpr.set_swoscaler(swoscaler);
        acpr.write(core)?;
        trace!("{:#x?}", TPIU_ACPR::read(core)?);
    }

    /*
     * Unlock the ITM.
     */
    ITM_LAR::unlock(core)?;

    /*
     * Disable the ITM.
     */
    let mut tcr = ITM_TCR::read(core)?;
    tcr.set_itm_enable(false);
    tcr.write(core)?;

    /*
     * Spin until the ITM is not busy
     */
    while ITM_TCR::read(core)?.itm_busy() {
        continue;
    }

    /*
     * Enable the DWT to generate a synchronization packet every 8M cycles.
     */
    let mut dwt = DWT_CTRL::read(core)?;
    dwt.set_synctap(DWTSyncTapFrequency::CycCnt8M);
    dwt.set_cyccnt_enabled(true);
    dwt.write(core)?;

    /*
     * Enable stimuli
     */
    let mut ter = ITM_TER::read(core)?;
    ter.set_enabled(stimuli);
    ter.write(core)?;

    /*
     * Allow unprivileged access to all stimulus ports
     */
    let mut tpr = ITM_TPR::read(core)?;
    tpr.set_privmask(0);
    tpr.write(core)?;

    /*
     * Set the trace ID
     */
    tcr = ITM_TCR::read(core)?;
    tcr.set_traceid(traceid.into());
    tcr.set_timestamp_enable(if stimuli & 0xffff_0000 != 0 {
        true
    } else {
        false
    });

    tcr.set_sync_enable(true);
    tcr.set_itm_enable(true);
    tcr.write(core)?;

    Ok(())
}

fn itmcmd_disable(core: &mut dyn core::Core) -> Result<()> {
    /*
     * Unlock the ITM.
     */
    ITM_LAR::unlock(core)?;

    /*
     * Disable the ITM.
     */
    let mut tcr = ITM_TCR::read(core)?;
    tcr.set_itm_enable(false);
    tcr.write(core)?;

    /*
     * Now disable TRCENA in the DEMCR.
     */
    let mut val = DEMCR::read(core)?;
    val.set_trcena(false);
    val.write(core)?;

    info!("ITM disabled");

    Ok(())
}

fn itmcmd_ingest(subargs: &ItmArgs, filename: &str) -> Result<()> {
    let file = File::open(filename)?;
    let traceid = if subargs.bypass { None } else { Some(subargs.traceid) };

    let process = |packet: &ITMPacket| -> Result<()> {
        if let ITMPayload::Instrumentation { payload, .. } = &packet.payload {
            for p in payload {
                print!("{}", *p as char);
            }
        }

        Ok(())
    };

    let mut rdr = csv::Reader::from_reader(file);

    match rdr.headers() {
        Ok(_hdr) => {
            type SaleaeTraceRecord = (f64, u8, Option<String>, Option<String>);
            let mut iter = rdr.deserialize();

            itm_ingest(
                traceid,
                || {
                    if let Some(line) = iter.next() {
                        let record: SaleaeTraceRecord = line?;
                        Ok(Some((record.1, record.0)))
                    } else {
                        Ok(None)
                    }
                },
                process,
            )
        }
        Err(_) => {
            info!("not a Saleae trace file; assuming raw input");

            let mut file = File::open(filename)?;
            let mut buffer = [0; 1];

            itm_ingest(
                traceid,
                || {
                    let nbytes = file.read(&mut buffer)?;

                    match nbytes {
                        1 => Ok(Some((buffer[0], 0.0))),
                        0 => Ok(None),
                        _ => {
                            panic!("illegal read");
                        }
                    }
                },
                process,
            )
        }
    }
}

fn itmcmd_ingest_attached<F>(
    core: &mut dyn core::Core,
    coreinfo: &CoreInfo,
    subargs: &ItmArgs,
    callback: F,
) -> Result<()>
where
    F: FnMut(&itm::ITMPacket) -> Result<()>,
{
    let mut bytes: Vec<u8> = vec![];
    let mut ndx = 0;

    let traceid = if coreinfo.address(CoreSightComponent::SWO).is_some() {
        None
    } else {
        Some(subargs.traceid)
    };

    let start = Instant::now();

    itm_ingest(
        traceid,
        || {
            while ndx == bytes.len() {
                bytes = core.read_swv()?;
                ndx = 0;
            }
            ndx += 1;
            Ok(Some((bytes[ndx - 1], start.elapsed().as_secs_f64())))
        },
        callback,
    )
}

#[derive(StructOpt, Debug)]
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
        parse(try_from_str = parse_int::parse)
    )]
    traceid: u8,
    /// ingest ITM data as CSV
    #[structopt(long, short, value_name = "filename")]
    ingest: Option<String>,
    /// ingest directly from attached device
    #[structopt(long, short, conflicts_with_all = &["disable", "ingest"])]
    attach: bool,
    /// assume bypassed TPIU in ingested file
    #[structopt(long, short, requires = "ingest")]
    bypass: bool,
    /// sets the value of SWOSCALER
    #[structopt(long, short, value_name = "scaler", requires = "enable",
        parse(try_from_str = parse_int::parse),
    )]
    clockscaler: Option<u16>,
}

fn itmcmd<F>(
    hubris: &HubrisArchive,
    args: &Args,
    subargs: &ItmArgs,
    callback: F,
) -> Result<()> 
where
    F: FnMut(&itm::ITMPacket) -> Result<()>,
{
    let mut rval = Ok(());

    let traceid = subargs.traceid;

    if traceid >= HUMILITY_ETM_TRACEID_MAX {
        fatal!("traceid has a maximum value of {:x}", HUMILITY_ETM_TRACEID_MAX);
    }

    if let Some(ingest) = &subargs.ingest {
        match itmcmd_ingest(subargs, ingest) {
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
    let mut c = attach(args)?;
    let core = c.as_mut();
    let coreinfo = CoreInfo::read(core)?;

    let _info = core.halt();
    info!("core halted");

    if subargs.probe {
        rval = itmcmd_probe(core, &coreinfo);
    }

    if subargs.disable {
        rval = itmcmd_disable(core);
    }

    if subargs.enable {
        if subargs.attach {
            core.init_swv()?;
        }

        /*
         * By default, we enable all logging (ports 0-7).
         */
        let stim = 0x0000_000f;
        let clockscaler = clock_scaler(hubris, core, subargs.clockscaler)?;
        rval = itmcmd_enable(core, &coreinfo, clockscaler, traceid, stim);
    }

    core.run()?;
    info!("core resumed");

    if rval.is_ok() && subargs.attach {
        match itmcmd_ingest_attached(core, &coreinfo, subargs, callback) {
            Err(e) => {
                panic!("failed to ingest from attached device: {}", e);
            }
            _ => {
                return Ok(());
            }
        }
    }

    rval
}

#[derive(StructOpt, Debug)]
struct TestArgs {
    /// sets ITM trace identifier
    #[structopt(
        long, short, default_value = "0x3a", value_name = "identifier",
        parse(try_from_str = parse_int::parse)
    )]
    traceid: u8,
    /// sets the value of SWOSCALER
    #[structopt(long, short, value_name = "scaler",
        parse(try_from_str = parse_int::parse),
    )]
    clockscaler: Option<u16>,
    /// dump full report even on success
    #[structopt(long, short)]
    dumpalways: bool,
    /// sets the output file
    #[structopt(long, short, value_name = "filename")]
    output: Option<String>,
}

fn testcmd_ingest(
    core: &mut dyn core::Core,
    coreinfo: &CoreInfo,
    subargs: &TestArgs,
    hubris: &HubrisArchive,
) -> Result<()> {
    let mut bufs: VecDeque<(Vec<u8>, f64)> = VecDeque::new();
    let mut ndx = 0;
    let mut current = None;

    let traceid = if coreinfo.address(CoreSightComponent::SWO).is_some() {
        None
    } else {
        Some(subargs.traceid)
    };

    let v = hubris
        .lookup_variable("TEST_KICK")
        .context("does not appear to be a test archive")?;

    if v.size != 4 {
        fatal!("expected TEST_KICK to be of size 4; found {}", v.size);
    }

    let start = Instant::now();

    let mut testrun = TestRun::new(hubris);
    let mut kicked = false;

    let shared = RefCell::new(core);

    let wirebuf = vec![];
    let wire = RefCell::new(wirebuf);

    let output = subargs.output.as_ref();
    let timeout = 30;

    let rval = itm_ingest(
        traceid,
        || {
            loop {
                if start.elapsed().as_secs() > timeout {
                    bail!("timed out after {} seconds", timeout);
                }

                /*
                 * We will keep reading until we have a zero byte read, at
                 * which time we will kick out and process one byte.
                 */
                let buf = shared.borrow_mut().read_swv()?;

                if buf.len() != 0 {
                    bufs.push_back((buf, start.elapsed().as_secs_f64()));
                    continue;
                }

                match current {
                    None => {
                        current = bufs.pop_front();
                        ndx = 0;
                    }

                    Some((ref buf, _)) => {
                        if ndx == buf.len() {
                            current = bufs.pop_front();
                            ndx = 0;
                        }
                    }
                }

                if current.is_none() {
                    continue;
                }

                break;
            }

            let (buf, pulled) = current.as_ref().unwrap();
            ndx += 1;

            let datum = (buf[ndx - 1], start.elapsed().as_secs_f64());
            wire.borrow_mut().push((datum.0, *pulled, datum.1));

            Ok(Some(datum))
        },
        |packet| match &packet.payload {
            ITMPayload::Instrumentation { payload, port } => {
                let source = match *port {
                    0 => TestSource::KernelLog,
                    1 => TestSource::UserLog,
                    8 => TestSource::Suite,
                    _ => {
                        bail!("spurious data on port {}: {:x?}", port, payload);
                    }
                };

                for p in payload {
                    match testrun.consume(source, *p as char) {
                        Ok(_) => {}
                        Err(err) => {
                            testrun.report(
                                output,
                                &wire.borrow(),
                                Some(&err),
                            )?;
                            return Err(err);
                        }
                    }

                    if testrun.completed() {
                        if testrun.failed() {
                            testrun.report(output, &wire.borrow(), None)?;
                            std::process::exit(1);
                        }

                        if subargs.dumpalways {
                            testrun.report(output, &wire.borrow(), None)?;
                        }

                        std::process::exit(0);
                    }
                }

                Ok(())
            }
            ITMPayload::None => {
                match packet.header {
                    ITMHeader::Sync => {
                        if !kicked {
                            shared.borrow_mut().halt()?;
                            shared.borrow_mut().write_word_32(v.addr, 1)?;
                            shared.borrow_mut().run()?;
                            kicked = true;
                        }
                    }
                    ITMHeader::Malformed(datum) => {
                        bail!("malformed datum: 0x{:x}", datum);
                    }
                    _ => {}
                }

                Ok(())
            }
            _ => {
                bail!("unknown packet: {:x?}", packet);
            }
        },
    );

    match rval {
        Ok(_) => rval,
        Err(err) => {
            testrun.report(output, &wire.borrow(), Some(&err))?;
            Err(err)
        }
    }
}

fn testcmd(
    hubris: &HubrisArchive,
    args: &Args,
    subargs: &TestArgs,
) -> Result<()> {
    let mut c = attach_live(args)?;
    let core = c.as_mut();
    hubris.validate(core)?;

    let coreinfo = CoreInfo::read(core)?;
    let _info = core.halt();
    core.init_swv()?;

    let clockscaler = clock_scaler(hubris, core, subargs.clockscaler)?;
    let stim = 0x0000_ffff;

    itmcmd_enable(core, &coreinfo, clockscaler, subargs.traceid, stim)?;

    core.run()?;

    testcmd_ingest(core, &coreinfo, subargs, hubris)?;

    Ok(())
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
struct ReadvarArgs {
    /// list variables
    #[structopt(long, short)]
    list: bool,
    #[structopt(conflicts_with = "list")]
    variable: Option<String>,
}

fn readvar(
    hubris: &HubrisArchive,
    args: &Args,
    subargs: &ReadvarArgs,
) -> Result<()> {
    if subargs.list {
        return hubris.list_variables();
    }

    let v = match subargs.variable {
        Some(ref variable) => hubris.lookup_variable(variable)?,
        None => fatal!("expected variable (use \"-l\" to list)"),
    };

    let mut core = attach(&args)?;
    hubris.validate(core.as_mut())?;

    let mut buf: Vec<u8> = vec![];
    buf.resize_with(v.size, Default::default);
    core.read_8(v.addr, buf.as_mut_slice())?;

    let fmt = HubrisPrintFormat { indent: 0, newline: true, hex: true };
    let name = subargs.variable.as_ref().unwrap();
    let dumped = hubris.printfmt(&buf, v.goff, &fmt)?;

    println!("{} (0x{:08x}) = {}", name, v.addr, dumped);

    Ok(())
}

#[derive(StructOpt, Debug)]
struct ReadmemArgs {
    /// print out as halfwords instead of as bytes
    #[structopt(long, short, conflicts_with_all = &["word"])]
    halfword: bool,

    /// print out as words instead of as bytes
    #[structopt(long, short)]
    word: bool,

    /// address to read
    #[structopt(parse(try_from_str = parse_int::parse))]
    address: u32,

    /// length to read
    #[structopt(parse(try_from_str = parse_int::parse))]
    length: Option<usize>,
}

fn readmem(
    _hubris: &HubrisArchive,
    args: &Args,
    subargs: &ReadmemArgs,
) -> Result<()> {
    let mut core = attach(&args)?;
    let max = crate::core::CORE_MAX_READSIZE;
    let width: usize = 16;
    let size = if subargs.word {
        4
    } else if subargs.halfword {
        2
    } else {
        1
    };

    let length = match subargs.length {
        Some(length) => length,
        None => 256,
    };

    if length & (size - 1) != 0 {
        fatal!("length must be {}-byte aligned", size);
    }

    if subargs.address & (size - 1) as u32 != 0 {
        fatal!("address must be {}-byte aligned", size);
    }

    if length > max {
        fatal!("cannot read more than {} bytes", max);
    }

    let mut bytes = vec![0u8; length];

    let _info = core.halt()?;

    let rval = core.read_8(subargs.address, &mut bytes);
    core.run()?;

    if rval.is_err() {
        return rval;
    }

    let print = |line: &[u8], addr, offs| {
        print!("0x{:08x} | ", addr);

        for i in (0..width).step_by(size) {
            if i < offs || i - offs >= line.len() {
                print!(" {:width$}", "", width = size * 2);
                continue;
            }

            let slice = &line[i - offs..i - offs + size];

            print!(
                "{:0width$x} ",
                match size {
                    1 => line[i - offs] as u32,
                    2 => u16::from_le_bytes(slice.try_into().unwrap()) as u32,
                    4 => u32::from_le_bytes(slice.try_into().unwrap()) as u32,
                    _ => {
                        panic!("invalid size");
                    }
                },
                width = size * 2
            );
        }

        print!("| ");

        for i in 0..width {
            if i < offs || i - offs >= line.len() {
                print!(" ");
            } else {
                let c = line[i - offs] as char;

                if c.is_ascii() && !c.is_ascii_control() {
                    print!("{}", c);
                } else {
                    print!(".");
                }
            }
        }

        println!("");
    };

    let mut addr = subargs.address;
    let offs = (addr & (width - 1) as u32) as usize;
    addr -= offs as u32;

    /*
     * Print out header line, OpenBoot PROM style
     */
    print!("  {:8}  ", "");

    for i in (0..width).step_by(size) {
        if i == offs {
            print!(" {:>width$}", "\\/", width = size * 2);
        } else {
            print!(" {:>width$x}", i, width = size * 2);
        }
    }

    println!("");

    /*
     * Print our first line.
     */
    let lim = std::cmp::min(width - offs, bytes.len());
    print(&bytes[0..lim], addr, offs);

    if lim < bytes.len() {
        let lines = bytes[lim..].chunks(width);

        for line in lines {
            addr += width as u32;
            print(line, addr, 0);
        }
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

    Ok(())
}

#[derive(StructOpt)]
struct TasksArgs {
    /// spin pulling tasks
    #[structopt(long, short)]
    spin: bool,
    /// verbose task output
    #[structopt(long, short)]
    verbose: bool,
}

#[rustfmt::skip::macros(println)]

fn taskscmd(
    hubris: &HubrisArchive,
    args: &Args,
    subargs: &TasksArgs,
) -> Result<()> {
    let mut core = attach(&args)?;

    hubris.validate(core.as_mut())?;

    let base = core.read_word_32(hubris.lookup_symword("TASK_TABLE_BASE")?)?;
    let size = core.read_word_32(hubris.lookup_symword("TASK_TABLE_SIZE")?)?;

    let task = hubris.lookup_struct_byname("Task")?;
    let taskdesc = hubris.lookup_struct_byname("TaskDesc")?;

    let state = task.lookup_member("state")?;
    let state_enum = hubris.lookup_enum(state.goff)?;

    loop {
        core.halt()?;

        let cur =
            core.read_word_32(hubris.lookup_symword("CURRENT_TASK_PTR")?)?;

        /*
         * We read the entire task table at a go to get as consistent a
         * snapshot as possible.
         */
        let mut taskblock: Vec<u8> = vec![];
        taskblock.resize_with(task.size * size as usize, Default::default);
        core.read_8(base, taskblock.as_mut_slice())?;

        core.run()?;

        let descriptor = task.lookup_member("descriptor")?.offset as u32;
        let generation = task.lookup_member("generation")?.offset as u32;
        let state = task.lookup_member("state")?.offset as u32;

        let entry_point = taskdesc.lookup_member("entry_point")?.offset as u32;

        println!("{:2} {:8} {:18} {:3} {:9}",
            "ID", "ADDR", "TASK", "GEN", "STATE");

        let taskblock32 =
            |o| u32::from_le_bytes(taskblock[o..o + 4].try_into().unwrap());

        for i in 0..size {
            let addr = base + i * task.size as u32;
            let offs = i as usize * task.size;
            let soffs = offs + state as usize;

            let gen = taskblock[offs + generation as usize];
            let daddr = taskblock32(offs + descriptor as usize);
            let entry = core.read_word_32(daddr + entry_point)?;
            let module = hubris.instr_mod(entry).unwrap_or("<unknown>");

            println!("{:2} {:08x} {:18} {:3} {:25} {}", i, addr, module, gen,
                hubris.print(&taskblock[soffs..], state_enum.goff)?,
                if addr == cur { " <-" } else { "" });

            if subargs.verbose {
                let fmt =
                    HubrisPrintFormat { indent: 16, newline: true, hex: true };

                println!(
                    "          |\n          +----> {}\n",
                    hubris.printfmt(&taskblock[offs..], task.goff, &fmt)?
                );
            }
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
    #[structopt(long, short)]
    statemap: bool,
}

#[rustfmt::skip::macros(println)]

fn tracecmd_ingest(
    core: &mut dyn core::Core,
    coreinfo: &CoreInfo,
    subargs: &TraceArgs,
    hubris: &HubrisArchive,
    tasks: &HashMap<u32, String>,
) -> Result<()> {
    let mut bytes: Vec<u8> = vec![];
    let mut ndx = 0;

    let start = Instant::now();
    let mut ts: f64 = 0.0;
    let mut time = 0;

    let mut states: HashMap<String, i32> = HashMap::new();

    if subargs.statemap {
        let t = SystemTime::now().duration_since(SystemTime::UNIX_EPOCH)?;

        let colors = [
            "#ed441f", "#ef5b3b", "#f27357", "#f48a73", "#f6a28f", "#f8b9ab",
            "#fad0c7", "#fde8e3",
        ];

        println!("{{");
        println!("\t\"start\": [ {}, {} ],", t.as_secs(), t.subsec_nanos());
        println!("\t\"title\": \"Hubris tasks\",");
        println!("\t\"entityKind\": \"Task\",");

        println!("\t\"states\": {{");
        println!("\t\t\"Running\": \
            {{ \"value\": 0, \"color\": \"#DAF7A6\" }},");
        println!("\t\t\"Runnable\": \
            {{ \"value\": 1, \"color\": \"#9BC362\" }},");
        println!("\t\t\"InRecv\": \
            {{ \"value\": 2, \"color\": \"#e0e0e0\" }},");

        states.insert("Runnable".to_string(), 1);
        states.insert("InRecv(None)".to_string(), 2);

        for i in 0..tasks.len() {
            let name = tasks.get(&(i as u32)).unwrap();
            let s = format!("InReply(({}))", i);
            let state = 3 + i;

            states.insert(s, state as i32);
            println!("\t\t\"InReply({})\": {{ \"value\": {}, \
                \"color\": \"{}\" }}{}", name, state, colors[i],
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
    let hh = hubris.lookup_struct(
        healthy.goff.ok_or_else(|| anyhow!("incomplete Healthy structure"))?,
    )?;

    let schedstate = hubris.lookup_enum(hh.lookup_member("__0")?.goff)?;
    let mut spayload = Vec::with_capacity(schedstate.size);
    let mut task = 0;
    let mut newtask = None;

    let traceid = if coreinfo.address(CoreSightComponent::SWO).is_some() {
        None
    } else {
        Some(subargs.traceid)
    };

    itm_ingest(
        traceid,
        || {
            while ndx == bytes.len() {
                bytes = core.read_swv().unwrap();
                ts = start.elapsed().as_secs_f64();
                ndx = 0;
            }
            ndx += 1;
            Ok(Some((bytes[ndx - 1], ts)))
        },
        |packet| {
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
                            hubris.print(&spayload[..], schedstate.goff)?,
                        );
                            return Ok(());
                        }

                        let state =
                            hubris.print(&spayload[..], schedstate.goff)?;

                        println!("{{ \"time\": \"{}\", \"entity\": \"{}\", \
                        \"state\": {} }}",
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

                ITMPayload::LocalTimestamp {
                    timedelta,
                    delayed: _,
                    early: _,
                } => {
                    time += timedelta;

                    if let Some(task) = newtask {
                        if subargs.statemap {
                            println!("{{ \"time\": \"{}\", \"entity\": \"{}\", \
                            \"state\": 0 }}",
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
        },
    )
}

fn tracecmd(
    hubris: &HubrisArchive,
    args: &Args,
    subargs: &TraceArgs,
) -> Result<()> {
    let mut tasks: HashMap<u32, String> = HashMap::new();

    let mut c = attach(args)?;
    let core = c.as_mut();

    let coreinfo = CoreInfo::read(core)?;

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

    let taskblock32 =
        |o| u32::from_le_bytes(taskblock[o..o + 4].try_into().unwrap());

    for i in 0..size {
        let offs = i as usize * task.size;
        let daddr = taskblock32(offs + descriptor as usize);
        let entry = core.read_word_32(daddr + entry_point)?;
        let module = hubris.instr_mod(entry).unwrap_or("<unknown>");

        tasks.insert(i, module.to_string());
    }

    /*
     * Now enable ITM and ingest.
     */
    core.init_swv()?;
    let stim = 0xf000_0000;
    let clockscaler = clock_scaler(hubris, core, subargs.clockscaler)?;
    itmcmd_enable(core, &coreinfo, clockscaler, subargs.traceid, stim)?;
    tracecmd_ingest(core, &coreinfo, subargs, hubris, &tasks)?;

    Ok(())
}

#[derive(StructOpt)]
#[structopt(name = "humility", max_term_width = 80)]
struct Args {
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
    /// probe for any attached devices
    Probe,
    /// commands for ARM's Embedded Trace Macrocell (ETM)
    Etm(EtmArgs),
    /// commands for ARM's Instrumentation Trace Macrocell (ITM)
    Itm(ItmArgs),
    /// generate Hubris dump
    Dump(DumpArgs),
    /// Get logging over ITM
    Log(ItmArgs),
    /// print memory map, with association of regions to tasks
    Map,
    /// print archive manifest
    Manifest,
    /// read and display memory region
    Readmem(ReadmemArgs),
    /// read and display a specified Hubris variable
    Readvar(ReadvarArgs),
    /// run Hubris test suite and parse results
    Test(TestArgs),
    /// list Hubris tasks
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

            Subcommand::Dump(..)
            | Subcommand::Readvar(..)
            | Subcommand::Manifest
            | Subcommand::Test(..)
            | Subcommand::Log(..)
            | Subcommand::Tasks(..)
            | Subcommand::Trace(..)
            | Subcommand::Map => {
                fatal!("must provide a Hubris archive");
            }
            _ => {}
        }
    }

    let itm_callback = |packet: &ITMPacket| {
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
            _ => (),
        }

        Ok(())
    };

    let mut frames = vec![];

    let log_callback = |packet: &ITMPacket| {
        match &packet.payload {
            ITMPayload::Instrumentation { payload, port: _ } => {
                frames.extend_from_slice(&payload);

                // find the first byte of our frame, add two since it's two bytes long
                if let Some(position) =
                    frames.windows(2).position(|bytes| bytes == [0xde, 0x01])
                {
                    let position = position + 2;

                    // do we have a task id + frame length? we need two bytes for each of them, four total
                    if frames.len() > position + 4 {
                        // these are u16, but we want to use them as usize, so we cast after constructing it
                        let task_id = u16::from_le_bytes([
                            frames[position],
                            frames[position + 1],
                        ]);

                        // this is duplicated from hubris, probably want to de-duplicate someday
                        let task_index = task_id & (1 << 10) - 1;

                        let frame_len = u16::from_le_bytes([
                            frames[position + 2],
                            frames[position + 3],
                        ]) as usize;

                        // have we got all the data yet?
                        let total_end_position = position + 4 + frame_len;

                        if frames.len() >= total_end_position {
                            let start = position + 4;
                            let end = start + frame_len;

                            let table = {
                                let table = hubris
                                .task_elf_map()
                                .get(&task_index);

                                if table.is_none() {
                                    println!("warning: couldn't find task #{}, continuing", task_index);
                                    return Ok(());
                                }

                                let table = table.unwrap().as_ref();

                                if table.is_none() {
                                    println!("warning: couldn't find log messages for task #{}, continuing", task_index);
                                    return Ok(());
                                }

                                table.unwrap()
                            };

                            if let Ok((frame, consumed)) =
                                defmt_decoder::decode(&frames[start..end], &table)
                            {
                                // just to be on the safe side
                                assert_eq!(frame_len, consumed);

                                println!(
                                    "#{}: {}",
                                    task_index,
                                    frame.display(true)
                                );

                                let len = frames.len();
                                frames.rotate_left(end);
                                frames.truncate(len - end);
                            }
                        }
                    }
                }
            }
            _ => {}
        }
        Ok(())
    };

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

        Subcommand::Itm(subargs) => {
            match itmcmd(&hubris, &args, subargs, itm_callback) {
                Err(err) => fatal!("itm failed: {:?}", err),
                _ => std::process::exit(0),
            }
        }

        Subcommand::Log(subargs) => {
            match itmcmd(&hubris, &args, subargs, log_callback) {
                Err(err) => fatal!("log failed: {:?}", err),
                _ => std::process::exit(0),
            }
        }

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

        Subcommand::Readvar(subargs) => {
            match readvar(&hubris, &args, subargs) {
                Err(err) => fatal!("readvar failed: {:?}", err),
                _ => std::process::exit(0),
            }
        }

        Subcommand::Readmem(subargs) => {
            match readmem(&hubris, &args, subargs) {
                Err(err) => fatal!("readmem failed: {:?}", err),
                _ => std::process::exit(0),
            }
        }

        Subcommand::Tasks(subargs) => match taskscmd(&hubris, &args, subargs) {
            Err(err) => fatal!("tasks failed: {:?}", err),
            _ => std::process::exit(0),
        },

        Subcommand::Test(subargs) => match testcmd(&hubris, &args, subargs) {
            Err(err) => fatal!("test failed: {:?}", err),
            _ => std::process::exit(0),
        },

        Subcommand::Trace(subargs) => match tracecmd(&hubris, &args, subargs) {
            Err(err) => fatal!("trace failed: {:?}", err),
            _ => std::process::exit(0),
        },
    }
}
