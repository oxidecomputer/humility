// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::{bail, Result};
use clap::{CommandFactory, Parser};
use humility::cli::Subcommand;
use humility::core::Core;
use humility::hubris::*;
use humility::warn;
use humility_cmd::{attach_live, CommandKind};
use humility_cmd::{Archive, Command};
use humility_cortex::debug::*;
use humility_cortex::etm::*;
use humility_cortex::scs::*;
use humility_cortex::tpiu::*;
use std::fs::File;
use std::time::Instant;

#[derive(Parser, Debug)]
#[clap(name = "etm", about = env!("CARGO_PKG_DESCRIPTION"))]
struct EtmArgs {
    /// probe for ETM capability on attached device
    #[clap(
        long, short, conflicts_with_all = &["enable", "disable", "ingest"]
    )]
    probe: bool,
    /// enable ETM on attached device
    #[clap(long, short, conflicts_with_all = &["disable", "ingest"])]
    enable: bool,
    /// disable ETM on attached device
    #[clap(long, short)]
    disable: bool,
    /// sets ETM trace identifier
    #[clap(
        long, short, value_name = "identifier",
        default_value_t = 0x54, parse(try_from_str = parse_int::parse),
    )]
    traceid: u8,
    /// ingest ETM data as CSV
    #[clap(long, short, value_name = "filename")]
    ingest: Option<String>,
    /// flowindent ingested data
    #[clap(long, short = 'F')]
    flowindent: bool,
    /// sets the value of SWOSCALER
    #[clap(
        long, short, value_name = "scaler", requires = "enable",
        parse(try_from_str = parse_int::parse)
    )]
    clockscaler: Option<u16>,
    /// output ETM data as CSV
    #[clap(long, short, conflicts_with = "ingest")]
    output: bool,
}

struct TraceInstruction {
    nsecs: u64,
    addr: u32,
    _len: u32,
    target: Option<HubrisTarget>,
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

const HUMILITY_ETM_SWOSCALER: u16 = 7;
const HUMILITY_ETM_TRACEID_MAX: u8 = 0x7f;
const HUMILITY_ETM_ALWAYSTRUE: u32 = 0b110_1111;

fn etmcmd_probe(core: &mut dyn Core) -> Result<()> {
    let coreinfo = CoreInfo::read(core)?;

    let etm = match coreinfo.address(CoreSightComponent::ETM) {
        None => {
            bail!("ETM is not available on this CPU");
        }
        Some(etm) => etm,
    };

    if etm != ETMCR::ADDRESS {
        bail!(
            "ETM base address (0x{:x}) is not at expected location ({:x})",
            etm,
            ETMCR::ADDRESS
        );
    }

    let etmccr = ETMCCR::read(core)?;
    humility::msg!("{:#x?}", etmccr);

    if !etmccr.has_etmidr() {
        warn!("ETMv1.3 and earlier not supported");
        return Ok(());
    }

    let etmidr = ETMIDR::read(core)?;
    humility::msg!("{:#x?}", etmidr);

    let etmccer = ETMCCER::read(core)?;
    humility::msg!("{:#x?}", etmccer);

    Ok(())
}

fn etmcmd_enable(
    core: &mut dyn Core,
    clockscaler: Option<u16>,
    traceid: u8,
) -> Result<()> {
    let etmccr = ETMCCR::read(core)?;

    if !etmccr.has_etmidr() {
        warn!("ETMv1.3 and earlier not supported");
        return Ok(());
    }

    let etmidr = ETMIDR::read(core)?;
    log::trace!("{:?}", etmidr);

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

    //
    // First, enable TRCENA in the DEMCR.
    //
    let mut val = DEMCR::read(core)?;
    val.set_trcena(true);
    val.write(core)?;

    //
    // Now unlock the ETM.
    //
    ETMLAR::unlock(core)?;

    //
    // STM32F407-specific: enable TRACE_IOEN in the DBGMCU_CR, and set the
    // trace mode to be asynchronous.
    //
    let mut val = STM32F4_DBGMCU_CR::read(core)?;
    val.set_trace_ioen(true);
    val.set_trace_mode(0);
    val.write(core)?;

    //
    // Now setup the TPIU.
    //
    let mut val = TPIU_SPPR::read(core)?;
    val.set_txmode(TPIUMode::NRZ);
    val.write(core)?;

    let mut val = TPIU_FFCR::read(core)?;
    val.set_continuous_formatting(true);
    val.write(core)?;

    let mut acpr = TPIU_ACPR::read(core)?;
    acpr.set_swoscaler(clockscaler.unwrap_or(HUMILITY_ETM_SWOSCALER).into());
    acpr.write(core)?;
    log::trace!("{:#x?}", TPIU_ACPR::read(core)?);

    //
    // We are now ready to enable ETM.  There are a bunch of steps involved
    // in this, but we need to first write to the ETMCR to indicate that
    // we are programming it.  Once done writing to the ETM control
    // registers, we need to write to ETMCR again to indicate that we are
    // done programming it.
    //
    log::trace!("{:#x?}", ETMCR::read(core)?);
    let mut etmcr = ETMCR::read(core)?;
    etmcr.set_branch_output(true);
    etmcr.set_stall_processor(true);
    etmcr.set_port_size(1);
    etmcr.set_port_select(true);
    etmcr.set_programming(true);
    etmcr.set_power_down(false);
    log::trace!("will write {:#x?}", etmcr);
    etmcr.write(core)?;

    //
    // Set to the hard-wired always-true event
    //
    let mut teevr = ETMTEEVR::read(core)?;
    teevr.set_resource_a(HUMILITY_ETM_ALWAYSTRUE);
    teevr.write(core)?;
    log::trace!("{:#x?}", ETMTEEVR::read(core)?);

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

    log::trace!("{:#x?}", ETMFFLR::read(core)?);

    log::trace!("{:#x?}", ETMTRACEIDR::read(core)?);
    let mut val = ETMTRACEIDR::read(core)?;
    val.set_traceid(traceid.into());
    val.write(core)?;
    log::trace!("{:#x?}", ETMTRACEIDR::read(core)?);

    //
    // Finally, indicate that we are done programming!
    //
    etmcr.set_programming(false);
    etmcr.write(core)?;

    humility::msg!("ETM enabled");

    Ok(())
}

fn etmcmd_disable(core: &mut dyn Core) -> Result<()> {
    let mut etmcr = ETMCR::read(core)?;

    if etmcr.power_down() {
        humility::msg!("ETM not enabled");
        return Ok(());
    }

    etmcr.set_programming(true);
    etmcr.write(core)?;

    etmcr.set_power_down(true);
    etmcr.write(core)?;

    etmcr.set_programming(false);
    etmcr.write(core)?;

    humility::msg!("ETM disabled");

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

    state.inlined.clear();

    state.target = instr.target;

    match instr.target {
        Some(HubrisTarget::Call(_)) | Some(HubrisTarget::IndirectCall) => {
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

        Some(HubrisTarget::Return) => {
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
    let mut target: (Option<u32>, Option<HubrisTarget>) = (None, None);

    let mut state = TraceState::default();

    etm_ingest(
        econfig,
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

                    bail!("non-ISync packet at time {}", nsecs);
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
                    bail!("unhandled packet: {:#x?}", packet);
                }
            }

            match packet.payload {
                ETM3Payload::ISync { address, .. } => {
                    if broken {
                        warn!("re-railing at offset {}", packet.offset);
                        broken = false;
                        target = (None, None);
                    }

                    curaddr = Some(address);
                    lastaddr = curaddr;
                }
                ETM3Payload::BranchAddress { addr, mask, exception } => {
                    curaddr = Some((lastaddr.unwrap() & mask) | addr);
                    lastaddr = curaddr;

                    match target {
                        (
                            Some(origin),
                            Some(HubrisTarget::Direct(expected)),
                        )
                        | (Some(origin), Some(HubrisTarget::Call(expected))) => {
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

                        (Some(origin), None) => {
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

fn etmcmd_output(core: &mut dyn Core) -> Result<()> {
    let start = Instant::now();

    println!("Time [s],Value,Parity Error,Framing Error");

    loop {
        let bytes = core.read_swv().unwrap();

        for b in bytes {
            println!("{:.15},0x{:02X},,", start.elapsed().as_secs_f64(), b);
        }
    }
}

fn etmcmd(context: &mut humility::ExecutionContext) -> Result<()> {
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let hubris = context.archive.as_ref().unwrap();

    let subargs = &EtmArgs::try_parse_from(subargs)?;
    let mut rval = Ok(());

    let traceid = subargs.traceid;

    if traceid >= HUMILITY_ETM_TRACEID_MAX {
        bail!("traceid has a maximum value of {:x}", HUMILITY_ETM_TRACEID_MAX);
    }

    if let Some(ingest) = &subargs.ingest {
        let config = TraceConfig {
            hubris,
            flowindent: subargs.flowindent,
            traceid: subargs.traceid,
        };

        match etmcmd_ingest(&config, ingest) {
            Err(e) => {
                bail!("failed to ingest {}: {}", ingest, e);
            }
            _ => {
                return Ok(());
            }
        }
    }

    //
    // For all of the other commands, we need to actually attach to the chip.
    //
    let mut core = attach_live(&context.cli, hubris)?;
    core.halt()?;

    humility::msg!("core halted");

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
    humility::msg!("core resumed");

    if subargs.output {
        match etmcmd_output(core.as_mut()) {
            Err(e) => {
                bail!("failed to output from attached device: {}", e);
            }
            _ => {
                return Ok(());
            }
        }
    }

    rval
}

pub fn init() -> Command {
    Command {
        app: EtmArgs::command(),
        name: "etm",
        run: etmcmd,
        kind: CommandKind::Unattached { archive: Archive::Required },
    }
}
