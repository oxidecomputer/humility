/*
 * Copyright 2020 Oxide Computer Company
 */

use probe_rs::{Core, Probe};

#[macro_use]
extern crate log;

mod debug;
use debug::*;

mod etm;
use etm::*;

mod tpiu;
use tpiu::*;

mod hubris;
use hubris::*;

use std::error::Error;
use std::fs::File;

use clap::{App, Arg, SubCommand};
use parse_int::parse;

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

impl log::Log for HumilityLog {
    fn enabled(&self, metadata: &log::Metadata) -> bool {
        metadata.level() <= self.level 
    }

    fn log(&self, record: &log::Record) {
        if !self.enabled(record.metadata()) {
            return;
        }

        if record.metadata().target() == "humility" {
            println!("humility: {}", record.args())
        } else {
            println!("humility: {} ({}): {}",
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
        match log::set_boxed_logger(Box::new(self.clone())) {
            Err(e) => {
                fatal!("unable to enable logging: {}", e);
            }
            Ok(_l) => {
                log::set_max_level(self.level);
            }
        };
    }
}

const HUMILITY_ETM_SWOSCALER: u16 = 10;
const HUMILITY_ETM_TRACEID: u8 = 0x54;
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

    Ok(())
}

fn etmcmd_enable(
    core: &probe_rs::Core,
    clockscaler: Option<u16>,
    traceid: Option<u8>,
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

    /*
     * HCLK seems to really vary, for reasons that aren't well understood.  We
     * use a default SWOSCALER of 10, which has historically brought the
     * TRACECLK to something attainable with a Saleae.  (The clock also seems
     * to need to evenly divide 4.5 MHz, which has resulted in a clock of 1.5
     * to 4.5 MHz in practice.)  Note that the size of the scaler has a direct
     * effect on probe effect:  the higher the scaler, the slower TRACECLK --
     * and therefore the more frequently that the CPU will stall on a full
     * TPIU FIFO.
     */
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
    val.set_traceid(traceid.unwrap_or(HUMILITY_ETM_TRACEID).into());
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

fn etmcmd_attach(matches: &clap::ArgMatches,
    _submatches: &clap::ArgMatches
) -> Result<probe_rs::Core, probe_rs::Error> {
    let chip = matches.value_of("chip").unwrap();

    info!("attaching as chip {} ...", chip);
    let core = Core::auto_attach(chip)?;
    info!("attached");

    Ok(core)
}

fn etmcmd_trace(
    hubris: &HubrisPackage,
    nsecs: u64,
    addr: u32,
    len: u32,
    skipped: bool
) -> Result<(), Box<dyn Error>> {

    let c = if !skipped { 'E' } else { 'N' };
    let module = hubris.instr_mod(addr).unwrap_or("<unknown>");

    println!("{:-15} {:08x} {} {} {:10}", nsecs, addr, c, len, module);

    Ok(())
}

fn etmcmd_ingest(
    hubris: &HubrisPackage,
    filename: &str,
    traceid: Option<u8>
) -> Result<(), Box<dyn Error>> {
    let file = File::open(filename)?;
    let mut rdr = csv::Reader::from_reader(file);
    let mut curaddr: Option<u32> = None;

    let config = &ETM3Config {
        alternative_encoding: true,
        context_id: 0,
        data_access: false,
        traceid: traceid.unwrap_or(HUMILITY_ETM_TRACEID),
    };

    type SaleaeTraceRecord = (f64, u8, Option<String>, Option<String>);

    let mut iter = rdr.deserialize();

    etm_ingest(&config, || {
        if let Some(line) = iter.next() {
            let record: SaleaeTraceRecord = line?;
            Ok(Some((record.1, record.0)))
        } else {
            Ok(None)
        }
    }, |packet| {
        let nsecs = (packet.time * 1_000_000_000 as f64) as u64;

        match (curaddr, packet.header) {
            (None, ETM3Header::ISync) | (Some(_), _) => {}
            (None, _) => {
                fatal!("non-ISync packet at time {}", nsecs);
            }
        }

        match packet.header {
            ETM3Header::PHeaderFormat1 { e, n } => {
                for _i in 0..e {
                    let addr = curaddr.unwrap();
                    let mut l = 0;

                    curaddr = match hubris.instr_len(addr) {
                        Some(len) => {
                            l = len;
                            Some(addr + len)
                        }
                        None => {
                            warn!("unknown instruction length at {:x}!", addr);
                            None
                        }
                    };

                    etmcmd_trace(hubris, nsecs, addr, l, false)?;
                }
        
                for _i in 0..n {
                    let addr = curaddr.unwrap();
                    let mut l = 0;

                    curaddr = match hubris.instr_len(addr) {
                        Some(len) => {
                            l = len;
                            Some(addr + len)
                        }
                        None => {
                            warn!("unknown instruction length at {:x}!", addr);
                            None
                        }
                    };

                    etmcmd_trace(hubris, nsecs, addr, l, true)?;
                }
            }
            ETM3Header::ExceptionExit |
            ETM3Header::ASync |
            ETM3Header::ISync |
            ETM3Header::BranchAddress { addr: _, c: _} => {}
            _ => {
                fatal!("unhandled packet: {:#x?}", packet);
            }
        }

        match packet.payload {
            ETM3Payload::ISync {
                context: _,
                reason: _,
                address,
                processor_state: _
            } => {
                curaddr = Some(address);
            }
            ETM3Payload::BranchAddress { addr, mask, exception: _ } => {
                curaddr = Some((curaddr.unwrap() & mask) | addr);
            }
            ETM3Payload::None => {}
        }

        Ok(())
    })
}

fn etmcmd(
    hubris: &HubrisPackage,
    matches: &clap::ArgMatches,
    submatches: &clap::ArgMatches
) -> Result<(), probe_rs::Error> {
    let mut rval = Ok(());

    let clockscaler = match submatches.value_of("clockscaler") {
        Some(str) => {
            match parse::<u16>(str) {
                Ok(val) => { Some(val) }
                Err(_e) => {
                    fatal!("clockscaler must be an unsigned 16-bit integer");
                }
            }
        }
        _ => None
    };

    let traceid = match submatches.value_of("traceid") {
        Some(str) => {
            match parse::<u8>(str) {
                Ok(val) if val < HUMILITY_ETM_TRACEID_MAX => {
                    Some(val)
                }
                _ => {
                    fatal!(
                        "traceid has a maximum value of {:x}",
                        HUMILITY_ETM_TRACEID_MAX
                    );
                }
            }
        }
        _ => None
    };

    if let Some(ingest) = submatches.value_of("ingest") {
        match etmcmd_ingest(hubris, ingest, traceid) {
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
    let core = etmcmd_attach(matches, submatches)?;
    let _info = core.halt();

    info!("core halted");

    if submatches.is_present("probe") {
        rval = etmcmd_probe(&core);
    }

    if submatches.is_present("enable") {
        rval = etmcmd_enable(&core, clockscaler, traceid);
    }

    if submatches.is_present("disable") {
        rval = etmcmd_disable(&core);
    }

    core.run()?;
    info!("core resumed");

    rval
}

fn probe(
    matches: &clap::ArgMatches,
    _submatches: &clap::ArgMatches
) -> Result<(), probe_rs::Error> {
    let chip = matches.value_of("chip").unwrap();

    let probe_list = Probe::list_all();
    info!("probes: {:?}", probe_list);

    info!("attaching as chip {} ...", chip);
    let _core = Core::auto_attach(chip)?;
    info!("attached");

    Ok(())
}

fn main() {
    let matches = App::new("humility")
        .set_term_width(80)
        .arg(
            Arg::with_name("verbose")
            .long("verbose")
            .short("v")
            .help("verbose messages")
        )
        .arg(
            Arg::with_name("chip")
            .long("chip")
            .short("c")
            .env("HUMILITY_CHIP")
            .default_value("STM32F407VGTx")
            .hide_default_value(true)
            .hide_env_values(true)
            .help("specific chip on attached device (defaults to STM32F407VGTx)")
        )
        .arg(
            Arg::with_name("package")
            .long("package")
            .short("p")
            .env("HUMILITY_PACKAGE")
            .hide_env_values(true)
            .help("directory containing Hubris package")
        )
        .subcommand(SubCommand::with_name("probe")
            .about("probe for attached devices")
        )
        .subcommand(SubCommand::with_name("etm")
            .about("commands for ARM's Embedded Trace Macrocell (ETM) facility")
            .arg(
                Arg::with_name("probe")
                .long("probe")
                .short("p")
                .help("probe for ETM capability on attached device")
                .conflicts_with_all(&["enable", "disable", "ingest"])
            )
            .arg(
                Arg::with_name("enable")
                .long("enable")
                .short("e")
                .help("enable ETM on attached device")
                .conflicts_with_all(&["disable", "ingest"])
            )
            .arg(
                Arg::with_name("disable")
                .long("disable")
                .short("d")
                .help("disable ETM on attached device")
            )
            .arg(
                Arg::with_name("traceid")
                .long("traceid")
                .short("t")
                .help("sets ETM trace identifer (defaults to 0x54)")
                .value_name("identifier")
                .conflicts_with("disable")
            )
            .arg(
                Arg::with_name("ingest")
                .long("ingest")
                .short("i")
                .help("ingest ETM data as CSV")
                .value_name("filename")
            )
            .arg(
                Arg::with_name("clockscaler")
                .long("clockscaler")
                .short("c")
                .help("sets the value of SWOSCALER")
                .value_name("scaler")
                .requires("enable")
            )
        )
        .get_matches();

    if matches.is_present("verbose") {
        HumilityLog { level: log::LevelFilter::Trace }.enable();
    } else {
        HumilityLog { level: log::LevelFilter::Info }.enable();
    }

    let mut hubris = HubrisPackage::new().map_err(|err| {
        fatal!("failed to initialize: {}", err);
    }).unwrap();

    match matches.value_of("package") {
        Some(dir) => {
            match hubris.load(dir) {
                Err(err) => {
                    fatal!("failed to load package {}: {}", dir, err);
                }
                Ok(package) => {
                    Some(package)
                }
            }
        }
        None => { None }
    };

    if let Some(submatches) = matches.subcommand_matches("probe") {
        match probe(&matches, &submatches) {
            Err(err) => { fatal!("probe failed: {} (raw: \"{:?})\"", err, err); }
            _ => { ::std::process::exit(0); }
        }
    }

    if let Some(submatches) = matches.subcommand_matches("etm") {
        match etmcmd(&hubris, &matches, &submatches) {
            Err(err) => { fatal!("etm failed: {} (raw: \"{:?})\"", err, err); }
            _ => { ::std::process::exit(0); }
        }
    }
}
