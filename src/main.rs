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
            eprint!("humility: {}\n", record.args())
        } else {
            eprint!("humility: {} ({}): {}\n",
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

fn etm_probe(
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

fn etm_enable(
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
     * programming it.  Once done writing to the EMT control registers, we
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

fn etm_disable(
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

fn etm_attach(matches: &clap::ArgMatches,
    _submatches: &clap::ArgMatches
) -> Result<probe_rs::Core, probe_rs::Error> {
    let chip = matches.value_of("chip").unwrap();

    info!("attaching as chip {} ...", chip);
    let core = Core::auto_attach(chip)?;
    info!("attached");

    Ok(core)
}

#[derive(Copy, Clone, Debug)]
enum ETM3Header {
    BranchAddress { addr: u8, c: bool },
    ASync,
    CycleCount,
    ISync,
    Trigger,
    OutOfOrder { tag: u8, size: u8 },
    StoreFailed,
    ISyncCycleCount,
    OutOfOrderPlaceholder { a: bool, tag: u8 },
    VMID,
    NormalData { a: bool, size: u8 },
    Timestamp { r: bool },
    DataSuppressed,
    Ignore,
    ValueNotTraced { a: bool },
    ContextID,
    ExceptionExit,
    ExceptionEntry,
    PHeaderFormat1 { e: u8, n: u8 },
    PHeaderFormat2 { e0: bool, e1: bool }
}

fn encode(hdr: ETM3Header) -> u8 {
    match hdr {
        ETM3Header::BranchAddress { addr, c } => {
            0b0000_0001 | if c { 1 << 7 } else { 0 } | (addr & 0b011_1111) << 1
        }
        ETM3Header::ASync => 0b0000_0000,
        ETM3Header::CycleCount => 0b0000_0100,
        ETM3Header::ISync => 0b0000_1000,
        ETM3Header::Trigger => 0b0000_1100,
        ETM3Header::OutOfOrder { tag, size } => {
            0b0000_0000 | ((tag & 0b11) << 5 | (size & 0b11) << 2)
        },
        ETM3Header::StoreFailed => 0b0101_0000,
        ETM3Header::ISyncCycleCount => 0b0111_0000,
        ETM3Header::OutOfOrderPlaceholder { a, tag } => {
            0b0101_0000 | if a { 1 << 5 } else { 0 } | ((tag & 0b11) << 2)
        },
        ETM3Header::VMID => 0b0011_1100,
        ETM3Header::NormalData { a, size } => {
            0b0000_0010 | if a { 1 << 5 } else { 0 } | ((size & 0b11) << 2)
        },
        ETM3Header::Timestamp { r } => {
            0b0100_0010 | if r { 1 << 2 } else { 0 }
        },
        ETM3Header::DataSuppressed => 0b0110_0010,
        ETM3Header::Ignore => 0b0110_0110,
        ETM3Header::ValueNotTraced { a } => {
            0b01101010 | if a { 1 << 4 } else { 0 }
        },
        ETM3Header::ContextID => 0b0110_1110,
        ETM3Header::ExceptionExit => 0b0111_0110,
        ETM3Header::ExceptionEntry => 0b0111_1110,
        ETM3Header::PHeaderFormat1 { n, e } => {
            0b1000_0000 | ((n & 0b1) << 6) | ((e & 0b1111) << 2)
        }
        ETM3Header::PHeaderFormat2 { e0, e1 } => {
            0b1000_0010 |
            if e0 { 1 << 3 } else { 0 } |
            if e1 { 1 << 2 } else { 0 }
        }
    }
}

fn set(table: &mut Vec<Option<ETM3Header>>, hdr: ETM3Header)
{
    let val = encode(hdr) as usize;

    match table[val] {
        None => { table[val] = Some(hdr); }
        Some(h) => {
            panic!("two values for 0x{:x} (0b{:b}): {:?} and {:?}",
                val, val, h, hdr);
        }
    }
}

fn etm_hdrs() -> Vec<Option<ETM3Header>>
{
    let mut hdr: Vec<Option<ETM3Header>> = vec![None; 256];

    for i in 0..=0b11_1111 {
        set(&mut hdr, ETM3Header::BranchAddress { addr: i, c: true });
        set(&mut hdr, ETM3Header::BranchAddress { addr: i, c: false });
    }

    set(&mut hdr, ETM3Header::ASync);
    set(&mut hdr, ETM3Header::CycleCount);
    set(&mut hdr, ETM3Header::ISync);
    set(&mut hdr, ETM3Header::Trigger);

    for tag in 1..=0b11 {
        for size in 0..=0b11 {
            set(&mut hdr, ETM3Header::OutOfOrder { tag: tag, size: size });
        }
    }

    set(&mut hdr, ETM3Header::StoreFailed);
    set(&mut hdr, ETM3Header::ISyncCycleCount);

    for tag in 1..=0b11 {
        set(&mut hdr, ETM3Header::OutOfOrderPlaceholder { tag: tag, a: true });
        set(&mut hdr, ETM3Header::OutOfOrderPlaceholder { tag: tag, a: false });
    }

    set(&mut hdr, ETM3Header::VMID);

    for size in 0..=0b11 {
        set(&mut hdr, ETM3Header::NormalData { a: true, size: size });
        set(&mut hdr, ETM3Header::NormalData { a: false, size: size });
    }

    set(&mut hdr, ETM3Header::Timestamp { r: true });
    set(&mut hdr, ETM3Header::Timestamp { r: false });

    set(&mut hdr, ETM3Header::DataSuppressed);
    set(&mut hdr, ETM3Header::Ignore);

    set(&mut hdr, ETM3Header::ValueNotTraced { a: true });
    set(&mut hdr, ETM3Header::ValueNotTraced { a: false });

    set(&mut hdr, ETM3Header::ContextID);
    set(&mut hdr, ETM3Header::ExceptionExit);
    set(&mut hdr, ETM3Header::ExceptionEntry);

    for e in 0..=0b1111 {
        for n in 0..=0b1 {
            set(&mut hdr, ETM3Header::PHeaderFormat1 { e: e, n: n });
        }
    }

    set(&mut hdr, ETM3Header::PHeaderFormat2 { e0: false, e1: false });
    set(&mut hdr, ETM3Header::PHeaderFormat2 { e0: false, e1: true });
    set(&mut hdr, ETM3Header::PHeaderFormat2 { e0: true, e1: false });
    set(&mut hdr, ETM3Header::PHeaderFormat2 { e0: true, e1: true });
    
    hdr
}

fn etm_ingest(
    filename: &str,
    traceid: Option<u8>
) -> Result<(), Box<dyn Error>> {
    let file = File::open(filename)?;
    let mut rdr = csv::Reader::from_reader(file);
    let mut valid = vec![false; 256];

    #[derive(Copy,Clone)]
    enum IngestState { ASyncSearching, ISyncSearching, Ingesting };
    let mut state: IngestState = IngestState::ASyncSearching;

    let target = traceid.unwrap_or(HUMILITY_ETM_TRACEID);
    valid[target as usize] = true;

    let mut runlen = 0;

    let hdrs = &etm_hdrs();

    println!("{:#x?}", hdrs);

    tpiu_ingest(&mut rdr, &valid, move |_id, data, _time, line| {
        match state {
            IngestState::ASyncSearching => {
                match data {
                    0 => { runlen += 1 }
                    0x80 => {
                        if runlen >= 5 {
                            info!(concat!("A-sync alignment synchronization ",
                                "packet found at line {}"), line);
                            state = IngestState::ISyncSearching;
                        }
                    }
                    _ => { runlen = 0; }
                }
            }
            IngestState::ISyncSearching => {
                let hdr = hdrs[data as usize];

                println!("{:?}", hdr);
                ::std::process::exit(0);
            }
            IngestState::Ingesting => {
                panic!("ingesting");
            }
        }

        Ok(())
    })
}

fn etm(matches: &clap::ArgMatches,
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
        match etm_ingest(ingest, traceid) {
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
    let core = etm_attach(matches, submatches)?;
    let _info = core.halt();

    info!("core halted");

    if submatches.is_present("probe") {
        rval = etm_probe(&core);
    }

    if submatches.is_present("enable") {
        rval = etm_enable(&core, clockscaler, traceid);
    }

    if submatches.is_present("disable") {
        rval = etm_disable(&core);
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

    if let Some(submatches) = matches.subcommand_matches("probe") {
        match probe(&matches, &submatches) {
            Err(err) => { fatal!("probe failed: {} (raw: \"{:?})\"", err, err); }
            _ => { ::std::process::exit(0); }
        }
    }

    if let Some(submatches) = matches.subcommand_matches("etm") {
        match etm(&matches, &submatches) {
            Err(err) => { fatal!("etm failed: {} (raw: \"{:?})\"", err, err); }
            _ => { ::std::process::exit(0); }
        }
    }
}
