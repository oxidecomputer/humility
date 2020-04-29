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

use bitfield::bitfield;
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

bitfield! {
    #[derive(Copy, Clone)]
    pub struct TPIUFrameHalfWord(u16);
    impl Debug;
    data_or_aux, _: 15, 8;
    data_or_id, _: 7, 1;
    f_control, _: 0;
}

impl From<u16> for TPIUFrameHalfWord {
    fn from(value: u16) -> Self {
        Self(value)
    }
}

impl From<(u8, u8)> for TPIUFrameHalfWord {
    fn from(value: (u8, u8)) -> Self {
        Self((value.1 as u16) << 8 | (value.0 as u16))
    }
}

fn etm_tpiu_check_frame(
    frame: &Vec<(u8, f64, usize)>,
    valid: &Vec<bool>,
    intermixed: bool,
) -> bool {
    /*
     * To check a frame, we go through its half words, checking them for
     * inconsistency.  The false positive rate will very much depend on how
     * crowded the ID space is:  the sparser the valid space, the less likely
     * we are to accept a frame that is in fact invalid.
     */
    let max = frame.len() / 2;

    for i in 0..max {
        let base = i * 2;
        let half = TPIUFrameHalfWord::from((frame[base].0, frame[base + 1].0));

        if half.f_control() {
            /*
             * The two conditions under which we can reject a frame:  we
             * either have an ID that isn't expected, or we are not expecting
             * intermixed output and we have an ID on anything but the first
             * half-word of the frame.
             */
            if !valid[half.data_or_id() as usize] || (i > 0 && !intermixed) {
                return false;
            }
        }
    }

    true
}

fn etm_tpiu_check_byte(
    byte: u8,
    valid: &Vec<bool>,
) -> bool {
    let check: TPIUFrameHalfWord = (byte as u16).into();

    check.f_control() && valid[check.data_or_id() as usize]
}

fn etm_tpiu_process_frame(
    frame: &Vec<(u8, f64, usize)>,
    id: Option<u8>,
    mut callback: impl FnMut(u8, u8, f64, usize) -> Result<(), Box<dyn Error>>,
) -> Result<u8, Box<dyn Error>> {

    let high = frame.len() - 1;
    let aux = TPIUFrameHalfWord::from((frame[high - 1].0, frame[high].0));
    let max = frame.len() / 2;
    let mut current = id;

    for i in 0..max {
        let base = i * 2;
        let half = TPIUFrameHalfWord::from((frame[base].0, frame[base + 1].0));
        let auxbit = ((aux.data_or_aux() & (1 << i)) >> i) as u8;
        let last = i == max - 1;
        if half.f_control() {
            /*
             * If our bit is set, the sense of the auxiliary bit tells us
             * if the ID takes effect with this halfword (bit is clear), or
             * with the next (bit is set).
             */
            let delay = auxbit != 0;
            let id = half.data_or_id() as u8;
            let data = half.data_or_aux() as u8;
            let time = frame[base + 1].1;
            let offset = frame[base + 1].2;

            if last {
                /*
                 * If this is the last half-word, the auxiliary bit "must be
                 * ignored" (Sec. D4.2 in the ARM CoreSight Architecture
                 * Specification), and applies to subsequent record.  So in
                 * this case, we just return the ID.
                 */
                return Ok(id);
            }

            match (delay, current) {
                (false, _) => {
                        callback(id, data, time, offset)?; 
                }
                (true, Some(current)) => {
                        callback(current, data, time, offset)?;
                }
                (true, None) => {
                    /*
                     * We have no old ID -- we are going to discard this byte,
                     * but also warn about it.
                     */
                    warn!("orphaned byte discarded at offset {}", offset);
                }
            }

            current = Some(id as u8);
        } else {
            /*
             * If our bit is NOT set, the auxiliary bit is the actual bit
             * of data.  We know that our current is set:  if we are still
             * seeking a first frame, we should not be here at all.
             */
            let id = current.unwrap();
            let data: u8 = (half.data_or_id() << 1) as u8 | auxbit;
            let aux: u8 = half.data_or_aux() as u8;

            callback(id, data, frame[base].1, frame[base].2)?;

            if last {
                return Ok(id);
            }
                    
            callback(id, aux, frame[base + 1].1, frame[base + 1].2)?;
        }
    }

    /*
     * We shouldn't be able to get here:  the last half-word handling logic
     * should assure that we return from within the loop.
     */
    unreachable!();
}

fn etm_ingest(
    filename: &str,
    traceid: Option<u8>
) -> Result<(), Box<dyn Error>> {
    let file = File::open(filename)?;
    let mut rdr = csv::Reader::from_reader(file);

    enum FrameState { Searching, Framing };
    let mut state: FrameState = FrameState::Searching;

    let mut ndx = 0;
    let mut frame: Vec<(u8, f64, usize)> = vec![(0u8, 0.0, 0); 16];

    let mut nvalid = 0;
    let mut id = None;

    type TraceRecord = (f64, u8, Option<String>, Option<String>);
    let mut valid = vec![false; 256];

    let headers = rdr.headers()?;
    println!("{:?}", headers);

    let mut line = 1;
    let target = traceid.unwrap_or(HUMILITY_ETM_TRACEID);

    valid[target as usize] = true;

    let mut runlen = 0;

    let callback = move |_id, data, _time, line| {
        match data {
            0 => { runlen += 1 }
            0x80 => {
                if runlen >= 5 {
                    info!("A-sync alignment synchronization ",
                        "packet found at line {}"), line);
                    runlen = 0;
                }
            }
            _ => { runlen = 0; }
        }

        // println!("line {} id {:02x}: {:02x} {:08b}", line, id, data, data);
        Ok(())
    };

    for result in rdr.deserialize() {
        let record: TraceRecord = result?;

        line += 1;

        match state {
            FrameState::Searching => {
                if ndx == 0 && !etm_tpiu_check_byte(record.1, &valid) {
                    continue;
                }

                frame[ndx] = (record.1, record.0, line);
                ndx += 1;

                if ndx < frame.len() {
                    continue;
                }

                /*
                 * We have a complete frame.  We need to now check the entire
                 * frame.
                 */
                if etm_tpiu_check_frame(&frame, &valid, false) {
                    info!("valid TPIU frame starting at line {}", frame[0].2);
                    id = Some(etm_tpiu_process_frame(&frame, id, callback)?);
                    state = FrameState::Framing;
                    nvalid = 1;
                    ndx = 0;
                    continue;
                }

                ndx = 0;

                /*
                 * That wasn't a valid frame; we need to scan our current frame
                 * to see if there is another plausible start to the frame.
                 */
                for check in 1..frame.len() {
                    if !etm_tpiu_check_byte(frame[check].0, &valid) {
                        continue;
                    }

                    /*
                     * We have a plausible start! Scoot the rest of the frame
                     * down to the start of the frame.
                     */
                    while ndx + check < frame.len() {
                        frame[ndx] = frame[check + ndx]; 
                        ndx += 1;
                    }

                    break;
                }
            }

            FrameState::Framing => {
                frame[ndx] = (record.1, record.0, line);
                ndx += 1;

                if ndx < frame.len() {
                    continue;
                }

                /*
                 * We have a complete frame, but we more or less expect it to
                 * be correct.  Warn if this fails.
                 */
                if !etm_tpiu_check_frame(&frame, &valid, false) {
                    if nvalid == 0 {
                        warn!("two consecutive invalid frames; resuming search");
                        state = FrameState::Searching;
                    } else {
                        warn!(
                            "after {} valid frame{}, invalid frame at line {}",
                            nvalid,
                            if nvalid == 1 { "" } else { "s" },
                            line - frame.len()
                        );

                        nvalid = 0;
                    }
                } else {
                    nvalid += 1;

                    id = Some(etm_tpiu_process_frame(&frame, id, callback)?);
                }

                ndx = 0;
            }
        }
    }

    info!("{} valid TPIU frames", nvalid);

    Ok(())
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
