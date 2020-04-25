use probe_rs::Core;

extern crate log;

mod debug;
use debug::*;

mod etm;
use etm::*;

mod tpiu;
use tpiu::*;

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

fn main() -> Result<(), probe_rs::Error> {
    env_logger::init();

    let core = Core::auto_attach("STM32F407VGTx")?;
    let _info = core.halt()?;

    let tab = read_debug_rom_table(&core)?;

    let etm = match tab.ETM {
        None => { 
            core.run()?;
            fatal!("ETM is not available on this CPU");
        }
        Some(etm) => etm
    };

    println!("{:8x}", etm);

    let etmccr = ETMCCR::read(&core)?;

    if !etmccr.has_etmidr() {
        core.run()?;
        fatal!("ETMv1.3 and earlier not supported");
    }

    let etmidr = ETMIDR::read(&core)?;
    println!("{:?}", etmidr);
    println!("{:x}", etmidr.0);

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
     * HCLK seems to really vary, for reasons that aren't well understood.
     * We use a SWOSCALER of 10, which has brought the TRACECLK to something
     * attainable with a Saleae.  The clock also seems to need to evenly
     * divide 4.5 MHz, which has resulted in a clock of 1.5 to 4.5 MHz in
     * practice.
     */
    let mut acpr = TPIU_ACPR::read(&core)?;
    acpr.set_swoscaler(10);
    acpr.write(&core)?;
    println!("{:#x?}", TPIU_ACPR::read(&core)?);
    
    /*
     * We are now ready to enable ETM.
     */

    /*
     * CR to 0xd90
     * TEEVR to 0x6f
     * TECR1 to EXCLUDE
     * ETM_FFRR to EXCLUDE
     * Trigger?
     * CR clears programming
     */
    println!("{:#x?}", ETMCR::read(&core)?);
    let mut etmcr = ETMCR::read(&core)?;
    etmcr.set_branch_output(true);
    etmcr.set_stall_processor(true);
    etmcr.set_port_size(1);
    etmcr.set_port_select(true);
    etmcr.set_programming(true);
    etmcr.set_power_down(false);
    println!("will write {:#x?}", etmcr);
    etmcr.write(&core)?;

    println!("wrote {:#x?}", ETMCR::read(&core)?);
    println!("{:#x?}", ETMSR::read(&core)?);

    /*
     * Set to the hard-wired always-true event
     */
    let mut teevr = ETMTEEVR::read(&core)?;
    teevr.set_resource_a(0b110_1111);
    teevr.write(&core)?;
    println!("{:#x?}", ETMTEEVR::read(&core)?);

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

    println!("{:#x?}", ETMFFLR::read(&core)?);

    println!("{:#x?}", ETMTRACEIDR::read(&core)?);
    let mut val = ETMTRACEIDR::read(&core)?;
    val.set_traceid(0x54);
    val.write(&core)?;
    println!("{:#x?}", ETMTRACEIDR::read(&core)?);

    /*
     * Finally, indicate that we are done programming!
     */
    etmcr.set_programming(false);
    etmcr.write(&core)?;

    println!("final: wrote: {:#x?}", ETMCR::read(&core)?);
    println!("{:#x?}", ETMSR::read(&core)?);

/*

    println!("{:#?}", DBGMCU_CR::read(&core)?);
    println!("{:#?}", TPIU_SPPR::read(&core)?);

    println!("{:#?}", ETMCR::read(&core)?);

    let e = ETMCR(0xd90);
    println!("{:#x?}", e);

    let e = ETMCR(0x1e1e);
    println!("{:#x?}", e);

    let e = ETMCR(0x191e);
    println!("{:#x?}", e);

    println!("{:#?}", ETMCCR::read(&core)?);
    println!("{:#?}", ETMSR::read(&core)?);
    println!("{:#?}", ETMSCR::read(&core)?);
    println!("{:#x?}", ETMLSR::read(&core)?);
    println!("{:#x?}", ETMLSR::read(&core)?);
*/

    core.run()?;

    println!("{:#x?}", ETMSR::read(&core)?);
    println!("{:#x?}", ETMSR::read(&core)?);
    println!("{:#x?}", TPIU_STMR::read(&core)?);
    println!("{:#x?}", TPIU_FSCR::read(&core)?);

    let mut val = TPIU_FSCR::read(&core)?;
    val.set_counter(10);
    val.write(&core)?;


//    loop {
        println!("{:#x?}", ETMSR::read(&core)?);
//    }

    Ok(())
}
