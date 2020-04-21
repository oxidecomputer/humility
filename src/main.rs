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

    let mut val = DEMCR::read(&core)?;
    val.set_trcena(true);
    val.write(&core)?;

    println!("{:?}", DEMCR::read(&core)?);

    println!("{:?}", DBGMCU_CR::read(&core)?);
    println!("{:?}", TPIU_SPPR::read(&core)?);

    core.run()?;

    Ok(())
}
