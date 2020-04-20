use probe_rs::Error;
use probe_rs::Core;

#[macro_use]
extern crate bitfield;
extern crate log;

mod debug;
use debug::*;

mod etm;
use etm::*;

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
    let info = core.halt()?;

    let tab = read_debug_rom_table(&core)?;

    let etm = match tab.ETM {
        None => { 
            core.run()?;
            fatal!("ETM is not available on this CPU");
        }
        Some(etm) => etm
    };

    println!("{:8x}", etm);

    let etmccr = Etmccr(read_etm_reg(&core, etm, 1)?);

    if !etmccr.has_etmidr() {
        core.run()?;
        fatal!("ETMv1.3 and earlier not supported");
    }

    let etmidr = Etmidr(read_etm_reg(&core, etm, 0x079)?);
    println!("{:?}", etmidr);
    println!("{:x}", etmidr.0);

    println!("{:?}", core.registers());

    core.run()?;

    Ok(())
}
