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

mod hubris;
use hubris::*;

mod cmd;
mod core;
mod dwt;
mod etm;
mod itm;
mod scs;
mod swo;
mod test;
mod tpiu;

use std::convert::TryInto;

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

fn probe(hubris: &HubrisArchive, args: &Args) -> Result<()> {
    let mut core = attach(args)?;

    hubris.validate(core.as_mut())?;
    cpuinfo(hubris, core.as_mut())?;

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

            Subcommand::Dump(..) | Subcommand::Manifest => {
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

        Subcommand::Dump(subargs) => match dump(&hubris, &args, subargs) {
            Err(err) => fatal!("dump failed: {:?}", err),
            _ => std::process::exit(0),
        },

        Subcommand::Manifest => match manifest(&hubris) {
            Err(err) => fatal!("manifest failed: {:?}", err),
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
