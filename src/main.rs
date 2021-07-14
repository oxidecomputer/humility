/*
 * Copyright 2020 Oxide Computer Company
 */

#[macro_use]
extern crate log;

#[macro_use]
extern crate num_derive;

use anyhow::{bail, Result};

use structopt::StructOpt;

mod hubris;
use hubris::*;

mod cmd;
mod core;
mod debug;
mod dwt;
mod etm;
mod hiffy;
mod itm;
mod scs;
mod swo;
mod test;
mod tpiu;

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

fn attach_live(args: &Args) -> Result<Box<dyn core::Core>> {
    if let Some(_) = &args.dump {
        bail!("must be run against a live system");
    } else {
        let probe = match &args.probe {
            Some(p) => p,
            None => "auto",
        };

        crate::core::attach(probe, &args.chip)
    }
}

fn attach_dump(
    args: &Args,
    hubris: &HubrisArchive,
) -> Result<Box<dyn core::Core>> {
    if let Some(dump) = &args.dump {
        crate::core::attach_dump(dump, hubris)
    } else {
        bail!("must be run against a dump");
    }
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
    }

    match &args.cmd {
        Subcommand::Other(ref subargs) => {
            match cmd::subcommand(&commands, &mut hubris, &args, subargs) {
                Err(err) => fatal!("{} failed: {:?}", subargs[0], err),
                _ => std::process::exit(0),
            }
        }
    }
}
