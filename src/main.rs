// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use humility_cmd::{Args, Subcommand};

use structopt::StructOpt;

mod cmd;

#[macro_use]
extern crate log;

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
        metadata.starts_with("humility")
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

fn main() {
    /*
     * This isn't hugely efficient, but we actually parse our arguments
     * twice: the first is with our subcommands grafted into our
     * arguments to get us a unified help and error message in the event
     * of any parsing value or request for a help message; if that works,
     * we parse our arguments again but relying on the
     * external_subcommand to directive to allow our subcommand to do any
     * parsing on its own.
     */
    let (commands, clap) = cmd::init(Args::clap());

    let m = clap.get_matches();
    let _args = Args::from_clap(&m);

    /*
     * If we're here, we know that our arguments pass muster from the
     * Structopt/ Clap perspective.
     */
    let mut args = Args::from_args();

    if args.verbose {
        HumilityLog { level: log::LevelFilter::Trace }.enable();
    } else {
        HumilityLog { level: log::LevelFilter::Info }.enable();
    }

    //
    // Check to see if we have both a dump and an archive.  Because these
    // conflict with one another but because we allow both of them to be
    // set with an environment variable, we need to manually resolve this:
    // we want to allow an explicitly set value (that is, via the command
    // line) to win the conflict.
    //
    if args.dump.is_some() && args.archive.is_some() {
        match (m.occurrences_of("dump") == 1, m.occurrences_of("archive") == 1)
        {
            (true, true) => {
                fatal!("cannot specify both a dump and an archive");
            }

            (false, false) => {
                fatal!(
                    "both dump and archive have been set via environment \
                    variables; unset one of them, or use a command-line option \
                    to override"
                );
            }

            (true, false) => {
                warn!("dump on command-line overriding archive in environment");
                args.archive = None;
            }

            (false, true) => {
                warn!("archive on command-line overriding dump in environment");
                args.dump = None;
            }
        }
    }

    match &args.cmd {
        Subcommand::Other(ref subargs) => {
            match cmd::subcommand(&commands, &args, subargs) {
                Err(err) => fatal!("{} failed: {:?}", subargs[0], err),
                _ => std::process::exit(0),
            }
        }
    }
}
