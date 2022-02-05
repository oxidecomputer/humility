// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use humility_cmd::{Args, Subcommand};

use clap::FromArgMatches;
use clap::IntoApp;
use clap::Parser;

mod cmd;

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
    let (commands, clap) = cmd::init(Args::into_app());

    let m = clap.get_matches();
    let _args = Args::from_arg_matches(&m);

    /*
     * If we're here, we know that our arguments pass muster from the
     * Structopt/ Clap perspective.
     */
    let mut args = Args::parse();

    //
    // The only condition under which we don't require a command is if
    // --version has been specified.
    //
    if args.version {
        println!("{} {}", env!("CARGO_BIN_NAME"), env!("CARGO_PKG_VERSION"));
        std::process::exit(0);
    } else if args.cmd.is_none() {
        eprintln!("humility failed: subcommand expected (--help to list)");
        std::process::exit(1);
    }

    let log_level = if args.verbose { "trace" } else { "info" };

    let env = env_logger::Env::default().filter_or("RUST_LOG", log_level);

    env_logger::init_from_env(env);

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
                log::error!("cannot specify both a dump and an archive");
                std::process::exit(1);
            }

            (false, false) => {
                log::error!(
                    "both dump and archive have been set via environment \
                    variables; unset one of them, or use a command-line option \
                    to override"
                );
                std::process::exit(1);
            }

            (true, false) => {
                log::warn!(
                    "dump on command-line overriding archive in environment"
                );
                args.archive = None;
            }

            (false, true) => {
                log::warn!(
                    "archive on command-line overriding dump in environment"
                );
                args.dump = None;
            }
        }
    }

    //
    // This unwrap is safe -- we have checked that cmd is non-None above.
    //
    let Subcommand::Other(subargs) = args.cmd.as_ref().unwrap();

    if let Err(err) = cmd::subcommand(&commands, &args, subargs) {
        eprintln!("humility {} failed: {:?}", subargs[0], err);
        std::process::exit(1);
    }
}
