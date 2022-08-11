// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use humility_cmd::{env::Environment, Args, Subcommand};

use clap::CommandFactory;
use clap::FromArgMatches;
use clap::Parser;

mod cmd;

fn main() {
    //
    // This isn't hugely efficient, but we actually parse our arguments
    // twice: the first is with our subcommands grafted into our
    // arguments to get us a unified help and error message in the event
    // of any parsing value or request for a help message; if that works,
    // we parse our arguments again but relying on the
    // external_subcommand to directive to allow our subcommand to do any
    // parsing on its own.
    //
    let (commands, clap) = cmd::init(Args::command());

    let m = clap.get_matches();
    let _args = Args::from_arg_matches(&m);

    //
    // If we're here, we know that our arguments pass muster from the
    // Structopt/ Clap perspective.
    //
    let mut args = Args::parse();

    //
    // The only condition under which we don't require a command is if
    // --version has been specified.
    //
    if args.version {
        println!("{} {}", env!("CARGO_BIN_NAME"), env!("CARGO_PKG_VERSION"));
        std::process::exit(0);
    }

    let log_level = if args.verbose { "trace" } else { "warn" };

    let env = env_logger::Env::default().filter_or("RUST_LOG", log_level);

    env_logger::init_from_env(env);

    let env = match (&args.environment, &args.target) {
        (Some(ref env), Some(ref target)) => {
            let env = match Environment::from_file(env, target) {
                Ok(e) => e,
                Err(err) => {
                    eprintln!("failed to match environment: {:?}", err);
                    std::process::exit(1);
                }
            };

            //
            // Cannot specify a dump/probe and also an environment and target
            //
            assert!(args.dump.is_none());
            assert!(args.probe.is_none());

            args.probe = Some(env.probe.clone());

            //
            // If we have an archive on the command-line or in an environment
            // variable, we want ot prefer that over whatever is in the
            // environment file -- but we also want to warn the user about
            // what is going on.
            //
            if args.archive.is_some() {
                let msg = if m.occurrences_of("archive") == 1 {
                    "archive on command-line"
                } else {
                    "archive in environment variable"
                };

                log::warn!("{} overriding archive in environment file", msg);
            } else {
                args.archive = match env.archive(&args.archive_name) {
                    Ok(a) => Some(a),
                    Err(e) => {
                        eprintln!("Failed to get archive: {}", e);
                        std::process::exit(1);
                    }
                }
            }

            Some(env)
        }

        (Some(ref env), None) => {
            if args.list_targets {
                let targets = match Environment::targets(env) {
                    Ok(targets) => targets,
                    Err(err) => {
                        eprintln!("failed to parse environment: {:?}", err);
                        std::process::exit(1);
                    }
                };

                if args.terse {
                    println!(
                        "{}",
                        targets
                            .iter()
                            .map(|(t, _)| &**t)
                            .collect::<Vec<_>>()
                            .join(", ")
                    );
                } else {
                    println!("{:15} DESCRIPTION", "TARGET");

                    for (target, description) in &targets {
                        println!(
                            "{:15} {}",
                            target,
                            match description {
                                Some(d) => d,
                                None => "-",
                            }
                        );
                    }
                }

                std::process::exit(0);
            }

            if let Err(err) = Environment::validate(env) {
                eprintln!("failed to parse environment: {:?}", err);
                std::process::exit(1);
            }

            None
        }

        _ => None,
    };

    if args.cmd.is_none() {
        eprintln!("humility failed: subcommand expected (--help to list)");
        std::process::exit(1);
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

    if let Err(err) = cmd::subcommand(&commands, &args, subargs, env.as_ref()) {
        eprintln!("humility {} failed: {:?}", subargs[0], err);
        std::process::exit(1);
    }
}

#[test]
fn validate_clap() {
    let (_, clap) = cmd::init(Args::command());
    clap.clone().debug_assert();
}
