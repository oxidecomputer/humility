// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

mod env;

use anyhow::Result;
use clap::{AppSettings, ArgGroup, ArgMatches, Parser};
use env::Environment;
use humility::{core::Core, hubris::HubrisArchive, msg, net, warn};

#[derive(Parser, Debug, Clone)]
#[clap(
    name = "humility", max_term_width = 80,
    group = ArgGroup::new("hubris").multiple(false)
)]
#[clap(global_setting(AppSettings::NoAutoVersion))]
pub struct Cli {
    /// verbose messages
    #[clap(long, short)]
    pub verbose: bool,

    /// terse output
    #[clap(long, short = 'T', hide = true)]
    pub terse: bool,

    /// sets timeout for Hubris-related operations
    #[clap(
        long, default_value_t = 2000, value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    pub timeout: u32,

    /// print version information
    #[clap(long, short = 'V')]
    pub version: bool,

    /// If a system has multiple debug probes attached, the specific probe to
    /// use.  If specifying a USB probe, its index can be used (e.g.,
    /// "usb-0"); if specifying an exact probe, this is of the form
    /// "vid:pid[:serial]". If set to "archive", the archive is used rather
    /// than any attached debug probe. This may also be set via the
    /// HUMILITY_PROBE environment variable. Run "humility doc" for more
    /// information on probes.
    #[clap(long, short, group = "hubris")]
    pub probe: Option<String>,

    /// File that contains the Hubris archive. This may also be set via
    /// the HUMILITY_ARCHIVE environment variable. Run "humility doc" for
    /// more information on Hubris archives.
    #[clap(long, short, env = "HUMILITY_ARCHIVE", hide_env = true)]
    pub archive: Option<String>,

    /// Hubris dump. This may also be set via the HUMILITY_DUMP environment
    /// variable. Run "humility doc" for more information on debugging
    /// from a hubris dump.
    #[clap(long, short, group = "hubris")]
    pub dump: Option<String>,

    /// IP address of remote Hubris instance. This may also be set via the
    /// HUMILITY_IP environment variable. Run "humility doc" for more
    /// information on running Humility over a network.
    #[clap(long, short, group = "hubris")]
    pub ip: Option<net::ScopedV6Addr>,

    /// Hubris environment file. Thie may also be set via the
    /// HUMILITY_ENVIRONMENT environment variable. Run "humility doc" for
    /// more information on Humility environments.
    #[clap(long, short, env = "HUMILITY_ENVIRONMENT", hide_env = true)]
    pub environment: Option<String>,

    /// Target to use from a specified environment. This may also be set via
    /// the HUMILITY_TARGET environment variable. Run "humility doc" for
    /// for information on specifying targets within an environment.
    #[clap(long, short, requires = "environment", group = "hubris")]
    pub target: Option<String>,

    /// If multiple archives are specified in an environment, name of
    /// the archive to use.  Run "humility doc" for more information on
    /// Humility environments and their relationship to archives.
    #[clap(long, requires = "environment")]
    pub archive_name: Option<String>,

    //
    // probe-rs requires the chip to be specified when creating a session,
    // even though it is only used for flashing (which we don't use probe-rs
    // to do).  Historically, we had a `-c` option to specify this, but its
    // presence was causing confusion and it has been deprecated.  However,
    // Hubris uses Humility to flash, and specifies this option.  Because we
    // may want to use this properly in the future (namely, if/when we use
    // probe-rs to flash), we continue to accept this option (and test for its
    // presence), thereby eliminating two potential Hubris flag days.  (Until
    // it once again means something, we hide the option from the help
    // output.)
    //
    #[clap(long, short, env = "HUMILITY_CHIP", hide = true)]
    pub chip: Option<String>,

    /// List targets within an environment. Run "humility doc" for more
    /// information on Humility environments.
    #[clap(
        long = "list-targets",
        requires = "environment",
        conflicts_with = "hubris"
    )]
    pub list_targets: bool,

    #[clap(subcommand)]
    pub cmd: Option<Subcommand>,
}

#[derive(Parser, Debug, Clone)]
pub enum Subcommand {
    #[clap(external_subcommand)]
    Other(Vec<String>),
}

pub struct ExecutionContext {
    pub core: Option<Box<dyn Core>>,
    pub history: Vec<String>,
    pub archive: Option<HubrisArchive>,
    pub environment: Option<Environment>,
    pub cli: Cli,
    pub is_interactive: bool,
}

impl ExecutionContext {
    pub fn new(
        mut cli: Cli,
        m: &ArgMatches,
        is_interactive: bool,
    ) -> Result<ExecutionContext> {
        //
        // Before we do our processing, we need to check for the presence of
        // environment variables on our mutually exclusive attach options
        // (i.e., dump/probe/ip/target).  Clap has support for options being
        // set as environment variables, so why are we doing it here?  We do
        // this manually because Clap's behavior when an option has a
        // specified environment variable is to treat a variable that is in
        // the environment just as if the option had been set on the command
        // line -- but that isn't actually what we want here: if an option is
        // set on the command line, we want to ignore the fact that a
        // different mutually exclusive option may be in the environment.  So
        // we only look at the environment if none of these is set (Clap
        // assures that at most one is set on the command line), and we
        // examine the environment in an order of precence: probe, ip, target,
        // dump.  If the user has specified no command line options but
        // multiple of these in the environment, they will get the first in
        // this ordering that we find.
        //
        if cli.dump.is_none()
            && cli.probe.is_none()
            && cli.ip.is_none()
            && cli.target.is_none()
        {
            use std::env;

            if let Ok(e) = env::var("HUMILITY_PROBE") {
                cli.probe = Some(e);
            } else if let Ok(e) = env::var("HUMILITY_IP") {
                cli.ip = Some(e.parse()?);
            } else if let Ok(e) = env::var("HUMILITY_TARGET") {
                cli.target = Some(e);
            } else if let Ok(e) = env::var("HUMILITY_DUMP") {
                cli.dump = Some(e);
            }
        }

        let environment = match (&cli.environment, &cli.target) {
            (Some(ref env), Some(ref target)) => {
                let env = match Environment::from_file(env, target) {
                    Ok(e) => e,
                    Err(err) => {
                        msg!("failed to match environment: {err:?}");
                        std::process::exit(1);
                    }
                };

                //
                // Cannot specify a dump/probe/IP address and also an
                // environment and target
                //
                assert!(cli.dump.is_none());
                assert!(cli.probe.is_none());
                assert!(cli.ip.is_none());

                cli.probe = Some(env.probe.clone());

                //
                // If we have an archive on the command-line or in an environment
                // variable, we want to prefer that over whatever is in the
                // environment file -- but we also want to warn the user about
                // what is going on.
                //
                if cli.archive.is_some() {
                    let msg = if m.occurrences_of("archive") == 1 {
                        "archive on command-line"
                    } else {
                        "archive in environment variable"
                    };

                    warn!("{} overriding archive in environment file", msg);
                } else {
                    cli.archive = match env.archive(&cli.archive_name) {
                        Ok(a) => Some(a),
                        Err(e) => {
                            msg!("Failed to get archive: {e}");
                            std::process::exit(1);
                        }
                    }
                }

                Some(env)
            }

            (Some(ref env), None) => {
                if cli.list_targets {
                    let targets = match Environment::targets(env) {
                        Ok(targets) => targets,
                        Err(err) => {
                            msg!("failed to parse environment: {err:?}");
                            std::process::exit(1);
                        }
                    };

                    if cli.terse {
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

        if cli.cmd.is_none() {
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
        if cli.dump.is_some() && cli.archive.is_some() {
            match (
                m.occurrences_of("dump") == 1,
                m.occurrences_of("archive") == 1,
            ) {
                (true, true) => {
                    msg!("cannot specify both a dump and an archive");
                    std::process::exit(1);
                }

                (false, false) => {
                    msg!(
                        "both dump and archive have been set via environment \
                    variables; unset one of them, or use a command-line option \
                    to override"
                    );
                    std::process::exit(1);
                }

                (true, false) => {
                    warn!(
                        "dump on command-line overriding archive in environment"
                    );
                    cli.archive = None;
                }

                (false, true) => {
                    warn!(
                        "archive on command-line overriding dump in environment"
                    );
                    cli.dump = None;
                }
            }
        }

        Ok(ExecutionContext {
            core: None,
            history: Vec::new(),
            archive: None,
            environment,
            cli,
            is_interactive,
        })
    }
}
