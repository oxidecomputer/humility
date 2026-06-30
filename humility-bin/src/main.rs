// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use clap::{
    ArgGroup, CommandFactory, FromArgMatches, Parser, parser::ValueSource,
};
mod cmd;
use humility::log::{info, warn};
use humility_cli::{Cli, ExecutionContext, env::Environment};
use tracing_subscriber::{EnvFilter, filter::LevelFilter};

#[cfg_attr(feature = "probes", doc = "Debugger for Hubris")]
#[cfg_attr(
    not(feature = "probes"),
    doc = "Debugger for Hubris (probeless build)"
)]
#[derive(Parser, Debug)]
#[clap(
    name = "humility", max_term_width = 80,
    group = ArgGroup::new("hubris").multiple(false),
    disable_version_flag = true,
)]
struct OuterCli {
    /// Inner [`Cli`] object
    ///
    /// The distinction between [`Cli`] and [`OuterCli`] is necessary to break a
    /// dependency loop: the `Cli` is stored in the [`ExecutionContext`]; commands
    /// need to take the `ExecutionContext`; and the `OuterCli` needs to depend on
    /// commands.
    #[clap(flatten)]
    cli: Cli,

    #[clap(subcommand)]
    cmd: Option<cmd::Subcommand>,
}

fn main() -> std::process::ExitCode {
    let m = OuterCli::command().get_matches();
    let outer_cli = match OuterCli::from_arg_matches(&m) {
        Ok(c) => c,
        Err(e) => {
            eprint!("{e:#}");
            return std::process::ExitCode::FAILURE;
        }
    };
    let OuterCli { mut cli, cmd } = outer_cli;
    let log = cli.log().clone();

    // The --version (-V) and --list-targets operations are implemented as
    // flags, rather than subcommands, so we'll special-case them here.
    if cli.version {
        println!(
            "{} {} {}",
            env!("CARGO_BIN_NAME"),
            env!("CARGO_PKG_VERSION"),
            PROBES_ENABLED
        );
        if cmd.is_some() {
            warn!(log, "ignoring subcommand after printing version");
        }
        return std::process::ExitCode::SUCCESS;
    } else if cli.list_targets {
        let env = cli.environment.as_ref().expect("checked by clap");
        let targets = match Environment::targets(env) {
            Ok(targets) => targets,
            Err(err) => {
                info!(log, "failed to parse environment: {err:?}");
                return std::process::ExitCode::FAILURE;
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

        if cmd.is_some() {
            warn!(log, "ignoring subcommand after listing targets");
        }
        return std::process::ExitCode::SUCCESS;
    }

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
    if cli.dump.is_none()
        && cli.probe.is_none()
        && cli.ip.is_none()
        && cli.target.is_none()
    {
        use std::env;

        if let Ok(e) = env::var("HUMILITY_PROBE") {
            cli.probe = Some(e);
        } else if let Ok(e) = env::var("HUMILITY_IP") {
            cli.ip = Some(e.parse().unwrap_or_else(|x| match x {}));
        } else if let Ok(e) = env::var("HUMILITY_TARGET") {
            cli.target = Some(e);
        } else if let Ok(e) = env::var("HUMILITY_DUMP") {
            cli.dump = Some(std::path::PathBuf::from(e));
        }
    }

    let environment = match (&cli.environment, &cli.target) {
        (Some(env), Some(target)) => {
            let env = match Environment::from_file(env, target) {
                Ok(e) => e,
                Err(err) => {
                    info!(log, "failed to match environment: {err:?}");
                    return std::process::ExitCode::FAILURE;
                }
            };

            // Cannot specify a dump/probe/IP address and also an
            // environment and target (enforced by clap)
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
                let msg = if m.value_source("archive")
                    == Some(ValueSource::CommandLine)
                {
                    "archive on command-line"
                } else {
                    "archive in environment variable"
                };

                warn!(log, "{msg} overriding archive in environment file");
            } else {
                cli.archive = match env.archive(&cli.archive_name) {
                    Ok(a) => Some(a),
                    Err(e) => {
                        info!(log, "Failed to get archive: {e}");
                        return std::process::ExitCode::FAILURE;
                    }
                }
            }

            Some(env)
        }

        (Some(env), None) => {
            if let Err(err) = Environment::validate(env) {
                eprintln!("failed to parse environment: {err:?}");
                return std::process::ExitCode::FAILURE;
            }

            None
        }

        _ => None,
    };

    //
    // Check to see if we have both a dump and an archive.  Because these
    // conflict with one another but because we allow both of them to be
    // set with an environment variable, we need to manually resolve this:
    // we want to allow an explicitly set value (that is, via the command
    // line) to win the conflict.
    //
    if cli.dump.is_some() && cli.archive.is_some() {
        match (
            m.value_source("dump") == Some(ValueSource::CommandLine),
            m.value_source("archive") == Some(ValueSource::CommandLine),
        ) {
            (true, true) => {
                info!(log, "cannot specify both a dump and an archive");
                return std::process::ExitCode::FAILURE;
            }

            (false, false) => {
                info!(
                    log,
                    "both dump and archive have been set via environment \
                        variables; unset one of them, or use a command-line \
                        option to override"
                );
                return std::process::ExitCode::FAILURE;
            }

            (true, false) => {
                warn!(
                    log,
                    "dump on command-line overriding archive in environment"
                );
                cli.archive = None;
            }

            (false, true) => {
                warn!(
                    log,
                    "archive on command-line overriding dump in environment"
                );
                cli.dump = None;
            }
        }
    }

    let level = if cli.verbose {
        LevelFilter::TRACE.into()
    } else {
        LevelFilter::WARN.into()
    };

    let mut context = ExecutionContext { environment, cli };
    let Some(cmd) = cmd else {
        eprintln!("humility failed: subcommand expected (--help to list)");
        return std::process::ExitCode::FAILURE;
    };
    let name = cmd::name(&cmd);

    // probe-rs now does tracing
    tracing_subscriber::fmt()
        .with_env_filter(
            EnvFilter::builder().with_default_directive(level).from_env_lossy(),
        )
        .init();

    if let Err(err) = cmd::dispatch(cmd, &mut context) {
        eprintln!("humility {name} failed: {:?}", err);
        std::process::ExitCode::FAILURE
    } else {
        std::process::ExitCode::SUCCESS
    }
}

#[cfg(feature = "probes")]
static PROBES_ENABLED: &str = "";

#[cfg(not(feature = "probes"))]
static PROBES_ENABLED: &str = "probeless";

#[test]
fn validate_clap() {
    OuterCli::command().debug_assert()
}
