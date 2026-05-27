// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use clap::{ArgGroup, CommandFactory, FromArgMatches, Parser};
mod cmd;
use humility_cli::{Cli, ExecutionContext};

/// Main CLI entry point
///
/// The distinction between [`Cli`] and [`OuterCli`] is necessary to break a
/// dependency loop: the `Cli` is stored in the [`ExecutionContext`]; commands
/// need to take the `ExecutionContext`; and the `OuterCli` needs to depend on
/// commands.
#[derive(Parser, Debug)]
#[clap(
    name = "humility", max_term_width = 80,
    group = ArgGroup::new("hubris").multiple(false),
    disable_version_flag = true,
)]
struct OuterCli {
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
    let OuterCli { cli, cmd } = outer_cli;
    if let Some(s) = version(&cli) {
        println!("{s}");
        return std::process::ExitCode::SUCCESS;
    };

    let log_level = if cli.verbose { "trace" } else { "warn" };
    let mut context = match ExecutionContext::new(cli, &m) {
        Ok(ctx) => ctx,
        Err(e) => {
            eprintln!("failed to build execution context: {e:#}");
            return std::process::ExitCode::FAILURE;
        }
    };

    let Some(cmd) = cmd else {
        eprintln!("humility failed: subcommand expected (--help to list)");
        return std::process::ExitCode::FAILURE;
    };
    let name = cmd::name(&cmd);

    let env = env_logger::Env::default().filter_or("RUST_LOG", log_level);

    env_logger::init_from_env(env);

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

pub(crate) fn version(cli: &Cli) -> Option<String> {
    if cli.version {
        Some(format!(
            "{} {} {}",
            env!("CARGO_BIN_NAME"),
            env!("CARGO_PKG_VERSION"),
            PROBES_ENABLED
        ))
    } else {
        None
    }
}

#[test]
fn validate_clap() {
    OuterCli::command().debug_assert()
}
