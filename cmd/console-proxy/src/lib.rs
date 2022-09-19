// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility console-proxy`
//!
//! Act as a proxy for the host serial console when it is jumpered to the SP.

use std::path::PathBuf;

use clap::{CommandFactory, Parser};

use humility_cmd::{Archive, Attach, Command, CommandKind, Validate};

#[cfg(not(windows))]
mod posix;

#[cfg(not(windows))]
use posix::console_proxy;

#[cfg(windows)]
fn console_proxy(
    _context: &mut humility::ExecutionContext,
) -> anyhow::Result<()> {
    anyhow::bail!("the console-proxy subcommand is not available on Windows")
}

#[derive(Parser, Debug)]
#[clap(name = "console-proxy", about = env!("CARGO_PKG_DESCRIPTION"))]
struct UartConsoleArgs {
    #[clap(
        long,
        short = 'T',
        default_value_t = 5000,
        value_name = "hiffy_timeout_ms"
    )]
    hiffy_timeout: u32,

    #[clap(
        long,
        short,
        default_value_t = 1000,
        help = "frequency of polling the SP for new data",
        value_name = "poll_interval_ms"
    )]
    poll_interval: u32,

    #[clap(subcommand)]
    cmd: UartConsoleCommand,
}

#[derive(Parser, Debug)]
enum UartConsoleCommand {
    /// Attach Humility as the host console uart client via the
    /// `control_plane_agent` task.
    ///
    /// By default, puts the current terminal into raw mode (use `--no-raw` to
    /// disable this). In raw mode, exit via Ctrl-A Ctrl-X. Ctrl-A Ctrl-A will
    /// send a Ctrl-A; no other prefixed commands are recognized.
    ///
    /// When Humility attaches as the uart client, it disables the SP's default
    /// behavior of constantly reading and discarding data from the console
    /// uart. If the attach process ends cleanly, it will detach and restore the
    /// client setting to MGS (and restore the default behavior). To check
    /// whether this happened, use the `client` subcommand. To forcibly set the
    /// client back to MGS (e.g., if `attach` did not exit cleanly), use the
    /// `detach` subcommand.
    Attach {
        #[clap(
            long = "--no-raw",
            help = "do not put terminal in raw mode",
            action = clap::ArgAction::SetFalse,
        )]
        raw: bool,

        #[clap(
            long,
            help = "Specifies the input character map (i.e., special characters to be replaced when reading from the serial port). See picocom's manpage.",
            default_value = "lfcrlf"
        )]
        imap: String,

        #[clap(
            long,
            help = "Specifies the output character map (i.e., special characters to be replaced when writing to the serial port). See picocom's manpage.",
            default_value = "crlf,delbs"
        )]
        omap: String,

        #[clap(
            long,
            help = "Record all input read from the serial port to this logfile (before any remapping)."
        )]
        log: Option<PathBuf>,
    },

    /// Detach Humility as the host console uart client via the
    /// `control_plane_agent` task, setting the client back to MGS.
    ///
    /// This has the side effect of putting the SP back into its default mode in
    /// which it constantly reads from the console uart, discarding data as
    /// needed (or forwarding to MGS, if one is attached). While Humility is
    /// attached as the client, that behavior is disabled and hardware flow
    /// control is used to apply backpressure to the host.
    Detach,

    /// Print the current console client (Humility or MGS).
    Client,
}

pub fn init() -> Command {
    Command {
        app: UartConsoleArgs::command(),
        name: "console-proxy",
        run: console_proxy,
        kind: CommandKind::Attached {
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
        },
    }
}
