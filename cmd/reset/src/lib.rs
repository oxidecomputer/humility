// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility reset`
//!
//! `humility reset` will hard reset the system using the debug pin
//! or using software reset with the appropriate flag

use anyhow::Result;
use clap::{CommandFactory, Parser};
use humility_cli::{ExecutionContext, Subcommand};
use humility_cmd::{Archive, Command, CommandKind};

#[derive(Parser, Debug)]
#[clap(name = "reset", about = env!("CARGO_PKG_DESCRIPTION"))]
struct ResetArgs {
    /// Use a software reset instead of pin reset
    #[clap(long, conflicts_with_all = &["halt"])]
    soft_reset: bool,
    /// Reset and halt instead of continuing
    #[clap(long, conflicts_with_all = &["soft-reset"])]
    halt: bool,
    /// Use measurement token handoff (usually decided automatically)
    #[clap(long, conflicts_with_all = &["soft-reset", "halt"])]
    use_token: Option<bool>,
}

fn reset(context: &mut ExecutionContext) -> Result<()> {
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let subargs = ResetArgs::try_parse_from(subargs)?;

    // `context.archive` is always `Some(..)` even if we have not specified
    // anything on the command line or through environment flags. However, if no
    // archive is specified, then the actual data in the archive is default
    // constructed (??!).
    let chip = context.archive.as_mut().unwrap().chip();

    let probe = match &context.cli.probe {
        Some(p) => p,
        None => "auto",
    };

    enum Behavior<'a> {
        Halt,
        ResetWithHandoff(&'a humility::hubris::HubrisArchive),
        Reset,
    }

    let behavior = if subargs.halt {
        Behavior::Halt
    } else {
        match subargs.use_token {
            None => {
                // Detect bogus archives by looking at the chip member
                if let Some(archive) = &context.archive
                    && chip.is_some()
                {
                    Behavior::ResetWithHandoff(archive)
                } else {
                    Behavior::Reset
                }
            }
            Some(false) => Behavior::Reset,
            Some(true) => {
                if let Some(archive) = &context.archive
                    && chip.is_some()
                {
                    Behavior::ResetWithHandoff(archive)
                } else {
                    anyhow::bail!(
                        "Need a Hubris archive to use measurement token handoff"
                    )
                }
            }
        }
    };

    let mut c = if subargs.soft_reset
        || matches!(behavior, Behavior::Halt | Behavior::ResetWithHandoff(..))
    {
        let chip = chip.ok_or_else(|| {
            anyhow::anyhow!(
                "Need a chip to do a soft reset, halt after reset, or handoff"
            )
        })?;
        humility_probes_core::attach_to_chip(
            probe,
            Some(&chip),
            context.cli.speed,
        )?
    } else {
        humility_probes_core::attach_to_probe(probe, context.cli.speed)?
    };

    let r = match behavior {
        Behavior::Halt => c.reset_and_halt(std::time::Duration::from_secs(2)),
        Behavior::ResetWithHandoff(archive) => c.reset_with_handoff(archive),
        Behavior::Reset => c.reset(),
    };

    if r.is_err() {
        humility::msg!(
            "There was an error when resetting. \
            The chip may be in an unknown state!"
        );
        humility::msg!("Full error: {r:x?}");
    }

    Ok(())
}

pub fn init() -> Command {
    Command {
        app: ResetArgs::command(),
        name: "reset",
        run: reset,
        kind: CommandKind::Unattached { archive: Archive::Optional },
    }
}
