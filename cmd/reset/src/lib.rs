// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility reset`
//!
//! `humility reset` will hard reset the system using the debug pin
//! or using software reset with the appropriate flag

use anyhow::Result;
use clap::{CommandFactory, Parser};
use humility::cli::Subcommand;
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
}

fn reset(context: &mut humility::ExecutionContext) -> Result<()> {
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let subargs = ResetArgs::try_parse_from(subargs)?;

    let hubris = context.archive.as_mut().unwrap();

    let probe = match &context.cli.probe {
        Some(p) => p,
        None => "auto",
    };

    let mut c = if subargs.soft_reset || subargs.halt {
        let chip = hubris.chip().ok_or_else(|| {
            anyhow::anyhow!(
                "Need a chip to do a soft reset or halt after reset"
            )
        })?;
        humility::core::attach_to_chip(probe, hubris, Some(&chip))?
    } else {
        humility::core::attach_to_probe(probe)?
    };

    let r = if subargs.halt {
        c.reset_and_halt(std::time::Duration::from_secs(2))
    } else {
        c.reset()
    };

    if r.is_err() {
        humility::msg!(
            "There was an error when resetting. \
            The chip may be in an unknown state!"
        );
        humility::msg!("Full error: {:x?}", r);
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
