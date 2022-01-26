// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility reset`
//!
//! `humility reset` will hard reset the system using the debug pin
//! or using software reset with the appropriate flag

use anyhow::Result;
use clap::Command as ClapCommand;
use clap::{CommandFactory, Parser};
use humility::cli::Subcommand;
use humility_cmd::{Archive, Command};

#[derive(Parser, Debug)]
#[clap(name = "reset", about = env!("CARGO_PKG_DESCRIPTION"))]
struct ResetArgs {
    /// Use a software reset instead of pin reset
    #[clap(long)]
    soft_reset: bool,
}

fn reset(context: &mut humility::ExecutionContext) -> Result<()> {
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let subargs = ResetArgs::try_parse_from(subargs)?;

    let hubris = context.archive.as_ref().unwrap();

    let probe = match &context.cli.probe {
        Some(p) => p,
        None => "auto",
    };

    let mut c = if subargs.soft_reset {
        // The LPC55 based chips have a special reset sequence. Using this
        // requires attaching with the specific chip name. Right now there
        // isn't an easy way to get the chip name from the Hubris archive
        // so we attach to a generic chip. The result is that a soft reset
        // on LPC55 targets will work but it will produce some error messages.
        humility::msg!("Some errors may be expected when doing a soft reset");
        humility::core::attach(probe, hubris)?
    } else {
        humility::core::attach_to_probe(probe)?
    };

    let r = c.reset();

    if r.is_err() {
        humility::msg!(
            "There was an error when resetting. \
            The chip may be in an unknown state!"
        );
        humility::msg!("Full error: {:x?}", r);
    }

    Ok(())
}

pub fn init() -> (Command, ClapCommand<'static>) {
    (
        Command::Unattached {
            name: "reset",
            archive: Archive::Ignored,
            run: reset,
        },
        ResetArgs::command(),
    )
}
