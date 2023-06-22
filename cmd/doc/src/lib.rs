// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility doc`
//!
//! Provides detailed documentation for Humility and its commands.  To
//! get documentation on Humility, run `humility doc`; to get documentation
//! for a specific command (like this one!) run `humility doc` and specify
//! the command name -- and run `humility --help` to list all commands.
//!

use anyhow::{bail, Result};
use clap::Command as ClapCommand;
use clap::{IntoApp, Parser};
use humility::hubris::HubrisArchive;
use humility_cmd::{Archive, Command, RunUnattached};
use std::collections::HashMap;
use termimad::*;

include!(concat!(env!("OUT_DIR"), "/docs.rs"));

#[derive(Parser, Debug)]
#[clap(name = "doc", about = env!("CARGO_PKG_DESCRIPTION"))]
struct DocArgs {
    /// Humility command for which to get documentation
    #[clap(value_name = "command")]
    command: Option<String>,
}

fn doc(_hubris: &mut HubrisArchive, subargs: &[String]) -> Result<()> {
    let subargs = DocArgs::try_parse_from(subargs)?;

    let text = match subargs.command {
        Some(ref cmd) => match cmd_docs(cmd) {
            Some(text) => text,
            None => {
                bail!(
                    "unrecognized command \"{}\"; run \
                    \"humility --help\" for list",
                    cmd
                );
            }
        },
        None => docs(),
    };

    let mut skin = MadSkin::default();
    skin.table.align = Alignment::Center;
    skin.code_block.align = Alignment::Center;
    skin.print_text(text);

    if let Some(ref cmd) = subargs.command {
        skin.print_text(&format!(
            "For all command line flags, run `humility {} --help`.",
            cmd
        ));
    }

    Ok(())
}

pub fn init() -> (Command, ClapCommand<'static>) {
    (
        Command::Unattached {
            name: "doc",
            archive: Archive::Ignored,
            run: RunUnattached::Subargs(doc),
        },
        DocArgs::command(),
    )
}
