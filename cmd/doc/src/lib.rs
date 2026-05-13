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

use anyhow::{Result, bail};
use clap::Parser;
use humility_cli::{ExecutionContext, HumilitySubcommand};
use std::collections::HashMap;
use termimad::*;

include!(concat!(env!("OUT_DIR"), "/docs.rs"));

#[derive(Parser, Debug)]
#[clap(name = "doc", about = env!("CARGO_PKG_DESCRIPTION"))]
pub struct DocArgs {
    /// Humility command for which to get documentation
    #[clap(value_name = "command")]
    command: Option<String>,
}

fn doc(subargs: DocArgs, _context: &mut ExecutionContext) -> Result<()> {
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

    let options = minimad::Options::default().continue_spans(true);
    let text = minimad::parse_text(text, options);

    let mut skin = MadSkin::default();
    skin.table.align = Alignment::Center;
    skin.code_block.align = Alignment::Center;
    let text = FmtText::from_text(&skin, text, Some(80));
    println!("{}", &text);

    if let Some(ref cmd) = subargs.command {
        skin.print_text(&format!(
            "For all command line flags, run `humility {} --help`.",
            cmd
        ));
    }

    Ok(())
}

pub type Args = DocArgs;
impl HumilitySubcommand for Args {
    fn run(args: Self, context: &mut ExecutionContext) -> Result<()> {
        doc(args, context)
    }
}
