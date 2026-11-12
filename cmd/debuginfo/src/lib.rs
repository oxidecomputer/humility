// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility debuginfo`
//!
//! TODO: Add docs

use std::path::PathBuf;

use anyhow::{Context, Result};
use clap::Parser;
use humility::hubris::HubrisArchive;
use humility_cli::{ExecutionContext, humility_cmd};

mod shapes;

#[derive(Parser, Debug)]
#[clap(name = "debuginfo", about = env!("CARGO_PKG_DESCRIPTION"))]
pub struct DebuginfoArgs {
    #[clap(subcommand)]
    cmd: DebuginfoCommand,
}

#[derive(Parser, Debug)]
enum DebuginfoCommand {
    /// TODO: Add help text
    Dump,
    /// TODO: Add help text
    Diff {
        /// TODO: Add help text
        #[clap(long)]
        against: PathBuf,
    },
}

fn debuginfo(
    subargs: DebuginfoArgs,
    context: &mut ExecutionContext,
) -> Result<()> {
    let hubris = &context.cli.archive()?;
    let new = shapes::shapes_of_archive(hubris)?;

    match subargs.cmd {
        DebuginfoCommand::Dump => {
            for (module, text) in &new {
                println!("=== {module}");
                print!("{text}");
            }
        }
        DebuginfoCommand::Diff { against } => {
            let old_hubris =
                HubrisArchive::load_from_path(&against, context.log())
                    .with_context(|| {
                        format!("loading {}", against.display())
                    })?;
            let old = shapes::shapes_of_archive(&old_hubris)?;
            let diff = shapes::diff(&old, &new);
            if diff.is_empty() {
                println!("no differences");
            } else {
                print!("{diff}");
            }
        }
    }
    Ok(())
}

humility_cmd!(DebuginfoArgs, debuginfo);
