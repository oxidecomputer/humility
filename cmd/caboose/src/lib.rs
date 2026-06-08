// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility caboose`
//!
//! Tools to read from the caboose (without an archive)

use anyhow::Result;
use clap::Parser;
use humility_cli::{ExecutionContext, humility_cmd};

#[derive(Parser, Debug)]
#[clap(name = "caboose", about = env!("CARGO_PKG_DESCRIPTION"))]
pub struct CabooseArgs {
    #[clap(subcommand)]
    cmd: CabooseCommand,
}

#[derive(Parser, Debug)]
enum CabooseCommand {
    Read,
}

fn caboose(subargs: CabooseArgs, context: &mut ExecutionContext) -> Result<()> {
    let log = context.log();
    match subargs.cmd {
        CabooseCommand::Read => {
            let mut probe = context.cli.attach_probe(None)?;
            let mut t = humility_caboose::read_tlvc_caboose(&mut probe, log)?;

            // Strip raw bytes from the end, for pretty-printing
            if let Some(tlvc_text::Piece::Bytes(bs)) = t.last()
                && bs.iter().all(|c| *c == 0xFF)
            {
                t.pop();
            }

            if t.is_empty() {
                panic!("caboose is empty");
            }

            let mut text = vec![];
            tlvc_text::save(&mut text, &t).unwrap();
            println!("{}", std::str::from_utf8(&text).unwrap());
            Ok(())
        }
    }
}

humility_cmd!(CabooseArgs, caboose);
