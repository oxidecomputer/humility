// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility extract`
//!

use anyhow::{bail, Result};
use clap::Command as ClapCommand;
use clap::{CommandFactory, Parser};
use humility::hubris::HubrisArchive;
use humility_cmd::{Args, Command};
use std::fs::File;
use std::io::Cursor;
use std::io::{self, Read, Write};

#[derive(Parser, Debug)]
#[clap(name = "extract", about = env!("CARGO_PKG_DESCRIPTION"))]
struct ExtractArgs {
    /// list contents
    #[clap(long, short)]
    list: bool,

    /// file for output
    #[clap(long, short)]
    output: Option<String>,

    /// Optional file to extract
    file: Option<String>,
}

fn extract(
    hubris: &mut HubrisArchive,
    _args: &Args,
    subargs: &[String],
) -> Result<()> {
    let archive = hubris.archive();
    let subargs = ExtractArgs::try_parse_from(subargs)?;

    if subargs.list {
        let cursor = Cursor::new(archive);
        let mut archive = zip::ZipArchive::new(cursor)?;

        println!("{:>12} NAME", "SIZE");

        for i in 0..archive.len() {
            let file = archive.by_index(i)?;
            println!("{:12} {}", file.size(), file.name());
        }

        return Ok(());
    }

    let buffer = if let Some(ref filename) = subargs.file {
        let cursor = Cursor::new(archive);
        let mut archive = zip::ZipArchive::new(cursor)?;
        let mut found = vec![];

        for i in 0..archive.len() {
            let file = archive.by_index(i)?;

            if file.name().contains(filename) {
                found.push((i, file.name().to_string()));
            }
        }

        if found.is_empty() {
            bail!(
                "\"{}\" doesn't match any files (\"--list\" to list)",
                filename
            );
        }

        if found.len() > 1 {
            bail!(
                "\"{}\" matches multiple files: {}",
                filename,
                found
                    .iter()
                    .map(|(_, name)| format!("\"{}\"", name))
                    .collect::<Vec<_>>()
                    .join(", ")
            );
        }

        humility::msg!("extracting {} to stdout", found[0].1);

        let mut file = archive.by_index(found[0].0)?;

        let mut buffer = Vec::new();
        file.read_to_end(&mut buffer)?;
        buffer
    } else {
        archive.to_vec()
    };

    if let Some(output) = subargs.output {
        let mut ofile = File::create(output)?;
        ofile.write_all(&buffer)?;
    } else {
        //
        // As a precaution against naive use, we force an output file to be
        // specified if the entire archive is to be written.
        //
        if subargs.file.is_none() {
            bail!("must specify output file name to extract entire archive");
        }

        io::stdout().write_all(&buffer)?;
    }

    Ok(())
}

pub fn init() -> (Command, ClapCommand<'static>) {
    (Command::Raw { name: "extract", run: extract }, ExtractArgs::command())
}
