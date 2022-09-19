// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility update`
//!
//! Writes a binary to the specified update target as defined in Hubris
//!
//! ```console
//! $ humility update --target ImageB update.bin
//! humility: attached via CMSIS-DAP
//! humility: Starting update using an update block size of 512
//! humility: (Erase may take a moment)
//! humility: Comitting update
//! humility: Update done.
//! ```
//!

use cmd_hiffy as humility_cmd_hiffy;

use humility::cli::Subcommand;
use humility_cmd::hiffy::*;
use humility_cmd::idol::{HubrisIdol, IdolArgument};
use humility_cmd::CommandKind;
use humility_cmd::{Archive, Attach, Command, Validate};
use humility_cmd_hiffy::HiffyLease;

use std::path::PathBuf;

use anyhow::{bail, Result};
use clap::{CommandFactory, Parser};

use indicatif::{ProgressBar, ProgressStyle};

extern crate log;

#[derive(Parser, Debug)]
#[clap(name = "update", about = "Write a software update")]
struct UpdateArgs {
    /// sets timeout
    #[clap(
        long, short = 'T', default_value_t = 50000, value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    #[clap(long)]
    /// Which image to update
    target: String,
    #[clap(long)]
    /// Don't commit the image, only write
    skip_commit: bool,
    /// path to binary to write
    path: PathBuf,
}

fn update(context: &mut humility::ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let hubris = context.archive.as_ref().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();

    let subargs = UpdateArgs::try_parse_from(subargs)?;
    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;

    let start = hubris.get_idol_command("Update.prep_image_update")?;
    let write = hubris.get_idol_command("Update.write_one_block")?;
    let finish = hubris.get_idol_command("Update.finish_image_update")?;
    let block_size = hubris.get_idol_command("Update.block_size")?;

    let blk_size = match humility_cmd_hiffy::hiffy_call(
        hubris,
        core,
        &mut context,
        &block_size,
        &[],
        None,
    )? {
        Ok(v) => v.as_base()?.as_u32().ok_or_else(|| {
            anyhow::anyhow!("Couldn't get a u32 for block size")
        })?,
        Err(e) => bail!("Hiffy error getting block size {}", e),
    };

    humility::msg!(
        "Starting update using an update block size of {}",
        blk_size
    );
    humility::msg!("(Erase may take a moment)");
    let binary_contents = std::fs::read(&subargs.path)?;

    match humility_cmd_hiffy::hiffy_call(
        hubris,
        core,
        &mut context,
        &start,
        &[("image_type", IdolArgument::String(&subargs.target))],
        None,
    )? {
        Ok(_) => (),
        Err(e) => bail!("Hiffy error doing prep {}", e),
    }

    let bar = ProgressBar::new(binary_contents.len() as u64);
    bar.set_style(ProgressStyle::default_bar().template(
        "humility: writing update image [{bar:30}] {bytes}/{total_bytes}",
    ));

    for (i, c) in binary_contents.chunks(blk_size as usize).enumerate() {
        bar.set_position((i * (blk_size as usize)) as u64);

        match humility_cmd_hiffy::hiffy_call(
            hubris,
            core,
            &mut context,
            &write,
            &[("block_num", IdolArgument::Scalar(i as u64))],
            Some(HiffyLease::Write(c)),
        )? {
            Ok(_) => (),
            Err(e) => bail!("Hiffy error writing block #{} {}", i, e),
        }
    }

    bar.finish_and_clear();
    if subargs.skip_commit {
        humility::msg!("Not committing update");
    } else {
        humility::msg!("Comitting update");

        match humility_cmd_hiffy::hiffy_call(
            hubris,
            core,
            &mut context,
            &finish,
            &[],
            None,
        )? {
            Ok(_) => (),
            Err(e) => bail!("Hiffy error committing update {}", e),
        }
    }
    humility::msg!("Update done.");
    Ok(())
}

pub fn init() -> Command {
    Command {
        app: UpdateArgs::command(),
        name: "update",
        run: update,
        kind: CommandKind::Attached {
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
        },
    }
}
