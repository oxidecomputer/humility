// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility auxflash`
//!
//! Tools to interact with the auxiliary flash.
use anyhow::{anyhow, bail, Context, Result};
use clap::{Command as ClapCommand, CommandFactory, Parser};
use colored::Colorize;
use indicatif::{ProgressBar, ProgressStyle};

use humility::core::Core;
use humility::hubris::*;
use humility_cmd::hiffy::HiffyContext;
use humility_cmd::idol::IdolArgument;
use humility_cmd::idol::IdolOperation;
use humility_cmd::{Archive, Attach, Command, Run, Validate};
use humility_cmd_hiffy::HiffyLease;

const SLOT_SIZE_BYTES: usize = 1024 * 1024;
const READ_CHUNK_SIZE: usize = 256; // limited by HIFFY_SCRATCH_SIZE
const WRITE_CHUNK_SIZE: usize = 2048; // limited by HIFFY_DATA_SIZE

#[derive(Parser, Debug)]
#[clap(name = "auxflash", about = env!("CARGO_PKG_DESCRIPTION"))]
struct AuxFlashArgs {
    /// sets timeout
    #[clap(
        long, short = 'T', default_value = "5000", value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    #[clap(subcommand)]
    cmd: AuxFlashCommand,
}

#[derive(Parser, Debug)]
enum AuxFlashCommand {
    /// Prints the auxiliary flash status
    Status {
        #[clap(long, short)]
        verbose: bool,
    },
    Read {
        #[clap(long, short)]
        slot: u32,
        #[clap(long, short)]
        output: String,

        /// Number of bytes to read (defaults to 1M)
        #[clap(long, short)]
        count: Option<usize>,
    },
    Write {
        #[clap(long, short)]
        slot: u32,
        #[clap(long, short)]
        input: String,
    },
}

fn slot_count(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
) -> Result<u32> {
    let op = IdolOperation::new(hubris, "AuxFlash", "slot_count", None)
        .context(
            "Could not find `AuxFlash.slot_count`, \
             is your Hubris archive new enough?",
        )?;
    let value =
        humility_cmd_hiffy::hiffy_call(hubris, core, context, &op, &[], None)?;
    let v = match value {
        Ok(v) => v,
        Err(e) => bail!("Got Hiffy error: {}", e),
    };
    let v = v.as_base()?;
    v.as_u32().ok_or_else(|| anyhow!("Couldn't get U32"))
}

fn slot_erase(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
    slot: u32,
) -> Result<()> {
    let op = IdolOperation::new(hubris, "AuxFlash", "erase_slot", None)
        .context(
            "Could not find `AuxFlash.erase_slot`, \
             is your Hubris archive new enough?",
        )?;
    let value = humility_cmd_hiffy::hiffy_call(
        hubris,
        core,
        context,
        &op,
        &[("slot", IdolArgument::Scalar(u64::from(slot)))],
        None,
    )?;
    match value {
        Ok(..) => Ok(()),
        Err(e) => bail!("Got Hiffy error: {}", e),
    }
}

fn slot_status(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
    slot: u32,
) -> Result<[u8; 32]> {
    let op = IdolOperation::new(hubris, "AuxFlash", "read_slot_chck", None)
        .context(
            "Could not find `AuxFlash.read_slock_chck`, \
             is your Hubris archive new enough?",
        )?;
    let value = humility_cmd_hiffy::hiffy_call(
        hubris,
        core,
        context,
        &op,
        &[("slot", IdolArgument::Scalar(u64::from(slot)))],
        None,
    )?;
    let v = match value {
        Ok(v) => v,
        Err(e) => bail!("{}", e),
    };
    let array = v.as_1tuple().unwrap().as_array().unwrap();
    assert_eq!(array.len(), 32);
    let mut out = [0u8; 32];
    for (o, v) in out
        .iter_mut()
        .zip(array.iter().map(|i| i.as_base().unwrap().as_u8().unwrap()))
    {
        *o = v;
    }

    Ok(out)
}

fn auxflash_status(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
    verbose: bool,
) -> Result<()> {
    let slot_count = slot_count(hubris, core, context)?;
    println!("slot | status");
    println!("-----|----------------------------");
    for i in 0..slot_count {
        print!(" {:>3} | ", i);
        match slot_status(hubris, core, context, i) {
            Err(e) => {
                let err_str = format!("{:?}", e);
                // Special-casing for a few known error codes
                match err_str.as_str() {
                    "MissingChck" => println!("{}", "Missing CHCK".yellow()),
                    _ => println!("{}", err_str.red()),
                }
            }
            Ok(v) => {
                print!("{} (", "Checkum match".green());
                if verbose {
                    for byte in v {
                        print!("{:0>2x}", byte);
                    }
                } else {
                    for byte in &v[0..4] {
                        print!("{:0>2x}", byte);
                    }
                    print!("...");
                }
                println!(")");
            }
        }
    }
    Ok(())
}

fn auxflash_read(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
    slot: u32,
    count: Option<usize>,
) -> Result<Vec<u8>> {
    let op =
        IdolOperation::new(hubris, "AuxFlash", "read_slot_with_offset", None)
            .context(
            "Could not find `AuxFlash.read_slot_with_offset`, \
             is your Hubris archive new enough?",
        )?;

    let mut out = vec![0u8; count.unwrap_or(SLOT_SIZE_BYTES)];
    let bar = ProgressBar::new(0);
    bar.set_style(
        ProgressStyle::default_bar()
            .template("humility: reading [{bar:30}] {bytes}/{total_bytes}"),
    );
    bar.set_length(out.len() as u64);
    for (i, chunk) in out.chunks_mut(READ_CHUNK_SIZE).enumerate() {
        let offset = i * READ_CHUNK_SIZE;
        let value = humility_cmd_hiffy::hiffy_call(
            hubris,
            core,
            context,
            &op,
            &[
                ("slot", IdolArgument::Scalar(slot as u64)),
                ("offset", IdolArgument::Scalar(offset as u64)),
            ],
            Some(HiffyLease::Read(chunk)),
        )?;
        if let Err(e) = value {
            bail!("Got Hubris error: {:?}", e);
        }
        bar.set_position(offset as u64);
    }

    Ok(out)
}

fn auxflash_write(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
    slot: u32,
    data: &[u8],
) -> Result<()> {
    humility::msg!("Erasing slot {}", slot);
    slot_erase(hubris, core, context, slot)?;

    if data.len() > SLOT_SIZE_BYTES {
        bail!(
            "Data is too large ({} bytes, slot size is {} bytes)",
            data.len(),
            SLOT_SIZE_BYTES
        );
    }
    let op =
        IdolOperation::new(hubris, "AuxFlash", "write_slot_with_offset", None)
            .context(
                "Could not find `AuxFlash.write_slot_with_offset`, \
                 is your Hubris archive new enough?",
            )?;

    let bar = ProgressBar::new(0);
    bar.set_style(
        ProgressStyle::default_bar()
            .template("humility: writing [{bar:30}] {bytes}/{total_bytes}"),
    );
    bar.set_length(data.len() as u64);
    for (i, chunk) in data.chunks(WRITE_CHUNK_SIZE).enumerate() {
        let offset = i * WRITE_CHUNK_SIZE;
        let value = humility_cmd_hiffy::hiffy_call(
            hubris,
            core,
            context,
            &op,
            &[
                ("slot", IdolArgument::Scalar(slot as u64)),
                ("offset", IdolArgument::Scalar(offset as u64)),
            ],
            Some(HiffyLease::Write(chunk)),
        )?;
        if let Err(e) = value {
            bail!("Got Hubris error: {:?}", e);
        }
        bar.set_position(offset as u64);
    }
    Ok(())
}

fn auxflash(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    subargs: &[String],
) -> Result<()> {
    let subargs = AuxFlashArgs::try_parse_from(subargs)?;
    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;
    match subargs.cmd {
        AuxFlashCommand::Status { verbose } => {
            auxflash_status(hubris, core, &mut context, verbose)?;
        }
        AuxFlashCommand::Read { slot, output, count } => {
            let data = auxflash_read(hubris, core, &mut context, slot, count)?;
            std::fs::write(&output, &data)?;
        }
        AuxFlashCommand::Write { slot, input } => {
            let data = std::fs::read(&input)?;
            auxflash_write(hubris, core, &mut context, slot, &data)?;
        }
    }
    Ok(())
}

pub fn init() -> (Command, ClapCommand<'static>) {
    (
        Command::Attached {
            name: "auxflash",
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
            run: Run::Subargs(auxflash),
        },
        AuxFlashArgs::command(),
    )
}
