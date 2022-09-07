// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility auxflash`
//!
//! Tools to interact with the auxiliary flash.
use std::collections::BTreeMap;

use anyhow::{anyhow, bail, Context, Result};
use clap::{Command as ClapCommand, CommandFactory, Parser};
use colored::Colorize;

use humility::core::Core;
use humility::hubris::*;
use humility::reflect::*;
use humility_cmd::hiffy::HiffyContext;
use humility_cmd::idol::IdolArgument;
use humility_cmd::idol::IdolOperation;
use humility_cmd::{Archive, Attach, Command, Run, Validate};

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
    Status,
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

fn slot_status(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
    slot: u32,
) -> Result<[u8; 32]> {
    let op = IdolOperation::new(hubris, "AuxFlash", "read_slot_chck", None)
        .context(
            "Could not find `AuxFlash.slot_count`, \
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
    let v = v.as_struct()?;
    assert_eq!(v.name(), "AuxFlashChecksum");
    let array = v["__0"].as_array().unwrap();
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
) -> Result<()> {
    let slot_count = slot_count(hubris, core, context)?;
    println!("slot | status");
    println!("-----|--------------------");
    for i in 0..slot_count {
        print!(" {:3>} |", i);
        match slot_status(hubris, core, context, i) {
            Err(e) => print!("{:?}", e),
            Ok(v) => {
                for byte in v {
                    print!("{:0>2x}", byte);
                }
            }
        }
        println!();
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
        AuxFlashCommand::Status => auxflash_status(hubris, core, &mut context)?,
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
