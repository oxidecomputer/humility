// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::{anyhow, bail, Result};
use base64::write::EncoderWriter;
use clap::Command as ClapCommand;
use clap::{CommandFactory, Parser};
use hif::Op;
use humility::core::Core;
use humility::hubris::HubrisArchive;
use humility_cmd::hiffy::HiffyContext;
use humility_cmd::{Archive, Args, Attach, Command, Validate};
use std::{io, io::Write};

#[derive(Parser, Debug)]
#[clap(name = "rng", about = env!("CARGO_PKG_DESCRIPTION"))]
struct RngArgs {
    /// base64 encode / ascii armor output
    #[clap(long, short)]
    ascii: bool,

    /// size of each block read from RNG
    #[clap(long, short, default_value = "32")]
    block_size: u32,

    /// number of blocks read from RNG, if omitted bytes will be read until
    /// interrupted by the user
    #[clap(long, short)]
    count: Option<u32>,

    /// sets timeout
    #[clap(
        long, short = 'T', default_value = "5000", value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,
}

fn rng(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    _args: &Args,
    subargs: &[String],
) -> Result<()> {
    let subargs = RngArgs::try_parse_from(subargs)?;
    let max: u32 = match subargs.count {
        Some(count) => {
            if count == 0 {
                bail!("'--count' must be > 0");
            }
            count
        }
        None => u32::MAX,
    };
    if subargs.block_size == 0 {
        bail!("'--block-size' must be > 0");
    }

    let mut ctx = HiffyContext::new(hubris, core, subargs.timeout)?;
    let funcs = ctx.functions()?;
    let cmd = funcs.get("Rng", 1)?;

    let ops = vec![Op::Push32(subargs.block_size), Op::Call(cmd.id), Op::Done];

    let mut out: Box<dyn Write> = if subargs.ascii {
        Box::new(EncoderWriter::new(io::stdout(), base64::STANDARD))
    } else {
        Box::new(io::stdout())
    };

    let mut count = 0;
    while subargs.count.is_none() || count < max {
        let results = ctx.run(core, ops.as_slice(), None)?;
        let r = results[0]
            .as_ref()
            .map_err(|e| anyhow!("Got error code: {}", e))?;
        if r.len() != subargs.block_size as usize {
            bail!("wrong length read: {:x?}", r);
        }
        out.write_all(r)?;
        if subargs.count.is_some() {
            count += 1;
        }
    }
    Ok(())
}

pub fn init() -> (Command, ClapCommand<'static>) {
    (
        Command::Attached {
            name: "rng",
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
            run: rng,
        },
        RngArgs::command(),
    )
}
