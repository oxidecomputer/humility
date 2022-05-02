// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use humility::core::Core;
use humility::hubris::*;
use humility_cmd::hiffy::*;
use humility_cmd::{Archive, Args, Attach, Command, Dumper, Validate};
use std::mem;

use anyhow::{anyhow, bail, Result};
use clap::Command as ClapCommand;
use clap::{ArgGroup, CommandFactory, Parser};
use hif::*;

#[derive(Parser, Debug)]
#[clap(
    name = "spirot", about = env!("CARGO_PKG_DESCRIPTION"),
    group = ArgGroup::new("command").multiple(false)
)]
struct SpirotArgs {
    /// sets timeout
    #[clap(
        long, short = 'T', default_value = "5000", value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// message type
    #[clap(long, short, default_value = "1")]
    msgtype: u32,

    /// perform a read
    #[clap(
        long, short, group = "command", requires_all = &["addr", "nbytes"]
    )]
    sendrecv: bool,

    /// specify size in bytes
    #[clap(long, short, value_name = "nbytes",
        parse(try_from_str = parse_int::parse),
    )]
    nbytes: Option<usize>,

    /// comma-separated bytes to send
    #[clap(
        long,
        short,
        value_name = "bytes",
        group = "command",
        requires = "addr"
    )]
    write: Option<String>,
}

fn optional_nbytes<'a>(
    core: &'a mut dyn Core,
    context: &'a mut HiffyContext,
    cmd: &HiffyFunction,
    nbytes: Option<usize>,
) -> Result<u32> {
    // Nbytes (-n) is optional and defaults to
    // the entire flash contents.
    // Read the flash size from the flash part itself if nbytes
    // is not specified.
    match nbytes {
        Some(nbytes) => Ok(nbytes as u32),
        None => {
            let ops = vec![Op::Call(cmd.id), Op::Done];
            // Result<
            //  Vec<
            //      Result<Vec<u8>, u32>
            //  >, anyhow::Error>
            match context.run(core, ops.as_slice(), None) {
                Ok(results) => match &results[0] {
                    Ok(buf) => {
                        if mem::size_of::<DeviceIdData>() == buf.len() {
                            let did: DeviceIdData = unsafe {
                                std::ptr::read(buf.as_ptr() as *const _)
                            };
                            Ok(did.size()? as u32)
                        } else {
                            Err(anyhow!(
                                "Unexpected result length: {} != {}",
                                mem::size_of::<DeviceIdData>(),
                                buf.len()
                            ))
                        }
                    }
                    Err(e) => Err(anyhow!("{}", e)),
                },
                Err(e) => Err(e),
            }
        }
    }
}

fn spirot(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    _args: &Args,
    subargs: &[String],
) -> Result<()> {
    let subargs = SpirotArgs::try_parse_from(subargs)?;
    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;
    let funcs = context.functions()?;

    let sector_size = 64 * 1024;
    let block_size = 256; // Conflating flash block size with hubris scratch buffer.

    let mut ops = vec![];
    let mut hash_name = "".to_string();

    let data = if subargs.sendrecv {
        let spirot_sendrecv = funcs.get("SpirotSendRecv", 2)?;
        let bytes: Vec<&str> = write.split(',').collect();
        let mut arr = vec![];

        for byte in &bytes {
            if let Ok(val) = parse_int::parse::<u8>(byte) {
                arr.push(val);
            } else {
                bail!("invalid byte {}", byte)
            }
        }

        ops.push(Op::Push32(subargs.msgtype as u32));
        ops.push(Op::Push32(arr.len() as u32));

        ops.push(Op::Call(spirot_sendrecv.id));
        Some(arr)
    } else {
        bail!("expected an operation");
    };

    ops.push(Op::Done);

    let results = context.run(
        core,
        ops.as_slice(),
        match data {
            Some(ref data) => Some(data.as_slice()),
            _ => None,
        },
    )?;

    if subargs.sendrecv {
        if let Ok(results) = &results[0] {
            Dumper::new().dump(results, 0);
            return Ok(());
        }
    }

    println!("{:x?}", results);

    Ok(())
}

pub fn init() -> (Command, ClapCommand<'static>) {
    (
        Command::Attached {
            name: "spirot",
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
            run: spirot,
        },
        SpirotArgs::command(),
    )
}
