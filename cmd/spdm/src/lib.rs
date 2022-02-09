// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use humility::core::Core;
use humility::hubris::*;
use humility_cmd::hiffy::*;
use humility_cmd::{Archive, Args, Attach, Command, Validate};

use anyhow::{bail, Result};
use hif::*;
use clap::{App, ArgGroup, IntoApp, Parser};

extern crate log;

#[derive(Parser, Debug)]
#[clap(
    name = "spdm", about = env!("CARGO_PKG_DESCRIPTION"),
    group = ArgGroup::new("command").multiple(false),
    group = ArgGroup::new("sending").multiple(false)
)]
struct SpdmArgs {

    /// Send a message
    #[clap(long, short, group = "command")]
    send: bool,

    /// Receive a messages
    #[clap(long, short, group = "command")]
    recv: bool,

    /// Exchange messages
    #[clap(long, short, group = "command")]
    exchange: bool,

    /// comma-separated hexidecimal bytes to write
    #[clap(long, short = 'x', group = "sending")]
    hex: Option<String>,

    /// Use a string as the payload to send.
    #[clap(long, short, group = "sending")]
    ascii: Option<String>,

    /// sets timeout
    #[clap(
        long, short = 'T', default_value = "5000", value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,
}

fn spdm(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    _args: &Args,
    subargs: &[String],
) -> Result<()> {
    let subargs = SpdmArgs::try_parse_from(subargs)?;
    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;
    let funcs = context.functions()?;
    let mut ops = vec![];

    let cmd = if subargs.send {
        funcs.get("SpdmSend", 1)?
    } else if subargs.recv {
        funcs.get("SpdmRecv", 0)?
    } else if subargs.exchange {
        funcs.get("SpdmExchange", 1)?
    } else {
        bail!("No spdm command given. Try ... spdm --help");
    };

    let mut arr = Vec::new();
    let data = if let Some(ref data) = subargs.ascii {
        ops.push(Op::Push32(data.len() as u32));
        Some(data.as_bytes())
    } else if let Some(hex) = subargs.hex {
        let bytes: Vec<&str> = hex.split(',').collect();
        for byte in &bytes {
            if let Ok(val) = u8::from_str_radix(byte, 16) {
                arr.push(val);
            } else {
                bail!("invalid byte {}", byte)
            }
        }
        ops.push(Op::Push32(arr.len() as u32));
        Some(arr.as_slice())
    } else {
        None
    };

    // One could check for data.len() > scratch_size
    ops.push(Op::Call(cmd.id));
    ops.push(Op::Done);
    println!("Ops: {:?}", ops);

    let results = context.run(
        core,
        ops.as_slice(),
        match data {
            Some(data) => Some(data),
            _ => None,
        },
    )?;
    println!("\nreturned results:");
    for res in &results {
        println!("RESULT: {:?}", *res);
    }
    println!("end results\n");

    match &results[0] {
        Ok(buf) => {
            println!("{:?}", buf);
        }
        Err(err) => {
            println!("Error returned: {}", err);
        }
    }

    Ok(())
}

pub fn init() -> (Command, App<'static>) {
    (
        Command::Attached {
            name: "spdm",
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
            run: spdm,
        },
        SpdmArgs::into_app(),
    )
}
