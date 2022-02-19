// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::{anyhow, bail, Result};
use clap::Command as ClapCommand;
use clap::{ArgGroup, CommandFactory, Parser};

use humility::core::Core;
use humility::hubris::*;
use humility_cmd::hiffy::*;
use humility_cmd::{Archive, Args, Attach, Command, Validate};

use hif::*;

use spdm::{
    config::NUM_SLOTS,
    crypto::{FakeSigner, FilledSlot},
    msgs::Msg,
    requester::RequesterInit,
};

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

    // Run through the SPDM protocol handshake
    #[clap(long, short = 'u', group = "command")]
    run: bool,

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
    } else if subargs.run {
        return run_vca(&mut context, core);
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

    let results = context.run(core, ops.as_slice(), data)?;

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

/// This will step through the VCA exchange of the SPDM protocol
///
/// TODO: Make the spdm command capture the shell so we can use it like a debugger.
fn run_vca(ctx: &mut HiffyContext, core: &mut dyn Core) -> Result<()> {
    let mut req_buf = [0u8; 256];
    const EMPTY_SLOT: Option<FilledSlot<'_, FakeSigner>> = None;
    let slots = [EMPTY_SLOT; NUM_SLOTS];
    let fake_root_cert = Vec::new();

    // Create a new requester. We don't use certs for VCA exchange so just use
    // an empty slice for now.
    let mut requester = RequesterInit::new(&fake_root_cert, slots);

    loop {
        println!("Requester State = {}", requester.state().name());
        let req = requester.next_request(&mut req_buf).unwrap();
        println!("Sending request: {:?}", req);
        let rsp = exchange(ctx, core, req)?;

        match rsp[1] {
            spdm::msgs::Version::SPDM_CODE => {
                let version =
                    spdm::msgs::Version::parse_body(&rsp[2..]).unwrap();
                println!("Received version {:?}", version);
            }
            spdm::msgs::Capabilities::SPDM_CODE => {
                let capabilities =
                    spdm::msgs::Capabilities::parse_body(&rsp[2..]).unwrap();
                println!("Received capabilities {:?}", capabilities);
            }
            spdm::msgs::Algorithms::SPDM_CODE => {
                let algorithms =
                    spdm::msgs::Algorithms::parse_body(&rsp[2..]).unwrap();
                println!("Received algorithms {:?}", algorithms);
            }
            _ => {
                println!("Unexpected Response: {:?}", rsp);
            }
        }

        println!("Received response: {:?}", rsp);
        if requester.handle_msg(&rsp).unwrap() {
            break;
        }
    }
    println!("Requester State = {}", requester.state().name());

    Ok(())
}

fn exchange(
    ctx: &mut HiffyContext,
    core: &mut dyn Core,
    req: &[u8],
) -> Result<Vec<u8>> {
    let funcs = ctx.functions()?;
    let cmd = funcs.get("SpdmExchange", 1)?;
    let mut ops = vec![];
    ops.push(Op::Push32(req.len() as u32));
    ops.push(Op::Call(cmd.id));
    ops.push(Op::Done);
    let mut results = ctx.run(core, ops.as_slice(), Some(req))?;
    assert_eq!(results.len(), 1);
    results.pop().unwrap().map_err(|e| anyhow!("Exchange Error: {}", e))
}

pub fn init() -> (Command, ClapCommand<'static>) {
    (
        Command::Attached {
            name: "spdm",
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
            run: spdm,
        },
        SpdmArgs::command(),
    )
}
