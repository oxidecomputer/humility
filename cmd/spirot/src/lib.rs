// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use humility::core::Core;
use humility::hubris::*;
use humility_cmd::hiffy::*;
use humility_cmd::{Archive, Args, Attach, Command, Dumper, Validate};

use anyhow::{bail, Result};
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
    #[clap(long, short, default_value = "echo")]
    msgtype: String,

    /// Send bytes and receive response
    #[clap(long, short, group = "command")]
    send: Option<String>,
}

#[derive(Copy,Clone,PartialEq,Eq,Debug)]
#[repr(u8)]
pub enum MsgType {
    Invalid = 0,
    Error = 1,
    Echo = 2,
    EchoReturn = 3,
    Status = 4,
    Sprockets = 5,
    Unknown = 0xff,
}

impl From<&str> for MsgType {
    fn from(typename: &str) -> Self {
        match typename {
            "invalid" => MsgType::Invalid,
            "error" => MsgType::Error,
            "echo" => MsgType::Echo,
            "echoreturn" => MsgType::EchoReturn,
            "status" => MsgType::Status,
            "sprockets" => MsgType::Sprockets,
            _ => MsgType::Unknown,
        }
    }
}

impl From<u8> for MsgType {
    fn from(protocol: u8) -> Self {
        match protocol {
            0 => MsgType::Invalid,
            1 => MsgType::Error,
            2 => MsgType::Echo,
            3 => MsgType::EchoReturn,
            4 => MsgType::Status,
            _ => MsgType::Unknown,
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

    let mut ops = vec![];

    let data = if let Some(ref send) = subargs.send {
        let spirot_sendrecv = funcs.get("SpirotSendRecv", 2)?;
        let bytes: Vec<&str> = send.split(',').collect();
        let mut arr = vec![];

        for byte in &bytes {
            if let Ok(val) = parse_int::parse::<u8>(byte) {
                arr.push(val);
            } else {
                bail!("invalid byte {}", byte)
            }
        }

        let msgtype = if let Ok(mtype) = subargs.msgtype.parse::<u32>() {
            mtype
        } else {
            MsgType::from(subargs.msgtype.as_str()) as u32
        };
        if msgtype == MsgType::Unknown as u32 {
            bail!("Using unknown message type. Expected one of: invalid, error, echo, echoReturn, status");
        }
        println!("msgtype: {:?}", msgtype);
        ops.push(Op::Push32(msgtype));
        ops.push(Op::Push32(arr.len() as u32));
        ops.push(Op::Call(spirot_sendrecv.id));
        Some(arr)
    } else {
        bail!("expected an operation");
    };

    ops.push(Op::Done);
    println!("ops: {:?}", ops);

    let results = context.run(
        core,
        ops.as_slice(),
        match data {
            Some(ref data) => Some(data.as_slice()),
            _ => None,
        },
    )?;

    if subargs.send.is_some() {
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
