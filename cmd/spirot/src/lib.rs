// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use hubpack::{deserialize, serialize, SerializedSize};
use humility::core::Core;
use humility::hubris::*;
use humility_cmd::hiffy::*;
use humility_cmd::{Archive, Args, Attach, Command, Dumper, Validate};
use sprockets_common::msgs::{RotOpV1, RotRequestV1, RotResponseV1};
use sprockets_common::Nonce;

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
    #[clap(long, short, default_value = "echoreq")]
    msgtype: String,

    /// Send bytes and receive response
    #[clap(long, short, group = "command")]
    send: Option<String>,
}

// XXX This needs to come from hubris spi-msg crate.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
#[repr(u8)]
pub enum MsgType {
    Invalid = 0,
    ErrorRsp = 1,
    EchoReq = 2,
    EchoRsp = 3,
    StatusReq = 4,
    StatusRsp = 5,
    SprocketsReq = 6,
    SprocketsRsp = 7,
    Unknown = 0xff,
}

impl From<&str> for MsgType {
    fn from(typename: &str) -> Self {
        let lc = typename.to_lowercase();
        match lc.as_str() {
            "invalid" => MsgType::Invalid,
            "errorrsp" => MsgType::ErrorRsp,
            "echoreq" => MsgType::EchoReq,
            "echorsp" => MsgType::EchoRsp,
            "statusreq" => MsgType::StatusReq,
            "statusrsp" => MsgType::StatusRsp,
            "sprocketsreq" => MsgType::SprocketsReq,
            "sprocketsrsp" => MsgType::SprocketsRsp,
            _ => MsgType::Unknown,
        }
    }
}

impl From<u8> for MsgType {
    fn from(protocol: u8) -> Self {
        match protocol {
            0 => MsgType::Invalid,
            1 => MsgType::ErrorRsp,
            2 => MsgType::EchoReq,
            3 => MsgType::EchoRsp,
            4 => MsgType::StatusReq,
            5 => MsgType::StatusRsp,
            6 => MsgType::SprocketsReq,
            7 => MsgType::SprocketsRsp,
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
    let msgtype = MsgType::from(subargs.msgtype.as_str());
    let data = if let Some(ref send) = subargs.send {
        match msgtype {
            MsgType::SprocketsReq => send_sprockets(send),
            MsgType::Unknown => bail!("Using unknown message type"),
            _ => send_other(send),
        }
    } else {
        match msgtype {
            MsgType::SprocketsReq => bail!("-s data is required"),
            MsgType::Unknown => bail!("Using unknown message type"),
            _ => send_other(""),
        }
    };

    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;

    println!("size = {}", context.scratch_size());

    let funcs = context.functions()?;
    let spirot_sendrecv = funcs.get("SpirotSendRecv", 2)?;
    let mut ops = vec![];
    ops.push(Op::Push32(msgtype as u32));
    ops.push(Op::Push32(data.len() as u32));
    ops.push(Op::Call(spirot_sendrecv.id));
    ops.push(Op::Done);
    println!("ops: {:?}", ops);

    println!("data: {:?}", data);

    let results = if data.len() > 0 {
        context.run(core, ops.as_slice(), Some(data.as_slice()))?
    } else {
        context.run(core, ops.as_slice(), None)?
    };
    println!("results={:?}", results);

    if subargs.send.is_some() {
        if let Ok(results) = &results[0] {
            if msgtype == MsgType::SprocketsRsp {
                println!("SprocketsRsp!");
                let (response, _) = deserialize::<RotResponseV1>(results)?;
                println!("{:x?}", response);
            } else {
                Dumper::new().dump(results, 0);
            }
        } else {
            println!("Fail!");
            println!("{:x?}", results);
        }
    }

    Ok(())
}

// TODO: This should probably be a clap subcommand, but that would require
// changing the overall flow of the existing comamnds for spirot.
fn send_sprockets(command: &str) -> Vec<u8> {
    let op = match command {
        "get-certs" => RotOpV1::GetCertificates,
        "get-measurements" => RotOpV1::GetMeasurements(Nonce::new()),
        _ => panic!(
            "Expected 'get-certs' or 'get-measurements'. Got {}",
            command
        ),
    };

    let req = RotRequestV1 {
        version: 1,
        id: 1,
        op,
    };

    let mut buf = vec![0; RotRequestV1::MAX_SIZE];
    let size = serialize(&mut buf, &req).unwrap();
    buf.truncate(size);
    buf
}

// Return a request other than sprockets to send to spirot via HIF
fn send_other(command: &str) -> Vec<u8> {
    let mut arr = vec![];

    for byte in command.split_terminator(',') {
        if let Ok(val) = parse_int::parse::<u8>(byte) {
            arr.push(val);
        } else {
            panic!("invalid byte {}", byte);
        }
    }
    arr
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
