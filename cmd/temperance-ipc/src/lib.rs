// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility temperance-ipc`
//!
//! TODO: Add docs

use std::collections::BTreeSet;
use std::time::Duration;

use anyhow::{Context, Result, anyhow, bail};
use clap::{Parser, Subcommand};
use hif::{Op, TargetFunction};
use humility::hubris::{HubrisArchive, HubrisTask};
use humility_cli::{ExecutionContext, humility_cmd};
use humility_doppel::nprpc_schema::{
    self, OwnedDataModelType, OwnedInterfaceInfo, OwnedMethodInfo,
};
use humility_hiffy::HiffyContext;
use humility_idol::IpcError;
use nprpc::wire::{HEADER_LEN, Header, Operation, WIRE_VERSION, WireError};
use postcard_schema_ng::Schema;

// TODO(robo): These mirror `temperance-ipc` in Hubris; keep them in sync.
const OP_BASE: u32 = 0x7E00;
const FLAG_RQST_LEASED: u32 = 1 << 0;
const REPLY_SERVER_FAILED: u32 = 1;

// TODO: Discuss where the hiffy `Send` payload limit comes from.
const MAX_INLINE_FRAME: usize = 28;

const SEQNO: u16 = 0x7E57;

#[derive(Parser, Debug)]
#[clap(name = "temperance-ipc", about = env!("CARGO_PKG_DESCRIPTION"))]
pub struct TemperanceIpcArgs {
    /// TODO: Add help text
    #[clap(long, short, global = true)]
    task: Option<String>,

    #[clap(subcommand)]
    cmd: TemperanceIpcCmd,
}

#[derive(Subcommand, Debug)]
enum TemperanceIpcCmd {
    /// TODO: Add help text
    List,
    /// TODO: Add help text
    Call {
        /// TODO: Add help text
        call: String,

        /// TODO: Add help text
        #[clap(long, default_value = "null")]
        arg: String,

        /// TODO: Add help text
        #[clap(
            long, short = 'T', default_value_t = 5000,
            value_name = "timeout_ms", value_parser = parse_int::parse::<u64>
        )]
        timeout: u64,
    },
}

struct Exposed {
    task_name: String,
    task: HubrisTask,
    iface: OwnedInterfaceInfo,
}

fn temperance_ipc(
    subargs: TemperanceIpcArgs,
    context: &mut ExecutionContext,
) -> Result<()> {
    let hubris = &context.cli.archive()?;
    let ifaces = load(hubris, subargs.task.as_deref())?;

    match subargs.cmd {
        TemperanceIpcCmd::List => {
            list(&ifaces);
            Ok(())
        }
        TemperanceIpcCmd::Call { call, arg, timeout } => {
            let (task, method) = find_method(&ifaces, &call)?;
            let arg: serde_json::Value = serde_json::from_str(&arg)
                .with_context(|| format!("parsing --arg as JSON: {arg}"))?;

            let log = context.log();
            let core = &mut *context.cli.attach_live_booted(hubris)?;
            let timeout = Duration::from_millis(timeout);
            let mut hiffy = HiffyContext::new(hubris, core, timeout, log)?;

            let resp = call_method(&mut hiffy, core, task, method, &arg)?;
            println!("{}", serde_json::to_string_pretty(&resp)?);
            Ok(())
        }
    }
}

fn load(hubris: &HubrisArchive, task: Option<&str>) -> Result<Vec<Exposed>> {
    let modules = match task {
        Some(name) => {
            let task = hubris
                .lookup_task(name)
                .ok_or_else(|| anyhow!("unknown task {name}"))?;
            vec![hubris.lookup_module(task)?]
        }
        None => hubris.modules().collect(),
    };

    let mut ifaces = vec![];
    for module in modules {
        for iface in nprpc_schema::load_ipc_interfaces(hubris, module)? {
            ifaces.push(Exposed {
                task_name: module.name.clone(),
                task: module.task,
                iface,
            });
        }
    }

    if ifaces.is_empty() {
        match task {
            Some(task) => {
                bail!(
                    "task {task} does not expose any temperance-ipc interfaces"
                )
            }
            None => bail!("no tasks expose any temperance-ipc interfaces"),
        }
    }

    Ok(ifaces)
}

// TODO(robo): Copied from `humility nprpc list`; share it?
fn list(ifaces: &[Exposed]) {
    println!("{:12} {:12} {:16} METHOD", "TASK", "INTERFACE", "KEY");
    for Exposed { task_name, iface, .. } in ifaces {
        for m in &iface.methods {
            let key = u64::from_le_bytes(m.key.to_bytes());
            println!("{task_name:12} {:12} {key:016x} {}", iface.name, m.name);
            println!("{:42} rqst: {}", "", m.req_schema);
            println!("{:42} resp: {}", "", m.resp_schema);
        }
    }

    for Exposed { task_name, iface, .. } in ifaces {
        let defs = iface
            .methods
            .iter()
            .flat_map(|m| [&m.req_schema, &m.resp_schema])
            .flat_map(|ty| ty.all_used_types())
            .filter(|ty| match ty {
                OwnedDataModelType::Struct { .. } => true,
                // TODO: Discuss skipping `Option`/`Result` (see `nprpc list`).
                OwnedDataModelType::Enum { name, .. } => {
                    !matches!(&**name, "Option<T>" | "Result<T, E>")
                }
                _ => false,
            })
            .map(|ty| ty.to_pseudocode())
            .collect::<BTreeSet<_>>();

        if !defs.is_empty() {
            println!("\nTYPES ({task_name}/{})", iface.name);
            for def in defs {
                println!("    {def}");
            }
        }
    }
}

// TODO: Discuss why duplicates must agree on both key and task.
fn find_method<'a>(
    ifaces: &'a [Exposed],
    name: &str,
) -> Result<(HubrisTask, &'a OwnedMethodInfo)> {
    let found = ifaces
        .iter()
        .flat_map(|e| e.iface.methods.iter().map(move |m| (e, m)))
        .filter(|(_, m)| m.name == name)
        .collect::<Vec<_>>();

    match found.as_slice() {
        [] => {
            bail!("no method named {name}, see `humility temperance-ipc list`")
        }
        [(first, m), rest @ ..] => {
            if let Some((other, _)) = rest
                .iter()
                .find(|(e, o)| o.key != m.key || e.task != first.task)
            {
                bail!(
                    "{name} is exposed by tasks {} and {}, use -t to select \
                     one",
                    first.task_name,
                    other.task_name,
                );
            }
            Ok((first.task, m))
        }
    }
}

fn max_resp_frame(method: &OwnedMethodInfo, hiffy: &HiffyContext) -> usize {
    let err = <WireError as Schema>::SCHEMA.max_size().unwrap_or(1);
    match method.resp_schema.max_size() {
        Some(n) => HEADER_LEN + n.max(err),
        None => hiffy.rstack_size(),
    }
}

fn call_method(
    hiffy: &mut HiffyContext,
    core: &mut dyn humility::core::Core,
    task: HubrisTask,
    method: &OwnedMethodInfo,
    arg: &serde_json::Value,
) -> Result<serde_json::Value> {
    let HubrisTask::Task(task_id) = task else {
        bail!("interface is exposed by the kernel?");
    };

    let frame = encode_request(method, arg)?;

    let nreply = max_resp_frame(method, hiffy);
    if nreply > hiffy.rstack_size() {
        bail!(
            "response of up to {nreply} bytes won't fit in hiffy's {} byte \
             return stack",
            hiffy.rstack_size()
        );
    }

    let leased = frame.len() > MAX_INLINE_FRAME;
    let func = if leased {
        let data_size = hiffy
            .data_size()
            .map_err(|_| anyhow!("hiffy has no data area for leases"))?;
        if frame.len() > data_size {
            bail!(
                "request of {} bytes won't fit in hiffy's {data_size} byte \
                 data area",
                frame.len()
            );
        }
        hiffy.get_function("SendLeaseRead", 5)?
    } else {
        hiffy.get_function("Send", 4)?
    };
    let ops = request_ops(task_id, &frame, nreply, func.id, leased);
    let data = leased.then_some(frame.as_slice());

    let mut results = hiffy.run(core, &ops, data)?;
    let reply = match results.pop() {
        Some(Ok(reply)) => reply,
        Some(Err(IpcError::ServerDied(_))) => {
            bail!("the server task died or restarted during the call")
        }
        Some(Err(IpcError::Error(REPLY_SERVER_FAILED))) => {
            bail!("the server failed to produce a reply")
        }
        Some(Err(e)) => bail!("call failed: {e:?}"),
        None => bail!("hiffy returned no results"),
    };

    decode_reply(method, &reply)
}

fn encode_request(
    method: &OwnedMethodInfo,
    arg: &serde_json::Value,
) -> Result<Vec<u8>> {
    let body = postcard_dyn_ng::to_stdvec_dyn(&method.req_schema, arg)
        .with_context(|| {
            format!(
                "--arg doesn't match the request schema {}",
                method.req_schema
            )
        })?;

    let mut frame = vec![0u8; HEADER_LEN];
    Header {
        version: WIRE_VERSION,
        op: Operation::Request,
        seqno: SEQNO,
        key: method.key,
    }
    .write_to(&mut frame)
    .map_err(|e| anyhow!("writing header: {e:?}"))?;
    frame.extend_from_slice(&body);
    Ok(frame)
}

// TODO: Discuss the HIF argument order (see `task/hiffy/src/common.rs`).
fn request_ops(
    task_id: u32,
    frame: &[u8],
    nreply: usize,
    func: TargetFunction,
    leased: bool,
) -> Vec<Op> {
    let push = |val: u32| {
        if val <= u8::MAX as u32 {
            Op::Push(val as u8)
        } else if val <= u16::MAX as u32 {
            Op::Push16(val as u16)
        } else {
            Op::Push32(val)
        }
    };

    let mut ops = vec![push(task_id)];
    if leased {
        ops.push(push(OP_BASE | FLAG_RQST_LEASED));
        ops.push(push(0));
        ops.push(push(nreply as u32));
        ops.push(push(frame.len() as u32));
        ops.push(Op::Call(func));
        ops.push(Op::DropN(5));
    } else {
        ops.push(push(OP_BASE));
        ops.extend(frame.iter().map(|b| Op::Push(*b)));
        ops.push(push(frame.len() as u32));
        ops.push(push(nreply as u32));
        ops.push(Op::Call(func));
        ops.push(Op::DropN((4 + frame.len()) as u8));
    }
    ops.push(Op::Done);
    ops
}

// TODO: Discuss trailing bytes in hiffy replies.
fn decode_reply(
    method: &OwnedMethodInfo,
    reply: &[u8],
) -> Result<serde_json::Value> {
    let (hedr, body) = Header::read_from(reply)
        .map_err(|e| anyhow!("bad response header: {e:?}"))?;
    if hedr.seqno != SEQNO {
        bail!("response has seqno {:#x}, expected {SEQNO:#x}", hedr.seqno);
    }
    match hedr.op {
        Operation::Response if hedr.key == method.key => {
            postcard_dyn_ng::from_slice_dyn(&method.resp_schema, body)
                .with_context(|| {
                    format!(
                        "response doesn't match the response schema {}",
                        method.resp_schema
                    )
                })
        }
        Operation::Response => bail!("response has the wrong key"),
        Operation::ErrorResponse => match body.first() {
            Some(&code) => {
                bail!("server rejected the request: {}", WireError(code))
            }
            None => bail!("server rejected the request, without a reason"),
        },
        op => bail!("unexpected operation in response: {op:?}"),
    }
}

humility_cmd!(TemperanceIpcArgs, temperance_ipc);

#[cfg(test)]
mod tests {
    use super::*;
    use nprpc::flat::OwnedDataModelType as Ty;

    fn method() -> OwnedMethodInfo {
        OwnedMethodInfo {
            name: "demo/bump".to_string(),
            key: postcard_schema_ng::key::Key::from_bytes([
                1, 2, 3, 4, 5, 6, 7, 8,
            ]),
            req_schema: Ty::from(<u32 as Schema>::SCHEMA),
            resp_schema: Ty::from(<u64 as Schema>::SCHEMA),
        }
    }

    fn reply(op: Operation, body: &[u8], pad: usize) -> Vec<u8> {
        let mut buf = vec![0u8; HEADER_LEN];
        Header { version: WIRE_VERSION, op, seqno: SEQNO, key: method().key }
            .write_to(&mut buf)
            .unwrap();
        buf.extend_from_slice(body);
        buf.extend(std::iter::repeat_n(0, pad));
        buf
    }

    #[test]
    fn inline_request() {
        let frame = encode_request(&method(), &serde_json::json!(300)).unwrap();
        assert_eq!(frame.len(), HEADER_LEN + 2);
        assert_eq!(frame[HEADER_LEN..], [0xAC, 0x02]);

        let ops = request_ops(9, &frame, 22, TargetFunction(3), false);
        let mut expected = vec![Op::Push(9), Op::Push16(OP_BASE as u16)];
        expected.extend(frame.iter().map(|b| Op::Push(*b)));
        expected.extend([
            Op::Push(frame.len() as u8),
            Op::Push(22),
            Op::Call(TargetFunction(3)),
            Op::DropN(4 + frame.len() as u8),
            Op::Done,
        ]);
        assert_eq!(ops, expected);
    }

    #[test]
    fn leased_request() {
        let frame = vec![0u8; 400];
        let ops = request_ops(9, &frame, 22, TargetFunction(4), true);
        assert_eq!(
            ops,
            [
                Op::Push(9),
                Op::Push16((OP_BASE | FLAG_RQST_LEASED) as u16),
                Op::Push(0),
                Op::Push(22),
                Op::Push16(400),
                Op::Call(TargetFunction(4)),
                Op::DropN(5),
                Op::Done,
            ]
        );
    }

    #[test]
    fn padded_response() {
        let r = reply(Operation::Response, &[42], 9);
        assert_eq!(decode_reply(&method(), &r).unwrap(), serde_json::json!(42));
    }

    #[test]
    fn error_response() {
        let r = reply(Operation::ErrorResponse, &[13], 9);
        let err = decode_reply(&method(), &r).unwrap_err().to_string();
        assert!(err.contains("unknown method"), "{err}");
    }
}
