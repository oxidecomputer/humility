// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility nprpc`
//!
//! `humility nprpc` works with the `nprpc` interfaces that tasks expose with
//! `nprpc_net::expose_interface!`. The schemas are read from a debug-only
//! INFO section of the archive.
//!
//! `humility nprpc list` lists methods and types from exposed interfaces.
//!
//! ```console
//! $ humility -a build-gimlet-f-dev-image-default.zip nprpc list
//! TASK         INTERFACE    KEY              METHOD
//! sbirpc       all_rpcs     0c8ae9f8fb4d8025 sp3_raw_sb_rmi/smm_read_word
//!                                            rqst: u8
//!                                            resp: enum Result<T, E> { Ok(u32), Err(SbRmiCallError) }
//! ...
//!
//! TYPES (sbirpc/all_rpcs)
//!     enum SbRmiCallError { I2cError { code: u32 }, Timeout, ... }
//!     ...
//! ```
//!
//! `humility nprpc call` calls a method over the network. This uses the
//! schema information from the archive to convert the JSON `--arg` into a
//! request, and the response back into JSON. Use `--ip` on the top
//! level to specify the address of the device. Providing `--task` will
//! automatically select the correct port, or a specific port can be provided
//! using `--port`.
//!
//! ```console
//! $ humility \
//!     -a build-gimlet-f-dev-image-default.zip \
//!     --ip 'fe80::c1d:7dff:feef:9f1d%eth0' \
//!     nprpc call --task 'sbirpc' sp3_sb_rmi/read_cclk_freq_limit
//! {
//!   "Ok": 3700
//! }
//! ```
//!
//! `--arg` defaults to `null`, which is suitable for methods that take `()`.
//! Enums use serde's default JSON representation, e.g. `"Timeout"` or
//! `{"I2cError": {"code": 3}}`.
//!
//! Use `-t` to select the interfaces of a specific task.

use std::collections::BTreeSet;
use std::net::{SocketAddr, SocketAddrV6};
use std::time::Duration;

use anyhow::{Context, Result, anyhow, bail};
use clap::{Parser, Subcommand};
use humility::hubris::HubrisArchive;
use humility_cli::{ExecutionContext, humility_cmd};
use humility_doppel::nprpc_schema::{
    self, OwnedDataModelType, OwnedInterfaceInfo, OwnedMethodInfo,
};
use humility_net::ScopedV6Addr;
use nprpc::io::client::Backend;
use nprpc::wire::HEADER_LEN;
use nprpc::{ClientError, ClientIoError};
use nprpc_udp::{MAX_UDP_PAYLOAD, UdpClient, VecStorage};

#[derive(Parser, Debug)]
#[clap(name = "nprpc", about = env!("CARGO_PKG_DESCRIPTION"))]
pub struct NprpcArgs {
    /// only use interfaces exposed by this task
    #[clap(long, short, global = true)]
    task: Option<String>,

    #[clap(subcommand)]
    cmd: NprpcCmd,
}

#[derive(Subcommand, Debug)]
enum NprpcCmd {
    /// list all methods, with their request and response types
    List,
    /// call a method over the network
    Call {
        /// full name of the method to call, e.g. `iface/method`
        call: String,

        /// the server's socket, e.g. `5501`
        #[clap(long)]
        port: Option<u16>,

        /// request body, as JSON. Can be omitted for methods that take `()` as
        /// an argument.
        #[clap(long, default_value = "null")]
        arg: String,

        /// how long to wait for the response, in milliseconds
        #[clap(long, default_value = "1000")]
        timeout_ms: u64,
    },
}

fn nprpccmd(subargs: NprpcArgs, context: &mut ExecutionContext) -> Result<()> {
    let hubris = context.cli.archive()?;
    let ifaces = load(&hubris, subargs.task.as_deref())?;

    match subargs.cmd {
        NprpcCmd::List => {
            list(&ifaces);
            Ok(())
        }
        NprpcCmd::Call { port, call, arg, timeout_ms } => {
            // Get IP addr from `humility --ip`
            let Some(ip_res) = context.cli.ip.as_ref().cloned() else {
                bail!("missing target IP, specify with `--ip`");
            };
            let ScopedV6Addr { ip, scopeid } =
                ip_res.0.context("failed to parse IP address for nprpc")?;

            let get_socket = |t: &str| -> Result<u16> {
                Ok(hubris
                    .manifest
                    .get_socket_by_task(t)
                    .with_context(|| {
                        format!("failed to get socket for task {t}")
                    })?
                    .port)
            };

            // Get port from `humility nprpc call --port`, OR if `--task` is
            // provided, then use that instead.
            let port = match (port, subargs.task) {
                // If ONLY `port` is provided, use that.
                (Some(p), None) => p,
                // If ONLY `task` is provided, attempt to use that.
                (None, Some(t)) => get_socket(&t)?,
                // if BOTH `task` AND `port` are provided, make sure they match.
                (Some(p), Some(t)) => {
                    let tp = get_socket(&t)?;
                    if p != tp {
                        bail!(
                            "`--task` detected {tp} \
                            but `--port` specified {p}, refusing to continue"
                        );
                    }
                    p
                }
                (None, None) => bail!("must provide `--port` or `--task`!"),
            };
            let target = SocketAddrV6::new(ip, port, 0, scopeid).into();

            let method = find_method(&ifaces, &call)?;
            let arg = serde_json::from_str(&arg)
                .with_context(|| format!("parsing --arg as JSON: {arg}"))?;
            let resp = call_method(method, &arg, target, timeout_ms)?;
            println!("{}", serde_json::to_string_pretty(&resp)?);
            Ok(())
        }
    }
}

/// Loads exposed interfaces (all interfaces in the archive, or filtered to a
/// specific task).
fn load(
    hubris: &HubrisArchive,
    task: Option<&str>,
) -> Result<Vec<(String, OwnedInterfaceInfo)>> {
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
        for iface in nprpc_schema::load_interfaces(hubris, module)? {
            ifaces.push((module.name.clone(), iface));
        }
    }

    if ifaces.is_empty() {
        match task {
            Some(task) => bail!("task {task} does not expose any interfaces"),
            None => bail!("no tasks expose any nprpc interfaces"),
        }
    }

    Ok(ifaces)
}

fn list(ifaces: &[(String, OwnedInterfaceInfo)]) {
    println!("{:12} {:12} {:16} METHOD", "TASK", "INTERFACE", "KEY");
    for (task, iface) in ifaces {
        for m in &iface.methods {
            let key = u64::from_le_bytes(m.key.to_bytes());
            println!("{task:12} {:12} {key:016x} {}", iface.name, m.name);
            println!("{:42} rqst: {}", "", m.req_schema);
            println!("{:42} resp: {}", "", m.resp_schema);
        }
    }

    for (task, iface) in ifaces {
        let defs = iface
            .methods
            .iter()
            .flat_map(|m| [&m.req_schema, &m.resp_schema])
            .flat_map(|ty| ty.all_used_types())
            .filter(|ty| match ty {
                OwnedDataModelType::Struct { .. } => true,
                // Skip top level `Option` and `Result` as unique types, they
                // are shown in the method listing, and non-primitve T/E types
                // are also listed as used types. `postcard-schema-ng`'s impls
                // of `Schema` for these types use the names `Result<T, E>` and
                // `Option<T>` specifically. This avoids a bunch of repetition
                // such as:
                //
                // ```
                // TYPES (sbirpc/all_rpcs)
                //   enum Result<T, E> { Ok(()), Err(u32) }
                //   enum Result<T, E> { Ok(f32), Err(u32) }
                //   enum Result<T, E> { Ok(u32), Err(SbRmiCallError) }
                //   enum Result<T, E> { Ok(u8), Err(u32) }
                //   enum SbRmiCallError { ... }
                // ```
                OwnedDataModelType::Enum { name, .. } => {
                    !matches!(&**name, "Option<T>" | "Result<T, E>")
                }
                _ => false,
            })
            .map(|ty| ty.to_pseudocode())
            .collect::<BTreeSet<_>>();

        if !defs.is_empty() {
            println!("\nTYPES ({task}/{})", iface.name);
            for def in defs {
                println!("    {def}");
            }
        }
    }
}

/// Finds a method by its full name.
///
/// A common method may be exposed by more than one task or interface. Dupes are
/// allowed as long as the name+keys of all instances match.
fn find_method<'a>(
    ifaces: &'a [(String, OwnedInterfaceInfo)],
    name: &str,
) -> Result<&'a OwnedMethodInfo> {
    let found = ifaces
        .iter()
        .flat_map(|(task, iface)| iface.methods.iter().map(move |m| (task, m)))
        .filter(|(_, m)| m.name == name)
        .collect::<Vec<_>>();

    match found.as_slice() {
        [] => bail!("no method named {name}, see `humility nprpc list`"),
        [(first, m), rest @ ..] => {
            if let Some((task, _)) = rest.iter().find(|(_, o)| o.key != m.key) {
                bail!(
                    "{name} differs between tasks {first} and {task}, use -t \
                     to select a specific task's method",
                );
            }
            Ok(m)
        }
    }
}

/// Calls `method` on the server at `target`, returning the response as JSON.
fn call_method(
    method: &OwnedMethodInfo,
    arg: &serde_json::Value,
    target: SocketAddr,
    timeout_ms: u64,
) -> Result<serde_json::Value> {
    let body = postcard_dyn_ng::to_stdvec_dyn(&method.req_schema, arg)
        .with_context(|| {
            format!(
                "--arg doesn't match the request schema {}",
                method.req_schema
            )
        })?;

    let mut client = connect(target, timeout_ms, body.len())?;
    let resp =
        client.send_then_receive_raw(method.key, &body).map_err(|e| {
            let context = match &e {
                ClientIoError::Io(io) if nprpc_udp::is_timeout(io) => {
                    format!("no response from {target} within {timeout_ms}ms")
                }
                ClientIoError::Io(_) => format!("calling {target}"),
                ClientIoError::Client(ClientError::Server(_)) => {
                    "server rejected the request".to_string()
                }
                _ => format!("calling {}", method.name),
            };
            anyhow::Error::new(e).context(context)
        })?;

    postcard_dyn_ng::from_slice_dyn(&method.resp_schema, resp.body)
        .with_context(|| {
            format!(
                "response doesn't match the response schema {}",
                method.resp_schema
            )
        })
}

/// Connects to the server at `target`, with room for a request body of
/// `rqst_len` bytes.
fn connect(
    target: SocketAddr,
    timeout_ms: u64,
    rqst_len: usize,
) -> Result<UdpClient<VecStorage>> {
    let storage = VecStorage::new(HEADER_LEN + rqst_len, MAX_UDP_PAYLOAD);
    let client = UdpClient::connect(target, storage)
        .with_context(|| format!("connecting to {target}"))?;
    client
        .set_timeout(Some(Duration::from_millis(timeout_ms)))
        .context("setting read timeout")?;
    Ok(client)
}

humility_cmd!(NprpcArgs, nprpccmd);
