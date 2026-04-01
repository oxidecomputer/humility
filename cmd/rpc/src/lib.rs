// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility rpc`
//!
//! `humility rpc` lets you discover SPs on a network, instead of using a
//! physically attached debugger.  Once SPs are discovered, they may be used as
//! a target by setting `HUMILITY_IP` or providing the `--ip` argument.
//!
//! You may need to configure an IPv6 network for `humility rpc` to work. On
//! illumos, it looks like this:
//!
//! ```console
//! $ pfexec ipadm create-addr -t -T addrconf e1000g0/addrconf
//! ```
//!
//! To listen for compatible devices on your network, run `humility rpc
//! --listen`
//!
//! ```console
//! $ humility rpc --listen
//! humility: listening... (ctrl-C to stop, or timeout in 5s)
//! MAC               IPv6                      COMPAT PART        REV SERIAL
//! a8:40:25:04:02:81 fe80::aa40:25ff:fe04:281  Yes    913-0000019   6 BRM42220066
//! a8:40:25:05:05:00 fe80::aa40:25ff:fe05:500  No     (legacy)      0 (legacy)
//! a8:40:25:05:05:00 fe80::aa40:25ff:fe05:501  No     (legacy)      0 (legacy)
//! ```
//!
//! Under the hood, this listens for packets from the Hubris `udpbroadcast`
//! task, which includes MAC address and image ID (checked for compatibility).
//! When listening, it is mandatory to specify the interface (e.g. `humility rpc
//! --listen -i en0` on MacOS). If the `Part` / `Serial` columns are marked as
//! `(legacy)`, the SP is running an older version of `udpbroadcast` that did
//! not include identity information. If they are marked as `(vpdfail)`, they
//! are running a new-enough `udpbroadcast`, but the SP was unable to read its
//! identity from its VPD.

use std::collections::BTreeSet;
use std::net::{IpAddr, Ipv6Addr, UdpSocket};
use std::time::{Duration, Instant};

use anyhow::{Result, bail};
use clap::{ArgGroup, IntoApp, Parser};
use colored::Colorize;
use hubpack::SerializedSize;
use humility::net::decode_iface;
use humility_cli::{ExecutionContext, Subcommand};
use humility_cmd::{Archive, Command, CommandKind};
use serde::{Deserialize, Serialize};

#[derive(Parser, Debug)]
#[clap(
    name = "rpc", about = env!("CARGO_PKG_DESCRIPTION"),
    group = ArgGroup::new("target").multiple(false)
)]
struct RpcArgs {
    /// sets timeout
    #[clap(
        long, short = 'T', default_value_t = 2000, value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// list interfaces
    #[clap(long, short)]
    list: bool,

    /// listen for compatible SPs on the network
    #[clap(
        long,
        conflicts_with = "list",
        group = "target",
        requires = "interface"
    )]
    listen: bool,

    /// interface on which to listen, e.g. 'en0'
    #[clap(short, requires = "listen", value_parser = decode_iface)]
    interface: Option<u32>,
}

#[derive(Debug, Clone, Copy, Deserialize, Serialize, SerializedSize)]
struct BroadcastDataV0 {
    mac_address: [u8; 6],
    image_id: [u8; 8],
}

#[derive(Debug, Clone, Copy, Deserialize, Serialize, SerializedSize)]
struct BroadcastDataV1 {
    version: u32,
    mac_address: [u8; 6],
    image_id: [u8; 8],
    identity_valid: bool,
    part_number: [u8; 11],
    revision: u32,
    serial: [u8; 11],
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
struct Target {
    mac: [u8; 6],
    image_id: [u8; 8],
    ip: IpAddr,
    part_number: String,
    revision: u32,
    serial: String,
}

fn rpc_listen_one(
    timeout: Duration,
    interface: u32,
    port: u32,
) -> Result<BTreeSet<Target>> {
    let socket = match UdpSocket::bind(format!("[::]:{port}")) {
        Ok(s) => s,
        Err(e) => {
            if e.kind() == std::io::ErrorKind::PermissionDenied {
                // If humility wasn't run as root, we can't listen on port 8;
                // print a warning message instead of erroring out entirely.
                humility::msg!(
                    "Cannot listen on port {port}; permission denied",
                );
                return Ok(Default::default());
            } else {
                return Err(e.into());
            }
        }
    };
    socket.set_read_timeout(Some(timeout))?;

    socket.join_multicast_v6(
        &Ipv6Addr::new(0xff02, 0, 0, 0, 0, 0, 0, 1),
        interface,
    )?;

    let mut seen = BTreeSet::new();

    let timeout = Instant::now() + timeout;
    let mut buf = [0u8; 1024];
    while timeout > Instant::now() {
        match socket.recv_from(&mut buf) {
            Ok((n, src)) => {
                // For both BroadcastDataV0 and BroadcastDataV1, we have no
                // fields that can serialize as variable size (e.g., no enums
                // with different-sized variants), so hubpack's `MAX_SIZE`
                // constant is equal to the actual serialized size.
                let target = match n {
                    BroadcastDataV0::MAX_SIZE => {
                        let data: BroadcastDataV0 =
                            match hubpack::deserialize(&buf[..n]) {
                                Ok((data, _)) => data,
                                Err(err) => {
                                    humility::msg!(
                                        "Failed to deserialize packet {:?}: {}",
                                        &buf[..n],
                                        err,
                                    );
                                    continue;
                                }
                            };
                        Target {
                            mac: data.mac_address,
                            image_id: data.image_id,
                            ip: src.ip(),
                            part_number: "(legacy)".to_string(),
                            revision: 0,
                            serial: "(legacy)".to_string(),
                        }
                    }
                    BroadcastDataV1::MAX_SIZE => {
                        let data: BroadcastDataV1 =
                            match hubpack::deserialize(&buf[..n]) {
                                Ok((data, _)) => data,
                                Err(err) => {
                                    humility::msg!(
                                        "Failed to deserialize packet {:?}: {}",
                                        &buf[..n],
                                        err,
                                    );
                                    continue;
                                }
                            };
                        let (part_number, serial) = if data.identity_valid {
                            (
                                String::from_utf8_lossy(&data.part_number)
                                    .to_string(),
                                String::from_utf8_lossy(&data.serial)
                                    .to_string(),
                            )
                        } else {
                            ("(vpdfail)".to_string(), "(vpdfail)".to_string())
                        };
                        Target {
                            mac: data.mac_address,
                            image_id: data.image_id,
                            ip: src.ip(),
                            part_number,
                            revision: data.revision,
                            serial,
                        }
                    }
                    _ => {
                        humility::msg!(
                            "Skipping unknown packet {:?}",
                            &buf[..n]
                        );
                        continue;
                    }
                };
                if target.mac[0..2] != [0x0e, 0x1d]
                    && target.mac[0..3] != [0xa8, 0x40, 0x25]
                {
                    humility::msg!(
                        "Skipping packet with non-matching MAC {:?}",
                        target.mac
                    );
                } else {
                    seen.insert(target);
                }
            }
            Err(e) => {
                // At least on macOS, timeouts are reported as `WouldBlock`,
                // rather than `TimedOut`.
                match e.kind() {
                    std::io::ErrorKind::WouldBlock
                    | std::io::ErrorKind::TimedOut => {
                        break;
                    }
                    e => panic!("Got error {:?}", e),
                }
            }
        }
    }
    Ok(seen)
}

fn rpc_listen(rpc_args: &RpcArgs) -> Result<BTreeSet<Target>> {
    // For some reason, macOS requires the interface to be non-zero:
    // https://users.rust-lang.org/t/ipv6-upnp-multicast-for-rust-dlna-server-macos/24425
    // https://bluejekyll.github.io/blog/posts/multicasting-in-rust/
    let interface = match &rpc_args.interface {
        None => {
            if cfg!(target_os = "macos") {
                bail!("Must specify interface with `-i` on macOS");
            } else {
                0
            }
        }
        Some(iface) => *iface,
    };

    let timeout = Duration::from_millis(rpc_args.timeout as u64);
    let ports = [8, 8888];
    humility::msg!(
        "listening for {} seconds on ports {ports:?}...",
        timeout.as_secs(),
    );

    let threads = ports
        .iter()
        .map(|&port| {
            std::thread::spawn(move || -> Result<BTreeSet<Target>> {
                rpc_listen_one(timeout, interface, port)
            })
        })
        .collect::<Vec<_>>();

    let mut seen = BTreeSet::new();
    for (i, t) in threads.into_iter().enumerate() {
        let port = ports[i];
        match t.join().unwrap() {
            Ok(out) => {
                seen.extend(out.into_iter());
            }
            Err(e) => {
                humility::warn!("thread for port {port} failed: {e:?}");
            }
        }
    }
    if seen.is_empty() {
        humility::msg!("timed out, exiting");
    }

    Ok(seen)
}

fn rpc_dump(seen: BTreeSet<Target>, image_id: &[u8]) {
    if !seen.is_empty() {
        println!(
            "{:17} {:25} {:6} {:11} {:3} {:11}",
            "MAC".bold(),
            "IPv6".bold(),
            "COMPAT".bold(),
            "PART".bold(),
            "REV".bold(),
            "SERIAL".bold(),
        );
    }

    for target in seen {
        for (i, byte) in target.mac.iter().enumerate() {
            print!("{}", if i == 0 { "" } else { ":" });
            print!("{:02x}", byte)
        }
        print!(" {:25} ", target.ip);
        if target.image_id == image_id {
            print!("{:6}", "Yes".green());
        } else {
            print!("{:6}", "No".red());
        }
        print!(" {:11}", target.part_number);
        print!(" {:3}", target.revision);
        println!(" {:11}", target.serial);
    }
}

fn rpc_run(context: &mut ExecutionContext) -> Result<()> {
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let subargs = RpcArgs::try_parse_from(subargs)?;
    let hubris = context.archive.as_ref().unwrap();

    // We previously had more subcommands here, but are down to just `--listen`
    if subargs.listen {
        rpc_dump(rpc_listen(&subargs)?, hubris.image_id().unwrap());
        Ok(())
    } else {
        bail!("expected --listen")
    }
}

pub fn init() -> Command {
    Command {
        app: RpcArgs::command(),
        name: "rpc",
        run: rpc_run,
        kind: CommandKind::Detached { archive: Archive::Required },
    }
}
