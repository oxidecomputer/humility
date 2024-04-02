// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility rpc`
//!
//! `humility rpc` allows for execution of Idol commands over a network, rather
//! than through a debugger.
//!
//! It requires the Hubris `udprpc` task to be listening on port 8.  This task
//! decodes bytes from a UDP packet, and shoves them directly into `sys_send` to
//! a target task.
//!
//! An archive is required so that `humility` knows what functions are available
//! and how to call them.  The archive ID is checked against the image ID on the
//! target; `udprpc` will refuse to execute commands when the ID does not match.
//!
//! Function calls are handled identically to the `humility hiffy` subcommand,
//! except that an `--ip` address is required:
//!
//! ```console
//! $ humility rpc --ip fe80::0c1d:9aff:fe64:b8c2%en0 -c UserLeds.led_on -aindex=0
//! UserLeds.led_on() = ()
//! ```
//!
//! Alternatively, you can set the `HUMILITY_RPC_IP` environmental variable.
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
//!
//! To call all targets that match an archive, `--listen` can be combined with
//! `--call`

use std::collections::BTreeSet;
use std::net::{IpAddr, Ipv6Addr, ToSocketAddrs, UdpSocket};
use std::time::{Duration, Instant};

use anyhow::{anyhow, bail, Result};
use clap::{ArgGroup, IntoApp, Parser};
use colored::Colorize;
use hubpack::SerializedSize;
use humility::net::{decode_iface, ScopedV6Addr};
use humility::{hubris::*, reflect};
use humility_cli::{ExecutionContext, Subcommand};
use humility_cmd::{Archive, Command, CommandKind};
use humility_doppel::RpcHeader;
use humility_idol as idol;
use serde::{Deserialize, Serialize};
use zerocopy::{AsBytes, U16, U64};

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

    /// call a particular function
    #[clap(long, short, conflicts_with = "list", requires = "target")]
    call: Option<String>,

    /// listen for compatible SPs on the network
    #[clap(
        long,
        conflicts_with = "list",
        group = "target",
        requires = "interface"
    )]
    listen: bool,

    /// arguments
    #[clap(long, short, requires = "call")]
    task: Option<String>,

    /// interface on which to listen, e.g. 'en0'
    #[clap(short, requires = "listen", value_parser = decode_iface)]
    interface: Option<u32>,

    /// arguments
    #[clap(long, short, requires = "call", use_delimiter = true)]
    arguments: Vec<String>,

    /// IPv6 addresses, e.g. `fe80::0c1d:9aff:fe64:b8c2%en0`
    #[clap(
        long,
        env = "HUMILITY_RPC_IP",
        group = "target",
        use_value_delimiter = true
    )]
    ip: Option<Vec<ScopedV6Addr>>,
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
    let socket = match UdpSocket::bind(&format!("[::]:{port}")) {
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
    for t in threads {
        let out = t.join().unwrap()?;
        seen.extend(out.into_iter());
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

pub struct RpcClient<'a> {
    hubris: &'a HubrisArchive,
    socket: UdpSocket,
    rpc_reply_type: &'a HubrisEnum,
    buf: [u8; 1024], // matches buffer size in `task-udprpc`
}

impl<'a> RpcClient<'a> {
    pub fn new(
        hubris: &'a HubrisArchive,
        ip: ScopedV6Addr,
        timeout: Duration,
    ) -> Result<Self> {
        // Hard-coded socket address, based on Hubris configuration
        let target = format!("[{ip}]:998");

        let dest = target.to_socket_addrs()?.collect::<Vec<_>>();
        let socket = UdpSocket::bind("[::]:0")?;
        socket.set_read_timeout(Some(timeout))?;
        socket.connect(&dest[..])?;

        let rpc_task = hubris.lookup_task("udprpc").ok_or_else(|| {
            anyhow!(
                "Could not find `udprpc` task in this image. \
                 Only -dev and -lab images include `udprpc`; \
                 are you running a production image?"
            )
        })?;
        let rpc_reply_type = hubris
            .lookup_module(*rpc_task)?
            .lookup_enum_byname(hubris, "RpcReply")?
            .ok_or_else(|| anyhow!("can't find RpcReply"))?;

        Ok(Self { hubris, socket, rpc_reply_type, buf: [0; 1024] })
    }

    pub fn call(
        &mut self,
        op: &idol::IdolOperation,
        args: &[(&str, idol::IdolArgument)],
    ) -> Result<Result<reflect::Value, String>> {
        let payload = op.payload(args)?;

        let our_image_id = self.hubris.image_id().unwrap();

        let nreply = op.reply_size()?;

        let header = RpcHeader {
            image_id: U64::from_bytes(our_image_id.try_into().unwrap()),
            task: U16::new(op.task.task().try_into().unwrap()),
            op: U16::new(op.code),
            nreply: U16::new(nreply as u16),
            nbytes: U16::new(payload.len().try_into().unwrap()),
        };
        let mut packet = header.as_bytes().to_vec();
        packet.extend(payload.iter());

        self.socket.send(&packet)?;
        let n = self.socket.recv(&mut self.buf)?;
        let buf = &self.buf[..n];

        if buf[0] != 0 {
            match self.rpc_reply_type.lookup_variant_by_tag(Tag::from(buf[0])) {
                Some(e) => {
                    let msg = format!("Got error from `udprpc`: {}", e.name);
                    if e.name == "BadImageId" {
                        bail!(
                            "{msg}: {:02x?} (Humility) {:02x?} (Hubris)",
                            our_image_id,
                            &buf[1..9]
                        );
                    } else {
                        bail!("{msg}");
                    }
                }
                None => bail!("Got unknown error from `udprpc`: {}", buf[0]),
            }
        } else {
            // Check the return code from the Idol call
            let rc = u32::from_be_bytes(buf[1..5].try_into().unwrap());
            let val = if rc == 0 { Ok(buf[5..].to_vec()) } else { Err(rc) };
            let result = humility_hiffy::hiffy_decode(self.hubris, op, val)?;
            Ok(result)
        }
    }
}

fn rpc_call(
    hubris: &HubrisArchive,
    op: &idol::IdolOperation,
    args: &[(&str, idol::IdolArgument)],
    ips: Vec<ScopedV6Addr>,
    timeout: u32,
) -> Result<()> {
    let timeout = Duration::from_millis(u64::from(timeout));

    for &ip in &ips {
        let mut client = RpcClient::new(hubris, ip, timeout)?;
        let result = client.call(op, args)?;
        print!("{:25} ", ip);
        humility_hiffy::hiffy_print_result(hubris, op, result)?;
    }

    Ok(())
}

fn rpc_run(context: &mut ExecutionContext) -> Result<()> {
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let subargs = RpcArgs::try_parse_from(subargs)?;
    let hubris = context.archive.as_ref().unwrap();

    if subargs.list {
        cmd_hiffy::hiffy_list(hubris, vec![])?;
        return Ok(());
    }

    if subargs.listen && subargs.call.is_none() {
        rpc_dump(rpc_listen(&subargs)?, hubris.image_id().unwrap());
        return Ok(());
    }

    // For some reason, macOS requires the interface to be non-zero:
    // https://users.rust-lang.org/t/ipv6-upnp-multicast-for-rust-dlna-server-macos/24425
    // https://bluejekyll.github.io/blog/posts/multicasting-in-rust/
    let interface = match subargs.interface {
        None => {
            if cfg!(target_os = "macos") {
                bail!("Must specify interface with `-i` on macOS");
            } else {
                0
            }
        }
        Some(iface) => iface,
    };

    let ips = if subargs.listen {
        let image_id = hubris.image_id().unwrap();

        rpc_listen(&subargs)?
            .iter()
            .filter(|t| t.image_id == image_id)
            .map(|target| match target.ip {
                IpAddr::V4(ip) => bail!(
                    "target {target:?} had an unanticipated \
                    IPv4 address ({ip})"
                ),
                IpAddr::V6(ip) => Ok(ScopedV6Addr { ip, scopeid: interface }),
            })
            .collect::<Result<Vec<_>>>()?
    } else {
        subargs.ip.ok_or_else(|| {
            anyhow!(
                "the `--ip <IPS>` argument is required by `humility rpc` \
                unless `--listen` is also set"
            )
        })?
    };

    if let Some(call) = &subargs.call {
        if hubris.lookup_task("udprpc").is_none() {
            bail!("no `udprpc` task in the target image");
        }

        let func: Vec<&str> = call.split('.').collect();

        if func.len() != 2 {
            bail!("calls must be interface.operation (-l to list)");
        }

        let mut args = vec![];

        for arg in &subargs.arguments {
            let arg: Vec<&str> = arg.split('=').collect();

            if arg.len() != 2 {
                bail!("arguments must be argument=value (-l to list)");
            }

            args.push((arg[0], idol::IdolArgument::String(arg[1])));
        }

        let task = match &subargs.task {
            Some(task) => Some(
                hubris
                    .lookup_task(task)
                    .ok_or_else(|| anyhow!("unknown task \"{}\"", task))?,
            ),
            None => None,
        };

        let op = idol::IdolOperation::new(hubris, func[0], func[1], task)?;
        rpc_call(hubris, &op, &args, ips, subargs.timeout)?;

        return Ok(());
    }

    bail!("expected --listen, --list, or --call")
}

pub fn init() -> Command {
    Command {
        app: RpcArgs::command(),
        name: "rpc",
        run: rpc_run,
        kind: CommandKind::Detached { archive: Archive::Required },
    }
}
