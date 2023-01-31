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
//! % rpc --ip fe80::0c1d:9aff:fe64:b8c2%en0 -c UserLeds.led_on -aindex=0
//! UserLeds.led_on() = ()
//! ```
//!
//! Alternatively, you can set the `HUMILITY_RPC_IP` environmental variable.
//!
//! You may need to configure an IPv6 network for `humility rpc` to work. On
//! illumos, it looks like this:
//!
//! ```console
//! % pfexec ipadm create-addr -t -T addrconf e1000g0/addrconf
//! ```
//!
//! To listen for compatible devices on your network, run `humility rpc
//! --listen`
//!
//! ```console
//! % humility rpc --listen
//! humility: listening... (ctrl-C to stop, or timeout in 5s)
//!        MAC         |            IPv6           | Compatible
//! -------------------|---------------------------|-----------
//!  0e:1d:27:87:03:bc | fe80::0c1d:27ff:fe87:03bc | Yes
//!  0e:1d:38:73:ce:c3 | fe80::0c1d:38ff:fe73:cec3 | No
//!  0e:1d:8d:3d:da:79 | fe80::0c1d:8dff:fe3d:da79 | No
//! ```
//!
//! Under the hood, this listens for packets from the Hubris `udpbroadcast`
//! task, which includes MAC address and image ID (checked for compatibility).
//! When listening, it is mandatory to specify the interface (e.g. `humility rpc
//! --listen -i en0` on MacOS).
//!
//! To call all targets that match an archive, `--listen` can be combined with
//! `--call`

use std::collections::BTreeSet;
use std::net::{IpAddr, Ipv6Addr, ToSocketAddrs, UdpSocket};
use std::time::{Duration, Instant};

use cmd_hiffy as humility_cmd_hiffy;

use anyhow::{anyhow, bail, Result};
use clap::{ArgGroup, IntoApp, Parser};
use colored::Colorize;
use humility::cli::Subcommand;
use humility::net::decode_iface;
use humility::{hubris::*, reflect};
use humility_cmd::doppel::RpcHeader;
use humility_cmd::{idol, CommandKind};
use humility_cmd::{Archive, Command};
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
    #[clap(short, requires = "listen")]
    interface: Option<String>,

    /// arguments
    #[clap(long, short, requires = "call", use_delimiter = true)]
    arguments: Vec<String>,

    /// IPv6 address, e.g. `fe80::0c1d:9aff:fe64:b8c2%en0`
    #[clap(
        long,
        env = "HUMILITY_RPC_IP",
        group = "target",
        use_value_delimiter = true
    )]
    ip: Option<Vec<String>>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
struct Target {
    mac: [u8; 6],
    image_id: [u8; 8],
    ip: IpAddr,
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
                    "Cannot listen on port {}; permission denied",
                    port
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
                if n != 14 {
                    humility::msg!("Skipping unknown packet {:?}", &buf[..n]);
                    continue;
                } else {
                    let mac: [u8; 6] = buf[..6].try_into().unwrap();
                    if mac[0..2] != [0x0e, 0x1d]
                        && mac[0..3] != [0xa8, 0x40, 0x25]
                    {
                        humility::msg!(
                            "Skipping packet with non-matching MAC {:?}",
                            mac
                        );
                    } else {
                        seen.insert(Target {
                            mac,
                            image_id: buf[6..n].try_into().unwrap(),
                            ip: src.ip(),
                        });
                    }
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
        Some(iface) => decode_iface(iface)?,
    };

    let timeout = Duration::from_millis(rpc_args.timeout as u64);
    let ports = [8, 8888];
    humility::msg!(
        "listening for {} seconds on ports {:?}...",
        timeout.as_secs(),
        ports
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
            "       {}         |            {}           | {}",
            "MAC".bold(),
            "IPv6".bold(),
            "Compatible".bold()
        );

        println!(
            "-------------------|---------------------------| -----------"
        );
    }

    for target in seen {
        for (i, byte) in target.mac.iter().enumerate() {
            print!("{}", if i == 0 { " " } else { ":" });
            print!("{:02x}", byte)
        }
        print!(" | {:25} | ", target.ip);
        if target.image_id == image_id {
            println!("{}", "Yes".green());
        } else {
            println!("{}", "No".red());
        }
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
        ip: &str,
        timeout: Duration,
    ) -> Result<Self> {
        let mut iter = ip.split('%');
        let ip = iter.next().expect("ip address is empty");
        let iface = iter
            .next()
            .ok_or_else(|| anyhow!("Missing scope id in IP (e.g. '%en0')"))?;

        let scopeid = decode_iface(iface)?;

        // Hard-coded socket address, based on Hubris configuration
        let target = format!("[{}%{}]:998", ip, scopeid);

        let dest = target.to_socket_addrs()?.collect::<Vec<_>>();
        let socket = UdpSocket::bind("[::]:0")?;
        socket.set_read_timeout(Some(timeout))?;
        socket.connect(&dest[..])?;

        let rpc_task = hubris.lookup_task("udprpc").ok_or_else(|| {
            anyhow!(
                "Could not find `udprpc` task in this image. \
                 Is it up to date?"
            )
        })?;
        let rpc_reply_type = hubris
            .lookup_module(*rpc_task)?
            .lookup_enum_byname(hubris, "RpcReply")?;

        Ok(Self { hubris, socket, rpc_reply_type, buf: [0; 1024] })
    }

    pub fn call(
        &mut self,
        op: &idol::IdolOperation,
        args: &[(&str, idol::IdolArgument)],
    ) -> Result<Result<reflect::Value, String>> {
        let payload = op.payload(args)?;

        let our_image_id = self.hubris.image_id().unwrap();

        let nreply = match op.operation.encoding {
            ::idol::syntax::Encoding::Zerocopy => {
                self.hubris.typesize(op.ok)?
            }
            ::idol::syntax::Encoding::Ssmarshal
            | ::idol::syntax::Encoding::Hubpack => {
                self.hubris.hubpack_serialized_maxsize(op.ok)?
            }
        };

        let header = RpcHeader {
            image_id: U64::from_bytes(our_image_id.try_into().unwrap()),
            task: U16::new(op.task.task().try_into().unwrap()),
            op: U16::new(op.code as u16),
            nreply: U16::new(nreply as u16),
            nbytes: U16::new(payload.len().try_into().unwrap()),
        };
        let mut packet = header.as_bytes().to_vec();
        packet.extend(payload.iter());

        self.socket.send(&packet)?;
        let n = self.socket.recv(&mut self.buf)?;
        let buf = &self.buf[..n];

        if buf[0] != 0 {
            match self.rpc_reply_type.lookup_variant_by_tag(buf[0] as u64) {
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
            let result =
                humility_cmd_hiffy::hiffy_decode(self.hubris, op, val)?;
            Ok(result)
        }
    }
}

fn rpc_call(
    hubris: &HubrisArchive,
    op: &idol::IdolOperation,
    args: &[(&str, idol::IdolArgument)],
    ips: Vec<String>,
    timeout: u32,
) -> Result<()> {
    let timeout = Duration::from_millis(u64::from(timeout));

    for ip in &ips {
        let mut client = RpcClient::new(hubris, ip, timeout)?;
        let result = client.call(op, args)?;
        print!("{:25} ", ip);
        humility_cmd_hiffy::hiffy_print_result(hubris, op, result)?;
    }

    Ok(())
}

fn rpc_run(context: &mut humility::ExecutionContext) -> Result<()> {
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let subargs = RpcArgs::try_parse_from(subargs)?;
    let hubris = context.archive.as_ref().unwrap();

    if subargs.list {
        humility_cmd_hiffy::hiffy_list(hubris, vec![])?;
        return Ok(());
    }

    if subargs.listen && subargs.call.is_none() {
        rpc_dump(rpc_listen(&subargs)?, hubris.image_id().unwrap());
        return Ok(());
    }

    let ips = if subargs.listen {
        let image_id = hubris.image_id().unwrap();
        let interface: &str = subargs.interface.as_ref().unwrap();

        rpc_listen(&subargs)?
            .iter()
            .filter(|t| t.image_id == image_id)
            .map(|t| format!("{}%{}", t.ip, interface))
            .collect::<Vec<String>>()
    } else {
        subargs.ip.unwrap()
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
