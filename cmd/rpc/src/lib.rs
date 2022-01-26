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
//! target; `udprcp` will refuse to execute commands when the ID does not match.
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
//!
//! (On macOS, it is mandatory to specify the interface, e.g. `humility rpc
//! --listen -ien0`)

use std::collections::BTreeSet;
use std::net::{Ipv6Addr, ToSocketAddrs, UdpSocket};
use std::time::{Duration, Instant};

use anyhow::{anyhow, bail, Result};
use clap::App;
use clap::IntoApp;
use clap::Parser;
use colored::Colorize;
use humility::cli::Subcommand;
use humility::hubris::*;
use humility_cmd::doppel::RpcHeader;
use humility_cmd::idol;
use humility_cmd::{Archive, Command};
use zerocopy::{AsBytes, U16, U64};

#[derive(Parser, Debug)]
#[clap(name = "rpc", about = env!("CARGO_PKG_DESCRIPTION"))]
struct RpcArgs {
    /// sets timeout
    #[clap(
        long, short = 'T', default_value = "5000", value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// verbose
    #[clap(long, short)]
    verbose: bool,

    /// list interfaces
    #[clap(long, short)]
    list: bool,

    /// call a particular function
    #[clap(long, short, conflicts_with_all = &["list"], requires = "ip")]
    call: Option<String>,

    /// listen for compatible SPs on the network
    #[clap(long, conflicts_with_all = &["list", "call"])]
    listen: bool,

    /// arguments
    #[clap(long, short, requires = "call")]
    task: Option<String>,

    /// interface on which to listen, e.g. 'en0' (required on macOS)
    #[clap(short, requires = "listen")]
    interface: Option<String>,

    /// arguments
    #[clap(long, short, requires = "call", use_delimiter = true)]
    arguments: Vec<String>,

    /// IPv6 address, e.g. `fe80::0c1d:9aff:fe64:b8c2%en0`
    #[clap(long, env = "HUMILITY_RPC_IP")]
    ip: Option<String>,
}

fn rpc_listen(hubris: &HubrisArchive, rpc_args: &RpcArgs) -> Result<()> {
    let socket = UdpSocket::bind("[::]:8")?;
    let timeout = Duration::from_millis(rpc_args.timeout as u64);
    socket.set_read_timeout(Some(timeout))?;

    // For some reason, macOS requires the interface to be non-zero:
    // https://users.rust-lang.org/t/ipv6-upnp-multicast-for-rust-dlna-server-macos/24425
    // https://bluejekyll.github.io/blog/posts/multicasting-in-rust/
    let interface = if cfg!(target_os = "macos") {
        match &rpc_args.interface {
            None => bail!("Must specify interface with `-i` on macOS"),
            Some(iface) => decode_iface(iface)?,
        }
    } else {
        0
    };
    socket.join_multicast_v6(
        &Ipv6Addr::new(0xff02, 0, 0, 0, 0, 0, 0, 1),
        interface,
    )?;

    let mut seen = BTreeSet::new();
    humility::msg!(
        "listening... (ctrl-C to stop, or timeout in {:?})",
        timeout
    );
    let timeout = Instant::now() + timeout;
    let mut buf = [0u8; 1024];
    let mut printed_header = false;
    loop {
        match socket.recv(&mut buf) {
            Ok(n) => {
                if n != 14 {
                    humility::msg!("Skipping unknown packet {:?}", &buf[..n]);
                    continue;
                } else {
                    let mac: [u8; 6] = buf[..6].try_into().unwrap();
                    if mac[0] != 0x0e || mac[1] != 0x1d {
                        humility::msg!(
                            "Skipping packet with non-matching MAC {:?}",
                            mac
                        );
                    } else if seen.insert(mac) {
                        if !printed_header {
                            println!(
                                "       {}         |            {}           | {}",
                                "MAC".bold(),
                                "IPv6".bold(),
                                "Compatible".bold()
                            );
                            println!(
                                "-------------------|\
                                 ---------------------------|\
                                 -----------"
                            );
                            printed_header = true;
                        }
                        let ip6 = humility_cmd_net::mac_to_ip6(mac);
                        for (i, byte) in mac.iter().enumerate() {
                            print!("{}", if i == 0 { " " } else { ":" });
                            print!("{:02x}", byte)
                        }
                        print!(" | {} | ", ip6);
                        let image_id = &buf[6..n];
                        if image_id == hubris.image_id().unwrap() {
                            println!("{}", "Yes".green());
                        } else {
                            println!("{}", "No".red());
                        }
                    }
                }

                if timeout <= Instant::now() {
                    break;
                }
            }
            Err(e) => {
                // At least on macOS, timeouts are reported as `WouldBlock`,
                // rather than `TimedOut`.
                match e.kind() {
                    std::io::ErrorKind::WouldBlock
                    | std::io::ErrorKind::TimedOut => {
                        if !printed_header {
                            humility::msg!("timed out, exiting");
                        }
                        break;
                    }
                    e => panic!("Got error {:?}", e),
                }
            }
        }
    }
    Ok(())
}

fn decode_iface(iface: &str) -> Result<u32> {
    #[cfg(not(windows))]
    use libc::if_nametoindex;
    #[cfg(windows)]
    use winapi::shared::netioapi::if_nametoindex;

    // Work around https://github.com/rust-lang/rust/issues/65976 by manually
    // converting from scopeid to numerical value.  I'm not any happier about
    // this than you are!
    let iface_c = std::ffi::CString::new(iface).unwrap();
    let scopeid: u32 = iface
        .parse()
        .unwrap_or_else(|_| unsafe { if_nametoindex(iface_c.as_ptr()) });
    if scopeid == 0 {
        bail!("Could not find interface for {}", iface);
    }
    Ok(scopeid)
}

fn rpc_call(
    hubris: &HubrisArchive,
    op: &idol::IdolOperation,
    args: &[(&str, idol::IdolArgument)],
    rpc_args: &RpcArgs,
) -> Result<()> {
    // rpc_args.ip must be Some(...), checked by clap
    let mut iter = rpc_args.ip.as_ref().unwrap().split('%');
    let ip = iter.next().unwrap();
    let iface = iter
        .next()
        .ok_or_else(|| anyhow!("Missing scope id in IP (e.g. '%en0')"))?;

    let scopeid = decode_iface(iface)?;

    // Hard-coded socket address, based on Hubris configuration
    let target = format!("[{}%{}]:998", ip, scopeid);
    humility::msg!("Connecting to {}", target);

    let dest = target.to_socket_addrs()?.collect::<Vec<_>>();
    let socket = UdpSocket::bind("[::]:0")?;
    let timeout = Duration::from_millis(rpc_args.timeout as u64);
    socket.set_read_timeout(Some(timeout))?;
    socket.connect(&dest[..])?;

    let payload = op.payload(args)?;

    let our_image_id = hubris.image_id().unwrap();

    let header = RpcHeader {
        image_id: U64::from_bytes(our_image_id.try_into().unwrap()),
        task: U16::new(op.task.task().try_into().unwrap()),
        op: U16::new(op.code as u16),
        nreply: U16::new(hubris.typesize(op.ok)? as u16),
        nbytes: U16::new(payload.len().try_into().unwrap()),
    };
    let mut packet = header.as_bytes().to_vec();
    packet.extend(payload.iter());

    socket.send(&packet)?;
    let mut buf = [0u8; 1024]; // matches buffer size in `task-udprpc`
    socket.recv(&mut buf)?;
    // Handle errors from the RPC task itself, which are reported as a non-zero
    // first byte in the reply packet.
    let rpc_task = hubris.lookup_task("udprpc").ok_or_else(|| {
        anyhow!(
            "Could not find `udprpc` task in this image. \
                 Is it up to date?"
        )
    })?;
    let rpc_reply_type = hubris
        .lookup_module(*rpc_task)?
        .lookup_enum_byname(hubris, "RpcReply")?;

    if buf[0] != 0 {
        match rpc_reply_type.lookup_variant(buf[0] as u64) {
            Some(e) => {
                println!("Got error from `udprpc`: {}", e.name);
                if e.name == "BadImageId" {
                    println!("   {:02x?} (Humility)", our_image_id);
                    println!("   {:02x?} (Hubris)", &buf[1..9]);
                }
            }
            None => println!("Got unknown error from `udprpc`: {}", buf[0]),
        }
    } else {
        // Check the return code from the Idol call
        let rc = u32::from_be_bytes(buf[1..5].try_into().unwrap());
        let val = if rc == 0 { Ok(buf[5..].to_vec()) } else { Err(rc) };
        let result = humility_cmd_hiffy::hiffy_decode(hubris, op, val)?;
        humility_cmd_hiffy::hiffy_print_result(hubris, op, result)?;
    }

    Ok(())
}

fn rpc_run(context: &mut humility::ExecutionContext) -> Result<()> {
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let subargs = RpcArgs::try_parse_from(subargs)?;
    let hubris = context.archive.as_ref().unwrap();

    if subargs.list {
        humility_cmd_hiffy::hiffy_list(hubris, subargs.verbose)?;
        return Ok(());
    } else if subargs.listen {
        return rpc_listen(hubris, &subargs);
    }

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
        rpc_call(hubris, &op, &args, &subargs)?;

        return Ok(());
    }

    bail!("expected --listen, --list, or --call")
}

pub fn init() -> (Command, App<'static>) {
    (
        Command::Unattached {
            name: "rpc",
            archive: Archive::Required,
            run: rpc_run,
        },
        RpcArgs::into_app(),
    )
}
