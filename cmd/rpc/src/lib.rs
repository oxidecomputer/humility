// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility rpc`
//!
//! `humility rpc` allows for querying and manipulation of Idol commands.
//!
//! To list all Idol interfaces present in Hubris, use the `-l` (`--list`) option:
//!
//! ```console
//! % humility rpc -l
//! TASK            INTERFACE    OPERATION           ARG             ARGTYPE
//! rcc_driver      Rcc          enable_clock_raw    peripheral      u32
//!                              disable_clock_raw   peripheral      u32
//!                              enter_reset_raw     peripheral      u32
//!                              leave_reset_raw     peripheral      u32
//! spi_driver      Spi          read                device_index    u8
//!                              write               device_index    u8
//!                              exchange            device_index    u8
//!                              lock                device_index    u8
//!                                                  cs_state        CsState
//!                              release             -
//! user_leds       UserLeds     led_on              index           usize
//!                              led_off             index           usize
//!                              led_toggle          index           usize
//! ```
//!
//! To enlist the `udprpc` task to call a particular interface and operation,
//! use `-c` (`--call`), using `-a` (`--arguments`) to indicate any arguments,
//! e.g.:
//!
//! ```console
//! % humility rpc -c UserLeds.led_toggle -a index=0
//! UserLeds.led_toggle() = ()
//! ```

use std::collections::BTreeSet;
use std::net::{Ipv6Addr, ToSocketAddrs, UdpSocket};
use std::time::{Duration, Instant};

use anyhow::{anyhow, bail, Result};
use clap::App;
use clap::IntoApp;
use clap::Parser;
use colored::Colorize;
use humility::hubris::*;
use humility_cmd::doppel::RpcHeader;
use humility_cmd::idol;
use humility_cmd::{Archive, Command, RunUnattached};
use zerocopy::AsBytes;

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
    println!("Making socket");
    let socket = UdpSocket::bind("[::]:8")?;
    let timeout = Duration::from_millis(rpc_args.timeout as u64);
    socket.set_read_timeout(Some(timeout))?;

    // For some reason, macOS requires the interface to be non-zero:
    // https://users.rust-lang.org/t/ipv6-upnp-multicast-for-rust-dlna-server-macos/24425
    // https://bluejekyll.github.io/blog/posts/multicasting-in-rust/
    let interface = if cfg!(target_os = "macos") {
        match &rpc_args.interface {
            None => bail!("Must specify interface with `-i` on macOS"),
            Some(iface) => unsafe {
                let iface_c = std::ffi::CString::new(iface.clone()).unwrap();
                libc::if_nametoindex(iface_c.as_ptr())
            },
        }
    } else {
        0
    };
    socket.join_multicast_v6(
        &Ipv6Addr::new(0xff02, 0, 0, 0, 0, 0, 0, 1),
        interface,
    )?;

    let mut seen = BTreeSet::new();
    humility::msg!("Listening (ctrl-C to cancel, or timeout in {:?})", timeout);
    let timeout = Instant::now() + timeout;
    let mut buf = [0u8; 1024];
    let mut printed_header = false;
    loop {
        match socket.recv(&mut buf) {
            Ok(n) => {
                let mac: [u8; 6] = buf[..6].try_into().unwrap();
                if seen.insert(mac) {
                    if !printed_header {
                        println!(
                            "      {}         |              {}         | {}",
                            "MAC".bold(),
                            "IPv6".bold(),
                            "Compatible".bold()
                        );
                        println!(
                            "------------------|\
                             ---------------------------|\
                             -----------"
                        );
                        printed_header = true;
                    }
                    let ip6 = humility_cmd_net::mac_to_ip6(mac);
                    for (i, byte) in mac.iter().enumerate() {
                        if i > 0 {
                            print!(":");
                        }
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
                if timeout <= Instant::now() {
                    break;
                }
            }
            Err(e) if e.kind() == std::io::ErrorKind::TimedOut => {
                if timeout <= Instant::now() {
                    panic!("Timeout");
                }
            }
            Err(e) => {
                panic!("Got error {:?}", e);
            }
        }
    }
    Ok(())
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
    let iface =
        iter.next().ok_or_else(|| anyhow!("Missing scope id (e.g. '%en0')"))?;

    // Work around https://github.com/rust-lang/rust/issues/65976 by manually
    // converting from scopeid to numerical value.  I'm not any happier about
    // this than you are!
    let scopeid: u32 = iface.parse().unwrap_or_else(|_| unsafe {
        let iface_c = std::ffi::CString::new(iface).unwrap();
        libc::if_nametoindex(iface_c.as_ptr())
    });

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
        image_id: u64::from_le_bytes(our_image_id.try_into().unwrap()),
        task: op.task.task().try_into().unwrap(),
        op: op.code as u16,
        nreply: hubris.typesize(op.ok)? as u16,
        nbytes: payload.len().try_into().unwrap(),
    };
    let mut packet = header.as_bytes().to_vec();
    packet.extend(payload.iter());

    socket.send(&packet)?;
    let timeout = Instant::now() + timeout;
    let mut buf = [0u8; 1024]; // matches buffer size in `task-udprpc`
    loop {
        match socket.recv(&mut buf) {
            Ok(_n) => {
                break;
            }
            Err(e) if e.kind() == std::io::ErrorKind::TimedOut => {
                if timeout <= Instant::now() {
                    panic!("Timeout");
                }
            }
            Err(e) => {
                panic!("Got error {:?}", e);
            }
        }
    }
    // Handle errors from the RPC task itself, which are reported as a non-zero
    // first byte in the reply packet.
    if buf[0] != 0 {
        let task = hubris.lookup_task("udprpc").ok_or_else(|| {
            anyhow!(
                "Could not find `udprpc` task in this image. \
                 Is it up to date?"
            )
        })?;
        let m = hubris.lookup_module(*task)?;
        match m
            .lookup_enum_byname(hubris, "RpcReply")?
            .lookup_variant(buf[0] as u64)
        {
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

fn rpc_run(hubris: &mut HubrisArchive, subargs: &[String]) -> Result<()> {
    let subargs = RpcArgs::try_parse_from(subargs)?;

    if subargs.list {
        humility_cmd_hiffy::hiffy_list(hubris, subargs.verbose)?;
        return Ok(());
    } else if subargs.listen {
        return rpc_listen(hubris, &subargs);
    }

    if let Some(call) = &subargs.call {
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

    Ok(())
}

pub fn init() -> (Command, App<'static>) {
    (
        Command::Unattached {
            name: "rpc",
            archive: Archive::Required,
            run: RunUnattached::Subargs(rpc_run),
        },
        RpcArgs::into_app(),
    )
}
