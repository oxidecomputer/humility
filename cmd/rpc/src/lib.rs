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
//!
//! To view the raw HIF functions provided to programmatic HIF consumers
//! within Humility, use `-L` (`--list-functions`).
//!

use std::net::{ToSocketAddrs, UdpSocket};
use std::time::{Duration, Instant};

use ::idol::syntax::{Operation, Reply};
use anyhow::{anyhow, bail, Result};
use clap::App;
use clap::IntoApp;
use clap::Parser;
use humility::hubris::*;
use humility_cmd::idol;
use humility_cmd::{Archive, Args, Command};

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

    /// list HIF functions
    #[clap(long = "list-functions", short = 'L')]
    listfuncs: bool,

    /// list interfaces
    #[clap(long, short, conflicts_with = "listfuncs")]
    list: bool,

    /// call a particular function
    #[clap(long, short, conflicts_with_all = &["list", "listfuncs"])]
    call: Option<String>,

    /// arguments
    #[clap(long, short, requires = "call")]
    task: Option<String>,

    /// arguments
    #[clap(long, short, requires = "call", use_delimiter = true)]
    arguments: Vec<String>,

    /// rpc IPv6 address
    #[clap(long, short, env = "HUMILITY_RPC_IP")]
    pub target: String,
}

fn rpc_list(hubris: &HubrisArchive, subargs: &RpcArgs) -> Result<()> {
    println!(
        "{:<15} {:<12} {:<19} {:<15} {:<15}",
        "TASK", "INTERFACE", "OPERATION", "ARG", "ARGTYPE"
    );

    let print_args = |op: &(&String, &Operation), module, margin| {
        let mut args = op.1.args.iter();
        let m = margin;

        match args.next() {
            None => {
                println!("-");
            }
            Some(arg) => {
                println!("{:<15} {}", arg.0, arg.1.ty.0);

                for arg in args {
                    println!("{:m$}{:<15} {}", "", arg.0, arg.1.ty.0, m = m);
                }
            }
        }

        if !subargs.verbose {
            return;
        }

        match idol::lookup_reply(hubris, module, op.0) {
            Ok((_, e)) => match &op.1.reply {
                Reply::Result { ok, .. } => {
                    println!("{:m$}{:<15} {}", "", "<ok>", ok.ty.0, m = m);
                    println!("{:m$}{:<15} {}", "", "<error>", e.name, m = m);
                }
            },
            Err(e) => {
                log::warn!("{}", e);
            }
        }
    };

    for i in 0..hubris.ntasks() {
        let module = hubris.lookup_module(HubrisTask::Task(i as u32))?;

        if let Some(iface) = &module.iface {
            let mut ops = iface.ops.iter();

            print!("{:15} {:<12} ", module.name, iface.name);

            match ops.next() {
                None => {
                    println!("-");
                }
                Some(op) => {
                    print!("{:<20}", op.0);
                    print_args(&op, module, 49);

                    for op in ops {
                        print!("{:29}{:<20}", "", op.0);
                        print_args(&op, module, 49);
                    }
                }
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
    let dest = rpc_args.target.to_socket_addrs()?.collect::<Vec<_>>();
    let socket = UdpSocket::bind("[::]:0")?;
    socket.set_read_timeout(Some(Duration::from_millis(
        rpc_args.timeout as u64,
    )))?;
    socket.connect(&dest[..])?;

    let mut packet = vec![];
    let payload = op.payload(args)?;

    // This has to match the decoding logic in `task-udprpc`
    if let HubrisTask::Task(id) = op.task {
        packet.extend((id as u16).to_be_bytes());
    } else {
        bail!("interface matches invalid task {:?}", op.task);
    }
    packet.extend((op.code as u16).to_be_bytes());
    packet.extend((hubris.typesize(op.ok)? as u16).to_be_bytes());
    packet.extend((payload.len() as u16).to_be_bytes());

    for b in payload {
        packet.push(b);
    }

    socket.send(&packet)?;
    let timeout = Instant::now() + Duration::from_secs(1);
    let mut buf = [0u8; 256];
    let out_bytes = loop {
        match socket.recv(&mut buf) {
            Ok(_n) => {
                break buf;
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
    };
    let code = u32::from_be_bytes(buf[0..4].try_into().unwrap());
    if code == 0 {
        let fmt =
            HubrisPrintFormat { hex: true, ..HubrisPrintFormat::default() };
        let dumped = hubris.printfmt(&buf[4..], op.ok, &fmt)?;
        println!("{}.{}() = {}", op.name.0, op.name.1, dumped);
    } else {
        println!("Err({:x?})", code);
    }

    /*
    let payload = op.payload(args)?;
    context.idol_call_ops(&funcs, op, &payload, &mut ops)?;
    ops.push(Op::Done);

    let results = context.run(core, ops.as_slice(), None)?;

    if results.len() != 1 {
        bail!("unexpected results length: {:?}", results);
    }

    let result = &results[0];
    let fmt = HubrisPrintFormat {
        newline: false,
        hex: true,
        ..HubrisPrintFormat::default()
    };

    match result {
        Ok(val) => {
            let dumped = hubris.printfmt(val, op.ok, &fmt)?;
            println!("{}.{}() = {}", op.name.0, op.name.1, dumped);
        }
        Err(e) => {
            println!("Err({:x?})", e);
        }
    }
    */

    Ok(())
}

fn rpc_run(
    hubris: &mut HubrisArchive,
    _args: &Args,
    subargs: &[String],
) -> Result<()> {
    let subargs = RpcArgs::try_parse_from(subargs)?;

    if subargs.list {
        rpc_list(hubris, &subargs)?;
        return Ok(());
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
            run: rpc_run,
        },
        RpcArgs::into_app(),
    )
}
