// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility hiffy`
//!
//! `humility hiffy` allows for querying and manipulation of `hiffy`, the
//! HIF agent present in Hubris.  To list all Idol interfaces present in
//! Hubris, use the `-l` (`--list`) option:
//!
//! ```console
//! % humility hiffy -l
//! humility: attached via ST-Link
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
//! To enlist the Hubris agent to call a particular interface and operation,
//! use `-c` (`--call`), using `-a` (`--arguments`) to indicate any arguments,
//! e.g.:
//!
//! ```console
//! % humility hiffy -c UserLeds.led_toggle -a index=0
//! humility: attached via ST-Link
//! UserLeds.led_toggle() = ()
//! ```
//!
//! To view the raw HIF functions provided to programmatic HIF consumers
//! within Humility, use `-L` (`--list-functions`).
//!

use ::idol::syntax::{Operation, Reply};
use anyhow::{anyhow, bail, Result};
use clap::Command as ClapCommand;
use clap::{CommandFactory, Parser};
use hif::*;
use humility::core::Core;
use humility::hubris::*;
use humility_cmd::hiffy::*;
use humility_cmd::idol;
use humility_cmd::{Archive, Args, Attach, Command, Validate};

#[derive(Parser, Debug)]
#[clap(name = "hiffy", about = env!("CARGO_PKG_DESCRIPTION"))]
struct HiffyArgs {
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
    #[clap(long, short, requires = "call", use_value_delimiter = true)]
    arguments: Vec<String>,
}

fn hiffy_list(hubris: &HubrisArchive, subargs: &HiffyArgs) -> Result<()> {
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
            Ok((_, Some(e))) => match &op.1.reply {
                Reply::Result { ok, .. } => {
                    println!("{:m$}{:<15} {}", "", "<ok>", ok.ty.0, m = m);
                    println!("{:m$}{:<15} {}", "", "<error>", e.name, m = m);
                }
                _ => {
                    log::warn!("Mismatch between expected reply and operation");
                }
            },
            Ok((_, None)) => match &op.1.reply {
                Reply::Simple(ok) => {
                    println!("{:m$}{:<15} {}", "", "<ok>", ok.ty.0, m = m);
                }
                _ => {
                    log::warn!("Mismatch between expected reply and operation");
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

fn hiffy_call(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
    op: &idol::IdolOperation,
    args: &[(&str, idol::IdolArgument)],
) -> Result<()> {
    let funcs = context.functions()?;
    let mut ops = vec![];

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
            let ty = hubris.lookup_type(op.ok).unwrap();
            let v = match op.operation.encoding {
                ::idol::syntax::Encoding::Zerocopy => {
                    humility_cmd::reflect::load_value(hubris, val, ty, 0)?
                }
                ::idol::syntax::Encoding::Ssmarshal => {
                    humility_cmd::reflect::deserialize_value(hubris, val, ty)?.0
                }
            };

            use humility_cmd::reflect::Format;
            let mut dumped = vec![];
            v.format(hubris, fmt, &mut dumped).unwrap();

            println!(
                "{}.{}() = {}",
                op.name.0,
                op.name.1,
                std::str::from_utf8(&dumped).unwrap()
            );
        }
        Err(e) => {
            let variant = if let Some(error) = op.error {
                error.lookup_variant(*e as u64)
            } else {
                None
            };

            if let Some(variant) = variant {
                println!("Err({})", variant.name);
            } else {
                println!("Err({:x?})", e);
            }
        }
    }

    Ok(())
}

fn hiffy(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    _args: &Args,
    subargs: &[String],
) -> Result<()> {
    let subargs = HiffyArgs::try_parse_from(subargs)?;

    if subargs.list {
        hiffy_list(hubris, &subargs)?;
        return Ok(());
    }

    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;

    if let Some(call) = subargs.call {
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

        let task = match subargs.task {
            Some(task) => Some(
                hubris
                    .lookup_task(&task)
                    .ok_or_else(|| anyhow!("unknown task \"{}\"", task))?,
            ),
            None => None,
        };

        let op = idol::IdolOperation::new(hubris, func[0], func[1], task)?;
        hiffy_call(hubris, core, &mut context, &op, &args)?;

        return Ok(());
    }

    if !subargs.listfuncs {
        bail!("expected one of -l, -L, or -c");
    }

    let funcs = context.functions()?;
    let mut byid: Vec<Option<(&String, &HiffyFunction)>> = vec![];

    byid.resize(funcs.len(), None);

    for (name, func) in &funcs.0 {
        let ndx = func.id.0 as usize;

        if ndx >= byid.len() {
            bail!("ID for function {} ({}) exceeds bounds", name, ndx);
        }

        if let Some((_, _)) = byid[ndx] {
            bail!("function ID {} has conflics", ndx);
        }

        byid[ndx] = Some((name, func));
    }

    println!("{:>3} {:30} #ARGS", "ID", "FUNCTION");

    for (i, id) in byid.iter().enumerate() {
        if let Some((name, func)) = id {
            println!("{:3} {:30} {}", i, name, func.args.len());
        } else {
            bail!("missing function for ID {}", i);
        }
    }

    Ok(())
}

pub fn init() -> (Command, ClapCommand<'static>) {
    (
        Command::Attached {
            name: "hiffy",
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
            run: hiffy,
        },
        HiffyArgs::command(),
    )
}
