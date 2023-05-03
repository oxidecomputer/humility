// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility hiffy`
//!
//! `humility hiffy` allows for querying and manipulation of `hiffy`, the
//! HIF agent present in Hubris.  To list all Idol interfaces present in
//! Hubris, use the `-l` (`--list`) option, optionally specifying a filter
//! for tasks or interface names if so desired:
//!
//! ```console
//! $ humility hiffy -l user_leds
//! humility: attached via ST-Link
//! INTERFACE                    TASK
//! UserLeds                     user_leds
//!   |
//!   +--> UserLeds.led_on
//!   |       index                       usize
//!   |       <ok>                        ()
//!   |       <error>                     LedError
//!   |
//!   +--> UserLeds.led_off
//!   |       index                       usize
//!   |       <ok>                        ()
//!   |       <error>                     LedError
//!   |
//!   +--> UserLeds.led_toggle
//!           index                       usize
//!           <ok>                        ()
//!           <error>                     LedError
//! ```
//!
//! To enlist the Hubris agent to call a particular interface and operation,
//! use `-c` (`--call`), using `-a` (`--arguments`) to indicate any arguments,
//! e.g.:
//!
//! ```console
//! $ humility hiffy -c UserLeds.led_toggle -a index=0
//! humility: attached via ST-Link
//! UserLeds.led_toggle() = ()
//! ```
//!
//! To view the raw HIF functions provided to programmatic HIF consumers
//! within Humility, use `-L` (`--list-functions`).
//!

use ::idol::syntax::{Operation, Reply};
use anyhow::{anyhow, bail, Context, Result};
use clap::{CommandFactory, Parser};
use humility::hubris::*;
use humility::warn;
use humility_cli::{ExecutionContext, Subcommand};
use humility_cmd::{Archive, Attach, Command, CommandKind, Dumper, Validate};
use humility_hiffy::*;
use humility_idol as idol;
use std::io::Read;

#[derive(Parser, Debug)]
#[clap(name = "hiffy", about = env!("CARGO_PKG_DESCRIPTION"))]
struct HiffyArgs {
    /// sets timeout
    #[clap(
        long, short = 'T', default_value_t = 5000, value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// list HIF functions
    #[clap(long = "list-functions", short = 'L')]
    listfuncs: bool,

    /// list interfaces
    #[clap(long, short, conflicts_with = "listfuncs")]
    list: bool,

    /// call a particular function
    #[clap(long, short, conflicts_with_all = &["list", "listfuncs"])]
    call: Option<String>,

    /// input for an operation that takes a lease
    #[clap(long, short, requires = "call", conflicts_with = "num")]
    input: Option<String>,

    /// number of bytes to return, when a function has a write-only lease
    #[clap(long, short, requires = "call", conflicts_with = "input")]
    num: Option<usize>,

    /// output for an operation that writes to a lease
    #[clap(long, short, requires = "call", conflicts_with = "input")]
    output: Option<String>,

    /// print returned data in hex
    #[clap(short = 'x', requires = "num")]
    hex: bool,

    /// arguments
    #[clap(long, short, requires = "call")]
    task: Option<String>,

    /// arguments
    #[clap(long, short, use_value_delimiter = true, requires = "call")]
    arguments: Vec<String>,

    /// filter for list output
    #[clap(use_value_delimiter = true)]
    filter: Vec<String>,
}

pub fn hiffy_list(hubris: &HubrisArchive, filter: Vec<String>) -> Result<()> {
    let print_args = |op: &(&String, &Operation), module, margin| {
        let mut args = op.1.args.iter();

        match args.next() {
            None => {}
            Some(arg) => {
                println!("{}{:<27} {}", margin, arg.0, arg.1.ty.0);

                for arg in args {
                    println!("{}{:<27} {}", margin, arg.0, arg.1.ty.0);
                }
            }
        }

        match idol::lookup_reply(hubris, module, op.0) {
            Ok((_, idol::IdolError::CLike(e))) => match &op.1.reply {
                Reply::Result { ok, .. } => {
                    println!("{}{:<27} {}", margin, "<ok>", ok.ty.0);
                    println!("{}{:<27} {}", margin, "<error>", e.name);
                }
                _ => warn!("mismatch on reply: found {op:?}"),
            },
            Ok((_, idol::IdolError::Complex(t))) => match &op.1.reply {
                Reply::Result { ok, .. } => {
                    println!("{}{:<27} {}", margin, "<ok>", ok.ty.0);
                    println!("{}{:<27} {}", margin, "<error>", t);
                }
                _ => warn!("mismatch on reply: found {op:?}"),
            },

            Ok((_, idol::IdolError::None)) => match &op.1.reply {
                Reply::Result { ok, .. } => {
                    //
                    // This is possible if the only error is ServerDeath
                    //
                    println!("{}{:<27} {}", margin, "<ok>", ok.ty.0);
                }
                Reply::Simple(ok) => {
                    println!("{}{:<27} {}", margin, "<ok>", ok.ty.0);
                }
            },
            Err(e) => {
                warn!("{}", e);
            }
        }
    };

    let mut matches = false;

    for i in 0..hubris.ntasks() {
        let module = hubris.lookup_module(HubrisTask::Task(i as u32))?;

        if let Some(iface) = &module.iface {
            if !filter.is_empty()
                && !filter.iter().any(|f| iface.name == *f || module.name == *f)
            {
                continue;
            }

            let mut ops = iface.ops.iter().peekable();

            matches = true;
            println!("{:<28} TASK", "INTERFACE");
            println!("{:<28} {}", iface.name, module.name);
            println!("  |");

            while let Some(op) = ops.next() {
                println!("  +--> {}.{}", iface.name, op.0);

                let last = ops.peek().is_none();
                let c = if last { "" } else { "|" };
                let margin = format!("  {:<8}", c);

                print_args(&op, module, margin);
                println!("  {}", c);
            }
        }
    }

    if !filter.is_empty() && !matches {
        bail!(
            "filter \"{}\" did not match any task or interface; \
            use --list without an argument to list all interfaces",
            filter.join(",")
        );
    }

    Ok(())
}

fn hiffy(context: &mut ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let hubris = context.archive.as_ref().unwrap();

    let subargs = HiffyArgs::try_parse_from(subargs)?;

    if subargs.list {
        hiffy_list(hubris, subargs.filter)?;
        return Ok(());
    } else if !subargs.filter.is_empty() {
        //
        // It is likely that the user has provided an argument to a HIF
        // call without specifying -a; generate a message that tries to
        // point them in the right direction.
        //
        bail!(
            "extraneous command line argument; missing {}?",
            if subargs.call.is_some() {
                "--arguments"
            } else {
                "--list or --call?"
            }
        );
    }

    //
    // Before we create our HiffyContext, check to see if this is a call and
    // we're on a dump; running call on a dump always fails (obviously?), but
    // in the event that we have a HIF mismatch (or any other failure to
    // create the HiffyContext) *and* we're running call on a dump, we would
    // rather fail with the dump message rather than with the HiffyContext
    // creation failure.  (Note that -L will still create the HiffyContext,
    // even if run on a dump.)
    //
    if subargs.call.is_some() && core.is_dump() {
        bail!("can't make HIF calls on a dump");
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

        // Very special-case handling: if someone didn't specify `--input`, but
        // is piping data into the `humility` command, then we use `stdin` as
        // the input source.
        let input = if let Some(input) = subargs.input {
            Some(std::fs::read(input)?)
        } else if op.operation.leases.len() == 1
            && op.operation.leases[0].read
            && !op.operation.leases[0].write
            && atty::isnt(atty::Stream::Stdin)
        {
            let mut v = vec![];
            std::io::stdin().read_to_end(&mut v)?;
            Some(v)
        } else {
            None
        };

        let (return_code, data) = if let Some(input) = input {
            (
                hiffy_call(
                    hubris,
                    core,
                    &mut context,
                    &op,
                    &args,
                    Some(HiffyLease::Write(&input)),
                )?,
                None,
            )
        } else if let Some(read_size) = subargs.num {
            let mut read = vec![0u8; read_size];
            let r = hiffy_call(
                hubris,
                core,
                &mut context,
                &op,
                &args,
                Some(HiffyLease::Read(&mut read)),
            )?;
            (r, Some(read))
        } else {
            (hiffy_call(hubris, core, &mut context, &op, &args, None)?, None)
        };

        hiffy_print_result(hubris, &op, return_code)?;
        if let Some(data) = data {
            if let Some(out) = &subargs.output {
                std::fs::write(out, &data)
                    .context(format!("Could not write to {}", out))?;
                println!("Wrote {} bytes to '{}'", data.len(), out);
            } else if subargs.hex {
                println!("Data: {:x?}", data);
            } else {
                Dumper::new().dump(&data, 0x0);
            }
        }

        return Ok(());
    }

    if !subargs.listfuncs {
        bail!("expected one of -l, -L, or -c");
    }

    let funcs = context.functions();
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

pub fn init() -> Command {
    Command {
        app: HiffyArgs::command(),
        name: "hiffy",
        run: hiffy,
        kind: CommandKind::Attached {
            archive: Archive::Required,
            attach: Attach::Any,
            validate: Validate::Booted,
        },
    }
}
