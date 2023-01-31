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
//! % humility hiffy -l user_leds
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
//! % humility hiffy -c UserLeds.led_toggle -a index=0
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
use hif::*;
use humility::cli::Subcommand;
use humility::core::Core;
use humility::hubris::*;
use humility::warn;
use humility_cmd::{hiffy::*, Archive, Attach, Command, Validate};
use humility_cmd::{idol, CommandKind};
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

#[derive(Debug)]
pub enum HiffyLease<'a> {
    Read(&'a mut [u8]),
    Write(&'a [u8]),
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
            Ok((_, Some(e))) => match &op.1.reply {
                Reply::Result { ok, .. } => {
                    println!("{}{:<27} {}", margin, "<ok>", ok.ty.0);
                    println!("{}{:<27} {}", margin, "<error>", e.name);
                }
                _ => {
                    warn!(
                        "mismatch on reply: expected Reply::Result, \
                            found {:?}",
                        op
                    );
                }
            },
            Ok((_, None)) => match &op.1.reply {
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

/// Check that the given operation and provided leases are compatible, bailing
/// with a user-friendly message if that's not the case.
fn check_lease(
    op: &idol::IdolOperation,
    lease: Option<&HiffyLease>,
) -> Result<()> {
    match lease {
        None => match op.operation.leases.len() {
            0 => (),
            1 => match (
                op.operation.leases[0].read,
                op.operation.leases[0].write,
            ) {
                (true, false) => {
                    bail!(
                        "this operation reads from a lease. \
                         Use `-i` to specify the data source"
                    );
                }
                (false, true) => {
                    bail!(
                        "this operation writes to a lease. \
                         Use `-n` to specify how much data you want back."
                    );
                }
                _ => {
                    bail!(
                        "cannot call a hiffy operation that uses a R/W \
                         (or nR/nW) lease"
                    )
                }
            },
            _ => bail!(
                "`humility hiffy` cannot call operations that use \
                 > 1 leases"
            ),
        },
        Some(HiffyLease::Read(..)) => {
            if op.operation.leases.len() != 1
                || op.operation.leases[0].read
                || !op.operation.leases[0].write
            {
                bail!(
                    "`humility hiffy --input ...` can only call functions that \
                     take a single, read-only lease"
                );
            }
        }
        Some(HiffyLease::Write(..)) => {
            if op.operation.leases.len() != 1
                || !op.operation.leases[0].read
                || op.operation.leases[0].write
            {
                bail!(
                    "`humility hiffy --num ...` can only call functions that \
                     take a single, read-only lease"
                );
            }
        }
    }
    Ok(())
}

/// Executes a Hiffy call, printing the output to the terminal
///
/// Returns an outer error if Hiffy communication fails, or an inner error
/// if the Hiffy call returns an error code (formatted as a String).
pub fn hiffy_call(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
    op: &idol::IdolOperation,
    args: &[(&str, idol::IdolArgument)],
    lease: Option<HiffyLease>,
) -> Result<std::result::Result<humility::reflect::Value, String>> {
    check_lease(op, lease.as_ref())?;

    let funcs = context.functions()?;
    let mut ops = vec![];

    let payload = op.payload(args)?;
    match lease.as_ref() {
        None => context.idol_call_ops(&funcs, op, &payload, &mut ops)?,
        // Read/Write is flipped when passing through the Idol operation;
        // HiffyLease::Read/Write is from the perspective of the host, but
        // idol_call_ops_read/write is from the perspective of the called
        // function.
        Some(HiffyLease::Read(n)) => context.idol_call_ops_write(
            &funcs,
            op,
            &payload,
            &mut ops,
            n.len().try_into().unwrap(),
        )?,
        Some(HiffyLease::Write(d)) => context.idol_call_ops_read(
            &funcs,
            op,
            &payload,
            &mut ops,
            d.len().try_into().unwrap(),
        )?,
    }
    ops.push(Op::Done);

    let data = lease.as_ref().and_then(|lease| {
        if let HiffyLease::Write(d) = *lease {
            Some(d)
        } else {
            None
        }
    });
    let mut results = context.run(core, ops.as_slice(), data)?;

    if results.len() != 1 {
        bail!("unexpected results length: {:?}", results);
    }

    let mut v: Result<Vec<u8>, u32> = results.pop().unwrap();

    // If this is a Read operation, steal extra data from the returned stack
    // and copy it into the incoming HiffyLease::Read argument
    let out = match lease {
        Some(HiffyLease::Read(data)) => {
            let ok_size = hubris.typesize(op.ok)?;
            if let Ok(v) = v.as_mut() {
                let extra_data = v.drain(ok_size..).collect::<Vec<u8>>();
                data.copy_from_slice(&extra_data);
            }

            // Shoehorn that extra data in, assuming decoding worked.
            hiffy_decode(hubris, op, v)?
        }
        _ => hiffy_decode(hubris, op, v)?,
    };
    Ok(out)
}

/// Decodes a value returned from [hiffy_call] or equivalent.
///
/// Returns an outer error if decoding fails, or an inner error if the Hiffy
/// call returns an error code (formatted as a String).
pub fn hiffy_decode(
    hubris: &HubrisArchive,
    op: &idol::IdolOperation,
    val: Result<Vec<u8>, u32>,
) -> Result<std::result::Result<humility::reflect::Value, String>> {
    let r = match val {
        Ok(val) => {
            let ty = hubris.lookup_type(op.ok).unwrap();
            Ok(match op.operation.encoding {
                ::idol::syntax::Encoding::Zerocopy => {
                    humility::reflect::load_value(hubris, &val, ty, 0)?
                }
                ::idol::syntax::Encoding::Ssmarshal
                | ::idol::syntax::Encoding::Hubpack => {
                    humility::reflect::deserialize_value(hubris, &val, ty)?.0
                }
            })
        }
        Err(e) => {
            let variant = if let Some(error) = op.error {
                error.lookup_variant_by_tag(e as u64)
            } else {
                None
            };

            if let Some(variant) = variant {
                Err(variant.name.to_string())
            } else {
                Err(format!("{:x?}", e))
            }
        }
    };
    Ok(r)
}

pub fn hiffy_format_result(
    hubris: &HubrisArchive,
    result: std::result::Result<humility::reflect::Value, String>,
) -> String {
    let fmt = HubrisPrintFormat {
        newline: false,
        hex: true,
        ..HubrisPrintFormat::default()
    };
    match result {
        Ok(val) => {
            use humility::reflect::Format;
            let mut dumped = vec![];
            val.format(hubris, fmt, &mut dumped).unwrap();

            std::str::from_utf8(&dumped).unwrap().to_string()
        }
        Err(e) => {
            format!("Err({})", e)
        }
    }
}

pub fn hiffy_print_result(
    hubris: &HubrisArchive,
    op: &idol::IdolOperation,
    result: std::result::Result<humility::reflect::Value, String>,
) -> Result<()> {
    println!(
        "{}.{}() => {}",
        op.name.0,
        op.name.1,
        hiffy_format_result(hubris, result)
    );

    Ok(())
}

fn hiffy(context: &mut humility::ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let hubris = context.archive.as_ref().unwrap();

    let subargs = HiffyArgs::try_parse_from(subargs)?;

    if subargs.list {
        hiffy_list(hubris, subargs.filter)?;
        return Ok(());
    } else if !subargs.filter.is_empty() {
        bail!("filters can only be provided with --list");
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
                println!("Data: {:?}", data);
            }
        }

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
