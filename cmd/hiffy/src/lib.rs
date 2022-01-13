// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use ::idol::syntax::{Operation, Reply};
use anyhow::{anyhow, bail, Result};
use hif::*;
use humility::core::Core;
use humility::hubris::*;
use humility_cmd::hiffy::*;
use humility_cmd::idol;
use humility_cmd::{Archive, Args, Attach, Command, Validate};
use structopt::clap::App;
use structopt::StructOpt;

#[macro_use]
extern crate log;

#[derive(StructOpt, Debug)]
#[structopt(name = "hiffy", about = env!("CARGO_PKG_DESCRIPTION"))]
struct HiffyArgs {
    /// sets timeout
    #[structopt(
        long, short = "T", default_value = "5000", value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// verbose
    #[structopt(long, short)]
    verbose: bool,

    /// list HIF functions
    #[structopt(long, short)]
    list: bool,

    /// list interfaces
    #[structopt(long, short, conflicts_with = "list")]
    interfaces: bool,

    /// call a particular function
    #[structopt(long, short, conflicts_with_all = &["list", "interfaces"])]
    call: Option<String>,

    /// arguments
    #[structopt(long, short, requires = "call")]
    task: Option<String>,

    /// arguments
    #[structopt(long, short, requires = "call", use_delimiter = true)]
    arguments: Vec<String>,
}

fn hiffy_interfaces(hubris: &HubrisArchive, subargs: &HiffyArgs) -> Result<()> {
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
                warn!("{}", e);
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
            let dumped = hubris.printfmt(val, op.ok, &fmt)?;
            println!("{}.{}() = {}", op.name.0, op.name.1, dumped);
        }
        Err(e) => {
            println!("Err({:x?})", e);
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
    let subargs = HiffyArgs::from_iter_safe(subargs)?;

    if subargs.interfaces {
        hiffy_interfaces(hubris, &subargs)?;
        return Ok(());
    }

    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;

    if let Some(call) = subargs.call {
        let func: Vec<&str> = call.split('.').collect();

        if func.len() != 2 {
            bail!("calls must be interface.operation (-i to list)");
        }

        let mut args = vec![];

        for arg in &subargs.arguments {
            let arg: Vec<&str> = arg.split('=').collect();

            if arg.len() != 2 {
                bail!("arguments must be argument=value (-i to list)");
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

    if !subargs.list {
        bail!("expected -l");
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

pub fn init<'a, 'b>() -> (Command, App<'a, 'b>) {
    (
        Command::Attached {
            name: "hiffy",
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
            run: hiffy,
        },
        HiffyArgs::clap(),
    )
}
