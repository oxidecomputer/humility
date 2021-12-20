// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::{bail, Result};
use hif::*;
use humility::core::Core;
use humility::hubris::*;
use humility_cmd::hiffy::*;
use humility_cmd::{Archive, Args, Attach, Command, Validate};
use idol::syntax::{AttributedTy, Operation};
use indexmap::IndexMap;
use structopt::clap::App;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(name = "hiffy", about = "manipulate HIF execution")]
struct HiffyArgs {
    /// sets timeout
    #[structopt(
        long, short, default_value = "5000", value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// list HIF functions
    #[structopt(long, short)]
    list: bool,

    /// list interfaces
    #[structopt(long, short, conflicts_with = "list")]
    interface: bool,

    /// call a particular function
    #[structopt(long, short, conflicts_with_all = &["list", "interfaces"])]
    call: Option<String>,

    /// arguments
    #[structopt(long, short, requires = "call", use_delimiter = true)]
    arguments: Vec<String>,
}

fn hiffy_operation<'a>(
    hubris: &'a HubrisArchive,
    interface: &str,
    operation: &str,
) -> Result<(HubrisTask, &'a Operation, u16)> {
    //
    // Find this interface and its operation.
    //
    for i in 0..hubris.ntasks() {
        let task = HubrisTask::Task(i as u32);
        let module = hubris.lookup_module(task)?;

        match &module.iface {
            Some(iface) if iface.name == interface => {
                for (idx, op) in iface.ops.iter().enumerate() {
                    if op.0 == operation {
                        return Ok((task, op.1, (idx + 1) as u16));
                    }
                }

                bail!(
                    "interace {} does not contain operation {}",
                    interface,
                    operation
                );
            }
            _ => {}
        }
    }

    bail!("interface {} not found", interface);
}

fn hiffy_call_arg(
    hubris: &HubrisArchive,
    member: &HubrisStructMember,
    value: &str,
    buf: &mut [u8],
) -> Result<()> {
    let t = hubris.lookup_type(member.goff)?;

    match t {
        HubrisType::Base(base) => {
            if member.offset + base.size > buf.len() {
                bail!("illegal argument type {}", member.goff);
            }

            let dest = &mut buf[member.offset..member.offset + base.size];

            match (base.encoding, base.size) {
                (HubrisEncoding::Unsigned, 4) => {
                    let b = parse_int::parse::<u32>(value)?.to_le_bytes();
                    dest.copy_from_slice(b.as_slice());
                }
                (HubrisEncoding::Unsigned, 2) => {
                    let b = parse_int::parse::<u16>(value)?.to_le_bytes();
                    dest.copy_from_slice(b.as_slice());
                }
                (HubrisEncoding::Unsigned, 1) => {
                    dest[0] = parse_int::parse::<u8>(value)?;
                }
                (_, _) => {
                    bail!(
                        "encoding of {} ({:?}) not yet supported",
                        member.name,
                        base
                    );
                }
            }
        }
        _ => {
            bail!("type of {} ({:?}) not yet supported", member.name, t);
        }
    };

    Ok(())
}

fn hiffy_call(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
    interface: &str,
    operation: &str,
    args: &[(&str, &str)],
) -> Result<()> {
    let (task, op, code) = hiffy_operation(hubris, interface, operation)?;
    let mut map = IndexMap::new();
    let funcs = context.functions()?;
    let send = funcs.get("Send", 4)?;

    for arg in args {
        map.insert(arg.0, arg.1);
    }

    let t = format!("{}_{}_ARGS", interface, operation);
    let module = hubris.lookup_module(task)?;
    let s = module.lookup_struct_byname(hubris, &t)?;

    let mut payload = vec![0u8; s.size];

    for arg in &op.args {
        if let Some(val) = map.remove(arg.0 as &str) {
            for member in &s.members {
                if member.name == *arg.0 {
                    hiffy_call_arg(hubris, member, val, &mut payload)?;
                    break;
                }
            }
        } else {
            bail!("argument \"{}\" is not specified", arg.0);
        }
    }

    if !map.is_empty() {
        bail!(
            "spurious arguments: {}",
            map.iter().map(|v| *v.0).collect::<Vec<&str>>().join(", ")
        );
    }

    let mut ops = vec![];

    if let HubrisTask::Task(id) = task {
        ops.push(Op::Push32(id));
    } else {
        bail!("interface matches invalid task {:?}", task);
    }

    ops.push(Op::Push16(code));

    for byte in &payload {
        ops.push(Op::Push(*byte));
    }

    ops.push(Op::Push32(payload.len() as u32));
    ops.push(Op::Call(send.id));
    ops.push(Op::Done);

    let results = context.run(core, ops.as_slice(), None)?;

    println!("{:?}", results);

    Ok(())
}

fn hiffy(
    hubris: &mut HubrisArchive,
    core: &mut dyn Core,
    _args: &Args,
    subargs: &[String],
) -> Result<()> {
    let subargs = HiffyArgs::from_iter_safe(subargs)?;

    if subargs.interface {
        println!(
            "{:<15} {:<12} {:<19} {:<15} {:<15}",
            "TASK", "INTERFACE", "OPERATION", "ARG", "ARGTYPE"
        );

        let print_args = |args: &IndexMap<String, AttributedTy>, margin| {
            let mut args = args.iter();

            match args.next() {
                None => {
                    println!("-");
                }
                Some(arg) => {
                    println!("{:<15} {}", arg.0, arg.1.ty.0);

                    for arg in args {
                        println!(
                            "{:margin$}{:<15} {}",
                            "",
                            arg.0,
                            arg.1.ty.0,
                            margin = margin
                        );
                    }
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
                        print_args(&op.1.args, 49);

                        for op in ops {
                            print!("{:29}{:<20}", "", op.0);
                            print_args(&op.1.args, 49);
                        }
                    }
                }
            }
        }

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

            args.push((arg[0], arg[1]));
        }

        hiffy_call(hubris, core, &mut context, func[0], func[1], &args)?;

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
