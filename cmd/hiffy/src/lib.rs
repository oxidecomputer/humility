// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::{anyhow, bail, Context, Result};
use hif::*;
use humility::core::Core;
use humility::hubris::*;
use humility_cmd::hiffy::*;
use humility_cmd::{Archive, Args, Attach, Command, Validate};
use idol::syntax::{Operation, RecvStrategy, Reply};
use indexmap::IndexMap;
use structopt::clap::App;
use structopt::StructOpt;

#[macro_use]
extern crate log;

#[derive(StructOpt, Debug)]
#[structopt(name = "hiffy", about = "manipulate HIF execution")]
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
    interface: bool,

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

fn hiffy_operation<'a>(
    hubris: &'a HubrisArchive,
    interface: &str,
    operation: &str,
    target: Option<&HubrisTask>,
) -> Result<(HubrisTask, &'a Operation, u16)> {
    //
    // Find this interface and its operation.
    //
    let mut rval = None;

    let tasks = match target {
        Some(task) => vec![*task],
        None => {
            (0..hubris.ntasks()).map(|t| HubrisTask::Task(t as u32)).collect()
        }
    };

    'nexttask: for task in tasks {
        let module = hubris.lookup_module(task)?;

        match &module.iface {
            Some(iface) if iface.name == interface => {
                for (idx, op) in iface.ops.iter().enumerate() {
                    if op.0 == operation {
                        rval = match rval {
                            None => Some((task, op.1, (idx + 1) as u16)),
                            Some((_, _, _)) => {
                                bail!(
                                    "interface {} is present in more than \
                                    one task",
                                    interface
                                );
                            }
                        };
                        continue 'nexttask;
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

    if let Some(rval) = rval {
        Ok(rval)
    } else {
        bail!("interface {} not found", interface);
    }
}

fn hiffy_call_arg(
    hubris: &HubrisArchive,
    member: &HubrisStructMember,
    value: &str,
    buf: &mut [u8],
) -> Result<()> {
    let t = hubris.lookup_type(member.goff)?;
    let arg = &member.name;

    let err = |err| anyhow!("illegal value for {}: {}", arg, err);

    match t {
        HubrisType::Base(base) => {
            if member.offset + base.size > buf.len() {
                bail!("illegal argument type {}", member.goff);
            }

            let dest = &mut buf[member.offset..member.offset + base.size];

            match (base.encoding, base.size) {
                (HubrisEncoding::Unsigned, 4) => {
                    let v = parse_int::parse::<u32>(value).map_err(err)?;
                    dest.copy_from_slice(v.to_le_bytes().as_slice());
                }
                (HubrisEncoding::Unsigned, 2) => {
                    let v = parse_int::parse::<u16>(value).map_err(err)?;
                    dest.copy_from_slice(v.to_le_bytes().as_slice());
                }
                (HubrisEncoding::Unsigned, 1) => {
                    dest[0] = parse_int::parse::<u8>(value).map_err(err)?;
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

fn hiffy_call_reply<'a>(
    hubris: &'a HubrisArchive,
    m: &HubrisModule,
    interface: &str,
    op: &str,
    reply: &Reply,
) -> Result<(HubrisGoff, &'a HubrisEnum)> {
    match reply {
        Reply::Result { ok, err } => {
            let err = match err {
                idol::syntax::Error::CLike(t) => m
                    .lookup_enum_byname(hubris, &t.0)
                    .context(format!("failed to find error type {:?}", reply)),
            }?;

            if let Ok(goff) = hubris.lookup_basetype_byname(&ok.ty.0) {
                Ok((*goff, err))
            } else if let Ok(e) = m.lookup_enum_byname(hubris, &ok.ty.0) {
                Ok((e.goff, err))
            } else if let Ok(s) = m.lookup_struct_byname(hubris, &ok.ty.0) {
                Ok((s.goff, err))
            } else {
                //
                // As a last ditch, we look up the REPLY type. This is a last
                // effort because it might not be there:  if no task calls
                // the function, the type will be absent.
                //
                let t = format!("{}_{}_REPLY", interface, op);

                if let Ok(s) = hubris.lookup_struct_byname(&t) {
                    Ok((s.goff, err))
                } else {
                    bail!("no type for {}.{}: {:?}", interface, op, reply);
                }
            }
        }
    }
}

fn hiffy_call_arg_enum(
    hubris: &HubrisArchive,
    arg: &str,
    member: &HubrisStructMember,
    e: &HubrisEnum,
    value: &str,
    buf: &mut [u8],
) -> Result<()> {
    let t = hubris.lookup_type(member.goff)?;

    let base = match t {
        HubrisType::Base(base) => base,
        _ => {
            bail!("type {:?} not expected to represent enum", t);
        }
    };

    if base.size != e.size {
        bail!("size mismatch: {:?} vs. enum arg {:?}", t, e);
    }

    let dest = &mut buf[member.offset..member.offset + e.size];

    for variant in &e.variants {
        if value == variant.name {
            if let Some(tag) = variant.tag {
                match e.size {
                    4 => {
                        let v: u32 = tag.try_into()?;
                        dest.copy_from_slice(v.to_le_bytes().as_slice());
                    }
                    2 => {
                        let v: u16 = tag.try_into()?;
                        dest.copy_from_slice(v.to_le_bytes().as_slice());
                    }
                    1 => {
                        let v: u8 = tag.try_into()?;
                        dest[0] = v;
                    }
                    _ => {
                        bail!("unknown enum size: {:?}", e);
                    }
                }

                return Ok(());
            } else {
                bail!("untagged enum: {:?}", e);
            }
        }
    }

    let all =
        e.variants.iter().map(|v| v.name.clone()).collect::<Vec<String>>();
    bail!("{} must be one of: {}", arg, all.join(", "));
}

fn hiffy_call(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
    interface: &str,
    operation: &str,
    args: &[(&str, &str)],
    task: Option<&HubrisTask>,
) -> Result<()> {
    let (task, op, code) = hiffy_operation(hubris, interface, operation, task)?;
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

    'nextone: for arg in &op.args {
        if let Some(val) = map.remove(arg.0 as &str) {
            match &arg.1.recv {
                RecvStrategy::FromBytes => {
                    for member in &s.members {
                        if member.name == *arg.0 {
                            hiffy_call_arg(hubris, member, val, &mut payload)?;
                            continue 'nextone;
                        }
                    }
                }

                _ => {
                    let raw = format!("raw_{}", arg.0);

                    for member in &s.members {
                        if member.name == raw {
                            let ty = &arg.1.ty.0;
                            let e = module.lookup_enum_byname(hubris, ty)?;
                            hiffy_call_arg_enum(
                                hubris,
                                arg.0,
                                member,
                                e,
                                val,
                                &mut payload,
                            )?;
                            continue 'nextone;
                        }
                    }
                }
            }

            bail!("did not find {} in {:?}", arg.0, s);
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

    let reply = &op.reply;

    let (ok, _) =
        hiffy_call_reply(hubris, module, interface, operation, reply)?;

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
    ops.push(Op::Push32(hubris.typesize(ok)? as u32));
    ops.push(Op::Call(send.id));
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
            let dumped = hubris.printfmt(val, ok, &fmt)?;
            println!("{}.{}() = {}", interface, operation, dumped);
        }
        Err(e) => {
            println!("Err({:x?})", e);
        }
    }

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

        let print_args = |iface: &idol::syntax::Interface,
                          op: &(&String, &Operation),
                          module,
                          margin| {
            let mut args = op.1.args.iter();

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

            if !subargs.verbose {
                return;
            }

            let r = hiffy_call_reply(
                hubris,
                module,
                &iface.name,
                op.0,
                &op.1.reply,
            );

            match r {
                Ok((_, e)) => match &op.1.reply {
                    Reply::Result { ok, .. } => {
                        println!(
                            "{:m$}{:<15} {}",
                            "",
                            "<ok>",
                            ok.ty.0,
                            m = margin
                        );
                        println!(
                            "{:m$}{:<15} {}",
                            "",
                            "<error>",
                            e.name,
                            m = margin
                        );
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
                        print_args(iface, &op, module, 49);

                        for op in ops {
                            print!("{:29}{:<20}", "", op.0);
                            print_args(iface, &op, module, 49);
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

        let task = match subargs.task {
            Some(task) => Some(
                hubris
                    .lookup_task(&task)
                    .ok_or_else(|| anyhow!("unknown task \"{}\"", task))?,
            ),
            None => None,
        };

        hiffy_call(hubris, core, &mut context, func[0], func[1], &args, task)?;

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
