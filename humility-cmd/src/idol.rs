// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use ::idol::syntax::{Operation, RecvStrategy, Reply};
use anyhow::{anyhow, bail, Context, Result};
use humility::hubris::*;
use indexmap::IndexMap;

#[derive(Debug)]
pub struct IdolOperation<'a> {
    pub name: (String, String),
    pub task: HubrisTask,
    pub operation: &'a Operation,
    pub code: u16,
    pub payload: Vec<u8>,
    pub ok: HubrisGoff,
    pub error: &'a HubrisEnum,
}

#[derive(Debug)]
pub enum IdolArgument<'a> {
    String(&'a str),
}

impl<'a> IdolOperation<'a> {
    pub fn new(
        hubris: &'a HubrisArchive,
        interface: &str,
        operation: &str,
        args: &[(&str, IdolArgument)],
        task: Option<&HubrisTask>,
    ) -> Result<Self> {
        let (task, op, code) = lookup(hubris, interface, operation, task)?;
        let mut map = IndexMap::new();
        let name = (interface.to_string(), operation.to_string());

        for arg in args {
            map.insert(arg.0, &arg.1);
        }

        let t = format!("{}_{}_ARGS", interface, operation);
        let module = hubris.lookup_module(task)?;
        let s = module.lookup_struct_byname(hubris, &t)?;

        let mut payload = vec![0u8; s.size];

        'nextone: for arg in &op.args {
            if let Some(val) = map.remove(arg.0 as &str) {
                if let RecvStrategy::FromBytes = arg.1.recv {
                    for member in &s.members {
                        if member.name == *arg.0 {
                            call_arg(hubris, member, val, &mut payload)?;
                            continue 'nextone;
                        }
                    }
                } else {
                    let raw = format!("raw_{}", arg.0);

                    for member in &s.members {
                        if member.name == raw {
                            let ty = &arg.1.ty.0;
                            let e = module.lookup_enum_byname(hubris, ty)?;
                            #[rustfmt::skip]
                            call_arg_enum(
                                hubris, arg.0, member, e, val, &mut payload,
                            )?;
                            continue 'nextone;
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

        let (ok, error) = lookup_reply(hubris, module, operation)?;

        //
        // We have our fully formed Idol call!
        //
        Ok(Self { name, task, operation: op, code, payload, ok, error })
    }
}

//
// Lookup an Idol operation based on the interface and operation
//
fn lookup<'a>(
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

//
// Store a call argument to the specified payload
//
fn call_arg(
    hubris: &HubrisArchive,
    member: &HubrisStructMember,
    value: &IdolArgument,
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

            let IdolArgument::String(value) = value;
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

fn call_arg_enum(
    hubris: &HubrisArchive,
    arg: &str,
    member: &HubrisStructMember,
    e: &HubrisEnum,
    value: &IdolArgument,
    buf: &mut [u8],
) -> Result<()> {
    let t = hubris.lookup_type(member.goff)?;
    let IdolArgument::String(value) = *value;

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

//
// Given a module that contains an interface, lookup the types for the reply
//
pub fn lookup_reply<'a>(
    hubris: &'a HubrisArchive,
    m: &HubrisModule,
    op: &str,
) -> Result<(HubrisGoff, &'a HubrisEnum)> {
    let iface = m.iface.as_ref().unwrap();
    let reply = &iface
        .ops
        .get(op)
        .ok_or_else(|| anyhow!("unknown operation \"{}\"", op))?
        .reply;

    match reply {
        Reply::Result { ok, err } => {
            let err = match err {
                ::idol::syntax::Error::CLike(t) => m
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
                let t = format!("{}_{}_REPLY", iface.name, op);

                if let Ok(s) = hubris.lookup_struct_byname(&t) {
                    Ok((s.goff, err))
                } else {
                    bail!("no type for {}.{}: {:?}", iface.name, op, reply);
                }
            }
        }
    }
}
