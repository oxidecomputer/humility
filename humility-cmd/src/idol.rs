// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use ::idol::syntax::{Operation, RecvStrategy, Reply};
use anyhow::{anyhow, bail, Context, Result};
use humility::hubris::*;
use indexmap::IndexMap;

#[derive(Debug)]
pub struct IdolOperation<'a> {
    hubris: &'a HubrisArchive,
    pub name: (String, String),
    pub task: HubrisTask,
    pub operation: &'a Operation,
    pub code: u16,
    pub args: &'a HubrisStruct,
    pub ok: HubrisGoff,
    pub error: Option<&'a HubrisEnum>,
}

#[derive(Debug)]
pub enum IdolArgument<'a> {
    String(&'a str),
    Scalar(u64),
}

impl<'a> IdolOperation<'a> {
    pub fn new(
        hubris: &'a HubrisArchive,
        interface: &str,
        operation: &str,
        task: Option<&HubrisTask>,
    ) -> Result<Self> {
        let (task, op, code) = lookup(hubris, interface, operation, task)?;
        let name = (interface.to_string(), operation.to_string());

        let t = format!("{}_{}_ARGS", interface, operation);
        let module = hubris.lookup_module(task)?;
        let args = module.lookup_struct_byname(hubris, &t)?;
        let (ok, error) = lookup_reply(hubris, module, operation)?;

        //
        // We have our fully formed Idol call!
        //
        Ok(Self { hubris, name, task, operation: op, code, args, ok, error })
    }

    pub fn payload(&self, args: &[(&str, IdolArgument)]) -> Result<Vec<u8>> {
        let hubris = self.hubris;
        let module = hubris.lookup_module(self.task)?;

        let mut map: IndexMap<_, _> =
            args.iter().map(|arg| (arg.0, &arg.1)).collect();

        let mut payload = vec![0u8; self.args.size];

        for arg in &self.operation.args {
            let val = map.remove(arg.0 as &str).ok_or_else(|| {
                anyhow!("argument \"{}\" is not specified", arg.0)
            })?;

            // Find the expected name of the argument in the struct, based
            // on its encoding strategy (with a special case for `bool`, which
            // is packed into a single `u8`).
            let ty = &arg.1.ty.0;
            let arg_name = if matches!(arg.1.recv, RecvStrategy::FromBytes)
                && ty != "bool"
            {
                arg.0.to_owned()
            } else {
                format!("raw_{}", arg.0)
            };

            let member = self
                .args
                .members
                .iter()
                .find(|&m| m.name == arg_name)
                .ok_or_else(|| {
                    anyhow!("did not find {} in {:?}", arg.0, self.args)
                })?;

            // Now, we have to decide how to pack the argument into the payload
            //
            // The easiest option is if we're doing `FromBytes`, which encodes
            // the value directly (with a special case for booleans).
            if matches!(arg.1.recv, RecvStrategy::FromBytes) {
                if ty != "bool" {
                    call_arg(hubris, member, val, &mut payload)?;
                } else {
                    let v = IdolArgument::String(match val {
                        IdolArgument::String("true") => "1",
                        IdolArgument::String("false") => "0",
                        _ => bail!("Invalid bool argument {:?}", val),
                    });
                    call_arg(hubris, member, &v, &mut payload)?;
                }
            }
            //
            // We have a raw type, so we need to figure out how
            // to encode it.  First, see if we can look up the
            // AttributedType as an enum...
            //
            else if let Ok(e) = module.lookup_enum_byname(hubris, ty) {
                call_arg_enum(hubris, arg.0, member, e, val, &mut payload)?;
            }
            //
            // Now look it up as a structure.  If it's a structure,
            // we will allow it if it's a newtype -- otherwise we'll
            // toss.
            //
            else if let Ok(s) = module.lookup_struct_byname(hubris, ty) {
                if s.newtype().is_some() {
                    call_arg(hubris, &s.members[0], val, &mut payload)?;
                } else {
                    bail!("structure arguments currently unsupported");
                }
            } else {
                bail!("don't know what to do with {:?}", self.args);
            }
        }

        if !map.is_empty() {
            bail!(
                "spurious arguments: {}",
                map.iter().map(|v| *v.0).collect::<Vec<&str>>().join(", ")
            );
        }

        Ok(payload)
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

    if let IdolArgument::Scalar(value) = value {
        let base = match t {
            HubrisType::Base(base) => base,
            _ => {
                bail!("scalar for {} ({:?}) not yet supported", member.name, t);
            }
        };

        if member.offset + base.size > buf.len() {
            bail!("illegal argument type {}", member.goff);
        }

        let dest = &mut buf[member.offset..member.offset + base.size];

        if base.encoding != HubrisEncoding::Unsigned {
            bail!("signed scalar arguments not yet supported");
        }

        let mut v = *value;

        // A little dirty
        for d in dest.iter_mut() {
            *d = (v & 0xff) as u8;
            v >>= 8;
        }

        if v != 0 {
            bail!("value of {} exceeds maximum for {}", value, member.name);
        }

        return Ok(());
    }

    match t {
        HubrisType::Base(base) => {
            if member.offset + base.size > buf.len() {
                bail!("illegal argument type {}", member.goff);
            }

            let value = match value {
                IdolArgument::String(value) => value,
                _ => {
                    bail!("unrecognized argument type: {:?}", value);
                }
            };

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
    let t = hubris
        .lookup_type(member.goff)
        .context(format!("expected type for arg {} ({})", arg, member.goff))?;

    let value = match value {
        IdolArgument::String(value) => *value,
        _ => {
            bail!("invalid value for arg {} {:?}", arg, value);
        }
    };

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
) -> Result<(HubrisGoff, Option<&'a HubrisEnum>)> {
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
                Ok((*goff, Some(err)))
            } else if let Ok(e) = m.lookup_enum_byname(hubris, &ok.ty.0) {
                Ok((e.goff, Some(err)))
            } else if let Ok(s) = m.lookup_struct_byname(hubris, &ok.ty.0) {
                Ok((s.goff, Some(err)))
            } else {
                //
                // As a last ditch, we look up the REPLY type. This is a last
                // effort because it might not be there:  if no task calls
                // the function, the type will be absent.
                //
                let t = format!("{}_{}_REPLY", iface.name, op);

                if let Ok(s) = hubris.lookup_struct_byname(&t) {
                    Ok((s.goff, Some(err)))
                } else {
                    bail!("no type for {}.{}: {:?}", iface.name, op, reply);
                }
            }
        }
        Reply::Simple(ok) => {
            if let Ok(goff) = hubris.lookup_basetype_byname(&ok.ty.0) {
                Ok((*goff, None))
            } else if let Ok(e) = m.lookup_enum_byname(hubris, &ok.ty.0) {
                Ok((e.goff, None))
            } else if let Ok(s) = m.lookup_struct_byname(hubris, &ok.ty.0) {
                Ok((s.goff, None))
            } else {
                //
                // As a last ditch, we look up the REPLY type. This is a last
                // effort because it might not be there:  if no task calls
                // the function, the type will be absent.
                //
                let t = format!("{}_{}_REPLY", iface.name, op);

                if let Ok(s) = hubris.lookup_struct_byname(&t) {
                    Ok((s.goff, None))
                } else {
                    bail!("no type for {}.{}: {:?}", iface.name, op, reply);
                }
            }
        }
    }
}
