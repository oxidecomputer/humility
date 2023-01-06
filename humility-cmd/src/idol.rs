// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use ::idol::syntax::{AttributedTy, Operation, RecvStrategy, Reply};
use anyhow::{anyhow, bail, Context, Result};
use hubpack::SerializedSize;
use humility::hubris::*;
use indexmap::IndexMap;
use serde::Serialize;

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

        let mut payload = Vec::new();
        match self.operation.encoding {
            // Zerocopy will populate payload based on its actual size, so
            // resize it appropriately.
            ::idol::syntax::Encoding::Zerocopy => {
                payload.resize(self.args.size, 0)
            }
            // Serializing args will append to `payload`, so leave it empty (for
            // now).
            ::idol::syntax::Encoding::Ssmarshal
            | ::idol::syntax::Encoding::Hubpack => (),
        }

        for arg in &self.operation.args {
            let val = map.remove(arg.0 as &str).ok_or_else(|| {
                anyhow!("argument \"{}\" is not specified", arg.0)
            })?;

            // Find the expected name of the argument in the struct, based
            // on its encoding strategy (with a special case for `bool`, which
            // is packed into a single `u8`).
            let ty = &arg.1.ty.0;
            let arg_name = match arg.1.recv {
                RecvStrategy::FromBytes if ty != "bool" => arg.0.to_string(),
                _ => format!("raw_{}", arg.0),
            };

            let member = self
                .args
                .members
                .iter()
                .find(|&m| m.name == arg_name)
                .ok_or_else(|| {
                    anyhow!("did not find {} in {:?}", arg.0, self.args)
                })?;

            match self.operation.encoding {
                ::idol::syntax::Encoding::Zerocopy => self
                    .payload_arg_zerocopy(
                        hubris,
                        module,
                        member,
                        arg,
                        val,
                        &mut payload,
                    )?,
                ::idol::syntax::Encoding::Ssmarshal
                | ::idol::syntax::Encoding::Hubpack => {
                    serialize_arg(hubris, member, val, &mut payload)?
                }
            };
        }

        if !map.is_empty() {
            bail!(
                "spurious arguments: {}",
                map.iter().map(|v| *v.0).collect::<Vec<&str>>().join(", ")
            );
        }

        Ok(payload)
    }

    fn payload_arg_zerocopy(
        &self,
        hubris: &HubrisArchive,
        module: &HubrisModule,
        member: &HubrisStructMember,
        arg: (&String, &AttributedTy),
        val: &IdolArgument,
        payload: &mut [u8],
    ) -> Result<()> {
        // Now, we have to decide how to pack the argument into the payload
        //
        // The easiest option is if we're doing `FromBytes`, which encodes
        // the value directly (with a special case for booleans).
        let ty = &arg.1.ty.0;
        if matches!(arg.1.recv, RecvStrategy::FromBytes) {
            if ty != "bool" {
                call_arg(hubris, member, val, payload)?;
            } else {
                let v = IdolArgument::String(match val {
                    IdolArgument::String("true") => "1",
                    IdolArgument::String("false") => "0",
                    _ => bail!("Invalid bool argument {:?}", val),
                });
                call_arg(hubris, member, &v, payload)?;
            }
        }
        //
        // We have a raw type, so we need to figure out how
        // to encode it.  First, see if we can look up the
        // AttributedType as an enum...
        //
        else if let Ok(e) = module.lookup_enum_byname(hubris, ty) {
            call_arg_enum(hubris, arg.0, member, e, val, payload)?;
        }
        //
        // Now look it up as a structure.  If it's a structure,
        // we will allow it if it's a newtype -- otherwise we'll
        // toss.
        //
        else if let Ok(s) = module.lookup_struct_byname(hubris, ty) {
            if s.newtype().is_some() {
                call_arg(hubris, &s.members[0], val, payload)?;
            } else {
                bail!("non-newtype structure arguments currently unsupported");
            }
        } else {
            bail!("don't know what to do with {:?}", self.args);
        }
        Ok(())
    }

    pub fn strerror(&self, code: u32) -> String {
        let variant = if let Some(error) = self.error {
            error.lookup_variant_by_tag(code as u64)
        } else {
            None
        };

        if let Some(variant) = variant {
            variant.name.to_string()
        } else {
            format!("<Unknown {}.{} error: {}>", self.name.0, self.name.1, code)
        }
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

            let err = |err: &dyn std::fmt::Display| {
                anyhow!("illegal value for {}: {}", arg, err)
            };

            match (base.encoding, base.size) {
                (HubrisEncoding::Unsigned, 8) => {
                    let v: u64 =
                        parse_int::parse(value).map_err(|e| err(&e))?;
                    dest.copy_from_slice(v.to_le_bytes().as_slice());
                }
                (HubrisEncoding::Unsigned, 4) => {
                    let v: u32 =
                        parse_int::parse(value).map_err(|e| err(&e))?;
                    dest.copy_from_slice(v.to_le_bytes().as_slice());
                }
                (HubrisEncoding::Unsigned, 2) => {
                    let v: u16 =
                        parse_int::parse(value).map_err(|e| err(&e))?;
                    dest.copy_from_slice(v.to_le_bytes().as_slice());
                }
                (HubrisEncoding::Unsigned, 1) => {
                    dest[0] = parse_int::parse(value).map_err(|e| err(&e))?;
                }
                (HubrisEncoding::Float, 4) => {
                    let v: f32 =
                        parse_int::parse(value).map_err(|e| err(&e))?;
                    dest.copy_from_slice(v.to_le_bytes().as_slice());
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

// Grow `buf` just enough to serialize `value` onto the end of it.
fn hubpack_serialize_append<T: Serialize + SerializedSize>(
    buf: &mut Vec<u8>,
    value: &T,
) -> hubpack::error::Result<()> {
    let old_size = buf.len();

    // Grow buf sufficiently for any possible T...
    buf.resize(old_size + T::MAX_SIZE, 0);
    let n = hubpack::serialize(&mut buf[old_size..], value)?;
    assert!(n <= T::MAX_SIZE);

    // ... then shrink it back if `value`'s serialized size is less than the max
    buf.truncate(old_size + n);

    Ok(())
}

// WARNING: This method assumes the argument type (`member`) derived its
// `serde::Serialize` implementation! If it has a custom implementation, our
// assumptions about how ssmarshal/hubpack encode data may be wrong.
fn serialize_arg(
    hubris: &HubrisArchive,
    member: &HubrisStructMember,
    value: &IdolArgument,
    buf: &mut Vec<u8>,
) -> Result<()> {
    let t = hubris.lookup_type(member.goff)?;
    let arg = &member.name;

    match t {
        HubrisType::Base(base) => {
            let err = |err| anyhow!("illegal value for {}: {}", arg, err);

            // If someone passed us a scalar, too bad - we're going to convert
            // it into a string then immediately reparse it.
            let value = match value {
                IdolArgument::String(value) => value.to_string(),
                IdolArgument::Scalar(value) => format!("{}", value),
            };

            match (base.encoding, base.size) {
                (HubrisEncoding::Unsigned, 8) => {
                    let v = parse_int::parse::<u64>(&value).map_err(err)?;
                    Ok(hubpack_serialize_append(buf, &v)?)
                }
                (HubrisEncoding::Unsigned, 4) => {
                    let v = parse_int::parse::<u32>(&value).map_err(err)?;
                    Ok(hubpack_serialize_append(buf, &v)?)
                }
                (HubrisEncoding::Unsigned, 2) => {
                    let v = parse_int::parse::<u16>(&value).map_err(err)?;
                    Ok(hubpack_serialize_append(buf, &v)?)
                }
                (HubrisEncoding::Unsigned, 1) => {
                    // Allow "true" or "false" to map to 1/0
                    let v = match value.as_str() {
                        "true" | "True" | "TRUE" => 1,
                        "false" | "False" | "FALSE" => 0,
                        s => parse_int::parse::<u8>(s).map_err(err)?,
                    };
                    Ok(hubpack_serialize_append(buf, &v)?)
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
        HubrisType::Enum(e) => {
            let value = match value {
                IdolArgument::String(s) => *s,
                IdolArgument::Scalar(_) => {
                    bail!("expected a variant name for {arg}")
                }
            };

            // Find the variant and its index.
            let (index, variant) = e
                .variants
                .iter()
                .enumerate()
                .find(|(_i, variant)| variant.name == value)
                .ok_or_else(|| {
                    let all_variants = e
                        .variants
                        .iter()
                        .map(|v| v.name.as_str())
                        .collect::<Vec<_>>();
                    anyhow!(
                        "illegal value for {arg}: must be one of {}",
                        all_variants.join(", ")
                    )
                })?;

            // Ensure `variant` has no associated data. We assume if
            // `variant.goff` is `None`, the variant has no associated data.
            if let Some(variant_goff) = variant.goff {
                let variant_t =
                    hubris.lookup_type(variant_goff).with_context(|| {
                        format!(
                            "failed to lookup type info for variant {}",
                            variant.name
                        )
                    })?;
                match variant_t {
                    HubrisType::Struct(s) => {
                        if !s.members.is_empty() {
                            bail!(
                                "cannot encode {}: contains associated data",
                                variant.name
                            );
                        }
                    }
                    HubrisType::Base(_)
                    | HubrisType::Enum(_)
                    | HubrisType::Array(_)
                    | HubrisType::Union(_)
                    | HubrisType::Ptr(_) => {
                        bail!(
                            "unexpected type for enum variant {}",
                            variant.name
                        )
                    }
                }
            }

            // We noted this above, but it's worth reiterating: We now assume
            // that `arg` derived its `Serialize` impl! We're going to encode
            // the variant as a u8 tag, which is what hubpack and ssmarshal do
            // by default, but if this type has a custom Serialize impl this
            // will be incorrect!
            let tag = u8::try_from(index).map_err(|_| {
                anyhow!(
                    "cannot encode {}: index {index} does not fit in a u8",
                    variant.name,
                )
            })?;

            Ok(hubpack_serialize_append(buf, &tag)?)
        }
        HubrisType::Struct(s) => {
            if s.newtype().is_some() {
                serialize_arg(hubris, &s.members[0], value, buf)
            } else {
                bail!("non-newtype structure arguments currently unsupported");
            }
        }
        _ => {
            bail!("type of {} ({:?}) not yet supported", member.name, t);
        }
    }
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

    let lookup_ok =
        |ok| {
            match hubris.lookup_basetype_byname(ok) {
                Ok(goff) => Ok(*goff),
                Err(basetype_err) => match m.lookup_enum_byname(hubris, ok) {
                    Ok(e) => Ok(e.goff),
                    Err(enum_err) => match m.lookup_struct_byname(hubris, ok) {
                        Ok(s) => Ok(s.goff),
                        Err(struct_err) => {
                            //
                            // As a last ditch, we look up the REPLY type.
                            // This is a last effort because it might not be
                            // there:  if no task calls the function, the type
                            // will be absent.
                            //
                            let t = format!("{}_{}_REPLY", iface.name, op);

                            match hubris.lookup_struct_byname(&t) {
                                Ok(s) => Ok(s.goff),
                                Err(reply_err) => {
                                    //
                                    // If all of that has failed, we want to
                                    // generate an error message that contains
                                    // all of the failures we encountered:
                                    // this is (much) more likely to be due to
                                    // Humility limitations rather than
                                    // anything else.
                                    //
                                    bail!(
                                        "no type for {}.{}: {:?} (as basetype: \
                                        \"{:?}\"; as enum: \"{:?}\"; as \
                                        struct: \"{:?}\"; as REPLY: \"{:?}\")",
                                        iface.name, op, reply, basetype_err,
                                        enum_err, struct_err, reply_err
                                    );
                                }
                            }
                        }
                    },
                },
            }
        };

    match reply {
        Reply::Result { ok, err } => match err {
            ::idol::syntax::Error::CLike(t) => {
                let err = m.lookup_enum_byname(hubris, &t.0).context(
                    format!("failed to find error type {:?}", reply),
                )?;
                Ok((lookup_ok(&ok.ty.0)?, Some(err)))
            }
            ::idol::syntax::Error::ServerDeath => {
                Ok((lookup_ok(&ok.ty.0)?, None))
            }
        },
        Reply::Simple(ok) => Ok((lookup_ok(&ok.ty.0)?, None)),
    }
}

/// Extention trait to add `get_idol_command` to a `HubrisArchive`
pub trait HubrisIdol {
    fn get_idol_command(&self, name: &str) -> Result<IdolOperation>;
}

impl HubrisIdol for HubrisArchive {
    fn get_idol_command(&self, name: &str) -> Result<IdolOperation> {
        let mut iter = name.split('.');
        let interface = iter
            .next()
            .ok_or_else(|| anyhow!("Interface name cannot be empty"))?;
        let op = iter
            .next()
            .ok_or_else(|| anyhow!("Operation name cannot be empty"))?;
        IdolOperation::new(self, interface, op, None).with_context(|| {
            format!(
                "Could not find `{interface}.{op}`; \
                 the task may be missing or your Hubris archive \
                 may be out of date",
            )
        })
    }
}
