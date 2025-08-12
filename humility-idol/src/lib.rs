// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! Tools to interface with the Idol interface description language
//!
//! Along with various data types, this crate defines and implements the
//! `HubrisIdol` trait on `HubrisArchive`, which adds a `get_idol_command`
//! function to easily look up an Idol command by name.

use ::idol::syntax::{AttributedTy, Operation, RecvStrategy, Reply};
use anyhow::{anyhow, bail, Context, Result};
use hubpack::SerializedSize;
use humility::hubris::*;
use indexmap::IndexMap;
use serde::Serialize;
use std::fmt;

pub struct IdolOperation<'a> {
    hubris: &'a HubrisArchive,
    pub name: (String, String),
    pub task: HubrisTask,
    pub operation: &'a Operation,
    pub code: u16,
    pub args: Option<&'a HubrisStruct>,
    pub ok: HubrisGoff,
    pub error: IdolError<'a>,
}

#[derive(Debug)]
pub enum IdolArgument<'a> {
    String(&'a str),
    Scalar(u64),
}

impl<T> From<T> for IdolArgument<'_>
where
    T: Into<u64>,
{
    fn from(t: T) -> Self {
        Self::Scalar(t.into())
    }
}

#[derive(Debug)]
pub enum IdolError<'a> {
    CLike(&'a HubrisEnum),
    Complex(&'a HubrisEnum),
    None,
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum IpcError {
    /// The IPC returned an error.
    Error(u32),
    /// The IPC returned a dead code, indicating that the server task has died.
    ServerDied(u32),
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

    fn args_size(&self) -> usize {
        self.args.map(|s| s.size).unwrap_or(0)
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
                payload.resize(self.args_size(), 0)
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
                .map(|s| &s.members[..])
                .unwrap_or(&[])
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
        else if let Some(e) = module.lookup_enum_byname(hubris, ty)? {
            call_arg_enum(hubris, arg.0, member, e, val, payload)?;
        }
        //
        // Now look it up as a structure.  If it's a structure,
        // we will allow it if it's a newtype -- otherwise we'll
        // toss.
        //
        else if let Some(s) = module.lookup_struct_byname(hubris, ty)? {
            if s.newtype().is_some() {
                call_arg(hubris, &s.members[0], val, payload)?;
            } else {
                bail!("non-newtype structure arguments currently unsupported");
            }
        } else {
            bail!("don't know what to do with {:?}", ty);
        }
        Ok(())
    }

    pub fn strerror(&self, error: impl Into<IpcError>) -> String {
        match error.into() {
            IpcError::Error(code) => {
                let variant = if let IdolError::CLike(error) = self.error {
                    // TODO: assumes discriminant is a u8. Since this is using Hiffy
                    // call results instead of looking at a Rust value in memory, it's
                    // not clear from context what changes would be required to fix
                    // this.
                    error.lookup_variant_by_tag(Tag::from(code as u64))
                } else {
                    None
                };

                if let Some(variant) = variant {
                    variant.name.to_string()
                } else {
                    format!(
                        "<Unknown {}.{} error: {}>",
                        self.name.0, self.name.1, code
                    )
                }
            }
            IpcError::ServerDied(id) => {
                format!("<{} server died: {id}>", self.name.0)
            }
        }
    }

    pub fn reply_size(&self) -> Result<usize> {
        let reply_size = match self.operation.encoding {
            ::idol::syntax::Encoding::Zerocopy => {
                self.hubris.typesize(self.ok)?
            }
            ::idol::syntax::Encoding::Ssmarshal
            | ::idol::syntax::Encoding::Hubpack => {
                let ok = self.hubris.hubpack_serialized_maxsize(self.ok)?;
                if let IdolError::Complex(e) = self.error {
                    ok.max(self.hubris.hubpack_serialized_maxsize(e.goff)?)
                } else {
                    ok
                }
            }
        };
        Ok(reply_size)
    }
}

impl IpcError {
    const DEAD_CODE: u32 = u32::MAX << 8;
}

impl From<u32> for IpcError {
    fn from(val: u32) -> Self {
        if val & Self::DEAD_CODE == Self::DEAD_CODE {
            Self::ServerDied(val & !Self::DEAD_CODE)
        } else {
            Self::Error(val)
        }
    }
}

impl From<IpcError> for u32 {
    fn from(err: IpcError) -> Self {
        match err {
            IpcError::Error(e) => e,
            IpcError::ServerDied(id) => id | IpcError::DEAD_CODE,
        }
    }
}

impl fmt::Display for IpcError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Error(e) => {
                write!(f, "server returned error code: {e:#x?}")
            }
            Self::ServerDied(id) => {
                write!(f, "server died; its new ID is {id}")
            }
        }
    }
}

impl fmt::Debug for IpcError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IpcError::Error(e) => {
                write!(f, "IpcError::Error({e:#x?})")
            }
            IpcError::ServerDied(id) => {
                write!(f, "IpcError::ServerDied({id})")
            }
        }
    }
}

impl std::error::Error for IpcError {}

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

/// Convert array from hiffy `--argument` list string to a vector of bytes.
///
/// The humility hiffy cmd accepts multiple encodings of array arguments:
/// - When passed a string of characters like `--arguments array=5432` the
///   string is passed to the operation 'as_bytes'. An idol op that takes a
///   4 byte array will receive [ 53, 52, 51, 50 ] given the argument string
///   above. This is intended as a mechanism for passing ASCII characters to a
///   task.
/// - To pass an array that's interpreted as the decimal representation of
///   bytes instead of ASCII, provide the array as a string enclosed in square
///   brackets with each array element separated by a space. The argument string
///   `--argument array=[37 1 255 127]` will result in the task receiving the
///   byte array `[ 37, 1, 255, 127 ]`.
fn bytes_from_str(value: &str) -> Result<Vec<u8>> {
    if value.starts_with('[') && value.ends_with(']') {
        // use double ended iterator to drop first and last chars
        let mut chars = value.chars();
        chars.next();
        chars.next_back();
        let value = chars.as_str();

        let mut bytes: Vec<u8> = Vec::new();
        for element in value.split(' ') {
            let element = element.trim();
            let byte: u8 = element.parse().context(format!(
                "cannot parse \"{}\" as u8 (is it base 10?)",
                element
            ))?;
            bytes.push(byte);
        }

        Ok(bytes)
    } else {
        Ok(Vec::from(value.as_bytes()))
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
        HubrisType::Array(&HubrisArray { goff, count }) => {
            let t = hubris.lookup_type(goff)?;
            if !matches!(
                t,
                HubrisType::Base(HubrisBasetype {
                    encoding: HubrisEncoding::Unsigned,
                    size: 1,
                })
            ) {
                bail!(
                    "array type in {} ({t:?}) not yet supported",
                    member.name
                );
            }
            match value {
                IdolArgument::String(value) => {
                    let bytes = bytes_from_str(value)?;
                    if bytes.len() != count {
                        bail!(
                            "Cannot convert '{value}' to [u8; {count}]; \
                             wrong length"
                        );
                    }
                    let dest = &mut buf[member.offset..member.offset + count];
                    dest.copy_from_slice(&bytes);
                }
                IdolArgument::Scalar(v) => {
                    bail!("Cannot convert scalar {v} to [u8; {count}]")
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
        HubrisType::Array(&HubrisArray { goff, count }) => {
            let t = hubris.lookup_type(goff)?;
            if !matches!(
                t,
                HubrisType::Base(HubrisBasetype {
                    encoding: HubrisEncoding::Unsigned,
                    size: 1,
                })
            ) {
                bail!(
                    "array type in {} ({t:?}) not yet supported",
                    member.name
                );
            }
            match value {
                IdolArgument::String(value) => {
                    let bytes = bytes_from_str(value)?;
                    if bytes.len() != count {
                        bail!(
                            "Cannot convert '{value}' to [u8; {count}]; \
                             wrong length"
                        );
                    }

                    for byte in bytes {
                        hubpack_serialize_append(buf, &byte)?;
                    }

                    Ok(())
                }
                _ => {
                    bail!("Bad idol argument value");
                }
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
) -> Result<(HubrisGoff, IdolError<'a>)> {
    let iface = m.iface.as_ref().unwrap();
    let reply = &iface
        .ops
        .get(op)
        .ok_or_else(|| anyhow!("unknown operation \"{}\"", op))?
        .reply;

    let lookup_ok_err = |ok| {
        m.lookup_enum_byname(hubris, ok)?;
        m.lookup_struct_byname(hubris, ok)?;
        bail!("couldn't find type {ok} for {}.{op}", iface.name);
    };

    let lookup_ok = |ok| {
        match hubris.lookup_basetype_byname(ok) {
            Ok(goff) => Ok(*goff),
            Err(_) => match m.lookup_enum_byname(hubris, ok) {
                Ok(Some(e)) => Ok(e.goff),
                _ => match m.lookup_struct_byname(hubris, ok) {
                    Ok(Some(s)) => Ok(s.goff),
                    _ => {
                        //
                        // As a last ditch, we look up the REPLY type.  This is
                        // a last effort because it might not be there:  if no
                        // task calls the function, the type will be absent.
                        //
                        let t = format!("{}_{}_REPLY", iface.name, op);

                        match hubris.lookup_struct_byname(&t) {
                            Ok(s) => Ok(s.goff),
                            Err(_) => lookup_ok_err(ok),
                        }
                    }
                },
            },
        }
    };

    match reply {
        Reply::Result { ok, err } => {
            let err = match err {
                ::idol::syntax::Error::CLike(t) => {
                    let t = m.lookup_enum_byname(hubris, &t.0)?.ok_or_else(
                        || anyhow!("failed to find error type {reply:?}"),
                    )?;

                    IdolError::CLike(t)
                }
                ::idol::syntax::Error::ServerDeath => IdolError::None,
                ::idol::syntax::Error::Complex(t) => {
                    let t = m.lookup_enum_byname(hubris, &t.0)?.ok_or_else(
                        || anyhow!("failed to find error type {reply:?}"),
                    )?;
                    IdolError::Complex(t)
                }
            };
            Ok((lookup_ok(&ok.ty.0)?, err))
        }
        Reply::Simple(ok) => Ok((lookup_ok(&ok.ty.0)?, IdolError::None)),
    }
}

/// Extention trait to add `get_idol_command` to a `HubrisArchive`
pub trait HubrisIdol {
    fn get_idol_command(&self, name: &str) -> Result<IdolOperation<'_>>;
}

impl HubrisIdol for HubrisArchive {
    fn get_idol_command(&self, name: &str) -> Result<IdolOperation<'_>> {
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
