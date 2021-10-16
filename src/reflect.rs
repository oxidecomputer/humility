//! An API for reflecting over Rust programs through debug interfaces.
//!
//! # Common case
//!
//! Imagine that a program under debug contains a value, at an address `a`, that
//! is of a struct type that the program calls "Thingy". You know the struct
//! contains at least two members:
//!
//! - `foo`, a `u32`, and
//! - `bar`, a `bool`.
//!
//! This module lets you express that knowledge by writing
//!
//! ```ignore
//! #[derive(Load)]
//! struct Thingy {
//!     foo: u32,
//!     bar: bool,
//! }
//! ```
//!
//! and then extract the contents from the program by writing
//!
//! ```ignore
//! let hubris: &HubrisArchive = ...;
//! let image: &[u8] = ...;
//!
//! let ty = hubris.lookup_struct_byname("Thingy")?;
//! let result: Thingy = reflect::load(hubris, buf, ty, addr)?;
//! ```
//!
//! # Details
//!
//! This module consists of two parts, `Value` and `Load`.
//!
//! `Value` is like a JSON model for Rust data. You can extract values from a
//! program image using `load_value` and walk over them, inspecting composition
//! and types and whatnot. For more info, start at `Value` and walk the types.
//!
//! `Load` is a trait for types that can create themselves from a `Value`. This
//! maps the abstract dynamic `Value` information onto a Rust struct, enum, or
//! whatever. `Load` can be `derive`d.
//!
//! The in-memory representation of a `Load` type (such as a `struct`) has
//! _nothing to do_ with the representation in the program under debug. We use
//! the DWARF info to map the types dynamically.
//!
//! # Pointers
//!
//! This module does not auto-traverse pointers, because it's awfully hard to
//! tell which pointers are known to be valid. Instead, you get a `Ptr`.
//!
//! `Ptr` captures the type from the DWARF info, so if you want to try
//! indirecting it, you can use `ptr.load_from`. Otherwise, you have an address
//! that you can print in hex or embroider on a jacket.
//!
//! # Compatibility
//!
//! The derived `Load` impls do not require _exact_ type match. In particular,
//!
//! - A struct will match even if it contains more fields than your Rust
//! version, and
//! - An enum will match even if you define variants that don't exist in the
//! program.
//!
//! This is deliberate, and is intended to help interpret programs across ABI
//! changes. If you implement a `Load` impl by hand, you should try to emulate
//! this.
//!
//! By writing the impl by hand, you can go a step farther. Say a type was
//! represented in one way in older versions, but has changed representation in
//! newer ones. You can write a `Load` impl that matches on `Value` and handles
//! either. There's an example of this in `doppel`.

use std::collections::BTreeMap;
use std::convert::TryInto;

use anyhow::{anyhow, bail, Result};

use crate::core::Core;
use crate::hubris::{
    HubrisArchive, HubrisArray, HubrisBasetype, HubrisEnum, HubrisGoff,
    HubrisStruct, HubrisType,
};

/// A dynamic representation of some data extracted from a program image.
///
/// Each variant of `Value` captures a different class of types, and thus
/// representations.
#[derive(Clone, Debug)]
pub enum Value {
    /// A basetype (primitive).
    Base(Base),
    /// An enumeration.
    Enum(Enum),
    /// A struct.
    Struct(Struct),
    /// A tuple, or tuple struct.
    Tuple(Tuple),
    /// An array.
    Array(Array),
    /// A pointer of some kind.
    Ptr(Ptr),
}

impl Value {
    /// "Down-casts" this to a `Struct`, returning an error if it isn't one.
    pub fn as_struct(&self) -> Result<&Struct> {
        if let Self::Struct(s) = self {
            Ok(s)
        } else {
            bail!("not a struct: {:?}", self)
        }
    }

    /// Interprets this as a 1-tuple (e.g. `(x,)`) and extracts its sole value,
    /// returning an error if it isn't a 1-tuple.
    pub fn as_1tuple(&self) -> Result<&Value> {
        let t = self.as_tuple()?;
        if t.len() == 1 {
            Ok(&t[0])
        } else {
            bail!("tuple had more than 1 element: {:?}", self);
        }
    }

    /// "Down-casts" this to a `Tuple`, returning an error if it isn't one.
    pub fn as_tuple(&self) -> Result<&Tuple> {
        if let Self::Tuple(s) = self {
            Ok(s)
        } else {
            bail!("not a tuple: {:?}", self)
        }
    }

    /// "Down-casts" this to an `Enum`, returning an error if it isn't one.
    pub fn as_enum(&self) -> Result<&Enum> {
        if let Self::Enum(s) = self {
            Ok(s)
        } else {
            bail!("not an enum: {:?}", self)
        }
    }

    /// "Down-casts" this to a `Base`, returning an error if it isn't one.
    pub fn as_base(&self) -> Result<&Base> {
        if let Self::Base(s) = self {
            Ok(s)
        } else {
            bail!("not a base type: {:?}", self)
        }
    }

    /// "Down-casts" this to an `Array`, returning an error if it isn't one.
    pub fn as_array(&self) -> Result<&Array> {
        if let Self::Array(s) = self {
            Ok(s)
        } else {
            bail!("not an array: {:?}", self)
        }
    }

    /// "Down-casts" this to a `Ptr`, returning an error if it isn't one.
    pub fn as_ptr(&self) -> Result<&Ptr> {
        if let Self::Ptr(s) = self {
            Ok(s)
        } else {
            bail!("not a pointer: {:?}", self)
        }
    }
}

/// A value of an enumeration.
#[derive(Clone, Debug, Default)]
pub struct Enum(String, Option<Box<Value>>);

impl Enum {
    /// Enumeration variant discriminator, as written in the source code.
    pub fn disc(&self) -> &str {
        &self.0
    }

    /// Contents/payload of this variant, if any.
    ///
    /// Note that the variant contents are a 1-tuple in cases like this:
    ///
    /// ```ignore
    /// enum Foo {
    ///     Bar(u32),
    /// }
    /// ```
    ///
    /// ...rather than a `u32`.
    pub fn contents(&self) -> Option<&Value> {
        self.1.as_ref().map(|b| &**b)
    }

    /// Interprets this as an `Option`-shaped enumeration consisting of `Some`
    /// and `None` variants, and extracts the contents if it's a `Some`.
    pub fn as_option(&self) -> Result<Option<&Value>> {
        match (self.disc(), self.contents()) {
            ("Some", Some(c)) => Ok(Some(c.as_1tuple()?)),
            ("None", _) => Ok(None),
            _ => bail!("not an option: {:?}", self),
        }
    }
}

/// A value of a basetype, often called a "primitive."
///
/// There is one variant of this enum for every fundamental type in Rust, unless
/// we've forgotten some.
#[derive(Clone, Debug)]
pub enum Base {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),

    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),

    Bool(bool),

    F32(f32),
    F64(f64),
}

impl Base {
    /// "Downcasts" this to a `bool`, returning `None` if it isn't one.
    pub fn as_bool(&self) -> Option<bool> {
        if let &Self::Bool(x) = self {
            Some(x)
        } else {
            None
        }
    }

    /// "Downcasts" this to a `u8`, returning `None` if it isn't one.
    pub fn as_u8(&self) -> Option<u8> {
        if let &Self::U8(x) = self {
            Some(x)
        } else {
            None
        }
    }

    /// "Downcasts" this to a `u16`, returning `None` if it isn't one.
    pub fn as_u16(&self) -> Option<u16> {
        if let &Self::U16(x) = self {
            Some(x)
        } else {
            None
        }
    }

    /// "Downcasts" this to a `u32`, returning `None` if it isn't one.
    pub fn as_u32(&self) -> Option<u32> {
        if let &Self::U32(x) = self {
            Some(x)
        } else {
            None
        }
    }

    /// "Downcasts" this to a `u64`, returning `None` if it isn't one.
    pub fn as_u64(&self) -> Option<u64> {
        if let &Self::U64(x) = self {
            Some(x)
        } else {
            None
        }
    }
}

/// A struct with named fields.
#[derive(Clone, Debug, Default)]
pub struct Struct {
    members: BTreeMap<String, Box<Value>>,
}

impl Struct {
    /// Verifies that the struct contains _at least_ members with the given
    /// `names`. If not, returns an error.
    pub fn check_members(&self, names: &[&str]) -> Result<()> {
        for &n in names {
            if !self.members.contains_key(n) {
                bail!("member missing from struct: {}", n);
            }
        }
        Ok(())
    }
}

/// Allows access to struct members using `s["foo"]`.
impl std::ops::Index<&str> for Struct {
    type Output = Value;

    fn index(&self, name: &str) -> &Self::Output {
        &*self.members[name]
    }
}

/// A tuple or tuple struct.
#[derive(Clone, Debug, Default)]
pub struct Tuple(Vec<Value>);

/// Allows the tuple to be treated like a slice, e.g. with `len()` and indexing.
impl std::ops::Deref for Tuple {
    type Target = [Value];

    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}

/// An array, e.g. `[T; N]`.
#[derive(Clone, Debug, Default)]
pub struct Array(Vec<Value>);

/// Allows the array to be treated like a slice, e.g. with `len()` and indexing.
impl std::ops::Deref for Array {
    type Target = [Value];

    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}

/// A pointer with an embedded type.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Ptr(HubrisGoff, u32);

impl Ptr {
    /// Retrieves the goff associated with the pointed-to type.
    pub fn goff(&self) -> HubrisGoff {
        self.0
    }

    /// Retrieves the address being pointed to.
    pub fn addr(&self) -> u32 {
        self.1
    }

    /// Indirects the pointer and maps it into a Rust `T`.
    pub fn load_from<T: Load>(
        &self,
        hubris: &HubrisArchive,
        core: &mut dyn Core,
    ) -> Result<T> {
        let ty = hubris.lookup_type(self.goff())?;
        let mut buf = vec![0; ty.size(hubris)?];
        core.read_8(self.addr(), &mut buf)?;
        load(hubris, &buf, ty, 0)
    }
}

/// Loads data from memory image `buf` at offset `addr` and maps it onto a Rust
/// `T`.
pub fn load<'a, T: Load>(
    hubris: &HubrisArchive,
    buf: &[u8],
    ty: impl Into<HubrisType<'a>>,
    addr: usize,
) -> Result<T> {
    Load::from_value(&load_value(hubris, buf, ty.into(), addr)?)
}

/// Loads data from memory image `buf` at offset `addr` and represents it as a
/// `Value`.
pub fn load_value(
    hubris: &HubrisArchive,
    buf: &[u8],
    ty: HubrisType<'_>,
    addr: usize,
) -> Result<Value> {
    match ty {
        HubrisType::Struct(sty) => load_struct_or_tuple(hubris, buf, sty, addr),
        HubrisType::Enum(ety) => {
            load_enum(hubris, buf, ety, addr).map(Value::Enum)
        }
        HubrisType::Base(bty) => load_base(buf, bty, addr).map(Value::Base),
        HubrisType::Array(bty) => {
            load_array(hubris, buf, bty, addr).map(Value::Array)
        }
        HubrisType::Ptr(t) => load_ptr(buf, t, addr).map(Value::Ptr),
        _ => panic!("{:?}", ty),
    }
}

/// Loads a pointer from image `buf` at offset `addr` and interprets it as a
/// pointer to the type designated by `ty`.
pub fn load_ptr(buf: &[u8], ty: HubrisGoff, addr: usize) -> Result<Ptr> {
    let buf = buf
        .get(addr..addr + 4)
        .ok_or_else(|| anyhow!("address {} out of range for pointer", addr))?;
    let dest = u32::from_le_bytes(buf.try_into().unwrap());
    Ok(Ptr(ty, dest))
}

/// Loads an `Array` from memory image `buf` at offset `addr`.
pub fn load_array(
    hubris: &HubrisArchive,
    buf: &[u8],
    ty: &HubrisArray,
    addr: usize,
) -> Result<Array> {
    let elt_ty = hubris.lookup_type(ty.goff)?;
    let elt_size = elt_ty.size(hubris)?;

    let mut elements = Vec::with_capacity(ty.count);
    for i in 0..ty.count {
        elements.push(load_value(hubris, buf, elt_ty, addr + i * elt_size)?);
    }
    Ok(Array(elements))
}

/// Loads either a struct or tuple from memory image `buf` at offset `addr`.
///
/// This operation might seem weird, and it is. Structs and tuples are both
/// represented in Rust DWARF as structs, except tuples have members that match
/// a particular naming convention.
///
/// Whether the result is a `Value::Tuple` or `Value::Struct` depends on the
/// contents of `ty`, so you at least don't have to worry about it changing for
/// different loaded values.
pub fn load_struct_or_tuple(
    hubris: &HubrisArchive,
    buf: &[u8],
    ty: &HubrisStruct,
    addr: usize,
) -> Result<Value> {
    // Scan the shape of the type to tell if it's tupley. Tuples are structs
    // that only have fields of the form __#, where # is a decimal number.
    let mut probably_a_tuple = true;
    for m in &ty.members {
        if !m.name.starts_with("__") {
            probably_a_tuple = false;
            break;
        }
        if !m.name[2..].chars().all(|c| c.is_numeric()) {
            probably_a_tuple = false;
            break;
        }
    }

    if probably_a_tuple {
        // Start off the tuple filled with nonsense, so that we can set the
        // values out of order if required. Do tuple members ever appear out of
        // order? No idea! But if they do, this method will keep working.
        let mut contents = vec![Value::Base(Base::U8(0)); ty.members.len()];
        for m in &ty.members {
            let maddr = addr + m.offset;
            let mty = hubris.lookup_type(m.goff)?;
            let v = load_value(hubris, buf, mty, maddr)?;

            let index = m.name[2..].parse::<usize>()?;
            contents[index] = v;
        }

        Ok(Value::Tuple(Tuple(contents)))
    } else {
        load_struct(hubris, buf, ty, addr).map(Value::Struct)
    }
}

/// Loads a struct from memory image `buf` at offset `addr`.
pub fn load_struct(
    hubris: &HubrisArchive,
    buf: &[u8],
    ty: &HubrisStruct,
    addr: usize,
) -> Result<Struct> {
    let mut s = Struct::default();

    for m in &ty.members {
        let maddr = addr + m.offset;
        let mty = hubris.lookup_type(m.goff)?;
        s.members.insert(
            m.name.clone(),
            Box::new(load_value(hubris, buf, mty, maddr)?),
        );
    }

    Ok(s)
}

/// Loads an enum from memory image `buf` at offset `addr`.
pub fn load_enum(
    hubris: &HubrisArchive,
    buf: &[u8],
    ty: &HubrisEnum,
    addr: usize,
) -> Result<Enum> {
    let var = ty.determine_variant(hubris, &buf[addr..])?;
    let val = if let Some(goff) = var.goff {
        Some(Box::new(load_value(
            hubris,
            buf,
            hubris.lookup_type(goff)?,
            addr,
        )?))
    } else {
        None
    };

    Ok(Enum(var.name.to_string(), val))
}

/// Loads a basetype from memory image `buf` at offset `addr`.
pub fn load_base(buf: &[u8], ty: &HubrisBasetype, addr: usize) -> Result<Base> {
    let buf = buf.get(addr..addr + ty.size).ok_or_else(|| {
        anyhow!("address {} out of range for type {:?}", addr, ty)
    })?;

    use crate::hubris::HubrisEncoding::*;
    let v = match (ty.encoding, ty.size) {
        (Signed, 1) => Base::I8(buf[0] as i8),
        (Signed, 2) => Base::I16(i16::from_le_bytes(buf.try_into().unwrap())),
        (Signed, 4) => Base::I32(i32::from_le_bytes(buf.try_into().unwrap())),
        (Signed, 8) => Base::I64(i64::from_le_bytes(buf.try_into().unwrap())),
        (Signed, 16) => {
            Base::I128(i128::from_le_bytes(buf.try_into().unwrap()))
        }

        (Unsigned, 1) => Base::U8(buf[0] as u8),
        (Unsigned, 2) => Base::U16(u16::from_le_bytes(buf.try_into().unwrap())),
        (Unsigned, 4) => Base::U32(u32::from_le_bytes(buf.try_into().unwrap())),
        (Unsigned, 8) => Base::U64(u64::from_le_bytes(buf.try_into().unwrap())),
        (Unsigned, 16) => {
            Base::U128(u128::from_le_bytes(buf.try_into().unwrap()))
        }

        (Bool, 1) => Base::Bool(buf[0] != 0),

        (Float, 4) => Base::F32(f32::from_le_bytes(buf.try_into().unwrap())),
        (Float, 8) => Base::F64(f64::from_le_bytes(buf.try_into().unwrap())),

        _ => panic!("unexpected basetype: {:?}", ty),
    };
    Ok(v)
}

/// Trait implemented by Rust types that can construct themselves from a
/// `Value`.
pub trait Load: Sized {
    /// Constructs a `Self` from `v`, or returns an error if that's not
    /// possible.
    fn from_value(v: &Value) -> Result<Self>;
}

impl Load for Ptr {
    fn from_value(v: &Value) -> Result<Self> {
        Ok(v.as_ptr()?.clone())
    }
}

//
// Load impls for primitive and common types below. Is an impl you're wanting
// missing? It is likely not deliberate! We're filling these in as we require
// them.
//

impl Load for Value {
    fn from_value(v: &Value) -> Result<Self> {
        Ok(v.clone())
    }
}

impl Load for bool {
    fn from_value(v: &Value) -> Result<Self> {
        v.as_base()?.as_bool().ok_or_else(|| anyhow!("not a bool: {:?}", v))
    }
}

impl Load for u8 {
    fn from_value(v: &Value) -> Result<Self> {
        v.as_base()?.as_u8().ok_or_else(|| anyhow!("not a u8: {:?}", v))
    }
}

impl Load for u16 {
    fn from_value(v: &Value) -> Result<Self> {
        v.as_base()?.as_u16().ok_or_else(|| anyhow!("not a u16: {:?}", v))
    }
}

impl Load for u32 {
    fn from_value(v: &Value) -> Result<Self> {
        v.as_base()?.as_u32().ok_or_else(|| anyhow!("not a u32: {:?}", v))
    }
}

impl Load for u64 {
    fn from_value(v: &Value) -> Result<Self> {
        v.as_base()?.as_u64().ok_or_else(|| anyhow!("not a u64: {:?}", v))
    }
}

impl<T: Load> Load for Option<T> {
    fn from_value(v: &Value) -> Result<Self> {
        Ok(match v.as_enum()?.as_option()? {
            Some(nv) => Some(T::from_value(nv)?),
            None => None,
        })
    }
}

impl<T: Load, const N: usize> Load for [T; N] {
    fn from_value(v: &Value) -> Result<Self> {
        let v = v.as_array()?;
        if v.len() != N {
            bail!("expected array of length {}, got: {:?}", N, v);
        }
        let mut out = Vec::with_capacity(N);
        for elt in v.iter() {
            out.push(T::from_value(elt)?);
        }
        Ok(out.try_into().map_err(|_| ()).unwrap())
    }
}

macro_rules! tuple_load_impl {
    ($n:expr, $($ty:ident),*) => {
        impl<$($ty: Load),*> Load for ($($ty,)*) {
            fn from_value(v: &Value) -> Result<Self> {
                let v = v.as_tuple()?;
                if v.len() != $n {
                    bail!("expected tuple of length {}, got: {:?}", $n, v);
                }
                let mut id = 0;
                #[allow(unused_assignments)]
                Ok(($($ty::from_value(&v[{let i = id; id += 1; i}])?,)*))
            }
        }
    };
}

tuple_load_impl!(1, A);
tuple_load_impl!(2, A, B);
tuple_load_impl!(3, A, B, C);
tuple_load_impl!(4, A, B, C, D);
tuple_load_impl!(5, A, B, C, D, E);
tuple_load_impl!(6, A, B, C, D, E, F);
tuple_load_impl!(7, A, B, C, D, E, F, G);
tuple_load_impl!(8, A, B, C, D, E, F, G, H);
