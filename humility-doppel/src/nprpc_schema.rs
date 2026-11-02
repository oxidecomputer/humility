// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! Mirrors of `nprpc::flat` types, for reading the `nprpc` interface schemas
//! a task exposes with `nprpc_net::expose_interface!`.
//!
//! These are loaded from the archive's DWARF, converted into the `nprpc`
//! types, and decoded by `nprpc` itself into owned schemas.

use anyhow::{Context, Result, bail};
use humility::hubris::{HubrisArchive, HubrisModule};
use humility::reflect::{self, Load};
use nprpc::flat;

pub use nprpc::flat::OwnedDataModelType;
pub use nprpc::flat::{OwnedInterfaceInfo, OwnedMethodInfo};

#[derive(Copy, Clone, Debug, Load)]
pub struct NameRef {
    pub offset: u32,
    pub len: u32,
}

#[derive(Copy, Clone, Debug, Load)]
pub struct Span {
    pub start: u32,
    pub len: u32,
}

#[derive(Copy, Clone, Debug, Load)]
pub enum FlatNode {
    Bool,
    I8,
    U8,
    I16,
    I32,
    I64,
    I128,
    U16,
    U32,
    U64,
    U128,
    Usize,
    Isize,
    F32,
    F64,
    Char,
    String { max_len: Option<u32> },
    ByteArray { max_len: Option<u32> },
    Option { inner: u32 },
    Unit,
    Seq { element: u32, max_len: Option<u32> },
    Tuple { elems: Span },
    Map { key: u32, val: u32, max_len: Option<u32> },
    Struct { name: NameRef, data: u32 },
    Enum { name: NameRef, variants: Span },
    Schema,
    Variant { name: NameRef, data: u32 },
    Field { name: NameRef, ty: u32 },
    DataUnit,
    DataNewtype { inner: u32 },
    DataTuple { elems: Span },
    DataStruct { fields: Span },
}

#[derive(Copy, Clone, Debug, Load)]
pub struct FlatMethod {
    pub name: NameRef,
    pub key: [u8; 8],
    pub rqst: u32,
    pub resp: u32,
}

/// Mirror of `nprpc::flat::FlatInterface`, of any size
#[derive(Clone, Debug, Load)]
#[load(check_name)]
pub struct FlatInterface {
    pub version: u32,
    pub name: NameRef,
    pub max_request_size: Option<u32>,
    pub max_response_size: Option<u32>,
    pub methods: Vec<FlatMethod>,
    pub nodes: Vec<FlatNode>,
    pub children: Vec<u32>,
    pub names: Vec<u8>,
}

impl From<NameRef> for flat::NameRef {
    fn from(n: NameRef) -> Self {
        Self { offset: n.offset, len: n.len }
    }
}

impl From<Span> for flat::Span {
    fn from(s: Span) -> Self {
        Self { start: s.start, len: s.len }
    }
}

impl From<FlatMethod> for flat::FlatMethod {
    fn from(m: FlatMethod) -> Self {
        Self { name: m.name.into(), key: m.key, rqst: m.rqst, resp: m.resp }
    }
}

impl From<FlatNode> for flat::FlatNode {
    fn from(n: FlatNode) -> Self {
        use flat::FlatNode as FlNd;
        match n {
            FlatNode::Bool => FlNd::Bool,
            FlatNode::I8 => FlNd::I8,
            FlatNode::U8 => FlNd::U8,
            FlatNode::I16 => FlNd::I16,
            FlatNode::I32 => FlNd::I32,
            FlatNode::I64 => FlNd::I64,
            FlatNode::I128 => FlNd::I128,
            FlatNode::U16 => FlNd::U16,
            FlatNode::U32 => FlNd::U32,
            FlatNode::U64 => FlNd::U64,
            FlatNode::U128 => FlNd::U128,
            FlatNode::Usize => FlNd::Usize,
            FlatNode::Isize => FlNd::Isize,
            FlatNode::F32 => FlNd::F32,
            FlatNode::F64 => FlNd::F64,
            FlatNode::Char => FlNd::Char,
            FlatNode::String { max_len } => FlNd::String { max_len },
            FlatNode::ByteArray { max_len } => FlNd::ByteArray { max_len },
            FlatNode::Option { inner } => FlNd::Option { inner },
            FlatNode::Unit => FlNd::Unit,
            FlatNode::Seq { element, max_len } => {
                FlNd::Seq { element, max_len }
            }
            FlatNode::Tuple { elems } => FlNd::Tuple { elems: elems.into() },
            FlatNode::Map { key, val, max_len } => {
                FlNd::Map { key, val, max_len }
            }
            FlatNode::Struct { name, data } => {
                FlNd::Struct { name: name.into(), data }
            }
            FlatNode::Enum { name, variants } => {
                FlNd::Enum { name: name.into(), variants: variants.into() }
            }
            FlatNode::Schema => FlNd::Schema,
            FlatNode::Variant { name, data } => {
                FlNd::Variant { name: name.into(), data }
            }
            FlatNode::Field { name, ty } => {
                FlNd::Field { name: name.into(), ty }
            }
            FlatNode::DataUnit => FlNd::DataUnit,
            FlatNode::DataNewtype { inner } => FlNd::DataNewtype { inner },
            FlatNode::DataTuple { elems } => {
                FlNd::DataTuple { elems: elems.into() }
            }
            FlatNode::DataStruct { fields } => {
                FlNd::DataStruct { fields: fields.into() }
            }
        }
    }
}

impl FlatInterface {
    /// Decodes into an owned interface description.
    pub fn decode(&self) -> Result<OwnedInterfaceInfo> {
        let methods: Vec<flat::FlatMethod> =
            self.methods.iter().map(|&m| m.into()).collect();
        let nodes: Vec<flat::FlatNode> =
            self.nodes.iter().map(|&n| n.into()).collect();
        let flat = flat::FlatInterfaceRef {
            version: self.version,
            name: self.name.into(),
            max_request_size: self.max_request_size,
            max_response_size: self.max_response_size,
            methods: &methods,
            nodes: &nodes,
            children: &self.children,
            names: &self.names,
        };
        Ok(flat.decode()?)
    }
}

/// Loads all `nprpc` interfaces exposed by a given module/task.
pub fn load_interfaces(
    hubris: &HubrisArchive,
    module: &HubrisModule,
) -> Result<Vec<OwnedInterfaceInfo>> {
    module
        .nprpc_schemas
        .iter()
        .map(|schema| {
            let ty = hubris.lookup_type(schema.goff)?;
            let flat: FlatInterface =
                reflect::load(hubris, &schema.contents, ty, 0)?;
            if flat.version != flat::FLAT_VERSION {
                bail!(
                    "unsupported nprpc schema version {} (expected {})",
                    flat.version,
                    flat::FLAT_VERSION,
                );
            }
            flat.decode()
        })
        .collect::<Result<_>>()
        .with_context(|| format!("loading nprpc schemas for {}", module.name))
}
