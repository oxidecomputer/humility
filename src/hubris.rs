/*
 * Copyright 2020 Oxide Computer Company
 */

use std::collections::HashMap;
use std::collections::{btree_map, BTreeMap};
use std::convert::Infallible;
use std::convert::TryInto;
use std::fmt;
use std::fmt::Write;
use std::fs;
use std::fs::OpenOptions;
use std::io::prelude::*;
use std::io::Cursor;
use std::mem::size_of;
use std::path::Path;
use std::str;
use std::time::Instant;

use fallible_iterator::FallibleIterator;

use anyhow::{anyhow, bail, ensure, Context, Result};

use capstone::prelude::*;
use capstone::InsnGroupType;
use goblin::elf::Elf;
use multimap::MultiMap;
use rustc_demangle::demangle;
use scroll::{IOwrite, Pwrite};

use gimli::UnwindSection;
use num_traits::FromPrimitive;

use crate::debug::*;

const OXIDE_NT_NAME: &str = "Oxide Computer Company";
const OXIDE_NT_BASE: u32 = 0x1de << 20;
const OXIDE_NT_HUBRIS_ARCHIVE: u32 = OXIDE_NT_BASE + 1;
const OXIDE_NT_HUBRIS_REGISTERS: u32 = OXIDE_NT_BASE + 2;

#[derive(Default, Debug)]
pub struct HubrisManifest {
    version: Option<String>,
    gitrev: Option<String>,
    features: Vec<String>,
    board: Option<String>,
    target: Option<String>,
    task_features: HashMap<String, Vec<String>>,
    pub task_irqs: HashMap<String, Vec<(u32, u32)>>,
    outputs: HashMap<String, (u32, u32)>,
    peripherals: BTreeMap<String, u32>,
}

#[derive(Debug)]
pub struct HubrisArchive {
    // the entire archive
    archive: Vec<u8>,

    // constructed manifest
    pub manifest: HubrisManifest,

    // app table
    apptable: (u32, Vec<u8>),

    // loaded regions
    loaded: BTreeMap<u32, HubrisRegion>,

    // current object
    current: u32,

    // Capstone library handle
    cs: capstone::Capstone,

    // Instructions: address to bytes/target tuple
    instrs: HashMap<u32, (Vec<u8>, HubrisTarget)>,

    // Manual stack pushes before a syscall
    syscall_pushes: HashMap<u32, Option<Vec<ARMRegister>>>,

    // Current registers (if a dump)
    registers: HashMap<ARMRegister, u32>,

    // Modules: text address to module
    modules: BTreeMap<u32, HubrisModule>,

    // Tasks: name of task to ID
    tasks: HashMap<String, HubrisTask>,

    // DWARF call frame debugging sections: task to raw bytes
    frames: HashMap<HubrisTask, Vec<u8>>,

    // DWARF source code: goff to file/line
    src: HashMap<HubrisGoff, HubrisSrc>,

    // DWARF symbols: address to name/length/goff tuple
    dsyms: BTreeMap<u32, (String, u32, HubrisGoff)>,

    // ELF symbols: address to name/length tuple
    esyms: BTreeMap<u32, (String, u32)>,

    // ELF symbols: name to value/length
    esyms_byname: MultiMap<String, (u32, u32)>,

    // Inlined: address/nesting tuple to length/goff/origin tuple
    inlined: BTreeMap<(u32, isize), (u32, HubrisGoff, HubrisGoff)>,

    // Subprograms: goff to name
    subprograms: HashMap<HubrisGoff, String>,

    // Base types: goff to size
    basetypes: HashMap<HubrisGoff, HubrisBasetype>,

    // Base types: goff to underlying type
    ptrtypes: HashMap<HubrisGoff, (String, HubrisGoff)>,

    // Structures: goff to struct
    structs: HashMap<HubrisGoff, HubrisStruct>,

    // Structures: name to goff
    structs_byname: MultiMap<String, HubrisGoff>,

    // Enums: goff to enum
    enums: HashMap<HubrisGoff, HubrisEnum>,

    // Enums: name to goff
    enums_byname: MultiMap<String, HubrisGoff>,

    // Arrays: goff to array
    arrays: HashMap<HubrisGoff, HubrisArray>,

    // Variables: name to goff/address/size tuple
    variables: MultiMap<String, HubrisVariable>,

    // Unions: goff to union
    unions: HashMap<HubrisGoff, HubrisUnion>,

    // Definitions: name to goff
    definitions: MultiMap<String, HubrisGoff>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum HubrisTask {
    Kernel,
    Task(u32),
}

///
/// An identifier that corresponds to a global offset within a particular DWARF
/// object.
///
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone)]
pub struct HubrisGoff {
    pub object: u32,
    pub goff: usize,
}

#[derive(Copy, Clone, Debug)]
pub struct HubrisSymbol<'a> {
    pub addr: u32,
    pub name: &'a str,
    pub goff: HubrisGoff,
}

#[derive(Debug)]
pub struct HubrisInlined<'a> {
    pub addr: u32,
    pub name: &'a str,
    pub id: HubrisGoff,
    pub origin: HubrisGoff,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum HubrisEncoding {
    Unknown,
    Signed,
    Unsigned,
    Float,
    Bool,
}

#[derive(Debug)]
pub struct HubrisBasetype {
    pub encoding: HubrisEncoding,
    pub size: usize,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct HubrisStructMember {
    pub offset: usize,
    pub name: String,
    pub goff: HubrisGoff,
}

#[derive(Debug)]
pub struct HubrisStruct {
    pub name: String,
    pub goff: HubrisGoff,
    pub size: usize,
    pub members: Vec<HubrisStructMember>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct HubrisVariable {
    pub goff: HubrisGoff,
    pub addr: u32,
    pub size: usize,
}

#[derive(Debug)]
pub struct HubrisArray {
    pub goff: HubrisGoff,
    pub count: usize,
}

#[derive(Copy, Clone, Debug)]
pub struct HubrisRegionAttr {
    pub read: bool,
    pub write: bool,
    pub execute: bool,
    pub device: bool,
    pub dma: bool,
}

#[derive(Copy, Clone, Debug)]
pub struct HubrisRegion {
    pub daddr: Option<u32>,
    pub base: u32,
    pub size: u32,
    pub mapsize: u32,
    pub attr: HubrisRegionAttr,
    pub task: HubrisTask,
}

#[derive(Debug)]
pub struct HubrisEnumVariant {
    pub name: String,
    pub offset: usize,
    pub goff: Option<HubrisGoff>,
    pub tag: Option<u64>,
}

#[derive(Copy, Clone, Debug)]
pub enum HubrisDiscriminant {
    None,
    Expected(HubrisGoff),
    Value(HubrisGoff, usize),
}

#[derive(Debug)]
pub struct HubrisEnum {
    pub name: String,
    pub goff: HubrisGoff,
    pub size: usize,
    pub discriminant: HubrisDiscriminant,
    /// temporary to hold tag of next variant
    pub tag: Option<u64>,
    pub variants: Vec<HubrisEnumVariant>,
}

#[derive(Debug)]
pub struct HubrisUnion {
    pub name: String,
    pub goff: HubrisGoff,
    pub size: usize,
    pub variants: Vec<HubrisEnumVariant>,
}

#[derive(Copy, Clone, Debug)]
pub enum HubrisType<'a> {
    Base(&'a HubrisBasetype),
    Struct(&'a HubrisStruct),
    Enum(&'a HubrisEnum),
    Array(&'a HubrisArray),
    Union(&'a HubrisUnion),
    Ptr(HubrisGoff), // TODO better type
}

impl HubrisType<'_> {
    pub fn size(&self, hubris: &HubrisArchive) -> Result<usize> {
        match self {
            Self::Base(t) => Ok(t.size),
            Self::Struct(t) => Ok(t.size),
            Self::Enum(t) => Ok(t.size),
            Self::Union(t) => Ok(t.size),
            Self::Ptr(_) => Ok(4), // TODO
            Self::Array(t) => {
                let elt_size = hubris.lookup_type(t.goff)?.size(hubris)?;
                Ok(elt_size * t.count)
            }
        }
    }
}

impl<'a> From<&'a HubrisBasetype> for HubrisType<'a> {
    fn from(x: &'a HubrisBasetype) -> Self {
        Self::Base(x)
    }
}

impl<'a> From<&'a HubrisStruct> for HubrisType<'a> {
    fn from(x: &'a HubrisStruct) -> Self {
        Self::Struct(x)
    }
}

impl<'a> From<&'a HubrisEnum> for HubrisType<'a> {
    fn from(x: &'a HubrisEnum) -> Self {
        Self::Enum(x)
    }
}

impl<'a> From<&'a HubrisArray> for HubrisType<'a> {
    fn from(x: &'a HubrisArray) -> Self {
        Self::Array(x)
    }
}

impl<'a> From<&'a HubrisUnion> for HubrisType<'a> {
    fn from(x: &'a HubrisUnion) -> Self {
        Self::Union(x)
    }
}

#[derive(Copy, Clone, Debug)]
pub enum HubrisTarget {
    None,
    Direct(u32),
    Indirect,
    Call(u32),
    IndirectCall,
    Return,
}

#[derive(Debug)]
pub struct HubrisStackFrame<'a> {
    pub cfa: u32,
    pub sym: Option<HubrisSymbol<'a>>,
    pub registers: HashMap<ARMRegister, u32>,
    pub inlined: Option<Vec<HubrisInlined<'a>>>,
}

#[derive(Debug)]
pub struct HubrisSrc {
    pub file: String,
    pub directory: Option<String>,
    pub comp_directory: Option<String>,
    pub line: u64,
}

#[derive(Debug)]
pub struct HubrisModule {
    pub name: String,
    pub object: u32,
    pub task: HubrisTask,
    pub textbase: u32,
    pub textsize: u32,
    pub memsize: u32,
    pub heapbss: (Option<u32>, Option<u32>),
}

#[derive(Copy, Clone, Debug, Default)]
pub struct HubrisPrintFormat {
    pub indent: usize,
    pub newline: bool,
    pub hex: bool,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum HubrisValidate {
    ArchiveMatch,
    Booted,
}

impl fmt::Display for HubrisGoff {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.object > 0 {
            write!(f, "GOFF 0x{:08x} (object {})", self.goff, self.object)
        } else {
            write!(f, "GOFF 0x{:08x}", self.goff)
        }
    }
}

impl fmt::Display for HubrisTask {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            HubrisTask::Kernel => write!(f, "kernel"),
            HubrisTask::Task(id) => write!(f, "Task #{}", id),
        }
    }
}

impl From<HubrisGoff> for HubrisTask {
    fn from(goff: HubrisGoff) -> Self {
        if goff.object == 0 {
            HubrisTask::Kernel
        } else {
            HubrisTask::Task(goff.object - 1)
        }
    }
}

fn dwarf_name<'a>(
    dwarf: &'a gimli::Dwarf<gimli::EndianSlice<gimli::LittleEndian>>,
    value: gimli::AttributeValue<gimli::EndianSlice<gimli::LittleEndian>>,
) -> Option<&'a str> {
    match value {
        gimli::AttributeValue::DebugStrRef(strref) => {
            let dstring = dwarf.debug_str.get_str(strref).ok()?;
            let ddstring = str::from_utf8(dstring.slice()).ok()?;
            Some(ddstring)
        }
        _ => None,
    }
}

impl HubrisTask {
    pub fn id(&self) -> String {
        match self {
            HubrisTask::Kernel => "-".to_string(),
            HubrisTask::Task(id) => format!("{}", id),
        }
    }
}

impl HubrisStruct {
    pub fn lookup_member(&self, name: &str) -> Result<&HubrisStructMember> {
        for member in &self.members {
            if member.name == name {
                return Ok(member);
            }
        }

        bail!("missing member: {}.{}", self.name, name)
    }
}

impl HubrisEnum {
    pub fn lookup_variant(&self, tag: u64) -> Option<&HubrisEnumVariant> {
        for variant in &self.variants {
            match variant.tag {
                Some(t) if t == tag => {
                    return Some(variant);
                }
                Some(_t) => {}
                None => {
                    return Some(variant);
                }
            }
        }
        None
    }

    pub fn lookup_variant_byname(
        &self,
        name: &str,
    ) -> Result<&HubrisEnumVariant> {
        for variant in &self.variants {
            if variant.name == name {
                return Ok(variant);
            }
        }

        bail!("missing variant: {}.{}", self.name, name)
    }

    pub fn determine_variant(
        &self,
        hubris: &HubrisArchive,
        buf: &[u8],
    ) -> Result<&HubrisEnumVariant> {
        let readval = |b: &[u8], o, sz| -> Result<u64> {
            Ok(match sz {
                1 => b[o] as u64,
                2 => u16::from_le_bytes(b[o..o + 2].try_into()?) as u64,
                4 => u32::from_le_bytes(b[o..o + 4].try_into()?) as u64,
                8 => u64::from_le_bytes(b[o..o + 8].try_into()?) as u64,
                _ => {
                    bail!("bad variant size!");
                }
            })
        };

        if let HubrisDiscriminant::Value(goff, offs) = self.discriminant {
            let size = match hubris.basetypes.get(&goff) {
                Some(v) => v.size,
                None => {
                    bail!("enum has discriminant of unknown type: {}", goff);
                }
            };

            let val = readval(buf, offs, size)?;

            match self.lookup_variant(val) {
                None => {
                    bail!("unknown variant: 0x{:x}", val);
                }

                Some(variant) => Ok(variant),
            }
        } else {
            if self.variants.is_empty() {
                bail!("enum {} has no variants");
            }

            if self.variants.len() > 1 {
                bail!("enum has multiple variants but no discriminant");
            }

            Ok(&self.variants[0])
        }
    }
}

impl HubrisUnion {
    /*
     * This is really horrid, but we are going to heuristically determine if
     * this is a MaybeUninit -- and if so, return the GOFF of the "value"
     * member.  It should go without saying that this is brittle in obvious
     * ways:  if another union has the same two members as MaybeUninit or
     * MaybeUninit starts naming its members differently, this will break --
     * and it will deserve to be broken.
     */
    pub fn maybe_uninit(&self) -> Option<HubrisGoff> {
        if self.variants.len() != 2 {
            return None;
        }

        if self.variants[0].name != "uninit" {
            return None;
        }

        if self.variants[1].name != "value" {
            return None;
        }

        self.variants[1].goff
    }
}

impl HubrisSrc {
    pub fn fullpath(&self) -> String {
        format!(
            "{}{}{}",
            if let Some(ref dir) = self.comp_directory {
                format!("{}/", dir)
            } else {
                "".to_string()
            },
            if let Some(ref dir) = self.directory {
                format!("{}/", dir)
            } else {
                "".to_string()
            },
            self.file
        )
    }
}

#[rustfmt::skip::macros(anyhow, bail)]
impl HubrisArchive {
    pub fn new() -> Result<HubrisArchive> {
        /*
         * Initialize Capstone, being sure to specify not only our architecture
         * but also that we are disassembling Thumb-2 -- and (importantly) to
         * allow M-profile instructions.
         */
        let cs = Capstone::new()
            .arm()
            .mode(arch::arm::ArchMode::Thumb)
            .extra_mode(std::iter::once(arch::arm::ArchExtraMode::MClass))
            .detail(true)
            .build();

        Ok(Self {
            archive: Vec::new(),
            apptable: (0, Vec::new()),
            manifest: Default::default(),
            loaded: BTreeMap::new(),
            cs: match cs {
                Ok(mut cs) => {
                    cs.set_skipdata(true).expect("failed to set skipdata");
                    cs
                }
                Err(err) => {
                    bail!("failed to initialize disassembler: {}", err);
                }
            },
            current: 0,
            instrs: HashMap::new(),
            syscall_pushes: HashMap::new(),
            registers: HashMap::new(),
            modules: BTreeMap::new(),
            tasks: HashMap::new(),
            frames: HashMap::new(),
            src: HashMap::new(),
            dsyms: BTreeMap::new(),
            esyms: BTreeMap::new(),
            esyms_byname: MultiMap::new(),
            inlined: BTreeMap::new(),
            subprograms: HashMap::new(),
            basetypes: HashMap::new(),
            ptrtypes: HashMap::new(),
            structs: HashMap::new(),
            structs_byname: MultiMap::new(),
            enums: HashMap::new(),
            enums_byname: MultiMap::new(),
            arrays: HashMap::new(),
            variables: MultiMap::new(),
            unions: HashMap::new(),
            definitions: MultiMap::new(),
        })
    }

    pub fn instr_len(&self, addr: u32) -> Option<u32> {
        self.instrs.get(&addr).map(|instr| instr.0.len() as u32)
    }

    pub fn instr_target(&self, addr: u32) -> HubrisTarget {
        self.instrs
            .get(&addr)
            .map(|instr| instr.1)
            .unwrap_or(HubrisTarget::None)
    }

    pub fn instr_mod(&self, addr: u32) -> Option<&str> {
        if let Some(module) = self.modules.range(..=addr).next_back() {
            if addr < *module.0 + (module.1).textsize {
                Some(&(module.1).name)
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn instr_sym(&self, addr: u32) -> Option<(&str, u32)> {
        let sym: Option<(&str, u32)>;

        /*
         * First, check our DWARF symbols.
         */
        sym = match self.dsyms.range(..=addr).next_back() {
            Some(sym) if addr < *sym.0 + (sym.1).1 => {
                Some((&(sym.1).0, *sym.0 as u32))
            }
            _ => None,
        };

        /*
         * Fallback to our ELF symbols.
         */
        match sym {
            Some(_) => sym,
            None => match self.esyms.range(..=addr).next_back() {
                Some(sym) if addr < *sym.0 + (sym.1).1 => {
                    Some((&(sym.1).0, *sym.0 as u32))
                }
                _ => None,
            },
        }
    }

    pub fn instr_inlined(&self, pc: u32, base: u32) -> Vec<HubrisInlined> {
        let mut inlined: Vec<HubrisInlined> = vec![];

        /*
         * We find our stack of inlined functions by searching backwards from
         * our address (which we know must be greater than or equal to all
         * inlined functions that it is in).  This yields a vector that starts
         * from the greatest depth and ends with the least depth -- so we
         * reverse it before we return it.  We know that our search is over
         * when the address plus the length is less than our base.
         */
        for ((addr, _depth), (len, goff, origin)) in
            self.inlined.range(..=(pc, std::isize::MAX)).rev()
        {
            if addr + len < base {
                break;
            }

            if addr + len <= pc {
                continue;
            }

            if let Some(func) = self.subprograms.get(origin) {
                inlined.push(HubrisInlined {
                    addr: *addr as u32,
                    name: func,
                    id: *goff,
                    origin: *origin,
                });
            }
        }

        inlined.reverse();
        inlined
    }

    fn instr_branch_target(&self, instr: &capstone::Insn) -> HubrisTarget {
        let detail = match self.cs.insn_detail(instr) {
            Ok(detail) => detail,
            _ => return HubrisTarget::None,
        };

        let mut jump = false;
        let mut call = false;
        let mut brel = None;

        const BREL: u8 = InsnGroupType::CS_GRP_BRANCH_RELATIVE as u8;
        const JUMP: u8 = InsnGroupType::CS_GRP_JUMP as u8;
        const CALL: u8 = InsnGroupType::CS_GRP_CALL as u8;
        const ARM_REG_PC: u16 = arch::arm::ArmReg::ARM_REG_PC as u16;
        const ARM_REG_LR: u16 = arch::arm::ArmReg::ARM_REG_LR as u16;
        const ARM_INSN_POP: u32 = arch::arm::ArmInsn::ARM_INS_POP as u32;

        for g in detail.groups() {
            match g {
                InsnGroupId(BREL) => {
                    let arch = detail.arch_detail();
                    let ops = arch.operands();

                    let op = ops.last().unwrap_or_else(|| {
                        panic!("missing operand!");
                    });

                    if let arch::ArchOperand::ArmOperand(op) = op {
                        if let arch::arm::ArmOperandType::Imm(a) = op.op_type {
                            brel = Some(a as u32);
                        }
                    }
                }

                InsnGroupId(JUMP) => {
                    jump = true;
                }

                InsnGroupId(CALL) => {
                    call = true;
                }
                _ => {}
            }
        }

        if let Some(addr) = brel {
            if call {
                return HubrisTarget::Call(addr);
            } else {
                return HubrisTarget::Direct(addr);
            }
        }

        if call {
            return HubrisTarget::IndirectCall;
        }

        /*
         * If this is a JUMP that isn't a CALL, check to see if one of
         * its operands is LR -- in which case it's a return (or could be
         * a return).
         */
        if jump {
            for op in detail.arch_detail().operands() {
                if let arch::ArchOperand::ArmOperand(op) = op {
                    if let arch::arm::ArmOperandType::Reg(RegId(ARM_REG_LR)) =
                        op.op_type
                    {
                        return HubrisTarget::Return;
                    }
                }
            }

            return HubrisTarget::Indirect;
        }

        /*
         * Capstone doesn't have a group denoting returns (they are control
         * transfers, but not considered in the JUMP group), so explicitly
         * look for a pop instruction that writes to the PC.
         */
        if let InsnId(ARM_INSN_POP) = instr.id() {
            for op in detail.arch_detail().operands() {
                if let arch::ArchOperand::ArmOperand(op) = op {
                    if let arch::arm::ArmOperandType::Reg(RegId(ARM_REG_PC)) =
                        op.op_type
                    {
                        return HubrisTarget::Return;
                    }
                }
            }
        }

        HubrisTarget::None
    }

    fn instr_operands(&self, instr: &capstone::Insn) -> Vec<ARMRegister> {
        let detail = self.cs.insn_detail(instr).unwrap();
        let mut rval: Vec<ARMRegister> = Vec::new();

        for op in detail.arch_detail().operands() {
            if let arch::ArchOperand::ArmOperand(op) = op {
                if let arch::arm::ArmOperandType::Reg(id) = op.op_type {
                    rval.push(id.into());
                }
            }
        }

        rval
    }

    fn dwarf_goff<R: gimli::Reader<Offset = usize>>(
        &self,
        unit: &gimli::Unit<R>,
        entry: &gimli::DebuggingInformationEntry<
            gimli::EndianSlice<gimli::LittleEndian>,
            usize,
        >,
    ) -> HubrisGoff {
        let goff = match entry.offset().to_unit_section_offset(unit) {
            gimli::UnitSectionOffset::DebugInfoOffset(o) => o.0,
            gimli::UnitSectionOffset::DebugTypesOffset(o) => o.0,
        };

        HubrisGoff { object: self.current, goff }
    }

    fn dwarf_value_goff<R: gimli::Reader<Offset = usize>>(
        &self,
        unit: &gimli::Unit<R>,
        value: &gimli::AttributeValue<
            gimli::EndianSlice<gimli::LittleEndian>,
            usize,
        >,
    ) -> Option<HubrisGoff> {
        let goff;

        match value {
            gimli::AttributeValue::UnitRef(offs) => {
                goff = match offs.to_unit_section_offset(unit) {
                    gimli::UnitSectionOffset::DebugInfoOffset(o) => o.0,
                    gimli::UnitSectionOffset::DebugTypesOffset(o) => o.0,
                };
            }

            gimli::AttributeValue::DebugInfoRef(offs) => {
                goff = offs.0;
            }

            _ => {
                return None;
            }
        }

        Some(HubrisGoff { object: self.current, goff })
    }

    fn dwarf_fileline<R: gimli::Reader<Offset = usize>>(
        &mut self,
        dwarf: &gimli::Dwarf<R>,
        unit: &gimli::Unit<R>,
        entry: &gimli::DebuggingInformationEntry<
            gimli::EndianSlice<gimli::LittleEndian>,
            usize,
        >,
    ) -> Result<()> {
        let mut attrs = entry.attrs();
        let mut file = None;
        let mut line = None;

        let goff = self.dwarf_goff(unit, entry);

        while let Some(attr) = attrs.next()? {
            match (attr.name(), attr.value()) {
                (
                    gimli::constants::DW_AT_decl_file,
                    gimli::AttributeValue::FileIndex(value),
                ) => {
                    file = Some(value);
                }
                (
                    gimli::constants::DW_AT_decl_line,
                    gimli::AttributeValue::Udata(value),
                ) => {
                    line = Some(value);
                }
                _ => {}
            }
        }

        if let (Some(file), Some(line)) = (file, line) {
            let mut comp = None;
            let mut dir = None;

            let header = match unit.line_program {
                Some(ref program) => program.header(),
                None => return Ok(()),
            };

            let file = match header.file(file) {
                Some(header) => header,
                None => {
                    bail!("no header at {}", goff);
                }
            };

            if let Some(directory) = file.directory(header) {
                let directory = dwarf.attr_string(unit, directory)?;
                let directory = directory.to_string_lossy()?;

                if !directory.starts_with('/') {
                    if let Some(ref comp_dir) = unit.comp_dir {
                        comp = Some(comp_dir.to_string_lossy()?.to_string());
                    }
                }

                dir = Some(directory.to_string());
            }

            let s = dwarf.attr_string(unit, file.path_name())?;
            let d = s.to_string_lossy()?;

            self.src.insert(
                goff,
                HubrisSrc {
                    file: d.to_string(),
                    directory: dir,
                    comp_directory: comp,
                    line,
                },
            );
        }

        Ok(())
    }

    fn dwarf_inlined<R: gimli::Reader<Offset = usize>>(
        &mut self,
        dwarf: &gimli::Dwarf<R>,
        unit: &gimli::Unit<R>,
        entry: &gimli::DebuggingInformationEntry<
            gimli::EndianSlice<gimli::LittleEndian>,
            usize,
        >,
        depth: isize,
    ) -> Result<()> {
        /*
         * Iterate over our attributes looking for addresses
         */
        let mut attrs = entry.attrs();
        let mut low: Option<u64> = None;
        let mut high: Option<u64> = None;
        let mut origin: Option<HubrisGoff> = None;

        let goff = self.dwarf_goff(unit, entry);

        while let Some(attr) = attrs.next()? {
            match (attr.name(), attr.value()) {
                (
                    gimli::constants::DW_AT_low_pc,
                    gimli::AttributeValue::Addr(addr),
                ) => {
                    low = Some(addr);
                }
                (
                    gimli::constants::DW_AT_high_pc,
                    gimli::AttributeValue::Udata(data),
                ) => {
                    high = Some(data);
                }
                (gimli::constants::DW_AT_abstract_origin, _) => {
                    origin = self.dwarf_value_goff(unit, &attr.value());
                }
                _ => {}
            }
        }

        match (low, high, origin) {
            (Some(addr), Some(len), Some(origin)) => {
                self.inlined
                    .insert((addr as u32, depth), (len as u32, goff, origin));

                return Ok(());
            }
            (None, None, Some(_)) => {}
            _ => {
                bail!("missing origin for {}", goff);
            }
        }

        let mut attrs = entry.attrs();
        while let Some(attr) = attrs.next()? {
            if let (
                gimli::constants::DW_AT_ranges,
                gimli::AttributeValue::RangeListsRef(r),
            ) = (attr.name(), attr.value())
            {
                let raw_ranges = dwarf.ranges.raw_ranges(r, unit.encoding())?;
                let raw_ranges: Vec<_> = raw_ranges.collect()?;

                for r in raw_ranges {
                    if let gimli::RawRngListEntry::AddressOrOffsetPair {
                        begin,
                        end,
                    } = r
                    {
                        let begin = begin as u32;
                        let end = end as u32;

                        self.inlined.insert(
                            (begin, depth),
                            (end - begin, goff, origin.unwrap()),
                        );
                    }
                }

                return Ok(());
            }
        }

        Err(anyhow!("missing address range for {}", goff))
    }

    fn dwarf_subprogram<'a, R: gimli::Reader<Offset = usize>>(
        &mut self,
        dwarf: &'a gimli::Dwarf<gimli::EndianSlice<gimli::LittleEndian>>,
        unit: &gimli::Unit<R>,
        entry: &gimli::DebuggingInformationEntry<
            gimli::EndianSlice<gimli::LittleEndian>,
            usize,
        >,
    ) -> Result<()> {
        let mut name = None;
        let mut _linkage_name = None;
        let mut addr = None;
        let mut len = None;

        let goff = self.dwarf_goff(unit, entry);

        // Iterate over the attributes in the DIE.
        let mut attrs = entry.attrs();
        while let Some(attr) = attrs.next()? {
            match (attr.name(), attr.value()) {
                (
                    gimli::constants::DW_AT_low_pc,
                    gimli::AttributeValue::Addr(value),
                ) => {
                    addr = Some(value);
                }
                (
                    gimli::constants::DW_AT_high_pc,
                    gimli::AttributeValue::Udata(value),
                ) => {
                    len = Some(value);
                }
                (gimli::constants::DW_AT_linkage_name, _) => {
                    _linkage_name = dwarf_name(dwarf, attr.value());
                }
                (gimli::constants::DW_AT_name, _) => {
                    name = dwarf_name(dwarf, attr.value());
                }
                _ => {}
            }
        }

        if let Some(name) = name {
            match (addr, len) {
                (Some(addr), Some(len)) if addr != 0 => {
                    self.dsyms.insert(
                        addr as u32,
                        (String::from(name), len as u32, goff),
                    );
                }
                _ => {}
            }

            self.subprograms.insert(goff, String::from(name));

            Ok(())
        } else {
            trace!("no name found for {}", goff);
            Ok(())
        }
    }

    fn dwarf_basetype<R: gimli::Reader<Offset = usize>>(
        &mut self,
        unit: &gimli::Unit<R>,
        entry: &gimli::DebuggingInformationEntry<
            gimli::EndianSlice<gimli::LittleEndian>,
            usize,
        >,
    ) -> Result<()> {
        let mut attrs = entry.attrs();
        let goff = self.dwarf_goff(unit, entry);
        let mut size = None;
        let mut encoding = None;

        while let Some(attr) = attrs.next()? {
            match attr.name() {
                gimli::constants::DW_AT_byte_size => {
                    if let gimli::AttributeValue::Udata(value) = attr.value() {
                        size = Some(value as usize)
                    }
                }
                gimli::constants::DW_AT_encoding => {
                    if let gimli::AttributeValue::Encoding(en) = attr.value() {
                        encoding = Some(match en {
                            gimli::constants::DW_ATE_signed => {
                                HubrisEncoding::Signed
                            }

                            gimli::constants::DW_ATE_unsigned => {
                                HubrisEncoding::Unsigned
                            }

                            gimli::constants::DW_ATE_float => {
                                HubrisEncoding::Float
                            }

                            gimli::constants::DW_ATE_boolean => {
                                HubrisEncoding::Bool
                            }

                            _ => HubrisEncoding::Unknown,
                        });
                    }
                }
                _ => {}
            }
        }

        if let (Some(size), Some(encoding)) = (size, encoding) {
            self.basetypes.insert(goff, HubrisBasetype { encoding, size });
        }

        Ok(())
    }

    fn dwarf_ptrtype<'a, R: gimli::Reader<Offset = usize>>(
        &mut self,
        dwarf: &'a gimli::Dwarf<gimli::EndianSlice<gimli::LittleEndian>>,
        unit: &gimli::Unit<R>,
        entry: &gimli::DebuggingInformationEntry<
            gimli::EndianSlice<gimli::LittleEndian>,
            usize,
        >,
    ) -> Result<()> {
        let mut attrs = entry.attrs();
        let goff = self.dwarf_goff(unit, entry);
        let mut underlying = None;
        let mut name = None;

        while let Some(attr) = attrs.next()? {
            match attr.name() {
                gimli::constants::DW_AT_type => {
                    underlying = self.dwarf_value_goff(unit, &attr.value());
                }
                gimli::constants::DW_AT_name => {
                    name = dwarf_name(dwarf, attr.value());
                }
                _ => {}
            }
        }

        if let (Some(name), Some(underlying)) = (name, underlying) {
            self.ptrtypes.insert(goff, (name.to_string(), underlying));
        }

        Ok(())
    }

    fn dwarf_array<R: gimli::Reader<Offset = usize>>(
        &mut self,
        unit: &gimli::Unit<R>,
        entry: &gimli::DebuggingInformationEntry<
            gimli::EndianSlice<gimli::LittleEndian>,
            usize,
        >,
        parent: HubrisGoff,
        array: Option<HubrisGoff>,
    ) -> Result<()> {
        let mut attrs = entry.attrs();
        let goff = self.dwarf_goff(unit, entry);
        let mut count = None;

        let array = match array {
            Some(array) => array,
            None => bail!("missing array type for subrange type {}", goff),
        };

        while let Some(attr) = attrs.next()? {
            if attr.name() == gimli::constants::DW_AT_count {
                count = attr.udata_value();
            }
        }

        if let Some(count) = count {
            self.arrays.insert(
                parent,
                HubrisArray { goff: array, count: count as usize },
            );
        }

        Ok(())
    }

    fn dwarf_variable<'a, R: gimli::Reader<Offset = usize>>(
        &mut self,
        dwarf: &'a gimli::Dwarf<gimli::EndianSlice<gimli::LittleEndian>>,
        unit: &gimli::Unit<R>,
        entry: &gimli::DebuggingInformationEntry<
            gimli::EndianSlice<gimli::LittleEndian>,
            usize,
        >,
    ) -> Result<()> {
        let mut attrs = entry.attrs();
        let goff = self.dwarf_goff(unit, entry);
        let mut name = None;
        let mut linkage_name = None;
        let mut external = None;
        let mut tgoff = None;

        while let Some(attr) = attrs.next()? {
            match attr.name() {
                gimli::constants::DW_AT_name => {
                    name = dwarf_name(dwarf, attr.value());
                }

                gimli::constants::DW_AT_external => {
                    if let gimli::AttributeValue::Flag(val) = attr.value() {
                        external = Some(val);
                    }
                }

                gimli::constants::DW_AT_linkage_name => {
                    linkage_name = dwarf_name(dwarf, attr.value());
                }

                gimli::constants::DW_AT_type => {
                    tgoff = self.dwarf_value_goff(unit, &attr.value());
                }
                _ => {}
            }
        }

        if external.is_none() && linkage_name.is_none() {
            return Ok(());
        }

        if let (Some(name), Some(tgoff)) = (name, tgoff) {
            let linkage = linkage_name.unwrap_or(name);

            if let Some(syms) = self.esyms_byname.get_vec(linkage) {
                for sym in syms {
                    if let btree_map::Entry::Vacant(e) = self.dsyms.entry(sym.0)
                    {
                        e.insert((String::from(name), sym.1, goff));
                        self.variables.insert(
                            String::from(name),
                            HubrisVariable {
                                goff: tgoff,
                                addr: sym.0,
                                size: sym.1 as usize,
                            },
                        );
                    }
                }
            } else {
                self.definitions.insert(String::from(name), tgoff);
            }
        }

        Ok(())
    }

    fn dwarf_struct<'a, R: gimli::Reader<Offset = usize>>(
        &mut self,
        dwarf: &'a gimli::Dwarf<gimli::EndianSlice<gimli::LittleEndian>>,
        unit: &gimli::Unit<R>,
        entry: &gimli::DebuggingInformationEntry<
            gimli::EndianSlice<gimli::LittleEndian>,
            usize,
        >,
    ) -> Result<()> {
        let mut attrs = entry.attrs();
        let goff = self.dwarf_goff(unit, entry);
        let mut name = None;
        let mut size = None;

        while let Some(attr) = attrs.next()? {
            match attr.name() {
                gimli::constants::DW_AT_name => {
                    name = dwarf_name(dwarf, attr.value());
                }

                gimli::constants::DW_AT_byte_size => {
                    if let gimli::AttributeValue::Udata(value) = attr.value() {
                        size = Some(value as usize)
                    }
                }

                _ => {}
            }
        }

        if let (Some(name), Some(size)) = (name, size) {
            self.structs.insert(
                goff,
                HubrisStruct {
                    name: name.to_string(),
                    size,
                    goff,
                    members: Vec::new(),
                },
            );

            self.structs_byname.insert(name.to_string(), goff);
        }

        Ok(())
    }

    fn dwarf_const_enum<'a, R: gimli::Reader<Offset = usize>>(
        &mut self,
        dwarf: &'a gimli::Dwarf<gimli::EndianSlice<gimli::LittleEndian>>,
        unit: &gimli::Unit<R>,
        entry: &gimli::DebuggingInformationEntry<
            gimli::EndianSlice<gimli::LittleEndian>,
            usize,
        >,
        goff: HubrisGoff,
    ) -> Result<()> {
        let mut attrs = entry.attrs();
        let mut name = None;
        let mut size = None;
        let mut dgoff = None;

        while let Some(attr) = attrs.next()? {
            match attr.name() {
                gimli::constants::DW_AT_name => {
                    name = dwarf_name(dwarf, attr.value());
                }

                gimli::constants::DW_AT_type => {
                    dgoff = self.dwarf_value_goff(unit, &attr.value());
                }

                gimli::constants::DW_AT_byte_size => {
                    if let gimli::AttributeValue::Udata(value) = attr.value() {
                        size = Some(value as usize)
                    }
                }

                _ => {}
            }
        }

        if let (Some(name), Some(dgoff), Some(size)) = (name, dgoff, size) {
            self.enums.insert(
                goff,
                HubrisEnum {
                    name: name.to_string(),
                    goff,
                    size,
                    discriminant: HubrisDiscriminant::Value(dgoff, 0),
                    tag: None,
                    variants: Vec::new(),
                },
            );

            self.enums_byname.insert(name.to_string(), goff);
            Ok(())
        } else {
            Err(anyhow!("missing enum on variant {}", goff))
        }
    }

    fn dwarf_enum_variant<'a, R: gimli::Reader<Offset = usize>>(
        &mut self,
        dwarf: &'a gimli::Dwarf<gimli::EndianSlice<gimli::LittleEndian>>,
        unit: &gimli::Unit<R>,
        entry: &gimli::DebuggingInformationEntry<
            gimli::EndianSlice<gimli::LittleEndian>,
            usize,
        >,
        parent: HubrisGoff,
    ) -> Result<()> {
        let goff = self.dwarf_goff(unit, entry);
        let mut attrs = entry.attrs();
        let mut value = None;
        let mut name = None;

        while let Some(attr) = attrs.next()? {
            match attr.name() {
                gimli::constants::DW_AT_name => {
                    name = dwarf_name(dwarf, attr.value());
                }

                gimli::constants::DW_AT_const_value => {
                    value = attr.value().udata_value();

                    if value.is_none() {
                        bail!("bad discriminant on const enum {}", parent);
                    }
                }

                _ => {}
            }
        }

        if let (Some(name), Some(value)) = (name, value) {
            if let Some(union) = self.enums.get_mut(&parent) {
                union.variants.push(HubrisEnumVariant {
                    name: name.to_string(),
                    offset: 0,
                    goff: None,
                    tag: Some(value),
                });

                Ok(())
            } else {
                Err(anyhow!("missing enum for variant {}", goff))
            }
        } else {
            Err(anyhow!("incomplete const enum variant {}", goff))
        }
    }

    fn dwarf_enum<R: gimli::Reader<Offset = usize>>(
        &mut self,
        unit: &gimli::Unit<R>,
        entry: &gimli::DebuggingInformationEntry<
            gimli::EndianSlice<gimli::LittleEndian>,
            usize,
        >,
        goff: HubrisGoff,
    ) -> Result<()> {
        let mut attrs = entry.attrs();
        let mut discr = None;

        /*
         * If we have an enum, we need to first remove it from our structures,
         * putting back any duplicate names that isn't this enum.
         */
        let union = self.structs.remove(&goff).unwrap();
        let mut removed = self.structs_byname.remove(&union.name).unwrap();
        removed.retain(|&g| g != goff);

        for replace in removed {
            self.structs_byname.insert(union.name.clone(), replace);
        }

        while let Some(attr) = attrs.next()? {
            if attr.name() == gimli::constants::DW_AT_discr {
                discr = self.dwarf_value_goff(unit, &attr.value());
            }
        }

        self.enums.insert(
            goff,
            HubrisEnum {
                name: union.name.clone(),
                goff,
                size: union.size,
                discriminant: match discr {
                    Some(discr) => HubrisDiscriminant::Expected(discr),
                    None => HubrisDiscriminant::None,
                },
                tag: None,
                variants: Vec::new(),
            },
        );

        self.enums_byname.insert(union.name, goff);

        Ok(())
    }

    fn dwarf_variant<R: gimli::Reader<Offset = usize>>(
        &mut self,
        unit: &gimli::Unit<R>,
        entry: &gimli::DebuggingInformationEntry<
            gimli::EndianSlice<gimli::LittleEndian>,
            usize,
        >,
        parent: HubrisGoff,
    ) -> Result<()> {
        let goff = self.dwarf_goff(unit, entry);
        let mut attrs = entry.attrs();
        let mut value = None;

        while let Some(attr) = attrs.next()? {
            if attr.name() == gimli::constants::DW_AT_discr_value {
                value = attr.value().udata_value();

                if value.is_none() {
                    bail!("bad discriminant on union {}", parent);
                }
            }
        }

        if let Some(union) = self.enums.get_mut(&parent) {
            union.tag = value;
            Ok(())
        } else {
            Err(anyhow!("missing enum on variant {}", goff))
        }
    }

    fn dwarf_union<'a, R: gimli::Reader<Offset = usize>>(
        &mut self,
        dwarf: &'a gimli::Dwarf<gimli::EndianSlice<gimli::LittleEndian>>,
        unit: &gimli::Unit<R>,
        entry: &gimli::DebuggingInformationEntry<
            gimli::EndianSlice<gimli::LittleEndian>,
            usize,
        >,
    ) -> Result<()> {
        let mut attrs = entry.attrs();
        let goff = self.dwarf_goff(unit, entry);
        let mut size = None;
        let mut name = None;

        while let Some(attr) = attrs.next()? {
            match attr.name() {
                gimli::constants::DW_AT_name => {
                    name = dwarf_name(dwarf, attr.value());
                }

                gimli::constants::DW_AT_byte_size => {
                    if let gimli::AttributeValue::Udata(value) = attr.value() {
                        size = Some(value as usize)
                    }
                }
                _ => {}
            }
        }

        if let (Some(name), Some(size)) = (name, size) {
            self.unions.insert(
                goff,
                HubrisUnion {
                    name: name.to_string(),
                    goff,
                    size,
                    variants: Vec::new(),
                },
            );
        }

        Ok(())
    }

    fn dwarf_member<'a, R: gimli::Reader<Offset = usize>>(
        &mut self,
        dwarf: &'a gimli::Dwarf<gimli::EndianSlice<gimli::LittleEndian>>,
        unit: &gimli::Unit<R>,
        entry: &gimli::DebuggingInformationEntry<
            gimli::EndianSlice<gimli::LittleEndian>,
            usize,
        >,
        parent: HubrisGoff,
    ) -> Result<()> {
        let mut attrs = entry.attrs();
        let mut name = None;
        let mut offset = None;
        let mut goff = None;
        let member = self.dwarf_goff(unit, entry);

        while let Some(attr) = attrs.next()? {
            match attr.name() {
                gimli::constants::DW_AT_name => {
                    name = dwarf_name(dwarf, attr.value());
                }

                gimli::constants::DW_AT_data_member_location => {
                    if let gimli::AttributeValue::Udata(value) = attr.value() {
                        offset = Some(value as usize)
                    }
                }

                gimli::constants::DW_AT_type => {
                    goff = self.dwarf_value_goff(unit, &attr.value());
                }

                _ => {}
            }
        }

        if let Some(pstruct) = self.structs.get_mut(&parent) {
            if let (Some(n), Some(offs), Some(g)) = (name, offset, goff) {
                pstruct.members.push(HubrisStructMember {
                    name: n.to_string(),
                    offset: offs,
                    goff: g,
                });
            } else {
                bail!("member {} is incomplete", member);
            }
        } else if let Some(union) = self.enums.get_mut(&parent) {
            if let HubrisDiscriminant::Expected(expect) = union.discriminant {
                if member != expect {
                    bail!("enum {}: expected discriminant {}, found {}",
                        union.goff, expect, member);
                }

                if let (Some(offs), Some(g)) = (offset, goff) {
                    union.discriminant = HubrisDiscriminant::Value(g, offs);
                    return Ok(());
                }

                bail!("enum {}: incomplete discriminant", union.goff);
            }

            /*
             * We have an enum variant; add it to our variants.
             */
            if let (Some(n), Some(offs), Some(g)) = (name, offset, goff) {
                union.variants.push(HubrisEnumVariant {
                    name: n.to_string(),
                    offset: offs,
                    goff: Some(g),
                    tag: union.tag,
                });

                union.tag = None;
            } else {
                bail!("enum variant {} is incomplete", member);
            }
        } else if let Some(union) = self.unions.get_mut(&parent) {
            /*
             * This is possible because of Rust's (unsafe-only) support for
             * C-style unions.  We track these because some structures are
             * implemented in terms of them (in particular, MaybeUninit).
             */
            if let (Some(n), Some(offs), Some(g)) = (name, offset, goff) {
                union.variants.push(HubrisEnumVariant {
                    name: n.to_string(),
                    offset: offs,
                    goff: Some(g),
                    tag: None,
                });
            } else {
                bail!("union {} is incomplete", parent);
            }
        } else {
            trace!("no struct/enum found for {}", parent);
        }

        Ok(())
    }

    fn load_object_dwarf(
        &mut self,
        buffer: &[u8],
        elf: &goblin::elf::Elf,
    ) -> Result<()> {
        // Load all of the sections. This "load" operation just gets the data in
        // RAM -- since we've already loaded the Elf file, this can't fail.
        let dwarf = gimli::Dwarf::<&[u8]>::load::<_, _, Infallible>(
            // Load the normal DWARF section(s) from our Elf image.
            |id| {
                let sec_result = elf.section_headers.iter().find(|sh| {
                    if let Some(Ok(name)) = elf.shdr_strtab.get(sh.sh_name) {
                        name == id.name()
                    } else {
                        false
                    }
                });
                Ok(sec_result
                    .map(|sec| {
                        let offset = sec.sh_offset as usize;
                        let size = sec.sh_size as usize;
                        buffer.get(offset..offset + size).unwrap()
                    })
                    .unwrap_or(&[]))
            },
            // We don't have a supplemental object file.
            |_| Ok(&[]),
        )?;

        // Borrow all sections wrapped in EndianSlices
        let dwarf = dwarf.borrow(|section| {
            gimli::EndianSlice::new(section, gimli::LittleEndian)
        });

        // Iterate over the compilation units.
        let mut iter = dwarf.units();
        while let Some(header) = iter.next()? {
            let unit = dwarf.unit(header)?;
            let mut entries = unit.entries();
            let mut depth = 0;
            let mut stack: Vec<HubrisGoff> = vec![];

            let mut array = None;

            while let Some((delta, entry)) = entries.next_dfs()? {
                depth += delta;

                let goff = self.dwarf_goff(&unit, entry);
                self.dwarf_fileline(&dwarf, &unit, entry)?;

                if depth as usize >= stack.len() {
                    stack.push(goff);
                } else {
                    stack[depth as usize] = goff;
                }

                match entry.tag() {
                    gimli::constants::DW_TAG_inlined_subroutine => {
                        self.dwarf_inlined(&dwarf, &unit, entry, depth)?;
                    }

                    gimli::constants::DW_TAG_subprogram => {
                        self.dwarf_subprogram(&dwarf, &unit, entry)?;
                    }

                    gimli::constants::DW_TAG_variable => {
                        self.dwarf_variable(&dwarf, &unit, entry)?;
                    }

                    gimli::constants::DW_TAG_structure_type => {
                        self.dwarf_struct(&dwarf, &unit, entry)?;
                    }

                    gimli::constants::DW_TAG_base_type => {
                        self.dwarf_basetype(&unit, entry)?;
                    }

                    gimli::constants::DW_TAG_pointer_type => {
                        self.dwarf_ptrtype(&dwarf, &unit, entry)?;
                    }

                    gimli::constants::DW_TAG_array_type => {
                        let mut attrs = entry.attrs();

                        while let Some(attr) = attrs.next()? {
                            if attr.name() != gimli::constants::DW_AT_type {
                                continue;
                            }

                            array = self.dwarf_value_goff(&unit, &attr.value());
                            break;
                        }
                    }

                    gimli::constants::DW_TAG_subrange_type => {
                        if depth == 0 {
                            bail!("no array for subrange {}", goff);
                        }

                        let parent = stack[depth as usize - 1];
                        self.dwarf_array(&unit, entry, parent, array)?;
                        array = None;
                    }

                    gimli::constants::DW_TAG_enumeration_type => {
                        self.dwarf_const_enum(&dwarf, &unit, entry, goff)?;
                    }

                    gimli::constants::DW_TAG_enumerator => {
                        let parent = stack[depth as usize - 1];

                        self.dwarf_enum_variant(&dwarf, &unit, entry, parent)?;
                    }

                    gimli::constants::DW_TAG_variant_part => {
                        if depth == 0 {
                            bail!("no enum for variant {}", goff);
                        }

                        let parent = stack[depth as usize - 1];
                        self.dwarf_enum(&unit, entry, parent)?;

                        /*
                         * The discriminant is a (grand)child member; we need
                         * to duplicate our parent's goff so our child can find
                         * it.
                         */
                        stack[depth as usize] = parent;
                    }

                    gimli::constants::DW_TAG_variant => {
                        if depth == 0 {
                            bail!("no enum for variant {}", goff);
                        }

                        let parent = stack[depth as usize - 1];
                        self.dwarf_variant(&unit, entry, parent)?;

                        /*
                         * Our discriminant is still below us as a child
                         * member, so, as in the DW_TAG_variant_part case
                         * (which is our parent), we need to copy our parent
                         * down.
                         */
                        stack[depth as usize] = parent;
                    }

                    gimli::constants::DW_TAG_member => {
                        if depth == 0 {
                            bail!("no parent for member {}", goff);
                        }

                        let parent = stack[depth as usize - 1];
                        self.dwarf_member(&dwarf, &unit, entry, parent)?;
                    }

                    gimli::constants::DW_TAG_union_type => {
                        self.dwarf_union(&dwarf, &unit, entry)?;
                    }

                    _ => {}
                }
            }

            if let Some(array) = array {
                bail!("missing subrange for array {}", array);
            }
        }

        self.current += 1;

        Ok(())
    }

    fn load_object_frames(
        &mut self,
        task: HubrisTask,
        buffer: &[u8],
        elf: &goblin::elf::Elf,
    ) -> Result<()> {
        let id = gimli::SectionId::DebugFrame.name();

        let sh = elf
            .section_headers
            .iter()
            .find(|sh| {
                if let Some(Ok(name)) = elf.shdr_strtab.get(sh.sh_name) {
                    name == id
                } else {
                    false
                }
            })
            .ok_or_else(|| anyhow!("couldn't find {}", id))?;

        let offs = sh.sh_offset as usize;
        let size = sh.sh_size as usize;

        let buf = buffer.get(offs..offs + size).unwrap();
        self.frames.insert(task, buf.to_vec());

        Ok(())
    }

    fn load_object(
        &mut self,
        object: &str,
        task: HubrisTask,
        buffer: &[u8],
    ) -> Result<()> {
        let mut heapbss = (None, None);

        let elf = Elf::parse(buffer).map_err(|e| {
            anyhow!("unrecognized ELF object: {}: {}", object, e)
        })?;

        let arm = elf.header.e_machine == goblin::elf::header::EM_ARM;

        if !arm {
            bail!("{} not an ARM ELF object", object);
        }

        for sym in elf.syms.iter() {
            if sym.st_name == 0 {
                continue;
            }

            let name = match elf.strtab.get(sym.st_name) {
                Some(n) => n?,
                None => {
                    bail!("bad symbol in {}: {}", object, sym.st_name);
                }
            };

            //
            // We track from the start of our BSS to the end of our heap
            //
            if name == "__sbss" {
                heapbss.0 = Some(sym.st_value as u32);
            }

            if name == "__eheap" {
                heapbss.1 = Some(sym.st_value as u32);
            }

            if sym.st_size == 0 {
                continue;
            }

            /*
             * On ARM, we must explicitly clear the low bit of the symbol
             * table, which exists only to indicate a function that contains
             * Thumb instructions (which is of course every function on a
             * microprocessor that executes only Thumb instructions).
             */
            assert!(arm);

            let val = if sym.is_function() {
                sym.st_value as u32 & !1
            } else {
                sym.st_value as u32
            };

            let dem = format!("{:#}", demangle(name));

            self.esyms_byname
                .insert(name.to_string(), (val, sym.st_size as u32));
            self.esyms.insert(val, (dem, sym.st_size as u32));
        }

        use goblin::elf::program_header::{PF_R, PF_W, PF_X};

        elf.program_headers
            .iter()
            .filter(|h| h.p_type == goblin::elf::program_header::PT_LOAD)
            .map(|h| HubrisRegion {
                daddr: None,
                base: h.p_vaddr as u32,
                size: h.p_memsz as u32,
                mapsize: h.p_memsz as u32,
                attr: HubrisRegionAttr {
                    read: h.p_flags & PF_R != 0,
                    write: h.p_flags & PF_W != 0,
                    execute: h.p_flags & PF_X != 0,
                    device: false,
                    dma: false,
                },
                task,
            })
            .for_each(|region| {
                self.loaded.insert(region.base, region);
            });

        let memsz = elf.program_headers.iter().fold(0, |ttl, hdr| {
            if hdr.p_type == goblin::elf::program_header::PT_LOAD {
                ttl + hdr.p_memsz
            } else {
                ttl
            }
        });

        if task == HubrisTask::Kernel {
            let apptable = elf.section_headers.iter().find(|sh| {
                if let Some(Ok(name)) = elf.shdr_strtab.get(sh.sh_name) {
                    name == ".hubris_app_table"
                } else {
                    false
                }
            });

            self.apptable = match apptable {
                None => {
                    bail!("kernel is missing .hubris_app_table");
                }
                Some(sec) => {
                    let base = sec.sh_offset as usize;
                    let len = sec.sh_size as usize;

                    (sec.sh_addr as u32, buffer[base..base + len].to_vec())
                }
            };
        }

        let text = elf.section_headers.iter().find(|sh| {
            if let Some(Ok(name)) = elf.shdr_strtab.get(sh.sh_name) {
                name == ".text"
            } else {
                false
            }
        });

        let textsec = match text {
            None => {
                bail!("couldn't find text in ELF object \"{}\"", object);
            }
            Some(sec) => sec,
        };

        let offset = textsec.sh_offset as usize;
        let size = textsec.sh_size as usize;

        trace!("loading {} as object {}", object, self.current);

        self.load_object_dwarf(buffer, &elf)
            .context(format!("{}: failed to load DWARF", object))?;

        self.load_object_frames(task, buffer, &elf)
            .context(format!("{}: failed to load debug frames", object))?;

        let t = buffer.get(offset..offset + size).unwrap();

        let instrs = match self.cs.disasm_all(t, textsec.sh_addr) {
            Ok(instrs) => instrs,
            Err(err) => {
                bail!(
                    "failed to disassemble \"{}\" (offs 0x{:08x}, {}): {}",
                    object, offset, size, err
                );
            }
        };

        let mut last: (u32, usize) = (0, 0);
        let mut lastpush = None;

        for instr in instrs.iter() {
            let addr: u32 = instr.address() as u32;

            if self.instrs.contains_key(&addr) {
                bail!("address 0x{:08x} is duplicated!", addr);
            }

            let b = instr.bytes();
            let mut v = Vec::with_capacity(b.len());

            for byte in b {
                v.push(*byte);
            }

            last = (addr, b.len());

            let target = self.instr_branch_target(&instr);
            self.instrs.insert(addr, (v, target));

            //
            // We need to keep track of the hand-written push before any
            // system call in order to be able to successfully unwind the
            // stack.
            //
            const ARM_INSN_PUSH: u32 = arch::arm::ArmInsn::ARM_INS_PUSH as u32;
            const ARM_INSN_SVC: u32 = arch::arm::ArmInsn::ARM_INS_SVC as u32;

            match instr.id() {
                InsnId(ARM_INSN_PUSH) => {
                    lastpush = Some(self.instr_operands(&instr));
                }
                InsnId(ARM_INSN_SVC) => {
                    self.syscall_pushes.insert(addr + b.len() as u32, lastpush);
                    lastpush = None;
                }
                _ => {}
            }
        }

        /*
         * Regrettably, if Capstone flies off the rails while disassembling,
         * it won't flag an error -- it will simply stop short.  Check to see
         * if we are in this case and explicitly fail.
         */
        if last.0 + last.1 as u32 != textsec.sh_addr as u32 + size as u32 {
            bail!(
                concat!(
                    "short disassembly for \"{}\": ",
                    "stopped at 0x{:x}, expected to go to 0x{:x}"
                ),
                object, last.0, textsec.sh_addr as u32 + size as u32
            );
        }

        self.modules.insert(
            textsec.sh_addr as u32,
            HubrisModule {
                name: String::from(object),
                object: self.current,
                textbase: (textsec.sh_addr as u32),
                textsize: size as u32,
                memsize: memsz as u32,
                heapbss,
                task,
            },
        );

        self.tasks.insert(object.to_string(), task);

        Ok(())
    }

    fn load_config(&mut self, toml: &toml::value::Table) -> Result<()> {
        let mut manifest = &mut self.manifest;

        let conf = |member| {
            toml.get(member).and_then(|m| m.as_str()).map(|s| s.to_string())
        };

        manifest.board = conf("board");
        manifest.target = conf("target");

        /*
         * It can be useful to see which features the kernel was compiled with,
         * so we go fishing for that explicitly.
         */
        let features = toml.get("kernel").and_then(|k| k.get("features"));

        if let Some(toml::Value::Array(ref features)) = features {
            manifest.features = features
                .iter()
                .map(|f| f.as_str().unwrap().to_string())
                .collect::<Vec<String>>();
        }

        /*
         * Now we want to iterate over our tasks to discover their features as
         * well.
         */
        if let Some(toml::Value::Table(tasks)) = toml.get("tasks") {
            for (task, config) in tasks {
                let features = config.get("features");

                if let Some(toml::Value::Array(ref features)) = features {
                    manifest.task_features.insert(
                        task.to_string(),
                        features
                            .iter()
                            .map(|f| f.as_str().unwrap().to_string())
                            .collect::<Vec<String>>(),
                    );
                }

                let irqs = config.get("interrupts");

                if let Some(toml::Value::Table(irqs)) = irqs {
                    manifest.task_irqs.insert(
                        task.to_string(),
                        irqs.iter()
                            .map(|(n, m)| {
                                (
                                    m.as_integer().unwrap() as u32,
                                    n.parse::<u32>().unwrap(),
                                )
                            })
                            .collect::<Vec<_>>(),
                    );
                }
            }
        }

        if let Some(toml::Value::Table(ref outputs)) = toml.get("outputs") {
            for (output, config) in outputs.into_iter() {
                let address = config.get("address");
                let size = config.get("size");

                match (address, size) {
                    (
                        Some(toml::Value::Integer(ref address)),
                        Some(toml::Value::Integer(ref size)),
                    ) => {
                        manifest.outputs.insert(
                            output.to_string(),
                            (*address as u32, *size as u32),
                        );
                    }
                    _ => {
                        bail!("manifest outputs malformed for \"{}\"", output);
                    }
                }
            }
        } else {
            bail!("manifest is missing outputs");
        }

        if let Some(toml::Value::Table(ref p)) = toml.get("peripherals") {
            for (peripheral, config) in p.into_iter() {
                let address = config.get("address");

                if let Some(toml::Value::Integer(ref address)) = address {
                    manifest
                        .peripherals
                        .insert(peripheral.to_string(), *address as u32);
                    continue;
                }

                bail!("manifest peripherals malformed for \"{}\"", peripheral);
            }
        }

        Ok(())
    }

    fn load_archive(&mut self, archive: &[u8]) -> Result<()> {
        let cursor = Cursor::new(archive);
        let mut archive = zip::ZipArchive::new(cursor)?;
        let mut manifest = &mut self.manifest;

        macro_rules! byname {
            ($name:tt) => {
                archive
                    .by_name($name)
                    .map_err(|e| anyhow!("failed to find \"{}\": {}", $name, e))
            };
        }

        /*
         * First, we'll load aspects of configuration.
         */
        manifest.version = Some(str::from_utf8(archive.comment())?.to_string());

        if let Ok(mut file) = archive.by_name("git-rev") {
            let mut gitrev = String::new();
            file.read_to_string(&mut gitrev)?;
            manifest.gitrev = Some(gitrev);
        }

        let mut app = String::new();
        byname!("app.toml")?.read_to_string(&mut app)?;

        match app.parse::<toml::Value>() {
            Ok(toml::Value::Table(ref toml)) => {
                self.load_config(toml)?;
            }
            Ok(_) => {
                bail!("app.toml valid TOML but malformed");
            }
            Err(err) => {
                bail!("failed to parse app.toml: {}", err);
            }
        }

        /*
         * Next up is the kernel.  Note that we refer to it explicitly with a
         * forward slash: regardless of platform, paths within a ZIP archive
         * use the forward slash as a separator.
         */
        let mut buffer = Vec::new();
        byname!("elf/kernel")?.read_to_end(&mut buffer)?;
        self.load_object("kernel", HubrisTask::Kernel, &buffer)?;

        let mut id = 0;

        /*
         * And now we need to find the tasks.  Note that we depend on the fact
         * that these are stored in task ID order in the archive.
         */
        for i in 0..archive.len() {
            let mut file = archive.by_index(i)?;
            let path = Path::new(file.name());
            let pieces = path.iter().collect::<Vec<_>>();

            /*
             * If the second-to-last element of our path is "task", we have a
             * winner!
             */
            if pieces.len() < 2 || pieces[pieces.len() - 2] != "task" {
                continue;
            }

            let object = match pieces[pieces.len() - 1].to_str() {
                Some(ref str) => str.to_string(),
                None => {
                    bail!("bad object name for \"{}\"", file.name());
                }
            };

            let mut buffer = Vec::new();
            file.read_to_end(&mut buffer)?;
            self.load_object(&object, HubrisTask::Task(id), &buffer)?;
            id += 1;
        }

        Ok(())
    }

    pub fn load(&mut self, archive: &str) -> Result<()> {
        let metadata = fs::metadata(archive)?;

        if metadata.is_dir() {
            bail!("a directory as an archive is deprecated; \
                use archive instead");
        }

        let mut file = fs::File::open(archive)?;

        /*
         * We read the entire archive into memory (and hold onto it) -- we are
         * going to need most of it anyway, and we want to have the entire
         * archive in memory to be able to write it out to any generated dump.
         */
        let mut contents = Vec::new();
        file.read_to_end(&mut contents)?;

        self.load_archive(&contents)?;
        self.archive = contents;

        Ok(())
    }

    fn load_registers(&mut self, r: &[u8]) -> Result<()> {
        if r.len() % 8 != 0 {
            bail!("bad length {} in registers note", r.len());
        }

        let nregs = r.len() / 8;

        for i in 0..nregs {
            let o = i * 8;

            let id = u32::from_le_bytes(r[o..o + 4].try_into()?);
            let val = u32::from_le_bytes(r[o + 4..o + 8].try_into()?);

            let reg = match ARMRegister::from_u32(id) {
                Some(r) => r,
                None => {
                    bail!("illegal register 0x{:x} at offset {}", id, o);
                }
            };

            if self.registers.insert(reg, val).is_some() {
                bail!("duplicate register {} ({}) at offset {}", reg, id, o);
            }
        }

        Ok(())
    }

    pub fn load_dump(&mut self, dumpfile: &str) -> Result<()> {
        /*
         * We expect the dump to be an ELF core dump.
         */
        let mut file = fs::File::open(dumpfile)?;

        let mut contents = Vec::new();
        file.read_to_end(&mut contents)?;

        let elf = Elf::parse(&contents).map_err(|e| {
            anyhow!("failed to parse {} as an ELF file: {}", dumpfile, e)
        })?;

        if let Some(notes) = elf.iter_note_headers(&contents) {
            for note in notes {
                match note {
                    Ok(note) => {
                        if note.name != OXIDE_NT_NAME {
                            continue;
                        }

                        match note.n_type {
                            OXIDE_NT_HUBRIS_ARCHIVE => {
                                self.load_archive(note.desc)?;
                            }
                            OXIDE_NT_HUBRIS_REGISTERS => {
                                self.load_registers(note.desc)?;
                            }
                            _ => {
                                bail!("unrecognized note 0x{:x}", note.n_type);
                            }
                        }
                    }
                    Err(e) => {
                        bail!("failed to parse note: {}", e);
                    }
                }
            }
        }

        Ok(())
    }

    /// Load *only* a kernel -- which itself is only useful when operating on
    /// a file and not an archive.  This will fail if an archive has already
    /// been loaded.
    pub fn load_kernel(&mut self, kernel: &str) -> Result<()> {
        if !self.modules.is_empty() {
            bail!("cannot specify both an archive and a kernel");
        }

        let mut file = fs::File::open(kernel)?;

        let mut contents = Vec::new();
        file.read_to_end(&mut contents)?;

        self.load_object("kernel", HubrisTask::Kernel, &contents)?;

        Ok(())
    }

    pub fn loaded(&self) -> bool {
        !self.modules.is_empty()
    }

    ///
    /// Looks up the specfied structure.  This returns a Result and not an
    /// Option because the assumption is that the structure is needed to be
    /// present, and be present exactly once.  If needed structures begin
    /// having their names duplicated in modules, we may need to support
    /// proper namespacing -- or kludgey namespacing...
    ///
    pub fn lookup_struct_byname(&self, name: &str) -> Result<&HubrisStruct> {
        match self.structs_byname.get_vec(name) {
            Some(v) => {
                if v.len() > 1 {
                    Err(anyhow!("{} matches more than one structure", name))
                } else {
                    Ok(self.structs.get(&v[0]).unwrap())
                }
            }
            None => Err(anyhow!("expected structure {} not found", name)),
        }
    }

    pub fn lookup_type(&self, goff: HubrisGoff) -> Result<HubrisType> {
        let r = self
            .lookup_struct(goff)
            .map(HubrisType::Struct)
            .or_else(|_| self.lookup_enum(goff).map(HubrisType::Enum))
            .or_else(|_| self.lookup_array(goff).map(HubrisType::Array))
            .or_else(|_| self.lookup_union(goff).map(HubrisType::Union))
            .or_else(|_| self.lookup_basetype(goff).map(HubrisType::Base))
            .or_else(|_| self.lookup_ptrtype(goff).map(HubrisType::Ptr))?;
        Ok(r)
    }

    pub fn lookup_struct(&self, goff: HubrisGoff) -> Result<&HubrisStruct> {
        match self.structs.get(&goff) {
            Some(s) => Ok(s),
            None => Err(anyhow!("expected struct {} not found", goff)),
        }
    }

    pub fn lookup_enum(&self, goff: HubrisGoff) -> Result<&HubrisEnum> {
        match self.enums.get(&goff) {
            Some(union) => Ok(union),
            None => Err(anyhow!("expected enum {} not found", goff)),
        }
    }

    pub fn lookup_union(&self, goff: HubrisGoff) -> Result<&HubrisUnion> {
        match self.unions.get(&goff) {
            Some(union) => Ok(union),
            None => Err(anyhow!("expected union {} not found", goff)),
        }
    }

    pub fn lookup_array(&self, goff: HubrisGoff) -> Result<&HubrisArray> {
        match self.arrays.get(&goff) {
            Some(array) => Ok(array),
            None => Err(anyhow!("expected {} to be an array", goff)),
        }
    }

    pub fn lookup_basetype(&self, goff: HubrisGoff) -> Result<&HubrisBasetype> {
        match self.basetypes.get(&goff) {
            Some(basetype) => Ok(basetype),
            None => Err(anyhow!("expected {} to be a basetype", goff)),
        }
    }

    pub fn lookup_ptrtype(&self, goff: HubrisGoff) -> Result<HubrisGoff> {
        match self.ptrtypes.get(&goff) {
            Some((_name, ptr)) => Ok(*ptr),
            None => Err(anyhow!("pointer type {} not found", goff)),
        }
    }

    ///
    /// Looks up the specified symbol.  This is more of a convenience routine
    /// that turns an Option into a Result.
    ///
    pub fn lookup_symword(&self, name: &str) -> Result<u32> {
        match self.esyms_byname.get(name) {
            Some(sym) => {
                if sym.1 != 4 {
                    Err(anyhow!("symbol {} is not word-sized", name))
                } else {
                    Ok(sym.0)
                }
            }
            None => Err(anyhow!("expected symbol {} not found", name)),
        }
    }

    pub fn lookup_variable(&self, name: &str) -> Result<&HubrisVariable> {
        match self.variables.get(name) {
            Some(variable) => Ok(variable),
            None => Err(anyhow!("variable {} not found", name)),
        }
    }

    pub fn lookup_variables(&self, name: &str) -> Result<&Vec<HubrisVariable>> {
        match self.variables.get_vec(name) {
            None => Err(anyhow!("variable {} not found", name)),
            Some(variables) => Ok(variables),
        }
    }

    pub fn lookup_definition(&self, name: &str) -> Result<&HubrisGoff> {
        match self.definitions.get(name) {
            Some(goff) => Ok(goff),
            None => Err(anyhow!("definition {} not found", name)),
        }
    }

    pub fn variables(&self) -> Vec<(&String, &HubrisVariable)> {
        let mut variables = vec![];

        for (name, variable) in &self.variables {
            for v in variable {
                variables.push((name, v));
            }
        }

        variables
    }

    pub fn lookup_module(&self, task: HubrisTask) -> Result<&HubrisModule> {
        match self.modules.values().find(|m| m.task == task) {
            Some(module) => Ok(module),
            None => Err(anyhow!("no such task: {}", task)),
        }
    }

    pub fn lookup_task(&self, name: &str) -> Option<&HubrisTask> {
        self.tasks.get(name)
    }

    pub fn task_name(&self, index: usize) -> Option<&str> {
        let index = HubrisTask::Task(index as u32);
        // TODO this is super gross but we don't have the inverse of the tasks
        // mapping at the moment.
        self.tasks.iter().find(|(_, &i)| i == index).map(|(name, _)| &**name)
    }

    pub fn lookup_src(&self, goff: HubrisGoff) -> Option<&HubrisSrc> {
        self.src.get(&goff)
    }

    fn ntasks(&self) -> usize {
        if self.current >= 1 {
            self.current as usize - 1
        } else {
            0
        }
    }

    pub fn validate(
        &self,
        core: &mut dyn crate::core::Core,
        criteria: HubrisValidate,
    ) -> Result<()> {
        let ntasks = self.ntasks();

        if self.current == 0 {
            /*
             * If we have no objects, we were never loaded -- and we consider
             * this to be validated because it will give no answers rather than
             * wrong ones.
             */
            return Ok(());
        }

        let addr = self.apptable.0;
        let nbytes = self.apptable.1.len();
        assert!(nbytes > 0);

        let mut apptable = vec![0; nbytes];
        core.read_8(addr, &mut apptable[0..nbytes]).context(format!(
            "failed to read .hubris_app_table at 0x{:x}; board mismatch?",
            addr
        ))?;

        let deltas = apptable
            .iter()
            .zip(self.apptable.1.iter())
            .filter(|&(lhs, rhs)| lhs != rhs)
            .count();

        if deltas > 0 || apptable.len() != self.apptable.1.len() {
            bail!("apptable at 0x{:x} does not match archive apptable", addr);
        }

        if criteria == HubrisValidate::ArchiveMatch {
            return Ok(());
        }

        let n = core.read_word_32(self.lookup_symword("TASK_TABLE_SIZE")?)?;

        if n == ntasks as u32 {
            return Ok(());
        }

        /*
         * We appear to not have booted, so we're going to fail -- but let's
         * see if it's the common case of being actually in Reset itself to
         * give a more certain message.
         */
        if let Some(sym) = self.esyms_byname.get("Reset") {
            if let Ok(pc) = core.read_reg(ARMRegister::PC) {
                if pc >= sym.0 && pc < sym.0 + sym.1 {
                    bail!("target is not yet booted (currently in Reset)");
                }
            }
        }

        bail!("target does not appear to be booted");
    }

    pub fn member_offset(
        &self,
        structure: &HubrisStruct,
        member: &str,
    ) -> Result<u32> {
        let mut s = structure;
        let mut offset = 0;

        let fields: Vec<&str> = member.split('.').collect();

        for i in 0..fields.len() {
            let field = fields[i];

            let m = match s.lookup_member(field) {
                Ok(member) => member,
                _ => {
                    return Err(anyhow!("struct {} ({}) doesn't contain {}",
                        s.name, s.goff, field))
                }
            };

            offset += m.offset;

            if i == fields.len() - 1 {
                /*
                 * We want to make sure that this is a 32-bit basetype or a
                 * ptrtype.
                 */
                if let Some(v) = self.basetypes.get(&m.goff) {
                    if v.size != 4 {
                        return Err(anyhow!(
                            "expected {} in struct {} ({}) to \
                            be 4 bytes, found to be {} bytes",
                            member, structure.name, structure.goff, v.size
                        ));
                    }
                } else if self.ptrtypes.contains_key(&m.goff) {
                    break;
                } else {
                    return Err(anyhow!(
                        "expected {} in struct {} ({}) to \
                        be 4 byte type, found to be {}",
                        member, structure.name, structure.goff, m.goff
                    ));
                }

                break;
            }

            /*
             * We need to descend -- make sure that this is a structure!
             */
            s = match self.lookup_struct(m.goff) {
                Ok(structure) => structure,
                Err(_) => {
                    return Err(anyhow!(
                        "struct {} ({}) doesn't contain {}: \
                        non-structure at {} ({})",
                        structure.name, structure.goff, member, field, m.goff
                    ));
                }
            }
        }

        Ok(offset as u32)
    }

    pub fn regions(
        &self,
        core: &mut dyn crate::core::Core,
    ) -> Result<BTreeMap<u32, HubrisRegion>> {
        let task_table_base = self.lookup_symword("TASK_TABLE_BASE")?;
        let base = core.read_word_32(task_table_base)?;
        let task = self.lookup_struct_byname("Task")?;
        let desc = self.lookup_struct_byname("RegionDesc")?;

        let ptr_offs = self.member_offset(task, "region_table.data_ptr")?;
        let len_offs = self.member_offset(task, "region_table.length")?;

        let base_offs = self.member_offset(desc, "base")?;
        let size_offs = self.member_offset(desc, "size")?;
        let attr_offs = self.member_offset(desc, "attributes.bits")?;

        /*
         * Regrettably copied out of Hubris -- there isn't DWARF for this.
         */
        const READ: u32 = 1 << 0;
        const WRITE: u32 = 1 << 1;
        const EXECUTE: u32 = 1 << 2;
        const DEVICE: u32 = 1 << 3;
        const DMA: u32 = 1 << 4;

        let mut regions: BTreeMap<u32, HubrisRegion> = BTreeMap::new();

        /*
         * Add our loaded kernel regions, which don't otherwise have
         * descriptors.
         */
        for region in self.loaded.values() {
            if region.task == HubrisTask::Kernel {
                regions.insert(region.base, *region);
            }
        }

        /*
         * Add a region for our kernel heap+bss, for which we don't have a
         * descriptor.
         */
        for module in
            self.modules.values().filter(|m| m.task == HubrisTask::Kernel)
        {
            if let (Some(sheapbss), Some(eheapbss)) = module.heapbss {
                regions.insert(
                    sheapbss,
                    HubrisRegion {
                        daddr: None,
                        base: sheapbss,
                        size: eheapbss - sheapbss,
                        mapsize: eheapbss - sheapbss,
                        attr: HubrisRegionAttr {
                            read: true,
                            write: true,
                            execute: false,
                            device: false,
                            dma: false,
                        },
                        task: HubrisTask::Kernel,
                    },
                );
            }
        }

        /*
         * Iterate over all tasks, reading their region descriptors.
         */
        for i in 0..self.ntasks() {
            let addr = base + i as u32 * task.size as u32;

            let ptr = core.read_word_32(addr + ptr_offs)?;
            let len = core.read_word_32(addr + len_offs)?;

            for j in 0..len {
                let daddr = core.read_word_32(ptr + j * 4)?;
                let base = core.read_word_32(daddr + base_offs)?;
                let size = core.read_word_32(daddr + size_offs)?;
                let attr = core.read_word_32(daddr + attr_offs)?;

                if base == 0 {
                    continue;
                }

                regions.insert(
                    base,
                    HubrisRegion {
                        daddr: Some(daddr),
                        base,
                        size: if attr & WRITE != 0 {
                            size
                        } else {
                            match self.loaded.get(&base) {
                                Some(region) => region.size,
                                None => size,
                            }
                        },
                        mapsize: size,
                        attr: HubrisRegionAttr {
                            read: attr & READ != 0,
                            write: attr & WRITE != 0,
                            execute: attr & EXECUTE != 0,
                            device: attr & DEVICE != 0,
                            dma: attr & DMA != 0,
                        },
                        task: HubrisTask::Task(i as u32),
                    },
                );
            }
        }

        Ok(regions)
    }

    pub fn dump_registers(&self) -> HashMap<ARMRegister, u32> {
        self.registers.clone()
    }

    pub fn registers(
        &self,
        core: &mut dyn crate::core::Core,
        t: HubrisTask,
    ) -> Result<HashMap<ARMRegister, u32>> {
        let base =
            core.read_word_32(self.lookup_symword("TASK_TABLE_BASE")?)?;
        let cur =
            core.read_word_32(self.lookup_symword("CURRENT_TASK_PTR")?)?;

        let module = self.lookup_module(t)?;
        let mut rval = HashMap::new();

        let ndx = match module.task {
            HubrisTask::Task(ndx) => ndx,
            _ => {
                bail!("must provide a user task")
            }
        };

        let task = self.lookup_struct_byname("Task")?;
        let save = task.lookup_member("save")?.offset as u32;
        let state = self.lookup_struct_byname("SavedState")?;

        let mut regs: Vec<u8> = vec![];
        regs.resize_with(state.size, Default::default);

        let offset = base + (ndx * task.size as u32) + save;
        core.read_8(offset, regs.as_mut_slice())?;

        //
        // If this is the current task, we want to pull the current PC.
        //
        if offset - save == cur {
            let pc = core.read_reg(ARMRegister::PC)?;

            //
            // If the PC falls within the task, then we are at user-level,
            // and we should take our register state directly rather than
            // from the stack.
            //
            let userland =
                if let Some(module) = self.modules.range(..=pc).next_back() {
                    pc < *module.0 + (module.1).textsize && module.1.task == t
                } else {
                    false
                };

            if userland {
                for i in 0..=31 {
                    let reg = match ARMRegister::from_u16(i) {
                        Some(r) => r,
                        None => {
                            continue;
                        }
                    };

                    let val = core.read_reg(reg)?;
                    rval.insert(reg, val);
                }

                return Ok(rval);
            }
        };

        let readreg = |rname| -> Result<u32> {
            let o = state.lookup_member(rname)?.offset as usize;
            Ok(u32::from_le_bytes(regs[o..o + 4].try_into().unwrap()))
        };

        //
        // R4-R11 are found in the structure.
        //
        for r in 4..=11 {
            let rname = format!("r{}", r);
            let o = state.lookup_member(&rname)?.offset as usize;
            let val = u32::from_le_bytes(regs[o..o + 4].try_into().unwrap());

            rval.insert(ARMRegister::from_usize(r).unwrap(), val);
        }

        let sp = readreg("psp")?;

        const NREGS_CORE: usize = 8;
        const NREGS_FP: usize = 17;
        const NREGS_FRAME: usize = NREGS_CORE + NREGS_FP + 1;

        let mut stack: Vec<u8> = vec![];
        stack.resize_with(NREGS_CORE * 4, Default::default);
        core.read_8(sp, stack.as_mut_slice())?;

        //
        // We manually adjust our stack pointer to peel off the entire frame
        //
        rval.insert(ARMRegister::SP, sp + (NREGS_FRAME as u32) * 4);

        //
        // R0-R3, and then R12, LR and the PSR are found on the stack
        //
        for r in 0..NREGS_CORE {
            let o = r * 4;
            let val = u32::from_le_bytes(stack[o..o + 4].try_into().unwrap());

            let reg = match r {
                0 | 1 | 2 | 3 => ARMRegister::from_usize(r).unwrap(),
                4 => ARMRegister::R12,
                5 => ARMRegister::LR,
                6 => ARMRegister::PC,
                7 => ARMRegister::xPSR,
                _ => panic!("bad register value"),
            };

            rval.insert(reg, val);
        }

        Ok(rval)
    }

    pub fn stack(
        &self,
        core: &mut dyn crate::core::Core,
        task: HubrisTask,
        limit: u32,
        regs: &HashMap<ARMRegister, u32>,
    ) -> Result<Vec<HubrisStackFrame>> {
        let regions = self.regions(core)?;
        let sp = regs.get(&ARMRegister::SP).unwrap();
        let pc = regs.get(&ARMRegister::PC).unwrap();

        let mut rval: Vec<HubrisStackFrame> = Vec::new();
        let mut frameregs = regs.clone();

        //
        // First, find the region that contains our stack pointer.  We want
        // to read that entire region.
        //
        let (_, region) = regions.range(..=sp).last().ok_or_else(|| {
            anyhow!("could not find memory region containing sp 0x{:x}", sp)
        })?;

        let mut buf: Vec<u8> = vec![];
        buf.resize_with(region.size as usize, Default::default);
        core.read_8(region.base, buf.as_mut_slice())?;

        let readval = |addr| {
            let o = (addr - region.base) as usize;
            u32::from_le_bytes(buf[o..o + 4].try_into().unwrap())
        };

        //
        // If our PC is in a system call (highly likely), we need to determine
        // what has been pushed on our stack via asm!().
        //
        if let Some(Some(pushed)) = self.syscall_pushes.get(pc) {
            for (i, &p) in pushed.iter().enumerate() {
                let val = readval(sp + (i * 4) as u32);
                frameregs.insert(p, val);
            }

            frameregs.insert(ARMRegister::SP, sp + (pushed.len() * 4) as u32);
        }

        let frames = self.frames.get(&task).unwrap();
        let frame = gimli::DebugFrame::new(frames, gimli::LittleEndian);

        loop {
            let bases = gimli::BaseAddresses::default();
            let mut ctx = gimli::UninitializedUnwindContext::new();
            let pc = *frameregs.get(&ARMRegister::PC).unwrap();

            //
            // Now we want to iterate up our frames
            //
            let unwind_info = frame.unwind_info_for_address(
                &bases,
                &mut ctx,
                pc as u64,
                gimli::DebugFrame::cie_from_offset,
            )?;

            //
            // Determine the CFA (Canonical Frame Address)
            //
            let cfa = match unwind_info.cfa() {
                gimli::CfaRule::RegisterAndOffset { register, offset } => {
                    let reg = ARMRegister::from_u16(register.0).unwrap();
                    *frameregs.get(&reg).unwrap() + *offset as u32
                }
                _ => {
                    panic!("unimplemented CFA rule");
                }
            };

            //
            // Now iterate over all of our register rules to transform
            // our registers.
            //
            for (register, rule) in unwind_info.registers() {
                let val = match rule {
                    gimli::RegisterRule::Offset(offset) => {
                        readval((i64::from(cfa) + offset) as u32)
                    }
                    _ => {
                        panic!("unimplemented register rule");
                    }
                };

                let reg = ARMRegister::from_u16(register.0).unwrap();
                frameregs.insert(reg, val);
            }

            frameregs.insert(ARMRegister::SP, cfa);

            //
            // Lookup the DWARF symbol associated with our PC
            //
            let sym = match self.dsyms.range(..=pc).next_back() {
                Some(sym) if pc < *sym.0 + (sym.1).1 => Some(HubrisSymbol {
                    addr: *sym.0 as u32,
                    name: &(sym.1).0,
                    goff: (sym.1).2,
                }),
                _ => None,
            };

            //
            // Determine if there is, in fact, an inlined stack here.
            //
            let inlined = match sym {
                Some(sym) => {
                    let mut inlined = self.instr_inlined(pc, sym.addr);
                    inlined.reverse();
                    Some(inlined)
                }
                None => None,
            };

            //
            // Our frame is complete -- push it and continue!
            //
            rval.push(HubrisStackFrame {
                cfa,
                sym,
                inlined,
                registers: frameregs.clone(),
            });

            //
            // Get our LR, and make sure that the low (Thumb) bit is clear
            //
            let lr = *frameregs.get(&ARMRegister::LR).unwrap() & !1;

            frameregs.insert(ARMRegister::PC, lr);

            if cfa >= limit {
                break;
            }
        }

        Ok(rval)
    }

    pub fn typesize(&self, goff: HubrisGoff) -> Result<usize> {
        if let Some(v) = self.structs.get(&goff) {
            return Ok(v.size);
        }

        if let Some(v) = self.basetypes.get(&goff) {
            return Ok(v.size);
        }

        if self.ptrtypes.get(&goff).is_some() {
            return Ok(4);
        }

        if let Some(v) = self.enums.get(&goff) {
            return Ok(v.size);
        }

        if let Some(union) = self.unions.get(&goff) {
            return Ok(union.size);
        }

        if let Some(v) = self.arrays.get(&goff) {
            return Ok(self.typesize(v.goff)? * v.count);
        }

        Err(anyhow!("unknown size for type {}", goff))
    }

    pub fn printfmt(
        &self,
        buf: &[u8],
        goff: HubrisGoff,
        fmt: &HubrisPrintFormat,
    ) -> Result<String> {
        let mut rval = String::new();
        let delim = if fmt.newline { "\n" } else { " " };

        let mut f = *fmt;

        let readval = |b: &[u8], o, sz| -> Result<u64> {
            Ok(match sz {
                1 => b[o] as u64,
                2 => u16::from_le_bytes(b[o..o + 2].try_into()?) as u64,
                4 => u32::from_le_bytes(b[o..o + 4].try_into()?) as u64,
                8 => u64::from_le_bytes(b[o..o + 8].try_into()?) as u64,
                _ => {
                    bail!("bad size!");
                }
            })
        };

        let readfloat = |b: &[u8], o, sz| -> Result<f64> {
            Ok(match sz {
                4 => f32::from_le_bytes(b[o..o + 4].try_into()?) as f64,
                8 => f64::from_le_bytes(b[o..o + 8].try_into()?) as f64,
                _ => {
                    bail!("bad float size!");
                }
            })
        };

        if let Some(v) = self.structs.get(&goff) {
            /*
             * This is a structure; iterate over its members.
             */
            if v.members.is_empty() {
                return Ok(rval);
            }

            f.indent += 4;

            if v.members[0].name == "__0" {
                let paren = !v.members.is_empty();

                if paren {
                    rval += "(";
                }

                for i in 0..v.members.len() {
                    let m = &v.members[i];

                    if let Some(child) = self.structs.get(&m.goff) {
                        rval += &child.name;
                    }

                    rval += &self.printfmt(&buf[m.offset..], m.goff, &f)?;

                    if i + 1 < v.members.len() {
                        rval += ", ";
                    }
                }

                if paren {
                    rval += ")";
                }

                return Ok(rval);
            }

            rval += " {";
            rval += delim;

            for i in 0..v.members.len() {
                let m = &v.members[i];

                if fmt.newline && f.indent > 0 {
                    rval += &format!("{:1$}", " ", f.indent);
                }

                rval += &m.name;
                rval += ": ";

                if let Some(child) = self.structs.get(&m.goff) {
                    rval += &child.name;
                }

                rval += &self.printfmt(&buf[m.offset..], m.goff, &f)?;

                if i + 1 < v.members.len() {
                    rval += ",";
                    rval += delim;
                }
            }

            rval += delim;

            if fmt.newline && fmt.indent > 0 {
                rval += &format!("{:1$}", " ", fmt.indent);
            }

            rval += "}";

            return Ok(rval);
        }

        if let Some(union) = self.enums.get(&goff) {
            /*
             * For enums, we need to determine which variant this actually
             * is -- which necessitates looking at our discriminant.
             */
            match union.discriminant {
                HubrisDiscriminant::Value(g, offs) => {
                    let size = match self.basetypes.get(&g) {
                        Some(v) => v.size,
                        None => {
                            bail!(
                                "enum {} has discriminant of unknown type: {}",
                                goff, g
                            );
                        }
                    };

                    let val = readval(buf, offs, size)
                        .context("failed to read discriminant".to_string())?;

                    match union.lookup_variant(val) {
                        None => {
                            write!(rval, "<? (0x{:x})>", val)?;
                        }

                        Some(variant) => {
                            rval += &variant.name;

                            if let Some(goff) = variant.goff {
                                rval += &self.printfmt(&buf[0..], goff, &f)?;
                            }
                        }
                    }

                    return Ok(rval);
                }

                HubrisDiscriminant::None => {
                    if union.variants.is_empty() {
                        bail!("enum {} has no variants", goff);
                    }

                    if union.variants.len() > 1 {
                        bail!(
                            "enum {} has multiple variants but no discriminant",
                            goff
                        );
                    }

                    let variant = &union.variants[0];
                    rval += &variant.name;

                    if let Some(goff) = variant.goff {
                        rval += &self.printfmt(&buf[0..], goff, &f)?;
                    }

                    return Ok(rval);
                }

                HubrisDiscriminant::Expected(_) => {
                    bail!("enum {} has incomplete discriminant", goff);
                }
            }
        }

        if let Some(array) = self.arrays.get(&goff) {
            let size = self.typesize(array.goff)?;

            f.indent += 4;
            let mut offset = 0;

            rval += "[";
            rval += delim;

            for i in 0..array.count {
                if fmt.newline && f.indent > 0 {
                    rval += &format!("{:1$}", " ", f.indent);
                }

                rval += &self.printfmt(&buf[offset..], array.goff, &f)?;
                offset += size;

                if i + 1 < array.count {
                    rval += ",";
                    rval += delim;
                }
            }

            rval += delim;

            if fmt.newline && fmt.indent > 0 {
                rval += &format!("{:1$}", " ", fmt.indent);
            }

            rval += "]";

            return Ok(rval);
        }

        if let Some(v) = self.basetypes.get(&goff) {
            if v.encoding == HubrisEncoding::Float {
                write!(rval, "{}", readfloat(buf, 0, v.size)?)?;
            } else {
                let val = readval(buf, 0, v.size)
                    .context(format!("failed to read base type {}", goff))?;

                if fmt.hex {
                    write!(rval, "0x{:x}", val)?;
                } else {
                    write!(rval, "{}", val)?;
                }
            }

            return Ok(rval);
        }

        if let Some(union) = self.unions.get(&goff) {
            if let Some(goff) = union.maybe_uninit() {
                rval += &self.printfmt(&buf[0..], goff, &f)?;
            } else {
                let val = readval(buf, 0, union.size)
                    .context(format!("failed to read union {}", goff))?;

                if fmt.hex {
                    write!(rval, "0x{:x}", val)?;
                } else {
                    write!(rval, "{}", val)?;
                }
            }

            return Ok(rval);
        }

        if let Some((name, _ptr)) = self.ptrtypes.get(&goff) {
            let val = readval(buf, 0, 4)
                .context(format!("failed to read pointer type {}", goff))?;

            write!(rval, "0x{:x} ({})", val, name)?;

            return Ok(rval);
        }

        rval = goff.to_string();

        Ok(rval)
    }

    pub fn print(&self, buf: &[u8], goff: HubrisGoff) -> Result<String> {
        self.printfmt(
            buf,
            goff,
            &HubrisPrintFormat { hex: true, ..HubrisPrintFormat::default() },
        )
    }

    pub fn dump(
        &self,
        core: &mut dyn crate::core::Core,
        dumpfile: Option<&str>,
    ) -> Result<()> {
        use indicatif::{HumanBytes, HumanDuration};
        use indicatif::{ProgressBar, ProgressStyle};
        use std::io::Write;

        let regions = self.regions(core)?;
        let nsegs = regions
            .values()
            .fold(0, |ttl, r| ttl + if !r.attr.device { 1 } else { 0 });

        macro_rules! pad {
            ($size:expr) => {
                ((4 - ($size & 0b11)) & 0b11) as u32
            };
        }

        let pad = [0u8; 4];

        let ctx = goblin::container::Ctx::new(
            goblin::container::Container::Little,
            goblin::container::Endian::Little,
        );

        let oxide = String::from(OXIDE_NT_NAME);

        let notesz = |note: &goblin::elf::note::Nhdr32| {
            size_of::<goblin::elf::note::Nhdr32>() as u32
                + note.n_namesz
                + pad!(note.n_namesz)
                + note.n_descsz
                + pad!(note.n_descsz)
        };

        let mut notes = vec![];
        let mut regs = vec![];

        for i in 0..31 {
            if let Some(reg) = ARMRegister::from_u16(i) {
                let val = core.read_reg(reg)?;
                regs.push((i, val));
            }
        }

        notes.push(goblin::elf::note::Nhdr32 {
            n_namesz: (oxide.len() + 1) as u32,
            n_descsz: regs.len() as u32 * 8,
            n_type: OXIDE_NT_HUBRIS_REGISTERS,
        });

        notes.push(goblin::elf::note::Nhdr32 {
            n_namesz: (oxide.len() + 1) as u32,
            n_descsz: self.archive.len() as u32,
            n_type: OXIDE_NT_HUBRIS_ARCHIVE,
        });

        let mut header = goblin::elf::header::Header::new(ctx);
        header.e_machine = goblin::elf::header::EM_ARM;
        header.e_type = goblin::elf::header::ET_CORE;
        header.e_phoff = header.e_ehsize as u64;
        header.e_phnum = (notes.len() + nsegs) as u16;

        let mut offset = header.e_phoff as u32
            + (header.e_phentsize * header.e_phnum) as u32;

        let filename = match dumpfile {
            Some(filename) => filename.to_owned(),
            None => {
                let mut filename;
                let mut i = 0;

                loop {
                    filename = format!("hubris.core.{}", i);

                    if let Ok(_f) = fs::File::open(&filename) {
                        i += 1;
                        continue;
                    }

                    break;
                }

                filename
            }
        };

        /*
         * Write our ELF header
         */
        let mut file =
            OpenOptions::new().write(true).create_new(true).open(&filename)?;

        info!("dumping to {}", filename);

        file.iowrite_with(header, ctx)?;

        let mut bytes = [0x0u8; goblin::elf32::program_header::SIZEOF_PHDR];

        /*
         * Write our program headers, starting with our note headers.
         */
        for note in &notes {
            let size = notesz(note);

            let phdr = goblin::elf32::program_header::ProgramHeader {
                p_type: goblin::elf::program_header::PT_NOTE,
                p_flags: goblin::elf::program_header::PF_R,
                p_offset: offset,
                p_filesz: size,
                ..Default::default()
            };

            bytes.pwrite_with(phdr, 0, ctx.le)?;
            file.write_all(&bytes)?;

            offset += size;
        }

        let mut total = 0;

        for (_, region) in regions.iter() {
            if region.attr.device {
                continue;
            }

            let seg_phdr = goblin::elf32::program_header::ProgramHeader {
                p_type: goblin::elf::program_header::PT_LOAD,
                p_flags: goblin::elf::program_header::PF_R,
                p_offset: offset,
                p_vaddr: region.base,
                p_filesz: region.size,
                p_memsz: region.size,
                ..Default::default()
            };

            bytes.pwrite_with(seg_phdr, 0, ctx.le)?;
            file.write_all(&bytes)?;

            offset += region.size + pad!(region.size);
            total += region.size;
        }

        for note in &notes {
            /*
             * Now write our note section, starting with our note header...
             */
            let mut bytes = [0x0u8; size_of::<goblin::elf::note::Nhdr32>()];
            bytes.pwrite_with(note, 0, ctx.le)?;
            file.write_all(&bytes)?;

            /*
             * ...and our note name
             */
            let bytes = oxide.as_bytes();
            file.write_all(bytes)?;
            let npad = 1 + pad!(note.n_namesz) as usize;
            file.write_all(&pad[0..npad])?;

            /*
             * ...and finally, the note itself.
             */
            match note.n_type {
                OXIDE_NT_HUBRIS_REGISTERS => {
                    let mut bytes = [0x0u8; 8];

                    for (reg, val) in regs.iter() {
                        bytes.pwrite_with(reg, 0, ctx.le)?;
                        bytes.pwrite_with(val, 4, ctx.le)?;
                        file.write_all(&bytes)?;
                    }
                }
                OXIDE_NT_HUBRIS_ARCHIVE => {
                    file.write_all(&self.archive)?;
                }
                _ => {
                    panic!("unimplemented note");
                }
            }

            let npad = pad!(note.n_descsz) as usize;
            file.write_all(&pad[0..npad])?;
        }

        /*
         * And now we write our segments.  This takes a little while, so
         * we're going to indicate our progress as we go.
         */
        let mut written = 0;

        let started = Instant::now();
        let bar = ProgressBar::new(total as u64);
        bar.set_style(
            ProgressStyle::default_bar()
                .template("humility: dumping [{bar:30}] {bytes}/{total_bytes}"),
        );

        for (_, region) in regions.iter() {
            if region.attr.device {
                continue;
            }

            let mut remain = region.size as usize;
            let mut bytes = vec![0; 1024];
            let mut addr = region.base;

            while remain > 0 {
                let nbytes =
                    if remain > bytes.len() { bytes.len() } else { remain };

                core.read_8(addr, &mut bytes[0..nbytes])?;
                file.write_all(&bytes[0..nbytes])?;
                remain -= nbytes;
                written += nbytes;
                addr += nbytes as u32;
                bar.set_position(written as u64);
            }

            let npad = pad!(region.size) as usize;
            file.write_all(&pad[0..npad])?;
        }

        bar.finish_and_clear();

        info!(
            "dumped {} in {}",
            HumanBytes(written as u64),
            HumanDuration(started.elapsed())
        );

        Ok(())
    }

    pub fn manifest(&self) -> Result<()> {
        ensure!(
            !self.modules.is_empty(),
            "must specify a valid Hubris archive"
        );

        let print = |what, val| {
            info!("{:>12} => {}", what, val);
        };

        let size = |task| {
            self.modules
                .iter()
                .find(|m| m.1.task == task)
                .map(|module| module.1.memsize)
                .unwrap()
        };

        print(
            "version",
            match self.manifest.version {
                Some(ref str) => str,
                None => "<unknown>",
            },
        );

        print(
            "git rev",
            match self.manifest.gitrev {
                Some(ref str) => str,
                None => "<unknown>",
            },
        );

        print(
            "board",
            match self.manifest.board {
                Some(ref str) => str,
                None => "<unknown>",
            },
        );

        print(
            "target",
            match self.manifest.target {
                Some(ref str) => str,
                None => "<unknown>",
            },
        );

        print("features", &self.manifest.features.join(", "));

        let ttl = self.modules.iter().fold(0, |ttl, m| ttl + (m.1).memsize);

        info!("{:>12} => {}K", "total size", ttl / 1024);
        info!("{:>12} => {}K", "kernel size", size(HubrisTask::Kernel) / 1024);
        info!("{:>12} => {}", "tasks", self.modules.len() - 1);
        info!("{:>18} {:18} {:>5} {}", "ID", "TASK", "SIZE", "FEATURES");

        let mut id = 0;

        for module in self.modules.values() {
            if module.task == HubrisTask::Kernel {
                continue;
            }

            let features = self.manifest.task_features.get(&module.name);

            info!(
                "{:>18} {:18} {:>4.1}K {}",
                id,
                module.name,
                module.memsize as f64 / 1024_f64,
                if let Some(f) = features {
                    f.join(", ")
                } else {
                    "".to_string()
                }
            );

            id += 1;
        }

        Ok(())
    }

    pub fn list_variables(&self) -> Result<()> {
        let mut variables = vec![];

        for (name, variable) in &self.variables {
            for v in variable {
                variables.push((HubrisTask::from(v.goff), name, v));
            }
        }

        variables.sort();

        info!("{:18} {:<30} {:<10} {}", "MODULE", "VARIABLE", "ADDR", "SIZE");

        for v in variables {
            let task = &self.lookup_module(v.0)?.name;
            info!("{:18} {:<30} 0x{:08x} {:<}", task, v.1, v.2.addr, v.2.size);
        }
        Ok(())
    }

    pub fn lookup_feature(&self, feature: &str) -> Result<Vec<HubrisTask>> {
        let mut rval = vec![];

        ensure!(
            !self.modules.is_empty(),
            "Hubris archive required specify a task feature"
        );

        for module in self.modules.values() {
            if module.task == HubrisTask::Kernel {
                continue;
            }

            if let Some(features) =
                self.manifest.task_features.get(&module.name)
            {
                for f in features {
                    if f == feature {
                        rval.push(module.task);
                    }
                }
            }
        }

        Ok(rval)
    }

    pub fn lookup_peripheral(&self, name: &str) -> Result<u32> {
        ensure!(
            !self.modules.is_empty(),
            "Hubris archive required to specify a peripheral"
        );

        if let Some(addr) = self.manifest.peripherals.get(name) {
            Ok(*addr)
        } else {
            let peripherals: Vec<&str> =
                self.manifest.peripherals.keys().map(String::as_str).collect();

            bail!("{} does not correspond to a peripheral; \
                expected one of: {}", name, peripherals.join(", "));
        }
    }

    pub fn clock(
        &self,
        core: &mut dyn crate::core::Core,
    ) -> Result<Option<u32>> {
        let name = "CLOCK_FREQ_KHZ";

        trace!("determining clock requency via {}", name);

        match self.variables.get(name) {
            Some(variable) => {
                if variable.size != 4 {
                    Err(anyhow!(
                        "{} has wrong size (expected 4, found {})",
                        name, variable.size
                    ))
                } else {
                    Ok(Some(core.read_word_32(variable.addr)?))
                }
            }

            None => Ok(None),
        }
    }

    pub fn apptable(&self) -> &[u8] {
        &self.apptable.1
    }
}
