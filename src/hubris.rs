/*
 * Copyright 2020 Oxide Computer Company
 */

use std::collections::BTreeMap;
use std::collections::HashMap;
use std::collections::HashSet;
use std::convert::Infallible;
use std::convert::TryInto;
use std::fmt;
use std::fmt::Write;
use std::fs;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::path::Path;
use std::str;

use fallible_iterator::FallibleIterator;

use anyhow::{anyhow, bail, ensure, Result};

use capstone::prelude::*;
use capstone::InsnGroupType;
use goblin::elf::Elf;
use multimap::MultiMap;
use rustc_demangle::demangle;

#[derive(Default, Debug)]
pub struct HubrisManifest {
    version: Option<String>,
    gitrev: Option<String>,
    features: Vec<String>,
    board: Option<String>,
    target: Option<String>,
    task_features: HashMap<String, Vec<String>>,
}

#[derive(Debug)]
pub struct HubrisPackage {
    // constructed manifest
    manifest: HubrisManifest,

    // current object
    current: u32,

    // Capstone library handle
    cs: capstone::Capstone,

    // Instructions: address to bytes/target tuple
    instrs: HashMap<u32, (Vec<u8>, HubrisTarget)>,

    // Modules: address to string/base/memsz tuple
    modules: BTreeMap<u32, (String, u32, u32)>,

    // DWARF symbols: address to name/length/goff tuple
    dsyms: BTreeMap<u32, (String, u32, HubrisGoff)>,

    // ELF symbols: address to name/length tuple
    esyms: BTreeMap<u32, (String, u32)>,

    // ELF symbols: name to value/length
    esyms_byname: HashMap<String, (u32, u32)>,

    // Inlined: address/nesting tuple to length/goff/origin tuple
    inlined: BTreeMap<(u32, isize), (u32, HubrisGoff, HubrisGoff)>,

    // Subprograms: goff to name
    subprograms: HashMap<HubrisGoff, String>,

    // Base types: goff to size
    basetypes: HashMap<HubrisGoff, usize>,

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
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone)]
///
/// An identifier that corresponds to a global offset within a particular DWARF
/// object.
///
pub struct HubrisGoff {
    pub object: u32,
    pub goff: usize,
}

#[derive(Copy, Clone, Debug)]
pub struct HubrisSymbol<'a> {
    pub addr: u32,
    pub name: &'a str,
    pub goff: Option<HubrisGoff>,
}

#[derive(Debug)]
pub struct HubrisInlined<'a> {
    pub addr: u32,
    pub name: &'a str,
    pub id: HubrisGoff,
    pub origin: HubrisGoff,
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

#[derive(Debug)]
pub struct HubrisEnumVariant {
    pub name: String,
    pub offset: usize,
    pub goff: HubrisGoff,
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
    pub tag: Option<u64>,
    pub variants: Vec<HubrisEnumVariant>,
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

#[derive(Copy, Clone, Debug)]
pub struct HubrisDumpFormat {
    pub indent: usize,
    pub newline: bool,
    pub hex: bool,
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
}

#[rustfmt::skip::macros(anyhow, bail)]
impl HubrisPackage {
    pub fn new() -> Result<HubrisPackage> {
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
            manifest: Default::default(),
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
            modules: BTreeMap::new(),
            dsyms: BTreeMap::new(),
            esyms: BTreeMap::new(),
            esyms_byname: HashMap::new(),
            inlined: BTreeMap::new(),
            subprograms: HashMap::new(),
            basetypes: HashMap::new(),
            ptrtypes: HashMap::new(),
            structs: HashMap::new(),
            structs_byname: MultiMap::new(),
            enums: HashMap::new(),
            enums_byname: MultiMap::new(),
        })
    }

    pub fn instr_len(&self, addr: u32) -> Option<u32> {
        match self.instrs.get(&addr) {
            Some(instr) => Some(instr.0.len() as u32),
            None => None,
        }
    }

    pub fn instr_target(&self, addr: u32) -> HubrisTarget {
        match self.instrs.get(&addr) {
            Some(instr) => instr.1,
            None => HubrisTarget::None,
        }
    }

    pub fn instr_mod(&self, addr: u32) -> Option<&str> {
        if let Some(module) = self.modules.range(..=addr).next_back() {
            if addr < *module.0 + (module.1).1 {
                Some(&(module.1).0)
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
                    name: &func,
                    id: *goff,
                    origin: *origin,
                });
            }
        }

        inlined.reverse();
        inlined
    }

    fn instr_branch_target(&self, instr: &capstone::Insn) -> HubrisTarget {
        let detail = match self.cs.insn_detail(&instr) {
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
                    if let arch::arm::ArmOperandType::Reg(reg) = op.op_type {
                        if let RegId(ARM_REG_LR) = reg {
                            return HubrisTarget::Return;
                        }
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
                    if let arch::arm::ArmOperandType::Reg(reg) = op.op_type {
                        if let RegId(ARM_REG_PC) = reg {
                            return HubrisTarget::Return;
                        }
                    }
                }
            }
        }

        HubrisTarget::None
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

    #[allow(dead_code)]
    pub fn lookup_struct(&self, goff: HubrisGoff) -> Result<&HubrisStruct> {
        match self.structs.get(&goff) {
            Some(str) => Ok(str),
            None => Err(anyhow!("expected struct {} not found", goff)),
        }
    }

    pub fn lookup_enum(&self, goff: HubrisGoff) -> Result<&HubrisEnum> {
        match self.enums.get(&goff) {
            Some(union) => Ok(union),
            None => Err(anyhow!("expected enum {} not found", goff)),
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

    pub fn ntasks(&self) -> usize {
        if self.current >= 1 {
            self.current as usize - 1
        } else {
            0
        }
    }

    pub fn validate(&self, core: &mut dyn crate::core::Core) -> Result<()> {
        let ntasks = self.ntasks();

        if self.current == 0 {
            /*
             * If we have no objects, we were never loaded -- and we consider
             * this to be validated because it will give no answers rather than
             * wrong ones.
             */
            return Ok(());
        }

        let size =
            core.read_word_32(self.lookup_symword("TASK_TABLE_SIZE")?)?;

        if size == 0 {
            /*
             * The TASK_TABLE_SIZE is set dynamically by Hubris on boot -- so
             * it can be 0 if the kernel hasn't booted.  If this is the case,
             * we expect the TASK_TABLE_BASE to also be 0, so as an added
             * check, we check that as well.
             */
            let base =
                core.read_word_32(self.lookup_symword("TASK_TABLE_BASE")?)?;

            if base != 0 {
                Err(anyhow!("TASK_TABLE_SIZE is 0, but TASK_TABLE_BASE is 0x{:x}; \
                    package mismatch?", base))
            } else {
                Ok(())
            }
        } else if size != ntasks as u32 {
            Err(anyhow!("tasks in image ({}) exceeds tasks in \
                package ({}); package mismatch?", size, ntasks))
        } else {
            Ok(())
        }
    }

    pub fn dumpfmt(
        &self,
        buf: &[u8],
        goff: HubrisGoff,
        fmt: &HubrisDumpFormat,
    ) -> Result<String> {
        let mut rval = String::new();
        let delim = if fmt.newline { "\n" } else { " " };

        let mut f = *fmt;
        f.indent += 4;

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

        if let Some(v) = self.structs.get(&goff) {
            /*
             * This is a structure; iterate over its members.
             */
            if v.members.len() == 0 {
                return Ok(rval);
            }

            if v.members[0].name == "__0" {
                let paren = v.members.len() > 1
                    || self.basetypes.get(&v.members[0].goff).is_none();

                if paren {
                    rval += "(";
                }

                for i in 0..v.members.len() {
                    let m = &v.members[i];
                    rval += &self.dumpfmt(&buf[m.offset..], m.goff, &f)?;

                    if i + 1 < v.members.len() {
                        rval += ", ";
                    }
                }

                if paren {
                    rval += ")";
                }

                return Ok(rval);
            }

            rval += "{";
            rval += delim;

            for i in 0..v.members.len() {
                let m = &v.members[i];

                if fmt.newline && fmt.indent > 0 {
                    rval += &format!("{:1$}", " ", f.indent);
                }

                rval += &m.name;
                rval += ": ";

                rval += &self.dumpfmt(&buf[m.offset..], m.goff, &f)?;

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
                        Some(size) => size,
                        None => {
                            bail!("enum {} has discriminant of unknown type: {}", goff, g);
                        }
                    };

                    let val = readval(buf, offs, *size)?;

                    match union.lookup_variant(val) {
                        None => {
                            write!(rval, "<? (0x{:x})>", val)?;
                        }

                        Some(variant) => {
                            rval += &variant.name;
                            rval +=
                                &self.dumpfmt(&buf[0..], variant.goff, &f)?;
                        }
                    }

                    return Ok(rval);
                }

                HubrisDiscriminant::None => {
                    if union.variants.len() == 0 {
                        bail!("enum {} has no variants", goff);
                    }

                    if union.variants.len() > 1 {
                        bail!("enum {} has multiple variants but no discriminant", goff);
                    }

                    let variant = &union.variants[0];

                    rval += &variant.name;
                    rval += &self.dumpfmt(&buf[0..], variant.goff, &f)?;

                    return Ok(rval);
                }

                HubrisDiscriminant::Expected(_) => {
                    bail!("enum {} has incomplete discriminant", goff);
                }
            }
        }

        if let Some(base) = self.basetypes.get(&goff) {
            let val = readval(buf, 0, *base)?;

            if fmt.hex {
                write!(rval, "0x{:x}", val)?;
            } else {
                write!(rval, "{}", val)?;
            }

            return Ok(rval);
        }

        if let Some((name, _ptr)) = self.ptrtypes.get(&goff) {
            let val = readval(buf, 0, 4)?;

            write!(rval, "0x{:x} ({})", val, name)?;

            return Ok(rval);
        }

        rval = goff.to_string();

        Ok(rval)
    }

    pub fn dump(&self, buf: &[u8], goff: HubrisGoff) -> Result<String> {
        self.dumpfmt(
            buf,
            goff,
            &HubrisDumpFormat { indent: 0, newline: false, hex: false },
        )
    }

    fn dwarf_basetype<'a, R: gimli::Reader<Offset = usize>>(
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

        while let Some(attr) = attrs.next()? {
            match attr.name() {
                gimli::constants::DW_AT_byte_size => {
                    if let gimli::AttributeValue::Udata(value) = attr.value() {
                        size = Some(value as usize)
                    }
                }
                _ => {}
            }
        }

        if let Some(size) = size {
            self.basetypes.insert(goff, size);
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
                    size: size,
                    goff: goff,
                    members: Vec::new(),
                },
            );

            self.structs_byname.insert(name.to_string(), goff);
        }

        Ok(())
    }

    fn dwarf_enum<'a, R: gimli::Reader<Offset = usize>>(
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
                goff: goff,
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

    fn dwarf_variant<'a, R: gimli::Reader<Offset = usize>>(
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
                    goff: g,
                    tag: union.tag,
                });

                union.tag = None;
            } else {
                bail!("enum variant {} is incomplete", member);
            }
        } else {
            /*
             * This is possible because of Rust's (unsafe-only) support for
             * C-style unions, which we ignore.
             */
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

            while let Some((delta, entry)) = entries.next_dfs()? {
                depth += delta;

                let goff = self.dwarf_goff(&unit, entry);

                if depth as usize >= stack.len() {
                    stack.push(goff);
                } else {
                    stack[depth as usize] = goff;
                }

                match entry.tag() {
                    gimli::constants::DW_TAG_inlined_subroutine => {
                        self.dwarf_inlined(&dwarf, &unit, &entry, depth)?;
                    }

                    gimli::constants::DW_TAG_subprogram => {
                        self.dwarf_subprogram(&dwarf, &unit, &entry)?;
                    }

                    gimli::constants::DW_TAG_structure_type => {
                        self.dwarf_struct(&dwarf, &unit, &entry)?;
                    }

                    gimli::constants::DW_TAG_base_type => {
                        self.dwarf_basetype(&unit, &entry)?;
                    }

                    gimli::constants::DW_TAG_pointer_type => {
                        self.dwarf_ptrtype(&dwarf, &unit, &entry)?;
                    }

                    gimli::constants::DW_TAG_variant_part => {
                        if depth == 0 {
                            bail!("no enum for variant {}", goff);
                        }

                        let parent = stack[depth as usize - 1];
                        self.dwarf_enum(&unit, &entry, parent)?;

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
                        self.dwarf_variant(&unit, &entry, parent)?;

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
                        self.dwarf_member(&dwarf, &unit, &entry, parent)?;
                    }

                    _ => {}
                }
            }
        }

        self.current += 1;

        Ok(())
    }

    fn load_object(&mut self, object: &str, buffer: &[u8]) -> Result<()> {
        let elf = Elf::parse(buffer).map_err(|e| {
            anyhow!("unrecognized ELF object: {}: {}", object, e)
        })?;

        let arm = elf.header.e_machine == goblin::elf::header::EM_ARM;

        if !arm {
            bail!("{} not an ARM ELF object", object);
        }

        for sym in elf.syms.iter() {
            if sym.st_name == 0 || sym.st_size == 0 {
                continue;
            }

            let name = match elf.strtab.get(sym.st_name) {
                Some(n) => n?,
                None => {
                    bail!("bad symbol in {}: {}", object, sym.st_name);
                }
            };

            /*
             * On ARM, we must explicitly clear the low bit of the symbol
             * table, which exists only to indicate a function that contains
             * Thumb instructions (which is of course every function on a
             * microprocessor that executes only Thumb instructions).
             */
            assert!(arm);
            let val = sym.st_value as u32 & !1;
            let dem = format!("{:#}", demangle(name));

            self.esyms_byname
                .insert(name.to_string(), (val, sym.st_size as u32));
            self.esyms.insert(val, (dem, sym.st_size as u32));
        }

        let memsz = elf.program_headers.iter().fold(0, |ttl, hdr| {
            if hdr.p_type == goblin::elf::program_header::PT_LOAD {
                ttl + hdr.p_memsz
            } else {
                ttl
            }
        });

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
        let t = buffer.get(offset..offset + size).unwrap();
        self.load_object_dwarf(buffer, &elf)?;

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
            (String::from(object), size as u32, memsz as u32),
        );

        Ok(())
    }

    fn load_config(&mut self, toml: &toml::value::Table) -> Result<()> {
        let mut manifest = &mut self.manifest;

        let conf = |member| {
            toml.get(member)
                .and_then(|m| m.as_str())
                .and_then(|s| Some(s.to_string()))
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
                .into_iter()
                .map(|f| f.as_str().unwrap().to_string())
                .collect::<Vec<String>>();
        }

        /*
         * Now we want to iterate over our tasks to discover their features as
         * well.
         */
        if let Some(toml::Value::Table(ref tasks)) = toml.get("tasks") {
            for (task, config) in tasks.into_iter() {
                let features = config.get("features");

                if let Some(toml::Value::Array(ref features)) = features {
                    manifest.task_features.insert(
                        task.to_string(),
                        features
                            .into_iter()
                            .map(|f| f.as_str().unwrap().to_string())
                            .collect::<Vec<String>>(),
                    );
                }
            }
        }

        Ok(())
    }

    fn load_package(&mut self, package: &str) -> Result<()> {
        let file = fs::File::open(package)?;
        let mut archive = zip::ZipArchive::new(file)?;
        let mut manifest = &mut self.manifest;

        macro_rules! byname {
            ($name:tt) => {
                archive.by_name($name).map_err(|e| {
                    anyhow!("failed to find \"{}\": {}", $name, e)
                })
            }
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
                self.load_config(&toml)?;
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
        self.load_object("kernel", &buffer)?;

        /*
         * And now we need to find the tasks.
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
            self.load_object(&object, &buffer)?;
        }

        Ok(())
    }

    pub fn load(&mut self, package: &str) -> Result<()> {
        let metadata = fs::metadata(package)?;

        if !metadata.is_dir() {
            return self.load_package(package);
        }

        warn!(
            "warning: a directory as a package is deprecated; \
            use archive instead"
        );

        let mapfile = package.to_owned() + "/map.txt";
        let expected = &["ADDRESS", "END", "SIZE", "FILE"];

        let map = match File::open(mapfile) {
            Ok(map) => map,
            Err(e) => {
                bail!("couldn't find map.txt: {}", e);
            }
        };

        let f = BufReader::new(map);
        let mut iter = f.lines();

        match iter.next() {
            Some(header) => {
                let h = header.unwrap();
                let headers: Vec<&str> = h.split_ascii_whitespace().collect();

                ensure!(
                    headers.len() != expected.len(),
                    "bad headers: found {}, expected {}",
                    headers.len(),
                    expected.len()
                );

                for i in 0..expected.len() {
                    if headers[i] == expected[i] {
                        continue;
                    }

                    bail!("bad header: found \"{}\", expected \"{}\"",
                        headers[i],
                        expected[i]
                    );
                }
            }

            None => {
                bail!("map file empty or otherwise missing a header");
            }
        };

        let mut lineno = 1;
        let mut seen = HashSet::new();

        for line in iter {
            let l = line.unwrap();
            let fields: Vec<&str> = l.split_ascii_whitespace().collect();

            lineno += 1;

            if fields.len() < expected.len() {
                bail!("short line at {}", lineno);
            }

            let file = Path::new(fields[3]);
            let pieces = file.components().collect::<Vec<_>>();

            /*
             * We expect our object name to be the last element of our path.
             */
            let o = pieces.len() - 1;

            let object = match pieces[o].to_owned().as_os_str().to_str() {
                Some(ref str) => str.to_string(),
                None => {
                    bail!(
                        "bad object name \"{}\" on line {}",
                        file.display(), lineno
                    );
                }
            };

            if !seen.contains(&object) {
                let p = Path::new(package).join(&object);
                let buffer = fs::read(p)?;
                self.load_object(&object, &buffer)?;
                seen.insert(object);
            }
        }

        Ok(())
    }

    pub fn manifest(&self) -> Result<()> {
        ensure!(self.modules.len() > 0, "must specify a valid Hubris package");

        let print = |what, val| {
            info!("{:>12} => {}", what, val);
        };

        let size = |task| {
            self.modules
                .iter()
                .find(|module| if (module.1).0 == task { true } else { false })
                .and_then(|module| Some((module.1).2))
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

        let ttl = self.modules.iter().fold(0, |ttl, module| ttl + (module.1).2);

        info!("{:>12} => {}K", "total size", ttl / 1024);
        info!("{:>12} => {}K", "kernel size", size("kernel") / 1024);
        info!("{:>12} => {}", "tasks", self.modules.len() - 1);
        info!("{:>18} {:18} {:>5} {}", "ID", "TASK", "SIZE", "FEATURES");

        let mut id = 0;

        for module in self.modules.iter().skip(1) {
            let features = self.manifest.task_features.get(&(module.1).0);

            info!(
                "{:>18} {:18} {:>4.1}K {}",
                id,
                (module.1).0,
                (module.1).2 as f64 / 1024 as f64,
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
}
