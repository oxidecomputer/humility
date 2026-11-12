// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

// TODO: Add comment

use std::collections::{BTreeMap, BTreeSet, HashMap, VecDeque};

use anyhow::{Context, Result, bail};
use gimli::{
    AttributeValue, DwAt, DwTag, EndianSlice, LittleEndian, constants,
};
use humility::hubris::{HubrisArchive, HubrisTask};

type Reader<'a> = EndianSlice<'a, LittleEndian>;

/// TODO: Add docs
pub fn diff(
    old: &BTreeMap<String, String>,
    new: &BTreeMap<String, String>,
) -> String {
    let mut out = String::new();
    let modules: BTreeSet<&String> = old.keys().chain(new.keys()).collect();
    for module in modules {
        let empty = String::new();
        let old_text = old.get(module).unwrap_or(&empty);
        let new_text = new.get(module).unwrap_or(&empty);
        if old_text == new_text {
            continue;
        }
        let diff = similar::TextDiff::from_lines(old_text, new_text);
        out.push_str(
            &diff
                .unified_diff()
                .context_radius(3)
                .header(&format!("old/{module}"), &format!("new/{module}"))
                .to_string(),
        );
    }
    out
}

/// TODO: Add docs
pub fn shapes_of_archive(
    hubris: &HubrisArchive,
) -> Result<BTreeMap<String, String>> {
    let mut out = BTreeMap::new();
    for module in hubris.modules() {
        let path = match module.task {
            HubrisTask::Kernel => "elf/kernel".to_owned(),
            HubrisTask::Task(_) => format!("elf/task/{}", module.name),
        };
        let Some(bytes) = hubris.read_file(&path)? else {
            bail!("archive has no {path}");
        };
        let text = shapes_of_elf(&bytes)
            .with_context(|| format!("processing {path}"))?;
        out.insert(module.name.clone(), text);
    }
    Ok(out)
}

/// TODO: Add docs
struct Die {
    tag: DwTag,
    name: Option<String>,
    attrs: Vec<(DwAt, String)>,
    type_ref: Option<usize>,
    discr_ref: Option<usize>,
    has_location: bool,
    parent: Option<usize>,
    children: Vec<usize>,
}

// TODO: Add comment
const SHOWN_ATTRS: &[DwAt] = &[
    constants::DW_AT_data_member_location,
    constants::DW_AT_discr_value,
    constants::DW_AT_const_value,
    constants::DW_AT_byte_size,
    constants::DW_AT_alignment,
    constants::DW_AT_artificial,
    constants::DW_AT_count,
    constants::DW_AT_upper_bound,
];

/// TODO: Add docs
fn shapes_of_elf(bytes: &[u8]) -> Result<String> {
    let dies = load_dies(bytes)?;
    let mut names = HashMap::new();

    let mut vars = BTreeSet::new();
    let mut queue = VecDeque::new();
    for die in dies.values() {
        if die.tag != constants::DW_TAG_variable
            || !die.has_location
            || in_function(&dies, die)
        {
            continue;
        }
        let Some(name) = &die.name else { continue };
        let qualified = qualify(&dies, die, name);
        let ty = type_name(&dies, &mut names, die.type_ref, 0);
        vars.insert(format!("var {qualified} : {ty}\n"));
        queue.extend(die.type_ref);
    }

    let mut seen = BTreeSet::new();
    let mut blocks = BTreeSet::new();
    while let Some(offset) = queue.pop_front() {
        if !seen.insert(offset) {
            continue;
        }
        let (name, block, refs) = describe(&dies, &mut names, offset);
        blocks.insert((name, block));
        queue.extend(refs);
    }

    let mut out = String::new();
    for v in vars {
        out.push_str(&v);
    }
    for (_, block) in blocks {
        out.push_str(&block);
    }
    Ok(out)
}

/// TODO: Add docs
fn load_dies(bytes: &[u8]) -> Result<BTreeMap<usize, Die>> {
    let elf = goblin::elf::Elf::parse(bytes)?;
    let section = |id: gimli::SectionId| -> Result<Reader<'_>> {
        for sh in &elf.section_headers {
            if elf.shdr_strtab.get_at(sh.sh_name) == Some(id.name()) {
                if sh.sh_flags
                    & u64::from(goblin::elf::section_header::SHF_COMPRESSED)
                    != 0
                {
                    bail!("compressed section {} is not supported", id.name());
                }
                let start = sh.sh_offset as usize;
                let end = start + sh.sh_size as usize;
                return Ok(EndianSlice::new(&bytes[start..end], LittleEndian));
            }
        }
        Ok(EndianSlice::new(&[], LittleEndian))
    };
    let dwarf = gimli::Dwarf::load(section)?;

    let mut dies: BTreeMap<usize, Die> = BTreeMap::new();
    let mut headers = dwarf.units();
    while let Some(header) = headers.next()? {
        let unit = dwarf.unit(header)?;
        let global = |offset: gimli::UnitOffset| {
            offset.to_debug_info_offset(&unit.header).map(|o| o.0)
        };
        let reference = |value: AttributeValue<Reader<'_>>| match value {
            AttributeValue::UnitRef(o) => global(o),
            AttributeValue::DebugInfoRef(o) => Some(o.0),
            _ => None,
        };

        let mut stack: Vec<usize> = vec![];
        let mut cursor = unit.entries();
        while let Some(entry) = cursor.next_dfs()? {
            let Some(offset) = global(entry.offset()) else {
                continue;
            };
            stack.truncate(entry.depth() as usize);
            let parent = stack.last().copied();
            stack.push(offset);

            let mut die = Die {
                tag: entry.tag(),
                name: None,
                attrs: vec![],
                type_ref: None,
                discr_ref: None,
                has_location: false,
                parent,
                children: vec![],
            };
            for attr in entry.attrs() {
                match attr.name() {
                    constants::DW_AT_name => {
                        let s = dwarf.attr_string(&unit, attr.value())?;
                        die.name = Some(s.to_string_lossy().into_owned());
                    }
                    constants::DW_AT_type => {
                        die.type_ref = reference(attr.value())
                    }
                    constants::DW_AT_discr => {
                        die.discr_ref = reference(attr.value())
                    }
                    constants::DW_AT_location => die.has_location = true,
                    name if SHOWN_ATTRS.contains(&name) => {
                        die.attrs.push((name, format_value(attr.value())));
                    }
                    _ => (),
                }
            }
            if let Some(p) = parent
                && let Some(parent) = dies.get_mut(&p)
            {
                parent.children.push(offset);
            }
            dies.insert(offset, die);
        }
    }
    Ok(dies)
}

// TODO: Add comment
fn format_value(value: AttributeValue<Reader<'_>>) -> String {
    match value {
        AttributeValue::Sdata(v) => v.to_string(),
        AttributeValue::Flag(v) => v.to_string(),
        v => match v.udata_value() {
            Some(u) => u.to_string(),
            None => format!("{v:?}"),
        },
    }
}

// TODO: Add comment
fn in_function(dies: &BTreeMap<usize, Die>, die: &Die) -> bool {
    let mut parent = die.parent;
    while let Some(p) = parent {
        let d = &dies[&p];
        if matches!(
            d.tag,
            constants::DW_TAG_subprogram
                | constants::DW_TAG_lexical_block
                | constants::DW_TAG_inlined_subroutine
        ) {
            return true;
        }
        parent = d.parent;
    }
    false
}

// TODO: Add comment
fn qualify(dies: &BTreeMap<usize, Die>, die: &Die, name: &str) -> String {
    let mut parts = vec![name.to_owned()];
    let mut parent = die.parent;
    while let Some(p) = parent {
        let d = &dies[&p];
        if matches!(
            d.tag,
            constants::DW_TAG_namespace
                | constants::DW_TAG_structure_type
                | constants::DW_TAG_union_type
                | constants::DW_TAG_enumeration_type
        ) && let Some(n) = &d.name
        {
            parts.push(n.clone());
        }
        parent = d.parent;
    }
    parts.reverse();
    parts.join("::")
}

/// TODO: Add docs
fn type_name(
    dies: &BTreeMap<usize, Die>,
    names: &mut HashMap<usize, String>,
    offset: Option<usize>,
    depth: usize,
) -> String {
    let Some(offset) = offset else {
        return "()".to_owned();
    };
    if let Some(n) = names.get(&offset) {
        return n.clone();
    }
    if depth > 20 {
        return "<deep>".to_owned();
    }
    let die = &dies[&offset];
    let inner = |names: &mut HashMap<usize, String>| {
        type_name(dies, names, die.type_ref, depth + 1)
    };
    let name = match (die.tag, &die.name) {
        (_, Some(n))
            if die.tag != constants::DW_TAG_array_type
                && die.tag != constants::DW_TAG_const_type
                && die.tag != constants::DW_TAG_volatile_type =>
        {
            qualify(dies, die, n)
        }
        (
            constants::DW_TAG_pointer_type | constants::DW_TAG_reference_type,
            _,
        ) => {
            format!("*{}", inner(names))
        }
        (constants::DW_TAG_array_type, _) => {
            let dims: Vec<String> = die
                .children
                .iter()
                .map(|c| &dies[c])
                .filter(|c| c.tag == constants::DW_TAG_subrange_type)
                .map(|c| array_count(c).unwrap_or_else(|| "?".to_owned()))
                .collect();
            format!("[{}; {}]", inner(names), dims.join(", "))
        }
        (constants::DW_TAG_const_type, _) => format!("const {}", inner(names)),
        (constants::DW_TAG_volatile_type, _) => {
            format!("volatile {}", inner(names))
        }
        (constants::DW_TAG_subroutine_type, _) => "fn(...)".to_owned(),
        (tag, _) => format!("<anon {tag}>"),
    };
    names.insert(offset, name.clone());
    name
}

// TODO: Add comment
fn array_count(subrange: &Die) -> Option<String> {
    let attr = |want: DwAt| {
        subrange
            .attrs
            .iter()
            .find(|(name, _)| *name == want)
            .map(|(_, value)| value.clone())
    };
    if let Some(count) = attr(constants::DW_AT_count) {
        return Some(count);
    }
    let upper: u64 = attr(constants::DW_AT_upper_bound)?.parse().ok()?;
    Some((upper + 1).to_string())
}

/// TODO: Add docs
fn describe(
    dies: &BTreeMap<usize, Die>,
    names: &mut HashMap<usize, String>,
    offset: usize,
) -> (String, String, Vec<usize>) {
    let die = &dies[&offset];
    let name = type_name(dies, names, Some(offset), 0);
    let mut refs = vec![];

    let mut block =
        format!("{} {name}{}", tag_label(die.tag), format_attrs(&die.attrs));
    if die.type_ref.is_some() {
        block.push_str(&format!(
            " -> {}",
            type_name(dies, names, die.type_ref, 0)
        ));
        refs.extend(die.type_ref);
    }
    block.push('\n');
    describe_children(dies, names, die, 1, &mut block, &mut refs);
    (name, block, refs)
}

// TODO: Add comment
fn describe_children(
    dies: &BTreeMap<usize, Die>,
    names: &mut HashMap<usize, String>,
    die: &Die,
    indent: usize,
    block: &mut String,
    refs: &mut Vec<usize>,
) {
    for c in &die.children {
        let child = &dies[c];
        if matches!(
            child.tag,
            constants::DW_TAG_subprogram
                | constants::DW_TAG_structure_type
                | constants::DW_TAG_enumeration_type
                | constants::DW_TAG_union_type
                | constants::DW_TAG_namespace
                | constants::DW_TAG_variable
                | constants::DW_TAG_subrange_type
        ) {
            continue;
        }
        block.push_str(&"  ".repeat(indent));
        block.push_str(tag_label(child.tag));
        if let Some(n) = &child.name {
            block.push_str(&format!(" name={n}"));
        }
        block.push_str(&format_attrs(&child.attrs));
        if let Some(d) = child.discr_ref {
            let discr = dies[&d].name.as_deref().unwrap_or("None");
            block.push_str(&format!(" discr={discr}"));
        }
        if child.type_ref.is_some() {
            block.push_str(&format!(
                " : {}",
                type_name(dies, names, child.type_ref, 0)
            ));
            refs.extend(child.type_ref);
        }
        block.push('\n');
        if matches!(
            child.tag,
            constants::DW_TAG_variant_part | constants::DW_TAG_variant
        ) {
            describe_children(dies, names, child, indent + 1, block, refs);
        }
    }
}

// TODO: Add comment
fn format_attrs(attrs: &[(DwAt, String)]) -> String {
    let mut out = String::new();
    for (name, value) in attrs {
        let label = name.static_string().unwrap_or("DW_AT_?");
        let label = label.strip_prefix("DW_AT_").unwrap_or(label);
        out.push_str(&format!(" {label}={value}"));
    }
    out
}

// TODO: Add comment
fn tag_label(tag: DwTag) -> &'static str {
    let s = tag.static_string().unwrap_or("DW_TAG_?");
    s.strip_prefix("DW_TAG_").unwrap_or(s)
}
