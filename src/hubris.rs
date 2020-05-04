/*
 * Copyright 2020 Oxide Computer Company
 */

use std::error::Error;
use std::fs;
use std::fs::File;
use std::fmt;
use std::io::BufReader;
use std::io::prelude::*;
use std::collections::HashSet;
use std::collections::HashMap;
use std::collections::BTreeMap;
use std::convert::Infallible;
use std::path::Path;
use std::str;

use goblin::elf::Elf;
use capstone::prelude::*;

#[derive(Debug)]
pub struct HubrisPackage {
    current: u32,                           // current object
    cs: capstone::Capstone,                 // Capstone library handle
    instrs: HashMap<u32, Vec<u8>>,          // instructions
    modules: BTreeMap<u32, (String, u32)>,  // modules
    symbols: BTreeMap<u32, (String, u32, HubrisGoff)>,
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

#[derive(Debug)]
pub struct HubrisError {
    errmsg: String,
}

impl<'a> From<&'a str> for HubrisError {
    fn from(msg: &'a str) -> HubrisError {
        msg.to_string().into()
    }
}

impl From<String> for HubrisError {
    fn from(errmsg: String) -> HubrisError {
        HubrisError { errmsg }
    }
}

impl fmt::Display for HubrisError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.errmsg)
    }
}

impl Error for HubrisError {
    fn description(&self) -> &str {
        &self.errmsg
    }
}

fn err<S: ToString>(msg: S) -> Box<dyn Error> {
    Box::new(HubrisError::from(msg.to_string()))
}

macro_rules! err {
    ($fmt:expr) => (
        Err(err($fmt))
    );
    ($fmt:expr, $($arg:tt)*) => (
        Err(err(&format!($fmt, $($arg)*)))
    )
}

impl fmt::Display for HubrisGoff {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.object > 0 {
            write!(f, "GOFF 0x{:x} in object {}", self.goff, self.object)
        } else {
            write!(f, "GOFF 0x{:x}", self.goff)
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

impl HubrisPackage {
    pub fn new() -> Result<HubrisPackage, Box<dyn Error>> {
        /*
         * Initialize Capstone, being sure to specify not only our architecture
         * but also that we are disassembling Thumb-2 -- and (importantly) to
         * allow M-profile instructions.
         */
        let cs = Capstone::new()
            .arm()
            .mode(arch::arm::ArchMode::Thumb)
            .extra_mode([arch::arm::ArchExtraMode::MClass].iter().map(|x| *x))
            .detail(true)
            .build();

        Ok(Self {
            cs: match cs {
                Ok(cs) => { cs }
                Err(err) => { 
                    return err!("failed to initialize disassembler: {}", err);
                }
            },
            current: 0,
            instrs: HashMap::new(),
            modules: BTreeMap::new(),
            symbols: BTreeMap::new()
        })
    }

    pub fn instr_len(
        &self,
        addr: u32
    ) -> Option<u32> {
        match self.instrs.get(&addr) {
            Some(instr) => {
                Some(instr.len() as u32)
            }
            None => None
        }
    }

    pub fn instr_mod(
        &self,
        addr: u32
    ) -> Option<&str> {
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

    pub fn instr_sym(
        &self,
        addr: u32
    ) -> Option<(&str, u32)> {
        if let Some(sym) = self.symbols.range(..=addr).next_back() {
            if addr < *sym.0 + (sym.1).1 {
                Some((&(sym.1).0, *sym.0 as u32))
            } else {
                None
            }
        } else {
            None
        }
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

        HubrisGoff {
            object: self.current,
            goff: goff
        }
    }

    fn dwarf_subprogram<'a, R: gimli::Reader<Offset = usize>>(
        &mut self,
        dwarf: &'a gimli::Dwarf<gimli::EndianSlice<gimli::LittleEndian>>,
        unit: &gimli::Unit<R>,
        entry: &gimli::DebuggingInformationEntry<
            gimli::EndianSlice<gimli::LittleEndian>,
            usize,
        >,
    ) -> Result<(), Box<dyn Error>> {
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
                (
                    gimli::constants::DW_AT_linkage_name,
                    _
                ) => {
                    _linkage_name = dwarf_name(dwarf, attr.value());
                }
                (
                    gimli::constants::DW_AT_name,
                    _
                ) => {
                    name = dwarf_name(dwarf, attr.value());
                }
                _ => {}
            }
        }

        if let (Some(name), Some(addr), Some(len)) = (name, addr, len) {
            if addr != 0 {
                self.symbols.insert(
                    addr as u32,
                    (String::from(name), len as u32, goff)
                );
            }
        }

        Ok(())
    }

    fn load_object_dwarf(
        &mut self,
        buffer: &[u8],
        elf: &goblin::elf::Elf,
    ) -> Result<(), Box<dyn Error>> {
        // Load all of the sections. This "load" operation just gets the data in
        // RAM -- since we've already loaded the Elf file, this can't fail.
        let dwarf = gimli::Dwarf::<&[u8]>::load::<_, _, Infallible>(
            // Load the normal DWARF section(s) from our Elf image.
            |id| {
                let sec_result = elf
                    .section_headers
                    .iter()
                    .filter(|sh| {
                        if let Some(Ok(name)) = elf.shdr_strtab.get(sh.sh_name)
                        {
                            name == id.name()
                        } else {
                            false
                        }
                    })
                    .next();
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
/*
                    gimli::constants::DW_TAG_formal_parameter => {
                        self.dwarf_formal_parameter(&dwarf, &unit, &entry,
                            stack[(depth - 1) as usize]
                        )?;
                    }

                    gimli::constants::DW_TAG_variable => {
                        self.dwarf_variable(&unit, &entry, depth)?;
                    }

                    gimli::constants::DW_TAG_inlined_subroutine => {
                        self.dwarf_inlined(&dwarf, &unit, &entry, depth)?;
                    }
*/
                    gimli::constants::DW_TAG_subprogram => {
                        self.dwarf_subprogram(&dwarf, &unit, &entry)?;
                    }
                    _ => {}
                }
            }
        }

        self.current += 1;

        Ok(())
    }

    fn load_object(
        &mut self,
        directory: &str,
        object: &str
    ) -> Result<(), Box<dyn Error>> {
        let p = Path::new(directory).join(object);

        let buffer = fs::read(p)?;

        let elf = Elf::parse(&buffer).map_err(|e| {
            err(format!("unrecognized ELF object: {}: {}", object, e))
        })?;

        let text = elf
            .section_headers
            .iter()
            .filter(|sh| {
                if let Some(Ok(name)) = elf.shdr_strtab.get(sh.sh_name) {
                    name == ".text"
                } else {
                    false
                }
            }).next();

        let textsec = match text {
            None => {
                return err!("couldn't find text in ELF object \"{}\"", object);
            }
            Some(sec) => { sec }
        };

        let offset = textsec.sh_offset as usize;
        let size = textsec.sh_size as usize;

        let t = buffer.get(offset..offset + size).unwrap();
        self.load_object_dwarf(&buffer, &elf)?;

        let instrs = match self.cs.disasm_all(t, textsec.sh_addr) {
            Ok(instrs) => { instrs }
            Err(err) => {
                return err!(
                    "failed to disassemble \"{}\" (offs 0x{:08x}, {}): {}",
                    object, offset, size, err
                );
            }
        };

        let mut last: (u32, usize) = (0, 0);

        for instr in instrs.iter() {
            let addr: u32 = instr.address() as u32;

            if self.instrs.contains_key(&addr) {
                return err!("address 0x{:08x} is duplicated!", addr);
            }

            let b = instr.bytes();
            let mut v = Vec::with_capacity(b.len());

            for i in 0..b.len() {
                v.push(b[i]);
            }

            last = (addr, b.len());

            self.instrs.insert(addr, v);
        }

        /*
         * Regrettably, if Capstone flies off the rails while disassembling,
         * it won't flag an error -- it will simply stop short.  Check to see
         * if we are in this case and explicitly fail.
         */
        if last.0 + last.1 as u32 != textsec.sh_addr as u32 + size as u32 {
            return err!(
                concat!(
                    "short disassembly for \"{}\": ",
                    "stopped at 0x{:x}, expected to go to 0x{:x}"
                ),
                object, last.0, textsec.sh_addr as u32 + size as u32
            );
        }

        self.modules.insert(
            textsec.sh_addr as u32,
            (String::from(object), size as u32)
        );

        Ok(())
    }

    pub fn load(
        &mut self, 
        directory: &str
    ) -> Result<(), Box<dyn Error>> {
        println!("loading package at {}", directory);

        let file = File::open(directory)?;
        let metadata = file.metadata()?;
        let mapfile = directory.clone().to_owned() + "/map.txt";
        let expected = &["ADDRESS", "END", "SIZE", "FILE"];

        if !metadata.is_dir() {
            return err!("package must be a directory");
        }

        let map = match File::open(mapfile) {
            Ok(map) => map,
            Err(e) => {
                return err!("couldn't find map.txt: {}", e);
            }
        };

        let f = BufReader::new(map);
        let mut iter = f.lines();

        match iter.next() {
            Some(header) => {
                let h = header.unwrap();
                let headers: Vec<&str> = h.split_ascii_whitespace().collect();

                if headers.len() != expected.len() {
                    return err!("bad headers: found {}, expected {}",
                        headers.len(),
                        expected.len()
                    );
                }

                for i in 0..expected.len() {
                    if headers[i] == expected[i] {
                        continue;
                    }

                    return err!("bad header: found \"{}\", expected \"{}\"",
                        headers[i],
                        expected[i]
                    );
                }
            }

            None => {
                return err!("map file empty or otherwise missing a header");
            }
        };

        let mut lineno = 1;
        let mut seen = HashSet::new();

        for line in iter {
            let l = line.unwrap();
            let fields: Vec<&str> = l.split_ascii_whitespace().collect();

            lineno += 1;

            if fields.len() < expected.len() {
                return err!("short line at {}", lineno);
            }

            let file = fields[3];
            let pieces: Vec<&str> = file.split('/').collect();

            /*
             * We expect a 3 element path.
             */
            if pieces.len() != 3 {
                return err!("bad object \"{}\" on line {}", file, lineno);
            }

            let object = pieces[2].to_owned();

            if !seen.contains(&object) {
                self.load_object(directory, &object)?;
                seen.insert(object);
            }
        }

        println!("{:?}", seen);
        Ok(())
    }
}
