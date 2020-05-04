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
use std::path::Path;

use goblin::elf::Elf;
use capstone::prelude::*;

#[derive(Debug)]
pub struct HubrisPackage {
    cs: capstone::Capstone,
    instrs: HashMap<u32, Vec<u8>>,
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
            instrs: HashMap::new()
        })
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

        Ok(())
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
