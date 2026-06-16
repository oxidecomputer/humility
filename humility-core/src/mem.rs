// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::{
    core::Core,
    hubris::{HubrisArchive, HubrisDataMap, OXIDE_NT_HUBRIS_REGISTERS},
};
use anyhow::{Context, Result, anyhow, bail};
use goblin::elf::Elf;
use humility_arch_arm::ARMRegister;
use num_traits::FromPrimitive;
use std::{collections::HashMap, fs::File, io::Read};

/// A core stored entirely in host memory
///
/// This may include a [`HubrisDataMap`] containing data from flash, a separate
/// [`HubrisDataMap`] storing other data from memory, and a map of registers.
pub struct InMemoryCore {
    /// Flash contents, loaded from an archive
    ///
    /// When reading, this is checked before `mem`
    flash: Option<HubrisDataMap>,

    /// Memory contents, loaded from other sources
    ///
    /// Note that when a dump is loaded, `mem` includes both flash and RAM
    /// (instead of storing data separately in `flash` and `mem`).  This only
    /// affects error messages.
    mem: Option<HubrisDataMap>,

    /// Register values (if present)
    registers: Option<HashMap<ARMRegister, u32>>,
}

impl Core for InMemoryCore {
    fn is_dump(&self) -> bool {
        true
    }

    fn is_archive(&self) -> bool {
        // An archive dump only has the flash data available
        self.flash.is_some() && self.mem.is_none() && self.registers.is_none()
    }

    fn is_net(&self) -> bool {
        false
    }

    fn read_8(&mut self, addr: u32, data: &mut [u8]) -> Result<()> {
        if self.flash.as_ref().is_some_and(|f| f.read_into(addr, data).is_ok())
        {
            return Ok(());
        }

        if self.mem.as_ref().is_some_and(|f| f.read_into(addr, data).is_ok()) {
            return Ok(());
        }

        let r = format!("read of {} bytes failed", data.len());
        match (self.flash.is_some(), self.mem.is_some()) {
            (true, true) => {
                bail!("{r}: could not find addr {addr:#08x} in flash or memory")
            }
            (true, false) => {
                bail!("{r}: could not find addr {addr:#08x} in flash")
            }
            (false, true) => {
                bail!("{r}: could not find addr {addr:#08x} in memory")
            }
            (false, false) => {
                bail!("{r}: core contains neither flash nor memory")
            }
        }
    }

    fn read_reg(&mut self, reg: ARMRegister) -> Result<u32> {
        match &self.registers {
            Some(regs) => {
                if let Some(val) = regs.get(&reg) {
                    Ok(*val)
                } else {
                    bail!("register {} not found in in-memory core", reg);
                }
            }
            None => bail!("in-memory core does not include register info"),
        }
    }

    fn write_word_32(&mut self, _addr: u32, _data: u32) -> Result<()> {
        bail!("cannot write a word on an in-memory core");
    }

    fn write_8(&mut self, _addr: u32, _data: &[u8]) -> Result<()> {
        bail!("cannot write a byte on an in-memory core");
    }

    fn halt(&mut self) -> Result<()> {
        Ok(())
    }

    fn run(&mut self) -> Result<()> {
        Ok(())
    }
}

impl InMemoryCore {
    pub fn from_dump(dump: &str) -> Result<Self> {
        let mut file = File::open(dump)?;
        let mut contents = Vec::new();
        file.read_to_end(&mut contents)?;

        let elf = Elf::parse(&contents).map_err(|e| {
            anyhow!("failed to parse {} as an ELF file: {}", dump, e)
        })?;

        let mut mem = HubrisDataMap::new();
        for phdr in elf.program_headers.iter() {
            if phdr.p_type != goblin::elf::program_header::PT_LOAD {
                continue;
            }
            mem.insert(
                phdr.p_vaddr as u32,
                contents[phdr.p_offset as usize..][..phdr.p_memsz as usize]
                    .to_vec(),
            )?;
        }
        mem.repack();

        let mut registers = None;
        if let Some(notes) = elf.iter_note_headers(&contents) {
            for note in notes {
                let note = note.context("failed to parse note")?;
                if note.n_type == OXIDE_NT_HUBRIS_REGISTERS {
                    if registers.is_some() {
                        bail!(
                            "multiple copies of OXIDE_NT_HUBRIS_REGISTERS \
                             found in dump"
                        );
                    }
                    registers = Some(load_registers(note.desc)?);
                }
            }
        }

        Ok(Self { flash: None, mem: Some(mem), registers })
    }

    pub fn from_archive(archive: &HubrisArchive) -> Result<Self> {
        Ok(Self {
            flash: Some(archive.flash_map()?),
            mem: None,
            registers: None,
        })
    }

    pub fn add_ram_region(
        &mut self,
        addr: u32,
        contents: Vec<u8>,
    ) -> Result<(), datamap::InsertError<u32>> {
        if self.mem.is_none() {
            self.mem = Some(HubrisDataMap::new())
        }
        self.mem.as_mut().unwrap().insert(addr, contents)
    }

    pub fn add_register(&mut self, reg: ARMRegister, val: u32) {
        if self.registers.is_none() {
            self.registers = Some(HashMap::new());
        }
        self.registers.as_mut().unwrap().insert(reg, val);
    }

    pub fn from_flash_map(flash: HubrisDataMap) -> Self {
        Self { flash: Some(flash), mem: None, registers: None }
    }
}

fn load_registers(r: &[u8]) -> Result<HashMap<ARMRegister, u32>> {
    if !r.len().is_multiple_of(8) {
        bail!("bad length {} in registers note", r.len());
    }
    let mut registers = HashMap::new();
    for (i, chunk) in r.chunks_exact(8).enumerate() {
        let (id, val) = chunk.split_at(4);
        // We unwrap here because it can only fail if the length is wrong,
        // but we've explicitly broken a chunk of 8 into two chunks of 4,
        // so a failure here would mean this code has been changed.
        let id = u32::from_le_bytes(id.try_into().unwrap());
        let val = u32::from_le_bytes(val.try_into().unwrap());

        let reg = match ARMRegister::from_u32(id) {
            Some(r) => r,
            None => {
                // This can totally happen if we encounter a future coredump
                // where we decided to store, say, additional MSRs or a
                // floating point register. Since this version of Humility
                // doesn't understand them, we'll just skip it.
                continue;
            }
        };

        if registers.insert(reg, val).is_some() {
            bail!("duplicate register {} ({}) at offset {}", reg, id, i * 8);
        }
    }

    Ok(registers)
}
