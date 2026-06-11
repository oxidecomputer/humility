// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::core::Core;
use crate::hubris::OXIDE_NT_HUBRIS_REGISTERS;
use anyhow::{Context, Result, anyhow, bail};
use goblin::elf::Elf;
use humility_arch_arm::ARMRegister;
use num_traits::FromPrimitive;
use std::collections::{BTreeMap, HashMap};
use std::fs::File;
use std::io::Read;

pub struct DumpCore {
    contents: Vec<u8>,
    regions: BTreeMap<u32, (u32, usize)>,
    registers: Option<HashMap<ARMRegister, u32>>,
}

impl DumpCore {
    pub(crate) fn new(dump: &str) -> Result<DumpCore> {
        let mut file = File::open(dump)?;
        let mut regions = BTreeMap::new();

        let mut contents = Vec::new();
        file.read_to_end(&mut contents)?;

        let elf = Elf::parse(&contents).map_err(|e| {
            anyhow!("failed to parse {} as an ELF file: {}", dump, e)
        })?;

        for phdr in elf.program_headers.iter() {
            if phdr.p_type != goblin::elf::program_header::PT_LOAD {
                continue;
            }

            regions.insert(
                phdr.p_vaddr as u32,
                (phdr.p_memsz as u32, phdr.p_offset as usize),
            );
        }

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

        Ok(Self { contents, regions, registers })
    }

    fn check_offset(&self, addr: u32, rsize: usize, offs: usize) -> Result<()> {
        if rsize + offs <= self.contents.len() {
            return Ok(());
        }

        //
        // This really shouldn't happen, as it means that we have a defined
        // region in a program header for memory that wasn't in fact dumped.
        // Still, this might occur if the dump is truncated or otherwise
        // corrupt; offer a message pointing in that direction.
        //
        bail!(
            "0x{:x} is valid, but offset in dump \
            (0x{:x}) + size (0x{:x}) exceeds max (0x{:x}); \
            is the dump truncated or otherwise corrupt?",
            addr,
            offs,
            rsize,
            self.contents.len()
        );
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

#[rustfmt::skip::macros(bail)]
impl Core for DumpCore {
    fn info(&self) -> (String, Option<String>) {
        ("core dump".to_string(), None)
    }

    fn read_8(&mut self, addr: u32, data: &mut [u8]) -> Result<()> {
        let rsize = data.len();

        if let Some((&base, &(size, offset))) =
            self.regions.range(..=addr).next_back()
            && base <= addr
            && addr < (base + size)
        {
            if (addr - base) + rsize as u32 > size {
                //
                // The memory we want to read starts in this region but
                // exceeds its bounds -- but if all of the memory we want
                // is in fact represented in the dump, we don't want to
                // return failure!  We recurse into reading whatever won't
                // be satisfied by this region, and (if that succeeds)
                // fall through into reading this one.
                //
                let next = (size - (addr - base)) as usize;

                if next >= rsize
                    || self.read_8(base + size, &mut data[next..]).is_err()
                {
                    bail!(
                            "0x{:x} is valid, but relative to base (0x{:x}), \
                            offset (0x{:x}) exceeds max (0x{:x})",
                            addr, base, (addr - base) + rsize as u32, size
                        );
                }
            }

            let offs = offset + (addr - base) as usize;
            self.check_offset(addr, rsize, offs)?;

            data[..rsize].copy_from_slice(&self.contents[offs..rsize + offs]);
            return Ok(());
        }

        bail!("read of {} bytes from invalid address: 0x{:x}", rsize, addr);
    }

    fn read_reg(&mut self, reg: ARMRegister) -> Result<u32> {
        match &self.registers {
            Some(regs) => {
                if let Some(val) = regs.get(&reg) {
                    Ok(*val)
                } else {
                    bail!("register {} not found in dump", reg);
                }
            }
            None => bail!("dump does not include register info"),
        }
    }

    fn write_word_32(&mut self, _addr: u32, _data: u32) -> Result<()> {
        bail!("cannot write a word on a dump");
    }

    fn write_8(&mut self, _addr: u32, _data: &[u8]) -> Result<()> {
        bail!("cannot write a byte on a dump");
    }

    fn halt(&mut self) -> Result<()> {
        Ok(())
    }

    fn run(&mut self) -> Result<()> {
        Ok(())
    }

    fn is_dump(&self) -> bool {
        true
    }
}
