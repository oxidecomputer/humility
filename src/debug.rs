use std::mem::size_of;
use bitfield::bitfield;

bitfield! {
    #[derive(Copy, Clone)]
    pub struct DebugROMEntry(u32);
    impl Debug;
    pub offset, _: 31, 12;
    pub res1, _: 11, 9;
    pub domain, _: 8, 4;
    pub res2, _: 3;
    pub domvalid, _: 2;
    pub valid, _: 1;
    pub present, _: 0;
}

impl From<u32> for DebugROMEntry {
    fn from(value: u32) -> Self {
        Self(value)
    }
}

impl From<DebugROMEntry> for u32 {
    fn from(entry: DebugROMEntry) -> Self {
        entry.0
    }
}

impl DebugROMEntry {
    ///
    /// Determines the address of the corresponding table, given the address
    /// of the entry.
    ///
    fn address(&self, addr: u32) -> u32 {
        (addr as i32 + ((self.offset() << 12) as i32)) as u32
    }        
}

#[allow(non_snake_case)]
#[derive(Copy, Clone, Debug)]
pub struct DebugROMTable {
    pub SCS: Option<u32>,
    pub DWT: Option<u32>,
    pub FPB: Option<u32>,
    pub ITM: Option<u32>,
    pub TPIU: Option<u32>,
    pub ETM: Option<u32>,
}

pub fn read_debug_rom_table(core: &probe_rs::Core)
    -> Result<DebugROMTable, probe_rs::Error>
{
    let base = 0xe00f_f000u32;
    let mut table: Vec<Option<u32>> = vec![None; 6];

    for index in 0..table.len() {
        let addr = base + index as u32 * size_of::<u32>() as u32;
        let ent = DebugROMEntry(core.read_word_32(addr)?);

        if ent.present() {
            table[index] = Some(ent.address(base));
        }
    }

    Ok(DebugROMTable {
        SCS: table[0], 
        DWT: table[1], 
        FPB: table[2], 
        ITM: table[3], 
        TPIU: table[4], 
        ETM: table[5], 
    })
}
