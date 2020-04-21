use std::mem::size_of;
use bitfield::bitfield;

pub trait Register: Clone + From<u32> + Into<u32> + Sized + std::fmt::Debug {
    const ADDRESS: u32;
    const NAME: &'static str;
}

#[macro_export]
macro_rules! register {
    ($reg:ty, $addr:expr, $($arg:tt)*) => (
        bitfield!(
            $($arg)*
        );

        impl From<u32> for $reg {
            fn from(value: u32) -> Self {
                Self(value)
            }
        }

        impl From<$reg> for u32 {
            fn from(reg: $reg) -> Self {
                reg.0
            }
        }

        impl Register for $reg {
            const ADDRESS: u32 = $addr;
            const NAME: &'static str = "$reg";
        }

        impl $reg {
            pub fn read(
                core: &probe_rs::Core
            ) -> Result<$reg, probe_rs::Error> {
                Ok(Self(core.read_word_32($addr)?))
            }

            pub fn write(
                &self,
                core: &probe_rs::Core
            ) -> Result<(), probe_rs::Error> {
                core.write_word_32($addr, self.0.into())?;
                Ok(())
            }
        }
    )
}

register!(DEMCR, 0xe000_edfc,
    #[derive(Copy, Clone)]
    pub struct DEMCR(u32);
    impl Debug;
    /// Global enable for DWT and ITM features
    pub trcena, set_trcena: 24;
    /// DebugMonitor semaphore bit
    pub mon_req, set_mon_req: 19;
    /// Step the processor?
    pub mon_step, set_mon_step: 18;
    /// Sets or clears the pending state of the DebugMonitor exception
    pub mon_pend, set_mon_pend: 17;
    /// Enable the DebugMonitor exception
    pub mon_en, set_mon_en: 16;
    /// Enable halting debug trap on a HardFault exception
    pub vc_harderr, set_vc_harderr: 10;
    /// Enable halting debug trap on a fault occurring during exception entry
    /// or exception return
    pub vc_interr, set_vc_interr: 9;
    /// Enable halting debug trap on a BusFault exception
    pub vc_buserr, set_vc_buserr: 8;
    /// Enable halting debug trap on a UsageFault exception caused by a state
    /// information error, for example an Undefined Instruction exception
    pub vc_staterr, set_vc_staterr: 7;
    /// Enable halting debug trap on a UsageFault exception caused by a
    /// checking error, for example an alignment check error
    pub vc_chkerr, set_vc_chkerr: 6;
    /// Enable halting debug trap on a UsageFault caused by an access to a
    /// Coprocessor
    pub vc_nocperr, set_vc_nocperr: 5;
    /// Enable halting debug trap on a MemManage exception.
    pub vc_mmerr, set_vc_mmerr: 4;
    /// Enable Reset Vector Catch
    pub vc_corereset, set_vc_corereset: 0;
);

register!(DBGMCU_CR, 0xe004_2004,
    #[derive(Copy, Clone)]
    #[allow(non_camel_case_types)]
    pub struct DBGMCU_CR(u32);
    impl Debug;
    pub dbg_tim11_stop, _: 30;
    pub dbg_tim10_stop, _: 29;
    pub dbg_tim9_stop, _: 28;
    pub dbg_tim14_stop, _: 27;
    pub dbg_tim13_stop, _: 26;
    pub dbg_tim12_stop, _: 25;
    pub dbg_can2_stop, _: 21;
    pub dbg_tim7_stop, _: 20;
    pub dbg_tim6_stop, _: 19;
    pub dbg_tim5_stop, _: 18;
    pub dbg_tim8_stop, _: 17;
    pub dbg_i2c2_timeout, _: 16;
    pub dbg_i2c1_timeout, _: 15;
    pub dbg_can1_stop, _: 14;
    pub dbg_tim4_stop, _: 13;
    pub dbg_tim3_stop, _: 12;
    pub dbg_tim2_stop, _: 11;
    pub dbg_tim1_stop, _: 10;
    pub dbg_wwdg_stop, _: 9;
    pub dbg_iwdg_stop, _: 8;
    pub trace_mode, set_trace_mode: 7, 6;
    pub trace_ioen, set_trace_ioen: 5;
    pub dbg_standby, _: 3;
    pub dbg_stop, _: 2;
    pub dbg_sleep, _: 1;
);

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

pub fn dbgmcu_cr_trace_enable(core: &probe_rs::Core)
    -> Result<(), probe_rs::Error>
{
    let val = DBGMCU_CR(core.read_word_32(DBGMCU_CR::ADDRESS)?);

    if !val.trace_ioen() {
        let mut enabled = val;

        enabled.set_trace_ioen(true);
        enabled.set_trace_mode(0);      /* 00 = asynchronous mode */

        core.write_word_32(DBGMCU_CR::ADDRESS, enabled.into())?;
    }

    Ok(())
}

