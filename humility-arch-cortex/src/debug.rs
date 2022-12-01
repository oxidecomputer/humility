// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::{anyhow, Result};

use bitfield::bitfield;
use humility::core::Core;
use humility::hubris::*;

pub trait Register:
    Clone + From<u32> + Into<u32> + Sized + std::fmt::Debug
{
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
                core: &mut dyn Core
            ) -> anyhow::Result<$reg> {
                Ok(Self(core.read_word_32($addr)?))
            }

            pub fn write(
                &self,
                core: &mut dyn Core
            ) -> anyhow::Result<()> {
                core.write_word_32($addr, self.0.into())?;
                Ok(())
            }
        }
    )
}

#[macro_export]
macro_rules! register_offs {
    ($reg:ident, $offs:expr, $($arg:tt)*) => (
        paste::item! {
            #[allow(non_camel_case_types)]
            #[allow(non_snake_case)]
            mod [<mod_ $reg>] {
                use bitfield::bitfield;

                bitfield!(
                    #[derive(Copy, Clone)]
                    pub struct $reg(u32);
                    impl Debug;
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
            }

            #[derive(Copy, Clone, Debug)]
            #[allow(non_camel_case_types)]
            pub struct $reg {
                pub base: u32,
                pub register: [<mod_ $reg>]::$reg
            }

            impl $reg {
                #[allow(dead_code)]
                pub fn read(
                    core: &mut dyn Core,
                    base: u32
                ) -> anyhow::Result<$reg> {
                    Ok(Self {
                        base,
                        register: [<mod_ $reg>]::$reg(
                            core.read_word_32(base + $offs)?
                        )
                    })
                }

                #[allow(dead_code)]
                pub fn write(
                    &self,
                    core: &mut dyn Core
                ) -> anyhow::Result<()> {
                    core.write_word_32(
                        self.base + $offs,
                        self.register.into()
                    )
                }

                #[allow(dead_code)]
                pub fn address(base: u32) -> u32 { base + $offs }
            }
        }
    )
}

register!(CPUID, 0xe000_ed00,
    #[derive(Copy, Clone)]
    pub struct CPUID(u32);
    impl Debug;
    pub implementer, _: 31, 24;
    pub variant, _: 23, 20;
    pub partno, _: 15, 4;
    pub revision, _: 3, 0;
);

register!(SFSR, 0xe000_ede4,
    #[derive(Copy, Clone)]
    pub struct SFSR(u32);
    pub lserr, _: 7;
    pub sfarvalid, _: 6;
    pub lsperr, _: 5;
    pub invtran, _: 4;
    pub auviol, _: 3;
    pub inver, _: 2;
    pub invis, _: 1;
    pub invep, _: 0;
);

impl std::fmt::Debug for SFSR {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.lserr() {
            write!(f, "Lazy state error, ")?;
        }
        if self.lsperr() {
            write!(f, "Error preserving floting point state, ")?;
        }
        if self.invtran() {
            write!(f, "Invalid transition from S -> NS world, ")?;
        }
        if self.auviol() {
            write!(f, "SAU Access violation,  ")?;
        }
        if self.inver() {
            write!(f, "Invalid exception return (check your stack), ")?;
        }
        if self.invis() {
            write!(f, "Invalid integrity signature, ")?;
        }
        if self.invep() {
            write!(f, "Invalid transition from NS -> S world")?;
        }
        Ok(())
    }
}

impl SFSR {
    pub fn has_fault(&self) -> bool {
        self.0 != 0
    }
}

register!(MMFAR, 0xe000_ed34,
    #[derive(Copy, Clone)]
    pub struct MMFAR(u32);
    impl Debug;
    pub address, _: 31, 0;
);

register!(BFAR, 0xe000_ed38,
    #[derive(Copy, Clone)]
    pub struct BFAR(u32);
    impl Debug;
    pub address, _: 31, 0;
);

register!(SFAR, 0xe000_ede8,
    #[derive(Copy, Clone)]
    pub struct SFAR(u32);
    impl Debug;
    pub address, _: 31, 0;
);

register!(CFSR, 0xe000_ed28,
    #[derive(Copy, Clone)]
    pub struct CFSR(u32);
    impl Debug;
    pub ufsr, _: 31, 16;
    pub bfsr, _: 15, 8;
    pub mmfsr, _: 7, 0;
);

impl CFSR {
    pub fn has_fault(&self) -> bool {
        self.0 != 0
    }

    pub fn get_ufsr(&self) -> Option<UFSR> {
        let ufsr = UFSR::from(self.ufsr() as u16);
        if ufsr.has_fault() {
            Some(ufsr)
        } else {
            None
        }
    }

    pub fn get_bfsr(&self) -> Option<BFSR> {
        let bfsr = BFSR::from(self.bfsr() as u8);
        if bfsr.has_fault() {
            Some(bfsr)
        } else {
            None
        }
    }

    pub fn get_mmfsr(&self) -> Option<MMFSR> {
        let mmfsr = MMFSR::from(self.mmfsr() as u8);
        if mmfsr.has_fault() {
            Some(mmfsr)
        } else {
            None
        }
    }
}

bitfield! {
    #[derive(Copy, Clone)]
    pub struct UFSR(u16);
    divbyzero, _: 9;
    unaligned, _: 8;
    stkof, _: 4;
    nocp, _: 3;
    invpc, _: 2;
    invstate, _: 1;
    undefinstr, _: 0;
}

impl std::fmt::Debug for UFSR {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.divbyzero() {
            write!(f, "Division by Zero, ")?;
        }
        if self.unaligned() {
            write!(f, "Unaligned access, ")?;
        }
        if self.stkof() {
            write!(f, "Stack overflow, ")?;
        }
        if self.nocp() {
            write!(f, "No coprocessor (disabled or not present) ")?;
        }
        if self.invpc() {
            write!(f, "Invalid PC, ")?;
        }
        if self.invstate() {
            write!(f, "Invalid state (check the THUMB bit), ")?;
        }
        if self.undefinstr() {
            write!(f, "Undefined instruction")?;
        }
        Ok(())
    }
}

impl From<u16> for UFSR {
    fn from(value: u16) -> Self {
        Self(value)
    }
}

impl UFSR {
    pub fn has_fault(&self) -> bool {
        self.0 != 0
    }
}

bitfield! {
    pub struct BFSR(u8);
    pub ibuserr, _: 0;
    pub preciserr, _: 1;
    pub impreciserr, _: 2;
    pub unstkerr, _: 3;
    pub stkerr, _: 4;
    pub lsperr, _: 5;
    pub bfarvalid, _: 7;
}

impl std::fmt::Debug for BFSR {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.ibuserr() {
            write!(f, "Bus Error on Instruction, ")?;
        }
        if self.preciserr() {
            write!(f, "Precise data access error, ")?;
        }
        if self.impreciserr() {
            write!(f, "Imprecise data access error, ")?;
        }
        if self.unstkerr() {
            write!(f, "Bus Error during exception return unstacking (check your  stack usage), ")?;
        }
        if self.stkerr() {
            write!(f, "Bus Error during exception entry stacking (check your stack usage), ")?;
        }
        if self.lsperr() {
            write!(f, "Bus Error during FP lazy state preservation")?;
        }

        Ok(())
    }
}

impl From<u8> for BFSR {
    fn from(value: u8) -> Self {
        Self(value)
    }
}

impl BFSR {
    pub fn has_fault(&self) -> bool {
        self.0 != 0
    }
}

bitfield! {
    pub struct MMFSR(u8);
    pub mmfarvalid, _: 7;
    pub mlsperr, _: 5;
    pub mstkerr, _: 4;
    pub munstkerr, _: 3;
    pub daccviol, _: 1;
    pub iaccviol, _: 0;
}

impl std::fmt::Debug for MMFSR {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.mlsperr() {
            write!(f, "MM Fault during FP state preservation, ")?;
        }
        if self.mstkerr() {
            write!(f, "MM Fault during exception entry stacking (check your stack usage), ")?;
        }
        if self.munstkerr() {
            write!(
                f,
                "MM fault during exception return (check your stack usage), "
            )?;
        }
        if self.daccviol() {
            write!(f, "MM Fault from data access, ")?;
        }
        if self.iaccviol() {
            write!(f, "MM fault from instruction access")?;
        }
        Ok(())
    }
}

impl From<u8> for MMFSR {
    fn from(value: u8) -> Self {
        Self(value)
    }
}

impl MMFSR {
    pub fn has_fault(&self) -> bool {
        self.0 != 0
    }
}

register!(HFSR, 0xe000_ed2c,
    #[derive(Copy, Clone)]
    pub struct HFSR(u32);
    impl Debug;
    pub debug_fault, _: 31;
    pub forced_fault, _: 30;
    pub vector_fault, _: 1;
);

//
// Debug Fault Status Register
//
register!(DFSR, 0xe000_ed30,
    #[derive(Copy, Clone)]
    pub struct DFSR(u32);
    impl Debug;
    pub external, _: 4;
    pub vector_catch, _: 3;
    pub watchpoint, _: 2;
    pub breakpoint, _: 1;
    pub halted, _: 0;
);

//
// Debug Halting Control Status Register
//
register!(DHCSR, 0xe000_edf0,
    #[derive(Copy, Clone)]
    pub struct DHCSR(u32);
    impl Debug;
    pub restart_status, _: 26;
    pub reset_status, _: 25;
    pub retire_status, _: 24;
    pub secure_debug_enabled, _: 20;
    pub locked_up, _: 19;
    pub sleeping, _: 18;
    pub halted, _: 17;
);

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

//
// Media and FP Feature Register 0
//
register!(MVFR0, 0xe000_ef40,
    #[derive(Copy, Clone)]
    pub struct MVFR0(u32);
    impl Debug;
    pub fp_rounding_modes, _: 31, 28;
    pub short_vectors, _: 27, 24;
    pub squre_root, _: 23, 20;
    pub divide, _: 19, 16;
    pub fp_exception_trapping, _: 15, 12;
    pub double_precision, _: 11, 8;
    pub single_precision, _: 7, 4;
    pub simd_registers, _: 3, 0;
);

register!(STM32F4_DBGMCU_IDCODE, 0xe004_2000,
    #[derive(Copy, Clone)]
    #[allow(non_camel_case_types)]
    pub struct STM32F4_DBGMCU_IDCODE(u32);
    impl Debug;
    pub rev_id, _: 31, 16;
    pub dev_id, _: 11, 0;
);

register!(STM32F4_DBGMCU_CR, 0xe004_2004,
    #[derive(Copy, Clone)]
    #[allow(non_camel_case_types)]
    pub struct STM32F4_DBGMCU_CR(u32);
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

register!(STM32G0X1_DBGMCU_IDCODE, 0x4001_5800,
    #[derive(Copy, Clone)]
    #[allow(non_camel_case_types)]
    pub struct STM32G0X1_DBGMCU_IDCODE(u32);
    impl Debug;
    pub rev_id, _: 31, 16;
    pub dev_id, _: 11, 0;
);

register!(STM32G0X1_DBGMCU_CR, 0x4001_5804,
    #[derive(Copy, Clone)]
    #[allow(non_camel_case_types)]
    pub struct STM32G0X1_DBGMCU_CR(u32);
    impl Debug;
    pub dbg_standby, _: 2;
    pub dbg_stop, _: 1;
);

register!(STM32G0X1_DBGMCU_APBFZ1, 0x4001_5808,
    #[derive(Copy, Clone)]
    #[allow(non_camel_case_types)]
    pub struct STM32G0X1_DBGMCU_APBFZ1(u32);
    impl Debug;
    pub dbg_lptim1_stop, _: 31;
    pub dbg_lptim2_stop, _: 30;
    pub dbg_i2c2_smbus_timeout, _: 22;
    pub dbg_i2c1_smbus_timeout, _: 21;
    pub dbg_iwdg_stop, _: 12;
    pub dbg_wwdg_stop, _: 11;
    pub dbg_rtc_stop, _: 10;
    pub dbg_tim7_stop, _: 5;
    pub dbg_tim6_stop, _: 4;
    pub dbg_tim4_stop, _: 2;
    pub dbg_tim3_stop, _: 1;
    pub dbg_tim2_stop, _: 0;
);

register!(STM32G0X1_DBGMCU_APBFZ2, 0x4001_580C,
    #[derive(Copy, Clone)]
    #[allow(non_camel_case_types)]
    pub struct STM32G0X1_DBGMCU_APBFZ2(u32);
    impl Debug;
    pub dbg_tim17_stop, _: 18;
    pub dbg_tim16_stop, _: 17;
    pub dbg_tim15_stop, _: 16;
    pub dbg_tim14_stop, _: 15;
    pub dbg_tim1_stop, _: 11;
);

register!(STM32H7_DBGMCU_IDC, 0x5c00_1000,
    #[derive(Copy, Clone)]
    #[allow(non_camel_case_types)]
    pub struct STM32H7_DBGMCU_IDC(u32);
    impl Debug;
    pub rev_id, _: 31, 16;
    pub dev_id, _: 11, 0;
);

register!(STM32H7_DBGMCU_CR, 0x5c00_1004,
    #[derive(Copy, Clone)]
    #[allow(non_camel_case_types)]
    pub struct STM32H7_DBGMCU_CR(u32);
    impl Debug;

    /// External trigger output enabled
    pub trgoen, _: 28;

    /// SmartRun domain debug clock enabled
    pub srdbgcken, set_srdbgcken: 22;

    /// CPU domain debug clock enabled
    pub cddbgcken, set_cddbgcken: 21;

    /// Trace clock enable
    pub traceclken, set_traceclken: 20;

    /// Debug in SmartRun domain Standby mode
    pub dbgstby_srd, _: 8;

    /// Debug in SmartRun domain Stop mode
    pub dbgstop_srd, _: 7;

    /// CPU domain debug in Standby mode
    pub dbgstby_cd, _: 2;

    /// CPU domain debug in Stop mode
    pub dbgstop_cd, _: 1;

    /// CPU domain debug in Sleep mode
    pub dbgsleep_cd, _: 0;
);

register!(LPC55_SYSCON_AHBCLKCTRL0, 0x5000_0200,
    #[derive(Copy, Clone)]
    #[allow(non_camel_case_types)]
    pub struct LPC55_SYSCON_AHBCLKCTRL0(u32);
    impl Debug;
    pub adc, _: 27;
    pub mailbox, _: 26;
    pub rtc, _: 23;
    pub wwdt, _: 22;
    pub crcgen, _: 21;
    pub dma0, _: 20;
    pub gint, _: 19;
    pub pint, _: 18;
    pub gpio3, _: 17;
    pub gpio2, _: 16;
    pub gpio1, _: 15;
    pub gpio0, _: 14;
    pub iocon, _: 13;
    pub mux, _: 11;
    pub fmc, _: 8;
    pub flash, _: 7;
    pub sram_ctrl4, _: 6;
    pub sram_ctrl3, _: 5;
    pub sram_ctrl2, _: 4;
    pub sram_ctrl1, _: 3;
    pub rom, _: 1;
);

register!(LPC55_SYSCON_TRACECLKSEL, 0x5000_0268,
    #[derive(Copy, Clone)]
    #[allow(non_camel_case_types)]
    pub struct LPC55_SYSCON_TRACECLKSEL(u32);
    impl Debug;
    pub sel, set_sel: 2, 0;
);

register!(LPC55_SYSCON_TRACECLKDIV, 0x5000_0308,
    #[derive(Copy, Clone)]
    #[allow(non_camel_case_types)]
    pub struct LPC55_SYSCON_TRACECLKDIV(u32);
    impl Debug;
    pub reqflag, set_reqflag: 31;
    pub halt, set_halt: 30;
    pub reset, set_reset: 29;
    pub div, _: 7, 0;
);

register!(LPC55_SYSCON_DEVID, 0x5000_0ff8,
    #[derive(Copy, Clone)]
    #[allow(non_camel_case_types)]
    pub struct LPC55_SYSCON_DEVID(u32);
    impl Debug;
    pub rom_revision, _: 23, 20;
);

register!(LPC55_SYSCON_DIEID, 0x5000_0ffc,
    #[derive(Copy, Clone)]
    #[allow(non_camel_case_types)]
    pub struct LPC55_SYSCON_DIEID(u32);
    impl Debug;
    pub rev_id, _: 3, 0;
);

register!(LPC55_ROM_PATCH_VERSION, 0x9fc14,
    #[derive(Copy, Clone)]
    #[allow(non_camel_case_types)]
    pub struct LPC55_ROM_PATCH_VERSION(u32);
    impl Debug;
    pub patch_version, _: 3, 0;
);

#[derive(Copy, Clone, Debug, FromPrimitive, PartialEq, Eq)]
pub enum ARMCore {
    CortexA5 = 0xc05,
    CortexA7 = 0xc07,
    CortexA8 = 0xc08,
    CortexA9 = 0xc09,
    CortexA12 = 0xc0d,
    CortexA15 = 0xc0f,
    CortexA17 = 0xc0e,
    CortexR4 = 0xc14,
    CortexR5 = 0xc15,
    CortexR7 = 0xc17,
    CortexR8 = 0xc18,
    CortexM0 = 0xc20,
    CortexM0Plus = 0xc60,
    CortexM1 = 0xc21,
    CortexM3 = 0xc23,
    CortexM4 = 0xc24,
    CortexM7 = 0xc27,
    CortexM23 = 0xd20,
    CortexM33 = 0xd21,
    CortexA32 = 0xd01,
    CortexA53 = 0xd03,
    CortexA35 = 0xd04,
    CortexA55 = 0xd05,
    CortexA57 = 0xd07,
    CortexA72 = 0xd08,
    CortexA73 = 0xd09,
    CortexA75 = 0xd0a,
    CortexR52 = 0xd13,
}

impl ARMCore {
    pub fn has_tz(&self) -> bool {
        matches!(*self, ARMCore::CortexM33)
    }
}

pub fn corename(partno: ARMCore) -> String {
    match partno {
        ARMCore::CortexA5 => "Cortex-A5",
        ARMCore::CortexA7 => "Cortex-A7",
        ARMCore::CortexA8 => "Cortex-A8",
        ARMCore::CortexA9 => "Cortex-A9",
        ARMCore::CortexA12 => "Cortex-A12",
        ARMCore::CortexA15 => "Cortex-A15",
        ARMCore::CortexA17 => "Cortex-A17",
        ARMCore::CortexR4 => "Cortex-R4",
        ARMCore::CortexR5 => "Cortex-R5",
        ARMCore::CortexR7 => "Cortex-R7",
        ARMCore::CortexR8 => "Cortex-R8",
        ARMCore::CortexM0 => "Cortex-M0",
        ARMCore::CortexM0Plus => "Cortex-M0+",
        ARMCore::CortexM1 => "Cortex-M1",
        ARMCore::CortexM3 => "Cortex-M3",
        ARMCore::CortexM4 => "Cortex-M4",
        ARMCore::CortexM7 => "Cortex-M7",
        ARMCore::CortexM23 => "Cortex-M23",
        ARMCore::CortexM33 => "Cortex-M33",
        ARMCore::CortexA32 => "Cortex-A32",
        ARMCore::CortexA53 => "Cortex-A53",
        ARMCore::CortexA35 => "Cortex-A35",
        ARMCore::CortexA55 => "Cortex-A55",
        ARMCore::CortexA57 => "Cortex-A57",
        ARMCore::CortexA72 => "Cortex-A72",
        ARMCore::CortexA73 => "Cortex-A73",
        ARMCore::CortexA75 => "Cortex-A75",
        ARMCore::CortexR52 => "Cortex-R52",
    }
    .to_string()
}

pub fn stm32_chipname(partno: u32) -> String {
    match partno {
        0x410 => "STM32F10xx8/STM32F10xxB",
        0x411 => "STM32F2xx/STM32F4xx",
        0x412 => "STM32F10xx4/STM32F10xx6",
        0x413 => "STM32F40x/STM32F41x",
        0x414 => "STM32F10xxC/STM32F10xxD/STM32F10xxE",
        0x415 => "STM32L4x1/STM32L4x5/STM32L4x6",
        0x416 => "STM32L151xx/STM32L152xx/STM32L100xx",
        0x417 => "STM32L0x1/STM32L0x2/STM32L0x3",
        0x418 => "STM32F105xx/STM32F107xx",
        0x419 => "STM32F42x/STM32F43x",
        0x420 => "STM32F100x4/STM32F100x6/STM32F100x8/STM32F100xB",
        0x421 => "STM32F446xx",
        0x422 => "STM32F30x",
        0x423 => "STM32F401x",
        0x425 => "STM32L0x1/STM32L0x2/STM32L0x3",
        0x427 => "STM32L151x/STM32L152xx/STM32L162xD/STM32L100xC",
        0x428 => "STM32F100xC/STM32F100xD/STM32F100xE",
        0x429 => "STM32L151xx/STM32L152xx/STM32L100xx",
        0x430 => "STM32F10xxF/STM32F10xxG",
        0x431 => "STM32F411x",
        0x432 => "STM32F37x",
        0x433 => "STM32F401x",
        0x434 => "STM32F469/STM32F479",
        0x435 => "STM32L4x2/STM32L431/STM32L433/STM32L443",
        0x436 => "STM32L151x/STM32L152x/STM32L162xD/STM32L100xC",
        0x437 => "STM32L152E",
        0x438 => "STM32F334xx/STM32F303x6/STM32F303x8/STM32F328",
        0x439 => "STM32F30x",
        0x440 => "STM32F05x/STM32F030x8",
        0x441 => "STM32F412",
        0x442 => "STM32F09x/STM32F030xC",
        0x444 => "STM32F03x/STM32F030x4/STM32F030x6",
        0x445 => "STM32F04x/STM32F070x6",
        0x446 => "STM32F303xD/STM32F303xE/STM32F398xE",
        0x447 => "STM32L0x1/STM32L0x2/STM32L0x3",
        0x448 => "STM32F07x/STM32F070xB",
        0x449 => "STM32F7x5/STM32F7x6",
        0x450 => "STM32H7",
        0x451 => "STM32F7x7/STM32F7x9",
        0x452 => "STM32F72x/STM32F73x",
        0x457 => "STM32L0x1/STM32L0x2/STM32L0x3",
        0x458 => "STM32F410x",
        0x460 => "STM32G0x0",
        0x461 => "STM32L496/STM32L4A6",
        0x462 => "STM32L451/STM32L452/STM32L462",
        0x463 => "STM32F413",
        0x464 => "STM32L412/STM32L422",
        0x466 => "STM32G0x0",
        0x467 => "STM32G0x1",
        0x468 => "STM32G4xxx6/STM32G4xxx8/STM32G4xxxB",
        0x469 => "STM32G4xxxC/STM32G4xxxE",
        0x470 => "STM32L4R/STM32L4S",
        0x472 => "STM32L552xx/STM32L562xx",
        0x480 => "STM32H7A3/STM32H7B3/STM32H7B0",
        0x495 => "STM32WB55xx",
        _ => {
            return format!("<Unknown STM32 chip 0x{:x}>", partno);
        }
    }
    .to_string()
}

pub fn swoscaler(hubris: &HubrisArchive, core: &mut dyn Core) -> Result<u16> {
    let debug_clock_mhz = 2_000_000;

    match hubris.clock(core)? {
        None => Err(anyhow!(
            "clock couldn't be determined; set clock scaler explicitly"
        )),
        Some(clock) => Ok(((clock * 1000) / debug_clock_mhz) as u16 - 1),
    }
}
