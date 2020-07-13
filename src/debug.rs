/*
 * Copyright 2020 Oxide Computer Company
 */

use crate::hubris::*;
use crate::itm::*;
use crate::scs::*;
use bitfield::bitfield;
use std::error::Error;

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
                core: &mut dyn crate::core::Core
            ) -> Result<$reg, Box<dyn Error>> {
                Ok(Self(core.read_word_32($addr)?))
            }

            pub fn write(
                &self,
                core: &mut dyn crate::core::Core
            ) -> Result<(), Box<dyn Error>> {
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
            pub struct $reg {
                base: u32,
                register: [<mod_ $reg>]::$reg
            }

            impl $reg {
                pub fn read(
                    core: &mut dyn crate::core::Core,
                    base: u32
                ) -> Result<$reg, Box<dyn Error>> {
                    Ok(Self {
                        base: base,
                        register: [<mod_ $reg>]::$reg(
                            core.read_word_32(base + $offs)?
                        )
                    })
                }
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

register!(CFSR, 0xe000_ed28,
    #[derive(Copy, Clone)]
    pub struct CFSR(u32);
    impl Debug;
    pub usage_divide_by_zero, _: 16 + 9;
    pub usage_unaligned, _: 16 + 8;
    pub usage_no_coprocessor, _: 16 + 3;
    pub usage_invalid_pc, _: 16 + 2;
    pub usage_invalid_state, _: 16 + 1;
    pub usage_undefined_instr, _: 16 + 0;
    pub bus_addr_valid, _: 8 + 7;
    pub bus_lazy_fp, _: 8 + 5;
    pub bus_exception_entry, _: 8 + 4;
    pub bus_exception_return, _: 8 + 3;
    pub bus_imprecise_data, _: 8 + 2;
    pub bus_precise_data, _: 8 + 1;
    pub bus_instr_prefetch, _: 8 + 0;
    pub mem_addr_valid, _: 7;
    pub mem_lazy_fp, _: 5;
    pub mem_exception_entry, _: 4;
    pub mem_exception_return, _: 3;
    pub mem_data_access, _: 1;
    pub mem_instr_access, _: 0;
);

register!(HFSR, 0xe000_ed2c,
    #[derive(Copy, Clone)]
    pub struct HFSR(u32);
    impl Debug;
    pub debug_fault, _: 31;
    pub forced_fault, _: 30;
    pub vector_fault, _: 1;
);

/*
 * Debug Fault Status Register
 */
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

/*
 * Debug Halting Control Status Register
 */
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

#[allow(non_camel_case_types)]
#[derive(Copy, Clone, Debug, FromPrimitive, ToPrimitive, PartialEq, Eq)]
pub enum ARMRegister {
    R0 = 0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R11,
    R12,
    SP = 0b1101,
    LR = 0b1110,
    PC = 0b1111,
    xPSR = 0b1_0000,
    MSP = 0b1_0001,
    PSP = 0b1_0010,
    SPR = 0b1_0100,
    FPSCR = 0b10_0001,
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

#[rustfmt::skip::macros(format)]

pub fn cpuinfo(
    hubris: &HubrisPackage,
    core: &mut dyn crate::core::Core,
) -> Result<(), Box<dyn Error>> {
    use num_traits::FromPrimitive;
    let mut status = vec![];

    let print = |what, val| {
        info!("{:>12} => {}", what, val);
    };

    let mut statusif = |val, str: &str| {
        if val {
            status.push(str.to_string());
        }
    };

    let config = Config::read(core)?;
    let part = config.part;

    let dhcsr = DHCSR::read(core)?;
    let dfsr = DFSR::read(core)?;

    print("core", corename(part));

    let m = &config.manufacturer;

    print(
        "manufacturer",
        if let Some(manufacturer) = m.get() {
            manufacturer.to_string()
        } else {
            format!("<JEP106 [0x{:x}, 0x{:x}]>", m.cc, m.id)
        },
    );

    print(
        "chip",
        if config.st && part == ARMCore::CortexM4 {
            if let Ok(idc) = STM32F4_DBGMCU_IDCODE::read(core) {
                format!(
                    "{}, revision 0x{:x}",
                    stm32_chipname(idc.dev_id()),
                    idc.rev_id()
                )
            } else {
                format!("<unknown ST part 0x{:x}>", config.manufacturer_part)
            }
        } else if config.st && part == ARMCore::CortexM7 {
            if let Ok(idc) = STM32H7_DBGMCU_IDC::read(core) {
                format!(
                    "{}, revision 0x{:x}",
                    stm32_chipname(idc.dev_id()),
                    idc.rev_id()
                )
            } else {
                format!("<unknown ST part 0x{:x}>", config.manufacturer_part)
            }
        } else {
            format!("<unknown part 0x{:x}>", config.manufacturer_part)
        },
    );

    print("debug units", config.components());

    statusif(dhcsr.restart_status(), "restarting");
    statusif(dhcsr.reset_status(), "resetting");
    statusif(dhcsr.retire_status(), "executing");
    statusif(dhcsr.locked_up(), "locked up");
    statusif(dhcsr.halted(), "halted");
    statusif(dfsr.external(), "external halt");
    statusif(dfsr.vector_catch(), "vector catch");
    statusif(dfsr.watchpoint(), "watchpoint");
    statusif(dfsr.breakpoint(), "breakpoint");
    statusif(dfsr.halted(), "debug halt");

    print(
        "status",
        if status.len() == 0 {
            /*
             * If the status is unknown, it doesn't mean very much; from the
             * ARMv7-M ARM on the meaning of S_RETIRE_ST:
             *
             *   The architecture does not define precisely when this bit is
             *   set to 1. It requires only that this happen periodically in
             *   Non-debug state to indicate that software execution is
             *   progressing.
             *
             * To see if the core is actually executing instructions, we
             * will attempt to halt it and step it, seeing if the PC moves.
             */
            let rval = core
                .halt()
                .and_then(|_| Ok(core.read_reg(ARMRegister::PC)?))
                .and_then(|val| {
                    core.step()?;
                    Ok(val)
                })
                .and_then(|val| {
                    if core.read_reg(ARMRegister::PC)? == val {
                        Ok("not progressing")
                    } else {
                        Ok("progressing")
                    }
                })
                .map_or_else(|_| "unable to step", |s| s)
                .to_string();
            core.run()?;
            rval
        } else {
            status.join(", ")
        },
    );

    print(
        "ITM",
        match config.address(CoreSightComponent::ITM) {
            None => "absent".to_string(),
            Some(_) => {
                let mut itm = vec![];

                if DEMCR::read(core)?.trcena() {
                    itm.push("TRCENA enabled");
                } else {
                    itm.push("TRCENA disabled");
                }

                if ITM_TCR::read(core)?.itm_enable() {
                    itm.push("TCR enabled")
                } else {
                    itm.push("TCR disabled")
                }

                let s = format!("TER=0x{:x}", u32::from(ITM_TER::read(core)?));

                itm.push(&s);
                itm.join(", ")
            }
        },
    );

    if !dhcsr.halted() {
        core.halt()?;
    }

    for i in 0..31 {
        let reg = match ARMRegister::from_u16(i) {
            Some(r) => r,
            None => {
                continue;
            }
        };

        let val = core.read_reg(reg)?;

        info!(
            "{:>12} => 0x{:8} {}",
            format!("{:?}", reg),
            format!("{:x}", val),
            if i <= 15 {
                if let Some(sval) = hubris.instr_sym(val) {
                    format!(" <- {}{}+0x{:x}", 
                        match hubris.instr_mod(val) {
                            Some(module) if module != "kernel" => {
                                format!("{}:", module)
                            }
                            _ => "".to_string()
                        },
                        sval.0, val - sval.1)
                } else {
                    "".to_string()
                }
            } else {
                "".to_string()
            }
        );
    }

    if !dhcsr.halted() {
        core.run()?;
    }

    Ok(())
}
