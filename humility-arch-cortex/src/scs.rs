// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::debug::*;
use crate::register;
use crate::register_offs;
use anyhow::{bail, Result};
use bitfield::bitfield;
use humility::core::Core;
use multimap::MultiMap;
use std::mem::size_of;

register_offs!(CIDR0, 0xff0,
    pub preamble_0, _: 7, 0;
);

register_offs!(CIDR1, 0xff4,
    pub class, _: 7, 4;
    pub preamble_1, _: 3, 0;
);

register_offs!(CIDR2, 0xff8,
    pub preamble_2, _: 7, 0;
);

register_offs!(CIDR3, 0xffc,
    pub preamble_3, _: 7, 0;
);

register_offs!(PIDR0, 0xfe0,
    pub part_0, _: 7, 0;
);

register_offs!(PIDR1, 0xfe4,
    pub des_0, _: 7, 4;
    pub part_1, _: 3, 0;
);

register_offs!(PIDR2, 0xfe8,
    pub revision, _: 7, 4;
    pub jedec, _: 3;
    pub des_1, _: 2, 0;
);

register_offs!(PIDR3, 0xfec,
    pub revand, _: 7, 4;
    pub cmod, _: 3, 0;
);

register_offs!(PIDR4, 0xfd0,
    pub size, _: 7, 4;
    pub des_3, _: 3, 0;
);

register_offs!(DEVARCH, 0xfbc,
    pub jep_cc, _: 31, 28;
    pub jep_id, _: 27, 21;
    pub present, _: 20;
    pub revision, _: 19, 16;
    pub archid, _: 15, 0;
);

register_offs!(DEVTYPE, 0xfcc,
    pub sub, _: 7, 4;
    pub major, _: 3, 0;
);

register_offs!(SWTF_CTRL, 0x0,
    pub min_hold_time, _: 11, 8;
    pub es0, set_es0: 0;
);

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum CoreSightClass {
    ROM,
    Component,
    PeripheralTestBlock,
    GenericIP,
    CoreLinkPrimeCell,
    Reserved(u32),
}

impl From<u32> for CoreSightClass {
    fn from(value: u32) -> Self {
        match value {
            0x1 => CoreSightClass::ROM,
            0x9 => CoreSightClass::Component,
            0xb => CoreSightClass::PeripheralTestBlock,
            0xe => CoreSightClass::GenericIP,
            0xf => CoreSightClass::CoreLinkPrimeCell,
            _ => CoreSightClass::Reserved(value),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct CoreSightPage {
    pub base: u32,
    pub manufacturer: jep106::JEP106Code,
    pub class: CoreSightClass,
    pub preamble: u32,
    pub part: u32,
    pub size: u32,
    pub revand: u32,
    pub cmod: u32,
}

impl CoreSightPage {
    pub fn new(core: &mut dyn humility::core::Core, base: u32) -> Result<Self> {
        let cidr0 = &CIDR0::read(core, base)?.register;
        let cidr1 = &CIDR1::read(core, base)?.register;
        let cidr2 = &CIDR2::read(core, base)?.register;
        let cidr3 = &CIDR3::read(core, base)?.register;

        let pidr0 = &PIDR0::read(core, base)?.register;
        let pidr1 = &PIDR1::read(core, base)?.register;
        let pidr2 = &PIDR2::read(core, base)?.register;
        let pidr3 = &PIDR3::read(core, base)?.register;
        let pidr4 = &PIDR4::read(core, base)?.register;

        let jep_id = ((pidr2.des_1() << 4) | pidr1.des_0()) as u8;
        let jep_cc = pidr4.des_3() as u8;

        Ok(Self {
            base,
            class: cidr1.class().into(),
            preamble: (cidr3.preamble_3() << 20)
                | (cidr2.preamble_2() << 12)
                | (cidr1.preamble_1() << 8)
                | cidr0.preamble_0(),
            manufacturer: jep106::JEP106Code::new(jep_cc, jep_id),
            part: (pidr1.part_1() << 8) | pidr0.part_0(),
            size: pidr4.size(),
            revand: pidr3.revand(),
            cmod: pidr3.cmod(),
        })
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone)]
pub enum CoreSightComponent {
    RAS,       // RAS
    ITM,       // Instruction Trace Macrocell
    DWT,       // Data and Watchpoint Tracing
    DebugV8M,  // ARM v8-M Debug Architecture
    DebugV8R,  // ARM v8-R Debug Architecture
    ETR,       // Embedded Trace Router
    ETM,       // Embedded Trace Macrocell
    CTI,       // Cross Trigger Interface
    DebugV80A, // ARM v8.0-A Debug Architecture
    DebugV81A, // ARM v8.1-A Debug Architecture
    DebugV82A, // ARM v8.2-A Debug Architecture
    MEMAP,     // MEM Access Port
    PWR,       // Power Requester
    CATU,      // CoreSight Address Translation Unit
    HSSTP,     // High-speed Serial Trace Port
    STM,       // System Trace Macrocell
    ELA,       // CoreSight Embedded Logic Analyzer
    ROM,       // CoreSight ROM table
    TPIU,      // Trace Processing Interface Unit
    SWO,       // Single Wire Output
    CSTF,      // CoreSight Trace Funnel
    CSTR,      // CoreSight Trace Replicator
    ETB,       // Embedded Trace Buffer
    TMC,       // Trace Memory Controller
    PMU,       // Performance Monitor Unit
    PTM,       // Program Trace Macrocell
    HTM,       // AHB Trace Macrocell -- and yes, that makes no sense
    GIC,       // Generic Interrupt Controller
    SCS,       // System Control Space
    FPB,       // Flash Patch and Breakpoint
    BPU,       // Breakpoint Unit
    PPB,       // Private Peripheral Bus ROM table
    Unknown { part: u32, arch: u32, major: u32, sub: u32 },
}

impl CoreSightComponent {
    pub fn new(
        core: &mut dyn humility::core::Core,
        page: &CoreSightPage,
    ) -> Result<Self> {
        let base = page.base;

        let archid = DEVARCH::read(core, base)?.register.archid();
        let devtype = &DEVTYPE::read(core, base)?.register;
        let major = devtype.major();
        let sub = devtype.sub();

        //
        // Determining the CoreSight component that we're looking at is -- in
        // classic ARM fashion -- ridiculously complicated.  Table B2-1 in the
        // CoreSight 3.0 Architecture Specification requires that we look at
        // up to 13 different fields (!) across 8 different registers (!!)
        // adding to a total of 74 bits (!!!).  On the one hand, it's
        // reassuring that we can have a unique CoreSight component for every
        // grain of sand on Earth and still have enough room to similarly
        // grant a CoreSight component for the grains of sand on a thousand
        // other such planets. On the other hand, this is obviously absurd,
        // and no two bodies of software seem to make this determination the
        // same way.  (One notable body of software from ARM claims to be
        // written from the canonical description, which is apparently to be
        // found on -- wait for it -- an internal Wiki!)
        //
        // Absent a single authoritative doc, we muddle through the best we
        // can, cribbing from various internet sources (searching GitHub for
        // known constants and their associated components is remarkably
        // effective).  The ordering here is: part numbers, falling back to
        // archid second, falling back to device type last.
        //
        Ok(match page.part {
            0x001 => CoreSightComponent::ITM,
            0x002 => CoreSightComponent::DWT,
            0x003 => CoreSightComponent::FPB,
            0x008 => CoreSightComponent::SCS,
            0x00a => CoreSightComponent::DWT,
            0x00b => CoreSightComponent::BPU,
            0x00c => CoreSightComponent::SCS,
            0x00d => CoreSightComponent::ETM,
            0x00e => CoreSightComponent::FPB,
            0x490 => CoreSightComponent::GIC,
            0x4c7 => CoreSightComponent::PPB,
            0x906 => CoreSightComponent::CTI,
            0x907 => CoreSightComponent::ETB,
            0x908 => CoreSightComponent::CSTF,
            0x909 => CoreSightComponent::CSTR,
            0x910 => CoreSightComponent::ETM,
            0x912 => CoreSightComponent::TPIU,
            0x913 => CoreSightComponent::ITM,
            0x914 => CoreSightComponent::SWO,
            0x917 => CoreSightComponent::HTM,
            0x920 => CoreSightComponent::ETM,
            0x921 => CoreSightComponent::ETM,
            0x922 => CoreSightComponent::CTI,
            0x923 => CoreSightComponent::TPIU,
            0x924 => CoreSightComponent::ETM,
            0x925 => CoreSightComponent::ETM,
            0x930 => CoreSightComponent::ETM,
            0x941 => CoreSightComponent::TPIU,
            0x95f => CoreSightComponent::PTM,
            0x961 => CoreSightComponent::TMC,
            0x962 => CoreSightComponent::STM,
            0x975 => CoreSightComponent::ETM,
            0x9a0 => CoreSightComponent::PMU,
            0x9a1 => CoreSightComponent::TPIU,
            0x9a9 => CoreSightComponent::TPIU,
            0x9a5 => CoreSightComponent::ETM,
            0x9a7 => CoreSightComponent::PMU,
            0x9af => CoreSightComponent::PMU,
            _ => {
                match archid {
                    0x0a00 => CoreSightComponent::RAS,
                    0x1a01 => CoreSightComponent::ITM,
                    0x1a02 => CoreSightComponent::DWT,
                    0x1a03 => CoreSightComponent::FPB,
                    0x2a04 => CoreSightComponent::DebugV8M,
                    0x6a05 => CoreSightComponent::DebugV8R,
                    0x0a11 => CoreSightComponent::ETR,
                    0x4a13 => CoreSightComponent::ETM,
                    0x1a14 => CoreSightComponent::CTI,
                    0x6a15 => CoreSightComponent::DebugV80A,
                    0x7a15 => CoreSightComponent::DebugV81A,
                    0x8a15 => CoreSightComponent::DebugV82A,
                    0x2a16 => CoreSightComponent::PMU,
                    0x0a17 => CoreSightComponent::MEMAP,
                    0x0a34 => CoreSightComponent::PWR, // Docs are wrong!
                    0x0a41 => CoreSightComponent::CATU,
                    0x0a50 => CoreSightComponent::HSSTP,
                    0x0a63 => CoreSightComponent::STM,
                    0x0a75 => CoreSightComponent::ELA,
                    0x0af7 => CoreSightComponent::ROM,
                    _ => match (major, sub) {
                        (0x1, 0x1) => CoreSightComponent::TPIU,
                        (0x1, 0x2) => CoreSightComponent::ETB,
                        (0x2, 0x1) => CoreSightComponent::CSTF,
                        _ => CoreSightComponent::Unknown {
                            arch: archid,
                            part: page.part,
                            major,
                            sub,
                        },
                    },
                }
            }
        })
    }

    pub fn displayable(&self) -> bool {
        !matches!(
            self,
            CoreSightComponent::DebugV8M
                | CoreSightComponent::DebugV8R
                | CoreSightComponent::DebugV80A
                | CoreSightComponent::DebugV81A
                | CoreSightComponent::DebugV82A
                | CoreSightComponent::Unknown { .. }
        )
    }
}

bitfield! {
    #[derive(Copy, Clone)]
    pub struct CoreSightROMEntry(u32);
    impl Debug;
    pub offset, _: 31, 12;
    pub res1, _: 11, 9;
    pub domain, _: 8, 4;
    pub res2, _: 3;
    pub domvalid, _: 2;
    pub valid, _: 1;
    pub present, _: 0;
}

impl From<u32> for CoreSightROMEntry {
    fn from(value: u32) -> Self {
        Self(value)
    }
}

impl From<CoreSightROMEntry> for u32 {
    fn from(entry: CoreSightROMEntry) -> Self {
        entry.0
    }
}

impl CoreSightROMEntry {
    ///
    /// Determines the address of the corresponding table, given the address
    /// of the entry.
    fn address(self, addr: u32) -> u32 {
        (addr as i32 + ((self.offset() << 12) as i32)) as u32
    }
}

pub fn read_rom(
    core: &mut dyn humility::core::Core,
    base: u32,
    components: &mut MultiMap<CoreSightComponent, u32>,
) -> Result<()> {
    //
    // We need to determine if this, in fact, a ROM table.
    //
    let page = CoreSightPage::new(core, base)?;
    let max = 0x900;

    match page.class {
        CoreSightClass::ROM => {
            //
            // This is a ROM table, so we want to read its entries, and
            // descend.
            //
            for offset in (0..max).step_by(size_of::<u32>() as usize) {
                let val = core.read_word_32(base + offset as u32)?;

                if val == 0 {
                    break;
                }

                let ent = CoreSightROMEntry(val);

                if ent.present() {
                    read_rom(core, ent.address(base), components)?;
                }
            }
        }

        CoreSightClass::Component | CoreSightClass::GenericIP => {
            components.insert(CoreSightComponent::new(core, &page)?, base);
        }

        _ => {}
    }

    Ok(())
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

//
// This is the much shorter list of vendors whose parts we support -- and
// therefore are willing to hard-code special case handling for, to some
// extent.
//
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Vendor {
    ST,
    NXP,
    ARM,
    Other,
}

#[derive(Debug)]
pub struct CoreInfo {
    pub part: ARMCore,
    pub vendor: Vendor,
    pub manufacturer: jep106::JEP106Code,
    pub manufacturer_part: u32,
    pub components: MultiMap<CoreSightComponent, u32>,
}

impl CoreInfo {
    pub fn read(core: &mut dyn humility::core::Core) -> Result<Self> {
        use num_traits::FromPrimitive;

        let cpuid = CPUID::read(core)?;
        let mut components = MultiMap::new();

        let part = match ARMCore::from_u32(cpuid.partno()) {
            Some(part) => part,
            None => {
                bail!("unknown core in CPUID {:x?}", cpuid);
            }
        };

        let rom = match part {
            ARMCore::CortexM7 | ARMCore::CortexM33 => 0xe00f_e000,
            _ => 0xe00f_f000,
        };

        let id = CoreSightPage::new(core, rom)?;
        let m = &id.manufacturer;

        let vendor = match m.get() {
            Some("STMicroelectronics") => Vendor::ST,
            Some("NXP (Philips)") => Vendor::NXP,
            Some("ARM Ltd") => Vendor::ARM,
            _ => Vendor::Other,
        };

        if vendor == Vendor::ST && part == ARMCore::CortexM7 {
            //
            // Before we can walk the M7's ROM tables, we need to make sure
            // that the debug parts are clocked.
            //
            let mut cr = STM32H7_DBGMCU_CR::read(core)?;
            cr.set_srdbgcken(true);
            cr.set_cddbgcken(true);
            cr.write(core)?;
            read_rom(core, 0x5c00_0000, &mut components)?;
        }

        if vendor == Vendor::NXP && part == ARMCore::CortexM33 {
            //
            // Before we can talk to the TPIU on the LPC55, we need to make
            // sure that it is clocked: TRACECLKSEL.SEL must be set to 0 to
            // select the Trace Divided Clock, and TRACECLKDIV.HALT,
            // TRACECLKDIV.REQFLAG and TRACECLKDIV.RESET must all be cleared.
            // (We also must check that IOCON is clocked -- though we expect
            // this to be true.)
            //
            let mut sel = LPC55_SYSCON_TRACECLKSEL::read(core)?;
            sel.set_sel(0);
            sel.write(core)?;

            let mut div = LPC55_SYSCON_TRACECLKDIV::read(core)?;
            div.set_reqflag(false);
            div.set_halt(false);
            div.set_reset(false);
            div.write(core)?;

            let ctrl = LPC55_SYSCON_AHBCLKCTRL0::read(core)?;

            if !ctrl.iocon() {
                warn!("IOCON is not clocked");
            }
        }

        read_rom(core, rom, &mut components)?;

        Ok(Self {
            part,
            vendor,
            components,
            manufacturer: id.manufacturer,
            manufacturer_part: id.part,
        })
    }

    pub fn address(&self, component: CoreSightComponent) -> Option<u32> {
        self.components.get(&component).cloned()
    }
}
