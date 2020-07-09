/*
 * Copyright 2020 Oxide Computer Company
 */

use crate::register_offs;
use std::error::Error;

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

#[derive(Copy, Clone, Debug, PartialEq)]
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
pub struct CoreSightComponent {
    pub manufacturer: jep106::JEP106Code,
    pub class: CoreSightClass,
    pub preamble: u32,
    pub part: u32,
    pub size: u32,
    pub revand: u32,
    pub cmod: u32,
}

impl CoreSightComponent {
    pub fn new(
        core: &mut dyn crate::core::Core,
        base: u32,
    ) -> Result<Self, Box<dyn Error>> {
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
