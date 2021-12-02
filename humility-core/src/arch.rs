// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

#[allow(non_camel_case_types)]
#[derive(
    Copy, Clone, Debug, Hash, FromPrimitive, ToPrimitive, PartialEq, Eq,
)]
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

use capstone::prelude::*;

impl From<RegId> for ARMRegister {
    fn from(reg: RegId) -> Self {
        use arch::arm::ArmReg::*;

        match reg.0 as u32 {
            ARM_REG_R0 => ARMRegister::R0,
            ARM_REG_R1 => ARMRegister::R1,
            ARM_REG_R2 => ARMRegister::R2,
            ARM_REG_R3 => ARMRegister::R3,
            ARM_REG_R4 => ARMRegister::R4,
            ARM_REG_R5 => ARMRegister::R5,
            ARM_REG_R6 => ARMRegister::R6,
            ARM_REG_R7 => ARMRegister::R7,
            ARM_REG_R8 => ARMRegister::R8,
            ARM_REG_R9 => ARMRegister::R9,
            ARM_REG_R10 => ARMRegister::R10,
            ARM_REG_R11 => ARMRegister::R11,
            ARM_REG_R12 => ARMRegister::R12,
            ARM_REG_SP => ARMRegister::SP,
            ARM_REG_PC => ARMRegister::PC,
            ARM_REG_LR => ARMRegister::LR,
            _ => {
                panic!("unrecognized register {:x}", reg.0);
            }
        }
    }
}

impl std::fmt::Display for ARMRegister {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.pad(&format!("{:?}", self))
    }
}
