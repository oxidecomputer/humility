// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::arch::{instr_operands, Register, RegisterField};
use anyhow::Result;
use num_traits::ToPrimitive;
use std::collections::BTreeMap;
use strum_macros::EnumIter;

#[allow(non_camel_case_types)]
#[derive(
    Copy,
    Clone,
    Debug,
    Hash,
    FromPrimitive,
    ToPrimitive,
    PartialEq,
    Eq,
    Ord,
    PartialOrd,
    EnumIter,
)]

///
/// See table 3.3 in the riscv debug spec.
/// (https://raw.githubusercontent.com/riscv/riscv-debug-spec/master/riscv-debug-stable.pdf#table.3.3)
/// The pc is read through the DPC register
///
/// THOU MUST ORDER THE REGISTERS ACCORDING TO ADDRESS!
///
pub enum RVRegister {
    MSTATUS = 0x300,
    MTVEC = 0x301,
    MIE = 0x302,
    MEPC = 0x341,
    MCAUSE = 0x342,
    MIP = 0x344,
    PMPCFG0 = 0x3a0,
    PMPCFG1,
    PMPCFG2,
    PMPCFG3,
    PMPADDR0 = 0x3b0,
    PMPADDR1,
    PMPADDR2,
    PMPADDR3,
    PMPADDR4,
    PMPADDR5,
    PMPADDR6,
    PMPADDR7,
    PMPADDR8,
    PMPADDR9,
    PMPADDR10,
    PMPADDR11,
    PMPADDR12,
    PMPADDR13,
    PMPADDR14,
    PMPADDR15,
    DCSR = 0x7b0,
    PC = 0x7b1,
    ZERO = 0x1_000,
    RA,
    SP,
    GP,
    TP,
    T0,
    T1,
    T2,
    S0,
    S1,
    A0,
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    S2,
    S3,
    S4,
    S5,
    S6,
    S7,
    S8,
    S9,
    S10,
    S11,
    T3,
    T4,
    T5,
    T6,
}

impl RVRegister {
    pub fn is_general_purpose(&self) -> bool {
        return self.to_u16().unwrap() >= 0x1000
            && self.to_u16().unwrap() <= 0x1000 + 32;
    }

    pub fn is_special(&self) -> bool {
        return self.to_u16().unwrap() < 0x1000;
    }

    pub fn is_floating_point(&self) -> bool {
        return false;
    }

    pub fn fields(&self) -> Option<Vec<RegisterField>> {
        match self {
            RVRegister::MCAUSE => {
                Some(vec![RegisterField::bit(31, "INTRERRUPT")])
            }
            RVRegister::MSTATUS => Some(vec![
                RegisterField::bit(31, "SD"),
                RegisterField::bit(22, "TSR"),
                RegisterField::bit(21, "TW"),
                RegisterField::bit(20, "TVM"),
                RegisterField::bit(19, "MXR"),
                RegisterField::bit(18, "SUM"),
                RegisterField::bit(17, "MPRV"),
                RegisterField::field(16, 15, "XS"),
                RegisterField::field(14, 13, "FS"),
                RegisterField::field(12, 11, "MPP"),
                RegisterField::field(10, 9, "VS"),
                RegisterField::bit(8, "SPP"),
                RegisterField::bit(7, "MPIE"),
                RegisterField::bit(6, "UBE"),
                RegisterField::bit(5, "SPIE"),
                RegisterField::bit(3, "MIE"),
                RegisterField::bit(1, "SIE"),
            ]),
            RVRegister::MIP => Some(vec![
                RegisterField::bit(11, "MEIP"),
                RegisterField::bit(9, "SEIP"),
                RegisterField::bit(7, "MTIP"),
                RegisterField::bit(5, "STIP"),
                RegisterField::bit(3, "MSIP"),
                RegisterField::bit(1, "SSIP"),
            ]),
            RVRegister::MIE => Some(vec![
                RegisterField::bit(11, "MEIE"),
                RegisterField::bit(9, "SEIE"),
                RegisterField::bit(7, "MTIE"),
                RegisterField::bit(5, "STIE"),
                RegisterField::bit(3, "MSIE"),
                RegisterField::bit(1, "SSIE"),
            ]),
            RVRegister::DCSR => Some(vec![
                RegisterField::field(31, 28, "debugver"),
                RegisterField::bit(17, "ebreakvs"),
                RegisterField::bit(16, "ebreakvu"),
                RegisterField::bit(15, "ebreakm"),
                RegisterField::bit(13, "ebreaks"),
                RegisterField::bit(12, "ebreaku"),
                RegisterField::bit(11, "stepie"),
                RegisterField::bit(10, "spotcount"),
                RegisterField::bit(9, "stoptime"),
                RegisterField::field(8, 6, "cause"),
                RegisterField::bit(5, "v"),
                RegisterField::bit(4, "mprven"),
                RegisterField::bit(3, "nmip"),
                RegisterField::bit(2, "step"),
                RegisterField::field(1, 0, "priv"),
            ]),
            _ => None,
        }
    }
}

use capstone::prelude::*;

impl From<RegId> for RVRegister {
    fn from(reg: RegId) -> Self {
        use arch::riscv::RiscVReg::*;

        match reg.0 as u32 {
            RISCV_REG_ZERO => RVRegister::ZERO,
            RISCV_REG_RA => RVRegister::RA,
            RISCV_REG_SP => RVRegister::SP,
            RISCV_REG_GP => RVRegister::TP,
            RISCV_REG_T0 => RVRegister::T0,
            RISCV_REG_T1 => RVRegister::T1,
            RISCV_REG_T2 => RVRegister::T2,
            RISCV_REG_T3 => RVRegister::T3,
            RISCV_REG_T4 => RVRegister::T4,
            RISCV_REG_T5 => RVRegister::T5,
            RISCV_REG_T6 => RVRegister::T6,
            RISCV_REG_TP => RVRegister::TP,
            RISCV_REG_S0 => RVRegister::S0,
            RISCV_REG_S1 => RVRegister::S1,
            RISCV_REG_S2 => RVRegister::S2,
            RISCV_REG_S3 => RVRegister::S3,
            RISCV_REG_S4 => RVRegister::S4,
            RISCV_REG_S5 => RVRegister::S5,
            RISCV_REG_S6 => RVRegister::S6,
            RISCV_REG_S7 => RVRegister::S7,
            RISCV_REG_S8 => RVRegister::S8,
            RISCV_REG_S9 => RVRegister::S9,
            RISCV_REG_S10 => RVRegister::S10,
            RISCV_REG_S11 => RVRegister::S11,
            _ => {
                panic!("unrecognized register {:x}", reg.0);
            }
        }
    }
}

impl std::fmt::Display for RVRegister {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.pad(&format!("RV: {:?}", self))
    }
}

//
// our stub frames (that is, those frames that contain system call
// instructions) have no DWARF information that describes how to unwind
// through them; for these frames we do some (very crude) analysis of the
// program text to determine what registers are pushed and how they are
// manipulated so we can properly determine register state before the system
// call. This is currently incomplete as it assums the registers are stored in order
// TODO the conditions to push probably need more rigor
pub fn riscv_presyscall_pushes(
    cs: &Capstone,
    instrs: &[capstone::Insn],
) -> Result<Vec<Register>> {
    const RV_INSN_SW: u32 = arch::riscv::RiscVInsn::RISCV_INS_SW as u32;
    const RV_INSN_C_SW: u32 = arch::riscv::RiscVInsn::RISCV_INS_C_SW as u32;

    let mut rval = vec![];
    for instr in instrs {
        match instr.id() {
            InsnId(RV_INSN_C_SW) | InsnId(RV_INSN_SW) => {
                for op in instr_operands(cs, instr).iter().rev() {
                    rval.push(*op);
                }
            }
            _ => {}
        }
    }

    Ok(rval)
}

//TODO need to poll debug registers to determine if unhalted reads are supported
pub fn unhalted_read_regions() -> BTreeMap<u32, u32> {
    let map = BTreeMap::new();
    map
}
