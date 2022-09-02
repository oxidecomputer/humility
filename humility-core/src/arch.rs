// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::arch_arm::ARMRegister;
use crate::arch_rv::RVRegister;
use anyhow::{bail, Result};
use capstone::arch::arm;
use capstone::arch::riscv;
use capstone::arch::ArchOperand;
use num_traits::cast::ToPrimitive;
use num_traits::FromPrimitive;

#[derive(Copy, Clone, Debug)]
pub struct RegisterField {
    pub highbit: u16,
    pub lowbit: u16,
    pub name: &'static str,
}

impl RegisterField {
    pub fn field(highbit: u16, lowbit: u16, name: &'static str) -> Self {
        Self { highbit, lowbit, name }
    }
    pub fn bit(bit: u16, name: &'static str) -> Self {
        Self { highbit: bit, lowbit: bit, name }
    }
}
#[allow(non_camel_case_types)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, Ord, PartialOrd)]
pub enum Register {
    Arm(ARMRegister),
    RiscV(RVRegister),
}

impl Register {
    pub fn is_general_purpose(&self) -> bool {
        match self {
            Register::Arm(reg) => reg.is_general_purpose(),
            Register::RiscV(reg) => reg.is_general_purpose(),
        }
    }
    pub fn is_special(&self) -> bool {
        match self {
            Register::Arm(reg) => reg.is_special(),
            Register::RiscV(reg) => reg.is_special(),
        }
    }
    pub fn is_floating_point(&self) -> bool {
        match self {
            Register::Arm(reg) => reg.is_floating_point(),
            Register::RiscV(reg) => reg.is_floating_point(),
        }
    }
    pub fn fields(&self) -> Option<Vec<RegisterField>> {
        match self {
            Register::Arm(reg) => reg.fields(),
            Register::RiscV(reg) => reg.fields(),
        }
    }
}

impl ToPrimitive for Register {
    fn to_u64(&self) -> Option<u64> {
        match self {
            Register::Arm(reg) => ARMRegister::to_u64(&reg),
            Register::RiscV(reg) => RVRegister::to_u64(&reg),
        }
    }
    fn to_i64(&self) -> Option<i64> {
        match self {
            Register::Arm(reg) => ARMRegister::to_i64(&reg),
            Register::RiscV(reg) => RVRegister::to_i64(&reg),
        }
    }
    fn to_u32(&self) -> Option<u32> {
        match self {
            Register::Arm(reg) => ARMRegister::to_u32(&reg),
            Register::RiscV(reg) => RVRegister::to_u32(&reg),
        }
    }
    fn to_u16(&self) -> Option<u16> {
        match self {
            Register::Arm(reg) => ARMRegister::to_u16(&reg),
            Register::RiscV(reg) => RVRegister::to_u16(&reg),
        }
    }
}

use capstone::prelude::*;

pub fn instr_operands(cs: &Capstone, instr: &capstone::Insn) -> Vec<Register> {
    let detail = cs.insn_detail(instr).unwrap();
    let mut rval: Vec<Register> = Vec::new();

    for op in detail.arch_detail().operands() {
        match op {
            ArchOperand::ArmOperand(op) => {
                if let arm::ArmOperandType::Reg(id) = op.op_type {
                    let reg: ARMRegister = id.into();
                    rval.push(Register::Arm(reg));
                }
            }
            ArchOperand::RiscVOperand(op) => {
                if let riscv::RiscVOperand::Reg(id) = op {
                    let reg: RVRegister = id.into();
                    rval.push(Register::RiscV(reg));
                }
            }
            _ => unimplemented!(),
        }
    }

    rval
}

pub fn instr_source_target(
    cs: &Capstone,
    instr: &capstone::Insn,
) -> Result<(Option<Register>, Option<Register>)> {
    let detail = cs.insn_detail(instr).unwrap();
    let mut source_id: Option<u16> = None;
    let mut source: Option<Register> = None;
    let mut target_id: Option<u16> = None;
    let mut target: Option<Register> = None;

    for op in detail.regs_read() {
        if source_id.is_some() {
            bail!("multiple source registers");
        }
        let RegId(op) = op;
        source_id = Some((*op).into());
    }

    for op in detail.regs_write() {
        if target_id.is_some() {
            bail!("multiple target registers");
        }
        let RegId(op) = op;
        target_id = Some((*op).into());
    }

    match detail.arch_detail() {
        ArchDetail::ArmDetail(_detail) => {
            if let Some(id) = source_id {
                source =
                    Some(Register::Arm(ARMRegister::from_u16(id).unwrap()));
            }
            if let Some(id) = target_id {
                target =
                    Some(Register::Arm(ARMRegister::from_u16(id).unwrap()));
            }
        }
        ArchDetail::RiscVDetail(_detail) => {
            if let Some(id) = source_id {
                source =
                    Some(Register::RiscV(RVRegister::from_u16(id).unwrap()));
            }
            if let Some(id) = target_id {
                target =
                    Some(Register::RiscV(RVRegister::from_u16(id).unwrap()));
            };
        }
        _ => unimplemented!(),
    }

    Ok((source, target))
}

impl std::fmt::Display for Register {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Register::Arm(reg) => formatter.pad(&format!("{:?}", reg)),
            Register::RiscV(reg) => formatter.pad(&format!("{:?}", reg)),
        }
    }
}
