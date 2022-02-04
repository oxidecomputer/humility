// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::{bail, Result};
use std::collections::{BTreeMap, HashMap};

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
    PSR = 0b1_0000,
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

fn instr_operands(cs: &Capstone, instr: &capstone::Insn) -> Vec<ARMRegister> {
    let detail = cs.insn_detail(instr).unwrap();
    let mut rval: Vec<ARMRegister> = Vec::new();

    for op in detail.arch_detail().operands() {
        if let arch::ArchOperand::ArmOperand(op) = op {
            if let arch::arm::ArmOperandType::Reg(id) = op.op_type {
                rval.push(id.into());
            }
        }
    }

    rval
}

fn instr_source_target(
    cs: &Capstone,
    instr: &capstone::Insn,
) -> Result<(Option<ARMRegister>, Option<ARMRegister>)> {
    let detail = cs.insn_detail(instr).unwrap();

    let mut source: Option<ARMRegister> = None;
    let mut target: Option<ARMRegister> = None;

    for op in detail.regs_read() {
        if source.is_some() {
            bail!("multiple source registers");
        }
        source = Some((*op).into());
    }

    for op in detail.regs_write() {
        if target.is_some() {
            bail!("multiple target registers");
        }
        target = Some((*op).into());
    }

    Ok((source, target))
}

//
// On ARM, our stub frames (that is, those frames that contain system call
// instructions) have no DWARF information that describes how to unwind
// through them; for these frames we do some (very crude) analysis of the
// program text to determine what registers are pushed and how they are
// manipulated so we can properly determine register state before the system
// call.  Note that this made slightly more challenging by ARMv6-M, which
// doesn't have as rich push instructions as ARMv7-M/ARMv8-M:  in order for it
// to push R8 through R11, it must first move them into the lower registers
// (R0 through R7).  We therefore have to track moves in addition to pushes to
// determine what landed where -- and yes, this heuristic is incomplete!
//
pub fn presyscall_pushes(
    cs: &Capstone,
    instrs: &[capstone::Insn],
) -> Result<Vec<ARMRegister>> {
    const ARM_INSN_PUSH: u32 = arch::arm::ArmInsn::ARM_INS_PUSH as u32;
    const ARM_INSN_MOV: u32 = arch::arm::ArmInsn::ARM_INS_MOV as u32;
    const ARM_INSN_POP: u32 = arch::arm::ArmInsn::ARM_INS_POP as u32;

    let mut map = HashMap::new();
    let mut rval = vec![];

    for instr in instrs {
        match instr.id() {
            InsnId(ARM_INSN_MOV) => {
                let (source, target) = instr_source_target(cs, instr)?;

                if let (Some(source), Some(target)) = (source, target) {
                    map.insert(target, source);
                }
            }

            InsnId(ARM_INSN_PUSH) => {
                for op in instr_operands(cs, instr).iter().rev() {
                    rval.push(if let Some(source) = map.get(op) {
                        *source
                    } else {
                        *op
                    });
                }
            }

            InsnId(ARM_INSN_POP) => {
                for _ in instr_operands(cs, instr).iter() {
                    rval.pop();
                }
            }

            _ => {}
        }
    }

    //
    // What we have now is the order that registers were pushed onto the
    // stack.  The addressing order is naturally the inverse of this, so
    // we reverse it before handing it back.
    //
    rval.reverse();

    Ok(rval)
}

//
// ARM normally requires the stack to be 8-byte aligned on function entry.
// However, because exceptions are asynchronous, on exception entry the
// stack may need to be aligned by the CPU.  If this is needed, bit 9 is
// set in the PSR to indicate this.  In our system call stubs, we can
// make our code simpler (and shave a cycle or two) by knowing that the
// CPU will do this -- but we must be sure to take that into account when
// unwinding the stack!  (And we know that we will only be off by 4 bytes;
// if the bit is set, the needed realignment is 4 -- not 1 or 2.)
//
pub fn exception_stack_realign(regs: &HashMap<ARMRegister, u32>) -> u32 {
    if let Some(psr) = regs.get(&ARMRegister::PSR) {
        if (psr & (1 << 9)) != 0 {
            return 4;
        }
    }

    0
}

pub fn unhalted_read_regions() -> BTreeMap<u32, u32> {
    let mut map = BTreeMap::new();

    //
    // On ARM, the PPB is mapped at 0xe000_000 and runs for 1MB.  This address
    // range contains the control registers that we need to read to determine
    // the state of the MCU; it can be read without halting the core on all
    // architectures.
    //
    map.insert(0xe000_0000, 1024 * 1024);

    map
}

impl std::fmt::Display for ARMRegister {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.pad(&format!("{:?}", self))
    }
}
