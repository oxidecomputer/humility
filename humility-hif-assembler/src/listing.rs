// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! Listing commands: inspect what a Hubris archive offers for HIF
//! program assembly.
//!
//! These functions extract and format information from the archive
//! without needing a target connection.  They are useful for
//! discovering bus names, available functions, and Idol interfaces
//! before writing a program.

use crate::assembler::HifAssembler;
use crate::types::{FunctionInfo, ResolvedBus};
use std::fmt;

/// A formatted listing of I2C buses.
pub struct BusListing<'a> {
    pub buses: Vec<&'a ResolvedBus>,
}

impl fmt::Display for BusListing<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "{:<16} {:>4}  {:<6} {:<6}",
            "BUS", "CTRL", "PORT", "NAME"
        )?;
        for bus in &self.buses {
            writeln!(
                f,
                "{:<16} {:>4}  {:<6} {}",
                bus.name, bus.controller, bus.port_index, bus.port_name,
            )?;
        }
        Ok(())
    }
}

/// A formatted listing of HIF functions.
pub struct FunctionListing<'a> {
    pub functions: Vec<&'a FunctionInfo>,
}

impl fmt::Display for FunctionListing<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{:>3}  {:<24} {:>4}", "ID", "NAME", "ARGS")?;
        for func in &self.functions {
            writeln!(
                f,
                "{:>3}  {:<24} {:>4}",
                func.id, func.name, func.arg_count,
            )?;
        }
        Ok(())
    }
}

impl HifAssembler {
    /// Produce a formatted listing of I2C buses.
    pub fn bus_listing(&self) -> BusListing<'_> {
        BusListing { buses: self.list_buses() }
    }

    /// Produce a formatted listing of HIF functions.
    pub fn function_listing(&self) -> FunctionListing<'_> {
        FunctionListing { functions: self.list_functions() }
    }

    /// Disassemble ops into raw syntax with hex and symbolic comments.
    ///
    /// The output is wrapped in `raw { }` and is valid assembler input.
    pub fn disassemble(&self, ops: &[hif::Op]) -> String {
        use postcard::to_allocvec;

        let mut out = String::new();
        out.push_str("raw {\n");

        let mut offset = 0usize;
        for op in ops {
            let raw = format_op(op, self);
            let hex = to_allocvec(op)
                .map(|bytes| {
                    bytes
                        .iter()
                        .map(|b| format!("{b:02x}"))
                        .collect::<Vec<_>>()
                        .join(" ")
                })
                .unwrap_or_else(|_| "??".into());

            out.push_str(&format!("  {raw:<26} # {offset:02x}: {hex}\n",));
            offset += to_allocvec(op).map(|b| b.len()).unwrap_or(0);
        }

        out.push_str("}\n");
        out
    }
}

/// Format a single op as raw assembler syntax.
fn format_op(op: &hif::Op, asm: &HifAssembler) -> String {
    match op {
        hif::Op::Push(v) => {
            if *v > 9 {
                format!("push 0x{v:02x}")
            } else {
                format!("push {v}")
            }
        }
        hif::Op::Push16(v) => format!("push16 0x{v:04x}"),
        hif::Op::Push32(v) => format!("push32 0x{v:08x}"),
        hif::Op::PushNone => "push_none".into(),
        hif::Op::Drop => "drop".into(),
        hif::Op::DropN(n) => format!("drop_n {n}"),
        hif::Op::Swap => "swap".into(),
        hif::Op::Dup => "dup".into(),
        hif::Op::Add => "add".into(),
        hif::Op::And => "and".into(),
        hif::Op::Or => "or".into(),
        hif::Op::Xor => "xor".into(),
        hif::Op::Expand32 => "expand32".into(),
        hif::Op::Collect32 => "collect32".into(),
        hif::Op::Label(hif::Target(t)) => format!("label {t}"),
        hif::Op::BranchGreaterThan(hif::Target(t)) => {
            format!("branch_gt {t}")
        }
        hif::Op::BranchGreaterThanOrEqualTo(hif::Target(t)) => {
            format!("branch_gte {t}")
        }
        hif::Op::BranchLessThan(hif::Target(t)) => {
            format!("branch_lt {t}")
        }
        hif::Op::BranchLessThanOrEqualTo(hif::Target(t)) => {
            format!("branch_lte {t}")
        }
        hif::Op::BranchEqualTo(hif::Target(t)) => {
            format!("branch_eq {t}")
        }
        hif::Op::BranchAlways(hif::Target(t)) => {
            format!("branch_always {t}")
        }
        hif::Op::Call(hif::TargetFunction(id)) => {
            let name = asm
                .list_functions()
                .iter()
                .find(|f| f.id == *id)
                .map(|f| f.name.as_str())
                .unwrap_or("?");
            format!("call {name}")
        }
        hif::Op::Done => "done".into(),
    }
}
