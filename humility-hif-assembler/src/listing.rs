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
    /// Each line has the raw op, byte offset, hex encoding, and a
    /// symbolic annotation where the op's role can be inferred from
    /// context (e.g. "controller (mid)" for a push before I2cRead).
    pub fn disassemble(&self, ops: &[hif::Op]) -> String {
        use postcard::to_allocvec;

        let annotations = annotate_ops(ops, self);

        let mut out = String::new();
        out.push_str("raw {\n");

        let mut offset = 0usize;
        for (i, op) in ops.iter().enumerate() {
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

            let annotation = annotations
                .get(i)
                .map(|s| format!("  {s}"))
                .unwrap_or_default();

            out.push_str(&format!(
                "  {raw:<26} # {offset:02x}: {hex}{annotation}\n",
            ));
            offset += to_allocvec(op).map(|b| b.len()).unwrap_or(0);
        }

        out.push_str("}\n");
        out
    }
}

/// Produce symbolic annotations for each op by recognizing patterns.
fn annotate_ops(ops: &[hif::Op], asm: &HifAssembler) -> Vec<String> {
    let mut notes: Vec<String> = vec![String::new(); ops.len()];

    // Find I2cRead/Write call patterns: 7 pushes + Call + DropN(7)
    let i2c_read_id = asm
        .list_functions()
        .iter()
        .find(|f| f.name.eq_ignore_ascii_case("I2cRead"))
        .map(|f| f.id);
    let i2c_write_id = asm
        .list_functions()
        .iter()
        .find(|f| f.name.eq_ignore_ascii_case("I2cWrite"))
        .map(|f| f.id);

    for (i, op) in ops.iter().enumerate() {
        if let hif::Op::Call(hif::TargetFunction(id)) = op {
            if Some(*id) == i2c_read_id || Some(*id) == i2c_write_id {
                // Annotate the 7 pushes before this call
                if i >= 7 {
                    let labels = [
                        "controller",
                        "port",
                        "mux",
                        "segment",
                        "address",
                        "register",
                        if Some(*id) == i2c_read_id {
                            "nbytes"
                        } else {
                            "data/len"
                        },
                    ];
                    for (j, label) in labels.iter().enumerate() {
                        let idx = i - 7 + j;
                        // Enrich with bus/device names where possible
                        let extra = match (j, &ops[idx]) {
                            (0, hif::Op::Push(ctrl)) => {
                                // Find bus name for this controller
                                asm.target_config()
                                    .buses
                                    .iter()
                                    .find(|b| b.controller == *ctrl)
                                    .map(|b| format!(" ({})", b.name))
                                    .unwrap_or_default()
                            }
                            (4, hif::Op::Push(addr)) => {
                                // Find device name for this address
                                // on any bus (best effort)
                                asm.target_config()
                                    .buses
                                    .iter()
                                    .flat_map(|b| b.devices.iter())
                                    .find(|d| d.address == *addr)
                                    .map(|d| format!(" ({})", d.device))
                                    .unwrap_or_default()
                            }
                            _ => String::new(),
                        };
                        notes[idx] = format!("{label}{extra}");
                    }
                }
            }

            // Annotate Send calls (Idol): task_id, op_code
            let send_id = asm
                .list_functions()
                .iter()
                .find(|f| f.name == "Send")
                .map(|f| f.id);
            if Some(*id) == send_id && i >= 4 {
                if let hif::Op::Push(task_id) = ops[i - 4] {
                    let task_name = asm
                        .target_config()
                        .idol_interfaces
                        .iter()
                        .find(|iface| iface.task_id == task_id as u32)
                        .map(|iface| iface.task.as_str())
                        .unwrap_or("?");
                    notes[i - 4] = format!("task ({task_name})");
                }
                notes[i - 3] = "op_code".into();
            }
        }

        // Annotate loop patterns
        if let hif::Op::BranchGreaterThan(_)
        | hif::Op::BranchGreaterThanOrEqualTo(_) = op
        {
            if i >= 2 {
                if let hif::Op::Add = ops[i - 2] {
                    notes[i - 2] = "counter += 1".into();
                    notes[i - 1] = "limit".into();
                    notes[i] = "loop".into();
                }
            }
        }

        if let hif::Op::Label(_) = op {
            notes[i] = "loop_start".into();
        }
    }

    notes
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
