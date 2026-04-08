// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! Statement lowering: translate parsed statements into HIF bytecode.
//!
//! This module contains the `impl HifAssembler` methods that lower
//! individual statements to `hif::Op` sequences, plus helper functions
//! for encoding Idol arguments and parsing raw ops.

use std::collections::BTreeSet;

use anyhow::{Context, Result, bail};
use hif::TargetFunction;

use crate::assembler::HifAssembler;
use crate::parser::{BusRef, DeviceRef, Located, MuxSpec, Statement};
use crate::types::*;

/// Internal assembly result: ops + data + stats + warnings.
pub(crate) struct LowerResult {
    pub ops: Vec<hif::Op>,
    pub data: Vec<u8>,
    pub warnings: Vec<String>,
    pub functions_used: BTreeSet<String>,
    pub buses_used: BTreeSet<String>,
    pub labels_used: usize,
    pub estimated_results: usize,
}

/// Rough per-result overhead in the HIFFY_RSTACK buffer.
///
/// Each `FunctionResult::Success` is postcard-encoded with a
/// discriminant byte, length prefix, and payload.  For small I2C
/// reads (1-4 bytes), 8 bytes per result is a reasonable estimate.
pub(crate) const RSTACK_BYTES_PER_RESULT: usize = 8;

impl HifAssembler {
    pub(crate) fn lower_program(
        &self,
        parsed: &crate::parser::ParsedProgram,
    ) -> Result<LowerResult> {
        let mut result = LowerResult {
            ops: vec![],
            data: vec![],
            warnings: vec![],
            functions_used: BTreeSet::new(),
            buses_used: BTreeSet::new(),
            labels_used: 0,
            estimated_results: 0,
        };
        let mut label_counter: u8 = 0;

        self.lower_statements(
            &parsed.statements,
            &mut result,
            &mut label_counter,
            0,
        )?;

        result.ops.push(hif::Op::Done);
        result.labels_used = label_counter as usize;

        Ok(result)
    }

    fn lower_statements(
        &self,
        stmts: &[Located<Statement>],
        result: &mut LowerResult,
        label_counter: &mut u8,
        depth: usize,
    ) -> Result<()> {
        for stmt in stmts {
            self.lower_statement(
                &stmt.value,
                stmt.line,
                result,
                label_counter,
                depth,
            )?;
        }
        Ok(())
    }

    fn lower_statement(
        &self,
        stmt: &Statement,
        line: usize,
        result: &mut LowerResult,
        label_counter: &mut u8,
        depth: usize,
    ) -> Result<()> {
        match stmt {
            Statement::I2cRead { bus, address, mux, register, nbytes } => {
                let func = self.require_function("i2c_read", line)?;
                self.track_bus(bus, result);
                result.functions_used.insert("i2c_read".to_string());
                result.estimated_results += 1;

                self.emit_i2c_params(
                    bus,
                    address,
                    mux,
                    &mut result.ops,
                    &mut result.warnings,
                    line,
                )?;
                match register {
                    Some(r) => result.ops.push(hif::Op::Push(*r)),
                    None => result.ops.push(hif::Op::PushNone),
                }
                result.ops.push(hif::Op::Push(*nbytes));
                result.ops.push(hif::Op::Call(TargetFunction(func.id)));
                result.ops.push(hif::Op::DropN(7));
            }
            Statement::I2cWrite { bus, address, mux, register, data } => {
                if data.len() > I2C_WRITE_MAX_DATA {
                    bail!(
                        "line {line}: i2c_write has {} data bytes, \
                         maximum is {I2C_WRITE_MAX_DATA}",
                        data.len(),
                    );
                }

                let func = self.require_function("i2c_write", line)?;
                self.track_bus(bus, result);
                result.functions_used.insert("i2c_write".to_string());
                result.estimated_results += 1;

                self.emit_i2c_params(
                    bus,
                    address,
                    mux,
                    &mut result.ops,
                    &mut result.warnings,
                    line,
                )?;
                match register {
                    Some(r) => result.ops.push(hif::Op::Push(*r)),
                    None => result.ops.push(hif::Op::PushNone),
                }
                for byte in data {
                    result.ops.push(hif::Op::Push(*byte));
                }
                result.ops.push(push_smallest(data.len() as u32));
                result.ops.push(hif::Op::Call(TargetFunction(func.id)));
                result.ops.push(hif::Op::DropN((7 + data.len()) as u8));
            }
            Statement::I2cScan { bus, mux } => {
                let func = self.require_function("i2c_read", line)?;
                let label = self.alloc_label(label_counter, line)?;
                self.track_bus(bus, result);
                result.functions_used.insert("i2c_read".to_string());
                result.estimated_results += 128;

                let (controller, port_index) =
                    self.resolve_bus(bus, line, &mut result.warnings)?;
                result.ops.push(hif::Op::Push(controller));
                result.ops.push(hif::Op::Push(port_index));
                self.emit_mux_params(mux, &mut result.ops);
                result.ops.push(hif::Op::PushNone); // reg
                result.ops.push(hif::Op::Push(0)); // address counter
                result.ops.push(hif::Op::PushNone); // sentinel
                result.ops.push(hif::Op::Label(hif::Target(label)));
                result.ops.push(hif::Op::Drop);
                result.ops.push(hif::Op::Swap);
                result.ops.push(hif::Op::Push(1));
                result.ops.push(hif::Op::Call(TargetFunction(func.id)));
                result.ops.push(hif::Op::Drop);
                result.ops.push(hif::Op::Swap);
                result.ops.push(hif::Op::Push(1));
                result.ops.push(hif::Op::Add);
                result.ops.push(hif::Op::Push(128));
                result.ops.push(hif::Op::BranchGreaterThanOrEqualTo(
                    hif::Target(label),
                ));
            }
            Statement::I2cRegScan { bus, address, mux } => {
                let func = self.require_function("i2c_read", line)?;
                let label = self.alloc_label(label_counter, line)?;
                self.track_bus(bus, result);
                result.functions_used.insert("i2c_read".to_string());
                result.estimated_results += 256;

                let (controller, port_index) =
                    self.resolve_bus(bus, line, &mut result.warnings)?;
                let addr = self.resolve_device(
                    bus,
                    address,
                    mux,
                    line,
                    &mut result.warnings,
                )?;
                result.ops.push(hif::Op::Push(controller));
                result.ops.push(hif::Op::Push(port_index));
                self.emit_mux_params(mux, &mut result.ops);
                result.ops.push(hif::Op::Push(addr));
                result.ops.push(hif::Op::Push(0));
                result.ops.push(hif::Op::PushNone);
                result.ops.push(hif::Op::Label(hif::Target(label)));
                result.ops.push(hif::Op::Drop);
                result.ops.push(hif::Op::Push(1));
                result.ops.push(hif::Op::Call(TargetFunction(func.id)));
                result.ops.push(hif::Op::Add);
                result.ops.push(hif::Op::Push(0xff));
                result.ops.push(hif::Op::BranchGreaterThanOrEqualTo(
                    hif::Target(label),
                ));
            }
            Statement::IdolCall { interface, operation, args } => {
                self.lower_idol_call(interface, operation, args, line, result)?;
            }
            Statement::Call { function, args } => {
                // HIF functions receive the entire stack (stack[0..sp])
                // and use positional args from the top.  Args pushed
                // here sit on top of any loop counter or other values
                // below; functions must only read their expected arg
                // count from the top of the stack.
                let func = self.require_function(function, line)?;
                result.functions_used.insert(function.clone());
                result.estimated_results += 1;

                for arg in args {
                    result.ops.push(push_smallest(*arg));
                }
                result.ops.push(hif::Op::Call(TargetFunction(func.id)));
                if !args.is_empty() {
                    result.ops.push(hif::Op::DropN(args.len() as u8));
                }
            }
            Statement::Sleep { ms } => {
                let func = self.require_function("Sleep", line)?;
                result.functions_used.insert("Sleep".to_string());
                let mut remaining = *ms;
                while remaining > 0 {
                    let chunk = remaining.min(100);
                    result.ops.push(push_smallest(chunk));
                    result.ops.push(hif::Op::Call(TargetFunction(func.id)));
                    result.ops.push(hif::Op::Drop);
                    remaining -= chunk;
                }
            }
            Statement::Repeat { count, sleep_ms, body } => {
                if *count == 0 {
                    bail!("line {line}: repeat count must be at least 1");
                }

                // Loop pattern (matches humility cmd/i2c):
                //   Push(0)         # counter = 0
                //   PushNone        # sentinel for first Drop
                //   Label(N)
                //   Drop            # drop previous limit (or sentinel)
                //   ... body ...
                //   Push(1)
                //   Add             # counter += 1
                //   Push(count)     # limit
                //   BranchGreaterThan(N)  # branch if limit > counter
                //
                // Stack at branch: [counter, limit]
                // BranchGreaterThan reads but doesn't pop.
                // Next iteration's Drop removes the limit.
                let label = self.alloc_label(label_counter, line)?;
                result.ops.push(push_smallest(0)); // counter
                result.ops.push(hif::Op::PushNone); // sentinel
                result.ops.push(hif::Op::Label(hif::Target(label)));
                result.ops.push(hif::Op::Drop); // drop limit/sentinel

                let before = result.estimated_results;
                self.lower_statements(body, result, label_counter, depth + 1)?;
                let body_results = result.estimated_results - before;
                result.estimated_results +=
                    body_results * (*count as usize - 1);

                if let Some(ms) = sleep_ms {
                    let func = self
                        .require_function("Sleep", line)
                        .context("repeat sleep= requires Sleep function")?;
                    result.functions_used.insert("Sleep".to_string());
                    let mut remaining = *ms;
                    while remaining > 0 {
                        let chunk = remaining.min(100);
                        result.ops.push(push_smallest(chunk));
                        result.ops.push(hif::Op::Call(TargetFunction(func.id)));
                        result.ops.push(hif::Op::Drop);
                        remaining -= chunk;
                    }
                }

                result.ops.push(push_smallest(1));
                result.ops.push(hif::Op::Add); // counter += 1
                result.ops.push(push_smallest(*count)); // limit
                result.ops.push(hif::Op::BranchGreaterThan(hif::Target(label)));
            }
            Statement::Raw { lines } => {
                for raw_line in lines {
                    let raw_line = raw_line.trim();
                    if raw_line.is_empty() {
                        continue;
                    }
                    let op = self.parse_raw_op(raw_line, line)?;
                    result.ops.push(op);
                }
            }
        }
        Ok(())
    }

    fn lower_idol_call(
        &self,
        interface: &str,
        operation: &str,
        args: &[(String, String)],
        line: usize,
        result: &mut LowerResult,
    ) -> Result<()> {
        let iface = self
            .config
            .idol_interfaces
            .iter()
            .find(|i| i.name == interface)
            .ok_or_else(|| {
                anyhow::anyhow!(
                    "line {line}: unknown Idol interface '{interface}'"
                )
            })?;

        let op = iface.ops.iter().find(|o| o.name == operation).ok_or_else(
            || {
                anyhow::anyhow!(
                    "line {line}: unknown operation \
                     '{interface}.{operation}'"
                )
            },
        )?;

        let (func_name, has_read_lease, has_write_lease) = match (
            op.leases.iter().any(|l| l.write),
            op.leases.iter().any(|l| l.read),
        ) {
            (false, false) => ("Send", false, false),
            (false, true) => ("SendLeaseRead", false, true),
            (true, false) => ("SendLeaseWrite", true, false),
            (true, true) => ("SendLeaseReadWrite", true, true),
        };
        let func = self.require_function(func_name, line)?;
        result.functions_used.insert(func_name.to_string());
        result.estimated_results += 1;

        let mut arg_map: std::collections::HashMap<&str, &str> =
            args.iter().map(|(k, v)| (k.as_str(), v.as_str())).collect();

        let mut payload = Vec::new();
        for expected_arg in &op.args {
            let val_str = arg_map
                .remove(expected_arg.name.as_str())
                .ok_or_else(|| {
                    anyhow::anyhow!(
                        "line {line}: {interface}.{operation} \
                             missing argument '{}'",
                        expected_arg.name,
                    )
                })?;

            let bytes = encode_idol_arg(
                val_str,
                &expected_arg.ty,
                &expected_arg.name,
                line,
            )?;
            payload.extend_from_slice(&bytes);
        }

        if let Some((extra, _)) = arg_map.iter().next() {
            bail!(
                "line {line}: {interface}.{operation} \
                 unexpected argument '{extra}'"
            );
        }

        result.ops.push(push_smallest(iface.task_id));
        result.ops.push(push_smallest(op.code as u32));
        for byte in &payload {
            result.ops.push(hif::Op::Push(*byte));
        }
        result.ops.push(push_smallest(payload.len() as u32));

        if op.reply_size == 0 && op.reply != "()" {
            result.warnings.push(format!(
                "line {line}: {interface}.{operation} reply type \
                 '{}' size could not be determined; \
                 results may be truncated",
                op.reply,
            ));
        }
        result.ops.push(push_smallest(op.reply_size as u32));

        if has_write_lease {
            result.ops.push(push_smallest(0));
        }
        if has_read_lease {
            result.ops.push(push_smallest(0));
        }

        result.ops.push(hif::Op::Call(TargetFunction(func.id)));

        let frame_size = 2
            + payload.len()
            + 1
            + 1
            + if has_write_lease { 1 } else { 0 }
            + if has_read_lease { 1 } else { 0 };
        result.ops.push(hif::Op::DropN(frame_size as u8));

        Ok(())
    }

    fn track_bus(&self, bus: &BusRef, result: &mut LowerResult) {
        if let BusRef::Named(name) = bus {
            result.buses_used.insert(name.clone());
        }
    }

    fn emit_i2c_params(
        &self,
        bus: &BusRef,
        address: &DeviceRef,
        mux: &Option<MuxSpec>,
        ops: &mut Vec<hif::Op>,
        warnings: &mut Vec<String>,
        line: usize,
    ) -> Result<()> {
        let (controller, port_index) = self.resolve_bus(bus, line, warnings)?;
        let addr = self.resolve_device(bus, address, mux, line, warnings)?;
        ops.push(hif::Op::Push(controller));
        ops.push(hif::Op::Push(port_index));
        self.emit_mux_params(mux, ops);
        ops.push(hif::Op::Push(addr));
        Ok(())
    }

    fn emit_mux_params(&self, mux: &Option<MuxSpec>, ops: &mut Vec<hif::Op>) {
        match mux {
            Some(m) => {
                ops.push(hif::Op::Push(m.address));
                ops.push(hif::Op::Push(m.segment));
            }
            None => {
                ops.push(hif::Op::PushNone);
                ops.push(hif::Op::PushNone);
            }
        }
    }

    pub(crate) fn resolve_bus(
        &self,
        bus: &BusRef,
        line: usize,
        warnings: &mut Vec<String>,
    ) -> Result<(u8, u8)> {
        match bus {
            BusRef::Named(name) => {
                let b = self.buses.get(name).ok_or_else(|| {
                    anyhow::anyhow!("line {line}: unknown I2C bus '{name}'")
                })?;
                Ok((b.controller, b.port_index))
            }
            BusRef::Explicit { controller, port } => {
                for b in self.buses.values() {
                    if b.controller == *controller
                        && b.port_name.eq_ignore_ascii_case(port)
                    {
                        return Ok((b.controller, b.port_index));
                    }
                }
                warnings.push(format!(
                    "line {line}: explicit bus {controller}.{port} \
                     not found in target config, using port index 0"
                ));
                Ok((*controller, 0))
            }
        }
    }

    /// Resolve a device reference to a numeric I2C address.
    fn resolve_device(
        &self,
        bus: &BusRef,
        device: &DeviceRef,
        mux: &Option<MuxSpec>,
        line: usize,
        _warnings: &mut Vec<String>,
    ) -> Result<u8> {
        match device {
            DeviceRef::Address(addr) => Ok(*addr),
            DeviceRef::Named(name) => {
                // Search for the device by part name or human name
                // in the specified bus (or all buses if bus is inferred).
                let bus_name = match bus {
                    BusRef::Named(n) => Some(n.as_str()),
                    BusRef::Explicit { .. } => None,
                };

                for b in self.config.buses.iter() {
                    if let Some(bn) = bus_name {
                        if b.name != bn {
                            continue;
                        }
                    }

                    // Search direct devices
                    for d in &b.devices {
                        if d.device.eq_ignore_ascii_case(name)
                            || d.name
                                .as_deref()
                                .map(|n| n.eq_ignore_ascii_case(name))
                                .unwrap_or(false)
                        {
                            // If muxed, skip direct devices
                            if mux.is_some() {
                                continue;
                            }
                            return Ok(d.address);
                        }
                    }

                    // Search muxed devices
                    for m in &b.muxes {
                        for seg in &m.segments {
                            for d in &seg.devices {
                                if d.device.eq_ignore_ascii_case(name)
                                    || d.name
                                        .as_deref()
                                        .map(|n| n.eq_ignore_ascii_case(name))
                                        .unwrap_or(false)
                                {
                                    return Ok(d.address);
                                }
                            }
                        }
                    }
                }

                bail!("line {line}: unknown I2C device '{name}'")
            }
        }
    }

    /// Look up a HIF function by name.
    ///
    /// Tries exact match first, then normalized match (strip `_`,
    /// lowercase).
    pub(crate) fn require_function(
        &self,
        name: &str,
        line: usize,
    ) -> Result<&FunctionInfo> {
        if let Some(f) = self.functions.get(name) {
            return Ok(f);
        }
        let normalized = normalize_function_name(name);
        if let Some(canonical) = self.function_aliases.get(&normalized) {
            if let Some(f) = self.functions.get(canonical) {
                return Ok(f);
            }
        }
        Err(anyhow::anyhow!("line {line}: unknown HIF function '{name}'"))
    }

    fn alloc_label(&self, counter: &mut u8, line: usize) -> Result<u8> {
        if *counter as usize >= MAX_LABELS {
            bail!(
                "line {line}: program uses {} labels, \
                 exceeds maximum of {MAX_LABELS}",
                *counter as usize + 1,
            );
        }
        let label = *counter;
        *counter += 1;
        Ok(label)
    }

    fn parse_raw_op(&self, line: &str, src_line: usize) -> Result<hif::Op> {
        let tokens: Vec<&str> = line.split_whitespace().collect();
        if tokens.is_empty() {
            bail!("line {src_line}: empty raw op");
        }
        match tokens[0] {
            "push" => {
                let v = raw_arg::<u8>(&tokens, 1, src_line)?;
                Ok(hif::Op::Push(v))
            }
            "push16" => {
                let v = raw_arg::<u16>(&tokens, 1, src_line)?;
                Ok(hif::Op::Push16(v))
            }
            "push32" => {
                let v = raw_arg::<u32>(&tokens, 1, src_line)?;
                Ok(hif::Op::Push32(v))
            }
            "push_none" => Ok(hif::Op::PushNone),
            "drop" => Ok(hif::Op::Drop),
            "drop_n" => {
                let v = raw_arg::<u8>(&tokens, 1, src_line)?;
                Ok(hif::Op::DropN(v))
            }
            "swap" => Ok(hif::Op::Swap),
            "add" => Ok(hif::Op::Add),
            "label" => {
                let v = raw_arg::<u8>(&tokens, 1, src_line)?;
                Ok(hif::Op::Label(hif::Target(v)))
            }
            "branch_gt" => {
                let v = raw_arg::<u8>(&tokens, 1, src_line)?;
                Ok(hif::Op::BranchGreaterThan(hif::Target(v)))
            }
            "branch_gte" => {
                let v = raw_arg::<u8>(&tokens, 1, src_line)?;
                Ok(hif::Op::BranchGreaterThanOrEqualTo(hif::Target(v)))
            }
            "branch_lt" => {
                let v = raw_arg::<u8>(&tokens, 1, src_line)?;
                Ok(hif::Op::BranchLessThan(hif::Target(v)))
            }
            "call" => {
                if let Ok(id) = crate::parser::parse_num::<u8>(tokens[1]) {
                    Ok(hif::Op::Call(TargetFunction(id)))
                } else {
                    let func = self.require_function(tokens[1], src_line)?;
                    Ok(hif::Op::Call(TargetFunction(func.id)))
                }
            }
            "done" => Ok(hif::Op::Done),
            other => {
                bail!("line {src_line}: unknown raw op '{other}'")
            }
        }
    }
}

/// Encode an Idol argument value as little-endian bytes.
fn encode_idol_arg(
    val: &str,
    ty: &str,
    arg_name: &str,
    line: usize,
) -> Result<Vec<u8>> {
    match ty {
        "u8" => {
            let v = parse_idol_num::<u8>(val, arg_name, line)?;
            Ok(vec![v])
        }
        "u16" => {
            let v = parse_idol_num::<u16>(val, arg_name, line)?;
            Ok(v.to_le_bytes().to_vec())
        }
        "u32" | "usize" => {
            let v = parse_idol_num::<u32>(val, arg_name, line)?;
            Ok(v.to_le_bytes().to_vec())
        }
        "u64" => {
            let v = parse_idol_num::<u64>(val, arg_name, line)?;
            Ok(v.to_le_bytes().to_vec())
        }
        "i8" => {
            let v = parse_idol_num::<u8>(val, arg_name, line)?;
            Ok(vec![v])
        }
        "i16" => {
            let v = parse_idol_num::<u16>(val, arg_name, line)?;
            Ok(v.to_le_bytes().to_vec())
        }
        "i32" | "isize" => {
            let v = parse_idol_num::<u32>(val, arg_name, line)?;
            Ok(v.to_le_bytes().to_vec())
        }
        "i64" => {
            let v = parse_idol_num::<u64>(val, arg_name, line)?;
            Ok(v.to_le_bytes().to_vec())
        }
        "f32" => {
            let v: f32 = val.parse().map_err(|_| {
                anyhow::anyhow!(
                    "line {line}: bad f32 value '{val}' \
                     for arg '{arg_name}'"
                )
            })?;
            Ok(v.to_le_bytes().to_vec())
        }
        "bool" => {
            let v = match val {
                "true" | "1" => 1u8,
                "false" | "0" => 0u8,
                _ => bail!(
                    "line {line}: bad bool value '{val}' for arg \
                     '{arg_name}' (expected true/false/0/1)"
                ),
            };
            Ok(vec![v])
        }
        _ => {
            if let Ok(v) = crate::parser::parse_num::<u32>(val) {
                Ok(v.to_le_bytes().to_vec())
            } else {
                bail!(
                    "line {line}: cannot encode '{val}' as type \
                     '{ty}' for arg '{arg_name}'; use a numeric value"
                )
            }
        }
    }
}

fn parse_idol_num<T>(val: &str, arg_name: &str, line: usize) -> Result<T>
where
    T: TryFrom<u64>,
{
    crate::parser::parse_num::<T>(val).map_err(|_| {
        anyhow::anyhow!(
            "line {line}: bad numeric value '{val}' for arg '{arg_name}'"
        )
    })
}

/// Normalize a function name for alias matching.
///
/// Strips underscores and lowercases: `i2c_read` -> `i2cread`,
/// `I2cRead` -> `i2cread`, `Sleep` -> `sleep`.
pub(crate) fn normalize_function_name(name: &str) -> String {
    name.replace('_', "").to_lowercase()
}

pub(crate) fn push_smallest(val: u32) -> hif::Op {
    if val <= u8::MAX as u32 {
        hif::Op::Push(val as u8)
    } else if val <= u16::MAX as u32 {
        hif::Op::Push16(val as u16)
    } else {
        hif::Op::Push32(val)
    }
}

fn raw_arg<T: TryFrom<u64>>(
    tokens: &[&str],
    idx: usize,
    line: usize,
) -> Result<T> {
    let s = tokens.get(idx).ok_or_else(|| {
        anyhow::anyhow!("line {line}: '{}' requires an argument", tokens[0])
    })?;
    crate::parser::parse_num::<T>(s)
        .map_err(|_| anyhow::anyhow!("line {line}: invalid number '{s}'"))
}
