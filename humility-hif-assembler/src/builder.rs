// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! Programmatic HIF program construction.
//!
//! [`ProgramBuilder`] provides a Rust API for generating HIF programs
//! without writing text.  It emits the text format internally so the
//! same assembler validation and lowering pipeline is used.
//!
//! Primary use case: PRNG-driven fuzz and stress test generators.
//!
//! ```rust,ignore
//! let asm = HifAssembler::new(config);
//! let mut prog = ProgramBuilder::new();
//!
//! prog.i2c_read("mid", 0x48, Some(0x00), 2);
//! prog.repeat(100, |body| {
//!     body.i2c_read("front", 0x49, None, 1);
//!     body.sleep_ms(10);
//! });
//! prog.idol_call("Sensor", "get", &[("id", "0")]);
//!
//! let output = asm.assemble(&prog.finish())?;
//! ```

use std::fmt::Write;

/// Builder for constructing HIF programs programmatically.
///
/// Methods mirror the text language sugar.  Call [`finish`](Self::finish)
/// to get the text source, then pass it to
/// [`HifAssembler::assemble`](crate::assembler::HifAssembler::assemble).
pub struct ProgramBuilder {
    lines: Vec<String>,
    indent: usize,
}

impl ProgramBuilder {
    pub fn new() -> Self {
        ProgramBuilder { lines: vec![], indent: 0 }
    }

    /// Emit the assembled text source.
    pub fn finish(&self) -> String {
        self.lines.join("\n")
    }

    /// Define a constant.
    pub fn constant(&mut self, name: &str, value: &str) -> &mut Self {
        self.push_line(&format!(".let {name} {value}"));
        self
    }

    /// Read bytes from an I2C device.
    pub fn i2c_read(
        &mut self,
        bus: &str,
        address: u8,
        register: Option<u8>,
        nbytes: u8,
    ) -> &mut Self {
        let mut line = format!("i2c_read {bus} 0x{address:02x}");
        if let Some(reg) = register {
            write!(line, " reg=0x{reg:02x}").unwrap();
        }
        write!(line, " {nbytes}").unwrap();
        self.push_line(&line);
        self
    }

    /// Read bytes from an I2C device behind a mux.
    pub fn i2c_read_mux(
        &mut self,
        bus: &str,
        address: u8,
        mux_addr: u8,
        segment: u8,
        register: Option<u8>,
        nbytes: u8,
    ) -> &mut Self {
        let mut line = format!(
            "i2c_read {bus} 0x{address:02x} mux=0x{mux_addr:02x}.{segment}"
        );
        if let Some(reg) = register {
            write!(line, " reg=0x{reg:02x}").unwrap();
        }
        write!(line, " {nbytes}").unwrap();
        self.push_line(&line);
        self
    }

    /// Write bytes to an I2C device.
    pub fn i2c_write(
        &mut self,
        bus: &str,
        address: u8,
        register: Option<u8>,
        data: &[u8],
    ) -> &mut Self {
        let mut line = format!("i2c_write {bus} 0x{address:02x}");
        if let Some(reg) = register {
            write!(line, " reg=0x{reg:02x}").unwrap();
        }
        let bytes: Vec<String> =
            data.iter().map(|b| format!("0x{b:02x}")).collect();
        write!(line, " {}", bytes.join(",")).unwrap();
        self.push_line(&line);
        self
    }

    /// Scan all addresses on a bus.
    pub fn i2c_scan(&mut self, bus: &str) -> &mut Self {
        self.push_line(&format!("i2c_scan {bus}"));
        self
    }

    /// Scan all registers of a device.
    pub fn i2c_regscan(&mut self, bus: &str, address: u8) -> &mut Self {
        self.push_line(&format!("i2c_regscan {bus} 0x{address:02x}"));
        self
    }

    /// Call an Idol interface operation.
    pub fn idol_call(
        &mut self,
        interface: &str,
        operation: &str,
        args: &[(&str, &str)],
    ) -> &mut Self {
        let mut line = format!("idol {interface}.{operation}");
        for (k, v) in args {
            write!(line, " {k}={v}").unwrap();
        }
        self.push_line(&line);
        self
    }

    /// Sleep for a number of milliseconds.
    pub fn sleep_ms(&mut self, ms: u32) -> &mut Self {
        self.push_line(&format!("sleep {ms}ms"));
        self
    }

    /// Repeat a block of operations.
    ///
    /// The closure receives a nested builder for the loop body.
    pub fn repeat(
        &mut self,
        count: u32,
        body: impl FnOnce(&mut ProgramBuilder),
    ) -> &mut Self {
        self.push_line(&format!("repeat {count}"));
        self.indent += 1;
        let mut inner = ProgramBuilder { lines: vec![], indent: self.indent };
        body(&mut inner);
        self.lines.extend(inner.lines);
        self.indent -= 1;
        self.push_line("end");
        self
    }

    /// Repeat with a sleep between iterations.
    pub fn repeat_with_sleep(
        &mut self,
        count: u32,
        sleep_ms: u32,
        body: impl FnOnce(&mut ProgramBuilder),
    ) -> &mut Self {
        self.push_line(&format!("repeat {count} sleep={sleep_ms}ms"));
        self.indent += 1;
        let mut inner = ProgramBuilder { lines: vec![], indent: self.indent };
        body(&mut inner);
        self.lines.extend(inner.lines);
        self.indent -= 1;
        self.push_line("end");
        self
    }

    /// Call any HIF function by name with optional numeric arguments.
    pub fn call(&mut self, function: &str, args: &[u32]) -> &mut Self {
        let mut line = format!("call {function}");
        for arg in args {
            line.push_str(&format!(" {arg}"));
        }
        self.push_line(&line);
        self
    }

    /// Add a raw HIF instruction line.
    pub fn raw_op(&mut self, instruction: &str) -> &mut Self {
        self.push_line(instruction);
        self
    }

    /// Add a comment.
    pub fn comment(&mut self, text: &str) -> &mut Self {
        self.push_line(&format!("# {text}"));
        self
    }

    fn push_line(&mut self, line: &str) {
        let indent = "    ".repeat(self.indent);
        self.lines.push(format!("{indent}{line}"));
    }
}

impl Default for ProgramBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn build_simple_read() {
        let mut prog = ProgramBuilder::new();
        prog.i2c_read("mid", 0x48, Some(0x00), 2);
        assert_eq!(prog.finish(), "i2c_read mid 0x48 reg=0x00 2");
    }

    #[test]
    fn build_muxed_read() {
        let mut prog = ProgramBuilder::new();
        prog.i2c_read_mux("front", 0x50, 0x70, 1, Some(0x00), 16);
        assert_eq!(prog.finish(), "i2c_read front 0x50 mux=0x70.1 reg=0x00 16");
    }

    #[test]
    fn build_write() {
        let mut prog = ProgramBuilder::new();
        prog.i2c_write("mid", 0x48, Some(0x01), &[0x00, 0x80]);
        assert_eq!(prog.finish(), "i2c_write mid 0x48 reg=0x01 0x00,0x80");
    }

    #[test]
    fn build_repeat_loop() {
        let mut prog = ProgramBuilder::new();
        prog.repeat(100, |body| {
            body.i2c_read("mid", 0x48, Some(0x00), 2);
        });
        let src = prog.finish();
        assert!(src.contains("repeat 100"));
        assert!(src.contains("    i2c_read mid 0x48 reg=0x00 2"));
        assert!(src.contains("end"));
    }

    #[test]
    fn build_nested_repeat() {
        let mut prog = ProgramBuilder::new();
        prog.repeat(10, |outer| {
            outer.repeat(5, |inner| {
                inner.i2c_read("mid", 0x48, None, 1);
            });
        });
        let src = prog.finish();
        assert!(src.contains("        i2c_read"));
    }

    #[test]
    fn build_idol_call() {
        let mut prog = ProgramBuilder::new();
        prog.idol_call("Sensor", "get", &[("id", "0")]);
        assert_eq!(prog.finish(), "idol Sensor.get id=0");
    }

    #[test]
    fn build_complex_program() {
        let mut prog = ProgramBuilder::new();
        prog.comment("stress test");
        prog.constant("ADDR", "0x48");
        prog.repeat_with_sleep(50, 10, |body| {
            body.i2c_read("mid", 0x48, Some(0x00), 2);
            body.idol_call("Sensor", "get", &[("id", "0")]);
        });

        let src = prog.finish();
        assert!(src.contains("# stress test"));
        assert!(src.contains(".let ADDR 0x48"));
        assert!(src.contains("repeat 50 sleep=10ms"));
        assert!(src.contains("end"));
    }

    #[test]
    fn build_and_assemble() {
        // Integration: build a program and assemble it
        use crate::assembler::HifAssembler;
        use crate::types::*;

        let config = TargetConfig {
            image_id: vec![0xDE, 0xAD],
            board: "test".into(),
            buses: vec![ResolvedBus {
                name: "mid".into(),
                controller: 3,
                port_index: 0,
                port_name: "H".into(),
                devices: vec![],
                muxes: vec![],
            }],
            functions: vec![
                FunctionInfo {
                    name: "Sleep".into(),
                    id: 0,
                    arg_count: 1,
                    args: vec![],
                    errors: vec![],
                },
                FunctionInfo {
                    name: "I2cRead".into(),
                    id: 5,
                    arg_count: 7,
                    args: vec![],
                    errors: vec![],
                },
            ],
            buffer_sizes: BufferSizes {
                text: 2048,
                data: 2048,
                rstack: 2048,
                scratch: 512,
            },
            idol_interfaces: vec![],
        };

        let asm = HifAssembler::new(config);
        let mut prog = ProgramBuilder::new();
        prog.repeat(10, |body| {
            body.i2c_read("mid", 0x48, Some(0x00), 2);
            body.sleep_ms(5);
        });

        let out = asm.assemble(&prog.finish()).expect("assemble failed");
        assert!(out.bundle.fits_in_target());
        assert_eq!(out.bundle.metadata.estimated_results, Some(10));
    }
}
