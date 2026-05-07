// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! Tests that verify our assembler produces the same ops as
//! humility's hardcoded HIF programs.
//!
//! Each test assembles an equivalent .hif program and checks
//! the generated ops match the expected pattern.

use std::path::PathBuf;

use humility_hif_assembler::assembler::{HifAssembler, TargetConfig};

fn fixture_dir() -> PathBuf {
    [env!("CARGO_MANIFEST_DIR"), "fixtures"].iter().collect()
}

fn examples_dir() -> PathBuf {
    [env!("CARGO_MANIFEST_DIR"), "examples"].iter().collect()
}

fn load_gimlet() -> TargetConfig {
    let path = fixture_dir().join("gimlet-c.json");
    let json = std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("reading {}: {e}", path.display()));
    serde_json::from_str(&json)
        .unwrap_or_else(|e| panic!("parsing {}: {e}", path.display()))
}

fn assemble(config: TargetConfig, source: &str) -> Vec<hif::Op> {
    let asm = HifAssembler::new(config);
    let output = asm.assemble(source).unwrap_or_else(|e| {
        panic!("assembling failed: {e}\nsource:\n{source}")
    });
    output.ops
}

fn assemble_file(config: TargetConfig, name: &str) -> Vec<hif::Op> {
    let path = examples_dir().join(name);
    let source = std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("reading {}: {e}", path.display()));
    assemble(config, &source)
}

//
// Equivalent program tests:
// Tedious testing to make sure that we can reproduce all of the
// hard-coded HIF programs that exist in humility prior to the
// introduction of the HIF assembler.
//

#[test]
fn equivalent_qspi_read_id() {
    let ops = assemble_file(load_gimlet(), "equivalent-qspi-read-id.hif");
    // Should be: Call(QspiReadId), Done
    assert!(ops.iter().any(|op| matches!(op, hif::Op::Call(_))));
    assert_eq!(*ops.last().unwrap(), hif::Op::Done);
    // No loops, no drops (zero-arg function)
    assert!(!ops.iter().any(|op| matches!(op, hif::Op::Label(_))));
}

#[test]
fn equivalent_qspi_read_status() {
    let ops = assemble_file(load_gimlet(), "equivalent-qspi-read-status.hif");
    assert!(ops.iter().any(|op| matches!(op, hif::Op::Call(_))));
    assert_eq!(*ops.last().unwrap(), hif::Op::Done);
}

#[test]
fn equivalent_i2c_scan() {
    let ops = assemble_file(load_gimlet(), "equivalent-i2c-scan.hif");
    // Should have: Label, BranchGreaterThanOrEqualTo (scan loop)
    assert!(ops.iter().any(|op| matches!(op, hif::Op::Label(_))));
    assert!(ops
        .iter()
        .any(|op| matches!(op, hif::Op::BranchGreaterThanOrEqualTo(_))));
    // Should call I2cRead
    assert!(ops.iter().any(|op| matches!(op, hif::Op::Call(_))));
    // Should push 128 as the scan limit
    assert!(ops.iter().any(|op| matches!(op, hif::Op::Push(128))));
}

#[test]
fn equivalent_i2c_read() {
    let ops = assemble_file(load_gimlet(), "equivalent-i2c-read.hif");
    // Simple: push params, Call, DropN(7), Done
    assert!(ops.iter().any(|op| matches!(op, hif::Op::Call(_))));
    assert!(ops.iter().any(|op| matches!(op, hif::Op::DropN(7))));
    assert_eq!(*ops.last().unwrap(), hif::Op::Done);
    // No loops
    assert!(!ops.iter().any(|op| matches!(op, hif::Op::Label(_))));
}

#[test]
fn equivalent_i2c_regscan() {
    let ops = assemble_file(load_gimlet(), "equivalent-i2c-regscan.hif");
    // Should have: Label, BranchGreaterThanOrEqualTo (regscan loop)
    assert!(ops.iter().any(|op| matches!(op, hif::Op::Label(_))));
    assert!(ops
        .iter()
        .any(|op| matches!(op, hif::Op::BranchGreaterThanOrEqualTo(_))));
    // Should push 0xff as the scan limit
    assert!(ops.iter().any(|op| matches!(op, hif::Op::Push(0xff))));
}

#[test]
fn equivalent_gpio_input() {
    let ops = assemble_file(load_gimlet(), "equivalent-gpio-input.hif");
    // call GpioInput 5 -> Push(5), Call, DropN(1), Done
    assert!(ops.iter().any(|op| matches!(op, hif::Op::Push(5))));
    assert!(ops.iter().any(|op| matches!(op, hif::Op::Call(_))));
    assert!(ops.iter().any(|op| matches!(op, hif::Op::DropN(1))));
}

#[test]
fn equivalent_gpio_toggle() {
    let ops = assemble_file(load_gimlet(), "equivalent-gpio-toggle.hif");
    // call GpioToggle 5 1 -> Push(5), Push(1), Call, DropN(2), Done
    assert!(ops.iter().any(|op| matches!(op, hif::Op::Push(5))));
    assert!(ops.iter().any(|op| matches!(op, hif::Op::Call(_))));
    assert!(ops.iter().any(|op| matches!(op, hif::Op::DropN(2))));
}

#[test]
fn equivalent_hash_digest() {
    let ops = assemble_file(load_gimlet(), "equivalent-hash-digest.hif");
    // call HashDigest 32 -> Push(32), Call, DropN(1), Done
    assert!(ops.iter().any(|op| matches!(op, hif::Op::Push(32))));
    assert!(ops.iter().any(|op| matches!(op, hif::Op::Call(_))));
}

#[test]
fn equivalent_idol_sensor_get() {
    let ops = assemble_file(load_gimlet(), "equivalent-idol-sensor-get.hif");
    // Idol call: push task_id, op_code, payload, sizes, Call(Send)
    assert!(ops.iter().any(|op| matches!(op, hif::Op::Call(_))));
    // Should have a DropN to clean up the frame
    assert!(ops.iter().any(|op| matches!(op, hif::Op::DropN(_))));
}

#[test]
fn equivalent_validate_i2c() {
    let ops = assemble_file(load_gimlet(), "equivalent-validate-i2c.hif");
    // 4 Idol calls, each with Call + DropN
    let call_count =
        ops.iter().filter(|op| matches!(op, hif::Op::Call(_))).count();
    assert_eq!(call_count, 4, "expected 4 Idol calls");
}

#[test]
fn all_equivalent_examples_assemble() {
    let config = load_gimlet();
    let asm = HifAssembler::new(config);
    let dir = examples_dir();

    let mut count = 0;
    for entry in std::fs::read_dir(&dir).unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        let name = path.file_name().unwrap().to_string_lossy();
        if name.starts_with("equivalent-") && name.ends_with(".hif") {
            let source = std::fs::read_to_string(&path).unwrap();
            let report = asm.verify(&source);
            assert!(report.ok, "{name}: {report}",);
            count += 1;
        }
    }
    assert!(
        count >= 8,
        "expected at least 8 equivalent examples, found {count}"
    );
}

// ProgramBuilder equivalents:
// These show how a Rust program can construct the the pre-assembler
// existing HIF programs using the ProgramBuilder API.

#[test]
fn builder_qspi_read_id() {
    use humility_hif_assembler::builder::ProgramBuilder;

    let mut prog = ProgramBuilder::new();
    prog.call("QspiReadId", &[]);

    let ops = assemble(load_gimlet(), &prog.finish());
    assert!(ops.iter().any(|op| matches!(op, hif::Op::Call(_))));
}

#[test]
fn builder_i2c_stress() {
    use humility_hif_assembler::builder::ProgramBuilder;

    let mut prog = ProgramBuilder::new();
    prog.comment("I2C stress: 100 reads on mid bus");
    prog.repeat(100, |body| {
        body.i2c_read("mid", 0x48, Some(0x00), 2);
    });

    let config = load_gimlet();
    let asm = HifAssembler::new(config);
    let output = asm.assemble(&prog.finish()).unwrap();
    assert!(output.bundle.fits_in_target());
    assert_eq!(output.stats.i2c_reads, 100);
    assert_eq!(output.stats.i2c_read_bytes, 200);
}

#[test]
fn builder_multi_bus_interleave() {
    use humility_hif_assembler::builder::ProgramBuilder;

    let mut prog = ProgramBuilder::new();
    prog.repeat(50, |body| {
        body.i2c_read("mid", 0x24, Some(0x00), 2);
        body.i2c_read("rear", 0x48, Some(0x00), 2);
    });

    let config = load_gimlet();
    let asm = HifAssembler::new(config);
    let output = asm.assemble(&prog.finish()).unwrap();
    assert_eq!(output.stats.i2c_reads, 100);
    assert_eq!(output.stats.buses_touched.len(), 2);
}

#[test]
fn builder_idol_sensor_loop() {
    use humility_hif_assembler::builder::ProgramBuilder;

    let mut prog = ProgramBuilder::new();
    prog.repeat(20, |body| {
        body.idol_call("Sensor", "get", &[("id", "0")]);
    });

    let config = load_gimlet();
    let asm = HifAssembler::new(config);
    let output = asm.assemble(&prog.finish()).unwrap();
    assert_eq!(output.stats.idol_calls, 20);
}

#[test]
fn builder_mixed_i2c_and_idol() {
    use humility_hif_assembler::builder::ProgramBuilder;

    let mut prog = ProgramBuilder::new();
    prog.comment("Mixed stress: I2C + Idol");
    prog.repeat(30, |body| {
        body.i2c_read("mid", 0x48, Some(0x00), 2);
        body.idol_call("Sensor", "get", &[("id", "0")]);
        body.sleep_ms(5);
    });

    let config = load_gimlet();
    let asm = HifAssembler::new(config);
    let output = asm.assemble(&prog.finish()).unwrap();
    assert_eq!(output.stats.i2c_reads, 30);
    assert_eq!(output.stats.idol_calls, 30);
    assert_eq!(output.stats.sleep_ms, 150);
}

#[test]
fn builder_mux_switching() {
    use humility_hif_assembler::builder::ProgramBuilder;

    let mut prog = ProgramBuilder::new();
    prog.repeat(20, |body| {
        body.i2c_read_mux("front", 0x50, 0x70, 1, None, 1);
        body.i2c_read_mux("front", 0x50, 0x70, 2, None, 1);
    });

    let config = load_gimlet();
    let asm = HifAssembler::new(config);
    let output = asm.assemble(&prog.finish()).unwrap();
    assert_eq!(output.stats.i2c_reads, 40);
    assert!(output.stats.mux_switches > 0);
}

#[test]
fn builder_call_generic_functions() {
    use humility_hif_assembler::builder::ProgramBuilder;

    // Show that any HIF function can be called by name
    let mut prog = ProgramBuilder::new();
    prog.call("QspiReadId", &[]);
    prog.call("QspiReadStatus", &[]);
    prog.call("HashInit", &[]);
    prog.call("HashDigest", &[32]);

    let config = load_gimlet();
    let asm = HifAssembler::new(config);
    let output = asm.assemble(&prog.finish()).unwrap();
    assert!(output.bundle.fits_in_target());
}
