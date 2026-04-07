// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! Tests that load checked-in TargetConfig fixtures.
//!
//! These tests don't need a Hubris archive — they use pre-generated
//! JSON files from `fixtures/`.

use std::path::PathBuf;

use humility_hif_assembler::assembler::{HifAssembler, TargetConfig};

fn fixture_dir() -> PathBuf {
    [env!("CARGO_MANIFEST_DIR"), "fixtures"].iter().collect()
}

fn load_fixture(name: &str) -> TargetConfig {
    let path = fixture_dir().join(name);
    let json = std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("reading {}: {e}", path.display()));
    serde_json::from_str(&json)
        .unwrap_or_else(|e| panic!("parsing {}: {e}", path.display()))
}

fn examples_dir() -> PathBuf {
    [env!("CARGO_MANIFEST_DIR"), "examples"].iter().collect()
}

fn load_gimlet() -> TargetConfig {
    load_fixture("gimlet-c.json")
}

#[test]
fn fixture_loads_and_has_topology() {
    let config = load_gimlet();
    assert_eq!(config.board, "gimlet-c");
    assert!(!config.image_id.is_empty());
    assert!(config.buses.len() >= 4);
    assert!(config.functions.len() >= 10);

    // Mid bus should have direct devices
    let mid = config.buses.iter().find(|b| b.name == "mid").unwrap();
    assert!(!mid.devices.is_empty());

    // Front bus should have muxes
    let front = config.buses.iter().find(|b| b.name == "front").unwrap();
    assert!(!front.muxes.is_empty());

    // Devices should have sensors
    let has_sensors = mid.devices.iter().any(|d| !d.sensors.is_empty());
    assert!(has_sensors, "expected some devices with sensors on mid bus");
}

#[test]
fn assemble_i2c_read_from_fixture() {
    let asm = HifAssembler::new(load_gimlet());
    let out =
        asm.assemble("i2c_read mid 0x48 reg=0x00 2").expect("assemble failed");
    assert!(out.bundle.fits_in_target());
    assert!(!out.bundle.metadata.image_id.is_empty());
}

#[test]
fn assemble_stress_loop_from_fixture() {
    let asm = HifAssembler::new(load_gimlet());
    let src = "repeat 500\n  i2c_read mid 0x48 reg=0x00 2\nend";
    let out = asm.assemble(src).expect("assemble failed");
    assert!(out.bundle.fits_in_target());
    assert_eq!(out.bundle.metadata.estimated_results, Some(500));
}

#[test]
fn assemble_muxed_read_from_fixture() {
    let config = load_gimlet();
    let front = config.buses.iter().find(|b| b.name == "front").unwrap();
    let mux = &front.muxes[0];
    let seg = &mux.segments[0];
    let dev = &seg.devices[0];

    let src = format!(
        "i2c_read front 0x{:02x} mux=0x{:02x}.{} 1",
        dev.address, mux.address, seg.segment,
    );
    let asm = HifAssembler::new(config);
    let out = asm.assemble(&src).expect("assemble muxed read failed");
    assert!(out.bundle.fits_in_target());
}

#[test]
fn assemble_bus_scan_from_fixture() {
    let asm = HifAssembler::new(load_gimlet());
    let report = asm.verify("i2c_scan mid");
    assert!(report.ok);
    assert_eq!(report.estimated_results, 128);
}

#[test]
fn assemble_multi_bus_from_fixture() {
    let config = load_gimlet();
    let bus_names: Vec<&str> =
        config.buses.iter().map(|b| b.name.as_str()).collect();

    // Read from each bus in sequence
    let mut lines = vec![];
    for name in &bus_names {
        lines.push(format!("i2c_read {name} 0x48 2"));
    }
    let src = lines.join("\n");

    let asm = HifAssembler::new(config);
    let out = asm.assemble(&src).expect("assemble multi-bus failed");
    assert!(out.bundle.fits_in_target());
}

#[test]
fn verify_reports_all_buses() {
    let config = load_gimlet();
    let bus_names: Vec<String> =
        config.buses.iter().map(|b| b.name.clone()).collect();

    let mut lines = vec![];
    for name in &bus_names {
        lines.push(format!("i2c_read {name} 0x48 2"));
    }
    let src = lines.join("\n");

    let asm = HifAssembler::new(config);
    let report = asm.verify(&src);
    assert!(report.ok);
    for name in &bus_names {
        assert!(
            report.buses_used.contains(name),
            "expected bus '{name}' in report"
        );
    }
}

#[test]
fn cosmo_fixture_loads() {
    let path = fixture_dir().join("cosmo-b.json");
    if !path.exists() {
        eprintln!("skipping: cosmo-b.json not found");
        return;
    }
    let config = load_fixture("cosmo-b.json");
    assert_eq!(config.board, "cosmo-b");
    assert!(!config.buses.is_empty());

    let asm = HifAssembler::new(config);
    let report = asm.verify("i2c_read front 0x48 2");
    assert!(report.ok, "verify failed: {report}");
}

#[test]
fn assemble_idol_sensor_get() {
    let asm = HifAssembler::new(load_gimlet());
    let out = asm
        .assemble("idol Sensor.get id=0")
        .expect("assemble idol Sensor.get failed");
    assert!(out.bundle.fits_in_target());
}

#[test]
fn assemble_idol_thermal_get_mode() {
    let config = load_gimlet();
    let has_thermal =
        config.idol_interfaces.iter().any(|i| i.name == "Thermal");
    if !has_thermal {
        eprintln!("skipping: no Thermal interface in fixture");
        return;
    }
    let asm = HifAssembler::new(config);
    let out = asm
        .assemble("idol Thermal.get_mode")
        .expect("assemble idol Thermal.get_mode failed");
    assert!(out.bundle.fits_in_target());
}

#[test]
fn assemble_idol_in_stress_loop() {
    let asm = HifAssembler::new(load_gimlet());
    let src = "repeat 50\n  idol Sensor.get id=0\nend";
    let out = asm.assemble(src).expect("assemble idol loop failed");
    assert!(out.bundle.fits_in_target());
    assert_eq!(out.bundle.metadata.estimated_results, Some(50));
}

#[test]
fn all_examples_assemble() {
    let asm = HifAssembler::new(load_gimlet());
    let dir = examples_dir();

    let mut count = 0;
    for entry in std::fs::read_dir(&dir)
        .unwrap_or_else(|e| panic!("reading {}: {e}", dir.display()))
    {
        let entry = entry.unwrap();
        let path = entry.path();
        if path.extension().map(|e| e == "hif").unwrap_or(false) {
            let source = std::fs::read_to_string(&path)
                .unwrap_or_else(|e| panic!("reading {}: {e}", path.display()));
            let report = asm.verify(&source);
            assert!(
                report.ok,
                "{}: {report}",
                path.file_name().unwrap().to_string_lossy(),
            );
            count += 1;
        }
    }
    assert!(count >= 8, "expected at least 8 examples, found {count}");
}
