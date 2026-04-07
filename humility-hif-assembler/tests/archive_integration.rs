// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! Integration tests that load real Hubris archives.
//!
//! These tests are skipped if the archives aren't present (they are
//! build artifacts, not checked in).  Set HUBRIS_ARCHIVE to point
//! at a specific archive, or the test will look in common locations.

use std::path::PathBuf;

use humility_hif_assembler::assembler::{HifAssembler, TargetConfig};

/// Find a Hubris archive for testing.
///
/// Set `HUBRIS_ARCHIVE` to a `.zip` path.  Tests are skipped if unset.
///
/// ```bash
/// # Build an archive and run integration tests:
/// cd ~/Oxide/src/hubris/master
/// cargo xtask dist app/gimlet/rev-c-dev.toml
/// HUBRIS_ARCHIVE=$(cargo -q xtask print --archive app/gimlet/rev-c-dev.toml) \
///     cargo test -p humility-hif-assembler --test archive_integration
/// ```
fn find_archive() -> Option<String> {
    let path = std::env::var("HUBRIS_ARCHIVE").ok()?;
    if std::path::Path::new(&path).exists() {
        Some(path)
    } else {
        eprintln!("HUBRIS_ARCHIVE={path} does not exist");
        None
    }
}

#[test]
fn load_archive_and_list() {
    let path = match find_archive() {
        Some(p) => p,
        None => {
            eprintln!(
                "skipping: no Hubris archive found; \
                 set HUBRIS_ARCHIVE or build a dev image"
            );
            return;
        }
    };

    eprintln!("loading archive: {path}");
    let config =
        TargetConfig::from_archive_file(&path).expect("failed to load archive");

    eprintln!("board: {}", config.board);
    eprintln!("image_id: {:02x?}", config.image_id);
    assert!(!config.image_id.is_empty(), "image ID should not be empty");
    assert!(!config.board.is_empty(), "board name should not be empty");

    // Should have I2C buses
    eprintln!("buses ({}):", config.buses.len());
    for bus in &config.buses {
        eprintln!(
            "  {:<12} controller={} port={}:{}",
            bus.name, bus.controller, bus.port_name, bus.port_index,
        );
    }
    assert!(!config.buses.is_empty(), "should have at least one I2C bus");

    // Should have HIF functions
    eprintln!("functions ({}):", config.functions.len());
    for f in &config.functions {
        eprintln!("  [{:>2}] {:<24} args={}", f.id, f.name, f.arg_count);
    }
    assert!(
        config.functions.len() >= 3,
        "expected at least Sleep, Send, and one I2C function"
    );

    // Should include known functions
    let names: Vec<&str> =
        config.functions.iter().map(|f| f.name.as_str()).collect();
    assert!(names.contains(&"Sleep"), "missing Sleep function");
    assert!(names.contains(&"Send"), "missing Send function");

    // Buffer sizes should be reasonable
    eprintln!(
        "buffers: text={} data={} rstack={} scratch={}",
        config.buffer_sizes.text,
        config.buffer_sizes.data,
        config.buffer_sizes.rstack,
        config.buffer_sizes.scratch,
    );
    assert!(config.buffer_sizes.text >= 256, "text buffer too small");
    assert!(config.buffer_sizes.rstack >= 64, "rstack buffer too small");

    // Should serialize to JSON and back
    let json = serde_json::to_string_pretty(&config)
        .expect("failed to serialize TargetConfig");
    eprintln!("config JSON size: {} bytes", json.len());
    let roundtrip: TargetConfig =
        serde_json::from_str(&json).expect("failed to deserialize");
    assert_eq!(roundtrip.board, config.board);
    assert_eq!(roundtrip.functions.len(), config.functions.len());
}

#[test]
fn assemble_against_real_archive() {
    let path = match find_archive() {
        Some(p) => p,
        None => {
            eprintln!("skipping: no Hubris archive found");
            return;
        }
    };

    let config =
        TargetConfig::from_archive_file(&path).expect("failed to load archive");

    // Pick the first bus for testing
    let bus_name = match config.buses.first() {
        Some(b) => b.name.clone(),
        None => {
            eprintln!("skipping: no I2C buses in archive");
            return;
        }
    };

    let asm = HifAssembler::new(config);

    // Simple I2C read
    let src = format!("i2c_read {bus_name} 0x48 reg=0x00 2");
    let out = asm.assemble(&src).expect("assemble failed");
    assert!(out.bundle.fits_in_target());
    eprintln!(
        "simple read: {} bytes text, image_id={:02x?}",
        out.bundle.text.len(),
        out.bundle.metadata.image_id,
    );

    // Loop
    let src = format!("repeat 100\n  i2c_read {bus_name} 0x48 reg=0x00 2\nend");
    let out = asm.assemble(&src).expect("assemble loop failed");
    assert!(out.bundle.fits_in_target());
    assert_eq!(out.bundle.metadata.estimated_results, Some(100));
    eprintln!("loop: {} bytes text", out.bundle.text.len());

    // Verify mode
    let report = asm.verify(&format!(
        "repeat 100\n  i2c_read {bus_name} 0x48 reg=0x00 2\nend"
    ));
    assert!(report.ok);
    eprintln!("verify:\n{report}");
}

/// Generate a fixture JSON file from a real archive.
///
/// Run with:
/// ```bash
/// HUBRIS_ARCHIVE=path/to/archive.zip \
///   GENERATE_FIXTURE=1 \
///   cargo test -p humility-hif-assembler --test archive_integration \
///     generate_fixture -- --nocapture
/// ```
#[test]
fn generate_fixture() {
    if std::env::var("GENERATE_FIXTURE").is_err() {
        return;
    }

    let path = match find_archive() {
        Some(p) => p,
        None => {
            eprintln!("skipping: no Hubris archive found");
            return;
        }
    };

    let config =
        TargetConfig::from_archive_file(&path).expect("failed to load archive");

    let json =
        serde_json::to_string_pretty(&config).expect("failed to serialize");

    let fixture_name = format!("{}.json", config.board);
    let fixture_dir: PathBuf =
        [env!("CARGO_MANIFEST_DIR"), "fixtures"].iter().collect();
    let fixture_path = fixture_dir.join(&fixture_name);

    std::fs::create_dir_all(&fixture_dir).ok();
    std::fs::write(&fixture_path, &json).expect("failed to write fixture");
    eprintln!(
        "wrote fixture: {} ({} bytes)",
        fixture_path.display(),
        json.len(),
    );
}
