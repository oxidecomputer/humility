// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! HIF assembler: the public API for assembling and verifying
//! programs.
//!
//! Types are defined in [`types`](crate::types).  Lowering logic is
//! in [`lower`](crate::lower).

use std::collections::{BTreeSet, HashMap};
use std::fmt;

use anyhow::{Context, Result, bail};

use crate::bundle::{BUNDLE_VERSION, BundleMetadata, HifBundle};
use crate::error::{HifError, HifErrorKind};
use crate::lower::{RSTACK_BYTES_PER_RESULT, normalize_function_name};
use crate::parser::parse;

// Re-export types so `crate::assembler::Foo` still works for
// existing imports in tests and other modules.
pub use crate::types::*;

/// The assembler, loaded from a [`TargetConfig`].
///
/// Create one per target, then call [`assemble`](Self::assemble) or
/// [`verify`](Self::verify) for each program.
pub struct HifAssembler {
    pub(crate) config: TargetConfig,
    pub(crate) buses: HashMap<String, ResolvedBus>,
    /// Functions by their canonical name (from DWARF).
    pub(crate) functions: HashMap<String, FunctionInfo>,
    /// Normalized aliases: `strip('_').lowercase()` -> canonical name.
    pub(crate) function_aliases: HashMap<String, String>,
}

/// Output of a successful assembly.
#[derive(Debug)]
pub struct AssembleOutput {
    pub bundle: HifBundle,
    pub warnings: Vec<String>,
}

/// Output of `verify` -- assembly stats without binary output.
#[derive(Debug)]
pub struct VerifyReport {
    /// Whether the program assembled without errors.
    pub ok: bool,
    /// Errors found during assembly.
    pub errors: Vec<HifError>,
    /// Non-fatal warnings.
    pub warnings: Vec<String>,
    /// Assembled text size in bytes (0 if errors).
    pub text_bytes: usize,
    /// Target HIFFY_TEXT limit.
    pub text_limit: usize,
    /// Estimated number of results the program produces.
    pub estimated_results: usize,
    /// Estimated result bytes.
    pub estimated_rstack_bytes: usize,
    /// Target HIFFY_RSTACK limit.
    pub rstack_limit: usize,
    /// HIF functions referenced.
    pub functions_used: BTreeSet<String>,
    /// I2C buses referenced.
    pub buses_used: BTreeSet<String>,
    /// Number of labels (loop nesting depth) used.
    pub labels_used: usize,
}

impl fmt::Display for VerifyReport {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.ok {
            writeln!(f, "OK")?;
        } else {
            writeln!(f, "ERRORS:")?;
            for err in &self.errors {
                writeln!(f, "  {err}")?;
            }
        }
        for w in &self.warnings {
            writeln!(f, "  warning: {w}")?;
        }
        writeln!(f)?;
        writeln!(
            f,
            "  text:    {:>5} / {} bytes{}",
            self.text_bytes,
            self.text_limit,
            if self.text_bytes > self.text_limit {
                " ** OVERFLOW **"
            } else {
                ""
            }
        )?;
        writeln!(
            f,
            "  rstack:  {:>5} / {} bytes (est. {} results){}",
            self.estimated_rstack_bytes,
            self.rstack_limit,
            self.estimated_results,
            if self.estimated_rstack_bytes > self.rstack_limit {
                " ** OVERFLOW **"
            } else {
                ""
            }
        )?;
        writeln!(f, "  labels:  {:>5} / {MAX_LABELS}", self.labels_used)?;
        if !self.functions_used.is_empty() {
            writeln!(
                f,
                "  functions: {}",
                self.functions_used
                    .iter()
                    .cloned()
                    .collect::<Vec<_>>()
                    .join(", ")
            )?;
        }
        if !self.buses_used.is_empty() {
            writeln!(
                f,
                "  buses: {}",
                self.buses_used.iter().cloned().collect::<Vec<_>>().join(", ")
            )?;
        }
        Ok(())
    }
}

impl HifAssembler {
    /// Create an assembler from a [`TargetConfig`].
    ///
    /// This is the primary constructor.  For archive-based
    /// construction, use [`TargetConfig::from_archive_file`] or
    /// [`TargetConfig::from_archive`] and pass the result here.
    pub fn new(config: TargetConfig) -> Self {
        let buses =
            config.buses.iter().map(|b| (b.name.clone(), b.clone())).collect();
        let functions: HashMap<String, FunctionInfo> = config
            .functions
            .iter()
            .map(|f| (f.name.clone(), f.clone()))
            .collect();

        let mut function_aliases = HashMap::new();
        for name in functions.keys() {
            let normalized = normalize_function_name(name);
            function_aliases.insert(normalized, name.clone());
        }

        HifAssembler { config, buses, functions, function_aliases }
    }

    /// The target configuration this assembler was built from.
    pub fn target_config(&self) -> &TargetConfig {
        &self.config
    }

    /// List known I2C buses for this target.
    pub fn list_buses(&self) -> Vec<&ResolvedBus> {
        let mut buses: Vec<_> = self.buses.values().collect();
        buses.sort_by_key(|b| &b.name);
        buses
    }

    /// List HIF functions available on this target.
    pub fn list_functions(&self) -> Vec<&FunctionInfo> {
        let mut funcs: Vec<_> = self.functions.values().collect();
        funcs.sort_by_key(|f| f.id);
        funcs
    }

    /// Image ID from the archive.
    pub fn image_id(&self) -> &[u8] {
        &self.config.image_id
    }

    /// Board name from the archive.
    pub fn board(&self) -> &str {
        &self.config.board
    }

    /// Target buffer sizes.
    pub fn buffer_sizes(&self) -> &BufferSizes {
        &self.config.buffer_sizes
    }

    /// Verify a program: assemble it and report stats and errors, but
    /// don't produce binary output.
    pub fn verify(&self, source: &str) -> VerifyReport {
        let mut report = VerifyReport {
            ok: false,
            errors: vec![],
            warnings: vec![],
            text_bytes: 0,
            text_limit: self.config.buffer_sizes.text,
            estimated_results: 0,
            estimated_rstack_bytes: 0,
            rstack_limit: self.config.buffer_sizes.rstack,
            functions_used: BTreeSet::new(),
            buses_used: BTreeSet::new(),
            labels_used: 0,
        };

        let parsed = match parse(source) {
            Ok(p) => p,
            Err(e) => {
                report.errors.push(e);
                return report;
            }
        };

        match self.lower_program(&parsed) {
            Ok(result) => {
                report.warnings = result.warnings;
                report.functions_used = result.functions_used;
                report.buses_used = result.buses_used;
                report.labels_used = result.labels_used;
                report.estimated_results = result.estimated_results;
                report.estimated_rstack_bytes =
                    result.estimated_results * RSTACK_BYTES_PER_RESULT;

                match postcard::to_allocvec::<Vec<hif::Op>>(&result.ops) {
                    Ok(bytes) => {
                        report.text_bytes = bytes.len();
                    }
                    Err(e) => {
                        report.errors.push(HifError {
                            line: 0,
                            col: None,
                            kind: HifErrorKind::Parse(format!(
                                "serialization failed: {e}"
                            )),
                        });
                    }
                }
            }
            Err(e) => {
                report.errors.push(HifError {
                    line: 0,
                    col: None,
                    kind: HifErrorKind::Parse(format!("{e}")),
                });
            }
        }

        if report.text_bytes > report.text_limit {
            report.errors.push(HifError {
                line: 0,
                col: None,
                kind: HifErrorKind::TextOverflow {
                    program_bytes: report.text_bytes,
                    limit: report.text_limit,
                },
            });
        }
        if report.estimated_rstack_bytes > report.rstack_limit {
            report.errors.push(HifError {
                line: 0,
                col: None,
                kind: HifErrorKind::RstackOverflow {
                    estimated_bytes: report.estimated_rstack_bytes,
                    limit: report.rstack_limit,
                },
            });
        }
        if report.labels_used > MAX_LABELS {
            report.errors.push(HifError {
                line: 0,
                col: None,
                kind: HifErrorKind::LabelOverflow {
                    used: report.labels_used,
                    max: MAX_LABELS,
                },
            });
        }

        report.ok = report.errors.is_empty();
        report
    }

    /// Assemble a program from source text into a [`HifBundle`].
    pub fn assemble(&self, source: &str) -> Result<AssembleOutput> {
        let parsed = parse(source).map_err(|e| anyhow::anyhow!("{e}"))?;
        let result = self.lower_program(&parsed)?;

        let text = postcard::to_allocvec(&result.ops)
            .context("serializing HIF bytecode")?;

        if text.len() > self.config.buffer_sizes.text {
            bail!(
                "assembled program is {} bytes, \
                 exceeds HIFFY_TEXT limit of {}",
                text.len(),
                self.config.buffer_sizes.text,
            );
        }

        let metadata = BundleMetadata {
            version: BUNDLE_VERSION,
            image_id: self.config.image_id.clone(),
            board: self.config.board.clone(),
            text_size: text.len(),
            data_size: result.data.len(),
            target_text_size: self.config.buffer_sizes.text,
            target_rstack_size: self.config.buffer_sizes.rstack,
            functions_used: result.functions_used.into_iter().collect(),
            source: None,
            estimated_results: Some(result.estimated_results),
            source_text: Some(source.to_string()),
        };

        let bundle = HifBundle { metadata, text, data: result.data };

        Ok(AssembleOutput { bundle, warnings: result.warnings })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A minimal target config for testing.
    fn test_config() -> TargetConfig {
        TargetConfig {
            image_id: vec![0xDE, 0xAD],
            board: "test-board".into(),
            buses: vec![
                ResolvedBus {
                    name: "mid".into(),
                    controller: 3,
                    port_index: 0,
                    port_name: "H".into(),
                    devices: vec![I2cDeviceInfo {
                        address: 0x48,
                        device: "sbtsi".into(),
                        name: Some("CPU".into()),
                        description: "CPU temperature".into(),
                        removable: false,
                        sensors: vec![SensorInfo {
                            name: "CPU".into(),
                            kind: "Temperature".into(),
                        }],
                    }],
                    muxes: vec![],
                },
                ResolvedBus {
                    name: "front".into(),
                    controller: 2,
                    port_index: 1,
                    port_name: "F".into(),
                    devices: vec![],
                    muxes: vec![I2cMuxInfo {
                        address: 0x70,
                        segments: vec![I2cMuxSegment {
                            segment: 1,
                            devices: vec![I2cDeviceInfo {
                                address: 0x50,
                                device: "at24csw080".into(),
                                name: None,
                                description: "U.2 A VPD".into(),
                                removable: true,
                                sensors: vec![],
                            }],
                        }],
                    }],
                },
            ],
            // Names match the DWARF enum variants from a real Hubris
            // hiffy task (PascalCase).  The alias table handles
            // snake_case lookups (e.g. i2c_read -> I2cRead).
            functions: vec![
                FunctionInfo {
                    name: "Sleep".into(),
                    id: 0,
                    arg_count: 1,
                    args: vec![FunctionArg {
                        name: "ms".into(),
                        ty: "u32".into(),
                        size: Some(4),
                    }],
                    errors: vec![],
                },
                FunctionInfo {
                    name: "Send".into(),
                    id: 1,
                    arg_count: 4,
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
                FunctionInfo {
                    name: "I2cWrite".into(),
                    id: 6,
                    arg_count: 8,
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
            idol_interfaces: vec![IdolInterfaceInfo {
                name: "Sensor".into(),
                task: "sensor".into(),
                task_id: 7,
                ops: vec![
                    IdolOpInfo {
                        name: "get".into(),
                        code: 1,
                        args: vec![IdolArgInfo {
                            name: "id".into(),
                            ty: "SensorId".into(),
                        }],
                        reply: "f32".into(),
                        error: Some("SensorError".into()),
                        leases: vec![],
                        idempotent: true,
                    },
                    IdolOpInfo {
                        name: "post".into(),
                        code: 8,
                        args: vec![
                            IdolArgInfo {
                                name: "id".into(),
                                ty: "SensorId".into(),
                            },
                            IdolArgInfo {
                                name: "value".into(),
                                ty: "f32".into(),
                            },
                            IdolArgInfo {
                                name: "timestamp".into(),
                                ty: "u64".into(),
                            },
                        ],
                        reply: "()".into(),
                        error: Some("SensorError".into()),
                        leases: vec![],
                        idempotent: false,
                    },
                ],
            }],
        }
    }

    fn test_assembler() -> HifAssembler {
        HifAssembler::new(test_config())
    }

    #[test]
    fn assemble_simple_i2c_read() {
        let c = test_assembler();
        let out = c.assemble("i2c_read mid 0x48 reg=0x00 2").unwrap();
        assert!(out.bundle.fits_in_target());
        assert!(out.bundle.text.len() > 0);
        assert_eq!(out.bundle.metadata.image_id, vec![0xDE, 0xAD]);
        assert!(
            out.bundle
                .metadata
                .functions_used
                .contains(&"i2c_read".to_string())
        );
    }

    #[test]
    fn assemble_i2c_read_with_mux() {
        let c = test_assembler();
        let out =
            c.assemble("i2c_read front 0x50 mux=0x70.1 reg=0x00 16").unwrap();
        assert!(out.bundle.fits_in_target());
    }

    #[test]
    fn assemble_i2c_write() {
        let c = test_assembler();
        let out = c.assemble("i2c_write mid 0x48 reg=0x01 0x00,0x80").unwrap();
        assert!(out.bundle.fits_in_target());
    }

    #[test]
    fn assemble_i2c_write_too_many_bytes() {
        let c = test_assembler();
        let bytes = (0..17)
            .map(|i| format!("0x{:02x}", i))
            .collect::<Vec<_>>()
            .join(",");
        let src = format!("i2c_write mid 0x48 {bytes}");
        let result = c.assemble(&src);
        assert!(result.is_err());
        let msg = format!("{}", result.unwrap_err());
        assert!(msg.contains("maximum is 16"), "got: {msg}");
    }

    #[test]
    fn assemble_repeat_loop() {
        let c = test_assembler();
        let out = c
            .assemble("repeat 10\n  i2c_read mid 0x48 reg=0x00 2\nend")
            .unwrap();
        assert!(out.bundle.fits_in_target());
        assert_eq!(out.bundle.metadata.estimated_results, Some(10));
    }

    #[test]
    fn assemble_repeat_with_sleep() {
        let c = test_assembler();
        let out = c
            .assemble(
                "repeat 5 sleep=50ms\n  i2c_read mid 0x48 reg=0x00 2\nend",
            )
            .unwrap();
        assert!(out.bundle.fits_in_target());
    }

    #[test]
    fn assemble_repeat_zero_is_error() {
        let c = test_assembler();
        let result = c.assemble("repeat 0\n  i2c_read mid 0x48 2\nend");
        assert!(result.is_err());
    }

    #[test]
    fn assemble_nested_repeat() {
        let c = test_assembler();
        let src = "\
            repeat 10\n\
              repeat 5\n\
                i2c_read mid 0x48 2\n\
              end\n\
            end";
        let out = c.assemble(src).unwrap();
        assert_eq!(out.bundle.metadata.estimated_results, Some(50));
    }

    #[test]
    fn assemble_sleep_over_100ms() {
        let c = test_assembler();
        let out = c.assemble("sleep 250ms").unwrap();
        assert!(out.bundle.fits_in_target());
    }

    #[test]
    fn assemble_raw_block() {
        let c = test_assembler();
        let out =
            c.assemble("raw {\n  push 0x48\n  push_none\n  done\n}").unwrap();
        assert!(out.bundle.fits_in_target());
    }

    #[test]
    fn assemble_raw_call_by_name() {
        let c = test_assembler();
        let out = c.assemble("raw {\n  call i2c_read\n  done\n}").unwrap();
        assert!(out.bundle.fits_in_target());
    }

    #[test]
    fn assemble_unknown_bus_is_error() {
        let c = test_assembler();
        assert!(c.assemble("i2c_read nonexistent 0x48 2").is_err());
    }

    #[test]
    fn assemble_unknown_function_is_error() {
        let c = test_assembler();
        assert!(c.assemble("raw {\n  call nonexistent\n}").is_err());
    }

    #[test]
    fn verify_reports_stats() {
        let c = test_assembler();
        let report =
            c.verify("repeat 100\n  i2c_read mid 0x48 reg=0x00 2\nend");
        assert!(report.ok);
        assert_eq!(report.estimated_results, 100);
        assert!(report.text_bytes > 0);
        assert!(report.functions_used.contains("i2c_read"));
        assert!(report.buses_used.contains("mid"));
        assert_eq!(report.labels_used, 1);
    }

    #[test]
    fn verify_catches_overflow() {
        let config = TargetConfig {
            buffer_sizes: BufferSizes {
                text: 10,
                data: 10,
                rstack: 10,
                scratch: 10,
            },
            ..test_config()
        };
        let c = HifAssembler::new(config);
        let report = c.verify("i2c_read mid 0x48 reg=0x00 2");
        assert!(!report.ok);
        assert!(
            report
                .errors
                .iter()
                .any(|e| matches!(&e.kind, HifErrorKind::TextOverflow { .. }))
        );
    }

    #[test]
    fn target_config_roundtrip() {
        let config = test_config();
        let json = serde_json::to_string(&config).unwrap();
        let parsed: TargetConfig = serde_json::from_str(&json).unwrap();
        assert_eq!(parsed.board, config.board);
        assert_eq!(parsed.image_id, config.image_id);
        assert_eq!(parsed.buses.len(), config.buses.len());
        assert_eq!(parsed.functions.len(), config.functions.len());
    }

    #[test]
    fn explicit_bus_unknown_port_warns() {
        let c = test_assembler();
        let report = c.verify("i2c_read 3.Z 0x48 2");
        assert!(report.ok);
        assert!(
            report.warnings.iter().any(|w| w.contains("not found")),
            "expected warning about unknown port, got: {:?}",
            report.warnings
        );
    }

    #[test]
    fn assemble_idol_call() {
        let c = test_assembler();
        let out = c.assemble("idol Sensor.get id=3").unwrap();
        assert!(out.bundle.fits_in_target());
        assert!(
            out.bundle.metadata.functions_used.contains(&"Send".to_string())
        );
    }

    #[test]
    fn assemble_idol_call_multi_arg() {
        let c = test_assembler();
        let out = c
            .assemble("idol Sensor.post id=0 value=25.5 timestamp=1000")
            .unwrap();
        assert!(out.bundle.fits_in_target());
    }

    #[test]
    fn assemble_idol_unknown_interface() {
        let c = test_assembler();
        let result = c.assemble("idol Nonexistent.get id=3");
        assert!(result.is_err());
        let msg = format!("{}", result.unwrap_err());
        assert!(msg.contains("unknown Idol interface"), "got: {msg}");
    }

    #[test]
    fn assemble_idol_unknown_operation() {
        let c = test_assembler();
        let result = c.assemble("idol Sensor.nonexistent id=3");
        assert!(result.is_err());
        let msg = format!("{}", result.unwrap_err());
        assert!(msg.contains("unknown operation"), "got: {msg}");
    }

    #[test]
    fn assemble_idol_missing_arg() {
        let c = test_assembler();
        let result = c.assemble("idol Sensor.get");
        assert!(result.is_err());
        let msg = format!("{}", result.unwrap_err());
        assert!(msg.contains("missing argument"), "got: {msg}");
    }

    #[test]
    fn assemble_idol_extra_arg() {
        let c = test_assembler();
        let result = c.assemble("idol Sensor.get id=3 extra=4");
        assert!(result.is_err());
        let msg = format!("{}", result.unwrap_err());
        assert!(msg.contains("unexpected argument"), "got: {msg}");
    }

    #[test]
    fn assemble_idol_in_loop() {
        let c = test_assembler();
        let out = c.assemble("repeat 10\n  idol Sensor.get id=3\nend").unwrap();
        assert!(out.bundle.fits_in_target());
        assert_eq!(out.bundle.metadata.estimated_results, Some(10));
    }
}
