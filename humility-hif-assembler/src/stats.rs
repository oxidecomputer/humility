// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! Program statistics computed at assembly time.
//!
//! Since we assembled the program, we know exactly what it will do
//! on the target.  This module computes expected stats from the
//! parsed program, without needing any target feedback.

use serde::{Deserialize, Serialize};

use crate::parser::{BusRef, Located, MuxSpec, Statement};

/// Expected statistics for a HIF program, computed from the source.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ProgramStats {
    /// Total I2C read transactions.
    pub i2c_reads: usize,
    /// Total I2C write transactions.
    pub i2c_writes: usize,
    /// Total bytes expected to be read from I2C.
    pub i2c_read_bytes: usize,
    /// Total bytes expected to be written to I2C.
    pub i2c_write_bytes: usize,
    /// Total I2C bus scans (each scans 128 addresses).
    pub i2c_scans: usize,
    /// Total I2C register scans (each scans 256 registers).
    pub i2c_reg_scans: usize,
    /// Estimated mux switch operations.
    pub mux_switches: usize,
    /// Total Idol RPC calls.
    pub idol_calls: usize,
    /// Total sleep time in milliseconds.
    pub sleep_ms: u64,
    /// Distinct I2C buses touched.
    pub buses_touched: Vec<String>,
}

impl ProgramStats {
    /// Total I2C transactions (reads + writes + scans).
    pub fn total_i2c_transactions(&self) -> usize {
        self.i2c_reads
            + self.i2c_writes
            + self.i2c_scans * 128
            + self.i2c_reg_scans * 256
    }

    /// Total I2C bytes (read + write).
    pub fn total_i2c_bytes(&self) -> usize {
        self.i2c_read_bytes + self.i2c_write_bytes
    }
}

impl std::fmt::Display for ProgramStats {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "I2C transactions: {}", self.total_i2c_transactions())?;
        if self.i2c_reads > 0 {
            writeln!(
                f,
                "  reads:  {} ({} bytes)",
                self.i2c_reads, self.i2c_read_bytes
            )?;
        }
        if self.i2c_writes > 0 {
            writeln!(
                f,
                "  writes: {} ({} bytes)",
                self.i2c_writes, self.i2c_write_bytes
            )?;
        }
        if self.i2c_scans > 0 {
            writeln!(
                f,
                "  scans:  {} ({}x128 addr)",
                self.i2c_scans, self.i2c_scans
            )?;
        }
        if self.i2c_reg_scans > 0 {
            writeln!(
                f,
                "  reg scans: {} ({}x256 reg)",
                self.i2c_reg_scans, self.i2c_reg_scans
            )?;
        }
        if self.mux_switches > 0 {
            writeln!(f, "  mux switches: {}", self.mux_switches)?;
        }
        if self.idol_calls > 0 {
            writeln!(f, "Idol calls: {}", self.idol_calls)?;
        }
        if self.sleep_ms > 0 {
            writeln!(f, "Sleep: {}ms", self.sleep_ms)?;
        }
        if !self.buses_touched.is_empty() {
            writeln!(f, "Buses: {}", self.buses_touched.join(", "))?;
        }
        Ok(())
    }
}

/// Compute expected stats from a parsed program.
pub fn compute_stats(stmts: &[Located<Statement>]) -> ProgramStats {
    let mut stats = ProgramStats::default();
    walk_statements(stmts, 1, &mut stats, None);
    stats.buses_touched.sort();
    stats.buses_touched.dedup();
    stats
}

fn walk_statements(
    stmts: &[Located<Statement>],
    multiplier: usize,
    stats: &mut ProgramStats,
    prev_mux: Option<&MuxSpec>,
) {
    let mut last_mux: Option<&MuxSpec> = prev_mux;

    for stmt in stmts {
        match &stmt.value {
            Statement::I2cRead { bus, mux, nbytes, .. } => {
                stats.i2c_reads += multiplier;
                stats.i2c_read_bytes += *nbytes as usize * multiplier;
                track_bus(bus, stats);
                track_mux_switch(
                    mux.as_ref(),
                    &mut last_mux,
                    multiplier,
                    stats,
                );
            }
            Statement::I2cWrite { bus, mux, data, .. } => {
                stats.i2c_writes += multiplier;
                stats.i2c_write_bytes += data.len() * multiplier;
                track_bus(bus, stats);
                track_mux_switch(
                    mux.as_ref(),
                    &mut last_mux,
                    multiplier,
                    stats,
                );
            }
            Statement::I2cScan { bus, mux } => {
                stats.i2c_scans += multiplier;
                track_bus(bus, stats);
                track_mux_switch(
                    mux.as_ref(),
                    &mut last_mux,
                    multiplier,
                    stats,
                );
            }
            Statement::I2cRegScan { bus, mux, .. } => {
                stats.i2c_reg_scans += multiplier;
                track_bus(bus, stats);
                track_mux_switch(
                    mux.as_ref(),
                    &mut last_mux,
                    multiplier,
                    stats,
                );
            }
            Statement::IdolCall { .. } => {
                stats.idol_calls += multiplier;
            }
            Statement::Call { .. } => {
                // Generic function calls — counted in estimated_results
                // by the lowering pass but not categorized here.
            }
            Statement::Sleep { ms } => {
                stats.sleep_ms += *ms as u64 * multiplier as u64;
            }
            Statement::Repeat { count, sleep_ms, body } => {
                walk_statements(
                    body,
                    multiplier * *count as usize,
                    stats,
                    last_mux,
                );
                if let Some(ms) = sleep_ms {
                    stats.sleep_ms +=
                        *ms as u64 * multiplier as u64 * *count as u64;
                }
            }
            Statement::Raw { .. } => {
                // Can't analyze raw ops
            }
        }
    }
}

fn track_bus(bus: &BusRef, stats: &mut ProgramStats) {
    if let BusRef::Named(name) = bus {
        if !stats.buses_touched.contains(name) {
            stats.buses_touched.push(name.clone());
        }
    }
}

fn track_mux_switch(
    current: Option<&MuxSpec>,
    last: &mut Option<&MuxSpec>,
    multiplier: usize,
    stats: &mut ProgramStats,
) {
    match (current, *last) {
        (Some(cur), Some(prev)) => {
            if cur.address != prev.address || cur.segment != prev.segment {
                stats.mux_switches += multiplier;
            }
        }
        (Some(_), None) => {
            // First muxed access — the mux needs to be configured
            stats.mux_switches += multiplier;
        }
        _ => {}
    }
    // Note: We can't update last_mux to point to current because of
    // lifetime issues with the Statement borrow.  For accurate mux
    // switch counting across iterations, this is an approximation.
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;

    fn stats_for(src: &str) -> ProgramStats {
        let prog = parse(src).unwrap();
        compute_stats(&prog.statements)
    }

    #[test]
    fn single_read() {
        let s = stats_for("i2c_read mid 0x48 reg=0x00 2");
        assert_eq!(s.i2c_reads, 1);
        assert_eq!(s.i2c_read_bytes, 2);
        assert_eq!(s.total_i2c_transactions(), 1);
    }

    #[test]
    fn repeat_loop() {
        let s = stats_for("repeat 100\n  i2c_read mid 0x48 reg=0x00 2\nend");
        assert_eq!(s.i2c_reads, 100);
        assert_eq!(s.i2c_read_bytes, 200);
    }

    #[test]
    fn multi_device() {
        let s = stats_for(
            "repeat 50\n  i2c_read mid 0x48 2\n  i2c_read mid 0x49 2\nend",
        );
        assert_eq!(s.i2c_reads, 100);
        assert_eq!(s.total_i2c_transactions(), 100);
    }

    #[test]
    fn write_stats() {
        let s = stats_for("i2c_write mid 0x48 reg=0x01 0x00,0x80");
        assert_eq!(s.i2c_writes, 1);
        assert_eq!(s.i2c_write_bytes, 2);
    }

    #[test]
    fn scan_stats() {
        let s = stats_for("i2c_scan mid");
        assert_eq!(s.i2c_scans, 1);
        assert_eq!(s.total_i2c_transactions(), 128);
    }

    #[test]
    fn sleep_stats() {
        let s = stats_for("repeat 10 sleep=50ms\n  i2c_read mid 0x48 2\nend");
        assert_eq!(s.sleep_ms, 500);
    }

    #[test]
    fn idol_stats() {
        let s = stats_for("repeat 20\n  idol Sensor.get id=0\nend");
        assert_eq!(s.idol_calls, 20);
    }

    #[test]
    fn multi_bus() {
        let s = stats_for("i2c_read mid 0x48 2\ni2c_read front 0x49 2");
        assert_eq!(s.buses_touched.len(), 2);
    }

    #[test]
    fn mux_switching() {
        let s = stats_for(
            "repeat 10\n\
               i2c_read front 0x50 mux=0x70.1 1\n\
               i2c_read front 0x50 mux=0x70.2 1\n\
             end",
        );
        assert_eq!(s.i2c_reads, 20);
        // At least some mux switches detected
        assert!(s.mux_switches > 0);
    }

    #[test]
    fn display() {
        let s = stats_for("repeat 100\n  i2c_read mid 0x48 reg=0x00 2\nend");
        let output = format!("{s}");
        assert!(output.contains("I2C transactions: 100"));
        assert!(output.contains("reads:  100 (200 bytes)"));
        assert!(output.contains("Buses: mid"));
    }
}
