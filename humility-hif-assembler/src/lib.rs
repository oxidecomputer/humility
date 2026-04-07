// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! # humility-hif-assembler
//!
//! Assemble HIF (Hubris Interchange Format) programs from a text-based
//! language into bytecode that can be executed by the `hiffy` task on a
//! Hubris target.
//!
//! ## Overview
//!
//! A HIF program describes operations to run on a Hubris SP or RoT.
//! The source text is written in terms of bus names, device addresses,
//! and Idol interface calls.  The assembler resolves these symbolic
//! references against a Hubris archive (`.zip`), producing bytecode
//! bound to that specific image.
//!
//! The archive supplies:
//!
//! - **Image ID** — embedded in the assembled bundle so the runner can
//!   reject mismatched uploads.
//! - **HIF function table** — maps names like `i2c_read` to numeric IDs
//!   (extracted from DWARF debug info in the `hiffy` task).
//! - **I2C topology** — bus names, controller indices, port indices, mux
//!   configurations, and known device addresses.
//! - **Idol interfaces** — task IDs, operation codes, argument encoding,
//!   and reply sizes for RPC calls.
//! - **Buffer sizes** — `HIFFY_TEXT`, `HIFFY_RSTACK`, etc. so the
//!   assembler can reject programs that won't fit.
//!
//! ## Text Format
//!
//! Programs use a line-oriented syntax with comments (`#`), constants
//! (`.let`), and sugar for common operations.
//!
//! ### I2C Operations
//!
//! ```text
//! # Read 2 bytes from register 0x00 of device 0x48 on the "mid" bus
//! i2c_read mid 0x48 reg=0x00 2
//!
//! # Read through a mux: bus, address, mux=<mux_addr>.<segment>, reg, nbytes
//! i2c_read front 0x50 mux=0x70.1 reg=0x00 16
//!
//! # Write bytes to a register
//! i2c_write mid 0x48 reg=0x01 0x00,0x80
//!
//! # Scan all addresses on a bus (like `humility i2c --scan`)
//! i2c_scan mid
//!
//! # Scan all registers of a device (like `humility i2c -s <addr>`)
//! i2c_regscan mid 0x48
//! ```
//!
//! Bus names (e.g. `mid`, `front`, `rear`, `m2`) come from the I2C
//! bus definitions in the archive's `app.toml`.  You can also use
//! explicit `<controller>.<port>` syntax (e.g. `3.H`).
//!
//! ### Idol RPC Calls
//!
//! ```text
//! # Call an Idol interface operation
//! idol Sensor.get id=3
//! idol Thermal.get_mode
//! idol Validate.validate_i2c index=0
//! ```
//!
//! The assembler resolves the interface and operation names from the
//! `.idolatry` sections in the archive, encodes arguments, and emits
//! the appropriate `Send` call sequence.
//!
//! ### Loops
//!
//! ```text
//! # Repeat a block N times
//! repeat 500
//!     i2c_read mid 0x48 reg=0x00 2
//! end
//!
//! # With a sleep between iterations
//! repeat 100 sleep=10ms
//!     i2c_read mid 0x48 reg=0x00 2
//!     i2c_read mid 0x49 reg=0x00 2
//! end
//! ```
//!
//! Loops assemble to `Label`/`BranchGreaterThan` pairs, consuming one
//! of the four available HIF labels per nesting level.
//!
//! ### Constants
//!
//! ```text
//! .let TEMP_REG 0x00
//! .let ITERATIONS 1000
//!
//! repeat $ITERATIONS
//!     i2c_read mid 0x48 reg=$TEMP_REG 2
//! end
//! ```
//!
//! ### Sleep
//!
//! ```text
//! sleep 50ms     # pause for 50 milliseconds (max 100ms per call)
//! ```
//!
//! ### Raw Ops
//!
//! For anything the sugar doesn't cover, raw HIF instructions are
//! available:
//!
//! ```text
//! raw {
//!     push 0x48
//!     push_none
//!     push 2
//!     call i2c_read
//!     drop_n 7
//!     done
//! }
//! ```
//!
//! ## Compilation
//!
//! ```rust,ignore
//! let asm = HifAssembler::from_archive(&archive)?;
//!
//! // Verify a program without producing binary output
//! let report = asm.verify("repeat 100\n  i2c_read mid 0x48 reg=0x00 2\nend")?;
//! println!("{}", report);
//!
//! // Assemble to a bundle
//! let bundle = asm.assemble("i2c_read mid 0x48 reg=0x00 2")?;
//! bundle.write_to_file("program.hifb")?;
//! ```
//!
//! ## Building Programs Programmatically
//!
//! When generating programs from a test harness (e.g. PRNG-driven
//! fuzzing), use [`ProgramBuilder`] instead of text:
//!
//! ```rust,ignore
//! // ProgramBuilder API -- not yet implemented
//! let asm = HifAssembler::from_archive(&archive)?;
//! let mut prog = asm.builder();
//!
//! prog.repeat(500, |body| {
//!     body.i2c_read("mid", 0x48, Some(0x00), 2)?;
//!     Ok(())
//! })?;
//!
//! let bundle = prog.assemble()?;
//! ```
//!
//! ## Verify Mode
//!
//! [`HifAssembler::verify`] assembles the program and returns a
//! [`VerifyReport`] without producing binary output.  The report
//! includes:
//!
//! - Whether the program text fits in `HIFFY_TEXT`
//! - Estimated result count and whether it fits in `HIFFY_RSTACK`
//! - Functions and buses referenced
//! - Warnings (e.g. addressing a device not in the archive config)
//! - Errors (unknown bus, unknown function, label overflow, etc.)
//!
//! ## Relationship to humility-hiffy
//!
//! This crate intentionally overlaps with parts of `humility-hiffy`
//! (DWARF function discovery, result decoding, I2C parameter
//! resolution).  The overlap exists because `humility-hiffy` is
//! coupled to a live target connection and cannot be used for offline
//! program construction or fixture generation.
//!
//! The types here (`TargetConfig`, `FunctionInfo`, `HifResult`) are
//! designed as the serializable contracts that a future humility
//! library rework (RFD 659) can adopt.  See `README.md` for the full
//! convergence plan.

pub mod archive;
pub mod assembler;
pub mod bundle;
pub mod error;
pub mod listing;
pub mod lower;
pub mod parser;
pub mod types;

pub use assembler::{AssembleOutput, HifAssembler, VerifyReport};
pub use bundle::{BundleMetadata, HifBundle, HifResult};
pub use error::HifError;
pub use parser::{ParsedProgram, Statement};
pub use types::{
    BufferSizes, FunctionArg, FunctionError, FunctionInfo, I2C_WRITE_MAX_DATA,
    I2cDeviceInfo, I2cMuxInfo, I2cMuxSegment, IdolArgInfo, IdolInterfaceInfo,
    IdolLeaseInfo, IdolOpInfo, MAX_LABELS, ResolvedBus, SensorInfo,
    TargetConfig,
};
