// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! Data types for the HIF assembler.
//!
//! These types describe the target configuration, I2C topology, HIF
//! functions, and Idol interfaces.  They are all `Serialize +
//! Deserialize` so they can be used as JSON fixtures and API
//! contracts.

use serde::{Deserialize, Serialize};

/// Maximum number of data bytes in a single HIF i2c_write call.
///
/// This matches the 17-byte write buffer in the hiffy task's
/// `i2c_write` function (16 data bytes + 1 optional register byte).
/// See `task/hiffy/src/stm32h7.rs` line 252.
pub const I2C_WRITE_MAX_DATA: usize = 16;

/// Maximum number of HIF labels (loop nesting depth).
pub const MAX_LABELS: usize = 4;

/// Resolved I2C bus: the numeric values needed by HIF i2c_read/write,
/// plus the devices and muxes known to be on this bus.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResolvedBus {
    pub name: String,
    pub controller: u8,
    pub port_index: u8,
    pub port_name: String,
    /// Devices directly on this bus (no mux).
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub devices: Vec<I2cDeviceInfo>,
    /// Muxes on this bus, each with their own devices per segment.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub muxes: Vec<I2cMuxInfo>,
}

/// A known I2C device on a bus.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct I2cDeviceInfo {
    /// I2C address (7-bit).
    pub address: u8,
    /// Part name (e.g. "tmp117", "tps546b24a", "max5970").
    pub device: String,
    /// Human-readable name if assigned (e.g. "Southeast temperature").
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
    /// Description from the config.
    pub description: String,
    /// Whether this device may be physically absent.
    #[serde(default)]
    pub removable: bool,
    /// Sensor names associated with this device, if any.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub sensors: Vec<SensorInfo>,
}

/// A sensor associated with an I2C device.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SensorInfo {
    pub name: String,
    pub kind: String,
}

/// An I2C mux on a bus.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct I2cMuxInfo {
    /// Mux I2C address.
    pub address: u8,
    /// Devices behind each segment.  Key is segment number.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub segments: Vec<I2cMuxSegment>,
}

/// One segment of a mux, with the devices behind it.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct I2cMuxSegment {
    pub segment: u8,
    pub devices: Vec<I2cDeviceInfo>,
}

/// Information about a HIF function available on the target.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FunctionInfo {
    /// Function name from DWARF (PascalCase, e.g. "I2cRead").
    pub name: String,
    /// Function ID (index in the HIFFY_FUNCTIONS enum).
    pub id: u8,
    /// Number of arguments the function expects.
    pub arg_count: usize,
    /// Argument names and types, in order.  Empty if type info
    /// couldn't be extracted from DWARF.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub args: Vec<FunctionArg>,
    /// Error variant names by numeric code.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub errors: Vec<FunctionError>,
}

/// A named, typed argument to a HIF function.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FunctionArg {
    pub name: String,
    /// Type description (e.g. "u8", "Option<u8>", "Controller").
    pub ty: String,
    /// Size in bytes, if known.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub size: Option<usize>,
}

/// A named error code returned by a HIF function.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FunctionError {
    pub code: u32,
    pub name: String,
}

/// Target buffer sizes extracted from the archive.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BufferSizes {
    /// HIFFY_TEXT: program bytecode buffer (bytes).
    pub text: usize,
    /// HIFFY_DATA: input data buffer for bulk writes (bytes).
    pub data: usize,
    /// HIFFY_RSTACK: return stack for function results (bytes).
    pub rstack: usize,
    /// HIFFY_SCRATCH: workspace for function execution (bytes).
    pub scratch: usize,
}

/// Everything the assembler needs to know about a target.
///
/// This is the intermediate form between a Hubris archive and the
/// assembler.  It can be:
///
/// - Extracted from a `HubrisArchive` via
///   [`TargetConfig::from_archive_file`] or
///   [`TargetConfig::from_archive`].
/// - Constructed directly for unit testing.
/// - Serialized to JSON for caching or inspection.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TargetConfig {
    /// Image ID from the Hubris archive.
    pub image_id: Vec<u8>,
    /// Board name (e.g. "gimlet-c-dev", "cosmo-b-dev").
    pub board: String,
    /// Known I2C buses on this target.
    pub buses: Vec<ResolvedBus>,
    /// HIF functions available in the hiffy task.
    pub functions: Vec<FunctionInfo>,
    /// Target buffer sizes.
    pub buffer_sizes: BufferSizes,
    /// Idol interfaces available on this target.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub idol_interfaces: Vec<IdolInterfaceInfo>,
}

/// An Idol interface exposed by a Hubris task.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IdolInterfaceInfo {
    /// Interface name (e.g. "Sensor", "Thermal", "Validate").
    pub name: String,
    /// Task name that implements this interface (e.g. "sensor", "thermal").
    pub task: String,
    /// Task ID (index in the Hubris task table).
    pub task_id: u32,
    /// Operations, in declaration order.  The operation code is
    /// index + 1 (1-based, per Idol convention).
    pub ops: Vec<IdolOpInfo>,
}

/// A single operation in an Idol interface.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IdolOpInfo {
    /// Operation name (e.g. "get", "set_mode", "validate_i2c").
    pub name: String,
    /// Operation code (1-based index in the interface).
    pub code: u16,
    /// Arguments, in declaration order.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub args: Vec<IdolArgInfo>,
    /// Total argument payload size in bytes.
    #[serde(default)]
    pub args_size: usize,
    /// Reply type name (e.g. "f32", "()", "SensorReading").
    pub reply: String,
    /// Reply payload size in bytes (needed by the Send HIF function).
    #[serde(default)]
    pub reply_size: usize,
    /// Error type name, if the operation can fail (e.g. "SensorError").
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub error: Option<String>,
    /// Encoding used for arguments and replies.
    #[serde(default = "default_encoding")]
    pub encoding: String,
    /// Leases (read/write buffers), if any.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub leases: Vec<IdolLeaseInfo>,
    /// Whether clients should auto-retry on server death.
    #[serde(default)]
    pub idempotent: bool,
}

/// An argument to an Idol operation.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IdolArgInfo {
    /// Argument name.
    pub name: String,
    /// Type name (e.g. "u32", "SensorId", "bool").
    pub ty: String,
}

fn default_encoding() -> String {
    "hubpack".to_string()
}

/// A lease (buffer) parameter for an Idol operation.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IdolLeaseInfo {
    /// Lease name.
    pub name: String,
    /// Type being leased.
    pub ty: String,
    /// Server can read from this lease.
    pub read: bool,
    /// Server can write to this lease.
    pub write: bool,
}
