// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! Extract a [`TargetConfig`] from a Hubris archive.
//!
//! This module bridges the gap between humility's `HubrisArchive` and
//! the assembler's `TargetConfig`.  It reads:
//!
//! - Image ID from the archive
//! - I2C bus topology from `manifest.i2c_buses`
//! - HIF function table from DWARF debug info (`HIFFY_FUNCTIONS`)
//! - Buffer sizes from HIFFY_TEXT/DATA/RSTACK variable sizes
//!
//! The DWARF walking logic mirrors `HiffyContext::new()` in
//! `humility-hiffy` but does not require a live target connection.

use std::path::Path;

use anyhow::{anyhow, Context, Result};

use humility::hubris::{
    HubrisArchive, HubrisArchiveDoneness, HubrisEncoding, HubrisGoff,
    HubrisSensorDevice, HubrisTask,
};

use crate::types::{
    BufferSizes, FunctionArg, FunctionError, FunctionInfo, I2cDeviceInfo,
    I2cMuxInfo, I2cMuxSegment, IdolArgInfo, IdolInterfaceInfo, IdolLeaseInfo,
    IdolOpInfo, ResolvedBus, SensorInfo, TargetConfig,
};

impl TargetConfig {
    /// Load a [`TargetConfig`] from a Hubris archive file (.zip).
    ///
    /// This reads the archive, parses DWARF debug info, and extracts
    /// everything the assembler needs.  No live target is required.
    ///
    /// ```rust,ignore
    /// let config = TargetConfig::from_archive_file(
    ///     "target/gimlet-c-dev/dist/default/build-gimlet-c-dev-image-default.zip"
    /// )?;
    /// let asm = HifAssembler::new(config);
    /// ```
    pub fn from_archive_file(path: impl AsRef<Path>) -> Result<Self> {
        let path = path.as_ref();
        let path_str =
            path.to_str().ok_or_else(|| anyhow!("non-UTF-8 archive path"))?;

        let mut hubris = HubrisArchive::new()?;
        hubris
            .load(path_str, HubrisArchiveDoneness::Cook)
            .with_context(|| format!("loading archive {}", path.display()))?;

        Self::from_archive(&hubris)
    }

    /// Extract a [`TargetConfig`] from an already-loaded
    /// `HubrisArchive`.
    ///
    /// Validates that the archive contains a usable hiffy task and
    /// reports warnings for missing network features.
    pub fn from_archive(hubris: &HubrisArchive) -> Result<Self> {
        // Validate that the archive has a hiffy task
        let hiffy_task = hubris.lookup_task("hiffy").ok_or_else(|| {
            anyhow!(
                "hiffy task not found in archive; \
                 this image cannot execute HIF programs"
            )
        })?;

        // Check for net feature (needed for NetHiffy over network)
        match hubris.does_task_have_feature(hiffy_task, "net") {
            Ok(true) => {}
            _ => {
                eprintln!(
                    "warning: hiffy task does not have 'net' feature; \
                     network execution requires adding \
                     features = [\"net\", \"vlan\"] and a hiffy \
                     socket to the app.toml (probe execution \
                     still works)"
                );
            }
        }

        let image_id =
            hubris.image_id().map(|id| id.to_vec()).unwrap_or_default();

        let board = hubris
            .manifest
            .board
            .clone()
            .unwrap_or_else(|| "unknown".to_string());

        let buses = extract_i2c_buses(hubris);
        let functions = extract_hiffy_functions(hubris)?;
        let (buffer_sizes, buf_warnings) = extract_buffer_sizes(hubris)?;

        for w in &buf_warnings {
            eprintln!("warning: {w}");
        }

        let idol_interfaces = extract_idol_interfaces(hubris);

        Ok(TargetConfig {
            image_id,
            board,
            buses,
            functions,
            buffer_sizes,
            idol_interfaces,
        })
    }
}

/// Extract I2C bus definitions from the archive manifest, including
/// device topology and mux structure.
fn extract_i2c_buses(hubris: &HubrisArchive) -> Vec<ResolvedBus> {
    use std::collections::BTreeMap;

    // Build bus list from controllers
    let mut buses: Vec<ResolvedBus> = hubris
        .manifest
        .i2c_buses
        .iter()
        .filter_map(|bus| {
            if bus.target {
                return None;
            }
            let name = bus.name.as_ref()?;
            Some(ResolvedBus {
                name: name.clone(),
                controller: bus.controller,
                port_index: bus.port.index,
                port_name: bus.port.name.clone(),
                devices: vec![],
                muxes: vec![],
            })
        })
        .collect();

    // Build a sensor lookup: i2c_device index -> Vec<SensorInfo>
    let mut sensor_map: BTreeMap<usize, Vec<SensorInfo>> = BTreeMap::new();
    for sensor in &hubris.manifest.sensors {
        if let HubrisSensorDevice::I2c(idx) = &sensor.device {
            sensor_map.entry(*idx).or_default().push(SensorInfo {
                name: sensor.name.clone(),
                kind: format!("{:?}", sensor.kind),
            });
        }
    }

    // Populate devices onto their buses
    for (dev_idx, dev) in hubris.manifest.i2c_devices.iter().enumerate() {
        let sensors = sensor_map.remove(&dev_idx).unwrap_or_default();

        let info = I2cDeviceInfo {
            address: dev.address,
            device: dev.device.clone(),
            name: dev.name.clone(),
            description: dev.description.clone(),
            removable: dev.removable,
            sensors,
        };

        // Find the matching bus
        let bus = buses.iter_mut().find(|b| {
            b.controller == dev.controller
                && b.port_name.eq_ignore_ascii_case(&dev.port.name)
        });

        let bus = match bus {
            Some(b) => b,
            None => continue, // device on an unknown/target bus
        };

        match (dev.mux, dev.segment) {
            (Some(mux_addr), Some(segment)) => {
                // Device is behind a mux
                let mux = bus.muxes.iter_mut().find(|m| m.address == mux_addr);
                let mux = match mux {
                    Some(m) => m,
                    None => {
                        bus.muxes.push(I2cMuxInfo {
                            address: mux_addr,
                            segments: vec![],
                        });
                        bus.muxes.last_mut().unwrap()
                    }
                };
                let seg =
                    mux.segments.iter_mut().find(|s| s.segment == segment);
                match seg {
                    Some(s) => s.devices.push(info),
                    None => {
                        mux.segments.push(I2cMuxSegment {
                            segment,
                            devices: vec![info],
                        });
                    }
                }
            }
            _ => {
                // Direct device on the bus
                bus.devices.push(info);
            }
        }
    }

    // Sort mux segments for deterministic output
    for bus in &mut buses {
        for mux in &mut bus.muxes {
            mux.segments.sort_by_key(|s| s.segment);
        }
        bus.muxes.sort_by_key(|m| m.address);
    }

    buses
}

/// Extract HIF function table from DWARF debug info.
///
/// This replicates the DWARF walking logic from `HiffyContext::new()`
/// (humility-hiffy/src/lib.rs lines 271-343) without requiring a live
/// target.
fn extract_hiffy_functions(
    hubris: &HubrisArchive,
) -> Result<Vec<FunctionInfo>> {
    // Look up HIFFY_FUNCTIONS definition.  May be in definitions or
    // variables depending on compiler version (see hubris#2169).
    let goff = hubris
        .lookup_definition("HIFFY_FUNCTIONS")
        .or_else(|_| hubris.lookup_variable("HIFFY_FUNCTIONS").map(|v| &v.goff))
        .copied()
        .context(
            "HIFFY_FUNCTIONS not found in archive; \
             does this image include the hiffy task?",
        )?;

    // Navigate: Option<&[fn]> -> Some variant -> pointer -> enum
    let option_enum =
        hubris.lookup_enum(goff).context("HIFFY_FUNCTIONS is not an enum")?;
    let some_goff = option_enum
        .lookup_variant_byname("Some")?
        .goff
        .ok_or_else(|| anyhow!("HIFFY_FUNCTIONS Some variant has no type"))?;

    let ptr_struct = hubris
        .lookup_struct(some_goff)
        .context("HIFFY_FUNCTIONS Some is not a struct")?;
    let ptr_goff = ptr_struct
        .lookup_member("__0")
        .context("HIFFY_FUNCTIONS Some has no __0 member")?
        .goff;
    let deref_goff = hubris
        .lookup_ptrtype(ptr_goff)
        .context("HIFFY_FUNCTIONS pointer dereference failed")?;
    let functions_enum = hubris
        .lookup_enum(deref_goff)
        .context("HIFFY_FUNCTIONS target is not an enum")?;

    // Iterate variants in program order — the index IS the function ID.
    let mut result = Vec::with_capacity(functions_enum.variants.len());
    for (id, variant) in functions_enum.variants.iter().enumerate() {
        let id = u8::try_from(id).context("more than 255 HIF functions")?;

        let (args, errors) = match variant.goff {
            Some(goff) => extract_function_signature(hubris, goff)?,
            None => (vec![], vec![]),
        };

        result.push(FunctionInfo {
            name: variant.name.to_string(),
            id,
            arg_count: args.len(),
            args,
            errors,
        });
    }

    Ok(result)
}

/// Extract argument and error info for a HIF function from DWARF.
///
/// Each function variant wraps a 2-tuple of (args, error_type).
/// The args member (__0) is either:
/// - A struct (multiple args): extract each member's name and type
/// - A zero-size basetype (no args): empty vec
/// - Anything else (single arg): one unnamed arg
///
/// The error member (__1) is either:
/// - An enum: extract variant names and tag values
/// - A basetype: ignored (no structured errors)
fn extract_function_signature(
    hubris: &HubrisArchive,
    goff: HubrisGoff,
) -> Result<(Vec<FunctionArg>, Vec<FunctionError>)> {
    let sig = hubris
        .lookup_struct(goff)
        .context("function variant is not a struct")?;

    // Extract args from __0
    let args_goff =
        sig.lookup_member("__0").context("function has no __0 member")?.goff;

    let args = if let Ok(args_struct) = hubris.lookup_struct(args_goff) {
        args_struct
            .members
            .iter()
            .map(|m| FunctionArg {
                name: m.name.clone(),
                ty: resolve_type_name(hubris, m.goff),
                size: resolve_type_size(hubris, m.goff),
            })
            .collect()
    } else {
        match hubris.lookup_basetype(args_goff) {
            Ok(basetype) if basetype.size == 0 => vec![],
            _ => vec![FunctionArg {
                name: "arg0".to_string(),
                ty: resolve_type_name(hubris, args_goff),
                size: resolve_type_size(hubris, args_goff),
            }],
        }
    };

    // Extract errors from __1
    let err_goff =
        sig.lookup_member("__1").context("function has no __1 member")?.goff;

    let errors = if let Ok(err_enum) = hubris.lookup_enum(err_goff) {
        err_enum
            .variants
            .iter()
            .filter_map(|v| {
                let tag = v.tag?;
                Some(FunctionError {
                    code: u32::try_from(tag).ok()?,
                    name: v.name.to_string(),
                })
            })
            .collect()
    } else {
        vec![]
    };

    Ok((args, errors))
}

/// Best-effort type name resolution from a DWARF goff.
fn resolve_type_name(hubris: &HubrisArchive, goff: HubrisGoff) -> String {
    if let Ok(s) = hubris.lookup_struct(goff) {
        return s.name.clone();
    }
    if let Ok(e) = hubris.lookup_enum(goff) {
        return e.name.clone();
    }
    if let Ok(b) = hubris.lookup_basetype(goff) {
        return match b.encoding {
            HubrisEncoding::Unsigned => format!("u{}", b.size * 8),
            HubrisEncoding::Signed => format!("i{}", b.size * 8),
            HubrisEncoding::Float => format!("f{}", b.size * 8),
            HubrisEncoding::Bool => "bool".to_string(),
            HubrisEncoding::Unknown => format!("unknown{}", b.size * 8),
        };
    }
    "?".to_string()
}

/// Best-effort type size resolution from a DWARF goff.
fn resolve_type_size(
    hubris: &HubrisArchive,
    goff: HubrisGoff,
) -> Option<usize> {
    if let Ok(s) = hubris.lookup_struct(goff) {
        return Some(s.size);
    }
    if let Ok(e) = hubris.lookup_enum(goff) {
        return Some(e.size);
    }
    if let Ok(b) = hubris.lookup_basetype(goff) {
        return Some(b.size);
    }
    None
}

/// Extract Idol interface definitions from all tasks in the archive.
fn extract_idol_interfaces(hubris: &HubrisArchive) -> Vec<IdolInterfaceInfo> {
    let mut interfaces = vec![];

    for i in 0..hubris.ntasks() {
        let task = HubrisTask::Task(i as u32);
        let module = match hubris.lookup_module(task) {
            Ok(m) => m,
            Err(_) => continue,
        };

        let iface = match &module.iface {
            Some(iface) => iface,
            None => continue,
        };

        let mut ops = vec![];
        for (idx, (op_name, op)) in iface.ops.iter().enumerate() {
            let code = (idx + 1) as u16; // Idol ops are 1-based

            let args: Vec<IdolArgInfo> = op
                .args
                .iter()
                .map(|(name, attr)| IdolArgInfo {
                    name: name.clone(),
                    ty: attr.ty.0.clone(),
                })
                .collect();

            let (reply, error) = match &op.reply {
                idol::syntax::Reply::Result { ok, err } => {
                    let err_name = match err {
                        idol::syntax::Error::CLike(ty) => ty.0.clone(),
                        idol::syntax::Error::Complex(ty) => ty.0.clone(),
                        idol::syntax::Error::ServerDeath => {
                            "ServerDeath".to_string()
                        }
                    };
                    (ok.ty.0.clone(), Some(err_name))
                }
                idol::syntax::Reply::Simple(ty) => (ty.ty.0.clone(), None),
            };

            let leases: Vec<IdolLeaseInfo> = op
                .leases
                .iter()
                .map(|(name, lease)| IdolLeaseInfo {
                    name: name.clone(),
                    ty: lease.ty.0.clone(),
                    read: lease.read,
                    write: lease.write,
                })
                .collect();

            let encoding = format!("{:?}", op.encoding);

            // Compute args_size and reply_size from DWARF.
            let args_size =
                compute_args_size(hubris, module, &iface.name, op_name);
            let ok_type_name = match &op.reply {
                idol::syntax::Reply::Result { ok, .. } => ok.ty.0.as_str(),
                idol::syntax::Reply::Simple(ty) => ty.ty.0.as_str(),
            };
            let reply_size = compute_reply_size(
                hubris,
                module,
                &iface.name,
                op_name,
                ok_type_name,
                &op.encoding,
            );

            ops.push(IdolOpInfo {
                name: op_name.clone(),
                code,
                args,
                args_size,
                reply,
                reply_size,
                error,
                encoding,
                leases,
                idempotent: op.idempotent,
            });
        }

        interfaces.push(IdolInterfaceInfo {
            name: iface.name.clone(),
            task: module.name.clone(),
            task_id: i as u32,
            ops,
        });
    }

    interfaces
}

/// Compute the args struct size for an Idol operation.
///
/// The args struct is named `{Interface}_{Operation}_ARGS` and lives
/// in the implementing task's module.
fn compute_args_size(
    hubris: &HubrisArchive,
    module: &humility::hubris::HubrisModule,
    iface_name: &str,
    op_name: &str,
) -> usize {
    let struct_name = format!("{}_{}_ARGS", iface_name, op_name);
    module
        .lookup_struct_byname(hubris, &struct_name)
        .ok()
        .flatten()
        .map(|s| s.size)
        .unwrap_or(0)
}

/// Compute the reply payload size for an Idol operation.
///
/// Mirrors the lookup chain from `humility-idol::lookup_reply` and
/// `IdolOperation::reply_size()`:
/// 1. Look up the ok type name as basetype, enum, struct
/// 2. Fall back to `{Interface}_{Operation}_REPLY`
/// 3. Compute size based on encoding
fn compute_reply_size(
    hubris: &HubrisArchive,
    module: &humility::hubris::HubrisModule,
    iface_name: &str,
    op_name: &str,
    ok_type_name: &str,
    encoding: &idol::syntax::Encoding,
) -> usize {
    // Find the ok type's goff using the same chain as humility-idol
    let ok_goff = hubris
        .lookup_basetype_byname(ok_type_name)
        .ok()
        .copied()
        .or_else(|| {
            module
                .lookup_enum_byname(hubris, ok_type_name)
                .ok()
                .flatten()
                .map(|e| e.goff)
        })
        .or_else(|| {
            module
                .lookup_struct_byname(hubris, ok_type_name)
                .ok()
                .flatten()
                .map(|s| s.goff)
        })
        .or_else(|| {
            let t = format!("{}_{}_REPLY", iface_name, op_name);
            hubris.lookup_struct_byname(&t).ok().map(|s| s.goff)
        });

    match ok_goff {
        Some(goff) => match encoding {
            idol::syntax::Encoding::Zerocopy => {
                hubris.typesize(goff).unwrap_or(0)
            }
            idol::syntax::Encoding::Ssmarshal
            | idol::syntax::Encoding::Hubpack => {
                hubris.hubpack_serialized_maxsize(goff).unwrap_or(0)
            }
        },
        None => 0,
    }
}

/// Extract HIFFY buffer sizes from variable sizes in DWARF.
///
/// Returns the sizes and a list of warnings for any variables that
/// could not be found (where defaults were used).
fn extract_buffer_sizes(
    hubris: &HubrisArchive,
) -> Result<(BufferSizes, Vec<String>)> {
    let mut warnings = Vec::new();

    let text_size = lookup_var_size(hubris, "HIFFY_TEXT", 2048, &mut warnings);
    let data_size = lookup_var_size(hubris, "HIFFY_DATA", 2048, &mut warnings);
    let rstack_size =
        lookup_var_size(hubris, "HIFFY_RSTACK", 2048, &mut warnings);
    let scratch_size =
        lookup_var_size(hubris, "HIFFY_SCRATCH", 512, &mut warnings);

    Ok((
        BufferSizes {
            text: text_size,
            data: data_size,
            rstack: rstack_size,
            scratch: scratch_size,
        },
        warnings,
    ))
}

fn lookup_var_size(
    hubris: &HubrisArchive,
    name: &str,
    default: usize,
    warnings: &mut Vec<String>,
) -> usize {
    match hubris.lookup_variable(name) {
        Ok(v) => v.size,
        Err(_) => {
            warnings.push(format!(
                "{name} not found in archive, using default {default}"
            ));
            default
        }
    }
}
