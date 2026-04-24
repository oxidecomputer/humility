// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use humility::hubris::*;
use humility::{
    reflect,
    reflect::{Base, Load, Value},
};
use humility_doppel as doppel;

use anyhow::{Result, bail};

/// Name of the packrat buffer which contains SPD data
static PACKRAT_BUF_NAME: &str = "task_packrat::main::BUFS";

/// Raw data read from a single SPD
pub struct SpdData(Vec<u8>);

impl SpdData {
    /// Builds a new [`SpdData`] object
    pub fn new(data: Vec<u8>) -> Self {
        Self(data)
    }

    /// Returns the inner data buffer
    pub fn get(&self) -> &[u8] {
        &self.0
    }
}

/// SPD array size for Gimlet
pub const GIMLET_SPD_SIZE: usize = spd::ee1004::MAX_SIZE;

/// Reads in-memory SPD data from a system (either live or post-mortem)
///
/// Looks for either a `SPD_DATA` global buffer or a field in the `packrat`
/// buffers.  If neither is available, returns `Ok(None)`; otherwise, returns a
/// `Vec<SpdData>` (one [`SpdData`] per DIMM in the system).
pub fn spd_lookup(
    hubris: &HubrisArchive,
    core: &mut dyn humility::core::Core,
) -> Result<Option<Vec<SpdData>>> {
    if let Ok(variables) = hubris.lookup_variables("SPD_DATA") {
        if variables.len() > 1 {
            bail!("more than one SPD_DATA?");
        }

        let var = variables[0];
        let mut buf: Vec<u8> = vec![0u8; var.size];

        core.halt()?;
        core.read_8(var.addr, &mut buf)?;
        core.run()?;

        if !buf.len().is_multiple_of(GIMLET_SPD_SIZE) {
            bail!(
                "SPD_DATA is {} bytes; expected even multiple \
                 of {GIMLET_SPD_SIZE}",
                buf.len(),
            );
        }
        Ok(Some(
            buf.chunks_exact(GIMLET_SPD_SIZE)
                .map(|chunk| SpdData(chunk.to_vec()))
                .collect(),
        ))
    } else if let Ok(var) = hubris.lookup_qualified_variable(PACKRAT_BUF_NAME) {
        let var_ty = hubris.lookup_type(var.goff)?;
        let mut buf: Vec<u8> = vec![0u8; var.size];

        core.halt()?;
        core.read_8(var.addr, &mut buf)?;
        core.run()?;

        let v = reflect::load_value(hubris, &buf, var_ty, 0)?;
        let as_static_cell = doppel::ClaimOnceCell::from_value(&v)?;
        let Value::Struct(packrat_bufs) = &as_static_cell.cell.value else {
            bail!("expected {PACKRAT_BUF_NAME} to be a struct");
        };
        let Some(Value::Struct(compute_sled_bufs)) = packrat_bufs
            .get("gimlet_bufs")
            .or_else(|| packrat_bufs.get("cosmo_bufs"))
        else {
            bail!("could not find `gimlet_bufs` or `cosmo_bufs`");
        };
        let Some(spd_data) = compute_sled_bufs.get("spd_data") else {
            bail!("could not find `spd_data` in sled-specific packrat bufs");
        };

        // We have multiple versions of SPD data.  In older firmwares (Gimlet
        // only), it's a single `[u8; 8192]` buffer; in newer firmwares, it's a
        // nested struct that contains a `[[u8; DATA_SIZE]; DIMM_COUNT]`.
        let spd_bufs = match spd_data {
            Value::Array(a) => {
                if a.len() % GIMLET_SPD_SIZE != 0 {
                    bail!(
                        "SPD data in {PACKRAT_BUF_NAME} is {} bytes;
                         expected even multiple of {GIMLET_SPD_SIZE}",
                        a.len(),
                    );
                }
                let mut out = Vec::with_capacity(a.len() / GIMLET_SPD_SIZE);
                for vs in a.chunks_exact(GIMLET_SPD_SIZE) {
                    let mut chunk = Vec::with_capacity(GIMLET_SPD_SIZE);
                    for v in vs {
                        let Value::Base(Base::U8(b)) = v else {
                            bail!("expected `u8` array");
                        };
                        chunk.push(*b);
                    }
                    out.push(SpdData(chunk))
                }
                out
            }
            Value::Struct(s) => {
                let Some(Value::Array(a)) = s.get("spd_data") else {
                    bail!("expected `spd_data` to be an array");
                };
                let mut out = Vec::with_capacity(a.len());
                for a in a.iter() {
                    let Value::Array(a) = a else {
                        bail!("expected array-of-arrays");
                    };
                    let mut chunk = Vec::with_capacity(a.len());
                    for v in a.iter() {
                        let Value::Base(Base::U8(b)) = v else {
                            bail!("expected `u8` array");
                        };
                        chunk.push(*b);
                    }
                    out.push(SpdData(chunk))
                }
                out
            }
            _ => bail!("expected `spd_data` to be an array or struct"),
        };
        Ok(Some(spd_bufs))
    } else {
        Ok(None)
    }
}

/// Returns true if any non-empty SPD devices are present
pub fn spd_any(
    hubris: &HubrisArchive,
    core: &mut dyn humility::core::Core,
) -> Result<bool> {
    match spd_lookup(hubris, core)? {
        Some(spd_data) => Ok(spd_data
            .iter()
            .flat_map(|d| d.0.iter())
            .any(|&datum| datum != 0)),
        None => Ok(false),
    }
}
