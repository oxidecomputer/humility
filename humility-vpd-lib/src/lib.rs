// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use hif::Op;
use humility::core::Core;
use humility::hubris::{HubrisArchive, HubrisI2cDevice};
use humility_hiffy::HiffyContext;
use humility_idol::{HubrisIdol, IdolArgument};
use indicatif::{ProgressBar, ProgressStyle};
use std::fs;
use std::path::PathBuf;
use tlvc::TlvcReadError;

#[derive(Debug, thiserror::Error)]
pub enum VpdError {
    #[error("hiffy error")]
    Hiffy(#[source] anyhow::Error),
    #[error("idol error")]
    Idol(#[source] anyhow::Error),
    #[error("I/O error")]
    Io {
        #[source]
        err: std::io::Error,
    },
    #[error("tlvc: {0:?}")]
    Tlvc(TlvcReadError<core::convert::Infallible>),
    #[error("VPD region is unprogrammed")]
    Unprogrammed,
    #[error("reflect: {0}")]
    Reflect(String),
    #[error("No VPD entries found")]
    NoVpd,
    #[error("Errors in VPD entry prevent locking")]
    VpdEntry,
    #[error("indicatif template")]
    TemplateError(#[source] indicatif::style::TemplateError),
    #[error("lock failure at index {ndx}")]
    LockFailure {
        ndx: usize,
        #[source]
        err: anyhow::Error,
    },
    #[error("text size too small for a single write")]
    TextTooSmall,
    #[error("failed to write VPD at offset {offset}")]
    WriteFailure {
        offset: usize,
        #[source]
        err: anyhow::Error,
    },
    #[error("failed to read VPD at offset {offset}")]
    ReadFailure {
        offset: usize,
        #[source]
        err: anyhow::Error,
    },
}

pub enum VpdTarget {
    Device(usize),
}

/// Represents a single VPD from the hubris archive
pub struct VpdEntry {
    /// Index into the vpd devices list. This is expected to be stable
    /// across hubris releases
    pub ndx: usize,
    /// Raw i2c data about the device
    pub device: HubrisI2cDevice,
    /// Whether the `VpdLock` command has been successfully issued to this
    /// device.
    pub locked: Result<bool, String>,
    /// Result of reading the VPD data. These are the raw bytes and can
    /// be formatted nicely with the `tlvc` crate.
    pub data: Result<Vec<u8>, VpdError>,
}

/// Name we expect to see from hubris
const VPD_EEPROM_NAME: &'static str = "at24csw080";

fn vpd_devices(
    hubris: &HubrisArchive,
) -> impl Iterator<Item = &HubrisI2cDevice> {
    hubris
        .manifest
        .i2c_devices
        .iter()
        .filter(|device| device.device == VPD_EEPROM_NAME)
}

/// List all available VPD devices
pub fn vpd_list(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    timeout: std::time::Duration,
) -> Result<Vec<VpdEntry>, VpdError> {
    let devices = vpd_devices(hubris).collect::<Vec<_>>();
    let mut context =
        HiffyContext::new(hubris, core, timeout).map_err(VpdError::Hiffy)?;

    let locked_op =
        hubris.get_idol_command("Vpd.is_locked").map_err(VpdError::Idol)?;

    let mut ops = vec![];

    for ndx in 0..devices.len() {
        let payload = locked_op
            .payload(&[(
                "index",
                humility_idol::IdolArgument::Scalar(ndx as u64),
            )])
            .map_err(VpdError::Idol)?;

        context
            .idol_call_ops(&locked_op, &payload, &mut ops)
            .map_err(VpdError::Idol)?;
    }

    ops.push(Op::Done);

    let mut items = vec![];

    let results =
        context.run(core, ops.as_slice(), None).map_err(VpdError::Idol)?;

    for (ndx, device) in devices.into_iter().enumerate() {
        let locked = context
            .idol_result::<bool>(&locked_op, &results[ndx])
            .map_err(|e| format!("{e:?}"));

        let mut target = VpdTarget::Device(ndx);

        let data = vpd_slurp(core, &mut context, hubris, &mut target);

        items.push(VpdEntry { ndx, locked, data, device: device.clone() });
    }

    if items.is_empty() { Err(VpdError::NoVpd) } else { Ok(items) }
}

/// Writes the VPD data from the file
pub fn vpd_write(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    target: VpdTarget,
    timeout: std::time::Duration,
    write: PathBuf,
) -> Result<(), VpdError> {
    vpd_erase_write(hubris, core, target, timeout, Some(write))
}

/// Erases 1k of the VPD (write as `0xff`)
pub fn vpd_erase(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    target: VpdTarget,
    timeout: std::time::Duration,
) -> Result<(), VpdError> {
    vpd_erase_write(hubris, core, target, timeout, None)
}

/// Permanently lock all VPDs, returns the total count of devices
/// locked. A return value of 0 indicates that all devices were locked.
pub fn vpd_lock_all(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    timeout: std::time::Duration,
    allow_missing: bool,
) -> Result<usize, VpdError> {
    let devices = vpd_list(hubris, core, timeout)?;

    let mut any_missing = false;
    let mut locking = vec![];
    for d in devices {
        match (d.locked, d.data) {
            (Ok(true), _) => {
                // Do nothing, this is already locked
            }
            (Ok(false), Ok(_)) => {
                locking.push(d.ndx);
            }
            (Err(_), _) | (_, Err(_)) => {
                any_missing = true;
            }
        }
    }

    let mut context =
        HiffyContext::new(hubris, core, timeout).map_err(VpdError::Idol)?;

    let mut ops = vec![];
    let lock_op = hubris
        .get_idol_command("Vpd.permanently_lock")
        .map_err(VpdError::Idol)?;

    if locking.is_empty() {
        return Ok(0);
    }

    if any_missing && !allow_missing {
        return Err(VpdError::VpdEntry);
    }

    for ndx in &locking {
        let payload = lock_op
            .payload(&[("index", IdolArgument::Scalar(*ndx as u64))])
            .map_err(VpdError::Idol)?;

        context
            .idol_call_ops(&lock_op, &payload, &mut ops)
            .map_err(VpdError::Idol)?;
    }

    ops.push(Op::Done);

    let results =
        context.run(core, ops.as_slice(), None).map_err(VpdError::Idol)?;

    for (ndx, r) in locking.iter().zip(results.iter()) {
        context
            .idol_result::<()>(&lock_op, r)
            .map_err(|err| VpdError::LockFailure { err, ndx: *ndx })?;
    }

    Ok(locking.len())
}

/// Permanently lock a single VPD
pub fn vpd_lock(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    mut target: VpdTarget,
    timeout: std::time::Duration,
) -> Result<(), VpdError> {
    let mut context =
        HiffyContext::new(hubris, core, timeout).map_err(VpdError::Hiffy)?;

    let op = hubris
        .get_idol_command("Vpd.permanently_lock")
        .map_err(VpdError::Idol)?;
    // Make sure we can read the VPD
    vpd_slurp(core, &mut context, hubris, &mut target)?;

    let index = match target {
        VpdTarget::Device(index) => index,
    };

    let payload = op
        .payload(&[("index", IdolArgument::Scalar(index as u64))])
        .map_err(VpdError::Idol)?;

    let mut ops = vec![];

    context.idol_call_ops(&op, &payload, &mut ops).map_err(VpdError::Idol)?;
    ops.push(Op::Done);

    let results =
        context.run(core, ops.as_slice(), None).map_err(VpdError::Idol)?;

    context
        .idol_result::<()>(&op, &results[0])
        .map_err(|err| VpdError::LockFailure { err, ndx: index })?;

    Ok(())
}

/// Read the raw bytes out of the VPD
pub fn vpd_read(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    mut target: VpdTarget,
    timeout: std::time::Duration,
) -> Result<Vec<u8>, VpdError> {
    let mut context =
        HiffyContext::new(hubris, core, timeout).map_err(VpdError::Hiffy)?;

    vpd_slurp(core, &mut context, hubris, &mut target)
}

// Erasing is just writing everything to `0xff`
fn vpd_erase_write(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    target: VpdTarget,
    timeout: std::time::Duration,
    write: Option<PathBuf>,
) -> Result<(), VpdError> {
    let mut context =
        HiffyContext::new(hubris, core, timeout).map_err(VpdError::Hiffy)?;
    let op = hubris.get_idol_command("Vpd.write").map_err(VpdError::Idol)?;

    let (bytes, erase) = if let Some(ref filename) = write {
        let file =
            fs::File::open(filename).map_err(|err| VpdError::Io { err })?;

        let p = tlvc_text::load(file).map_err(|err| VpdError::Io { err })?;

        (tlvc_text::pack(&p), true)
    } else {
        // Should probably cite where the 1k comes from
        (vec![0xffu8; 1024], false)
    };

    let target = match target {
        VpdTarget::Device(target) => target,
    };

    let mut all_ops = vec![];

    for (offset, b) in bytes.iter().enumerate() {
        let mut ops = vec![];
        let payload = op
            .payload(&[
                ("index", IdolArgument::Scalar(target as u64)),
                ("offset", IdolArgument::Scalar(offset as u64)),
                ("contents", IdolArgument::Scalar(*b as u64)),
            ])
            .map_err(VpdError::Idol)?;

        context
            .idol_call_ops(&op, &payload, &mut ops)
            .map_err(VpdError::Idol)?;
        all_ops.push(ops);
    }

    let nops = (context.text_size()
        / context.ops_size(&all_ops[0]).map_err(VpdError::Idol)?)
        - 1;

    if nops == 0 {
        return Err(VpdError::TextTooSmall);
    }

    let mut offset = 0;

    // Do we want to make this an argument for optionally not having
    // a progress bar?
    let bar = ProgressBar::new(bytes.len() as u64);

    bar.set_style(
        ProgressStyle::default_bar()
            .template(if erase {
                "humility: erasing VPD [{bar:30}] {bytes}/{total_bytes}"
            } else {
                "humility: writing VPD [{bar:30}] {bytes}/{total_bytes}"
            })
            .map_err(VpdError::TemplateError)?,
    );

    for chunk in all_ops.chunks(nops) {
        let mut ops = chunk.iter().flatten().copied().collect::<Vec<Op>>();
        ops.push(Op::Done);

        let results =
            context.run(core, ops.as_slice(), None).map_err(VpdError::Idol)?;

        for (o, result) in results.iter().enumerate() {
            context.idol_result::<()>(&op, result).map_err(|err| {
                VpdError::WriteFailure { err, offset: offset + o }
            })?;
        }

        offset += results.len();

        bar.set_position(offset as u64);
    }

    bar.finish_and_clear();

    Ok(())
}

fn vpd_read_at(
    core: &mut dyn Core,
    context: &mut HiffyContext,
    op: &humility_idol::IdolOperation,
    target: &mut VpdTarget,
    offset: usize,
) -> Result<Vec<u8>, VpdError> {
    let target = match target {
        VpdTarget::Device(target) => *target,
    };

    let payload = op
        .payload(&[
            ("index", humility_idol::IdolArgument::Scalar(target as u64)),
            ("offset", humility_idol::IdolArgument::Scalar(offset as u64)),
        ])
        .map_err(VpdError::Idol)?;

    let mut ops = vec![];

    context.idol_call_ops(op, &payload, &mut ops).map_err(VpdError::Idol)?;
    ops.push(Op::Done);

    let results =
        context.run(core, ops.as_slice(), None).map_err(VpdError::Hiffy)?;

    context
        .idol_result::<Vec<u8>>(op, &results[0])
        .map_err(|err| VpdError::ReadFailure { err, offset })
}

fn vpd_slurp(
    core: &mut dyn Core,
    context: &mut HiffyContext,
    hubris: &HubrisArchive,
    target: &mut VpdTarget,
) -> Result<Vec<u8>, VpdError> {
    let op = hubris.get_idol_command("Vpd.read").map_err(VpdError::Idol)?;
    //
    // First, read in enough to read just the header.
    //
    let mut vpd = vpd_read_at(core, context, &op, target, 0)?;

    let reader = tlvc::TlvcReader::begin(&vpd[..]).map_err(VpdError::Tlvc)?;

    //
    // If this isn't a header, see if it's all 0xff -- in which case we
    // will suggest that the part is unprogrammed.
    //
    let header = reader.read_header().map_err(|e| {
        match vpd.iter().find(|&b| *b != 0xffu8) {
            Some(_) => VpdError::Tlvc(e),
            None => VpdError::Unprogrammed,
        }
    })?;

    //
    // And now go back and read everything.
    //
    let total = header.total_len_in_bytes();

    while vpd.len() < total {
        vpd.extend(vpd_read_at(core, context, &op, target, vpd.len())?);
    }

    Ok(vpd[..total].to_vec())
}
