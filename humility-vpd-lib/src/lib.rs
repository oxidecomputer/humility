// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! Library to handle VPD programing
//!
//! VPD is short for Vital Product Data and is stored in an EEPROM chip.
//! We expect VPD to be `tlvc` encoded. The VPD can be write multiple
//! times until it is permanently locked.
#![warn(missing_docs)]

use hif::Op;
use humility::core::Core;
use humility::hubris::{HubrisArchive, HubrisI2cDevice};
use humility::log::Logger;
use humility_hiffy::HiffyContext;
use humility_idol::{HubrisIdol, IdolArgument, IdolDecodeError};
use indicatif::{ProgressBar, ProgressStyle};
use std::fs;
use std::path::Path;
use tlvc::TlvcReadError;

/// Errors returned from VPD functions
#[derive(Debug, thiserror::Error)]
pub enum VpdError {
    /// Failure from hiffy
    #[error("hiffy error")]
    Hiffy(#[source] anyhow::Error),
    /// Failure from idol
    #[error("idol error")]
    Idol(#[source] anyhow::Error),
    /// I/O problem
    #[error("I/O error")]
    Io {
        /// raw I/O error
        #[source]
        err: std::io::Error,
    },
    /// TLV reading issue
    #[error("tlvc: {0:?}")]
    Tlvc(TlvcReadError<core::convert::Infallible>),
    /// VPD region was never programmed
    #[error("VPD region is unprogrammed")]
    Unprogrammed,
    /// No VPD entries were in the manifest
    #[error("No VPD entries found")]
    NoVpd,
    /// A VPD Entry must be valid to be locked
    #[error("Errors in VPD entry prevent locking")]
    VpdEntry,
    /// A failure occured during locking
    #[error("lock failure at index {ndx}")]
    LockFailure {
        /// Manifest index that failed
        ndx: usize,
        /// Raw idol error
        #[source]
        err: IdolDecodeError,
    },
    /// The VPD has a minimum write size and the requested data
    /// was too small
    #[error("text size too small for a single write")]
    TextTooSmall,
    /// Failure to write to the VPD
    #[error("failed to write VPD at offset {offset}")]
    WriteFailure {
        /// Manifest index that failed
        offset: usize,
        /// Raw idol error
        #[source]
        err: IdolDecodeError,
    },
    /// Failure to read the VPD
    #[error("failed to read VPD at offset {offset}")]
    ReadFailure {
        /// Manifest index that failed
        offset: usize,
        /// Raw idol error
        #[source]
        err: IdolDecodeError,
    },
}

/// Selection of a single VPD entry
pub enum VpdTarget {
    /// Device index in the hubris manifest
    Device(usize),
}

/// Options for reading VPD data
pub enum VpdData {
    /// Data was not requested to be read
    NotRead,
    /// Result of reading the VPD data. These are the raw bytes
    /// and can be formatted with the `tlvc` crate.
    Data(Result<Vec<u8>, VpdError>),
}

/// Represents a single VPD from the hubris archive
pub struct VpdEntry<'a> {
    /// Index into the vpd devices list. This is expected to be stable
    /// across hubris releases
    pub ndx: usize,
    /// Raw i2c data about the device
    pub device: &'a HubrisI2cDevice,
    /// Whether the `VpdLock` command has been successfully issued to this
    /// device.
    pub locked: Result<bool, IdolDecodeError>,
    /// Reading out the VPD can be very slow and can be omitted
    pub data: VpdData,
}

/// Name we expect to see from hubris
const VPD_EEPROM_NAME: &str = "at24csw080";

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
pub fn vpd_list<'a>(
    hubris: &'a HubrisArchive,
    core: &mut dyn Core,
    timeout: std::time::Duration,
    read_data: bool,
    log: &Logger,
) -> Result<Vec<VpdEntry<'a>>, VpdError> {
    let devices = vpd_devices(hubris).collect::<Vec<_>>();
    let mut context = HiffyContext::new(hubris, core, timeout, log)
        .map_err(VpdError::Hiffy)?;

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
        let locked = locked_op.decode::<bool>(&results[ndx]);

        let target = VpdTarget::Device(ndx);

        let data = if read_data {
            VpdData::Data(vpd_slurp(core, &mut context, hubris, &target))
        } else {
            VpdData::NotRead
        };

        items.push(VpdEntry { ndx, locked, data, device });
    }

    if items.is_empty() { Err(VpdError::NoVpd) } else { Ok(items) }
}

/// Writes the VPD data from the file
pub fn vpd_write(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    target: VpdTarget,
    timeout: std::time::Duration,
    write: &Path,
    log: &Logger,
) -> Result<(), VpdError> {
    vpd_erase_write(hubris, core, target, timeout, Some(write), log)
}

/// Erase the entire VPD (write as `0xff`)
pub fn vpd_erase(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    target: VpdTarget,
    timeout: std::time::Duration,
    log: &Logger,
) -> Result<(), VpdError> {
    vpd_erase_write(hubris, core, target, timeout, None, log)
}

/// Return value from `vpd_lock_all`
pub enum VpdLockStatus {
    /// All entries in the manifest were already locked
    AlreadyLocked,
    /// Total number of entries locked
    Count(usize),
}

/// Permanently lock all VPDs
pub fn vpd_lock_all(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    timeout: std::time::Duration,
    log: &Logger,
) -> Result<VpdLockStatus, VpdError> {
    let devices = vpd_list(hubris, core, timeout, true, log)?;

    let mut locking = vec![];
    for d in devices {
        match (d.locked, d.data) {
            (Ok(true), _) => {
                // Do nothing, this is already locked
            }
            (Ok(false), VpdData::Data(Ok(_))) => {
                locking.push(d.ndx);
            }
            (Err(_), _) | (_, VpdData::Data(Err(_))) => {
                // We can't proceed with locking if there's an error
                // with either the lock status or the data
                return Err(VpdError::VpdEntry);
            }
            (_, VpdData::NotRead) => unreachable!(),
        }
    }

    let mut context = HiffyContext::new(hubris, core, timeout, log)
        .map_err(VpdError::Idol)?;

    let mut ops = vec![];
    let lock_op = hubris
        .get_idol_command("Vpd.permanently_lock")
        .map_err(VpdError::Idol)?;

    if locking.is_empty() {
        return Ok(VpdLockStatus::AlreadyLocked);
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
        lock_op
            .decode::<()>(r)
            .map_err(|err| VpdError::LockFailure { err, ndx: *ndx })?;
    }

    Ok(VpdLockStatus::Count(locking.len()))
}

/// Permanently lock a single VPD
pub fn vpd_lock(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    target: VpdTarget,
    timeout: std::time::Duration,
    log: &Logger,
) -> Result<(), VpdError> {
    let mut context = HiffyContext::new(hubris, core, timeout, log)
        .map_err(VpdError::Hiffy)?;

    let op = hubris
        .get_idol_command("Vpd.permanently_lock")
        .map_err(VpdError::Idol)?;
    // Make sure we can read the VPD
    vpd_slurp(core, &mut context, hubris, &target)?;

    let VpdTarget::Device(index) = target;

    let payload = op
        .payload(&[("index", IdolArgument::Scalar(index as u64))])
        .map_err(VpdError::Idol)?;

    let mut ops = vec![];

    context.idol_call_ops(&op, &payload, &mut ops).map_err(VpdError::Idol)?;
    ops.push(Op::Done);

    let results =
        context.run(core, ops.as_slice(), None).map_err(VpdError::Idol)?;

    op.decode::<()>(&results[0])
        .map_err(|err| VpdError::LockFailure { err, ndx: index })?;

    Ok(())
}

/// Read the raw bytes out of the VPD
pub fn vpd_read(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    target: VpdTarget,
    timeout: std::time::Duration,
    log: &Logger,
) -> Result<Vec<u8>, VpdError> {
    let mut context = HiffyContext::new(hubris, core, timeout, log)
        .map_err(VpdError::Hiffy)?;

    vpd_slurp(core, &mut context, hubris, &target)
}

// Erasing is just writing everything to `0xff`
fn vpd_erase_write(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    target: VpdTarget,
    timeout: std::time::Duration,
    write: Option<&Path>,
    log: &Logger,
) -> Result<(), VpdError> {
    let mut context = HiffyContext::new(hubris, core, timeout, log)
        .map_err(VpdError::Hiffy)?;
    let op = hubris.get_idol_command("Vpd.write").map_err(VpdError::Idol)?;

    let (bytes, erase) = if let Some(filename) = write {
        let file =
            fs::File::open(filename).map_err(|err| VpdError::Io { err })?;

        let p = tlvc_text::load(file).map_err(|err| VpdError::Io { err })?;

        (tlvc_text::pack(&p), false)
    } else {
        // All our EEPROMs are 1KiB so this will erase the whole space
        (vec![0xffu8; 1024], true)
    };

    let VpdTarget::Device(target) = target;

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

    let bar = ProgressBar::new(bytes.len() as u64);

    bar.set_style(
        ProgressStyle::default_bar()
            .template(if erase {
                "humility: erasing VPD [{bar:30}] {bytes}/{total_bytes}"
            } else {
                "humility: writing VPD [{bar:30}] {bytes}/{total_bytes}"
            })
            .unwrap(),
    );

    for chunk in all_ops.chunks(nops) {
        let mut ops = chunk.iter().flatten().copied().collect::<Vec<Op>>();
        ops.push(Op::Done);

        let results =
            context.run(core, ops.as_slice(), None).map_err(VpdError::Idol)?;

        for (o, result) in results.iter().enumerate() {
            op.decode::<()>(result).map_err(|err| VpdError::WriteFailure {
                err,
                offset: offset + o,
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
    target: &VpdTarget,
    offset: usize,
) -> Result<Vec<u8>, VpdError> {
    let VpdTarget::Device(target) = *target;

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

    op.decode::<Vec<u8>>(&results[0])
        .map_err(|err| VpdError::ReadFailure { err, offset })
}

fn vpd_slurp(
    core: &mut dyn Core,
    context: &mut HiffyContext,
    hubris: &HubrisArchive,
    target: &VpdTarget,
) -> Result<Vec<u8>, VpdError> {
    let op = hubris.get_idol_command("Vpd.read").map_err(VpdError::Idol)?;

    // First, read in enough to read just the header.
    let mut vpd = vpd_read_at(core, context, &op, target, 0)?;

    let reader = tlvc::TlvcReader::begin(&vpd[..]).map_err(VpdError::Tlvc)?;

    // If this isn't a header, see if it's all 0xff -- in which case we
    // will suggest that the part is unprogrammed.
    let header = reader.read_header().map_err(|e| {
        match vpd.iter().find(|&b| *b != 0xffu8) {
            Some(_) => VpdError::Tlvc(e),
            None => VpdError::Unprogrammed,
        }
    })?;

    // And now go back and read everything.
    let total = header.total_len_in_bytes();

    while vpd.len() < total {
        vpd.extend(vpd_read_at(core, context, &op, target, vpd.len())?);
    }

    Ok(vpd[..total].to_vec())
}
