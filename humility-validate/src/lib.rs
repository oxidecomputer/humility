// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//! Library to validate Hubris devices over I2C
#![warn(missing_docs)]

use humility::{
    core::Core,
    hubris::{HubrisArchive, HubrisGoff, Tag},
};
use humility_hiffy::HiffyContext;
use humility_idol::{HubrisIdol, IdolArgument, IdolError, IpcError};

/// Successful validation result
pub enum ValidateSuccess {
    /// The device is present (and has no associated validation)
    Present,
    /// The device is present and validated
    Validated,
    /// The device is removable and not present
    Removed,
}

/// Failed validation result
pub enum ValidateFailure<'a> {
    /// The device is not removable and is not present
    Absent,
    /// The device's validation routine failed
    BadValidation,
    /// The device timed out while validating
    DeviceTimeout,
    /// The device encountered another error while validating
    DeviceError,
    /// The device NAKed a register request
    Unavailable,
    /// The I2C server died while trying to communicate with the device
    ServerDied,
    /// Failure with an unknown variant name
    UnknownFailure(&'a str),
    /// Successful validation with an unknown variant name
    UnknownSuccess(&'a str),
    /// Failure with an unknown variant tag
    UnknownFailureTag(u32),
    /// Operation has an failure unknown error type
    UnknownFailureErrorType(IdolError<'a>),
    /// Successful validation with an unknown tag
    UnknownSuccessTag {
        /// Raw data bytes for the success result
        val: Vec<u8>,
        /// Hubris type associated with the raw data
        ty: HubrisGoff,
    },
}

/// Error type for things that go wrong when running validation
///
/// This error type *does not* capture errors reported by the validation process
/// itself, which are represented by [`ValidateFailure`].
#[derive(Debug, thiserror::Error)]
pub enum Error {
    /// Could not build hiffy context
    #[error("could not build hiffy context")]
    CouldNotBuildContext(#[source] anyhow::Error),

    /// Could not find `validate_i2c` operation
    #[error("could not find `validate_i2c` operation")]
    MissingValidateI2cOp(#[source] anyhow::Error),

    /// Could not pack `validate_i2c` payload
    #[error("could not pack `validate_i2c` payload")]
    PayloadPackingFailed(#[source] anyhow::Error),

    /// Could not construct `validate_i2c` call ops
    #[error("could not construct `validate_i2c` call ops")]
    CallOpsFailed(#[source] anyhow::Error),

    /// Could not run HIF program
    #[error("could not run HIF program")]
    RunFailed(#[source] anyhow::Error),

    /// Could not find `Ok` type
    #[error("could not find `Ok` type")]
    MissingOkEnum(#[source] anyhow::Error),
}

/// Result of running validation on a particular device
pub struct ValidateResult<'a> {
    /// Device index in the I2C array
    pub index: usize,
    /// Result of validation
    pub result: Result<ValidateSuccess, ValidateFailure<'a>>,
}

/// Validates a list of devices by I2C device index
pub fn validate_by_index<'a>(
    hubris: &'a HubrisArchive,
    core: &mut dyn Core,
    devices: &[usize],
    timeout: std::time::Duration,
) -> Result<Vec<ValidateResult<'a>>, Error> {
    let mut context = HiffyContext::new(
        hubris,
        core,
        timeout.as_millis().try_into().unwrap(),
    )
    .map_err(Error::CouldNotBuildContext)?;
    let op = hubris
        .get_idol_command("Validate.validate_i2c")
        .map_err(Error::MissingValidateI2cOp)?;
    let mut ops = vec![];
    for dev in devices {
        let payload = op
            .payload(&[("index", IdolArgument::Scalar(*dev as u64))])
            .map_err(Error::PayloadPackingFailed)?;
        context
            .idol_call_ops(&op, &payload, &mut ops)
            .map_err(Error::CallOpsFailed)?;
    }
    ops.push(hif::Op::Done);
    let results =
        context.run(core, ops.as_slice(), None).map_err(Error::RunFailed)?;
    let ok = hubris.lookup_enum(op.ok).map_err(Error::MissingOkEnum)?;
    let mut out = Vec::with_capacity(devices.len());
    for (r, ndx) in results.into_iter().zip(devices.iter()) {
        let device = &hubris.manifest.i2c_devices[*ndx];
        let result = match r {
            Ok(val) => {
                // TODO: assumes discriminant is a u8. Since this is using Hiffy
                // call results instead of looking at a Rust value in memory,
                // it's not clear from context what changes would be required to
                // fix this.
                if let Some(variant) =
                    ok.lookup_variant_by_tag(Tag::from(val[0]))
                {
                    match variant.name.as_str() {
                        "Present" => Ok(ValidateSuccess::Present),
                        "Validated" => Ok(ValidateSuccess::Validated),
                        s => Err(ValidateFailure::UnknownSuccess(s)),
                    }
                } else {
                    Err(ValidateFailure::UnknownSuccessTag { val, ty: op.ok })
                }
            }
            Err(IpcError::Error(e)) => {
                if let IdolError::CLike(err) = op.error {
                    match err.lookup_variant_by_tag(Tag::from(e)) {
                        Some(variant) => match variant.name.as_str() {
                            "NotPresent" => {
                                if device.removable {
                                    Ok(ValidateSuccess::Removed)
                                } else {
                                    Err(ValidateFailure::Absent)
                                }
                            }
                            "BadValidation" => {
                                Err(ValidateFailure::BadValidation)
                            }
                            "DeviceTimeout" => {
                                Err(ValidateFailure::DeviceTimeout)
                            }
                            "DeviceError" => Err(ValidateFailure::DeviceError),
                            "Unavailable" => Err(ValidateFailure::Unavailable),
                            s => Err(ValidateFailure::UnknownFailure(s)),
                        },
                        None => Err(ValidateFailure::UnknownFailureTag(e)),
                    }
                } else {
                    Err(ValidateFailure::UnknownFailureErrorType(op.error))
                }
            }
            Err(IpcError::ServerDied(_)) => Err(ValidateFailure::ServerDied),
        };
        out.push(ValidateResult { index: *ndx, result });
    }

    Ok(out)
}

/// Validates all available I2C devices
pub fn validate_all<'a>(
    hubris: &'a HubrisArchive,
    core: &mut dyn Core,
    timeout: std::time::Duration,
) -> Result<Vec<ValidateResult<'a>>, Error> {
    let devices = (0..hubris.manifest.i2c_devices.len()).collect::<Vec<_>>();
    validate_by_index(hubris, core, &devices, timeout)
}
