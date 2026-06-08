// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! Library to validate Hubris devices over I2C
//!
//! The main entry point is [`validate_all`].
#![warn(missing_docs)]

use humility::{core::Core, hubris::HubrisArchive, log::Logger};
use humility_hiffy::HiffyContext;
use humility_idol::{HubrisIdol, IdolArgument, IdolDecodeError, IdolError};

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
pub enum ValidateFailure {
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
}

/// Validation errors which occur at the Humility level
#[derive(Debug, thiserror::Error)]
pub enum ValidateError {
    /// Failure with an unknown variant name
    #[error(
        "validate failed and error variant has unknown name `{variant_name}`"
    )]
    UnknownFailure {
        /// Name of the error variant
        variant_name: String,
    },

    /// Successful validation with an unknown variant name
    #[error(
        "validate succeeded but ok variant has unknown name `{variant_name}`"
    )]
    UnknownSuccess {
        /// Name of the ok variant
        variant_name: String,
    },

    /// Decoding the error failed
    #[error("could not decode the error")]
    DecodeFailed(#[source] humility_idol::IdolDecodeFailed),

    /// The error was decoded into something other than [`IdolError::Named`]
    #[error("the error was decoded into an unexpected error type")]
    UnexpectedErrorType(#[source] humility_idol::IdolError),
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

    /// An error occurred while performing validation on a specific device
    #[error("validation error")]
    ValidateError {
        /// Index of the device where the error occurred
        device_index: usize,
        /// Validation error which occurred
        #[source]
        err: ValidateError,
    },

    /// Device index was out of range
    #[error("invalid device index {device_index} in a list of {count} devices")]
    InvalidDeviceIndex {
        /// Invalid device index
        device_index: usize,
        /// Number of devices
        count: usize,
    },
}

/// Result of running validation on a particular device
pub enum ValidateResult {
    /// The target reported that validation succeeded
    Success(ValidateSuccess),
    /// The target reported that validation failed
    Failure(ValidateFailure),
}

/// Validates a list of devices by I2C device index
///
/// Indices are relative to the
/// [`i2c_devices`](humility::hubris::HubrisManifest::i2c_devices) field in the
/// manifest.
///
/// Returns a list of one result per item in `devices`
pub fn validate_by_index(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    devices: &[usize],
    timeout: std::time::Duration,
    log: &Logger,
) -> Result<Vec<ValidateResult>, Error> {
    let mut context = HiffyContext::new(hubris, core, timeout, log)
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
    let mut out = Vec::with_capacity(devices.len());
    assert_eq!(
        results.len(),
        devices.len(),
        "result and device count must be equal"
    );
    for (r, ndx) in results.into_iter().zip(devices.iter()) {
        let Some(device) = hubris.manifest.i2c_devices.get(*ndx) else {
            return Err(Error::InvalidDeviceIndex {
                device_index: *ndx,
                count: hubris.manifest.i2c_devices.len(),
            });
        };
        let result = match op.decode::<humility::reflect::Enum>(&r) {
            Ok(val) => match val.disc() {
                "Present" => ValidateResult::Success(ValidateSuccess::Present),
                "Validated" => {
                    ValidateResult::Success(ValidateSuccess::Validated)
                }
                s => {
                    return Err(Error::ValidateError {
                        device_index: *ndx,
                        err: ValidateError::UnknownSuccess {
                            variant_name: s.to_string(),
                        },
                    });
                }
            },
            Err(IdolDecodeError::DecodeFailed(e)) => {
                return Err(Error::ValidateError {
                    device_index: *ndx,
                    err: ValidateError::DecodeFailed(e),
                });
            }
            Err(IdolDecodeError::Idol(e)) => match e {
                IdolError::Named(n) => match n.as_str() {
                    "NotPresent" => {
                        if device.removable {
                            ValidateResult::Success(ValidateSuccess::Removed)
                        } else {
                            ValidateResult::Failure(ValidateFailure::Absent)
                        }
                    }
                    "BadValidation" => {
                        ValidateResult::Failure(ValidateFailure::BadValidation)
                    }
                    "DeviceTimeout" => {
                        ValidateResult::Failure(ValidateFailure::DeviceTimeout)
                    }
                    "DeviceError" => {
                        ValidateResult::Failure(ValidateFailure::DeviceError)
                    }
                    "Unavailable" => {
                        ValidateResult::Failure(ValidateFailure::Unavailable)
                    }
                    n => {
                        return Err(Error::ValidateError {
                            device_index: *ndx,
                            err: ValidateError::UnknownFailure {
                                variant_name: n.to_string(),
                            },
                        });
                    }
                },
                IdolError::ServerDied(..) => {
                    ValidateResult::Failure(ValidateFailure::ServerDied)
                }
                IdolError::ComplexError(..)
                | IdolError::UnknownErrorVariant(..) => {
                    return Err(Error::ValidateError {
                        device_index: *ndx,
                        err: ValidateError::UnexpectedErrorType(e),
                    });
                }
            },
        };
        out.push(result);
    }

    Ok(out)
}

/// Validates all available I2C devices
///
/// Returns a list of per-device results; devices are ordered based on
/// [`hubris.manifest.i2c_devices`](humility::hubris::HubrisManifest::i2c_devices)
pub fn validate_all(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    timeout: std::time::Duration,
    log: &Logger,
) -> Result<Vec<ValidateResult>, Error> {
    let devices = (0..hubris.manifest.i2c_devices.len()).collect::<Vec<_>>();
    validate_by_index(hubris, core, &devices, timeout, log)
}
