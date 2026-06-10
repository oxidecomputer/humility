// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! Functions to check flash state and reprogram a processor
#![warn(missing_docs)]

use humility::{
    core::Core,
    hubris::{HubrisArchive, HubrisValidate},
    log::{Logger, info},
};
use humility_auxflash::{AuxFlashHandler, AuxFlashWriter};
use humility_probes_core::ProbeCore;

/// Result of checking image state
#[must_use]
pub enum ImageStateResult {
    /// Image state check succeeded
    Matches,
    /// Image state does not match
    DoesNotMatch(ImageStateMismatch),
}

/// Error type when checking image state
#[derive(Debug, thiserror::Error)]
pub enum ImageStateError {
    /// Could not read auxflash from archive
    #[error("could not read auxflash from archive")]
    CouldNotReadAuxflash(#[source] anyhow::Error),

    /// Could not call [`Core::run`]
    #[error("run failed")]
    CouldNotRun(#[source] anyhow::Error),

    /// Could not call [`Core::halt`]
    #[error("halt failed")]
    CouldNotHalt(#[source] anyhow::Error),

    /// Could not build an [`AuxFlashHandler`]
    #[error("could not build auxflash handler")]
    CouldNotBuildHandler(#[source] anyhow::Error),

    /// [`AuxFlashHandler::active_slot`] returned an error
    #[error("could not get active slot")]
    CouldNotGetAuxflashSlot(#[source] anyhow::Error),
}

/// Type indicating a mismatch when checking image state
#[derive(Debug, thiserror::Error)]
pub enum ImageStateMismatch {
    /// Failed at the archive ID check
    #[error("flash/archive mismatch")]
    FlashArchiveMismatch(#[source] anyhow::Error),

    /// Call to [`HubrisArchive::verify`] failed
    #[error("flash contents do not match archive contents")]
    VerifyFailed(#[source] anyhow::Error),

    /// The processor does not have an active auxflash slot
    #[error(
        "archive has auxflash data but processor does \
         not have an active auxflash slot"
    )]
    NoActiveAuxflashSlot,
}

/// Type of image checks that can be run
pub enum ImageCheckType {
    /// Check that the image ID matches
    ImageId,
    /// Check that the image ID matches and that flash data matches
    ImageIdAndFlash {
        /// Check every single byte, rather than bailing on the first mismatch
        check_every_byte: bool,
    },
}

/// Checks the image and auxiliary flash against a Hubris archive
///
/// Returns `Ok(r)` if the function succeeded; `r` may indicate that the check
/// itself is either successful or unsuccessful.
///
/// Returns `Err(..)` on a Humility-level error.
///
/// The core is halted when this function exits.
pub fn get_image_state(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    check_type: ImageCheckType,
    log: &Logger,
) -> Result<ImageStateResult, ImageStateError> {
    core.halt().map_err(ImageStateError::CouldNotHalt)?;

    // First pass: check only the image ID
    if let Err(e) = hubris.validate(core, HubrisValidate::ArchiveMatch) {
        return Ok(ImageStateResult::DoesNotMatch(
            ImageStateMismatch::FlashArchiveMismatch(e),
        ));
    }

    // More rigorous checks if requested
    if let ImageCheckType::ImageIdAndFlash { check_every_byte } = check_type
        && let Err(e) = hubris.verify(core, check_every_byte, log)
    {
        return Ok(ImageStateResult::DoesNotMatch(
            ImageStateMismatch::VerifyFailed(e),
        ));
    }

    if hubris
        .read_auxflash_data()
        .map_err(ImageStateError::CouldNotReadAuxflash)?
        .is_some()
    {
        // The core must be running for us to check the auxflash slot.
        //
        // However, we want it halted before we exit this function, which
        // requires careful handling of functions that could bail out.
        core.run().map_err(ImageStateError::CouldNotRun)?;

        // Note that we only run this check if we pass the image ID check;
        // otherwise, the Idol / hiffy memory maps are unknown.
        let mut worker = match AuxFlashHandler::new(
            hubris,
            core,
            std::time::Duration::from_millis(15_000),
            log,
        ) {
            Ok(w) => w,
            Err(e) => {
                // Halt the core before returning!
                core.halt().map_err(ImageStateError::CouldNotHalt)?;
                return Err(ImageStateError::CouldNotBuildHandler(e));
            }
        };
        let r = match worker.active_slot() {
            Ok(Some(s)) => {
                info!(log, "verified auxflash in slot {s}");
                Ok(ImageStateResult::Matches)
            }
            Ok(None) => Ok(ImageStateResult::DoesNotMatch(
                ImageStateMismatch::NoActiveAuxflashSlot,
            )),
            Err(e) => Err(ImageStateError::CouldNotGetAuxflashSlot(e)),
        };

        // Halt the core before returning results
        core.halt().map_err(ImageStateError::CouldNotHalt)?;
        r
    } else {
        Ok(ImageStateResult::Matches)
    }
}

/// Error type when programming auxflash
#[derive(Debug, thiserror::Error)]
pub enum ProgramAuxflashError {
    /// Could not build an [`AuxFlashHandler`]
    #[error("could not build auxflash handler")]
    CouldNotBuildHandler(#[source] anyhow::Error),

    /// [`AuxFlashHandler::active_slot`] returned an error
    #[error("could not get active slot {} programming",
        if *before { "before" } else { "after" })]
    CouldNotGetAuxflashSlot {
        /// Original error
        #[source]
        err: anyhow::Error,
        /// Whether this occurred before or after programming auxflash
        before: bool,
    },

    /// [`AuxFlashHandler::slot_count`] returned an error
    #[error("could not get slot_count")]
    CouldNotGetSlotCount(#[source] anyhow::Error),

    /// [`AuxFlashWriter::auxflash_write`] returned an error
    #[error("could not write auxflash")]
    WriteFailed(#[source] anyhow::Error),

    /// [`ProbeCore::reset`] returned an error
    #[error("could not reset processor")]
    ResetFailed(#[source] anyhow::Error),

    /// No active auxflash slot after programming
    #[error("no active auxflash slot, even after programming")]
    NoActiveSlot,
}

/// Success type for programming auxflash
pub enum ProgramAuxflashSuccess {
    /// The image successfully booted, meaning our data was already programmed
    AlreadyProgrammed {
        /// Which slot the auxflash is using
        slot: u32,
    },
    /// We successfully programmed the auxflash
    Success,
}

/// Writes the given data to an auxflash slot
pub fn program_auxflash(
    hubris: &HubrisArchive,
    core: &mut ProbeCore,
    data: &[u8],
    log: &Logger,
) -> Result<ProgramAuxflashSuccess, ProgramAuxflashError> {
    let mut worker = AuxFlashWriter::new(
        hubris,
        core,
        std::time::Duration::from_millis(15_000),
        log,
    )
    .map_err(ProgramAuxflashError::CouldNotBuildHandler)?;

    // At this point, we've already rebooted into the new image.
    //
    // If the Hubris auxflash task has picked up an active slot, then we're all
    // set: our target image was already loaded (or was unchanged).
    if let Some(i) = worker.active_slot().map_err(|err| {
        ProgramAuxflashError::CouldNotGetAuxflashSlot { err, before: true }
    })? {
        return Ok(ProgramAuxflashSuccess::AlreadyProgrammed { slot: i });
    }

    // Otherwise, we need to pick a slot.  This is tricky, because we don't
    // actually know whether there's an image on the B partition that's using
    // auxiliary data.  We'll prioritize picking an empty (even) slot, and will
    // otherwise pick slot 0 arbitrarily.
    let slot_count = worker
        .slot_count()
        .map_err(ProgramAuxflashError::CouldNotGetSlotCount)?;
    let mut target_slot = 0;
    for i in 0..slot_count {
        if i % 2 == 0 && matches!(worker.slot_status(i), Ok(None)) {
            target_slot = i;
            break;
        }
    }

    worker
        .auxflash_write(target_slot, data, false)
        .map_err(ProgramAuxflashError::WriteFailed)?;

    // After a reset, two things will happen:
    // - The SP `auxflash` task will automatically mirror from the even slot to
    //   the odd slot, since the even slot will have valid data and the odd slot
    //   will not.
    // - The SP will recognize the programmed slot as valid and choose it as the
    //   active slot.
    worker.reset().map_err(ProgramAuxflashError::ResetFailed)?;

    // Give the SP plenty of time to do its mirroring operation
    info!(log, "resetting the SP, please wait...");
    std::thread::sleep(std::time::Duration::from_secs(5));

    match worker.active_slot() {
        Ok(Some(..)) => Ok(ProgramAuxflashSuccess::Success),
        Ok(None) => Err(ProgramAuxflashError::NoActiveSlot),
        Err(err) => Err(ProgramAuxflashError::CouldNotGetAuxflashSlot {
            err,
            before: false,
        }),
    }
}
