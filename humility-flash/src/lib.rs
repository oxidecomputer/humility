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
    RunFailed(#[source] anyhow::Error),

    /// Could not call [`Core::halt`]
    #[error("halt failed")]
    HaltFailed(#[source] anyhow::Error),

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
    core.halt().map_err(ImageStateError::HaltFailed)?;

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
        core.run().map_err(ImageStateError::RunFailed)?;

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
                core.halt().map_err(ImageStateError::HaltFailed)?;
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
        core.halt().map_err(ImageStateError::HaltFailed)?;
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

/// Success type returned from [`program_image`]
pub struct ProgramImageSuccess {
    /// Result of programming the auxiliary flash (if relevant)
    pub auxflash: Option<ProgramAuxflashSuccess>,
}

/// Error type returned from [`program_image`]
#[derive(Debug, thiserror::Error)]
pub enum ProgramImageError {
    /// Could not call [`Core::halt`]
    #[error("halt failed")]
    HaltFailed(#[source] anyhow::Error),

    /// Could not call [`ProbeCore::load`]
    #[error("load failed")]
    LoadFailed(#[source] humility_probes_core::LoadError),

    /// Failed to call [`ProbeCore::reset`]
    #[error("reset failed")]
    ResetFailed(#[source] anyhow::Error),

    /// Could not read auxflash data from archive
    #[error("could not read auxflash data from archive")]
    CouldNotReadAuxflashData(#[source] anyhow::Error),

    /// Could not read ELF data using [`HubrisArchive::load_flash_elf`]
    #[error("could not read image ELF data from archive")]
    CouldNotReadElfData(#[source] anyhow::Error),

    /// Error while programming auxflash
    #[error("failed to program auxflash; your system may not be functional")]
    Auxflash(#[from] ProgramAuxflashError),
}

/// Write an image (including auxflash) to a particular probe
pub fn program_image(
    hubris: &HubrisArchive,
    core: &mut ProbeCore,
    reset_delay: Option<std::time::Duration>,
    log: &Logger,
) -> Result<ProgramImageSuccess, ProgramImageError> {
    // The core may already be halted, but this is idempotent
    core.halt().map_err(ProgramImageError::HaltFailed)?;

    //
    // Load the flash image.  If that fails, we're in a world of hurt:  we
    // really don't want to run the core for fear of masking the initial
    // error.  (It will hopefully be pretty clear to the user that a
    // half-flashed part is going to be in an ill-defined state!)
    //
    let elf = hubris
        .load_flash_elf()
        .map_err(ProgramImageError::CouldNotReadElfData)?;
    core.load(&elf).map_err(ProgramImageError::LoadFailed)?;

    //
    // On Gimlet Rev B, the BOOT0 pin is unstrapped -- and during a flash,
    // it seems to float high enough to bounce the part onto the wrong
    // image (that is, the BOOT1 image -- which by default is the ST
    // bootloader).  This seems to only be true when resetting immediately
    // after flashing the part:  if there is a delay on the order of ~35
    // milliseconds or more, the BOOT0 pin is seen as low when the part
    // resets.  Because this delay is (more or less) harmless, we do it on
    // all platforms, and further make it tunable.
    //
    if let Some(d) = reset_delay {
        std::thread::sleep(d);
    }

    // Reset, using the handoff token if present in the archive
    core.reset_with_handoff(hubris, log)
        .map_err(ProgramImageError::ResetFailed)?;

    // At this point, we can attempt to program the auxiliary flash.  This has
    // to happen *after* the image is flashed and the core is reset, because it
    // uses hiffy calls to the `auxflash` task to actually do the programming;
    // because we have no knowledge of the archive previously flashed onto the
    // chip, we couldn't do hiffy calls before flashing.
    //
    // This is called out in RFD 311 as a weakness of our approach!
    let auxflash = hubris
        .read_auxflash_data()
        .map_err(ProgramImageError::CouldNotReadAuxflashData)?
        .map(|auxflash| program_auxflash(hubris, core, &auxflash, log))
        .transpose()
        .map_err(ProgramImageError::Auxflash)?;

    Ok(ProgramImageSuccess { auxflash })
}
