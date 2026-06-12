// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! Library to get [caboose](https://hubris.oxide.computer/reference/#caboose)
//! information from an attached system
//!
//! The main entry point is [`read_tlvc_caboose`].
#![warn(missing_docs)]
use anyhow::Result;

use humility::{
    core::Core,
    log::{Logger, info},
};
use humility_doppel::{
    CABOOSE_MAGIC, POSSIBLE_HEADER_MAGIC, POSSIBLE_IMAGE_HEADER_OFFSETS,
};

/// Error returned when trying to read a caboose
#[derive(Debug, thiserror::Error)]
pub enum CabooseError {
    /// Failed when calling [`Core::halt`]
    #[error("could not halt core")]
    CouldNotHalt(#[source] anyhow::Error),

    /// Failed when calling [`Core::read_reg`] on the `PC` register
    #[error("could not read PC")]
    CouldNotReadPc(#[source] anyhow::Error),

    /// Failed when calling [`Core::run`]
    #[error("could not run core")]
    CouldNotRun(#[source] anyhow::Error),

    /// Program counter does not match any of our expected ranges
    #[error("could not find image start point based on program counter {0:#x}")]
    UnknownPc(u32),

    /// Flash read failed
    #[error("flash read failed")]
    FlashReadFailed(#[source] anyhow::Error),

    /// Failed to find [`POSSIBLE_HEADER_MAGIC`] at any expected offset
    #[error("failed to find header magic with base address {0:#x}")]
    MissingMagic(u32),

    /// Could not compute caboose offset
    #[error("could not compute caboose offset; it is probably missing")]
    MissingCaboose,

    /// Found a magic value which is not [`CABOOSE_MAGIC`]
    #[error("bad caboose magic: expected {CABOOSE_MAGIC:#x}, got {actual:#x}")]
    BadCabooseMagic {
        /// Magic value that was found in the system
        actual: u32,
    },

    /// Caboose exceeds [`humility::core::CORE_MAX_READSIZE`]
    #[error(
        "caboose size {caboose_size} exceeds max read size {}",
        humility::core::CORE_MAX_READSIZE
    )]
    GiganticCaboose {
        /// Actual size of the caboose
        caboose_size: usize,
    },

    /// Forwarded error from the TLVC library
    #[error("tlvc error: {0}")]
    TlvcError(String), // tlvc::Error does not implement the `Error` trait
}

/// Reads a TLVC-encoded caboose from an attached system
pub fn read_tlvc_caboose(
    core: &mut humility_probes_core::ProbeCore,
    log: &Logger,
) -> Result<Vec<tlvc_text::Piece>, CabooseError> {
    // We'll get the program counter to estimate where we should try to read the
    // image header, since we don't necessarily have our chip info here.
    core.halt().map_err(CabooseError::CouldNotHalt)?;
    let pc = core
        .read_reg(humility_arch_arm::ARMRegister::PC)
        .map_err(CabooseError::CouldNotReadPc)?;
    core.run().map_err(CabooseError::CouldNotRun)?;

    // Find the flash base by looking at the program counter
    //
    // This assumes that we're executing from flash, which is true for all
    // systems that we care about.
    let known_pc_ranges = [
        0x00000..0x10000,       // LPC55, bootleby
        0x10000..0x50000,       // LPC55, image A
        0x50000..0x90000,       // LPC55, image B,
        0x08000000..0x08100000, // STM32H7
    ];
    let base = known_pc_ranges
        .iter()
        .find(|r| r.contains(&pc))
        .ok_or(CabooseError::UnknownPc(pc))?
        .start;
    info!(log, "found flash base at {base:#x} (pc at {pc:#x})");

    // Find the image header by looking for the appropriate magic word
    let mut found_header = None;
    for header_offset in POSSIBLE_IMAGE_HEADER_OFFSETS {
        let header_magic = core
            .read_word_32(base + header_offset)
            .map_err(CabooseError::FlashReadFailed)?;
        if POSSIBLE_HEADER_MAGIC.contains(&header_magic) {
            found_header = Some(header_offset);
            info!(log, "found image header at {:#x}", base + header_offset);
            break;
        }
    }
    let Some(header_offset) = found_header else {
        return Err(CabooseError::MissingMagic(base));
    };

    // Check that the caboose exists and is valid
    let image_size_addr = base
        .checked_add(header_offset)
        .and_then(|b| b.checked_add(4))
        .ok_or(CabooseError::MissingCaboose)?;
    let image_size = core
        .read_word_32(image_size_addr)
        .map_err(CabooseError::FlashReadFailed)?;

    let caboose_size_addr = base
        .checked_add(image_size)
        .and_then(|b| b.checked_sub(4))
        .ok_or(CabooseError::MissingCaboose)?;
    let caboose_size = core
        .read_word_32(caboose_size_addr)
        .map_err(CabooseError::FlashReadFailed)?;

    let caboose_magic_addr = (base + image_size)
        .checked_sub(caboose_size)
        .ok_or(CabooseError::MissingCaboose)?;
    let caboose_magic = core
        .read_word_32(caboose_magic_addr)
        .map_err(CabooseError::FlashReadFailed)?;
    if caboose_magic != CABOOSE_MAGIC {
        return Err(CabooseError::BadCabooseMagic { actual: caboose_magic });
    }

    // Compute start and end for the raw caboose range (including magic and len)
    let raw_caboose_start = base + image_size - caboose_size;
    let raw_caboose_end = base + image_size;
    info!(
        log,
        "found caboose at {:#x}..{:#x}", raw_caboose_start, raw_caboose_end,
    );

    // The caboose data range skips the magic word and length word
    let caboose_data_range = raw_caboose_start + 4..raw_caboose_end - 4;
    let caboose_data_size = caboose_data_range.len();

    if caboose_data_size > humility::core::CORE_MAX_READSIZE {
        return Err(CabooseError::GiganticCaboose {
            caboose_size: caboose_data_size,
        });
    }

    // Read the whole caboose into memory
    let mut caboose_data = vec![0u8; caboose_data_size];
    core.read_8(caboose_data_range.start, &mut caboose_data)
        .map_err(CabooseError::FlashReadFailed)?;

    let reader = tlvc::TlvcReader::begin(caboose_data.as_slice())
        .map_err(|e| CabooseError::TlvcError(format!("{e:?}")))?;
    Ok(tlvc_text::dump(reader))
}
