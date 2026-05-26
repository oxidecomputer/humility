// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
use anyhow::{Result, anyhow, bail};
use indicatif::{ProgressBar, ProgressStyle};

use humility::core::Core;
use humility::hubris::*;
use humility_hiffy::{HiffyContext, HiffyError};
use humility_idol::{HubrisIdol, IdolArgument};
use std::time::Duration;

const DEFAULT_SLOT_SIZE_BYTES: usize = 2 * 1024 * 1024;
const READ_CHUNK_SIZE: usize = 256; // limited by HIFFY_SCRATCH_SIZE

/// Handle to interact with auxiliary flash
pub struct AuxFlashHandler<'a> {
    hubris: &'a HubrisArchive,
    core: &'a mut dyn Core,
    context: HiffyContext<'a>,
}

impl<'a> AuxFlashHandler<'a> {
    /// Builds a new [`AuxFlashHandler`]
    pub fn new(
        hubris: &'a HubrisArchive,
        core: &'a mut dyn Core,
        hiffy_timeout: Duration,
    ) -> Result<Self> {
        let context = HiffyContext::new(hubris, core, hiffy_timeout)?;
        Ok(Self { hubris, core, context })
    }

    /// Returns the auxflash slot size
    pub fn slot_size_bytes(&self) -> Result<usize> {
        self.hubris
            .manifest
            .auxflash
            .as_ref()
            .map(|i| i.slot_size_bytes())
            .unwrap_or(Ok(DEFAULT_SLOT_SIZE_BYTES))
    }

    /// Returns the number of auxflash slots
    pub fn slot_count(&mut self) -> Result<u32> {
        let op = self.hubris.get_idol_command("AuxFlash.slot_count")?;
        let value = humility_hiffy::hiffy_call::<u32>(
            self.core,
            &mut self.context,
            &op,
            &[],
            None,
            None,
        )?;
        Ok(value)
    }

    /// Returns the active slot, or `None` if there is no active slot
    pub fn active_slot(&mut self) -> Result<Option<u32>> {
        let op = self
            .hubris
            .get_idol_command("AuxFlash.scan_and_get_active_slot")?;
        let value = humility_hiffy::hiffy_call::<u32>(
            self.core,
            &mut self.context,
            &op,
            &[],
            None,
            None,
        );
        match value {
            Ok(v) => Ok(Some(v)),
            Err(HiffyError::Hiffy(humility_idol::IdolError::Named(e)))
                if e == "NoActiveSlot" =>
            {
                Ok(None)
            }
            Err(e) => Err(e.into()),
        }
    }

    /// Erases a single auxflash slot
    pub fn slot_erase(&mut self, slot: u32) -> Result<()> {
        let op = self.hubris.get_idol_command("AuxFlash.erase_slot")?;
        humility_hiffy::hiffy_call::<()>(
            self.core,
            &mut self.context,
            &op,
            &[("slot", IdolArgument::Scalar(u64::from(slot)))],
            None,
            None,
        )?;
        Ok(())
    }

    /// Returns the checksum of an auxflash slot
    ///
    /// This is `None` if the checksum is not present (because the slot has been
    /// erased).
    pub fn slot_status(&mut self, slot: u32) -> Result<Option<[u8; 32]>> {
        let op = self.hubris.get_idol_command("AuxFlash.read_slot_chck")?;
        let value = humility_hiffy::hiffy_call::<([u8; 32],)>(
            self.core,
            &mut self.context,
            &op,
            &[("slot", IdolArgument::Scalar(u64::from(slot)))],
            None,
            None,
        );
        match value {
            Ok(v) => Ok(Some(v.0)),
            Err(HiffyError::Hiffy(humility_idol::IdolError::Named(e)))
                if e == "MissingChck" =>
            {
                Ok(None)
            }
            Err(e) => Err(e.into()),
        }
    }

    /// Reads some number of bytes from a particular slot
    ///
    /// If `count` is empty, reads the entire slot
    pub fn auxflash_read(
        &mut self,
        slot: u32,
        count: Option<usize>,
    ) -> Result<Vec<u8>> {
        let op =
            self.hubris.get_idol_command("AuxFlash.read_slot_with_offset")?;
        let slot_size = self.slot_size_bytes()?;

        let mut out = vec![0u8; count.unwrap_or(slot_size)];
        let bar = ProgressBar::new(0);
        bar.set_style(
            ProgressStyle::default_bar().template(
                "humility: reading [{bar:30}] {bytes}/{total_bytes}",
            )?,
        );
        bar.set_length(out.len() as u64);
        for (i, chunk) in out.chunks_mut(READ_CHUNK_SIZE).enumerate() {
            let offset = i * READ_CHUNK_SIZE;
            humility_hiffy::hiffy_call::<()>(
                self.core,
                &mut self.context,
                &op,
                &[
                    ("slot", IdolArgument::Scalar(slot as u64)),
                    ("offset", IdolArgument::Scalar(offset as u64)),
                ],
                None,
                Some(chunk),
            )?;
            bar.set_position(offset as u64);
        }
        bar.set_position(out.len() as u64);
        bar.finish_and_clear();

        Ok(out)
    }

    /// Writes some number of bytes to a particular slot
    ///
    /// If the slot is already programmed with a matching `CHCK` field, then
    /// writing is skipped (unless `force` is set).
    pub fn auxflash_write(
        &mut self,
        slot: u32,
        data: &[u8],
        force: bool,
    ) -> Result<()> {
        // If the input data can be parsed as TLV-C, then check against
        // the slot checksum currently loaded in auxiliary flash, skipping
        // flashing if they match.
        let mut reader = tlvc::TlvcReader::begin(data)
            .map_err(|e| anyhow!("TlvcReader::begin failed: {:?}", e))?;
        let mut chck_data = None;
        loop {
            match reader.next() {
                Ok(Some(chunk)) => {
                    if &chunk.header().tag == b"CHCK" {
                        assert_eq!(chunk.len(), 32);
                        let mut out = [0; 32];
                        chunk.read_exact(0, &mut out).map_err(|e| {
                            anyhow!("Failed to read chunk: {:?}", e)
                        })?;
                        chck_data = Some(out);
                        break;
                    }
                }
                Ok(None) => break,
                Err(e) => {
                    humility::msg!(
                        "Failed to load data as TLV-C ({e:?}); \
                         skipping reflash check",
                    );
                    break;
                }
            }
        }
        if let Some(chck_data) = chck_data
            && let Ok(Some(chck_slot)) = self.slot_status(slot)
            && chck_data == chck_slot
        {
            humility::msg!("Slot {slot} is already programmed with our data",);
            if force {
                humility::msg!("Reprogramming it anyways!");
            } else {
                humility::msg!("Skipping reprogramming.");
                return Ok(());
            }
        }

        // Get the hiffy data size before doing any work, because this could
        // fail (if we're on a backend that doesn't support data transfers)
        let data_size = self.context.data_size()?;

        humility::msg!("erasing slot {slot}");
        self.slot_erase(slot)?;

        let slot_size = self.slot_size_bytes()?;
        if data.len() > slot_size {
            bail!(
                "Data is too large ({} bytes, slot size is {} bytes)",
                data.len(),
                slot_size
            );
        }
        let op =
            self.hubris.get_idol_command("AuxFlash.write_slot_with_offset")?;

        let bar = ProgressBar::new(0);
        bar.set_style(
            ProgressStyle::default_bar().template(
                "humility: writing [{bar:30}] {bytes}/{total_bytes}",
            )?,
        );
        bar.set_length(data.len() as u64);
        for (i, chunk) in data.chunks(data_size).enumerate() {
            let offset = i * data_size;
            humility_hiffy::hiffy_call::<()>(
                self.core,
                &mut self.context,
                &op,
                &[
                    ("slot", IdolArgument::Scalar(slot as u64)),
                    ("offset", IdolArgument::Scalar(offset as u64)),
                ],
                Some(chunk),
                None,
            )?;
            bar.set_position(offset as u64);
        }
        bar.set_position(data.len() as u64);
        bar.finish_and_clear();
        humility::msg!("done");
        Ok(())
    }

    pub fn reset(&mut self) -> Result<()> {
        self.core.reset_with_handoff(self.hubris)
    }

    pub fn auxflash_write_from_archive(
        &mut self,
        slot: u32,
        force: bool,
    ) -> Result<()> {
        let data = self.hubris.read_auxflash_data()?.ok_or_else(|| {
            anyhow!(
                "Could not find auxiliary data in the archive. \
                 Does this Hubris app include auxiliary blobs?"
            )
        })?;
        humility::msg!("Flashing auxiliary data from the Hubris archive");
        self.auxflash_write(slot, &data, force)
    }
}
