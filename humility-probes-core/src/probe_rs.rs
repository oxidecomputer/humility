// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::{Context, Result, bail};
use humility::core::Core;
use humility_arch_arm::ARMRegister;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::rc::Rc;

use probe_rs::MemoryInterface;
use probe_rs::flashing;

pub struct ProbeCore {
    pub session: probe_rs::Session,
    pub identifier: String,
    pub vendor_id: u16,
    pub product_id: u16,
    pub serial_number: Option<String>,
    halted: bool,
    unhalted_read: BTreeMap<u32, u32>,
    can_flash: bool,
}

impl ProbeCore {
    pub(crate) fn new(
        session: probe_rs::Session,
        identifier: String,
        vendor_id: u16,
        product_id: u16,
        serial_number: Option<String>,
        can_flash: bool,
    ) -> Self {
        Self {
            session,
            identifier,
            vendor_id,
            product_id,
            serial_number,
            halted: false,
            unhalted_read: humility_arch_arm::unhalted_read_regions(),
            can_flash,
        }
    }

    fn halt_and_read(
        &mut self,
        mut func: impl FnMut(&mut probe_rs::Core) -> Result<()>,
    ) -> Result<()> {
        let mut core = self.session.core(0)?;

        let halted = if !self.halted && !core.core_halted()? {
            core.halt(std::time::Duration::from_millis(1000))?;
            true
        } else {
            false
        };

        let rval = func(&mut core);

        if halted {
            core.run()?;
        }

        rval
    }
}

pub const CORE_MAX_READSIZE: usize = 65536; // 64K ought to be enough for anyone

#[rustfmt::skip::macros(anyhow, bail)]
impl Core for ProbeCore {
    fn info(&self) -> (String, Option<String>) {
        let ident = format!(
            "{}, VID {:04x}, PID {:04x}",
            self.identifier, self.vendor_id, self.product_id
        );

        (ident, self.serial_number.clone())
    }

    fn vid_pid(&self) -> Option<(u16, u16)> {
        Some((self.vendor_id, self.product_id))
    }

    fn read_word_32(&mut self, addr: u32) -> Result<u32> {
        log::trace!("reading word at {:x}", addr);
        let mut rval = 0;

        if let Some(range) = self.unhalted_read.range(..=addr).next_back()
            && addr + 4 < range.0 + range.1
        {
            let mut core = self.session.core(0)?;
            return core.read_word_32(addr).with_context(|| {
                format!(
                    "failed to perform unhalted word read at address {addr:#x}",
                )
            });
        }

        self.halt_and_read(|core| {
            rval = core.read_word_32(addr).with_context(|| {
                format!(
                    "failed to perform halted word read at address {addr:#x}"
                )
            })?;

            Ok(())
        })?;

        Ok(rval)
    }

    fn read_8(&mut self, addr: u32, data: &mut [u8]) -> Result<()> {
        if data.len() > CORE_MAX_READSIZE {
            bail!("read of {} bytes at 0x{:x} exceeds max of {}",
                data.len(), addr, CORE_MAX_READSIZE);
        }

        if let Some(range) = self.unhalted_read.range(..=addr).next_back()
            && addr + (data.len() as u32) < range.0 + range.1
        {
            let mut core = self.session.core(0)?;
            return core.read_8(addr, data).with_context(|| {
                format!(
                    "failed to perform unhalted read at address \
                     {addr:#x} for length {}",
                    data.len()
                )
            });
        }

        self.halt_and_read(|core| {
            core.read_8(addr, data).with_context(|| {
                format!(
                    "failed to perform halted read at address \
                    {addr:#x} for length {}",
                    data.len()
                )
            })
        })
    }

    fn read_reg(&mut self, reg: ARMRegister) -> Result<u32> {
        let mut core = self.session.core(0)?;
        use num_traits::ToPrimitive;

        Ok(core.read_core_reg(Into::<probe_rs::CoreRegisterAddress>::into(
            ARMRegister::to_u16(&reg).unwrap(),
        ))?)
    }

    fn write_reg(&mut self, reg: ARMRegister, value: u32) -> Result<()> {
        let mut core = self.session.core(0)?;
        use num_traits::ToPrimitive;

        core.write_core_reg(
            Into::<probe_rs::CoreRegisterAddress>::into(
                ARMRegister::to_u16(&reg).unwrap(),
            ),
            value,
        )?;

        Ok(())
    }

    fn write_word_32(&mut self, addr: u32, data: u32) -> Result<()> {
        let mut core = self.session.core(0)?;
        core.write_word_32(addr, data)?;
        Ok(())
    }

    fn write_8(&mut self, addr: u32, data: &[u8]) -> Result<()> {
        let mut core = self.session.core(0)?;
        core.write_8(addr, data)?;
        Ok(())
    }

    fn halt(&mut self) -> Result<()> {
        if !self.halted {
            let mut core = self.session.core(0)?;
            core.halt(std::time::Duration::from_millis(1000))?;
            self.halted = true;
        }

        Ok(())
    }

    fn run(&mut self) -> Result<()> {
        if self.halted {
            let mut core = self.session.core(0)?;
            core.run()?;
            self.halted = false;
        }

        Ok(())
    }

    fn step(&mut self) -> Result<()> {
        let mut core = self.session.core(0)?;
        core.step()?;
        Ok(())
    }

    fn op_start(&mut self) -> Result<()> {
        self.halt()?;

        Ok(())
    }

    fn op_done(&mut self) -> Result<()> {
        self.run()?;

        Ok(())
    }
}

/// Error returned from [`ProbeCore::load`]
#[derive(Debug, thiserror::Error)]
pub enum LoadError {
    /// Loading is not allowed when `can_flash = false`
    #[error("probe core must be initialized with flashing enabled")]
    NotAllowed,

    /// `probe_rs` internal error when pre-loading file data
    #[error("could not load data")]
    FileDownloadError(#[from] probe_rs::flashing::FileDownloadError),

    /// `probe_rs` internal error when writing to flash
    #[error("could not program flash")]
    FlashError(#[from] probe_rs::flashing::FlashError),
}

impl ProbeCore {
    /// Loads an ELF file onto the target
    pub fn load(&mut self, elf_data: &[u8]) -> Result<(), LoadError> {
        #[derive(Debug, Default)]
        struct LoadProgress {
            /// total bytes that need to be erased
            total_erase: usize,

            /// bytes that have been erased
            erased: usize,

            /// total bytes that need to be written
            total_write: usize,

            /// number of bytes that have been written
            written: usize,
        }

        use indicatif::{ProgressBar, ProgressStyle};

        if !self.can_flash {
            return Err(LoadError::NotAllowed);
        }

        let progress =
            Rc::new(RefCell::new(LoadProgress { ..Default::default() }));

        let bar = ProgressBar::new(0);
        let erase_style = ProgressStyle::default_bar()
            .template("humility: erasing [{bar:30}] {bytes}/{total_bytes}")
            .unwrap();
        let flash_style = ProgressStyle::default_bar()
            .template("humility: flashing [{bar:30}] {bytes}/{total_bytes}")
            .unwrap();
        let progress = flashing::FlashProgress::new(move |event| match event {
            flashing::ProgressEvent::Initialized { flash_layout } => {
                progress.borrow_mut().total_erase = flash_layout
                    .sectors()
                    .iter()
                    .map(|s| s.size() as usize)
                    .sum();

                progress.borrow_mut().total_write = flash_layout
                    .pages()
                    .iter()
                    .map(|s| s.size() as usize)
                    .sum();

                bar.set_style(erase_style.clone());
                bar.set_length(progress.borrow().total_erase as u64);
            }

            flashing::ProgressEvent::SectorErased { size, .. } => {
                progress.borrow_mut().erased += size as usize;
                bar.set_position(progress.borrow().erased as u64);
            }

            flashing::ProgressEvent::PageProgrammed { size, .. } => {
                let mut progress = progress.borrow_mut();

                if progress.written == 0 {
                    progress.erased = progress.total_erase;
                    bar.set_style(flash_style.clone());
                    bar.set_length(progress.total_write as u64);
                }

                progress.written += size as usize;
                bar.set_position(progress.written as u64);
            }

            flashing::ProgressEvent::FinishedProgramming => {
                bar.finish_and_clear();
            }

            _ => {}
        });

        let mut options = flashing::DownloadOptions::default();
        options.progress = Some(&progress);

        let mut loader = self.session.target().flash_loader();
        loader.load_elf_data(&mut std::io::Cursor::new(elf_data))?;
        loader.commit(&mut self.session, options)?;

        Ok(())
    }

    /// Reset the chip, with special handling for measurement handoff
    ///
    /// If this image uses handoff to send a measurement token between the RoT
    /// and SP, this won't work with a debugger physically attached.  To prevent
    /// the SP from resetting itself, we write a different token which skips
    /// this reboot loop.  The memory address and token values are pulled from
    /// the `measurement-token` crate in `lpc55_support`, which is also used in
    /// the SP firmware.
    pub fn reset_with_handoff(
        &mut self,
        hubris: &humility::hubris::HubrisArchive,
    ) -> Result<()> {
        if hubris.wants_reset_handoff_token() {
            self.reset_and_halt(std::time::Duration::from_millis(25))?;
            crate::msg!("skipping measurement token handoff");
            self.write_word_32(
                measurement_token::SP_ADDR as u32,
                measurement_token::SKIP,
            )?;
            self.run()
        } else {
            self.reset()
        }
    }

    pub fn reset(&mut self) -> Result<()> {
        let mut core = self.session.core(0)?;
        core.reset()?;
        self.halted = false;
        Ok(())
    }

    pub fn reset_and_halt(&mut self, dur: std::time::Duration) -> Result<()> {
        let mut core = self.session.core(0)?;
        core.reset_and_halt(dur)?;
        self.halted = true;
        Ok(())
    }
}
