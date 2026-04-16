// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::{Context, Result, bail};
use humility::core::Core;
use humility_arch_arm::ARMRegister;
use std::collections::{BTreeMap, HashMap};
use std::path::Path;

use probe_rs::MemoryInterface;
use probe_rs::flashing;

use indicatif::{MultiProgress, ProgressBar, ProgressStyle};
use probe_rs::flashing::Format;
use probe_rs::flashing::ProgressEvent;
use std::time::Duration;

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

// XXX borrowed???
//
//

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum Operation {
    /// Reading back flash contents to restore erased regions that should be kept unchanged.
    Fill,

    /// Erasing flash sectors.
    Erase,

    /// Writing data to flash.
    Program,

    /// Checking flash contents.
    Verify,
}

impl From<flashing::ProgressOperation> for Operation {
    fn from(operation: flashing::ProgressOperation) -> Self {
        match operation {
            flashing::ProgressOperation::Fill => Operation::Fill,
            flashing::ProgressOperation::Erase => Operation::Erase,
            flashing::ProgressOperation::Program => Operation::Program,
            flashing::ProgressOperation::Verify => Operation::Verify,
        }
    }
}

#[derive(Default)]
pub struct ProgressBars {
    bars: HashMap<Operation, ProgressBarGroup>,
}

impl ProgressBars {
    fn get_mut(&mut self, operation: Operation) -> &mut ProgressBarGroup {
        self.bars.entry(operation).or_insert_with(|| {
            let message = match operation {
                Operation::Erase => "Erasing",
                Operation::Fill => "Reading flash",
                Operation::Program => "Programming",
                Operation::Verify => "Verifying",
            };
            ProgressBarGroup::new(format!("{message:>13}"))
        })
    }
}

pub struct ProgressBarGroup {
    message: String,
    bars: Vec<ProgressBar>,
    selected: usize,
}

impl ProgressBarGroup {
    pub fn new(message: String) -> Self {
        Self { message, bars: vec![], selected: 0 }
    }

    fn idle(has_length: bool) -> ProgressStyle {
        let template = if has_length {
            "{msg:.green.bold} {spinner} {percent:>3}% [{bar:20}]"
        } else {
            "{msg:.green.bold} {spinner}"
        };
        ProgressStyle::with_template(template)
            .expect("Error in progress bar creation. This is a bug, please report it.")
            .progress_chars("--")
    }

    fn active(has_length: bool) -> ProgressStyle {
        let template = if has_length {
            "{msg:.green.bold} {spinner} {percent:>3}% [{bar:20}] {bytes:>10} @ {bytes_per_sec:>12} (ETA {eta})"
        } else {
            "{msg:.green.bold} {spinner} {elapsed}"
        };
        ProgressStyle::with_template(template)
            .expect("Error in progress bar creation. This is a bug, please report it.")
            .progress_chars("##-")
    }

    fn finished(has_length: bool) -> ProgressStyle {
        let template = if has_length {
            "{msg:.green.bold} {spinner} {percent:>3}% [{bar:20}] {bytes:>10} @ {bytes_per_sec:>12} (took {elapsed})"
        } else {
            "{msg:.green.bold} {spinner} {elapsed}"
        };
        ProgressStyle::with_template(template)
            .expect("Error in progress bar creation. This is a bug, please report it.")
            .progress_chars("##")
    }

    pub fn add(&mut self, bar: ProgressBar) {
        if !self.bars.is_empty() {
            bar.set_message(format!(
                "{} {}",
                self.message,
                self.bars.len() + 1
            ));
        } else {
            bar.set_message(self.message.clone());
        }
        bar.set_style(Self::idle(bar.length().is_some()));
        bar.enable_steady_tick(Duration::from_millis(100));
        bar.reset_elapsed();

        self.bars.push(bar);
    }

    pub fn inc(&mut self, size: u64) {
        if let Some(bar) = self.bars.get(self.selected) {
            bar.set_style(Self::active(bar.length().is_some()));
            bar.inc(size);
        }
    }

    pub fn abandon(&mut self) {
        if let Some(bar) = self.bars.get(self.selected) {
            bar.abandon();
        }
        self.next();
    }

    pub fn finish(&mut self) {
        if let Some(bar) = self.bars.get(self.selected) {
            bar.set_style(Self::finished(bar.length().is_some()));
            if let Some(length) = bar.length() {
                bar.inc(length.saturating_sub(bar.position()));
            }
            bar.finish();
        }
        self.next();
    }
    pub fn next(&mut self) {
        self.selected += 1;
    }

    pub fn mark_start_now(&mut self) {
        if let Some(bar) = self.bars.get(self.selected) {
            bar.set_style(Self::active(bar.length().is_some()));
            bar.reset_elapsed();
            bar.reset_eta();
        }
    }
}

// end borrowed

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
            return core.read_word_32(addr.into()).with_context(|| {
                format!(
                    "failed to perform unhalted word read at address {addr:#x}",
                )
            });
        }

        self.halt_and_read(|core| {
            rval = core.read_word_32(addr.into()).with_context(|| {
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
            return core.read_8(addr.into(), data).with_context(|| {
                format!(
                    "failed to perform unhalted read at address \
                     {addr:#x} for length {}",
                    data.len()
                )
            });
        }

        self.halt_and_read(|core| {
            core.read_8(addr.into(), data).with_context(|| {
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

        Ok(core.read_core_reg(Into::<probe_rs::RegisterId>::into(
            ARMRegister::to_u16(&reg).unwrap(),
        ))?)
    }

    fn write_reg(&mut self, reg: ARMRegister, value: u32) -> Result<()> {
        let mut core = self.session.core(0)?;
        use num_traits::ToPrimitive;

        core.write_core_reg(
            Into::<probe_rs::RegisterId>::into(
                ARMRegister::to_u16(&reg).unwrap(),
            ),
            value,
        )?;

        Ok(())
    }

    fn write_word_32(&mut self, addr: u32, data: u32) -> Result<()> {
        let mut core = self.session.core(0)?;
        core.write_word_32(addr.into(), data)?;
        Ok(())
    }

    fn write_8(&mut self, addr: u32, data: &[u8]) -> Result<()> {
        let mut core = self.session.core(0)?;
        core.write_8(addr.into(), data)?;
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

    fn load(&mut self, path: &Path) -> Result<()> {
        if !self.can_flash {
            bail!("cannot flash without explicitly attaching to flash");
        }

        let multi_progress = MultiProgress::new();
        let mut progress_bars = ProgressBars::default();

        let progress = flashing::FlashProgress::new(move |event| match event {
            ProgressEvent::FlashLayoutReady { .. } => {}

            ProgressEvent::AddProgressBar { operation, total } => {
                let bar = multi_progress.add(if let Some(total) = total {
                    // We were promised a length, but in this implementation it
                    // may come later in the Started message. Set to at least 1
                    // to avoid progress bars starting from 100%
                    ProgressBar::new(total.max(1))
                } else {
                    ProgressBar::no_length()
                });
                progress_bars.get_mut(operation.into()).add(bar);
            }
            ProgressEvent::Started(operation) => {
                progress_bars.get_mut(operation.into()).mark_start_now();
            }
            ProgressEvent::Progress { operation, size, time: _ } => {
                progress_bars.get_mut(operation.into()).inc(size);
            }
            ProgressEvent::Failed(operation) => {
                progress_bars.get_mut(operation.into()).abandon();
            }
            ProgressEvent::Finished(operation) => {
                progress_bars.get_mut(operation.into()).finish();
            }
            ProgressEvent::DiagnosticMessage { message } => {
                println!("{}", message);
            }
        });

        let mut options = flashing::DownloadOptions::default();
        options.progress = progress;

        if let Err(e) = flashing::download_file_with_options(
            &mut self.session,
            path,
            Format::Hex,
            options,
        ) {
            bail!("Flash loading failed {:?}", e);
        };

        Ok(())
    }
    fn reset(&mut self) -> Result<()> {
        let mut core = self.session.core(0)?;
        core.reset()?;
        self.halted = false;
        Ok(())
    }

    fn reset_and_halt(&mut self, dur: std::time::Duration) -> Result<()> {
        let mut core = self.session.core(0)?;
        core.reset_and_halt(dur)?;
        self.halted = true;
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

    fn wait_for_halt(&mut self, dur: std::time::Duration) -> Result<()> {
        if !self.halted {
            let mut core = self.session.core(0)?;
            core.wait_for_core_halted(dur)?;
            self.halted = true;
        }

        Ok(())
    }
}
