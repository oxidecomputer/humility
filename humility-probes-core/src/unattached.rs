// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::{bail, Result};
use humility::core::Core;
use humility_arch_arm::ARMRegister;
use std::path::Path;

pub struct UnattachedCore {
    pub probe: probe_rs::Probe,
    pub identifier: String,
    pub vendor_id: u16,
    pub product_id: u16,
    pub serial_number: Option<String>,
}

impl UnattachedCore {
    pub(crate) fn new(
        probe: probe_rs::Probe,
        identifier: String,
        vendor_id: u16,
        product_id: u16,
        serial_number: Option<String>,
    ) -> Self {
        Self { probe, identifier, vendor_id, product_id, serial_number }
    }
}

impl Core for UnattachedCore {
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

    fn read_8(&mut self, _addr: u32, _data: &mut [u8]) -> Result<()> {
        bail!("Core::read_8 unimplemented when unattached!");
    }

    fn read_reg(&mut self, _reg: ARMRegister) -> Result<u32> {
        bail!("Core::read_reg unimplemented when unattached!");
    }

    fn write_reg(&mut self, _reg: ARMRegister, _value: u32) -> Result<()> {
        bail!("Core::write_reg unimplemented when unattached!");
    }

    fn write_word_32(&mut self, _addr: u32, _data: u32) -> Result<()> {
        bail!("Core::write_word_32 unimplemented when unattached!");
    }

    fn write_8(&mut self, _addr: u32, _data: &[u8]) -> Result<()> {
        bail!("Core::write_8 unimplemented when unattached!");
    }

    fn halt(&mut self) -> Result<()> {
        bail!("Core::halt unimplemented when unattached!");
    }

    fn run(&mut self) -> Result<()> {
        bail!("Core::run unimplemented when unattached!");
    }

    fn step(&mut self) -> Result<()> {
        bail!("Core::step unimplemented when unattached!");
    }

    fn load(&mut self, _path: &Path) -> Result<()> {
        bail!("Core::load unimplemented when unattached!");
    }

    fn reset(&mut self) -> Result<()> {
        self.probe.target_reset_assert()?;

        // The closest available documentation on hold time is
        // a comment giving a timeout
        // https://open-cmsis-pack.github.io/Open-CMSIS-Pack-Spec/main/html/debug_description.html#resetHardwareDeassert
        std::thread::sleep(std::time::Duration::from_millis(1000));

        self.probe.target_reset_deassert()?;

        Ok(())
    }

    fn reset_and_halt(&mut self, _dur: std::time::Duration) -> Result<()> {
        bail!("Can't reset and halt for an unattached chip");
    }

    fn wait_for_halt(&mut self, _dur: std::time::Duration) -> Result<()> {
        bail!("Waiting and halting on an unattched chip isn't available!");
    }
}
