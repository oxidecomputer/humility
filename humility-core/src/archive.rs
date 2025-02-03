// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::core::Core;
use crate::hubris::HubrisArchive;
use crate::hubris::HubrisFlashMap;
use anyhow::{anyhow, bail, Result};
use humility_arch_arm::ARMRegister;
use std::path::Path;

pub struct ArchiveCore {
    flash: HubrisFlashMap,
}

impl ArchiveCore {
    pub(crate) fn new(hubris: &HubrisArchive) -> Result<ArchiveCore> {
        Ok(Self { flash: HubrisFlashMap::new(hubris)? })
    }

    fn read(&mut self, addr: u32, data: &mut [u8]) -> Result<()> {
        self.flash.read(addr, data).ok_or_else(|| {
            anyhow!(
                "{addr:#x} is not in flash and therefore can't \
                be read from just an archive; did you mean to plug in a \
                probe or connect via the network?"
            )
        })
    }
}

impl Core for ArchiveCore {
    fn is_archive(&self) -> bool {
        true
    }

    fn info(&self) -> (String, Option<String>) {
        ("archive".to_string(), None)
    }

    fn read_8(&mut self, addr: u32, data: &mut [u8]) -> Result<()> {
        self.read(addr, data)
    }

    fn read_reg(&mut self, reg: ARMRegister) -> Result<u32> {
        bail!("cannot read register {} from an archive", reg);
    }

    fn write_reg(&mut self, reg: ARMRegister, _value: u32) -> Result<()> {
        bail!("cannot write register {} to an archive", reg);
    }

    fn write_word_32(&mut self, _addr: u32, _data: u32) -> Result<()> {
        bail!("cannot write a word to an archive");
    }

    fn write_8(&mut self, _addr: u32, _data: &[u8]) -> Result<()> {
        bail!("cannot write a byte to an archive");
    }

    fn halt(&mut self) -> Result<()> {
        Ok(())
    }

    fn run(&mut self) -> Result<()> {
        Ok(())
    }

    fn step(&mut self) -> Result<()> {
        bail!("can't step an archive");
    }

    fn load(&mut self, _path: &Path) -> Result<()> {
        bail!("cannot load flash to an archive");
    }

    fn reset(&mut self) -> Result<()> {
        bail!("cannot reset an archive");
    }

    fn reset_and_halt(&mut self, _dur: std::time::Duration) -> Result<()> {
        bail!("cannot reset an archive");
    }

    fn wait_for_halt(&mut self, _dur: std::time::Duration) -> Result<()> {
        bail!("cannot wait for halt of an archive");
    }
}
