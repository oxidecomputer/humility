// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::{bail, Result};

use crate::archive::ArchiveCore;
use crate::dump::DumpCore;
use crate::hubris::*;
use humility_arch_arm::ARMRegister;
use std::path::Path;
use std::str;
use std::time::Duration;
use thiserror::Error;

pub trait Core {
    fn info(&self) -> (String, Option<String>);

    fn vid_pid(&self) -> Option<(u16, u16)> {
        None
    }

    fn read_8(&mut self, addr: u32, data: &mut [u8]) -> Result<()>;
    fn read_reg(&mut self, reg: ARMRegister) -> Result<u32>;
    fn write_reg(&mut self, reg: ARMRegister, value: u32) -> Result<()>;
    fn init_swv(&mut self) -> Result<()>;
    fn read_swv(&mut self) -> Result<Vec<u8>>;
    fn write_word_32(&mut self, addr: u32, data: u32) -> Result<()>;
    fn write_8(&mut self, addr: u32, data: &[u8]) -> Result<()>;

    fn halt(&mut self) -> Result<()>;
    fn run(&mut self) -> Result<()>;
    fn step(&mut self) -> Result<()>;
    fn is_dump(&self) -> bool {
        false
    }

    fn is_net(&self) -> bool {
        false
    }

    fn is_archive(&self) -> bool {
        false
    }

    fn set_timeout(&mut self, _timeout: Duration) -> Result<()> {
        Ok(())
    }

    fn read_word_32(&mut self, addr: u32) -> Result<u32> {
        let mut buf = [0; 4];
        self.read_8(addr, &mut buf)?;
        Ok(u32::from_le_bytes(buf))
    }

    fn read_word_64(&mut self, addr: u32) -> Result<u64> {
        let mut buf = [0; 8];
        self.read_8(addr, &mut buf)?;
        Ok(u64::from_le_bytes(buf))
    }

    ///
    /// Called to load a flash image.
    ///
    fn load(&mut self, path: &Path) -> Result<()>;

    /// Reset the chip
    fn reset(&mut self) -> Result<()>;

    /// Reset the chip and halt afterwards. Requires a timeout to wait for
    /// halt
    fn reset_and_halt(&mut self, dur: std::time::Duration) -> Result<()>;

    /// Called before starting a series of operations.  May halt the target if
    /// the target does not allow operations while not halted.  Should not be
    /// intermixed with [`halt`]/[`run`].
    fn op_start(&mut self) -> Result<()> {
        Ok(())
    }

    /// Called after completing a series of operations.  May run the target if
    /// the target does not allow operations while not halted.  Should not be
    /// intermixed with [`halt`]/[`run`].
    fn op_done(&mut self) -> Result<()> {
        Ok(())
    }

    /// Wait `duration` seconds for the targe to halt.
    fn wait_for_halt(&mut self, dur: std::time::Duration) -> Result<()>;

    /// Send over network, if applicable
    fn send(&self, _buf: &[u8], _agent: NetAgent) -> Result<usize> {
        bail!("cannot send over network");
    }

    /// Receive from network, if applicable
    fn recv(&self, _buf: &mut [u8], _agent: NetAgent) -> Result<usize> {
        bail!("cannot receive from network");
    }
}

/// Something that you can talk to on the network
///
/// `control-plane-agent` is deliberately skipped, because it's best talked to
/// via `faux-mgs`.
pub enum NetAgent {
    UdpRpc,
    DumpAgent,
}

pub fn attach_dump(
    dump: &str,
    hubris: &HubrisArchive,
) -> Result<Box<dyn Core>> {
    let core = DumpCore::new(dump, hubris)?;
    crate::msg!("attached to dump");
    Ok(Box::new(core))
}

pub fn attach_archive(hubris: &HubrisArchive) -> Result<Box<dyn Core>> {
    let core = ArchiveCore::new(hubris)?;
    crate::msg!("attached to archive");
    Ok(Box::new(core))
}

pub const CORE_MAX_READSIZE: usize = 65536; // 64K ought to be enough for anyone

#[derive(Error, Debug)]
pub enum ProbeError {
    #[error("no debug probe found; is it plugged in?")]
    NoProbeFound,
}
