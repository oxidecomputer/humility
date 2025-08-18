// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility rebootleby`
//!
//! Recovers an LPC55 target by reflashing the CFPA, CMPA, and bootleby image.
//! This will _not_ work if the target has a locked CMPA or SWD access has
//! been disabled!
//!
//! !!! Using this can be dangerous and should be undertaken with caution

use anyhow::{Context, Result, anyhow, bail};
use clap::{CommandFactory, Parser};
use humility::core::Core;
use humility_arch_arm::ARMRegister;
use humility_cli::{ExecutionContext, Subcommand};
use humility_cmd::{Archive, Attach, Command, CommandKind, Validate};
use std::io::Read;
use std::ops::RangeInclusive;
use std::path::PathBuf;
use std::time::Duration;
use zip::ZipArchive;

#[derive(Parser, Debug)]
#[clap(about = env!("CARGO_PKG_DESCRIPTION"))]
struct Rebootleby {
    /// Path to a Bootleby Bundle file.
    path: PathBuf,
    /// Flag to ensure you _really_ mean to run this command
    #[clap(long)]
    yes_really: bool,
}

fn rebootleby(context: &mut ExecutionContext) -> Result<()> {
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let subargs = Rebootleby::try_parse_from(subargs)?;

    // Load bundle
    let bundle_reader = std::fs::File::open(&subargs.path)
        .with_context(|| format!("loading {}", subargs.path.display()))?;
    let mut zip =
        ZipArchive::new(bundle_reader).context("opening bundle file as ZIP")?;

    let img_bootleby = if zip.file_names().any(|x| x == "bootleby.bin") {
        let mut entry = zip
            .by_name("bootleby.bin")
            .context("can't find bootleby.bin in bundle")?;
        let mut data = vec![];
        entry.read_to_end(&mut data).context("reading bootleby.bin")?;
        data
    } else {
        let mut entry = zip
            .by_name("img/final.bin")
            .context("can't find bootleby.bin or img/final.bin in bundle")?;
        let mut data = vec![];
        entry.read_to_end(&mut data).context("reading bootleby.bin")?;
        data
    };
    let img_cmpa = {
        let mut entry =
            zip.by_name("cmpa.bin").context("can't find cmpa.bin in bundle")?;
        let mut data = vec![];
        entry.read_to_end(&mut data).context("reading cmpa.bin")?;
        data
    };
    let img_cfpa = {
        let mut entry =
            zip.by_name("cfpa.bin").context("can't find cfpa.bin in bundle")?;
        let mut data = vec![];
        entry.read_to_end(&mut data).context("reading cfpa.bin")?;
        data
    };

    let img_cmpa: &[u8; 512] = img_cmpa[..]
        .try_into()
        .map_err(|_| anyhow!("CMPA file is wrong length!"))?;
    let img_cfpa: &[u8; 512] = img_cfpa[..]
        .try_into()
        .map_err(|_| anyhow!("CFPA file is wrong length!"))?;

    humility::msg!("Loaded bundle {}", subargs.path.display());

    if !subargs.yes_really {
        humility::msg!("!!! This is a potentially dangerous operation");
        humility::msg!("!!! Pass --yes-really to be extra cautious");
        bail!("Re-run with appropriate flag");
    }

    let core = &mut **context.core.as_mut().unwrap();
    let mut flash = FlashHack { core: &mut *core };

    humility::msg!("Connected to core.");

    flash.prep()?;

    humility::msg!("Beginning Bootleby image reprogramming.");
    for (i, chunk) in img_bootleby.chunks(512).enumerate() {
        let addr = i as u32 * 512;
        let mut buffer = chunk.to_vec();
        // Pad if required (affects last chunk only)
        buffer.resize(512, 0xFF);

        flash
            .erase_region(addr >> 4..=(addr + 511) >> 4)
            .with_context(|| format!("failed to erase page at {addr:#x}"))?;
        flash
            .write_page(addr >> 4, &buffer.try_into().unwrap())
            .with_context(|| format!("failed to write page at {addr:#x}"))?;
    }
    humility::msg!("Done.");
    humility::msg!("Beginning CFPA sector reprogramming.");
    for addr in [0x9de00, 0x9e000, 0x9e200] {
        humility::msg!("beginning erase at {addr:#x}");
        flash
            .erase_region(addr >> 4..=(addr + 511) >> 4)
            .with_context(|| format!("failed to erase page at {addr:#x}"))?;
        humility::msg!("beginning write at {addr:#x}");
        flash
            .write_page(addr >> 4, img_cfpa)
            .with_context(|| format!("failed to write page at {addr:#x}"))?;
        humility::msg!("done; sleeping");
        std::thread::sleep(std::time::Duration::from_millis(100));
    }
    humility::msg!("Done.");
    humility::msg!("Beginning CMPA reprogramming.");
    {
        let addr = 0x9e400;
        flash
            .erase_region(addr >> 4..=(addr + 511) >> 4)
            .with_context(|| format!("failed to erase page at {addr:#x}"))?;
        flash
            .write_page(addr >> 4, img_cmpa)
            .with_context(|| format!("failed to write page at {addr:#x}"))?;
    }
    humility::msg!("Done.");
    humility::msg!("Good luck.");

    Ok(())
}

pub fn init() -> Command {
    Command {
        app: Rebootleby::command(),
        name: "rebootleby",
        run: rebootleby,
        kind: CommandKind::Attached {
            archive: Archive::Optional,
            attach: Attach::LiveOnly,
            validate: Validate::None,
        },
    }
}

// The undocumented registers. These look remarkably similar to the
// documented ones in the LPC55S3X though!
const CONFIGLOCKOUT: u32 = 0x5000_0FE4;
const FLASHBANKENABLE: u32 = 0x5000_0450;

const LOCKOUTSETTINGS: u32 = 0x0;
const BANKENABLESETTINGS: u32 = 0xaa0;

struct FlashHack<'a> {
    core: &'a mut dyn Core,
}

impl FlashHack<'_> {
    fn prep(&mut self) -> Result<()> {
        // Before we attempt anything let's make sure we stand a chance
        // of succeeding. This will not work if the bank is locked
        let enable = self.peek(FLASHBANKENABLE)?;

        // This is a simplified check
        if enable != 0 && enable != BANKENABLESETTINGS {
            bail!(
                "Flash bank looks to be locked, {:x} this trick won't work!",
                enable
            );
        }

        // We have no guarantee where the PC is, set it somewhere
        // "safe"
        self.core.halt()?;
        // Branch to self instruction
        self.poke(0x1400_0000, 0xe7fee7fe)?;
        self.core.write_reg(ARMRegister::PC, 0x1400_0000)?;

        // We have to clear the super sekrit bits to have this work on
        // the CFPA/CMPA region. No these are not documented!
        self.poke(CONFIGLOCKOUT, LOCKOUTSETTINGS)?;
        self.poke(FLASHBANKENABLE, BANKENABLESETTINGS)?;
        Ok(())
    }

    fn erase_region(&mut self, range: RangeInclusive<u32>) -> Result<()> {
        humility::msg!("erasing {range:#x?}");
        self.clear_status_flags()?;
        self.set_word_range(*range.start(), *range.end())?;
        self.cmd(CMD_ERASE_RANGE)?;

        self.await_done(0b0101)
    }

    fn write_page(
        &mut self,
        word_address: u32,
        contents: &[u8; 512],
    ) -> Result<()> {
        for (i, chunk) in contents.chunks_exact(16).enumerate() {
            log::debug!("writing {word_address:#x?} chunk {i}");
            self.clear_status_flags()?;
            self.set_word_range(i as u32, i as u32)?;
            for (j, word) in chunk.chunks_exact(4).enumerate() {
                self.poke_and_check(
                    DATAW0 + j as u32 * 4,
                    u32::from_le_bytes(word.try_into().unwrap()),
                )?;
            }

            self.cmd(CMD_WRITE)?;
            self.await_done(0b0101)?;
        }

        humility::msg!("programming {word_address:#x?}");
        self.clear_status_flags()?;
        self.set_word_range(word_address, word_address)?;
        self.cmd(CMD_PROGRAM)?;
        self.await_done(0b0101)
    }

    fn await_done(&mut self, mask: u32) -> Result<()> {
        let mask = mask | 4;
        log::debug!("awaiting done code");
        for _retry in 0..100_000 {
            let v = self.peek(INT_STATUS)?;
            if v != 0 {
                if v & mask == 4 {
                    return Ok(());
                }

                bail!("status {v:#x}");
            }
            std::thread::sleep(std::time::Duration::from_millis(10));
        }

        bail!("still waiting for completion after 100 tries...");
    }

    fn clear_status_flags(&mut self) -> Result<()> {
        self.poke(INT_CLR_STATUS, 0xF) // don't and_check this it's
        // write-1-to-clear
    }

    fn set_word_range(&mut self, start: u32, end: u32) -> Result<()> {
        self.poke_and_check(STARTA, start)?;
        self.poke_and_check(STOPA, end)
    }

    fn cmd(&mut self, number: u32) -> Result<()> {
        self.poke(CMD, number) // do not and-check, this is WO
    }

    fn poke_and_check(&mut self, addr: u32, value: u32) -> Result<()> {
        for _ in 0..10 {
            self.poke(addr, value)?;

            let read_back = self.peek(addr)?;
            if read_back == value {
                return Ok(());
            }
            humility::warn!(
                "warning: wrote {value:#x} to address {addr:#x} but read back {read_back:#x}, retrying"
            );
        }
        bail!("having a damn hard time writing address {addr:#x}, halp");
    }

    fn poke(&mut self, addr: u32, value: u32) -> Result<()> {
        loop {
            match self.core.write_word_32(addr, value) {
                Ok(()) => break Ok(()),
                Err(e) => {
                    let text = format!("{e:?}");
                    if text.contains("WAIT") {
                        humility::msg!("Got wait");
                        std::thread::sleep(Duration::from_millis(10));
                        continue;
                    } else {
                        return Err(e);
                    }
                }
            }
        }
    }

    fn peek(&mut self, addr: u32) -> Result<u32> {
        loop {
            match self.core.read_word_32(addr) {
                Ok(v) => break Ok(v),
                Err(e) => {
                    let text = format!("{e:?}");
                    if text.contains("WAIT") {
                        humility::msg!("Got wait");
                        std::thread::sleep(Duration::from_millis(10));
                        continue;
                    } else {
                        return Err(e);
                    }
                }
            }
        }
    }
}

// See Chapter 5 of the LPC55 manual (UM11126) for where
// these magic numbers come from
const CMD_ERASE_RANGE: u32 = 4;
const CMD_WRITE: u32 = 8;
const CMD_PROGRAM: u32 = 12;

const INT_STATUS: u32 = 0x4003_4FE0;
const INT_CLR_STATUS: u32 = 0x4003_4FE8;
const STARTA: u32 = 0x4003_4010;
const STOPA: u32 = 0x4003_4014;
const CMD: u32 = 0x4003_4000;
const DATAW0: u32 = 0x4003_4080;
