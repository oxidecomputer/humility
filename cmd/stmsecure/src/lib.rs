// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility stmsecure`
//!
//! Humility has support to manage the Root Security Services (RSS) and various
//! flash options bits
//!
//! A typical sequence to set the secure region at 0x08000000
//!
//! ```text
//! humility stmsecure set-secure-bit
//! humility stmsecure set-secure-region 0x08000000 0xa000
//! ```
//!
//! To undo the secure region:
//!
//! ```text
//! humility stmsecure unset-secure-region
//! humility stmsecure unset-secure-bit
//! ```
//!
//! The STM32 has support for flash bank swapping as well
//!
//! ```text
//! humility stmsecure bank-swap
//! ```

use anyhow::{anyhow, Result};
use clap::{CommandFactory, Parser};
use humility::arch::ARMRegister;
use humility::cli::Subcommand;
use humility::core::Core;
use humility_cmd::{Archive, Attach, Command, CommandKind, Validate};

const FLASH_OPT_KEY1: u32 = 0x0819_2A3B;
const FLASH_OPT_KEY2: u32 = 0x4C5D_6E7F;

const FLASH_KEY1: u32 = 0x4567_0123;
const FLASH_KEY2: u32 = 0xCDEF_89AB;

const FLASH_KEYR1: u32 = 0x5200_2004;
const FLASH_CR1: u32 = 0x5200_200C;
const FLASH_SR1: u32 = 0x5200_2010;
const FLASH_OPT_KEYR: u32 = 0x5200_2008;
const FLASH_OPT_CR: u32 = 0x5200_2018;
const FLASH_OPTSR_CUR: u32 = 0x5200_201C;
const FLASH_OPTSR_PRG: u32 = 0x5200_2020;
const FLASH_SCAR_CUR1: u32 = 0x5200_2030;
const FLASH_SCAR_PRG1: u32 = 0x5200_2034;

#[derive(Parser, Debug)]
#[clap(name = "stmsecure", about = env!("CARGO_PKG_DESCRIPTION"))]
enum StmSecureArgs {
    /// Show status about secure region settings
    Status,
    /// Enable Read Out Protection (RDP) i.e. can't read flash from debugger
    SetRDP,
    /// Disable Read Out Protection (RDP).
    /// !!! This may also trigger an erase of flash if a secure region is set
    /// !!!
    UnsetRDP,
    /// Set the security option bit
    SetSecureBit,
    /// Unset the security option bit
    UnsetSecureBit,
    /// Set the secure region. The secure option bit must be set first.
    /// !!! You must make sure the application will boot out of the secure
    /// region before this is programmed otherwise you will brick the device
    /// !!!
    SetSecureRegion {
        #[clap(parse(try_from_str = parse_int::parse))]
        address: u32,
        #[clap(parse(try_from_str = parse_int::parse))]
        size: u32,
        #[clap(long)]
        doit: bool,
    },
    /// Unset the secure region. Read out protection must be enabled.
    /// !!! This will erase all the flash as well !!!
    UnsetSecureRegion,
    /// Swap the flash banks (Bank 1 -> Bank 2 or Bank 2 -> Bank 1)
    /// !!! Make sure secure regions are appropriately programmed !!!
    SwapBanks,
}

fn stmsecure_unlock_flash(core: &mut dyn Core) -> Result<()> {
    core.write_word_32(FLASH_KEYR1, FLASH_KEY1)?;
    core.write_word_32(FLASH_KEYR1, FLASH_KEY2)?;
    Ok(())
}

fn stmsecure_unlock_option(core: &mut dyn Core) -> Result<()> {
    core.write_word_32(FLASH_OPT_KEYR, FLASH_OPT_KEY1)?;
    core.write_word_32(FLASH_OPT_KEYR, FLASH_OPT_KEY2)?;
    Ok(())
}

fn stmsecure_commit_option(core: &mut dyn Core) -> Result<()> {
    // set start bit
    core.write_word_32(FLASH_OPT_CR, 0x2)?;

    loop {
        let stat = core.read_word_32(FLASH_OPTSR_CUR)?;
        if (stat & 0x1) == 0 {
            break;
        }
    }
    Ok(())
}

fn stmsecure_rdpset(core: &mut dyn Core) -> Result<()> {
    println!("setting rdp to level 1 (You will not be able to read the flash)");
    stmsecure_unlock_option(core)?;
    let optsr = core.read_word_32(FLASH_OPTSR_CUR)?;
    core.write_word_32(FLASH_OPTSR_PRG, (optsr & !0x0000_ff00) | 0x0000_bb00)?;
    stmsecure_commit_option(core)?;
    println!("done.");
    Ok(())
}

fn stmsecure_rdpunset_nocommit(core: &mut dyn Core) -> Result<()> {
    let optsr = core.read_word_32(FLASH_OPTSR_CUR)?;
    core.write_word_32(FLASH_OPTSR_PRG, (optsr & !0x0000_ff00) | 0x0000_aa00)?;
    Ok(())
}

fn stmsecure_rdpunset(core: &mut dyn Core) -> Result<()> {
    println!(
        "setting rdp level to 0. This may also erase the flash depending
    on your system settings!"
    );
    stmsecure_unlock_option(core)?;
    stmsecure_rdpunset_nocommit(core)?;
    stmsecure_commit_option(core)?;
    println!("done.");
    Ok(())
}

fn stmsecure_lockbit_set(core: &mut dyn Core) -> Result<()> {
    println!("Setting the secure option bit");
    stmsecure_unlock_option(core)?;
    let optsr = core.read_word_32(FLASH_OPTSR_CUR)?;
    core.write_word_32(FLASH_OPTSR_PRG, optsr | 0x20_0000)?;
    stmsecure_commit_option(core)?;
    println!("done.");
    Ok(())
}

fn stmsecure_lockbit_unset(core: &mut dyn Core) -> Result<()> {
    println!("Unsetting the secure option bit");
    stmsecure_unlock_option(core)?;
    let optsr = core.read_word_32(FLASH_OPTSR_CUR)?;
    core.write_word_32(FLASH_OPTSR_PRG, optsr & !0x20_0000)?;
    stmsecure_commit_option(core)?;
    println!("done.");
    Ok(())
}

fn stmsecure_status(core: &mut dyn Core) -> Result<()> {
    let optsr = core.read_word_32(FLASH_OPTSR_CUR)?;
    let rdp = (optsr & 0x0000_ff00) >> 8;
    let sec_en = (optsr & 0x20_0000) == 0x20_0000;

    let scar_cur1 = core.read_word_32(FLASH_SCAR_CUR1)?;
    let dmes1 = (scar_cur1 & 0x8000_0000) == (0x8000_0000);
    let sec_start = ((scar_cur1 & 0x0000_0FFF) << 8) | 0x0800_0000;
    let sec_end = (((scar_cur1 & 0x00FF_F000) >> 16) << 8) | 0x0800_00ff;

    println!("Sec bit: {}", sec_en);
    println!("Start: {:x}", sec_start);
    println!("End: {:x}", sec_end);
    println!("Erase on regression: {}", dmes1);
    println!("RDP: {:x}", rdp);
    Ok(())
}

fn stmsecure_setsecureregion(
    core: &mut dyn Core,
    address: u32,
    size: u32,
    commit: bool,
) -> Result<()> {
    // Basic checks to make sure we're not doing anything too weird
    if !(0x0800_0000..0x081f_ffff).contains(&address) {
        return Err(anyhow!("Secure address out of range: {:x}", address));
    }

    // Secure ranges are per bank
    if let Some(result) = address.checked_add(size) {
        if !(0x0800_0000..0x080f_ffff).contains(&result) {
            return Err(anyhow!(
                "secure address end size out of range {:x}-{:x}",
                address,
                result
            ));
        }
    } else {
        return Err(anyhow!(
            "Size and range overflowed {:x} {:x}",
            address,
            size
        ));
    }

    println!("Setting secure region: {:x}-{:x}", address, address + size);

    if !commit {
        println!("Not committing anything.");
        return Ok(());
    }

    let optsr = core.read_word_32(FLASH_OPTSR_CUR)?;
    if (optsr & 0x20_0000) != 0x20_0000 {
        return Err(anyhow!(
            "Set the secure bit before setting the secure region"
        ));
    }

    // We have to use the delightful ROM API in order to write this register
    core.halt()?;

    // Set up the structure at 0x2000_0000
    // typedef struct
    // {
    // uint32_t sizeInBytes; /**< pass 0 for an empty secure area */
    // uint32_t startAddress; /**< pass NULL for an empty secure area */
    // uint32_t removeDuringBankErase; /**< if 0, keep area during bank/mass
    // erase. else area will be removed*/ }RSS_SecureArea_t;
    //
    core.write_word_32(0x2000_0000, size)?;
    core.write_word_32(0x2000_0004, address)?;
    // We always remove during bank erase for now, otherwise we could get stuck
    // with a bricked board
    core.write_word_32(0x2000_0008, 0x1)?;

    // void RSS_resetAndInitializeSecureAreas(uint32_t nbAreas,
    // RSS_SecureArea_t* areas);
    core.write_reg(ARMRegister::R0, 1)?;
    core.write_reg(ARMRegister::R1, 0x2000_0000)?;

    // STM does not document very well how to call functions but this is the
    // address of the function we want
    core.write_reg(ARMRegister::PC, 0x1ff08a70)?;
    core.run()?;

    Ok(())
}

fn stmsecure_unsetsecureregion(core: &mut dyn Core) -> Result<()> {
    println!("Unsetting the secure region. This will erase the bank!");

    // This sequence is from the manual section 4.3.10
    // This can also be done with an RDP regression but that has the
    // disadvantage of erasing all flash as opposed to just a bank
    stmsecure_unlock_option(core)?;
    // Unset secure region by setting start > end
    // Make sure to set the DMES bit so the secure are gets erased as well
    core.write_word_32(FLASH_SCAR_PRG1, 0x8000_00ff)?;

    stmsecure_unlock_flash(core)?;

    // Set BER1 (bank erase) and the start bit to start the erase
    core.write_word_32(FLASH_CR1, 0x88)?;

    // This particular sequence will also automatically program the option bits
    // so there is no need to call option commit

    // Wait for the flash erase to complete
    loop {
        let stat = core.read_word_32(FLASH_SR1)?;
        if (stat & 0x4) == 0 {
            break;
        }
    }
    println!("done.");
    Ok(())
}

fn stmsecure_swapbanks(core: &mut dyn Core) -> Result<()> {
    println!("Swapping banks");
    stmsecure_unlock_option(core)?;
    let optsr = core.read_word_32(FLASH_OPTSR_CUR)?;
    // Bit 31 is used to swap banks. If it's set, unset it etc.
    if (optsr & 0x8000_0000) == 0x8000_0000 {
        core.write_word_32(FLASH_OPTSR_PRG, optsr & !0x8000_0000)?;
    } else {
        core.write_word_32(FLASH_OPTSR_PRG, optsr | 0x8000_0000)?;
    }
    stmsecure_commit_option(core)?;
    println!("done.");
    Ok(())
}

#[rustfmt::skip::macros(format)]
fn stmsecure(context: &mut humility::ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();

    let subargs = StmSecureArgs::try_parse_from(subargs)?;

    match subargs {
        StmSecureArgs::Status => stmsecure_status(core),
        StmSecureArgs::SetSecureBit => stmsecure_lockbit_set(core),
        StmSecureArgs::UnsetSecureBit => stmsecure_lockbit_unset(core),
        StmSecureArgs::SetSecureRegion { address, size, doit } => {
            stmsecure_setsecureregion(core, address, size, doit)
        }
        StmSecureArgs::UnsetSecureRegion => stmsecure_unsetsecureregion(core),
        StmSecureArgs::SetRDP => stmsecure_rdpset(core),
        StmSecureArgs::UnsetRDP => stmsecure_rdpunset(core),
        StmSecureArgs::SwapBanks => stmsecure_swapbanks(core),
    }
}

pub fn init() -> Command {
    Command {
        app: StmSecureArgs::command(),
        name: "stmsecure",
        run: stmsecure,
        kind: CommandKind::Attached {
            archive: Archive::Optional,
            attach: Attach::Any,
            validate: Validate::None,
        },
    }
}
