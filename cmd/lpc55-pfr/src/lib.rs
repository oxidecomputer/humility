// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility lpc55-pfr`
//!
//! Allows for modification of the LPC55 protected flash region (PFR) via
//! SWD. Also includes commands for displaying the values of the protected
//! flash region.
//!
//! ```code
//!
//! $ humility lpc55-pfr read-cfpa
//! humility: attached via CMSIS-DAP
//! Last scratch version: 33
//! PONG version 33 is active
//! Debug settings pin fd0002ff dflt ff3000cf
//! Active RKTH revoke: 5
//! Active image revoke: 0
//!
//! $ humility lpc55-pfr write-cfpa CFPA_enabled.bin
//! humility: attached via CMSIS-DAP
//! Wrote CFPA! Resetting chip now
//! ```

use anyhow::{bail, Context, Result};
use clap::{CommandFactory, Parser};
use humility::cli::Subcommand;
use humility::{arch::ARMRegister, core::Core};
use humility_cmd::{Archive, Attach, Command, CommandKind, Validate};
use lpc55_areas::{CFPAPage, CMPAPage};
use std::io::Read;
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[clap(name = "subcmd")]
enum PfrCmd {
    /// Read the CFPA region
    ReadCFPA,
    /// Write the CFPA region
    WriteCFPA {
        #[clap(parse(from_os_str))]
        file: PathBuf,
    },
    /// Read CMPA region
    ReadCMPA,
    /// Write CMPA region (can only be done if not locked)
    WriteCMPA {
        #[clap(parse(from_os_str))]
        file: PathBuf,
    },
    /// Erase CMPA (can only be done if not locked)
    EraseCMPA {
        #[clap(long)]
        full: bool,
    },
    /// Read Keystore
    ReadKeystore,
}

#[derive(Parser, Debug)]
#[clap(name = "lpc55-pfr", about = env!("CARGO_PKG_DESCRIPTION"))]
struct PfrArgs {
    #[clap(subcommand)]
    cmd: PfrCmd,
}

// Holds flash_config_t (see NXP manual)
const FLASH_CONFIG_ADDR: u32 = 0x1400_0000;
const FLASH_CONFIG_SIZE: u32 = 60;
const FLASH_SYSTEM_SPEED_OFFSET: u32 = 40;
const FLASH_MAX_SPEED: u32 = 96;
// Staging area for our data
const DATA_ADDR: u32 = 0x1400_1000;

// Set LR to this address after each of the functions
const END_ADDR: u32 = 0x1400_4000;
// Stack!
const STACK_ADDR: u32 = 0x1400_3ffc;

// Lovingly taken from the flash table
const FLASH_INIT_ADDR: u32 = 0x1300_71fc;
const FFR_INIT_ADDR: u32 = 0x1300_5848;
const FFR_INFIELD_WRITE_ADDR: u32 = 0x1300_59bc;
const FFR_FACTORY_WRITE_ADDR: u32 = 0x1300_5c3e;

const CFPA_SCRATCH: u32 = 0x9_de00;
const CFPA_PING: u32 = 0x9_e000;
const CFPA_PONG: u32 = 0x9_e200;

const CMPA: u32 = 0x9_e400;
const ACTIVATION_CODE: u32 = 0x9_e600;
const ACTIVATION_CODE_MARKER: u32 = 0x9595_9595;
const SBKEK_CODE: u32 = 0x9_eab0;
const UDS_CODE: u32 = 0x9_eb20;
const KEY_MARKER: u32 = 0x5959_5959;

const FLASH_PAGE_SIZE: u32 = 512;

const BREAKPOINT_INSN: u32 = 0xBE00_BE00;

fn run_fn(core: &mut dyn Core, regs: Vec<(ARMRegister, u32)>) -> Result<()> {
    // Always reset the stack
    core.write_reg(ARMRegister::SP, STACK_ADDR)?;
    // End address designed to give a breakpoint
    // Remember to set the thumb bit!
    core.write_reg(ARMRegister::LR, END_ADDR | 1)?;
    for (reg, addr) in regs {
        core.write_reg(reg, addr)?;
    }

    core.run()?;
    core.wait_for_halt(std::time::Duration::from_secs(2))
        .context("Failed to hit breakpoint after 2 seconds!")
}

fn flash_init(core: &mut dyn Core) -> Result<()> {
    core.halt()?;

    // clear our space
    for i in (0..FLASH_CONFIG_SIZE).step_by(4) {
        core.write_word_32(FLASH_CONFIG_ADDR + i, 0)?;
    }

    // hack always say we're running at 96 because it is fast
    // This _should_ ensure we always wait enough time for programming
    // because 96 is the upper limit
    core.write_word_32(
        FLASH_CONFIG_ADDR + FLASH_SYSTEM_SPEED_OFFSET,
        FLASH_MAX_SPEED,
    )?;

    // Borrow this from the flash algorithms and mark our end function as
    // a breakpoint
    core.write_word_32(END_ADDR, BREAKPOINT_INSN)?;

    run_fn(
        core,
        vec![
            (ARMRegister::R0, FLASH_CONFIG_ADDR),
            (ARMRegister::PC, FLASH_INIT_ADDR),
        ],
    )?;

    run_fn(
        core,
        vec![
            (ARMRegister::R0, FLASH_CONFIG_ADDR),
            (ARMRegister::PC, FFR_INIT_ADDR),
        ],
    )?;

    Ok(())
}

fn write_cfpa(core: &mut dyn Core, cfpa: &[u8]) -> Result<()> {
    flash_init(core)?;

    // write to our staging area
    core.write_8(DATA_ADDR, cfpa)?;

    run_fn(
        core,
        vec![
            (ARMRegister::R0, FLASH_CONFIG_ADDR),
            // arg1 = data
            (ARMRegister::R1, DATA_ADDR),
            // arg2 = len (always 512)
            (ARMRegister::R2, FLASH_PAGE_SIZE),
            (ARMRegister::PC, FFR_INFIELD_WRITE_ADDR),
        ],
    )?;

    Ok(())
}

fn write_cmpa(core: &mut dyn Core, cmpa: &[u8]) -> Result<()> {
    flash_init(core)?;

    // write to our staging area
    core.write_8(DATA_ADDR, cmpa)?;

    run_fn(
        core,
        vec![
            (ARMRegister::R0, FLASH_CONFIG_ADDR),
            // arg1 = data
            (ARMRegister::R1, DATA_ADDR),
            // arg2 = seal (this should be zero!)
            (ARMRegister::R2, 0),
            (ARMRegister::PC, FFR_FACTORY_WRITE_ADDR),
        ],
    )?;

    Ok(())
}

fn lpc55_pfr(context: &mut humility::ExecutionContext) -> Result<()> {
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let subargs = PfrArgs::try_parse_from(subargs)?;
    let core = &mut **context.core.as_mut().unwrap();

    match subargs.cmd {
        PfrCmd::ReadCFPA => {
            let mut bytes = [0u8; FLASH_PAGE_SIZE as usize];
            core.read_8(CFPA_SCRATCH, &mut bytes)?;
            let scratch = CFPAPage::from_bytes(&bytes)?;

            core.read_8(CFPA_PING, &mut bytes)?;
            let ping = CFPAPage::from_bytes(&bytes)?;

            core.read_8(CFPA_PONG, &mut bytes)?;
            let pong = CFPAPage::from_bytes(&bytes)?;

            println!("Last scratch version: {}", scratch.version);

            if ping.version > pong.version {
                println!("PING version {} is active", ping.version);
                println!(
                    "Debug settings pin {:x} dflt {:x}",
                    ping.dcfg_cc_socu_ns_pin, ping.dcfg_cc_socu_ns_dflt
                );
                println!("Active RKTH revoke: {:x}", ping.rkth_revoke);
                println!("Active image revoke: {:x}", ping.image_key_revoke);
            } else {
                println!("PONG version {} is active", pong.version);
                println!(
                    "Debug settings pin {:x} dflt {:x}",
                    pong.dcfg_cc_socu_ns_pin, pong.dcfg_cc_socu_ns_dflt
                );
                println!("Active RKTH revoke: {:x}", pong.rkth_revoke);
                println!("Active image revoke: {:x}", pong.image_key_revoke);
            }
        }
        PfrCmd::WriteCFPA { file } => {
            let mut file_bytes = Vec::new();
            let mut infile =
                std::fs::OpenOptions::new().read(true).open(&file)?;

            infile.read_to_end(&mut file_bytes)?;

            let mut bytes = [0u8; FLASH_PAGE_SIZE as usize];
            bytes.clone_from_slice(&file_bytes);
            let mut cfpa = CFPAPage::from_bytes(&bytes)?;

            core.read_8(CFPA_PING, &mut bytes)?;
            let ping = CFPAPage::from_bytes(&bytes)?;

            core.read_8(CFPA_PONG, &mut bytes)?;
            let pong = CFPAPage::from_bytes(&bytes)?;

            if ping.version > pong.version {
                cfpa.version = ping.version;
            } else {
                cfpa.version = pong.version;
            }

            core.read_8(CMPA, &mut bytes)?;
            let cmpa = CMPAPage::from_bytes(&bytes)?;

            if (cmpa.cc_socu_pin == 0 || cmpa.cc_socu_dflt == 0)
                && (cfpa.dcfg_cc_socu_ns_pin != 0
                    || cfpa.dcfg_cc_socu_ns_dflt != 0)
            {
                bail!("The CMPA does not have debug setting set but the CFPA does, this will brick the chip!");
            }

            cfpa.update_version();
            core.reset_and_halt(std::time::Duration::from_secs(2))?;
            write_cfpa(core, &cfpa.to_vec()?)?;

            println!("Wrote CFPA! Resetting chip now");
            core.reset()?;
        }
        PfrCmd::ReadCMPA => {
            let mut bytes = [0u8; FLASH_PAGE_SIZE as usize];
            core.read_8(CMPA, &mut bytes)?;
            let cmpa = CMPAPage::from_bytes(&bytes)?;

            println!("Boot Config: {:x}", cmpa.boot_cfg);
            println!(
                "Debug settings pin {:x} dflt {:x}",
                cmpa.cc_socu_pin, cmpa.cc_socu_dflt
            );
            println!("Secure Boot Config: {:x}", cmpa.secure_boot_cfg);
            println!("Key Hash: {:x?}", cmpa.rotkh);
            println!("Sealed: {:x?}", cmpa.sha256_digest);
        }
        PfrCmd::WriteCMPA { file } => {
            let mut file_bytes = Vec::new();
            let mut infile =
                std::fs::OpenOptions::new().read(true).open(&file)?;

            infile.read_to_end(&mut file_bytes)?;

            let mut bytes = [0u8; FLASH_PAGE_SIZE as usize];
            bytes.clone_from_slice(&file_bytes);
            let cmpa = CMPAPage::from_bytes(&bytes)?;

            core.read_8(CFPA_PING, &mut bytes)?;
            let ping = CFPAPage::from_bytes(&bytes)?;

            core.read_8(CFPA_PONG, &mut bytes)?;
            let pong = CFPAPage::from_bytes(&bytes)?;

            let (pin, dflt) = if ping.version > pong.version {
                (ping.dcfg_cc_socu_ns_pin, ping.dcfg_cc_socu_ns_dflt)
            } else {
                (pong.dcfg_cc_socu_ns_pin, pong.dcfg_cc_socu_ns_dflt)
            };

            if (pin != 0 || dflt != 0)
                && (cmpa.cc_socu_dflt == 0 || cmpa.cc_socu_pin == 0)
            {
                bail!("CFPA has non-zero debug settings but CMPA has zero! This would brick the chip!");
            }

            core.reset_and_halt(std::time::Duration::from_secs(2))?;
            write_cmpa(core, &file_bytes)?;
            println!("Wrote CMPA! Resetting chip now");
            core.reset()?;
        }
        PfrCmd::EraseCMPA { full } => {
            let b = if full {
                let mut bytes = [0u8; FLASH_PAGE_SIZE as usize];
                core.read_8(CFPA_PING, &mut bytes)?;
                let ping = CFPAPage::from_bytes(&bytes)?;

                core.read_8(CFPA_PONG, &mut bytes)?;
                let pong = CFPAPage::from_bytes(&bytes)?;

                let (pin, dflt) = if ping.version > pong.version {
                    (ping.dcfg_cc_socu_ns_pin, ping.dcfg_cc_socu_ns_dflt)
                } else {
                    (pong.dcfg_cc_socu_ns_pin, pong.dcfg_cc_socu_ns_dflt)
                };

                if pin != 0 || dflt != 0 {
                    bail!("CFPA has non-zero debug settings -- erasing would brick the chip!");
                }
                vec![0; 512]
            } else {
                let mut bytes = [0u8; FLASH_PAGE_SIZE as usize];
                core.read_8(CMPA, &mut bytes)?;
                let mut cmpa = CMPAPage::from_bytes(&bytes)?;
                cmpa.secure_boot_cfg = 0;
                cmpa.to_vec()?
            };

            core.reset_and_halt(std::time::Duration::from_secs(2))?;
            write_cmpa(core, &b)?;
            println!("Erased CMPA! Resetting chip now");
            core.reset()?;
        }
        PfrCmd::ReadKeystore => {
            // The key store marks each valid entry with a header. We don't
            // actually care about the key value itself, only that the header
            // is valid
            let activation_code = core.read_word_32(ACTIVATION_CODE)?;
            let sbkek_code = core.read_word_32(SBKEK_CODE)?;
            let uds_code = core.read_word_32(UDS_CODE)?;

            println!(
                "Activation code set: {}",
                activation_code == ACTIVATION_CODE_MARKER
            );
            println!("SBKEK set: {}", sbkek_code == KEY_MARKER);
            println!("UDS set: {}", uds_code == KEY_MARKER);
        }
    }

    Ok(())
}

pub fn init() -> Command {
    Command {
        app: PfrArgs::command(),
        name: "lpc55-pfr",
        run: lpc55_pfr,
        kind: CommandKind::Attached {
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
        },
    }
}
