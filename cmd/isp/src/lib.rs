// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility isp`
//!
//! The LPC55 has a built-in In-System Programming (ISP) mode. ISP mode
//! does _not_ work over the regular debug probe but over UART. You must
//! give the UART path as an argument.
//!
//! $ humility isp --port /dev/ttyUSB0 read-memory 0x0 32
//!             \/  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f
//! 0x00000000 | 00 40 00 20 31 01 00 00 f3 02 00 00 a5 04 00 00 | .@. 1...........
//! 0x00000010 | a1 01 00 00 a3 01 00 00 a5 01 00 00 a7 01 00 00 | ................
//!

use anyhow::Result;
use byteorder::ByteOrder;
use clap::{CommandFactory, Parser};
use humility::cli::Subcommand;
use humility_cmd::{Archive, Command, CommandKind, Dumper};
use lpc55_areas::{CFPAPage, CMPAPage};
use serialport::{DataBits, FlowControl, Parity, StopBits};
use std::io::Read;
use std::path::PathBuf;

mod cmd;
mod protocol;

use crate::protocol::BootloaderProperty;

#[derive(Parser, Debug)]
#[clap(name = "subcmd")]
enum IspCmd {
    /// Runs a single ping to verify communication with the target
    Ping,
    /// Reads memory from the specified address
    ReadMemory {
        #[clap(parse(try_from_str = parse_int::parse))]
        address: u32,
        #[clap(parse(try_from_str = parse_int::parse))]
        count: u32,
    },
    /// Write the file to the specified address
    WriteMemory {
        #[clap(parse(try_from_str = parse_int::parse))]
        address: u32,
        #[clap(parse(from_os_str))]
        file: PathBuf,
    },
    /// Erases all non-secure flash. This MUST be done before writing!
    FlashEraseAll,
    /// Write a file to the CMPA region
    WriteCMPA,
    /// Write a file to the CFPA region
    WriteCFPA,
    /// Read the CMPA region
    ReadCMPA,
    /// Read the CFPA regions (scratch, ping, pong)
    ReadCFPA,
    /// Erase the CMPA region (use to boot non-secure binaries again)
    EraseCMPA {
        #[clap(long)]
        full: bool,
    },
    /// Put a minimalist program on to allow attaching via SWD
    Restore,
    /// Set up key store this involves
    /// - Enroll
    /// - Setting UDS
    /// - Writing to persistent storage
    SetupKeyStore,
    /// Trigger a new enrollment in the PUF
    Enroll,
    /// Generate a new device secret for use in DICE
    GenerateUDS,
    /// Write keystore to flash
    WriteKeyStore,
    /// Erase existing keystore
    EraseKeyStore,
    /// Get Bootloader property
    GetProperty { prop: BootloaderProperty },
    /// Get information about why the chip put itself in ISP mode
    LastError,
}

#[derive(Parser, Debug)]
#[clap(name = "isp", about = env!("CARGO_PKG_DESCRIPTION"))]
struct IspArgs {
    /// sets timeout
    #[clap(
        long, short = 'T', default_value_t = 5000, value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// UART port
    #[clap(long, value_name = "port")]
    port: PathBuf,

    #[clap(short = 'b', default_value_t = 57600)]
    baud_rate: u32,

    #[clap(subcommand)]
    cmd: IspCmd,

    /// print out data read as words rather than bytes
    #[clap(long, short = 'W')]
    word: bool,
}

fn pretty_print_error(params: Vec<u32>) {
    let reason = params[1] & 0xfffffff0;
    if reason == 0 {
        println!("No errors reported");
    } else if reason == 0x0602f300 {
        println!("Passive boot failed, reason:");
        let specific_reason = params[2] & 0xfffffff0;
        match specific_reason {
            0x0b36f300 => {
                println!("Secure image authentication failed. Check:");
                println!("- Is the image you are booting signed?");
                println!("- Is the image signed with the corresponding key?");
            }
            0x0b37f300 => {
                println!("Application CRC failed");
            }
            0x0b35f300 => {
                println!("Application entry point and/or stack is invalid");
            }
            0x0b38f300 => {
                println!("DICE failure. Check:");
                println!("- Key store is set up properly (UDS)");
            }
            0x0d70f300 => {
                println!("Trying to boot a TZ image on a device that doesn't have TZ!");
            }
            0x0d71f300 => {
                println!("Error reading TZ Image type from CMPA");
            }
            0x0d72f300 => {
                println!("Bad TZ image mode, check your image");
            }
            0x0c00f500 => {
                println!("Application returned to the ROM?");
            }
            _ => {
                println!("Some other reason, raw bytes: {:x?}", params);
            }
        }
    } else {
        println!("Something bad happen: {:x?}", params);
    }
}

fn pretty_print_bootloader_prop(prop: BootloaderProperty, params: Vec<u32>) {
    match prop {
        BootloaderProperty::BootloaderVersion => {
            println!("Version {:x}", params[1]);
        }
        BootloaderProperty::AvailablePeripherals => {
            println!("Bitmask of peripherals {:x}", params[1]);
        }
        BootloaderProperty::FlashStart => {
            println!("Flash start = 0x{:x}", params[1]);
        }
        BootloaderProperty::FlashSize => {
            println!("Flash Size = {:x}", params[1]);
        }
        BootloaderProperty::FlashSectorSize => {
            println!("Flash Sector Size = {:x}", params[1]);
        }
        BootloaderProperty::AvailableCommands => {
            println!("Bitmask of commands = {:x}", params[1]);
        }
        BootloaderProperty::CRCStatus => {
            println!("CRC status = {}", params[1]);
        }
        BootloaderProperty::VerifyWrites => {
            println!("Verify Writes (bool) {}", params[1]);
        }
        BootloaderProperty::MaxPacketSize => {
            println!("Max Packet Size = {}", params[1]);
        }
        BootloaderProperty::ReservedRegions => {
            println!("Reserved regions? = {:x?}", params);
        }
        BootloaderProperty::RAMStart => {
            println!("RAM start = 0x{:x}", params[1]);
        }
        BootloaderProperty::RAMSize => {
            println!("RAM size = 0x{:x}", params[1]);
        }
        BootloaderProperty::SystemDeviceID => {
            println!("DEVICE_ID0 register = 0x{:x}", params[1]);
        }
        BootloaderProperty::SecurityState => {
            println!(
                "Security State = {}",
                if params[1] == 0x5aa55aa5 { "UNLOCKED" } else { "LOCKED" }
            );
        }
        BootloaderProperty::UniqueID => {
            println!(
                "UUID = {:x}{:x}{:x}{:x}",
                params[1], params[2], params[3], params[4]
            );
        }
        BootloaderProperty::TargetVersion => {
            println!("Target version = {:x}", params[1]);
        }
        BootloaderProperty::FlashPageSize => {
            println!("Flash page size = {:x}", params[1]);
        }
        BootloaderProperty::IRQPinStatus => {
            println!("IRQ Pin Status = {}", params[1]);
        }
        BootloaderProperty::FFRKeyStoreStatus => {
            println!("FFR Store Status = {}", params[1]);
        }
    }
}

fn ispcmd(context: &mut humility::ExecutionContext) -> Result<()> {
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let subargs = IspArgs::try_parse_from(subargs)?;

    // The target _technically_ has autobaud but it's very flaky
    // and these seem to be the preferred settings

    let mut port =
        serialport::new(subargs.port.to_string_lossy(), subargs.baud_rate)
            .timeout(std::time::Duration::from_millis(subargs.timeout as u64))
            .data_bits(DataBits::Eight)
            .flow_control(FlowControl::None)
            .parity(Parity::None)
            .stop_bits(StopBits::One)
            .open()?;

    crate::cmd::do_ping(&mut *port)?;

    match subargs.cmd {
        IspCmd::Ping => {
            println!("ping success.");
        }
        IspCmd::ReadMemory { address, count } => {
            let m = crate::cmd::do_isp_read_memory(&mut *port, address, count)?;

            let mut dumper = Dumper::new();
            dumper.size = if subargs.word { 4 } else { 1 };
            dumper.dump(&m, address);
        }
        IspCmd::WriteMemory { address, file } => {
            println!("If you didn't already erase the flash this operation will fail!");
            println!("This operation may take a while");
            let mut infile =
                std::fs::OpenOptions::new().read(true).open(&file)?;

            let mut bytes = Vec::new();

            infile.read_to_end(&mut bytes)?;

            crate::cmd::do_isp_write_memory(&mut *port, address, bytes)?;
            println!("Write complete!");
        }
        IspCmd::FlashEraseAll => {
            crate::cmd::do_isp_flash_erase_all(&mut *port)?;

            println!("Flash erased!");
        }
        IspCmd::WriteCMPA => {
            let bytes = match &context.archive {
                None => anyhow::bail!("Missing required archive"),
                Some(archive) => match archive.read_cmpa()? {
                    None => anyhow::bail!("CMPA not found in archive"),
                    Some(cmpa) => cmpa,
                },
            };

            let mut cmpa_bytes = [0u8; 512];
            cmpa_bytes.clone_from_slice(&bytes);
            let cmpa = CMPAPage::from_bytes(&cmpa_bytes)?;

            let m1 = crate::cmd::do_isp_read_memory(&mut *port, 0x9e000, 512)?;
            let m2 = crate::cmd::do_isp_read_memory(&mut *port, 0x9e200, 512)?;
            let mut ping_bytes = [0u8; 512];
            let mut pong_bytes = [0u8; 512];

            ping_bytes.clone_from_slice(&m1);
            pong_bytes.clone_from_slice(&m2);
            let ping = CFPAPage::from_bytes(&ping_bytes)?;
            let pong = CFPAPage::from_bytes(&pong_bytes)?;

            let (pin, dflt) = if ping.version > pong.version {
                (ping.dcfg_cc_socu_ns_pin, ping.dcfg_cc_socu_ns_dflt)
            } else {
                (pong.dcfg_cc_socu_ns_pin, pong.dcfg_cc_socu_ns_dflt)
            };

            if (pin != 0 || dflt != 0)
                && (cmpa.cc_socu_pin == 0 || cmpa.cc_socu_dflt == 0)
            {
                anyhow::bail!("CFPA has non-zero debug settings but CMPA has zero settings! This would brick the chip!");
            }

            crate::cmd::do_isp_write_memory(&mut *port, 0x9e400, bytes)?;
            println!("Write to CMPA done!");
        }
        IspCmd::WriteCFPA => {
            let bytes = match &context.archive {
                None => anyhow::bail!("Missing required archive"),
                Some(archive) => match archive.read_cfpa()? {
                    None => anyhow::bail!("CMPA not found in archive"),
                    Some(cmpa) => cmpa,
                },
            };

            let m = crate::cmd::do_isp_read_memory(&mut *port, 0x9e400, 512)?;
            let mut cmpa_bytes = [0u8; 512];
            cmpa_bytes.clone_from_slice(&m);

            let cmpa = CMPAPage::from_bytes(&cmpa_bytes)?;

            let mut cfpa_bytes = [0u8; 512];
            cfpa_bytes.clone_from_slice(&bytes);

            let cfpa = CFPAPage::from_bytes(&cfpa_bytes)?;

            if (cfpa.dcfg_cc_socu_ns_pin != 0 || cfpa.dcfg_cc_socu_ns_dflt != 0)
                && (cmpa.cc_socu_pin == 0 || cmpa.cc_socu_dflt == 0)
            {
                anyhow::bail!("It looks like the CMPA debug settings aren't set but the CFPA settings are! This will brick the chip!");
            }

            crate::cmd::do_isp_write_memory(&mut *port, 0x9de00, bytes)?;
            println!("Write to CFPA done!");
        }
        IspCmd::ReadCFPA => {
            let m = crate::cmd::do_isp_read_memory(&mut *port, 0x9de00, 512)?;

            let mut dumper = Dumper::new();
            dumper.size = 4;
            println!("=====Scratch Page=====");
            dumper.dump(&m, 0x9de00);

            let m = crate::cmd::do_isp_read_memory(&mut *port, 0x9e000, 512)?;

            println!("=====Ping Page=====");
            dumper.dump(&m, 0x9e000);

            let m = crate::cmd::do_isp_read_memory(&mut *port, 0x9e200, 512)?;
            println!("=====Pong Page=====");
            dumper.dump(&m, 0x9e200);
        }
        IspCmd::EraseCMPA { full } => {
            let b = if full {
                let m1 =
                    crate::cmd::do_isp_read_memory(&mut *port, 0x9e000, 512)?;
                let m2 =
                    crate::cmd::do_isp_read_memory(&mut *port, 0x9e200, 512)?;
                let mut ping_bytes = [0u8; 512];
                let mut pong_bytes = [0u8; 512];

                ping_bytes.clone_from_slice(&m1);
                pong_bytes.clone_from_slice(&m2);
                let ping = CFPAPage::from_bytes(&ping_bytes)?;
                let pong = CFPAPage::from_bytes(&pong_bytes)?;

                let (pin, dflt) = if ping.version > pong.version {
                    (ping.dcfg_cc_socu_ns_pin, ping.dcfg_cc_socu_ns_dflt)
                } else {
                    (pong.dcfg_cc_socu_ns_pin, pong.dcfg_cc_socu_ns_dflt)
                };
                if pin != 0 || dflt != 0 {
                    anyhow::bail!("The CFPA has non-zero settings! Erasing the CMPA would brick the chip!");
                }

                vec![0; 512]
            } else {
                let m =
                    crate::cmd::do_isp_read_memory(&mut *port, 0x9e400, 512)?;
                let mut bytes = [0u8; 512];
                bytes.clone_from_slice(&m);

                let mut cmpa = CMPAPage::from_bytes(&bytes)?;

                cmpa.secure_boot_cfg = 0;
                cmpa.to_vec()?
            };
            crate::cmd::do_isp_write_memory(&mut *port, 0x9e400, b)?;
            println!("CMPA region erased!");
            println!("You can now boot unsigned images");
        }
        IspCmd::ReadCMPA => {
            let m = crate::cmd::do_isp_read_memory(&mut *port, 0x9e400, 512)?;

            let mut dumper = Dumper::new();
            dumper.size = 4;
            dumper.dump(&m, 0x9e400);
        }
        IspCmd::Restore => {
            println!("Erasing flash");
            crate::cmd::do_isp_flash_erase_all(&mut *port)?;
            println!("Erasing done.");

            // we need to fill 0x134 bytes to cover the vector table
            // plus all interrupts
            let mut bytes: [u8; 0x134] = [0u8; 0x134];

            // Choose a RAM address for the stack (we shouldn't use the stack
            // but it should be valid anyway)
            byteorder::LittleEndian::write_u32(
                &mut bytes[0x0..0x4],
                0x20004000,
            );
            // Everything else targets the loop to branch instruction at 0x00000130
            let mut offset = 4;
            while offset < 0x130 {
                byteorder::LittleEndian::write_u32(
                    &mut bytes[offset..offset + 4],
                    0x00000131,
                );
                offset += 4;
            }
            // This is two branch to self instructions
            byteorder::LittleEndian::write_u32(
                &mut bytes[0x130..0x134],
                0xe7fee7fe,
            );

            println!("Writing bytes");
            crate::cmd::do_isp_write_memory(&mut *port, 0x0, bytes.to_vec())?;

            println!("Restore done! SWD should work now.");
        }
        IspCmd::Enroll => {
            println!("Generating new activation code");

            crate::cmd::do_enroll(&mut *port)?;
            println!("done.");
            println!("If you want to save this, remember to write to non-volatile memory");
        }
        IspCmd::GenerateUDS => {
            println!("Generating new UDS");

            crate::cmd::do_generate_uds(&mut *port)?;
            println!("done.");
            println!("If you want to save this, remember to write to non-volatile memory");
        }
        IspCmd::WriteKeyStore => {
            println!("Writing key store to flash");
            crate::cmd::do_save_keystore(&mut *port)?;
            println!("done.");
        }
        IspCmd::EraseKeyStore => {
            println!("Erasing existing keystore");
            // Write 3 * 512 bytes of 0
            let bytes = vec![0; 512 * 3];

            crate::cmd::do_isp_write_keystore(&mut *port, bytes)?;
            crate::cmd::do_save_keystore(&mut *port)?;
            println!("done.")
        }
        IspCmd::SetupKeyStore => {
            // Step 1: Enroll
            println!("Generating new activation code");
            crate::cmd::do_enroll(&mut *port)?;

            // Step 2: Generate UDS
            println!("Generating new UDS");
            crate::cmd::do_generate_uds(&mut *port)?;

            println!("Writing keystore");
            // Step 3: Write the keystore to persistent storage
            crate::cmd::do_save_keystore(&mut *port)?;
        }
        IspCmd::GetProperty { prop } => {
            let result = crate::cmd::do_isp_get_property(&mut *port, prop)?;
            pretty_print_bootloader_prop(prop, result);
        }
        IspCmd::LastError => {
            let result = crate::cmd::do_isp_last_error(&mut *port)?;
            pretty_print_error(result);
        }
    };

    Ok(())
}

pub fn init() -> Command {
    Command {
        app: IspArgs::command(),
        name: "isp",
        run: ispcmd,
        kind: CommandKind::Unattached { archive: Archive::Optional },
    }
}
