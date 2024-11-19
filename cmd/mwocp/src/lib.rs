// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility mwocp`
//!
//! `humility mwocp` allows for flashing the MWOCP68 family of PSUs.
//!

use humility::hubris::*;
use humility::msg;
use humility::reflect::{Base, Value};
use humility_cli::{ExecutionContext, Subcommand};
use humility_cmd::{Archive, Attach, Command, CommandKind, Validate};
use humility_hiffy::*;
use humility_i2c::I2cArgs;

use anyhow::{anyhow, bail, Context, Result};
use clap::{CommandFactory, Parser};
use hif::*;
use indicatif::{HumanBytes, HumanDuration};
use indicatif::{ProgressBar, ProgressStyle};
use pmbus::commands::mwocp68::*;
use std::collections::HashMap;
use std::fs::{self, OpenOptions};
use std::io::prelude::*;
use std::io::BufReader;
use std::io::Write;
use std::thread;
use std::thread::sleep;
use std::time::SystemTime;
use std::time::{Duration, Instant};
use zerocopy::{AsBytes, FromBytes};

#[derive(Parser, Debug)]
#[clap(name = "mwocp", about = env!("CARGO_PKG_DESCRIPTION"),
    group = clap::ArgGroup::new("subcommand").multiple(false)
)]
struct MwocpArgs {
    /// sets timeout
    #[clap(
        long, short, default_value_t = 5000, value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    #[clap(flatten)]
    dev: DeviceIdentity,

    /// flash a new image on the primary microcontroller
    #[clap(long, short, value_name = "filename", group = "subcommand")]
    flash: Option<String>,
}

#[derive(Parser, Debug)]
struct DeviceIdentity {
    /// specifies a device by rail name
    #[clap(long, short = 'r', value_name = "rail")]
    rail: Option<String>,

    /// specifies a PMBus driver
    #[clap(long, short = 'D')]
    driver: Option<String>,

    /// specifies an I2C bus by name
    #[clap(long, short, value_name = "bus",
        conflicts_with_all = &["port", "controller"]
    )]
    bus: Option<String>,

    /// specifies an I2C controller
    #[clap(long, short, value_name = "controller",
        parse(try_from_str = parse_int::parse),
    )]
    controller: Option<u8>,

    /// specifies an I2C controller port
    #[clap(long, short, value_name = "port")]
    port: Option<String>,

    /// specifies I2C multiplexer and segment
    #[clap(long, short, value_name = "mux:segment")]
    mux: Option<String>,

    /// specifies an I2C device address
    #[clap(long, short = 'd', value_name = "address")]
    device: Option<String>,
}

fn all_commands(
    device: pmbus::Device,
) -> HashMap<String, (u8, pmbus::Operation, pmbus::Operation)> {
    let mut all = HashMap::new();

    for i in 0..=255u8 {
        device.command(i, |cmd| {
            all.insert(
                cmd.name().to_string(),
                (i, cmd.read_op(), cmd.write_op()),
            );
        });
    }

    all
}

const MWOCP68_MFR_ID: &str = "Murata-PS";
const MWOCP68_MFR_MODEL: &str = "MWOCP68-3600-D-RM";
const MWOCP68_BOOT_LOADER_KEY: &str = "InVe";
const MWOCP68_PRODUCT_KEY: &str = "M5813-0000000000";
const MWOCP68_KEY_DELAY: u64 = 3;
const MWOCP68_BOOT_DELAY: u64 = 1;
const MWOCP68_RESET_DELAY: u64 = 2;
const MWOCP68_BLOCK_LENGTH: usize = 32;
const MWOCP68_BLOCK_DELAY_MS: u8 = 100;
const MWOCP68_CHECKSUM_DELAY: u64 = 2;
const MWOCP68_REBOOT_DELAY: u64 = 5;

fn mwocp(context: &mut ExecutionContext) -> Result<()> {
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let hubris = context.archive.as_mut().unwrap();

    let core = &mut **context.core.as_mut().unwrap();

    let subargs = MwocpArgs::try_parse_from(subargs)?;

    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;

    let i2c_read = context.get_function("I2cRead", 7)?;
    let i2c_write = context.get_function("I2cWrite", 8)?;
    let i2c_bulk_write = context.get_function("I2cBulkWrite", 8)?;
    let sleep = context.get_function("Sleep", 1)?;

    let hargs = match (&subargs.dev.rail, &subargs.dev.device) {
        (Some(rail), None) => {
            let mut found = None;

            for device in &hubris.manifest.i2c_devices {
                if let HubrisI2cDeviceClass::Pmbus { rails } = &device.class {
                    for r in rails {
                        if rail == &r.name {
                            found = match found {
                                Some(_) => {
                                    bail!("multiple devices match {}", rail);
                                }
                                None => Some(device),
                            }
                        }
                    }
                }
            }

            match found {
                None => {
                    bail!("rail {} not found", rail);
                }
                Some(device) => I2cArgs::from_device(device),
            }
        }

        (None, None) => {
            bail!("must provide a device as either a rail or an address");
        }

        (_, _) => I2cArgs::parse(
            hubris,
            &subargs.dev.bus,
            subargs.dev.controller,
            &subargs.dev.port,
            &subargs.dev.mux,
            &subargs.dev.device,
        )?,
    };

    let device = if let Some(driver) = &subargs.dev.driver {
        match pmbus::Device::from_str(driver) {
            Some(device) => device,
            None => {
                bail!("unknown device \"{}\"", driver);
            }
        }
    } else if let Some(ref driver) = hargs.device {
        match pmbus::Device::from_str(driver) {
            Some(device) => device,
            None => {
                bail!("{} is not recognized as a PMBus device", driver);
            }
        }
    } else {
        bail!("not recognized as a device");
    };

    let all = all_commands(device);

    let mut base = vec![Op::Push(hargs.controller), Op::Push(hargs.port.index)];

    if let Some(mux) = hargs.mux {
        base.push(Op::Push(mux.0));
        base.push(Op::Push(mux.1));
    } else {
        base.push(Op::PushNone);
        base.push(Op::PushNone);
    }

    let address = match hargs.address {
        Some(address) => address,
        None => {
            bail!("expected device");
        }
    };

    base.push(Op::Push(address));

    let str_result =
        |result: &Result<Vec<u8>, IpcError>, what| -> Result<String> {
            match result {
                &Err(err) => {
                    bail!("failed to read {what}: {}", i2c_read.strerror(err));
                }
                &Ok(ref result) => match String::from_utf8(result.clone()) {
                    Ok(str) => Ok(str),
                    Err(_) => {
                        bail!("failed to read {what} as string: {result:?}");
                    }
                },
            }
        };

    let check_str_result = |result, what, check| -> Result<()> {
        let s = str_result(result, what)?;
        if s != check {
            bail!("expected {what} to be {check}, found {s}");
        }

        Ok(())
    };

    let boot_loader_status_result = |result: &Result<Vec<u8>, IpcError>| -> Result<
        BOOT_LOADER_STATUS::CommandData,
    > {
        match result {
            &Err(err) => {
                bail!(
                    "failed to read boot loader status: {}",
                    i2c_read.strerror(err)
                );
            }
            &Ok(ref result) => {
                match BOOT_LOADER_STATUS::CommandData::from_slice(result) {
                    Some(command) => Ok(command),
                    None => {
                        bail!("failed to read boot loader status: {result:?}");
                    }
                }
            }
        }
    };

    let check_boot_loader = |result, mode| -> Result<()> {
        let bl = boot_loader_status_result(result)?;

        match bl.get_mode() {
            Some(m) if m == mode => Ok(()),
            Some(m) => {
                bail!("expected mode to be {mode:?}, found {m:?}");
            }
            None => {
                bail!("expected mode to be {mode:?}, found illegal mode");
            }
        }
    };

    //
    // First, confirm that we're talking to a Murata device, that the
    // revision is what we expect
    //
    let mut ops = base.clone();

    ops.push(Op::Push(CommandCode::MFR_ID as u8));
    ops.push(Op::PushNone);
    ops.push(Op::Call(i2c_read.id));
    ops.push(Op::DropN(2));

    ops.push(Op::Push(CommandCode::MFR_MODEL as u8));
    ops.push(Op::PushNone);
    ops.push(Op::Call(i2c_read.id));
    ops.push(Op::DropN(2));

    ops.push(Op::Push(CommandCode::MFR_REVISION as u8));
    ops.push(Op::PushNone);
    ops.push(Op::Call(i2c_read.id));
    ops.push(Op::DropN(2));

    ops.push(Op::Done);
    let results = context.run(core, ops.as_slice(), None)?;

    check_str_result(&results[0], "MFR_ID", MWOCP68_MFR_ID)?;
    check_str_result(&results[1], "MFR_MODEL", MWOCP68_MFR_MODEL)?;
    let model = str_result(&results[2], "MFR_MODEL")?;

    let bytes = if let Some(filename) = subargs.flash {
        fs::read(filename)?
    } else {
        bail!("expected a command");
    };

    //
    // The sequence here is:  write the
    //
    msg!("writing boot loader key");

    let key = MWOCP68_BOOT_LOADER_KEY;
    let mut ops = base.clone();
    ops.push(Op::Push(CommandCode::BOOT_LOADER_KEY as u8));
    ops.push(Op::Push(key.len() as u8));

    for c in key.chars() {
        ops.push(Op::Push(c as u8));
    }

    ops.push(Op::Push32((key.len() + 1) as u32));
    ops.push(Op::Call(i2c_write.id));
    ops.push(Op::DropN((key.len() + 3) as u8));

    ops.push(Op::Push(CommandCode::BOOT_LOADER_STATUS as u8));
    ops.push(Op::PushNone);
    ops.push(Op::Call(i2c_read.id));

    ops.push(Op::Done);

    let results = context.run(core, ops.as_slice(), None)?;
    println!("{:?}", results);

    //
    // We expect to NOT be in boot loader mode.
    //
    check_boot_loader(&results[1], BOOT_LOADER_STATUS::Mode::NotBootLoader)?;

    msg!("sleeping for {MWOCP68_KEY_DELAY} seconds...");
    thread::sleep(std::time::Duration::from_secs(MWOCP68_KEY_DELAY));

    //
    // Now write the product key.  Goofily, this is NOT a block write, but
    // rather a raw 16-byte write.
    //
    msg!("writing product key");

    let key = MWOCP68_PRODUCT_KEY;
    let mut ops = base.clone();
    ops.push(Op::Push(CommandCode::BOOT_LOADER_PRODUCT_KEY as u8));

    for c in key.chars() {
        ops.push(Op::Push(c as u8));
    }

    ops.push(Op::Push32(key.len() as u32));
    ops.push(Op::Call(i2c_write.id));
    ops.push(Op::DropN((key.len() + 2) as u8));

    ops.push(Op::Push(CommandCode::BOOT_LOADER_STATUS as u8));
    ops.push(Op::PushNone);
    ops.push(Op::Call(i2c_read.id));

    ops.push(Op::Done);

    let results = context.run(core, ops.as_slice(), None)?;
    println!("{:?}", results);

    check_boot_loader(&results[1], BOOT_LOADER_STATUS::Mode::NotBootLoader)?;

    msg!("sleeping for {MWOCP68_KEY_DELAY} seconds...");
    thread::sleep(std::time::Duration::from_secs(MWOCP68_KEY_DELAY));

    //
    // And now we can boot into the primary bootloader.
    //
    msg!("booting into primary boot loader");
    let mut ops = base.clone();
    ops.push(Op::Push(CommandCode::BOOT_LOADER_STATUS as u8));
    ops.push(Op::Push(1));
    ops.push(Op::Push(0x12));
    ops.push(Op::Push32(2));
    ops.push(Op::Call(i2c_write.id));

    ops.push(Op::Done);

    let results = context.run(core, ops.as_slice(), None)?;
    println!("{results:x?}");

    msg!("sleeping for {MWOCP68_BOOT_DELAY} seconds...");
    thread::sleep(std::time::Duration::from_secs(MWOCP68_BOOT_DELAY));

    let mut ops = base.clone();
    ops.push(Op::Push(CommandCode::BOOT_LOADER_STATUS as u8));
    ops.push(Op::PushNone);
    ops.push(Op::Call(i2c_read.id));
    ops.push(Op::Done);

    let results = context.run(core, ops.as_slice(), None)?;
    check_boot_loader(&results[0], BOOT_LOADER_STATUS::Mode::BootLoader)?;

    msg!("indicating write start...");
    let mut ops = base.clone();
    ops.push(Op::Push(CommandCode::BOOT_LOADER_STATUS as u8));
    ops.push(Op::Push(1));
    ops.push(Op::Push(0x1));
    ops.push(Op::Push32(2));
    ops.push(Op::Call(i2c_write.id));

    let results = context.run(core, ops.as_slice(), None)?;
    println!("{results:x?}");

    let start = SystemTime::now();

    msg!("sleeping for {MWOCP68_RESET_DELAY} seconds...");
    thread::sleep(std::time::Duration::from_secs(MWOCP68_RESET_DELAY));

    let mut offs = 0;
    let mut cksum = 0u32;

    if bytes.len() % MWOCP68_BLOCK_LENGTH != 0 {
        bail!(
            "image size ({}) is not a multiple of {MWOCP68_BLOCK_LENGTH}",
            bytes.len()
        );
    }

    msg!("writing");

    let chunksize = 10;

    while offs < bytes.len() {
        let mut data = vec![];
        let mut ops = base.clone();
        let mut nblocks = 0;

        ops.push(Op::Push(CommandCode::BOOT_LOADER_STATUS as u8));
        ops.push(Op::PushNone);
        ops.push(Op::Call(i2c_read.id));
        ops.push(Op::DropN(2));

        //
        // Our register must be None for bulk writes
        //
        ops.push(Op::PushNone);

        while offs < bytes.len() && nblocks < chunksize {
            let doffs = data.len();

            data.push(CommandCode::BOOT_LOADER_MEMORY_BLOCK as u8);

            for i in 0..MWOCP68_BLOCK_LENGTH {
                cksum += bytes[offs + i] as u32;
                data.push(bytes[offs + i]);
            }

            offs += MWOCP68_BLOCK_LENGTH;
            nblocks += 1;

            ops.push(Op::Push32(doffs as u32));
            ops.push(Op::Push16((MWOCP68_BLOCK_LENGTH + 1) as u16));
            ops.push(Op::Call(i2c_bulk_write.id));
            ops.push(Op::DropN(2));

            ops.push(Op::Push(MWOCP68_BLOCK_DELAY_MS));
            ops.push(Op::Call(sleep.id));
            ops.push(Op::Drop);

            ops.push(Op::Drop);
            ops.push(Op::Push(CommandCode::STATUS_CML as u8));
            ops.push(Op::Push(1));
            ops.push(Op::Call(i2c_read.id));
            ops.push(Op::DropN(2));
            ops.push(Op::PushNone);
        }

        ops.push(Op::Done);

        //
        // Away it goes!
        //
        let results = context.run(core, ops.as_slice(), Some(&data))?;
        println!("{:?} {results:x?}", SystemTime::now().duration_since(start));

        for r in results {
            if let Err(_) = r {
                bail!("failed!");
            }
        }

        println!("wrote {offs} of {}", bytes.len());
    }

    //
    // Now send the checksum.
    //
    msg!("sending checksum (0x{:08x})", cksum);
    let mut ops = base.clone();
    ops.push(Op::Push(CommandCode::IMAGE_CHECKSUM as u8));
    ops.push(Op::Push(2));
    ops.push(Op::Push((cksum & 0xff) as u8));
    ops.push(Op::Push(((cksum >> 8) & 0xff) as u8));

    ops.push(Op::Push32(3));
    ops.push(Op::Call(i2c_write.id));
    ops.push(Op::Done);
    let results = context.run(core, ops.as_slice(), None)?;
    println!("{:?} {results:x?}", SystemTime::now().duration_since(start));
    let check = SystemTime::now();

    msg!("sleeping {MWOCP68_CHECKSUM_DELAY} seconds");
    thread::sleep(std::time::Duration::from_secs(MWOCP68_CHECKSUM_DELAY));

    let mut ops = base.clone();
    ops.push(Op::Push(CommandCode::BOOT_LOADER_STATUS as u8));
    ops.push(Op::PushNone);
    ops.push(Op::Call(i2c_read.id));
    ops.push(Op::Done);
    let results = context.run(core, ops.as_slice(), None)?;

    let status = boot_loader_status_result(&results[0])?;

    match status.get_checksum_successful() {
        Some(BOOT_LOADER_STATUS::ChecksumSuccessful::Successful) => {
            msg!("checksum successful!");
        }
        Some(BOOT_LOADER_STATUS::ChecksumSuccessful::NotSuccessful) => {
            bail!("checksum was not successful!");
        }
        None => panic!(),
    }

    msg!("resetting PSU...");

    let mut ops = base.clone();
    ops.push(Op::Push(CommandCode::BOOT_LOADER_STATUS as u8));
    ops.push(Op::Push(1));
    ops.push(Op::Push(0x3));
    ops.push(Op::Push32(2));
    ops.push(Op::Call(i2c_write.id));

    let results = context.run(core, ops.as_slice(), None)?;
    println!("{results:x?}");

    msg!("sleeping {MWOCP68_REBOOT_DELAY} seconds");
    thread::sleep(std::time::Duration::from_secs(MWOCP68_REBOOT_DELAY));

    let mut ops = base.clone();
    ops.push(Op::Push(CommandCode::BOOT_LOADER_STATUS as u8));
    ops.push(Op::PushNone);
    ops.push(Op::Call(i2c_read.id));
    ops.push(Op::Done);

    let results = context.run(core, ops.as_slice(), None)?;
    check_boot_loader(&results[0], BOOT_LOADER_STATUS::Mode::NotBootLoader)?;

    Ok(())
}

pub fn init() -> Command {
    Command {
        app: MwocpArgs::command(),
        name: "mwocp",
        run: mwocp,
        kind: CommandKind::Attached {
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
        },
    }
}
