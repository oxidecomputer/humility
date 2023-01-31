// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility spi`
//!
//! `humility spi` can be used to read or write to attached SPI devices,
//! (as defined in the application TOML).
//!
//! The SPI peripheral (block) to be used can be specified with the
//! `--peripheral` (`-p`) option.  This should be a number that matches SPI
//! peripheral number; if it is not specified (and there is only one
//! SPI-controlling task found), the peripheral associated with that task will
//! be assumed.
//!
//! Because of the full duplex nature of SPI, bytes will always be written
//! *and* read; to actually write specific bytes, the bytes to be written
//! should be specified via `--write` (`-w`).  To report bytes read back,
//! `--read` (`-r`) should be specified, along with the number of bytes via
//! `--nbytes` (`-n`).
//!
//! For example, to write the byte sequence `0x1`, `0x0`, `0x0` and then read
//! 32 bytes (discarding the first three) from device 0 on SPI2:
//!
//! ```console
//! % humility spi -p 2 --nbytes 32 --write 0x1,0x0,0x0 --read --discard 3
//! humility: attached to 0483:374e:003C00174741500520383733 via ST-Link V3
//! humility: SPI master is spi2_driver
//!              \/  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f
//! 0x00000000 | 01 de aa 55 00 00 00 ff 06 00 00 00 1c 01 0b 00 | ...U............
//! 0x00000010 | 00 00 00 00 ff ff ff 06 12 00 00 00 06          | .............
//! ```
//!

use humility::cli::Subcommand;
use humility::hubris::*;
use humility_cmd::{hiffy::*, CommandKind};
use humility_cmd::{Archive, Attach, Command, Dumper, Validate};

use std::convert::TryInto;
use std::str;

use anyhow::{bail, Result};
use clap::{CommandFactory, Parser};
use hif::*;

#[derive(Parser, Debug)]
#[clap(name = "spi", about = env!("CARGO_PKG_DESCRIPTION"))]
struct SpiArgs {
    /// sets timeout
    #[clap(
        long, short = 'T', default_value_t = 5000, value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// SPI peripheral on which to operate
    #[clap(long, short, value_name = "peripheral")]
    peripheral: Option<u8>,

    /// comma-separated bytes to write
    #[clap(long, short, value_name = "bytes")]
    write: Option<String>,

    /// perform a read
    #[clap(long, short, requires = "nbytes")]
    read: bool,

    /// specify number of bytes to read
    #[clap(long, short, value_name = "nbytes",
        parse(try_from_str = parse_int::parse),
    )]
    nbytes: Option<usize>,

    /// print out data read as words rather than bytes
    #[clap(long, short = 'W', requires = "read")]
    word: bool,

    /// interpret the specified number of trailing bytes on a write as a
    /// bigendian address
    #[clap(
        long, short = 'A', requires_all = &["read", "write", "discard"],
        conflicts_with = "littleendian-address"
    )]
    bigendian_address: Option<usize>,

    /// interpret the specified number of trailing bytes on a write as a
    /// bigendian address
    #[clap(
        long, short = 'a', requires_all = &["read", "write", "discard"],
        conflicts_with = "bigendian-address"
    )]
    littleendian_address: Option<usize>,

    /// number of bytes to discard when printing read result
    #[clap(long, short, value_name = "nbytes", requires = "read")]
    discard: Option<usize>,

    /// device open which to operate
    #[clap(long, short = 'D', value_name = "device")]
    device: Option<String>,
}

/// Looks up which Hubris task is associated with SPI (accepting a peripheral
/// hint to disambiguate).
pub fn spi_task(
    hubris: &HubrisArchive,
    peripheral: Option<u8>,
) -> Result<HubrisTask> {
    let lookup = |peripheral| {
        let spi = format!("spi{}", peripheral);
        let tasks = hubris.lookup_feature(&spi)?;
        let tasks: Vec<HubrisTask> = tasks
            .into_iter()
            .filter(|t| {
                hubris
                    .lookup_module(*t)
                    .ok()
                    .and_then(|m| m.iface.as_ref())
                    .map(|iface| iface.name == "Spi")
                    .unwrap_or(false)
            })
            .collect();

        match tasks.len() {
            0 => Ok(None),
            1 => Ok(Some(tasks[0])),
            _ => {
                bail!("more than one task has {}", spi);
            }
        }
    };

    let task = if let Some(peripheral) = peripheral {
        match lookup(peripheral)? {
            Some(task) => task,
            None => {
                bail!("SPI peripheral {} not found", peripheral);
            }
        }
    } else {
        let mut found = vec![];

        for peripheral in 0..9 {
            if let Some(task) = lookup(peripheral)? {
                found.push((peripheral, task));
            }
        }

        if found.is_empty() {
            bail!("no SPI peripherals found")
        }

        if found.len() > 1 {
            bail!(
                "SPI peripheral must be specified; valid peripherals: {}",
                found
                    .iter()
                    .map(|v| v.0.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            );
        }

        found[0].1
    };
    if task == HubrisTask::Kernel {
        bail!("SPI task cannot be the kernel");
    }
    Ok(task)
}

fn spi(context: &mut humility::ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let hubris = context.archive.as_ref().unwrap();

    let subargs = SpiArgs::try_parse_from(subargs)?;
    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;
    let funcs = context.functions()?;

    let spi_read = funcs.get("SpiRead", 4)?;
    let spi_write = funcs.get("SpiWrite", 3)?;

    let task = spi_task(hubris, subargs.peripheral)?;
    let mut ops = vec![];

    if let HubrisTask::Task(task) = task {
        ops.push(Op::Push32(task));
    } else {
        bail!("SPI task cannot be the kernel");
    }

    if let Some(device) = subargs.device {
        if let Ok(device) = parse_int::parse::<u8>(&device) {
            ops.push(Op::Push(device));
        } else {
            bail!("illegal device {}", device);
        }
    } else {
        // Device 0
        ops.push(Op::Push(0));
    }

    humility::msg!("SPI master is {}", hubris.lookup_module(task)?.name);

    let mut addr = 0;

    let data = if let Some(ref write) = subargs.write {
        let bytes: Vec<&str> = write.split(',').collect();
        let mut arr = vec![];

        for byte in &bytes {
            if let Ok(val) = parse_int::parse::<u8>(byte) {
                arr.push(val);
            } else {
                bail!("invalid byte {}", byte)
            }
        }

        if let Some(size) = subargs.littleendian_address {
            let l = arr.len();

            if size > l {
                bail!("number of address bytes cannot exceed bytes to write");
            }

            addr = match size {
                1 => arr[l - size] as u32,
                2 => u16::from_le_bytes(arr[l - size..l].try_into().unwrap())
                    as u32,
                4 => u32::from_le_bytes(arr[l - size..l].try_into().unwrap())
                    as u32,
                _ => {
                    bail!("invalid address size");
                }
            };
        }

        if let Some(size) = subargs.bigendian_address {
            let l = arr.len();

            if size > l {
                bail!("number of address bytes cannot exceed bytes to write");
            }

            addr = match size {
                1 => arr[l - size] as u32,
                2 => u16::from_be_bytes(arr[l - size..l].try_into().unwrap())
                    as u32,
                4 => u32::from_be_bytes(arr[l - size..l].try_into().unwrap())
                    as u32,
                _ => {
                    bail!("invalid address size");
                }
            };
        }

        ops.push(Op::Push32(arr.len() as u32));
        Some(arr)
    } else {
        if !subargs.read {
            bail!("must specify read or write");
        }

        ops.push(Op::Push(0));
        None
    };

    let discard = if let Some(discard) = subargs.discard {
        if discard > subargs.nbytes.unwrap() {
            bail!("cannot discard more than specified number of bytes");
        }
        discard
    } else {
        0
    };

    if subargs.read {
        ops.push(Op::Push32(subargs.nbytes.unwrap() as u32));
        ops.push(Op::Call(spi_read.id));
    } else {
        ops.push(Op::Call(spi_write.id));
    }

    ops.push(Op::Done);

    let results = context.run(
        core,
        ops.as_slice(),
        match data {
            Some(ref data) => Some(data.as_slice()),
            _ => None,
        },
    )?;

    if subargs.read {
        if let Ok(results) = &results[0] {
            if results.len() < discard {
                bail!("short read: {:x?}", results);
            }

            let mut dumper = Dumper::new();
            dumper.size = if subargs.word { 4 } else { 1 };
            dumper.dump(&results[discard..], addr);

            return Ok(());
        }
    }

    println!("{:x?}", results);

    Ok(())
}

pub fn init() -> Command {
    Command {
        app: SpiArgs::command(),
        name: "spi",
        run: spi,
        kind: CommandKind::Attached {
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
        },
    }
}
