// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use humility::core::Core;
use humility::hubris::*;
use humility_cmd::hiffy::*;
use humility_cmd::printmem;
use humility_cmd::{Archive, Args, Attach, Command, Validate};

use std::convert::TryInto;
use std::str;

use anyhow::{bail, Result};
use hif::*;
use structopt::clap::App;
use structopt::StructOpt;

#[macro_use]
extern crate log;

#[derive(StructOpt, Debug)]
#[structopt(name = "spi", about = "SPI reading and writing")]
struct SpiArgs {
    /// sets timeout
    #[structopt(
        long, short = "T", default_value = "5000", value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// SPI peripheral on which to operate
    #[structopt(long, short, value_name = "peripheral")]
    peripheral: Option<u8>,

    /// comma-separated bytes to write
    #[structopt(long, short, value_name = "bytes")]
    write: Option<String>,

    /// perform a read
    #[structopt(long, short, requires = "nbytes")]
    read: bool,

    /// specify number of bytes to read
    #[structopt(long, short, value_name = "nbytes",
        parse(try_from_str = parse_int::parse),
    )]
    nbytes: Option<usize>,

    /// print out data read as words rather than bytes
    #[structopt(long, short = "W", requires = "read")]
    word: bool,

    /// interpret the specified number of trailing bytes on a write as a
    /// bigendian address
    #[structopt(
        long, short = "A", requires_all = &["read", "write", "discard"]
    )]
    bigendian_address: Option<usize>,

    /// interpret the specified number of trailing bytes on a write as a
    /// bigendian address
    #[structopt(
        long, short = "a", requires_all = &["read", "write", "discard"],
        conflicts_with = "bigendian_address"
    )]
    littleendian_address: Option<usize>,

    /// number of bytes to discard when printing read result
    #[structopt(long, short, value_name = "nbytes", requires = "read")]
    discard: Option<usize>,
}

/// Looks up which Hubris task is associated with SPI (accepting a peripheral
/// hint to disambiguate).
pub fn spi_task(
    hubris: &HubrisArchive,
    peripheral: Option<u8>
) -> Result<HubrisTask> {
    let lookup = |peripheral| {
        let spi = format!("spi{}", peripheral);
        let tasks = hubris.lookup_feature(&spi)?;

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

fn spi(
    hubris: &mut HubrisArchive,
    core: &mut dyn Core,
    _args: &Args,
    subargs: &Vec<String>,
) -> Result<()> {
    let subargs = SpiArgs::from_iter_safe(subargs)?;
    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;
    let funcs = context.functions()?;

    let spi_read = funcs.get("SpiRead", 3)?;
    let spi_write = funcs.get("SpiWrite", 2)?;

    let task = spi_task(hubris, subargs.peripheral)?;
    let mut ops = vec![];

    if let HubrisTask::Task(task) = task {
        ops.push(Op::Push32(task));
    } else {
        bail!("SPI task cannot be the kernel");
    }

    info!("SPI master is {}", hubris.lookup_module(task)?.name);

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

            let size = if subargs.word { 4 } else { 1 };
            printmem(&results[discard..], addr, size, 16);
            return Ok(());
        }
    }

    println!("{:x?}", results);

    Ok(())
}

pub fn init<'a, 'b>() -> (Command, App<'a, 'b>) {
    (
        Command::Attached {
            name: "spi",
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
            run: spi,
        },
        SpiArgs::clap(),
    )
}
