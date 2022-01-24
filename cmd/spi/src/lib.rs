// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility spi`
//!
//! On platforms that have SPI support, `humility spi` can be used to read
//! from a device and/or write to a device.  It can also be used to program
//! SPI EEPROMs (assuming 3-byte addressing).
//!
//! On platforms with more than one SPI peripheral, the specific peripheral
//! must be specified with `--peripheral`, e.g. `--peripheral 3` for SPI 3.
//! (Note that while Hubris supports multiple devices connected to a SPI
//! peripheral, `humility spi` always operates on the device indexed at 0.)
//!
//! To read from the specified device, specify the `--read` option along
//! with `--nbytes` to specify the number of bytes; to write, specify the
//! `--write` option followed by comma-delimited values. These options may
//! be (and often are) combined; for example, to write the sequence `0x03`,
//! `0x00`, `0x01`, `0xde` and read 32 bytes:
//!
//! ```console
//! % humility spi --write 0x3,0x0,0x1,0xde --read --nbytes 32
//! humility: attached via ST-Link
//! humility: SPI master is spi_driver
//!              \/  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f
//! 0x00000000 | 00 00 00 00 6f 78 79 3a 78 3a 31 33 3a 31 33 3a | ....oxy:x:13:13:
//! 0x00000010 | 70 72 6f 78 79 3a 2f 62 69 6e 3a 2f 75 73 72 2f | proxy:/bin:/usr/
//! ```
//!
//! Devices that are responding to a read based on the value written to them
//! will often have some number of dummy bytes that should be discarded; to
//! specify this, use the `--discard` option.  For example, to dicard the
//! first 4 bytes returned from the read:
//!
//! ```console
//! % humility spi --write 0x3,0x0,0x1,0xde --read --nbytes 32 --discard 4
//! humility: attached via ST-Link
//! humility: SPI master is spi_driver
//!              \/  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f
//! 0x00000000 | 6f 78 79 3a 78 3a 31 33 3a 31 33 3a 70 72 6f 78 | oxy:x:13:13:prox
//! 0x00000010 | 79 3a 2f 62 69 6e 3a 2f 75 73 72 2f             | y:/bin:/usr/
//! ```
//!
//! Many devices have a notion of a command that writes some number of bytes
//! of address; it can be helpful to interpret this as such and display the
//! address with the output.  This is done by specifying the number of
//! address bytes to the `--littleendian-address` option (for little-endian
//! address interpretation) or the `--bigendian-address` option (for
//! big-endian address interpretation).  For example, to treat the three
//! trailing bytes as a big-endian address:
//!
//! ```console
//! % humility spi --write 0x3,0x0,0x1,0xde --read --nbytes 36 --discard 4 \
//!   --bigendian-address 3
//! humility: attached via ST-Link
//! humility: SPI master is spi_driver
//!               0  1  2  3  4  5  6  7  8  9  a  b  c  d \/  f
//! 0x000001d0 |                                           6f 78 |               ox
//! 0x000001e0 | 79 3a 78 3a 31 33 3a 31 33 3a 70 72 6f 78 79 3a | y:x:13:13:proxy:
//! 0x000001f0 | 2f 62 69 6e 3a 2f 75 73 72 2f 73 62 69 6e       | /bin:/usr/sbin
//! ```
//!
//! To treat the SPI device as a SPI EEPROM and flash it with a specified
//! file, use the `--flash` option:
//!
//! ```console
//! % humility spi --flash /path/to/file/to/flash
//! ```
//!
//! To treat the SPI device as a SPI EEPROM and read its contents, use the
//! `--readflash` option, which will write its contents to the standard
//! output.

use humility::core::Core;
use humility::hubris::*;
use humility_cmd::hiffy::*;
use humility_cmd::{Archive, Args, Attach, Command, Dumper, Validate};

use std::convert::TryInto;
use std::str;

use anyhow::{bail, Result};
use hif::*;
use std::fs;
use std::fs::File;
use std::io::{Read, Write};
use std::thread;
use std::time::{Duration, Instant};
use structopt::clap::App;
use structopt::StructOpt;

use indicatif::{HumanBytes, HumanDuration};
use indicatif::{ProgressBar, ProgressStyle};

#[macro_use]
extern crate log;

#[derive(StructOpt, Debug)]
#[structopt(name = "spi", about = env!("CARGO_PKG_DESCRIPTION"))]
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

    /// flash the specified file, assuming three byte addressing
    #[structopt(long, short,
        conflicts_with_all = &[
            "write", "read", "nbytes", "word", "discard"
        ],
        value_name = "filename",
    )]
    flash: Option<String>,

    /// read as flash, writing to stdout
    #[structopt(long, short = "R",
        conflicts_with_all = &[
            "flash", "write", "read", "word", "discard"
        ],
        requires = "nbytes",
    )]
    readflash: bool,
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

fn spiflash_cmd(
    core: &mut dyn Core,
    context: &mut HiffyContext,
    task: u32,
    spi_read: &HiffyFunction,
    cmd: u8,
) -> Result<u8> {
    let mut ops = vec![];
    let data = [cmd];

    ops.push(Op::Push32(task));
    ops.push(Op::Push(0)); // Device index 0
    ops.push(Op::Push(0)); // Offset 0
    ops.push(Op::Push32(data.len() as u32));
    ops.push(Op::Push(2));
    ops.push(Op::Call(spi_read.id));
    ops.push(Op::Done);

    let results = context.run(core, ops.as_slice(), Some(data.as_slice()))?;

    if let Ok(results) = &results[0] {
        if results.len() != 2 {
            bail!("bad read on command 0x{:x}: {:x?}", cmd, results);
        }

        Ok(results[1])
    } else {
        bail!("failed to read command 0x{:x}: {:?}", cmd, results);
    }
}

fn spiflash(
    core: &mut dyn Core,
    subargs: &SpiArgs,
    context: &mut HiffyContext,
    spi_read: &HiffyFunction,
    spi_write: &HiffyFunction,
    sleep: &HiffyFunction,
    task: &HubrisTask,
) -> Result<()> {
    // We are going to make some assumptions here:
    //
    // - That there is a write enable command as 0x6
    // - That there is a write command as 0x2
    // - That there is a status command as 0x5
    // - That the low bit of the status command indicates write busy
    // - That there might be a chip erase command as 0xc7
    // - That addressing is 3-byte, big-endian
    // - That we can write at least 16 bytes on a write
    // - That each write can take up to 10 ms
    //
    // Clearly, if any of these is wrong, wildly wrong behavior will result --
    // and if it needs to be said, this should really only be used in a pinch.
    // This -- by design -- does *not* perform well; it is optimized for
    // programming a small amount of data correctly.

    const SPIFLASH_WREN: u8 = 0x6;
    const SPIFLASH_WRITE: u8 = 0x2;
    const SPIFLASH_STATUS: u8 = 0x5;
    const SPIFLASH_ERASE: u8 = 0xc7;

    const SPIFLASH_STATUS_BUSY: u8 = 0x1;
    const SPIFLASH_ADDRSIZE: usize = 3;

    let task = match task {
        HubrisTask::Task(task) => *task,
        _ => {
            panic!("cannot pass a kernel task");
        }
    };

    //
    // Read status and determine if we're busy.  If the chip is busy, something
    // is wrong.
    //
    let s0 = spiflash_cmd(core, context, task, spi_read, SPIFLASH_STATUS)?;

    if s0 & SPIFLASH_STATUS_BUSY != 0 {
        bail!(
            "status command (0x{:x}) yielded busy status (0x{:x}",
            SPIFLASH_STATUS,
            s0
        );
    }

    let _ = spiflash_cmd(core, context, task, spi_read, SPIFLASH_WREN)?;
    let _ = spiflash_cmd(core, context, task, spi_read, SPIFLASH_ERASE)?;

    info!("erasing chip...");

    loop {
        let s = spiflash_cmd(core, context, task, spi_read, SPIFLASH_STATUS)?;

        if s & SPIFLASH_STATUS_BUSY == 0 {
            break;
        }

        thread::sleep(Duration::from_millis(100));
    }

    info!("... done");

    let mut file = File::open(subargs.flash.as_ref().unwrap())?;

    let filelen = {
        let len = fs::metadata(subargs.flash.as_ref().unwrap())?.len();
        let max = (1 << (SPIFLASH_ADDRSIZE * 8)) - 1;

        if len > max {
            info!("file will be clamped at {}", max);
            max as usize
        } else {
            len as usize
        }
    };

    let block_size = 128;
    let addr_size = SPIFLASH_ADDRSIZE;
    let cmd_size = 1;
    let wren_size = 1;

    let nibble_size = block_size + addr_size + cmd_size;

    // We take a byte off the end of our data size for our WREN command
    let data_size = context.data_size() - wren_size;

    let mut chunk: usize = data_size - (data_size % nibble_size as usize);
    let mut offset = 0usize;

    let mut buf = vec![0u8; chunk + wren_size];
    let wren_offs = buf.len() - wren_size;
    buf[wren_offs] = SPIFLASH_WREN;

    let mut last = false;

    let started = Instant::now();
    let bar = ProgressBar::new(filelen as u64);
    bar.set_style(
        ProgressStyle::default_bar()
            .template("humility: flashing [{bar:30}] {bytes}/{total_bytes}"),
    );

    loop {
        let mut noffs = 0usize;

        loop {
            assert!(offset < filelen);

            if noffs + nibble_size as usize > chunk {
                //
                // No more room; we have filled up our chunk.
                //
                break;
            }

            let len = if offset + block_size > filelen {
                filelen - offset
            } else {
                block_size
            };

            // Plop in our address, 24-bit big endian
            let b = (offset as u32).to_be_bytes();
            buf[noffs] = SPIFLASH_WRITE;
            buf[noffs + 1] = b[1];
            buf[noffs + 2] = b[2];
            buf[noffs + 3] = b[3];
            noffs += 4;

            // Read the file contents
            file.read_exact(&mut buf[noffs..noffs + len as usize])?;
            noffs += len as usize;

            if offset + len >= filelen {
                //
                // We are at the end of our file; clamp our chunk size at
                // the amount of nibbles we actually took to prevent any
                // spurious writes
                //
                chunk = noffs;
                last = true;
                break;
            }

            offset += len;
        }

        let ops = vec![
            Op::Push32(task),
            Op::Push(0),
            Op::Push32(0),
            Op::PushNone,
            Op::Label(Target(0)),
            Op::Drop,
            //
            // WREN
            //
            Op::Push32(task),
            Op::Push(0),
            Op::Push32((buf.len() - wren_size) as u32),
            Op::Push(wren_size as u8),
            Op::Call(spi_write.id),
            Op::DropN(4),
            //
            // And now the actual write -- offset is already pushed
            //
            Op::Push16(nibble_size as u16),
            Op::Call(spi_write.id),
            Op::Push(5),
            Op::Call(sleep.id),
            Op::Drop,
            Op::Add,
            Op::Push32(chunk as u32),
            Op::BranchGreaterThan(Target(0)),
            Op::Done,
        ];

        let results = context.run(core, ops.as_slice(), Some(&buf))?;

        bar.set_position(offset as u64);

        for (i, item) in results.into_iter().enumerate() {
            if let Err(err) = item {
                if i % 2 == 0 {
                    bail!(
                        "failed to write enable block {}: {}",
                        i / 2,
                        spi_write.strerror(err)
                    );
                } else {
                    bail!(
                        "failed to write block {} at offset {}: {}",
                        i / 2,
                        offset,
                        spi_write.strerror(err)
                    );
                }
            }
        }

        if offset >= filelen || last {
            break;
        }
    }

    bar.finish_and_clear();

    info!(
        "flashed {} in {}",
        HumanBytes(filelen as u64),
        HumanDuration(started.elapsed())
    );

    Ok(())
}

fn spiflash_read(
    core: &mut dyn Core,
    subargs: &SpiArgs,
    context: &mut HiffyContext,
    spi_read: &HiffyFunction,
    task: &HubrisTask,
) -> Result<()> {
    let nbytes = match subargs.nbytes {
        Some(nbytes) => nbytes,
        None => panic!("expected nbytes to be set"),
    };

    let task = match task {
        HubrisTask::Task(task) => *task,
        _ => {
            panic!("cannot pass a kernel task");
        }
    };

    //
    // As on the write side, we're going to make some assumptions here:
    //
    // - That we have a read command as 0x3
    // - That addressing is 3-byte, big-endian
    // - That there are no additional dummy cycles other than that for
    //   addressing + command
    //
    const SPIFLASH_READ: u8 = 3;
    const SPIFLASH_ADDRSIZE: usize = 3;
    const SPIFLASH_DUMMY: usize = SPIFLASH_ADDRSIZE + 1;

    let block_size = 8;
    let nblocks = context.return_size() / (block_size + 32);

    let mut buf = vec![0u8; nblocks * (SPIFLASH_ADDRSIZE + 1)];
    let mut offset = 0;
    let mut last = false;

    loop {
        let mut noffs = 0usize;
        let mut block = 0;
        let mut ops = vec![];
        let mut addr = vec![];
        ops.push(Op::Push32(task));

        // For now, always device index 0
        ops.push(Op::Push(0));

        loop {
            assert!(offset < nbytes);

            if block == nblocks {
                //
                // We're full!
                //
                break;
            }

            let len = if offset + block_size > nbytes {
                nbytes - offset
            } else {
                block_size
            };

            // Plop in our address, 24-bit big endian
            let b = (offset as u32).to_be_bytes();
            buf[noffs] = SPIFLASH_READ;
            buf[noffs + 1] = b[1];
            buf[noffs + 2] = b[2];
            buf[noffs + 3] = b[3];
            addr.push(offset as u32);

            // Push our write details
            ops.push(Op::Push32(noffs as u32));
            ops.push(Op::Push(4));

            // Push our read size and then call
            ops.push(Op::Push32((len + 4) as u32));
            ops.push(Op::Call(spi_read.id));
            ops.push(Op::DropN(3));

            if offset + len >= nbytes {
                last = true;
                break;
            }

            noffs += 4;
            offset += len;
            block += 1;
        }

        ops.push(Op::Done);

        let results = context.run(core, ops.as_slice(), Some(&buf))?;

        for (i, result) in results.into_iter().enumerate() {
            match result {
                Err(err) => {
                    bail!(
                        "failed to read block at 0x{:x}: {}",
                        addr[i],
                        spi_read.strerror(err)
                    );
                }
                Ok(rval) => {
                    std::io::stdout().write_all(&rval[SPIFLASH_DUMMY..])?;
                }
            }
        }

        if offset >= nbytes || last {
            break;
        }
    }

    Ok(())
}

fn spi(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    _args: &Args,
    subargs: &[String],
) -> Result<()> {
    let subargs = SpiArgs::from_iter_safe(subargs)?;
    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;
    let funcs = context.functions()?;

    let spi_read = funcs.get("SpiRead", 5)?;
    let spi_write = funcs.get("SpiWrite", 4)?;

    let task = spi_task(hubris, subargs.peripheral)?;
    let mut ops = vec![];

    if let HubrisTask::Task(task) = task {
        ops.push(Op::Push32(task));
    } else {
        bail!("SPI task cannot be the kernel");
    }

    info!("SPI master is {}", hubris.lookup_module(task)?.name);

    // For now, always device index 0
    ops.push(Op::Push(0));

    if subargs.flash.is_some() {
        spiflash(
            core,
            &subargs,
            &mut context,
            spi_read,
            spi_write,
            funcs.get("Sleep", 1)?,
            &task,
        )?;

        return Ok(());
    }

    if subargs.readflash {
        spiflash_read(core, &subargs, &mut context, spi_read, &task)?;
        return Ok(());
    }

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

            let b = l - size;

            addr = match size {
                1 => arr[b] as u32,
                2 => u16::from_le_bytes(arr[b..l].try_into().unwrap()) as u32,
                3 => u32::from_le_bytes([arr[b], arr[b + 1], arr[b + 2], 0]),
                4 => u32::from_le_bytes(arr[b..l].try_into().unwrap()) as u32,
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

            let b = l - size;

            addr = match size {
                1 => arr[b] as u32,
                2 => u16::from_be_bytes(arr[b..l].try_into().unwrap()) as u32,
                3 => u32::from_be_bytes([0, arr[b], arr[b + 1], arr[b + 2]]),
                4 => u32::from_be_bytes(arr[b..l].try_into().unwrap()) as u32,
                _ => {
                    bail!("invalid address size");
                }
            };
        }

        ops.push(Op::Push(0));
        ops.push(Op::Push32(arr.len() as u32));
        Some(arr)
    } else {
        if !subargs.read {
            bail!("must specify read or write");
        }

        ops.push(Op::Push(0));
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
