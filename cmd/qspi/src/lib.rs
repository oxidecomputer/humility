// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility qspi`
//!
//! `humility qspi` manipulates (and importantly, writes to) QSPI-attached
//! flash in Hubris.  To read the device identifier, use the `--id` (`-i`)
//! option:
//!
//! ```console
//! % humility qspi -i
//! humility: attached via ST-Link V3
//! DeviceIdData {
//!     manufacturer_id: Micron(0x20)
//!     memory_type: 3V(186)
//!     memory_capacity: 33554432
//!     uid_n: 16
//!     # If a Micron device:
//!     ext_device_id: 0b1000100
//!         2nd device generation,
//!         Standard BP scheme,
//!         HOLD#/RESET#=HOLD
//!         Additional HW RESET# is available,
//!         Sector size is Uniform 64KB,
//!     device_configuration_info: 0
//!     uid: [9a, ec, 0b, 00, 19, f9, ff$, 39, 00, be, 69, 97, f4, a2]
//! }
//! [Ok([20, ba, 19, 10, 44, 0, 9a, ec, b, 0, 19, f9, ff, 39, 0, be, 69, 97, f4, a2])]
//! ```
//!
//! To write an image from a file, use the `--writefile` (`-W`) option:
//!
//! ```console
//! % humility -W ./milan-spew-115k2-2dpc-0.4.1-dataeye.bin
//! humility: attached via ST-Link V3
//! humility: erasing 16777216 bytes...
//! humility: ... done
//! humility: flashed 16.00MB in 5 minutes
//! ```
//!
//! If writing similar images, it is much faster to write only those blocks
//! that differ.  To perform a differential write, use the `--diffwrite` (`-D`)
//! option:
//!
//! ```console
//! % humility qspi -D ./milan-spew-115k2-2dpc-0.4.1.bin
//! humility: attached via ST-Link V3
//! humility: erasing 65536 bytes...
//! humility: ... done
//! humility: hashed 16.00MB, wrote 64.00KB in 16 seconds
//! ```
//!
//! To read, write or hash a particular region, use the `--read` (`-r`),
//! `--write` (`-w`), or `--hash` (`-H`) respectively -- giving the address
//! via `--address` (`-a`) and the number of bytes via `--nbytes` (`-n`).
//! For example, to read 128 bytes from address 0x120000:
//!
//! ```console
//! % humility qspi -r -a 0x120000 -n 128
//!              \/  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f
//! 0x00120000 | 24 50 53 50 ee 7a 31 28 10 00 00 00 20 cd 48 20 | $PSP.z1(.... .H
//! 0x00120010 | 00 00 00 00 40 04 00 00 00 30 12 00 00 00 00 00 | ....@....0......
//! 0x00120020 | 01 00 00 00 00 54 01 00 00 40 12 00 00 00 00 00 | .....T...@......
//! 0x00120030 | 03 00 00 00 00 54 01 00 00 a0 13 00 00 00 00 00 | .....T..........
//! 0x00120040 | 08 00 00 00 40 ea 01 00 00 00 15 00 00 00 00 00 | ....@...........
//! 0x00120050 | 09 00 00 00 40 06 00 00 00 f0 16 00 00 00 00 00 | ....@...........
//! 0x00120060 | 0a 00 00 00 40 06 00 00 00 00 17 00 00 00 00 00 | ....@...........
//! 0x00120070 | 0b 00 00 00 ff ff ff ff 01 00 00 00 00 00 00 00 | ................
//! ```
//!
//! To get the SHA256 hash for that same region:
//!
//! ```console
//! % humility qspi -H -a 0x120000 -n 128
//! humility: attached via ST-Link V3
//! 120000..000080: 4d07112733efe240f990fad785726c52de4335d6c5c30a33e60096d4c2576742
//! ```
//!
//! By default, Hubris reserved sector 0 (the lowest 64 KiB) of QSPI for its own
//! internal bookkeeping.  To override this, use the `--write-sector0` flag;
//! this is required for erases and writes that would otherwise modify sector 0,
//! as well as bulk erase.

use cmd_hiffy as humility_cmd_hiffy;
use humility::cli::Subcommand;
use humility::core::Core;
use humility_cmd::idol::{HubrisIdol, IdolArgument};
use humility_cmd::CommandKind;
use humility_cmd::{hiffy::*, Archive, Attach, Command, Dumper, Validate};
use sha2::{Digest, Sha256};
use std::fmt;
use std::fs;
use std::fs::File;
use std::io::{BufWriter, Read, Write};
use std::mem;
use std::time::Instant;

use anyhow::{anyhow, bail, Result};
use clap::{ArgGroup, CommandFactory, Parser};
use hif::*;

use indicatif::{HumanBytes, HumanDuration};
use indicatif::{ProgressBar, ProgressStyle};

#[derive(Parser, Debug)]
#[clap(
    name = "qspi", about = env!("CARGO_PKG_DESCRIPTION"),
    group = ArgGroup::new("command").multiple(false)
)]
struct QspiArgs {
    /// sets timeout
    #[clap(
        long, short = 'T', default_value_t = 30000, value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// pull status string
    #[clap(long, short, group = "command")]
    status: bool,

    /// pull identifier
    #[clap(long, short, group = "command")]
    id: bool,

    /// Return the hash of a region of the flash
    /// -a and -n are optional. Default is the entire contents.
    #[clap(long, short = 'H', group = "command")]
    hash: bool,

    /// perform a sector erase
    #[clap(
        long, short,
        group = "command",
        requires_all = &["addr"]
    )]
    erase: bool,

    /// perform a bulk erase (requires --write-sector0)
    #[clap(long, short = 'E', group = "command")]
    bulkerase: bool,

    /// perform a read
    #[clap(
        long, short, group = "command", requires_all = &["addr", "nbytes"]
    )]
    read: bool,

    /// specify flash address in bytes
    #[clap(long, short, value_name = "address",
        parse(try_from_str = parse_int::parse),
    )]
    addr: Option<usize>,

    /// specify size in bytes
    #[clap(long, short, value_name = "nbytes",
        parse(try_from_str = parse_int::parse),
    )]
    nbytes: Option<usize>,

    /// comma-separated bytes to write
    #[clap(
        long,
        short,
        value_name = "bytes",
        group = "command",
        requires = "addr"
    )]
    write: Option<String>,

    /// file to write or verify
    #[clap(long, short = 'W', value_name = "filename", group = "command")]
    writefile: Option<String>,

    /// do not preserve the contents of sector 0
    #[clap(long)]
    write_sector0: bool,

    /// verify instead of writing
    #[clap(long, short = 'V', requires = "writefile")]
    verify: bool,

    /// file to read
    #[clap(long, short = 'R', value_name = "filename", group = "command")]
    readfile: Option<String>,

    /// file to differentially write
    #[clap(long, short = 'D', value_name = "filename", group = "command")]
    diffwrite: Option<String>,

    /// persistently selects a storage slot
    #[clap(long, group = "command")]
    set_persistent_slot: Option<u8>,
}

struct QspiDevice {
    block_size: u32,
    sector_size: u32,
}

fn optional_nbytes<'a>(
    core: &'a mut dyn Core,
    context: &'a mut HiffyContext,
    qspi_read_id: &HiffyFunction,
    nbytes: Option<usize>,
) -> Result<u32> {
    // Nbytes (-n) is optional and defaults to
    // the entire flash contents.
    // Read the flash size from the flash part itself if nbytes
    // is not specified.
    match nbytes {
        Some(nbytes) => Ok(nbytes as u32),
        None => {
            let ops = vec![Op::Call(qspi_read_id.id), Op::Done];
            // Result<
            //  Vec<
            //      Result<Vec<u8>, u32>
            //  >, anyhow::Error>
            match context.run(core, ops.as_slice(), None) {
                Ok(results) => match &results[0] {
                    Ok(buf) => {
                        if mem::size_of::<DeviceIdData>() == buf.len() {
                            let did: DeviceIdData = unsafe {
                                std::ptr::read(buf.as_ptr() as *const _)
                            };
                            Ok(did.size()? as u32)
                        } else {
                            Err(anyhow!(
                                "Unexpected result length: {} != {}",
                                mem::size_of::<DeviceIdData>(),
                                buf.len()
                            ))
                        }
                    }
                    Err(e) => Err(anyhow!(
                        "failed to read ID: {}",
                        qspi_read_id.strerror(*e)
                    )),
                },
                Err(e) => Err(e),
            }
        }
    }
}

///
/// Determine the deltas between the on-disk file and the specified vector
/// of sha256 sums
///
fn deltas(
    device: &QspiDevice,
    filename: &str,
    compare: &[(u32, Vec<u8>)],
    mut diff: impl FnMut(u32, &[u8]) -> Result<()>,
) -> Result<()> {
    let filelen = fs::metadata(filename)?.len() as u32;
    let mut file = File::open(filename)?;

    let mut offset = 0;

    for (c, result) in compare {
        let mut buf = vec![0u8; device.sector_size as usize];

        if offset != *c {
            bail!("mismatched offset; expected {}, found {}", offset, c);
        }

        let len = if offset + device.sector_size > filelen {
            filelen - offset
        } else {
            device.sector_size
        };

        file.read_exact(&mut buf[..len as usize])?;

        let mut hasher = Sha256::new();
        hasher.update(&buf);
        let sum = hasher.finalize();

        if !sum.iter().eq(result.iter()) {
            diff(offset, &buf[..len as usize])?;
        }

        offset += device.sector_size;
    }

    Ok(())
}

///
/// Erase specified sectors
///
/// This will erase sector 0 when requested; it's up to the user to check the
/// command-line arguments for --write-sector0 before calling this function.
///
fn erase(
    device: &QspiDevice,
    core: &mut dyn Core,
    context: &mut HiffyContext,
    funcs: &HiffyFunctions,
    all_sectors: &[u32],
) -> Result<()> {
    let f = funcs.get("QspiSectorErase", 1)?;
    let started = Instant::now();
    let len = all_sectors.len() as u32 * device.sector_size;

    let bar = ProgressBar::new(len.into());

    bar.set_style(
        ProgressStyle::default_bar()
            .template("humility: erasing [{bar:30}] {bytes}/{total_bytes}"),
    );

    bar.set_position(0);

    //
    // To unroll our loop, we need to know how many operations we can stuff in
    // our text.  For this calculation, we'll be a bit pessimal -- we would
    // actually expect to be able to serialize our per-sector operations in
    // fewer than 16 bytes -- but we want to give ourselves a bit of slop.
    // Finally, we clamp our maximum at 512K of erasing at a go to allow us to
    // show progress at a reasonable granularity.
    //
    let op_bytes_per_sector = 16;
    let max_calls = std::cmp::min(
        context.text_size() / op_bytes_per_sector,
        (512 * 1024) / device.sector_size as usize,
    );

    for (n, sectors) in all_sectors.chunks(max_calls).enumerate() {
        let mut ops = vec![];

        for addr in sectors {
            if addr % device.sector_size != 0 {
                bail!("illegal erase address 0x{:x}", addr);
            }

            // Pick the appropriate function to erase this sector
            let (f, needs_drop) = if *addr == 0 {
                if let Ok(f) = funcs.get("QspiSector0Erase", 0) {
                    (f, false)
                } else {
                    // Fallback for older Hubris images
                    ops.push(Op::Push32(*addr));
                    (funcs.get("QspiSectorErase", 1)?, true)
                }
            } else {
                ops.push(Op::Push32(*addr));
                (funcs.get("QspiSectorErase", 1)?, true)
            };

            ops.push(Op::Call(f.id));
            if needs_drop {
                ops.push(Op::Drop);
            }
        }

        ops.push(Op::Done);

        let results = context.run(core, ops.as_slice(), None)?;

        for (i, block_result) in results.iter().enumerate() {
            if let Err(err) = *block_result {
                bail!(
                    "failed to erase 0x{:x}: {}",
                    sectors[i],
                    f.strerror(err)
                );
            }
        }

        bar.set_position(
            ((n * sectors.len()) as u32 * device.sector_size).into(),
        );
    }

    bar.finish_and_clear();

    humility::msg!(
        "erased {} in {}",
        HumanBytes(len.into()),
        HumanDuration(started.elapsed())
    );

    Ok(())
}

///
/// Write in units of blocksize.
///
/// This will write to sector 0 if requested; it's up to the user to check the
/// command-line arguments for --write-sector0 before calling this function.
///
fn write(
    device: &QspiDevice,
    core: &mut dyn Core,
    context: &mut HiffyContext,
    funcs: &HiffyFunctions,
    addr: u32,
    writelen: u32,
    mut getbytes: impl FnMut(&mut [u8]) -> Result<()>,
) -> Result<()> {
    let qspi_page_program =
        // This works on all pages, but importantly, allows us to write to
        // sector 0.
        if let Ok(f) = funcs.get("QspiPageProgramSector0", 3) {
            f
        } else {
            // Backwards-compatibility for older Hubris images
            funcs.get("QspiPageProgram", 3)?
        };

    let data_size = context.data_size() as u32;
    let chunk = data_size - (data_size % device.block_size);
    let mut offset = 0;

    let mut buf = vec![0u8; chunk as usize];

    loop {
        let len = if offset + chunk > writelen {
            //
            // Zero the end of the buffer so we don't have to deal with
            // sub-block size writes inside of HIF
            //
            for i in writelen - offset..chunk {
                buf[i as usize] = 0;
            }

            writelen - offset
        } else {
            chunk
        };

        getbytes(&mut buf[..len as usize])?;

        //
        // We have our chunk; now a HIF loop to write our chunk
        // in block_size nibbles.
        //
        let ops = vec![
            Op::Push32(addr + offset),        // Push flash address.
            Op::Push32(0),                    // Buffer offset = 0.
            Op::PushNone,                     // Placeholder to be dropped.
            Op::Label(Target(0)),             // Start of loop
            Op::Drop,                         // Drop placeholder/limit.
            Op::Push32(device.block_size),    // Push length of this xfer.
            Op::Call(qspi_page_program.id),   // Call (&flash, &buf, len)
            Op::Add,                          // Add len of xfer to buf offset
            Op::Swap,                         // Address now at top
            Op::Push32(device.block_size),    // Push length
            Op::Add,                          // Add to address
            Op::Swap,                         // Buf offset back to top
            Op::Push32(len),                  // Push limit.
            Op::BranchGreaterThan(Target(0)), // Continue if not at limit.
            Op::Done,
        ];

        let results = context.run(core, ops.as_slice(), Some(&buf))?;

        for (i, block_result) in results.iter().enumerate() {
            if let Err(err) = block_result {
                bail!(
                    "failed on block {} at offset {}: {}",
                    i,
                    offset,
                    qspi_page_program.strerror(*err),
                );
            }
        }

        offset += chunk;

        if offset >= writelen {
            return Ok(());
        }
    }
}

fn qspi(context: &mut humility::ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let hubris = context.archive.as_ref().unwrap();

    let subargs = QspiArgs::try_parse_from(subargs)?;
    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;
    let funcs = context.functions()?;

    const SECTOR_SIZE: u32 = 64 * 1024;
    const BLOCK_SIZE: u32 = 256; // Conflating flash block size with hubris scratch buffer.

    let device =
        QspiDevice { block_size: BLOCK_SIZE, sector_size: SECTOR_SIZE };

    let mut ops = vec![];
    let mut hash_name = "".to_string();

    let (data, func) = if subargs.status {
        let qspi_read_status = funcs.get("QspiReadStatus", 0)?;
        ops.push(Op::Call(qspi_read_status.id));
        (None, qspi_read_status)
    } else if subargs.id {
        let qspi_read_id = funcs.get("QspiReadId", 0)?;
        ops.push(Op::Call(qspi_read_id.id));
        (None, qspi_read_id)
    } else if subargs.hash {
        let qspi_hash = funcs.get("QspiHash", 2)?;
        let qspi_read_id = funcs.get("QspiReadId", 0)?;
        // Address is optional and defaults to zero.
        let addr = subargs.addr.unwrap_or(0) as u32;
        // nbytes is optional and defaults to the size of the entire flash.
        let nbytes =
            optional_nbytes(core, &mut context, qspi_read_id, subargs.nbytes)?;
        ops.push(Op::Push32(addr));
        ops.push(Op::Push32(nbytes));
        ops.push(Op::Call(qspi_hash.id));
        hash_name = format!("{:06x}..{:06x}", addr, nbytes);
        (None, qspi_hash)
    } else if subargs.erase {
        let addr = subargs.addr.unwrap() as u32;
        let qspi_sector_erase = if addr / SECTOR_SIZE == 0 {
            if !subargs.write_sector0 {
                bail!("cannot erase sector 0 without --write-sector0 flag");
            } else if let Ok(f) = funcs.get("QspiSector0Erase", 0) {
                f
            } else {
                // Backwards-compatibility for older Hubris images
                ops.push(Op::Push32(addr));
                funcs.get("QspiSectorErase", 1)?
            }
        } else {
            ops.push(Op::Push32(addr));
            funcs.get("QspiSectorErase", 1)?
        };
        ops.push(Op::Call(qspi_sector_erase.id));
        (None, qspi_sector_erase)
    } else if subargs.bulkerase {
        if !subargs.write_sector0 {
            bail!("cannot perform bulk erase without --write-sector0 flag");
        }
        let qspi_bulk_erase = funcs.get("QspiBulkErase", 0)?;
        ops.push(Op::Call(qspi_bulk_erase.id));
        (None, qspi_bulk_erase)
    } else if subargs.read {
        let qspi_read = funcs.get("QspiRead", 2)?;
        ops.push(Op::Push32(subargs.addr.unwrap() as u32));
        ops.push(Op::Push32(subargs.nbytes.unwrap() as u32));
        ops.push(Op::Call(qspi_read.id));
        (None, qspi_read)
    } else if let Some(ref write) = subargs.write {
        let addr = subargs.addr.unwrap() as u32;
        let qspi_page_program = if addr / SECTOR_SIZE == 0 {
            if !subargs.write_sector0 {
                bail!("cannot write to sector 0 without --write-sector0 flag");
            } else if let Ok(f) = funcs.get("QspiPageProgramSector0", 3) {
                f
            } else {
                // Backwards-compatibility for older Hubris images
                funcs.get("QspiPageProgram", 3)?
            }
        } else {
            funcs.get("QspiPageProgram", 3)?
        };
        let bytes: Vec<&str> = write.split(',').collect();
        let mut arr = vec![];

        for byte in &bytes {
            if let Ok(val) = parse_int::parse::<u8>(byte) {
                arr.push(val);
            } else {
                bail!("invalid byte {}", byte)
            }
        }

        ops.push(Op::Push32(addr));
        ops.push(Op::Push(0));
        ops.push(Op::Push32(arr.len() as u32));
        ops.push(Op::Call(qspi_page_program.id));
        (Some(arr), qspi_page_program)
    } else if let Some(filename) = subargs.writefile {
        let qspi_page_program = if subargs.verify {
            funcs.get("QspiVerify", 3)?
        } else if subargs.write_sector0 {
            // This works on all pages, but importantly, allows us to write to
            // sector 0.
            if let Ok(f) = funcs.get("QspiPageProgramSector0", 3) {
                f
            } else {
                // Backwards-compatibility for older Hubris images
                funcs.get("QspiPageProgram", 3)?
            }
        } else {
            funcs.get("QspiPageProgram", 3)?
        };

        let filelen = fs::metadata(filename.clone())?.len() as u32;

        if !subargs.verify {
            let start_addr =
                if subargs.write_sector0 { 0 } else { SECTOR_SIZE };
            //
            // If we're not verifying, we're erasing!
            //
            let sectors = (start_addr..filelen)
                .step_by(SECTOR_SIZE as usize)
                .collect::<Vec<u32>>();

            erase(&device, core, &mut context, &funcs, &sectors)?;
        } else {
            humility::msg!("will verify {} bytes...", filelen);
        }

        //
        // Now we're ready to write/verify in units of blocksize.
        //
        let data_size = context.data_size() as u32;
        let chunk = data_size - (data_size % BLOCK_SIZE);
        let mut offset = 0;

        let mut buf = vec![0u8; chunk as usize];
        let mut file = File::open(filename)?;

        // Skip sector0 if the user didn't specify --write-sector0
        if !subargs.write_sector0 {
            let mut sector0 = vec![0u8; SECTOR_SIZE as usize];
            file.read_exact(&mut sector0)?;
            offset += SECTOR_SIZE;
            if sector0.iter().any(|c| *c != 0xFF) {
                bail!("cannot skip sector 0 with non-empty bytes");
            } else {
                humility::msg!("skipping sector 0");
            }
        }

        let started = Instant::now();
        let bar = ProgressBar::new(filelen as u64);

        if !subargs.verify {
            bar.set_style(ProgressStyle::default_bar().template(
                "humility: flashing [{bar:30}] {bytes}/{total_bytes}",
            ));
        } else {
            bar.set_style(ProgressStyle::default_bar().template(
                "humility: verifying [{bar:30}] {bytes}/{total_bytes}",
            ));
        }

        while offset < filelen {
            let len = if offset + chunk > filelen {
                //
                // Zero the end of the buffer so we don't have to deal with
                // sub-block size writes inside of HIF
                //
                for i in filelen - offset..chunk {
                    buf[i as usize] = 0;
                }

                filelen - offset
            } else {
                chunk
            };

            file.read_exact(&mut buf[..len as usize])?;

            //
            // We have our chunk; now a HIF loop to write/verify our chunk
            // in BLOCK_SIZE nibbles.
            //
            let ops = vec![
                Op::Push32(offset),               // Push flash address.
                Op::Push32(0),                    // Buffer offset = 0.
                Op::PushNone,                     // Placeholder to be dropped.
                Op::Label(Target(0)),             // Start of loop
                Op::Drop,                         // Drop placeholder/limit.
                Op::Push32(BLOCK_SIZE),           // Push length of this xfer.
                Op::Call(qspi_page_program.id),   // Call (&flash, &buf, len)
                Op::Add,  // Add len of that xfer to both values on stack.
                Op::Swap, //
                Op::Push32(BLOCK_SIZE), //
                Op::Add,  //
                Op::Swap, // Now pointing to next xfer.
                Op::Push32(len), // Push limit.
                Op::BranchGreaterThan(Target(0)), // Continue if not at limit.
                Op::Done,
            ];

            let results = context.run(core, ops.as_slice(), Some(&buf))?;

            bar.set_position((offset + len).into());

            for (i, block_result) in results.iter().enumerate() {
                match block_result {
                    Err(err) => {
                        bail!(
                            "failed on block {} at offset {}: {}",
                            i,
                            offset,
                            qspi_page_program.strerror(*err),
                        );
                    }
                    Ok(r) if subargs.verify => {
                        if r.len() != 1 {
                            bail!("expected single byte return value");
                        }

                        if r[0] != 0 {
                            let a = offset + (i as u32 * BLOCK_SIZE);
                            humility::msg!(
                                "block at 0x{:x} failed to verify\n",
                                a
                            );
                        }
                    }
                    _ => {}
                }
            }

            offset += chunk;
        }

        bar.finish_and_clear();

        if subargs.verify {
            humility::msg!(
                "verified {} in {}",
                HumanBytes(filelen as u64),
                HumanDuration(started.elapsed())
            );
        } else {
            humility::msg!(
                "flashed {} in {}",
                HumanBytes(filelen as u64),
                HumanDuration(started.elapsed())
            );
        }

        return Ok(());
    } else if let Some(filename) = subargs.readfile {
        // Address(default=0) and n-bytes(default=size of part-address) are
        // optional.
        let qspi_read_id = funcs.get("QspiReadId", 0)?;
        let qspi_read = funcs.get("QspiRead", 2)?;

        // Address is optional and defaults to zero.
        // The default can/should be done in `#[clap(...` for "address"
        // if that works for the other users of the -a flag.
        let mut address = subargs.addr.unwrap_or(0) as u32;
        println!("addr={:?}", address);

        let nbytes =
            optional_nbytes(core, &mut context, qspi_read_id, subargs.nbytes)?;
        println!("nbytes={:?}", nbytes);

        //
        // Low-level reads are in units less than or equal to
        // context.scratch_size().
        // Those can be batched, depending on serialization overhead, into
        // context.rstack_size().
        //
        // TODO: check alignment of start and end.
        // Things are broken if they aren't, so an assert would be ok.
        //
        let rstack_size = context.rstack_size() as u32;
        let scratch_size = context.scratch_size() as u32;
        // TODO: Don't guess at the overhead. Make sure that data and
        // serialization meta data are both in rstack.
        let overhead = 1; // XXX Assume each item in rstack has 1-byte overhead.

        let chunk = scratch_size;
        let max_chunks = rstack_size / (chunk + overhead);

        let buf = vec![0u8; rstack_size as usize];
        let output_file =
            File::create(filename).expect("Cannot create output file");
        let mut writer = BufWriter::with_capacity(nbytes as usize, output_file);

        let started = Instant::now();
        let bar = ProgressBar::new(nbytes as u64);
        bar.set_style(
            ProgressStyle::default_bar()
                .template("humility: reading [{bar:30}] {bytes}/{total_bytes}"),
        );
        let update_cycle = 64;
        let mut updates = 0;

        let end_address = address + nbytes;
        // let mut out_address = address;
        println!(
            "address={}, chunk={}, end_address={}",
            address, chunk, end_address
        );
        println!("max_chunks={}", max_chunks);
        assert!(max_chunks > 0);
        loop {
            let mut ops = vec![];
            for _ in 0..max_chunks {
                let len = if address + chunk > end_address {
                    end_address - address
                } else {
                    chunk
                };

                if len == 0 {
                    break;
                }
                ops.push(Op::Push32(address));
                ops.push(Op::Push32(len));
                ops.push(Op::Call(qspi_read.id));

                address += len;
            }
            ops.push(Op::Done);

            let results = context.run(core, ops.as_slice(), Some(&buf))?;

            if updates % update_cycle == 0 {
                bar.set_position((address).into());
            }
            updates += 1;

            for (i, block_result) in results.iter().enumerate() {
                match block_result {
                    Err(err) => bail!(
                        "failed to read block {} at offset {}: {}",
                        i,
                        address,
                        qspi_read.strerror(*err)
                    ),
                    Ok(buf) => {
                        writer.write_all(buf).expect("write error");
                        // _out_address += buf.len() as u32;
                    }
                }
            }
            if address >= end_address {
                // redundant with check at top of loop
                break;
            }
        }
        writer.flush().unwrap();
        bar.finish_and_clear();
        humility::msg!(
            "read {} in {}",
            HumanBytes(nbytes as u64),
            HumanDuration(started.elapsed())
        );

        return Ok(());
    } else if let Some(filename) = subargs.diffwrite {
        let filelen = fs::metadata(filename.clone())?.len() as u32;
        let qspi_hash = funcs.get("QspiHash", 2)?;
        let started = Instant::now();

        //
        // We are going to hash the contents to find the differences, and
        // then erase/flash the different sectors.
        //
        let mut address = 0u32;
        let mut sums = vec![];

        let bar = ProgressBar::new(filelen as u64);

        bar.set_style(
            ProgressStyle::default_bar()
                .template("humility: hashing [{bar:30}] {bytes}/{total_bytes}"),
        );

        loop {
            let mut ops = vec![];
            let max = 8;
            let mut laps = 0;
            let base = address;

            bar.set_position(address.into());

            loop {
                let len = if address + SECTOR_SIZE > filelen {
                    filelen - address
                } else {
                    SECTOR_SIZE
                };

                ops.push(Op::Push32(address));
                ops.push(Op::Push32(len));
                ops.push(Op::Call(qspi_hash.id));

                laps += 1;
                address += len;

                if address >= filelen || laps >= max {
                    break;
                }
            }

            ops.push(Op::Done);
            let results = context.run(core, ops.as_slice(), None)?;

            for (sector, result) in results.iter().enumerate() {
                match result {
                    Err(err) => {
                        bail!(
                            "failed on address 0x{:x}: {}",
                            base + sector as u32 * SECTOR_SIZE,
                            qspi_hash.strerror(*err),
                        );
                    }
                    Ok(hash) => {
                        sums.push((
                            base + sector as u32 * SECTOR_SIZE,
                            hash.clone(),
                        ));
                    }
                }
            }

            if address + SECTOR_SIZE >= filelen {
                break;
            }
        }

        bar.finish_and_clear();

        let mut sectors = vec![];
        let mut bufs: Vec<Vec<u8>> = vec![];
        let mut nbytes = 0;

        deltas(&device, &filename, &sums, |offset, buf| {
            sectors.push(offset);
            bufs.push(buf.to_vec());
            nbytes += buf.len();
            Ok(())
        })?;

        if sectors.is_empty() {
            humility::msg!(
                "no delta; hashed {} in {}",
                HumanBytes(filelen as u64),
                HumanDuration(started.elapsed())
            );

            return Ok(());
        }

        if !subargs.write_sector0 {
            if let Some(i) = sectors.iter().position(|p| *p == 0) {
                sectors.remove(i);
                let buf = bufs.remove(i);
                if buf.iter().any(|c| *c != 0xFF) {
                    bail!("cannot skip sector 0 with non-empty bytes");
                } else {
                    humility::msg!("skipping sector 0");
                }
            }
        }
        erase(&device, core, &mut context, &funcs, &sectors)?;

        let bar = ProgressBar::new(nbytes as u64);

        bar.set_style(
            ProgressStyle::default_bar()
                .template("humility: writing [{bar:30}] {bytes}/{total_bytes}"),
        );

        let mut total = 0;

        //
        // Now write each sector.
        //
        for (addr, buf) in sectors.iter().zip(bufs.iter()) {
            let mut offs = 0;
            let writelen = buf.len() as u32;

            let w = |dest: &mut [u8]| {
                bar.set_position(total);
                dest.clone_from_slice(&buf[offs..offs + dest.len()]);
                offs += dest.len();
                total += dest.len() as u64;
                Ok(())
            };

            write(&device, core, &mut context, &funcs, *addr, writelen, w)?;
        }

        bar.finish_and_clear();

        humility::msg!(
            "hashed {}, wrote {} in {}",
            HumanBytes(filelen as u64),
            HumanBytes(total),
            HumanDuration(started.elapsed())
        );

        return Ok(());
    } else if let Some(dev_select) = subargs.set_persistent_slot {
        let dev_select = match dev_select {
            0 => "Flash0",
            1 => "Flash1",
            _ => bail!("dev_select must be 0 or 1"),
        };
        let out = humility_cmd_hiffy::hiffy_call(
            hubris,
            core,
            &mut context,
            &hubris.get_idol_command("HostFlash.write_persistent_data")?,
            &[("dev_select", IdolArgument::String(dev_select))],
            None,
        )?;
        if let Err(e) = out {
            bail!("write_persistent_data failed: {e}");
        } else {
            humility::msg!("write_persistent_data succeeded");
        }
        return Ok(());
    } else {
        bail!("expected an operation");
    };

    ops.push(Op::Done);

    let results = context.run(
        core,
        ops.as_slice(),
        match data {
            Some(ref data) => Some(data.as_slice()),
            _ => None,
        },
    )?;

    if let Err(e) = results[0] {
        bail!("operation failed: {}", func.strerror(e));
    }

    if subargs.read {
        if let Ok(results) = &results[0] {
            Dumper::new().dump(results, subargs.addr.unwrap_or(0) as u32);
            return Ok(());
        }
    } else if subargs.id {
        // If the response is well formatted, print it out in addition to raw hex.
        if let Ok(results) = &results[0] {
            if mem::size_of::<DeviceIdData>() == results.len() {
                let did: DeviceIdData =
                    unsafe { std::ptr::read(results.as_ptr() as *const _) };
                println!("{}", did);
            } else {
                println!(
                    "Unexpected result length: {} != {}",
                    mem::size_of::<DeviceIdData>(),
                    results.len()
                );
            }
        }
    } else if subargs.hash {
        if let Ok(buf) = &results[0] {
            print!("{}: ", hash_name);
            for byte in buf {
                print!("{:02x}", byte);
            }
            println!();
        }

        return Ok(());
    }

    println!("{:x?}", results);

    Ok(())
}

/// Micron's Device ID Data
// Responses to the Read ID family of instructions can be more flexible than
// the struct below, but until we need support beyond Micron MT25Q, this will do.
#[derive(Debug, Copy, Clone)]
#[repr(C, packed)]
struct DeviceIdData {
    manufacturer_id: u8,
    memory_type: u8,
    memory_capacity: u8,
    uid_n: u8,
    ext_device_id: u8,
    device_configuration_info: u8,
    uid: [u8; 14],
}

impl DeviceIdData {
    /// Return flash part size in bytes
    pub fn size(&self) -> Result<usize> {
        match self.memory_capacity {
            // This is currently limited to the codes returned from Micron MT25Q parts.
            0 => Err(anyhow!(
                "unknown size code=0x{:02x?}",
                self.memory_capacity
            )),
            _ => Ok(1usize << (self.memory_capacity)),
        }
    }
}

impl fmt::Display for DeviceIdData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mfg = match self.manufacturer_id {
            0x20 => "Micron".to_string(),
            0xEF => "Winbond".to_string(),
            _ => format!("unknown({:x?}", self.manufacturer_id),
        };
        let memtype: String = match self.memory_type {
            0xBA => "3V".to_string(),
            0xBB => "1.8V".to_string(),
            _ => format!("unknown(0x{:02x?})", self.memory_type),
        };

        write!(
            f,
            "DeviceIdData {{\n\
            \tmanufacturer_id: {}(0x{:02x})\n\
            \tmemory_type: {}({})\n\
            \tmemory_capacity: {}\n\
            \tuid_n: {}\n\
            \t# If a Micron device:\n\
            \text_device_id: 0b{:b}\n\
            \t\t{} device generation,\n\
            \t\t{} BP scheme,\n\
            \t\tHOLD#/RESET#={}\n\
            \t\tAdditional HW RESET# is {}available,\n\
            \t\tSector size is {},\n\
            \tdevice_configuration_info: {}\n\
            \tuid: {:02x?}\n}}",
            mfg,
            self.manufacturer_id,
            memtype,
            self.memory_type,
            match self.size() {
                Ok(capacity) => capacity.to_string(),
                Err(e) => e.to_string(),
            },
            self.uid_n,
            self.ext_device_id,
            if (self.ext_device_id & 0b01000000) != 0 { "2nd" } else { "1st" },
            if (self.ext_device_id & 0b00100000) != 0 {
                "Athernate"
            } else {
                "Standard"
            },
            if (self.ext_device_id & 0b00001000) != 0 {
                "RESET"
            } else {
                "HOLD"
            },
            if (self.ext_device_id & 0b00000100) != 0 { "" } else { "not " },
            if (self.ext_device_id & 0b00000011) == 0b00 {
                "Uniform 64KB"
            } else {
                "unknown"
            },
            self.device_configuration_info,
            self.uid
        )
    }
}

pub fn init() -> Command {
    Command {
        app: QspiArgs::command(),
        name: "qspi",
        run: qspi,
        kind: CommandKind::Attached {
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
        },
    }
}
