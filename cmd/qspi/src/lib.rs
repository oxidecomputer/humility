// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use humility::core::Core;
use humility::hubris::*;
use humility_cmd::hiffy::*;
use humility_cmd::{Archive, Args, Attach, Command, Dumper, Validate};
use std::fmt;
use std::fs;
use std::fs::File;
use std::io::{BufWriter, Read, Write};
use std::mem;
use std::time::Instant;

use anyhow::{anyhow, bail, Result};
use clap::Command as ClapCommand;
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
        long, short = 'T', default_value = "5000", value_name = "timeout_ms",
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

    /// perform a bulk erase
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

    /// verify instead of writing
    #[clap(long, short = 'V', requires = "writefile")]
    verify: bool,

    /// file to read
    #[clap(long, short = 'R', value_name = "filename", group = "command")]
    readfile: Option<String>,
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
                    Err(e) => Err(anyhow!("{}", e)),
                },
                Err(e) => Err(e),
            }
        }
    }
}

fn qspi(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    _args: &Args,
    subargs: &[String],
) -> Result<()> {
    let subargs = QspiArgs::try_parse_from(subargs)?;
    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;
    let funcs = context.functions()?;

    let sector_size = 64 * 1024;
    let block_size = 256; // Conflating flash block size with hubris scratch buffer.

    let mut ops = vec![];
    let mut hash_name = "".to_string();

    let data = if subargs.status {
        let qspi_read_status = funcs.get("QspiReadStatus", 0)?;
        ops.push(Op::Call(qspi_read_status.id));
        None
    } else if subargs.id {
        let qspi_read_id = funcs.get("QspiReadId", 0)?;
        ops.push(Op::Call(qspi_read_id.id));
        None
    } else if subargs.hash {
        let qspi_hash = funcs.get("QspiHash", 2)?;
        let qspi_read_id = funcs.get("QspiReadId", 0)?;
        // Address is optional and defaults to zero.
        let addr = subargs.addr.or(Some(0)).unwrap() as u32;
        // nbytes is optional and defaults to the size of the entire flash.
        let nbytes =
            optional_nbytes(core, &mut context, qspi_read_id, subargs.nbytes)?;
        ops.push(Op::Push32(addr));
        ops.push(Op::Push32(nbytes));
        ops.push(Op::Call(qspi_hash.id));
        hash_name = format!("{:06x}..{:06x}", addr, nbytes);
        None
    } else if subargs.erase {
        let qspi_sector_erase = funcs.get("QspiSectorErase", 1)?;
        ops.push(Op::Push32(subargs.addr.unwrap() as u32));
        ops.push(Op::Call(qspi_sector_erase.id));
        None
    } else if subargs.bulkerase {
        let qspi_bulk_erase = funcs.get("QspiBulkErase", 0)?;
        ops.push(Op::Call(qspi_bulk_erase.id));
        None
    } else if subargs.read {
        let qspi_read = funcs.get("QspiRead", 2)?;
        ops.push(Op::Push32(subargs.addr.unwrap() as u32));
        ops.push(Op::Push32(subargs.nbytes.unwrap() as u32));
        ops.push(Op::Call(qspi_read.id));
        None
    } else if let Some(ref write) = subargs.write {
        let qspi_page_program = funcs.get("QspiPageProgram", 3)?;
        let bytes: Vec<&str> = write.split(',').collect();
        let mut arr = vec![];

        for byte in &bytes {
            if let Ok(val) = parse_int::parse::<u8>(byte) {
                arr.push(val);
            } else {
                bail!("invalid byte {}", byte)
            }
        }

        ops.push(Op::Push32(subargs.addr.unwrap() as u32));
        ops.push(Op::Push(0));
        ops.push(Op::Push32(arr.len() as u32));
        ops.push(Op::Call(qspi_page_program.id));
        Some(arr)
    } else if let Some(filename) = subargs.writefile {
        let qspi_sector_erase = funcs.get("QspiSectorErase", 1)?;
        let qspi_page_program = if subargs.verify {
            funcs.get("QspiVerify", 3)
        } else {
            funcs.get("QspiPageProgram", 3)
        }?;

        let filelen = fs::metadata(filename.clone())?.len() as u32;

        if !subargs.verify {
            //
            // First, we need to erase the sectors
            //
            ops.push(Op::Push32(filelen));
            ops.push(Op::Push32(0));
            ops.push(Op::Label(Target(0)));
            ops.push(Op::Call(qspi_sector_erase.id));
            ops.push(Op::Push32(sector_size));
            ops.push(Op::Add);
            ops.push(Op::BranchLessThan(Target(0)));
            ops.push(Op::Done);

            humility::msg!("erasing {} bytes...", filelen);

            let results = context.run(core, ops.as_slice(), None)?;
            let f = qspi_sector_erase;

            for (i, block_result) in results.iter().enumerate() {
                if let Err(err) = *block_result {
                    bail!("failed to erase sector {}: {}", i, f.strerror(err));
                }
            }

            humility::msg!("... done");
        } else {
            humility::msg!("will verify {} bytes...", filelen);
        }

        //
        // Now we're ready to write/verify in units of blocksize.
        //
        let data_size = context.data_size() as u32;
        let chunk = data_size - (data_size % block_size);
        let mut offset = 0;

        let mut buf = vec![0u8; chunk as usize];
        let mut file = File::open(filename)?;

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

        loop {
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
            // in block_size nibbles.
            //
            let ops = vec![
                Op::Push32(offset),               // Push flash address.
                Op::Push32(0),                    // Buffer offset = 0.
                Op::PushNone,                     // Placeholder to be dropped.
                Op::Label(Target(0)),             // Start of loop
                Op::Drop,                         // Drop placeholder/limit.
                Op::Push32(block_size),           // Push length of this xfer.
                Op::Call(qspi_page_program.id),   // Call (&flash, &buf, len)
                Op::Add,  // Add len of that xfer to both values on stack.
                Op::Swap, //
                Op::Push32(block_size), //
                Op::Add,  //
                Op::Swap, // Now pointing to next xfer.
                Op::Push32(chunk), // Push limit.
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
                            let a = offset + (i as u32 * block_size);
                            humility::msg!(
                                "block at 0x{:x} failed to verify",
                                a
                            );
                        }
                    }
                    _ => {}
                }
            }

            offset += chunk;

            if offset >= filelen {
                break;
            }
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
        let mut address = subargs.addr.or(Some(0)).unwrap() as u32;
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
                match &*block_result {
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

    if subargs.read {
        if let Ok(results) = &results[0] {
            Dumper::new().dump(results, 0);
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
        match &results[0] {
            Ok(buf) => {
                print!("{}: ", hash_name);
                for byte in buf {
                    print!("{:02x}", byte);
                }
                println!();
            }
            Err(e) => {
                bail!("hash failed: {}", e);
            }
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
            0 => {
                return Err(anyhow!(
                    "unknown size code=0x{:02x?}",
                    self.memory_capacity
                ))
            }
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

pub fn init() -> (Command, ClapCommand<'static>) {
    (
        Command::Attached {
            name: "qspi",
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
            run: qspi,
        },
        QspiArgs::command(),
    )
}
