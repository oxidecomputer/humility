/*
 * Copyright 2021 Oxide Computer Company
 */

use crate::cmd::printmem;
use crate::cmd::{Archive, Attach, Validate};
use crate::core::Core;
use crate::hiffy::*;
use crate::hubris::*;
use crate::Args;
use std::fs;
use std::fs::File;
use std::io::prelude::*;
use std::io::{BufWriter,Read};
use std::thread;
use std::time::Instant;
use std::mem;
use std::fmt;

use anyhow::{anyhow, bail, Result};
use hif::*;
use std::time::Duration;
use structopt::{clap::App, clap::ArgGroup, StructOpt};

use indicatif::{HumanBytes, HumanDuration};
use indicatif::{ProgressBar, ProgressStyle};

#[derive(StructOpt, Debug)]
#[structopt(
    name = "qspi", about = "QSPI status, reading and writing",
    group = ArgGroup::with_name("command").multiple(false)
)]
struct QspiArgs {
    /// sets timeout
    #[structopt(
        long, short = "T", default_value = "5000", value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// pull status string
    #[structopt(long, short, group = "command")]
    status: bool,

    /// pull identifier
    #[structopt(long, short, group = "command")]
    id: bool,

    /// perform a sector erase
    #[structopt(
        long, short,
        group = "command",
        requires_all = &["addr"]
    )]
    erase: bool,

    /// perform a bulk erase
    #[structopt(long, short = "E", group = "command")]
    bulkerase: bool,

    /// perform a read
    #[structopt(
        long, short, group = "command", requires_all = &["addr", "nbytes"]
    )]
    read: bool,

    /// specify flash address in bytes
    #[structopt(long, short, value_name = "address",
        parse(try_from_str = parse_int::parse),
    )]
    addr: Option<usize>,

    /// specify size in bytes
    #[structopt(long, short, value_name = "nbytes",
        parse(try_from_str = parse_int::parse),
    )]
    nbytes: Option<usize>,

    /// comma-separated bytes to write
    #[structopt(
        long,
        short,
        value_name = "bytes",
        group = "command",
        requires = "addr"
    )]
    write: Option<String>,

    /// file to write
    #[structopt(long, short = "W", value_name = "filename", group = "command")]
    writefile: Option<String>,

    /// verbose (for readfile)
    #[structopt(long, short)]
    verbose: bool,

    /// file to read
    #[structopt(long, short = "R", value_name = "filename", group = "command")]
    readfile: Option<String>,
}

fn qspi(
    hubris: &mut HubrisArchive,
    core: &mut dyn Core,
    _args: &Args,
    subargs: &Vec<String>,
) -> Result<()> {
    let subargs = QspiArgs::from_iter_safe(subargs)?;
    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;
    let funcs = context.functions()?;

    let sector_size = 64 * 1024;
    let block_size = 256;   // XXX This conflates SPI NOR Flash block size with hubris scratch buffer.

    let func = |name, nargs| {
        let f = funcs
            .get(name)
            .ok_or_else(|| anyhow!("did not find {} function", name))?;

        if f.args.len() != nargs {
            bail!("mismatched function signature on {}", name);
        }

        Ok(f)
    };

    let mut ops = vec![];

    let data =
        if subargs.status {
            let qspi_read_status = func("QspiReadStatus", 0)?;
            ops.push(Op::Call(qspi_read_status.id));
            None
        } else if subargs.id {
            let qspi_read_id = func("QspiReadId", 0)?;
            ops.push(Op::Call(qspi_read_id.id));
            None
        } else if subargs.erase {
            let qspi_sector_erase = func("QspiSectorErase", 1)?;
            ops.push(Op::Push32(subargs.addr.unwrap() as u32));
            ops.push(Op::Call(qspi_sector_erase.id));
            None
        } else if subargs.bulkerase {
            let qspi_bulk_erase = func("QspiBulkErase", 0)?;
            ops.push(Op::Call(qspi_bulk_erase.id));
            None
        } else if subargs.read {
            let qspi_read = func("QspiRead", 2)?;
            ops.push(Op::Push32(subargs.addr.unwrap() as u32));
            ops.push(Op::Push32(subargs.nbytes.unwrap() as u32));
            ops.push(Op::Call(qspi_read.id));
            None
        } else if let Some(ref write) = subargs.write {
            let qspi_page_program = func("QspiPageProgram", 3)?;
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
            let qspi_sector_erase = func("QspiSectorErase", 1)?;
            let qspi_page_program = func("QspiPageProgram", 3)?;

            //
            // First, we need to erase the sectors
            //
            let filelen = fs::metadata(filename.clone())?.len() as u32;

            ops.push(Op::Push32(filelen));
            ops.push(Op::Push32(0));
            ops.push(Op::Label(Target(0)));
            ops.push(Op::Call(qspi_sector_erase.id));
            ops.push(Op::Push32(sector_size));
            ops.push(Op::Add);
            ops.push(Op::BranchLessThan(Target(0)));
            ops.push(Op::Done);

            info!("erasing {} bytes...", filelen);

            context.execute(core, ops.as_slice(), None)?;

            loop {
                if context.done(core)? {
                    break;
                }

                thread::sleep(Duration::from_millis(100));
            }

            let results = context.results(core)?;
            let f = qspi_sector_erase;

            for (i, block_result) in results.iter().enumerate() {
                if let Err(err) = *block_result {
                    bail!("failed to erase sector {}: {}", i, f.strerror(err));
                }
            }

            info!("... done");

            //
            // Okay, it's erased.  Now we need to write it in units of blocksize.
            //
            let data_size = context.data_size() as u32;
            let chunk = data_size - (data_size % block_size);
            let mut offset = 0;

            let mut buf = vec![0u8; chunk as usize];
            let mut file = File::open(filename)?;

            let started = Instant::now();
            let bar = ProgressBar::new(filelen as u64);
            bar.set_style(ProgressStyle::default_bar().template(
                "humility: flashing [{bar:30}] {bytes}/{total_bytes}",
            ));

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
                // We have our chunk; now a HIF loop to write our chunk in
                // block_size nibbles.
                //
                let ops = vec![
                    Op::Push32(offset),             // Push flash address.
                    Op::Push32(0),                  // Buffer offset = 0.
                    Op::PushNone,                   // Placeholder to be dropped.
                    Op::Label(Target(0)),           // Start of loop
                    Op::Drop,                       // Drop placeholder/limit.
                    Op::Push32(block_size),         // Push length of this xfer.
                    Op::Call(qspi_page_program.id), // Call (&flash, &buf, len)
                    Op::Add,                        // Add len of that xfer to both values on stack.
                    Op::Swap,                       //
                    Op::Push32(block_size),         //
                    Op::Add,                        //
                    Op::Swap,                       // Now pointing to next xfer.
                    Op::Push32(chunk),              // Push limit.
                    Op::BranchGreaterThan(Target(0)), // Continue if not at limit.
                    Op::Done,
                ];

                context.execute(core, ops.as_slice(), Some(&buf))?;

                loop {
                    if context.done(core)? {
                        break;
                    }

                    thread::sleep(Duration::from_millis(100));
                }

                let results = context.results(core)?;

                bar.set_position((offset + len).into());

                for (i, block_result) in results.iter().enumerate() {
                    if let Err(err) = *block_result {
                        bail!(
                            "failed to write block {} at offset {}: {}",
                            i,
                            offset,
                            f.strerror(err)
                        );
                    }
                }

                offset += chunk;

                if offset >= filelen {
                    break;
                }
            }

            bar.finish_and_clear();

            info!(
                "flashed {} in {}",
                HumanBytes(filelen as u64),
                HumanDuration(started.elapsed())
            );

            bail!("all done");
        } else if let Some(filename) = subargs.readfile {
            // Address(default=0) and n-bytes(entire part) are optional.
            // TODO: This clause can be merged with the subargs.read code above.
            let qspi_read_id = func("QspiReadId", 0)?;
            let qspi_read = func("QspiRead", 2)?;

            // Address is optional and defaults to zero.
            // The default can/should be done in `#[structopt(...` for "address"
            // if that works for the other users of the -a flag.
            let mut address = match subargs.addr {
                Some(addr) => addr as u32,
                _ => 0,
            };
            // Nbytes (-n) is optional and for this command defaults to
            // the entire flash contents.
            // Read the flash size from the flash part itself if nbytes
            // is not specified.
            let nbytes = match subargs.nbytes {
                Some(nbytes) => nbytes as u32,
                _ => {
                    ops.push(Op::Call(qspi_read_id.id));
                    ops.push(Op::Done);
                    context.execute(core, ops.as_slice(), None)?;
                    loop {
                        if context.done(core)? {
                            break;
                        }
                        thread::sleep(Duration::from_millis(100));
                    }
                    let results = context.results(core)?;
                    match &results[0] {
                        Ok(buf) => {
                            if mem::size_of::<DeviceIdData>() == buf.len() {
                                let did: DeviceIdData = unsafe { std::ptr::read(buf.as_ptr() as *const _)};
                                did.size()
                            } else {
                                bail!("Unexpected result length: {} != {}", mem::size_of::<DeviceIdData>(), buf.len());
                            }
                        },
                        _ => bail!("Invalid size in JEDIC Id"),
                    }?
                },
            };

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
            let overhead = 1;   // Assume each item in rstack has 1-byte overhead.

            let chunk = scratch_size;
            let max_chunks = rstack_size / (chunk + overhead);

            let buf = vec![0u8; rstack_size as usize];
            let mut output_file = File::create(filename).expect("Cannot create output file");
            let mut writer = BufWriter::with_capacity(nbytes as usize, output_file);

            let started = Instant::now();
            let bar = ProgressBar::new(nbytes as u64);
            bar.set_style(ProgressStyle::default_bar().template(
                "humility: reading [{bar:30}] {bytes}/{total_bytes}",
            ));
            let update_cycle = 64;
            let mut updates = 0;

            let end_address = address + nbytes;
            let mut out_address = address;
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

                match context.execute(core, ops.as_slice(), Some(&buf)) {
                    Ok(x) => Ok(x),
                    Err(e) => {
                        println!("Err={:?}", e);
                        Err(e)
                    },
                }?;

                loop {
                    if context.done(core)? {
                        break;
                    }

                    thread::sleep(Duration::from_millis(100));
                }

                let results = context.results(core)?;

                if updates % update_cycle == 0 {
                    bar.set_position((address).into());
                }
                updates += 1;

                for (i, block_result) in results.iter().enumerate() {
                    match &*block_result {
                        Err(err) => bail!(
                            "failed to read block {} at offset {}: {}",
                            i, address, qspi_read.strerror(*err)),
                        Ok(buf) => {
                            if subargs.verbose {
                                printmem(buf, out_address, 1, 16);
                            }
                            writer.write_all(buf).expect("write error");
                            out_address += buf.len() as u32;
                        },
                    }
                }
                if address >= end_address { // redundant with check at top of loop
                    break;
                }
            }
            writer.flush().unwrap();

            bar.finish_and_clear();

            info!(
                "read {} in {}",
                HumanBytes(nbytes as u64),
                HumanDuration(started.elapsed())
            );

            return Ok(());
        } else {
            bail!("expected an operation");
        };

    ops.push(Op::Done);

    context.execute(
        core,
        ops.as_slice(),
        match data {
            Some(ref data) => Some(data.as_slice()),
            _ => None,
        },
    )?;

    loop {
        if context.done(core)? {
            break;
        }

        thread::sleep(Duration::from_millis(100));
    }

    let results = context.results(core)?;

    if subargs.read {
        if let Ok(results) = &results[0] {
            printmem(results, 0, 1, 16);
            return Ok(());
        }
    } else if subargs.id {
        // If the response is well formatted, print it out in addition to raw hex.
        if let Ok(results) = &results[0] {
            if mem::size_of::<DeviceIdData>() == results.len() {
                let did: DeviceIdData = unsafe { std::ptr::read(results.as_ptr() as *const _)};
                println!("{}", did);
            } else {
                println!("Unexpected result length: {} != {}",
                    mem::size_of::<DeviceIdData>(),
                    results.len());
            }
        }
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
    uid: [u8;14],
}

impl DeviceIdData {
    /// Return flash part size in bytes
    pub fn size(&self) -> Result<u32> {
        match self.memory_capacity {
            // This is currently limited to the codes returned from Micron MT25Q parts.
            0x17..=0x22 => Ok(1u32 << (self.memory_capacity + 1)),
            _ => bail!("unknown DeviceIdData capacity code {:x?}h", self.memory_capacity),
        }
    }
}

impl fmt::Display for DeviceIdData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mfg = match self.manufacturer_id {
            0x20 => format!("Micron"),
            _ => format!("unknown({:x?}", self.manufacturer_id),
        };
        let memtype: String = match self.memory_type {
            0xBA => "3V".to_string(),
            0xBB => "1.8V".to_string(),
            _ => format!("unknown({:x?}", self.memory_type),
        };
        let capacity = match self.memory_capacity {
            0x17..=0x22 => format!("{}MB", 1u32 << (self.memory_capacity - 20)),
            _ => format!("unknown code {:x?}h", self.memory_capacity),
        };

        write!(f, "DeviceIdData {{\n\
            \tmanufacturer_id: {}\n\
            \tmemory_type: {}\n\
            \tmemory_capacity: {}\n\
            \tuid_n: {}\n\
            \text_device_id: 0b{:b}\n\
            \t\t{} device generation,\n\
            \t\t{} BP scheme,\n\
            \t\tHOLD#/RESET#={}\n\
            \t\tAdditional HW RESET# is {}available,\n\
            \t\tSector size is {},\n\
            \tdevice_configuration_info: {}\n\
            \tuid: {:02x?}\n}}",
            mfg, memtype, capacity, self.uid_n,
            self.ext_device_id,
            if (self.ext_device_id & 0b01000000) != 0 {
                "2nd"
            } else {
                "1st"
            },
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
            if (self.ext_device_id & 0b00000100) != 0 {
                ""
            } else {
                "not "
            },
            if (self.ext_device_id & 0b00000011) == 0b00 {
                "Uniform 64KB"
            } else {
                "unknown"
            },
            self.device_configuration_info, self.uid)
    }
}


pub fn init<'a, 'b>() -> (crate::cmd::Command, App<'a, 'b>) {
    (
        crate::cmd::Command::Attached {
            name: "qspi",
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
            run: qspi,
        },
        QspiArgs::clap(),
    )
}
