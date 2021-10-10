/*
 * Copyright 2021 Oxide Computer Company
 */

use crate::cmd::{Archive, Attach, Validate};
use crate::core::Core;
use crate::hiffy::*;
use crate::hubris::*;
use crate::Args;
use std::str;
use std::thread;

use anyhow::{anyhow, bail, Result};
use hif::*;
use std::time::Duration;
use structopt::clap::App;
use structopt::StructOpt;
use core::num::{ParseIntError};
use std::path::PathBuf;
// use std::io;
use std::io::prelude::*;
use std::fs::File;
use std::io::SeekFrom;

fn parse_hex(src: &str) -> Result<u32, ParseIntError> {
    u32::from_str_radix(src, 16)
}

fn parse_instruction(src: &str) -> Result<u32, ParseIntError> {
    match src {
        "jedecid" => Ok(0x9f),
        "read" => Ok(0x03),
        "uuid" => Ok(0x4b),
        "fastread" => Ok(0x0b),
        "page-program" => Ok(0x02),
        "sector-erase" => Ok(0x20),
        "chip-erase" => Ok(0xc7),
        "write-enable" | "wen" => Ok(0x06),
        "read-sfdp" => Ok(0x5a),
        "read-status-1" => Ok(0x05),
        "read-status-2" => Ok(0x35),
        "read-status-3" => Ok(0x15),
        "write-status-1" => Ok(0x01),
        "write-status-2" => Ok(0x31),
        "write-status-3" => Ok(0x11),
        "reset-en" => Ok(0x66),
        "reset" => Ok(0x99),
        // Add the other supported codes to the above.
        _ => match parse_hex(src) {
            Ok(inst) => Ok(inst),
            Err(e) => Err(e),
        },
    }
}

#[derive(StructOpt, Debug)]
#[structopt(name = "spiflash", about = "SPIFLASH Flash reading, writing, etc.")]
struct SpiFlashArgs {
    /// sets timeout
    #[structopt(
        long, short = "T", default_value = "5000", value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    // perform a read
    #[structopt(subcommand)]
    cmd: Command,
}

/// Read data from a SPI flash part
#[derive(StructOpt, Debug)]
enum Command {
    Read {
        /// Flash address
        #[structopt(parse(try_from_str = parse_hex))]
        address: u32,

        /// Data transfer length
        #[structopt(parse(try_from_str = parse_hex))]
        length: u32,

        /// Binary output file, no file name means hexdump to stdout
        /// XXX add formatting options
        #[structopt(long = "out", parse(from_os_str))]
        output: Option<PathBuf>,
    },

    /// Write data to a SPI flash part
    Write {
        /// Flash address
        #[structopt(parse(try_from_str = parse_hex))]
        address: u32,

        /// Data transfer length
        /// 
        /// Length can be inferred from input file or --data option
        #[structopt(parse(try_from_str = parse_hex))]
        length: Option<u32>,

        /// Offset into data source
        #[structopt(long, parse(try_from_str = parse_hex))]
        offset: Option<u32>,

        /// Binary input file
        #[structopt(short, long = "in", parse(from_os_str))]
        input: Option<PathBuf>,

        /// comma-separated hex bytes to write
        /// 
        /// Take list length as --nbytes or if --nbytes then truncate
        /// or pad with 0xFF.
        #[structopt(long, short)]
        data: Option<String>,
    },

    Get {
        #[structopt(
            long, short = "I",
            parse(try_from_str = parse_instruction)
        )]
        instruction: u32,

        #[structopt(long, short)]
        length: Option<u32>,

        /// Some instructions require an additional parameter
        param: Option<u32>,
    }
}

fn spiflash(
    hubris: &mut HubrisArchive,
    core: &mut dyn Core,
    _args: &Args,
    subargs: &Vec<String>,
) -> Result<()> {
    let subargs = SpiFlashArgs::from_iter_safe(subargs)?;
    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;
    let funcs = context.functions()?;

    let func = |name, nargs| {
        let f = funcs
            .get(name)
            .ok_or_else(|| anyhow!("did not find {} function", name))?;

        if f.args.len() != nargs {
            bail!("mismatched function signature on {}, actual:{} != expected:{}\n\targs={:?}",
                name, f.args.len(), nargs, f.args);
        }

        Ok(f)
    };

    let spiflash_read = func("SpiFlashRead", 3)?;
    let spiflash_write = func("SpiFlashWrite", 3)?;
    let spiflash_get = func("SpiFlashGet", 3)?;

    let tasks = hubris.lookup_feature("spiflash");
    if !tasks.is_ok() {
        bail!("no spiflash task exists");
    }
    let tasks = tasks.unwrap();
    let task = match tasks.len() {
        0 => bail!("there is no spiflash task"),
        1 => tasks[0],
        _ => bail!("more than one task has spiflash"),
    };

    // All of the SpiFlash commands are in the form:
    //
    //    (Task, Instruction, Option<32>, Option<32>, Buffer)
    //
    // Instruction is a SPI NOR Flash opcode from the MT25Q instruction set.
    // Those instructions are mapped 1:1 for now for the actual MT25Q device
    // we are currently using.
    // Instructions can be mapped to different instructions in the MT25Q
    // set or another manufacturer's set if needed.
    let mut ops = vec![];

    if let HubrisTask::Task(task) = task {
        ops.push(Op::Push32(task));
    } else {
        bail!("SPIFLASH task cannot be the kernel");
    }

    info!("SPIFLASH master is {}", hubris.lookup_module(task)?.name);

    let timeout = subargs.timeout;
    let buffer = match subargs.cmd {
        Command::Get{instruction, length, param} => {
        // e.g. "spiflash get -I jedecid -l 3"
        println!("inst {:?}", instruction);
        println!("length {:?}", length);
        println!("param {:?}", param);
        ops.push(Op::Push(instruction as u8));
        let arr = if length.is_some() {
            let getlen = length.unwrap();
            ops.push(Op::Push32(getlen));
            let mut arr = vec![0u8; getlen as usize];
            Some(arr)
        } else {
            ops.push(Op::PushNone);
            None
        };
        if param.is_some() {
            ops.push(Op::Push32(param.unwrap()));    // optional param for some instructions
        } else {
            ops.push(Op::PushNone);    // optional param for some instructions
        }
        ops.push(Op::Call(spiflash_get.id));
        arr
    },
     Command::Read{ address, length, output } => {
        // e.g. spiflash read 4000 100 --out ./data.bin
        println!("address {:?}", address);
        println!("length {:?}", length);
        println!("output {:?}", output);
        // XXX What is the right order to push these Ops?
        ops.push(Op::Push(parse_instruction("read").unwrap() as u8));
        ops.push(Op::Push32(address));
        ops.push(Op::Push32(length));
        let mut arr = vec![0u8; length as usize];
        ops.push(Op::Call(spiflash_read.id));
        Some(arr)
    },
     Command::Write{address, length, offset, input, data} => {
        // Write data to a SPI flash.
        //
        // Hubris drv_spiflash_api handles issuing the WEN instruction before PP.
        //
        // e.g. spiflash write [--erase] address <length> \
        //          [-offset xxx] [--input path] [--data xx,xx,... ]

        // Note: It is assumed that a chip erase has been done and that no page
        // needs to be erased before programming.
        // Alternatively, a series of sector erase commands could be issued to
        // reset bytes in the flash before programming an area.
        // Some devices have limits on the safety of multiple writes to the same
        // flash area (line, page, sector) between erases.
        // TODO: check supported write/erase attributes of the parts being used.
        println!("address {:?}", address);
        // If length is not specified, it should be the length of the source.
        // If length is specified, it should override the length of the source.
        // If the source has less data than specified by length, that's an error.
        println!("length {:?}", length);

        // There are limits on buffer size that can be transferred to Hubris,
        // so, an offset into the source is allowed.
        // TODO: Add appropriate buffering and looping to allow fully automated
        // writing of entire flash images.
        println!("offset {:?}", offset);

        println!("input {:?}", input);
        println!("data {:?}", data);
        // Input and Data should not be specified at the same time.
        if input.is_some() == data.is_some() {
            bail!("One of '--in path' or '--data bytes' are required, but not both");
        }

        let buffer = if data.is_some() {
            // Parse a string of comma separated hex bytes.
            let data = data.unwrap();
            let bytes: Vec<&str> = data.split(",").collect();
            let mut arr = Vec::new();

            for byte in &bytes {
                if let Ok(val) = u8::from_str_radix(byte, 16) {
                    arr.push(val);
                } else {
                    bail!("invalid byte {}", byte)
                }
            }
            Some(arr)
        } else {
            let path = input.unwrap();
            let mut f = File::open(path)?;
            let mut filelen = f.metadata().unwrap().len() as usize;
            if offset.is_some() {
                let offset = offset.unwrap();
                f.seek(SeekFrom::Start(offset.into()))?;
                // Assuming seek past end of file does work (TODO: check that),
                // then it's ok to subtract offset from filelen.
                filelen -= offset as usize;
            }
            if filelen > 65536 {
                // TODO: There is a 64k limit somewhere
                // TODO: --data xx,yy,zz,.. could exceed 64k too.
                info!("Exceeding 64k buffer. Truncating to 64k");
                filelen = 65536;
            }

            let mut arr = vec![0u8; filelen as usize];
            match f.read(&mut arr) {
                Ok(_) => (),
                Err(err) => bail!("Error {}: Cannot read", err),
            }
            Some(arr)
        };
        // Write is page program. spiflash-api can break it down to multiple
        // operations.
        // TODO: Use cleaner abstractions that don't conflict with lower-level
        // semantics.

        // XXX What is the right order to push these Ops?
        let inst = parse_instruction("page-program").unwrap();
        ops.push(Op::Push(inst as u8));
        ops.push(Op::Push32(address));

        if !buffer.is_some() {
            bail!("SpiFlash write is missing input data to write to flash.");
        }
        let buffer = buffer.unwrap();

        if length.is_some() {
            let length = length.unwrap();
            if length as usize > buffer.len() {
                bail!("--length is greater than provided data");
            }
            ops.push(Op::Push32(length));
        } else {
            ops.push(Op::Push32(buffer.len() as u32));
        }
        ops.push(Op::Call(spiflash_write.id));
        Some(buffer)
    },
    };


    ops.push(Op::Done);

    context.execute(
        core,
        ops.as_slice(),
        match buffer {
            Some(ref buffer) => Some(buffer.as_slice()),
            _ => None,
        },
    )?;

    loop {
        if context.done(core)? {
            break;
        }

        thread::sleep(Duration::from_millis(100));
        // XXX Is timeout implemented?
    }

    let results = context.results(core)?;

    // TODO: In the case of SpiFlashRead we optionally want the data
    // to be written to a file.
    println!("{:x?}", results);

    Ok(())
}

pub fn init<'a, 'b>() -> (crate::cmd::Command, App<'a, 'b>) {
    (
        crate::cmd::Command::Attached {
            name: "spiflash",
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
            run: spiflash,
        },
        SpiFlashArgs::clap(),
    )
}
