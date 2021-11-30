// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use humility::core::Core;
use humility::hubris::*;
use humility_cmd::hiffy::*;
use humility_cmd::printmem;
use humility_cmd::{Archive, Args, Attach, Command, Validate};
use std::fs;
use std::fs::File;
use std::io::Read;
use std::time::Instant;

use anyhow::{bail, Result};
use hif::*;
use structopt::{clap::App, clap::ArgGroup, StructOpt};

use indicatif::{HumanBytes, HumanDuration};
use indicatif::{ProgressBar, ProgressStyle};

#[macro_use]
extern crate log;

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

    /// file to write or verify
    #[structopt(long, short = "W", value_name = "filename", group = "command")]
    writefile: Option<String>,

    /// verify instead of writing
    #[structopt(long, short = "V", requires = "writefile")]
    verify: bool,
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
    let block_size = 256;

    let mut ops = vec![];

    let data = if subargs.status {
        let qspi_read_status = funcs.get("QspiReadStatus", 0)?;
        ops.push(Op::Call(qspi_read_status.id));
        None
    } else if subargs.id {
        let qspi_read_id = funcs.get("QspiReadId", 0)?;
        ops.push(Op::Call(qspi_read_id.id));
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

            info!("erasing {} bytes...", filelen);

            let results =
                context.execute_blocking(core, ops.as_slice(), None)?;
            let f = qspi_sector_erase;

            for (i, block_result) in results.iter().enumerate() {
                if let Err(err) = *block_result {
                    bail!("failed to erase sector {}: {}", i, f.strerror(err));
                }
            }

            info!("... done");
        } else {
            info!("will verify {} bytes...", filelen);
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
                Op::Push32(offset),
                Op::Push32(0),
                Op::PushNone,
                Op::Label(Target(0)),
                Op::Drop,
                Op::Push32(block_size),
                Op::Call(qspi_page_program.id),
                Op::Add,
                Op::Swap,
                Op::Push32(block_size),
                Op::Add,
                Op::Swap,
                Op::Push32(chunk),
                Op::BranchGreaterThan(Target(0)),
                Op::Done,
            ];

            let results =
                context.execute_blocking(core, ops.as_slice(), Some(&buf))?;

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
                            info!("block at 0x{:x} failed to verify", a);
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
            info!(
                "verified {} in {}",
                HumanBytes(filelen as u64),
                HumanDuration(started.elapsed())
            );
        } else {
            info!(
                "flashed {} in {}",
                HumanBytes(filelen as u64),
                HumanDuration(started.elapsed())
            );
        }

        return Ok(());
    } else {
        bail!("expected an operation");
    };

    ops.push(Op::Done);

    let results = context.execute_blocking(
        core,
        ops.as_slice(),
        match data {
            Some(ref data) => Some(data.as_slice()),
            _ => None,
        },
    )?;

    if subargs.read {
        if let Ok(results) = &results[0] {
            printmem(results, 0, 1, 16);
            return Ok(());
        }
    }

    println!("{:x?}", results);

    Ok(())
}

pub fn init<'a, 'b>() -> (Command, App<'a, 'b>) {
    (
        Command::Attached {
            name: "qspi",
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
            run: qspi,
        },
        QspiArgs::clap(),
    )
}
