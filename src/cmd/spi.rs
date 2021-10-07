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
    nbytes: Option<u32>,
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

    let func = |name, nargs| {
        let f = funcs
            .get(name)
            .ok_or_else(|| anyhow!("did not find {} function", name))?;

        if f.args.len() != nargs {
            bail!("mismatched function signature on {}", name);
        }

        Ok(f)
    };

    let spi_read = func("SpiRead", 3)?;
    let spi_write = func("SpiWrite", 2)?;

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

    let task = if let Some(peripheral) = subargs.peripheral {
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

        if found.len() == 0 {
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

    let mut ops = vec![];

    if let HubrisTask::Task(task) = task {
        ops.push(Op::Push32(task));
    } else {
        bail!("SPI task cannot be the kernel");
    }

    info!("SPI master is {}", hubris.lookup_module(task)?.name);

    let data = if let Some(ref write) = subargs.write {
        let mut arr = vec![];
        if write.starts_with("@") {
            use std::io::Read;
            let mut f = std::fs::File::open(&write[1..])?;
            f.read_to_end(&mut arr)?;
        } else {
            let bytes: Vec<&str> = write.split(",").collect();

            for byte in &bytes {
                if let Ok(val) = parse_int::parse::<u8>(byte) {
                    arr.push(val);
                } else {
                    bail!("invalid byte {}", byte)
                }
            }
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

    if subargs.read {
        ops.push(Op::Push32(subargs.nbytes.unwrap()));
        ops.push(Op::Call(spi_read.id));
    } else {
        ops.push(Op::Call(spi_write.id));
    }

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

    println!("{:x?}", results);

    Ok(())
}

pub fn init<'a, 'b>() -> (crate::cmd::Command, App<'a, 'b>) {
    (
        crate::cmd::Command::Attached {
            name: "spi",
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
            run: spi,
        },
        SpiArgs::clap(),
    )
}

#[derive(StructOpt, Debug)]
#[structopt(name = "spiload", about = "SPI file loading")]
struct SpiLoadArgs {
    /// sets timeout
    #[structopt(
        long, short = "T", default_value = "5000", value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// SPI peripheral on which to operate
    #[structopt(long, short, value_name = "peripheral")]
    peripheral: Option<u8>,

    /// Size of chunk to use for each transfer.
    #[structopt(long, short, value_name = "peripheral", default_value="512")]
    chunk_size: usize,

    file: std::path::PathBuf,
}

fn spiload(
    hubris: &mut HubrisArchive,
    core: &mut dyn Core,
    _args: &Args,
    subargs: &Vec<String>,
) -> Result<()> {
    let subargs = SpiLoadArgs::from_iter_safe(subargs)?;
    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;
    let funcs = context.functions()?;

    let func = |name, nargs| {
        let f = funcs
            .get(name)
            .ok_or_else(|| anyhow!("did not find {} function", name))?;

        if f.args.len() != nargs {
            bail!("mismatched function signature on {}", name);
        }

        Ok(f)
    };

    let spi_write = func("SpiWrite", 2)?;

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

    let task = if let Some(peripheral) = subargs.peripheral {
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

        if found.len() == 0 {
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

    info!("SPI master is {}", hubris.lookup_module(task)?.name);

    let task = if let HubrisTask::Task(task) = task {
        task
    } else {
        bail!("SPI task cannot be the kernel");
    };

    let data = std::fs::read(&subargs.file)?;

    let mut ops = vec![
        Op::Push32(task),
        Op::Push32(0),
        Op::Call(spi_write.id),
        Op::Done,
    ];

    for chunk in data.chunks(subargs.chunk_size) {
        ops[1] = Op::Push32(chunk.len() as u32);
        context.execute(
            core,
            &ops,
            Some(&chunk),
        )?;
        loop {
            if context.done(core)? {
                break;
            }

            thread::sleep(Duration::from_millis(100));
        }

        let results = context.results(core)?;

        println!("{:x?}", results);
        if results[0].is_err() { break; }
    }

    Ok(())
}

pub fn init2<'a, 'b>() -> (crate::cmd::Command, App<'a, 'b>) {
    (
        crate::cmd::Command::Attached {
            name: "spiload",
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
            run: spiload,
        },
        SpiLoadArgs::clap(),
    )
}

