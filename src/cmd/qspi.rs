/*
 * Copyright 2021 Oxide Computer Company
 */

use crate::cmd::{Archive, Attach, Validate};
use crate::core::Core;
use crate::hiffy::*;
use crate::hubris::*;
use crate::Args;
use std::thread;

use anyhow::{anyhow, bail, Result};
use hif::*;
use std::time::Duration;
use structopt::clap::App;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(name = "qspi", about = "QSPI status, reading and writing")]
struct QspiArgs {
    /// sets timeout
    #[structopt(
        long, short = "T", default_value = "5000", value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// pull status string
    #[structopt(
        long, short, conflicts_with_all = &["id", "erase", "read", "write"]
    )]
    status: bool,

    /// pull identifier
    #[structopt(
        long, short, conflicts_with_all = &["erase", "read", "write"]
    )]
    id: bool,

    /// perform an erase
    #[structopt(long, short, conflicts_with_all = &["read", "write"])]
    erase: bool,

    /// perform a read
    #[structopt(
        long, short,
        conflicts_with_all = &["write"],
        requires_all = &["nbytes", "addr"]
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

    /// perform a write
    write: Option<String>
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

    if subargs.status {
        let qspi_read_status = func("QspiReadStatus", 0)?;
        ops.push(Op::Call(qspi_read_status.id));
    } else if subargs.id {
        let qspi_read_id = func("QspiReadId", 0)?;
        ops.push(Op::Call(qspi_read_id.id));
    } else if subargs.erase {
        let qspi_bulk_erase = func("QspiBulkErase", 0)?;
        ops.push(Op::Call(qspi_bulk_erase.id));
    } else if subargs.read {
        let qspi_read = func("QspiRead", 2)?;
        ops.push(Op::Push32(subargs.addr.unwrap() as u32));
        ops.push(Op::Push32(subargs.nbytes.unwrap() as u32));
        ops.push(Op::Call(qspi_read.id));
    } else if let Some(ref write) = subargs.write {
        bail!("not yet on write");
/*
        let qspi_page_program = func("QspiPageProgram", 2)?;
        let bytes: Vec<&str> = write.split(",").collect();
        let mut arr = vec![];

        for byte in &bytes {
            if let Ok(val) = parse_int::parse::<u8>(byte) {
                arr.push(val);
            } else {
                bail!("invalid byte {}", byte)
            }
        }

        ops.push(Op::Push32(arr.len() as u32));
        ops.push(Op::Call(func.id));

        context.execute(
            core,
            ops.as_slice(),
            arr,
        )?;
        */
    }

    ops.push(Op::Done);

    context.execute(
        core,
        ops.as_slice(),
        None,
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
            name: "qspi",
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
            run: qspi,
        },
        QspiArgs::clap(),
    )
}
