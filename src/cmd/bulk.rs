/*
 * Copyright 2020 Oxide Computer Company
 */

use crate::cmd::*;
use crate::hubris::*;
use crate::hiffy::*;
use crate::Args;
use anyhow::{anyhow, bail, Result};
use std::thread;
use structopt::clap::App;
use structopt::StructOpt;
use std::time::Instant;
use std::time::Duration;
use hif::*;

#[derive(StructOpt, Debug)]
#[structopt(name = "bulk", about = "bulk transfer operations")]
struct BulkArgs {
    /// sets timeout
    #[structopt(
        long, short = "T", default_value = "5000", value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    #[structopt(
        long, short, default_value = "1024", value_name = "len",
        parse(try_from_str = parse_int::parse)
    )]
    len: u32,

    /// number of bytes to synthetically benchmark
    #[structopt(
        long, short, default_value = "10000", value_name = "nbytes",
        parse(try_from_str = parse_int::parse)
    )]
    nbytes: u32,
}

fn bulk(
    hubris: &mut HubrisArchive,
    core: &mut dyn crate::core::Core,
    _args: &Args,
    subargs: &Vec<String>,
) -> Result<()> {
    let subargs = BulkArgs::from_iter_safe(subargs)?;
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

    let cksum = func("Checksum", 1)?;
    let mut ops = vec![];

    let mut bytes = vec![0u8; subargs.len as usize];
    bytes[0] = 0x1d;

    ops.push(Op::Push32(subargs.len));
    ops.push(Op::Call(cksum.id));
    ops.push(Op::Done);

    let started = Instant::now();
    let mut total = 0;

    loop {
        context.execute(core, ops.as_slice(), Some(bytes.as_slice()))?;

        loop {
            if context.done(core)? {
                break;
            }
            thread::sleep(Duration::from_millis(1));
        }

        total += bytes.len();
        let results = context.results(core)?;

        // println!("{:?}", results);

        if total > subargs.nbytes as usize {
            break;
        }

        bytes[0] += 1;
    }

    let elapsed = started.elapsed().as_secs_f64();

    println!("bufsize {}: {} bytes in {} ({} bytes/sec)", subargs.len, total, elapsed,
        (total as f64) / elapsed);

    Ok(())
}

pub fn init<'a, 'b>() -> (Command, App<'a, 'b>) {
    (
        crate::cmd::Command::Attached {
            name: "bulk",
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
            run: bulk,
        },
        BulkArgs::clap(),
    )
}
