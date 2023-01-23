// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility spctrl`
//!
//! `humility spctrl` runs commands on the RoT to control the SP.
//!
//! You must run `humility spctrl init` before any other commands
//!
//! ```console
//! % humility spctrl init
//! [Ok([])]
//! ```
//!
//! You can read/write memory on the SP via the RoT
//! ```console
//! % humility spctrl -W read 0x08000000 64
//! humility: attached via CMSIS-DAP
//!                    \/        4        8        c
//! 0x08000000 | 20000400 08000299 08003b6d 08004271 | ... ....m;..qB..
//! 0x08000010 | 08003c8d 08003ccd 08003cd3 00000000 | .<...<...<......
//! 0x08000020 | 00000000 00000000 00000000 0800398b | .............9..
//! 0x08000030 | 08003b6d 00000000 08003aa9 080039dd | m;.......:...9..
//!
//! % humility spctrl -W read 0x00000000 64
//! humility: attached via CMSIS-DAP
//!                    \/        4        8        c
//! 0x00000000 | 3d0fbf49 991373d9 9107611c f6d84242 | I..=.s...a..BB..
//! 0x00000010 | 742397db c7c60242 decc7515 ce719848 | ..#tB....u..H.q.
//! 0x00000020 | b2d2639e faf8049b e202de8c 2ae12025 | .c..........% .*
//! 0x00000030 | f739ba6f 20067a60 310c4e08 e42eca28 | o.9.`z. .N.1(...
//!
//! % humility spctrl -W write 0x00000000 0x11,0x22,0x33,0x44,0x55,0x66,0x77,0x88
//! humility: attached via CMSIS-DAP
//! [Ok([])]
//!
//! % humility spctrl -W read 0x00000000 64
//! humility: attached via CMSIS-DAP
//!                    \/        4        8        c
//! 0x00000000 | 44332211 88776655 9107611c f6d84242 | ."3DUfw..a..BB..
//! 0x00000010 | 742397db c7c60242 decc7515 ce719848 | ..#tB....u..H.q.
//! 0x00000020 | b2d2639e faf8049b e202de8c 2ae12025 | .c..........% .*
//! 0x00000030 | f739ba6f 20067a60 310c4e08 e42eca28 | o.9.`z. .N.1(...
//! ```

use humility::cli::Subcommand;
use humility_cmd::{hiffy::*, CommandKind};
use humility_cmd::{Archive, Attach, Command, Dumper, Validate};

use std::str;

use anyhow::{bail, Result};
use clap::{CommandFactory, Parser};
use hif::*;

#[derive(Parser, Debug)]
#[clap(name = "subcmd")]
enum SpCtrlCmd {
    Write {
        #[clap(parse(try_from_str = parse_int::parse))]
        addr: u32,
        bytes: String,
    },
    Read {
        #[clap(parse(try_from_str = parse_int::parse))]
        addr: u32,
        nbytes: usize,
    },
    Init,
}

#[derive(Parser, Debug)]
#[clap(name = "spctrl", about = env!("CARGO_PKG_DESCRIPTION"))]
struct SpCtrlArgs {
    /// sets timeout
    #[clap(
        long, short = 'T', default_value_t = 5000, value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    #[clap(subcommand)]
    cmd: SpCtrlCmd,

    /// print out data read as words rather than bytes
    #[clap(long, short = 'W')]
    word: bool,
}

fn spctrl(context: &mut humility::ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let subargs = SpCtrlArgs::try_parse_from(subargs)?;
    let hubris = context.archive.as_ref().unwrap();
    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;
    let funcs = context.functions()?;
    let mut ops = vec![];

    match subargs.cmd {
        SpCtrlCmd::Write { addr, bytes } => {
            let bytes: Vec<&str> = bytes.split(',').collect();
            let mut arr = vec![];
            for byte in &bytes {
                if let Ok(val) = parse_int::parse::<u8>(byte) {
                    arr.push(val);
                } else {
                    bail!("invalid byte {}", byte)
                }
            }
            ops.push(Op::Push32(addr as u32));
            ops.push(Op::Push32(arr.len() as u32));

            let sp_write = funcs.get("WriteToSp", 2)?;
            ops.push(Op::Call(sp_write.id));
            ops.push(Op::Done);

            let results = context.run(core, ops.as_slice(), Some(&arr))?;

            println!("{:x?}", results);
        }
        SpCtrlCmd::Read { addr, nbytes } => {
            ops.push(Op::Push32(addr as u32));
            ops.push(Op::Push32(nbytes as u32));
            let sp_read = funcs.get("ReadFromSp", 2)?;
            ops.push(Op::Call(sp_read.id));
            ops.push(Op::Done);

            let results = context.run(core, ops.as_slice(), None)?;

            if let Ok(results) = &results[0] {
                let mut dumper = Dumper::new();
                dumper.size = if subargs.word { 4 } else { 1 };
                dumper.dump(results, addr);

                return Ok(());
            } else {
                println!("{:x?}", results);
            }
        }
        SpCtrlCmd::Init => {
            let init = funcs.get("SpCtrlInit", 0)?;
            ops.push(Op::Call(init.id));
            ops.push(Op::Done);

            let results = context.run(core, ops.as_slice(), None)?;

            println!("{:?}", results);
        }
    }

    Ok(())
}

pub fn init() -> Command {
    Command {
        app: SpCtrlArgs::command(),
        name: "spctrl",
        run: spctrl,
        kind: CommandKind::Attached {
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
        },
    }
}
