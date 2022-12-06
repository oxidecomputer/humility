// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility dump`
//!
//! `humility dump` takes a dump of the attached system, writing out an ELF
//! core file:
//!
//! ```console
//! % humility dump
//! humility: attached via ST-Link
//! humility: core halted
//! humility: dumping to hubris.core.0
//! humility: dumped 1.12MB in 24 seconds
//! humility: core resumed
//! ```
//!
//! A dump file name may also be specified:
//!
//! ```console
//! % humility dump hubris.core.`date +%s`
//! humility: attached via ST-Link
//! humility: core halted
//! humility: dumping to hubris.core.1600718079
//! humility: dumped 1.12MB in 24 seconds
//! humility: core resumed
//! ```
//!
//! The resulting dump can be used with many commands (including `manifest`,
//! `map`, `readvar`, and `tasks`) -- and need not be run on the same machine
//! as the debugged MCU, e.g.:
//!
//! ```console
//! % humility -d hubris.core.0 tasks
//! humility: attached to dump
//! ID ADDR     TASK               GEN STATE
//!  0 20000168 jefe                 0 Healthy(InRecv(None))
//!  1 200001d8 rcc_driver           0 Healthy(InRecv(None))
//!  2 20000248 gpio_driver          0 Healthy(InRecv(None))
//!  3 200002b8 usart_driver         0 Healthy(InRecv(None))
//!  4 20000328 i2c_driver           0 Healthy(InRecv(None))
//!  5 20000398 user_leds            0 Healthy(InRecv(None))
//!  6 20000408 pong                 0 Healthy(InRecv(None))
//!  7 20000478 ping                40 Healthy(InReply(TaskId(0x3)))
//!  8 200004e8 adt7420              0 Healthy(InRecv(Some(TaskId(0xffff))))
//!  9 20000558 idle                 0 Healthy(Runnable)          <-
//! ```
//!

use anyhow::Result;
use clap::Command as ClapCommand;
use clap::{CommandFactory, Parser};
use humility::cli::Subcommand;
use humility_cmd::{Archive, Attach, Command, Validate};

#[derive(Parser, Debug)]
#[clap(name = "dump", about = env!("CARGO_PKG_DESCRIPTION"))]
struct DumpArgs {
    dumpfile: Option<String>,
}

fn dumpcmd(context: &mut humility::ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let hubris = context.archive.as_ref().unwrap();

    let subargs = DumpArgs::try_parse_from(subargs)?;

    core.halt()?;
    humility::msg!("core halted");

    let rval = hubris.dump(core, subargs.dumpfile.as_deref());

    core.run()?;
    humility::msg!("core resumed");

    rval
}

pub fn init() -> (Command, ClapCommand<'static>) {
    (
        Command::Attached {
            name: "dump",
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
            run: dumpcmd,
        },
        DumpArgs::command(),
    )
}
