// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility jefe`
//!
//! Humility allows for some (well-defined) manipulation of tasks via `jefe`,
//! the Hubris supervisor.  By default, `jefe` will carry out a configured set
//! of responses if tasks fault -- restarting them unless configured otherwise.
//! The policy that governs Jefe's response to task failure is called the
//! _disposition._ While debugging, it can be useful to change Jefe's
//! disposition on a particular task, so that the task will not automatically
//! restart on fault.  This is done with the `-H`/`--hold` option:
//!
//! ```console
//! % humility jefe --hold ping
//! humility: attached via ST-Link
//! humility: successfully changed disposition for ping
//! ```
//!
//! This doesn't make any immediate changes to the task, but merely updates a
//! table inside `jefe`. When the task next faults, it will not be restarted,
//! e.g.:
//!
//! ```console
//! % humility tasks ping
//! humility: attached via ST-Link
//! system time = 26597
//! ID TASK                 GEN PRI STATE    
//!  8 ping                 121   4 FAULT: divide by zero (was: ready)
//! ```
//!
//! This can be particularly useful to couple with either `humility dump` or
//! `humility tasks -sl`:
//!
//! ```console
//! % humility tasks -sl ping
//! humility: attached via ST-Link
//! system time = 103879
//! ID TASK                 GEN PRI STATE    
//!  8 ping                 121   4 FAULT: divide by zero (was: ready)
//!    |
//!    +--->  0x200065b0 0x0802a05e task_ping::divzero
//!                      @ /home/bmc/hubris/task/ping/src/main.rs:24
//!           0x20006600 0x0802a0fe userlib::sys_panic
//!                      @ /home/bmc/hubris/sys/userlib/src/lib.rs:678
//!           0x20006600 0x0802a0fe main
//!                      @ /home/bmc/hubris/task/ping/src/main.rs:35
//!
//! ```
//!
//! To change the disposition of a task (back) to be restarted, use the
//! `--release`/`-r` flag:
//!
//! ```console
//! % humility jefe --release ping
//! humility: attached via ST-Link
//! humility: successfully changed disposition for ping
//! ```
//!
//! `--release` undoes the effect of `--hold`. If the task is parked at a fault,
//! it will be restarted immediately.  Otherwise just the disposition will be
//! updated and the task will be left alone.
//!
//! If you would like to cause a task to restart, use the `--start`/`-s` flag.
//! This will restart a running task immediately, and will re-run a held task
//! *without releasing the hold.* This means the task will still be caught at
//! the next fault, giving you a way of precisely trying an operation again.
//!
//! To inject a fault into a task, use the `--fault`/`-f` flag. This will cause
//! `jefe` to use Kernel Trickery(tm) to inject a fault into the target task,
//! and also switch its disposition to "held" so you can inspect the result.
//! The injected fault has a dedicated type to make clear that the fault
//! originated outside of the task:
//!
//! ```console
//! % humility jefe --fault pong
//! humility: attached via ST-Link
//! humility: successfully changed disposition for pong
//! % humility tasks pong
//! humility: attached via ST-Link
//! system time = 191227
//! ID TASK                 GEN PRI STATE    
//!  7 pong                   0   3 FAULT: killed by jefe/gen0 (was: recv, notif: bit0)
//!    |
//!    +--->  0x200063b8 0x08028c0a userlib::sys_recv_stub
//!                      @ /home/bmc/hubris/sys/userlib/src/lib.rs:288
//!           0x20006400 0x080280ac userlib::sys_recv
//!                      @ /home/bmc/hubris/sys/userlib/src/lib.rs:236
//!           0x20006400 0x080280ba main
//!                      @ /home/bmc/hubris/task/pong/src/main.rs:13
//! ```
//!
//! As with tasks held by `--hold`, use `--release`/`-r` to set the task back to
//! normal, or `--start`/`-s` to run it once but catch the next fault.

use anyhow::{anyhow, bail, Result};
use clap::{CommandFactory, Parser};
use humility::cli::Subcommand;
use humility::hubris::*;
use humility_cmd::jefe::{send_request, JefeRequest};
use humility_cmd::{Archive, Attach, Command, CommandKind, Validate};
use std::num::NonZeroU32;

#[derive(Parser, Debug)]
#[clap(name = "jefe", about = env!("CARGO_PKG_DESCRIPTION"))]
struct JefeArgs {
    /// sets timeout
    #[clap(
        long, short, default_value_t = 5000, value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// fault the specified task
    #[clap(long, short, conflicts_with_all = &["start", "release", "hold"])]
    fault: bool,

    /// start the specified task
    #[clap(long, short, conflicts_with_all = &["release", "hold"])]
    start: bool,

    /// hold the specified task
    #[clap(long, short = 'H', conflicts_with = "release")]
    hold: bool,

    /// release the specified task
    #[clap(long, short)]
    release: bool,

    task: String,
}

fn jefe(context: &mut humility::ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let hubris = context.archive.as_ref().unwrap();

    let subargs = JefeArgs::try_parse_from(subargs)?;

    let request = if subargs.fault {
        JefeRequest::Fault
    } else if subargs.start {
        JefeRequest::Start
    } else if subargs.hold {
        JefeRequest::Hold
    } else if subargs.release {
        JefeRequest::Release
    } else {
        bail!("one of fault, start, hold, or release must be specified");
    };

    let task = hubris
        .lookup_task(&subargs.task)
        .ok_or_else(|| anyhow!("couldn't find task {}", subargs.task))?;

    let id = match task {
        HubrisTask::Kernel => {
            bail!("cannot change disposition of kernel");
        }
        HubrisTask::Task(id) => {
            if let Some(id) = NonZeroU32::new(*id) {
                id
            } else {
                bail!("cannot change disposition of supervisor task");
            }
        }
    };

    send_request(hubris, core, request, id, subargs.timeout)?;

    humility::msg!("successfully changed disposition for {}", subargs.task);

    Ok(())
}

pub fn init() -> Command {
    Command {
        app: JefeArgs::command(),
        name: "jefe",
        run: jefe,
        kind: CommandKind::Attached {
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
        },
    }
}
