// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility stackmargin`
//!
//! `humility stackmargin` calculates and print stack margins by task. The
//! margin is determined by walking up each stack, looking for the first
//! word that does not contain the uninitialized pattern (`0xbaddcafe`),
//! from which it infers maximum depth, and therefore margin:
//!
//! ```console
//! % humility -d ./hubris.core.10 stackmargin
//! humility: attached to dump
//! ID TASK                STACKBASE  STACKSIZE   MAXDEPTH     MARGIN
//!  0 jefe               0x20001000       1024        768        256
//!  1 rcc_driver         0x20001400       1024        176        848
//!  2 usart_driver       0x20001800       1024        216        808
//!  3 user_leds          0x20001c00       1024        208        816
//!  4 ping               0x20002000        512        224        288
//!  5 pong               0x20002400       1024        208        816
//!  6 idle               0x20002800        256        104        152
//! ```
//!
//! Note that the margin is only valid for the task's lifetime -- and in
//! particular, will not be correct if the task has restarted due to a
//! stack overflow!
//!

use anyhow::{bail, Result};
use clap::{CommandFactory, Parser};
use humility::hubris::*;
use humility_cmd::{Archive, Attach, Command, CommandKind, Validate};
use std::convert::TryInto;

#[derive(Parser, Debug)]
#[clap(name = "stackmargin", about = env!("CARGO_PKG_DESCRIPTION"))]
struct StackmarginArgs {}

#[rustfmt::skip::macros(println, bail)]
fn stackmargin(context: &mut humility::ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let hubris = context.archive.as_ref().unwrap();

    let regions = hubris.regions(core)?;

    let (base, size) = hubris.task_table(core)?;
    let task = hubris.lookup_struct_byname("Task")?;
    let taskdesc = hubris.lookup_struct_byname("TaskDesc")?;

    let mut taskblock: Vec<u8> = vec![];
    taskblock.resize_with(task.size * size as usize, Default::default);
    core.read_8(base, taskblock.as_mut_slice())?;

    let descriptor = task.lookup_member("descriptor")?.offset as u32;
    let initial_stack = taskdesc.lookup_member("initial_stack")?.offset as u32;

    println!("{:2} {:18} {:>10} {:>10} {:>10} {:>10}",
        "ID", "TASK", "STACKBASE", "STACKSIZE", "MAXDEPTH", "MARGIN");

    let taskblock32 =
        |o| u32::from_le_bytes(taskblock[o..o + 4].try_into().unwrap());

    let find = |addr| {
        for (_, region) in regions.iter() {
            if addr > region.base && addr <= region.base + region.mapsize {
                return Ok(region);
            }
        }

        bail!(format!("could not find region for address {:x}", addr));
    };

    for i in 0..size {
        let offs = i as usize * task.size;
        let daddr = taskblock32(offs + descriptor as usize);
        let initial = core.read_word_32(daddr + initial_stack)?;

        let module = hubris.lookup_module(HubrisTask::Task(i))?;
        let region = find(initial)?;

        if region.tasks.len() != 1 || region.tasks[0] != module.task {
            bail!(format!(
                "mismatched task on 0x{:x}: expected {:?}, found {:?}",
                initial, module.task, region.tasks)
            )
        }

        let size = (initial - region.base) as usize;
        let mut stack: Vec<u8> = vec![];
        stack.resize_with(size, Default::default);
        core.read_8(region.base, stack.as_mut_slice())?;

        let mut o = 0;

        let depth = loop {
            let c = u32::from_le_bytes(stack[o..o + 4].try_into().unwrap());

            if c != 0xbaddcafe || o + 4 >= size {
                break size - o;
            }

            o += 4;
        };

        println!("{:2} {:18} 0x{:<8x} {:10} {:10} {:10}",
            i, module.name, region.base,
            size, depth, size - depth);
    }

    Ok(())
}

pub fn init() -> Command {
    Command {
        app: StackmarginArgs::command(),
        name: "stackmargin",
        run: stackmargin,
        kind: CommandKind::Attached {
            archive: Archive::Required,
            attach: Attach::Any,
            validate: Validate::Booted,
        },
    }
}
