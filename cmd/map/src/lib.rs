// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility map`
//!
//! One common pathology in Hubris tasks is a fault induced when a task
//! attempts to access a memory address outside of its designated regions.
//! (This can happen, for example, because the task attempted to access device
//! memory that was not allocated to it in the build description, because it
//! exceeded the device memory allocated to it, or because it allocated the
//! memory allocated to it, e.g., because of a stack overflow.) `humility
//! tasks` can be useful to see this happening; a non-zero generation count
//! will indicate that a task has been restarted -- and the log message from
//! `jefe` will indicate the specific address, e.g.:
//!
//! ```console
//! % humility itm -ea
//! humility: attached via OpenOCD
//! humility: core halted
//! humility: core resumed
//! humility: TPIU sync packet found at offset 1
//! humility: ITM synchronization packet found at offset 12
//! Task #7 Memory fault at address 0x200028fc
//! Task #7 Memory fault at address 0x200028fc
//! ^C
//! ```
//!
//! To better understand the memory that a task is trying to access, one can
//! run the `humility map` command, which shows the memory regions that have
//! been mapped into tasks, in address order:
//!
//! ```console
//! % humility -a ~/hubris/target/demo/dist/build-demo.zip map
//! humility: attached via OpenOCD
//! DESC       LOW          HIGH          SIZE ATTR  ID TASK
//! 0x08004864 0x08010000 - 0x08017fff   32KiB r-x--  0 jefe
//! 0x08004884 0x08018000 - 0x08019fff    8KiB r-x--  1 rcc_driver
//! 0x080048a4 0x0801c000 - 0x0801ffff   16KiB r-x--  2 usart_driver
//! 0x080048c4 0x08020000 - 0x08023fff   16KiB r-x--  3 user_leds
//! 0x080048e4 0x08024000 - 0x08025fff    8KiB r-x--  4 ping
//! 0x08004904 0x08026000 - 0x08027fff    8KiB r-x--  5 pong
//! 0x08004924 0x08028000 - 0x080280ff     256 r-x--  6 idle
//! 0x08004944 0x0802a000 - 0x0802bfff    8KiB r-x--  7 oh_no
//! 0x08004964 0x0802c000 - 0x0802dfff    8KiB r-x--  8 oh_no2
//! 0x08004874 0x20001000 - 0x200013ff    1KiB rwx--  0 jefe
//! 0x08004894 0x20001400 - 0x200017ff    1KiB rwx--  1 rcc_driver
//! 0x080048b4 0x20001800 - 0x20001bff    1KiB rwx--  2 usart_driver
//! 0x080048d4 0x20001c00 - 0x20001fff    1KiB rwx--  3 user_leds
//! 0x080048f4 0x20002000 - 0x200021ff     512 rwx--  4 ping
//! 0x08004914 0x20002400 - 0x200027ff    1KiB rwx--  5 pong
//! 0x08004934 0x20002800 - 0x200028ff     256 rwx--  6 idle
//! 0x08004954 0x20002900 - 0x200029ff     256 rwx--  7 oh_no
//! 0x08004974 0x20002a00 - 0x20002aff     256 rwx--  8 oh_no2
//! 0x08004824 0x40004400 - 0x400047ff    1KiB rw-d-  2 usart_driver
//! 0x08004844 0x40020000 - 0x400203ff    1KiB rw-d-  2 usart_driver
//! 0x08004854 0x40020c00 - 0x40020fff    1KiB rw-d-  3 user_leds
//! 0x08004834 0x40023800 - 0x40023bff    1KiB rw-d-  1 rcc_driver
//! ```
//!
//! (In this case, task 7, `oh_no`, has overflowed its stack -- which
//! we can see from the `map` output has been sized to only 256 bytes.)

use anyhow::Result;
use clap::{CommandFactory, Parser};
use humility_cmd::{Archive, Attach, Command, CommandKind, Validate};

#[derive(Parser, Debug)]
#[clap(name = "map", about = env!("CARGO_PKG_DESCRIPTION"))]
struct MapArgs {}

fn mapcmd(context: &mut humility::ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    core.op_start()?;

    let hubris = context.archive.as_ref().unwrap();

    let regions = hubris.regions(core)?;
    core.op_done()?;

    println!(
        "{:10} {:10}   {:10} {:>7} {:5} {:2} TASK",
        "DESC", "LOW", "HIGH", "SIZE", "ATTR", "ID",
    );

    for (_, region) in regions.iter() {
        let name = {
            let mut names = vec![];

            for t in &region.tasks {
                names.push(hubris.lookup_module(*t)?.name.clone());
            }

            names.join(", ")
        };

        println!(
            "{:10} 0x{:08x} - 0x{:08x} {:>7} {}{}{}{}{} {:2} {}",
            match region.daddr {
                Some(daddr) => format!("0x{:08x}", daddr),
                None => "-".to_owned(),
            },
            region.base,
            region.base + region.mapsize - 1,
            if region.mapsize >= 1024 {
                format!("{}KiB", region.mapsize >> 10)
            } else {
                format!("{}", region.mapsize)
            },
            if region.attr.read { "r" } else { "-" },
            if region.attr.write { "w" } else { "-" },
            if region.attr.execute { "x" } else { "-" },
            if region.attr.device { "d" } else { "-" },
            if region.attr.dma { "m" } else { "-" },
            region.tasks[0].id(),
            if region.attr.device {
                if let Some(p) = hubris.lookup_peripheral_byaddr(region.base) {
                    format!("[{}] {}", p, name)
                } else {
                    format!("[??] {}", name)
                }
            } else {
                name.to_string()
            }
        );
    }

    Ok(())
}

/// This is some init right here
pub fn init() -> Command {
    Command {
        app: MapArgs::command(),
        name: "map",
        run: mapcmd,
        kind: CommandKind::Attached {
            archive: Archive::Required,
            attach: Attach::Any,
            validate: Validate::Booted,
        },
    }
}
