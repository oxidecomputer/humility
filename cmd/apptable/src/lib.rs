// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility apptable`
//!
//! Hubris encodes the applications at build time by creating a
//! `.hubris_app_table` section in the kernel ELF binary.  `humility apptable`
//! allows this table to be printed and formatted.  As with other Humility
//! commands, `humility apptable` can run on an archive (or dump):
//!
//! ```console
//! % humility apptable
//! App = {
//!         magic: 0x1defa7a1,
//!         task_count: 0x4,
//!         region_count: 0x9,
//!         irq_count: 0x0,
//!         fault_notification: 0x1,
//!         zeroed_expansion_space: [
//!             0x0,
//!             0x0,
//!             0x0,
//!             0x0,
//!             0x0,
//!             0x0,
//!             0x0,
//!             0x0,
//!             0x0,
//!             0x0,
//!             0x0,
//!             0x0
//!         ]
//!     }
//!
//! RegionDesc[0x0] = {
//!         base: 0x0,
//!         size: 0x20,
//!         attributes: RegionAttributes {
//!             bits: 0x0
//!         },
//!         reserved_zero: 0x0
//!     }
//! ...
//! ```
//!
//! `humility apptable` can also operate directly on a kernel in lieu of an
//! archive or dump by providing the kernel ELF file as an argument:
//!
//! ```console
//! % humility apptable ~/hubris/target/demo/dist/kernel
//! App = {
//!         magic: 0x1defa7a1,
//!         task_count: 0x7,
//!         region_count: 0x13,
//!         irq_count: 0x1,
//!         fault_notification: 0x1,
//!         zeroed_expansion_space: [
//!             0x0,
//!             0x0,
//!             0x0,
//!             0x0,
//!             0x0,
//!             0x0,
//!             0x0,
//!             0x0,
//!             0x0,
//!             0x0,
//!             0x0,
//!             0x0
//!         ]
//!     }
//! ...
//! ```
//!

use anyhow::{bail, Result};
use humility::hubris::{HubrisArchive, HubrisPrintFormat};
use humility_cmd::{Archive, Args, Command};
use std::convert::TryInto;
use structopt::clap::App;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(name = "apptable", about = env!("CARGO_PKG_DESCRIPTION"))]
struct ApptableArgs {
    #[structopt(
        help = "path to kernel ELF object (in lieu of Hubris archive)"
    )]
    kernel: Option<String>,
}

#[rustfmt::skip::macros(println, bail)]
fn apptablecmd(
    hubris: &mut HubrisArchive,
    _args: &Args,
    subargs: &[String],
) -> Result<()> {
    let subargs = ApptableArgs::from_iter_safe(subargs)?;

    if !hubris.loaded() {
        if let Some(ref kernel) = subargs.kernel {
            if let Err(err) = hubris.load_kernel(kernel) {
                bail!("can't load {}: {:?}", kernel, err);
            }
        } else {
            bail!("must provide an archive, dump or kernel");
        }
    }

    let app = hubris.lookup_struct_byname("App")?;
    let task = hubris.lookup_struct_byname("TaskDesc")?;
    let region = hubris.lookup_struct_byname("RegionDesc")?;
    let interrupt = hubris.lookup_struct_byname("Interrupt")?;
    let fmt = HubrisPrintFormat {
        indent: 4,
        newline: true,
        hex: true,
        ..HubrisPrintFormat::default()
    };
    let apptable = hubris.apptable();

    macro_rules! appbail {
        ($msg:expr, $expected:expr) => {
            bail!(
                "short app table on {}: found {} bytes, expected at least {}",
                $msg,
                apptable.len(),
                $expected
            );
        };
    }

    if app.size > apptable.len() {
        appbail!("App header", app.size);
    }

    let lookup = |m| -> Result<u32> {
        let o = app.lookup_member(m)?.offset;
        Ok(u32::from_le_bytes(apptable[o..o + 4].try_into().unwrap()))
    };

    let task_count = lookup("task_count")?;
    let region_count = lookup("region_count")?;
    let irq_count = lookup("irq_count")?;

    println!(
        "App ={}\n",
        hubris.printfmt(&apptable[0..app.size], app.goff, &fmt)?
    );

    let mut offs = app.size;

    for i in 0..region_count {
        let str = format!("RegionDesc[0x{:x}]", i);

        if offs + region.size > apptable.len() {
            appbail!(&str, offs + region.size);
        }

        println!("{} ={}\n", str, hubris.printfmt(
            &apptable[offs..offs + region.size],
            region.goff,
            &fmt
        )?);

        offs += region.size;
    }

    for i in 0..task_count {
        let str = format!("TaskDesc[0x{:x}]", i);

        if offs + task.size > apptable.len() {
            appbail!(&str, offs + task.size);
        }

        println!("{} ={}\n", str, hubris.printfmt(
            &apptable[offs..offs + task.size],
            task.goff,
            &fmt)?
        );

        offs += task.size;
    }

    for i in 0..irq_count {
        let str = format!("Interrupt[0x{:x}]", i);

        if offs + interrupt.size > apptable.len() {
            appbail!(&str, offs + interrupt.size);
        }

        println!("{} ={}\n", str, hubris.printfmt(
            &apptable[offs..offs + interrupt.size],
            interrupt.goff,
            &fmt)?
        );

        offs += interrupt.size;
    }

    Ok(())
}

pub fn init<'a, 'b>() -> (Command, App<'a, 'b>) {
    (
        Command::Unattached {
            name: "apptable",
            archive: Archive::Optional,
            run: apptablecmd,
        },
        ApptableArgs::clap(),
    )
}
