// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility manifest`
//!
//! `humility manifest` displays information about the Hubris archive.  It
//! does not connect at all to a Hubris target to operate.  In addition to
//! archive-wide attributes, `humility manifest` displays the features enabled
//! for each task, as well as any device toplogy information present in
//! the archive, e.g.:
//!
//! ```console
//! % humility manifest
//!      version => hubris build archive v1.0.0
//!      git rev => 753a57169eba699e73ee59e0cf5345eb1d6e1ae2-dirty
//!        board => nucleo-h743zi2
//!         name => demo-stm32h753-nucleo
//!       target => thumbv7em-none-eabihf
//!     features => h743, itm
//!   total size => 140K
//!  kernel size => 30K
//!        tasks => 12
//!                 ID TASK                SIZE FEATURES
//!                  0 hiffy              42.9K h743, stm32h7, itm, i2c, gpio, qspi
//!                  1 jefe                6.7K itm
//!                  2 i2c_driver          9.8K h743
//!                  3 spi_driver         10.7K spi3, h743
//!                  4 hf                  8.6K h743
//!                  5 rcc_driver          4.7K h743
//!                  6 gpio_driver         5.8K h743
//!                  7 usart_driver        5.9K h743
//!                  8 user_leds           5.5K stm32h7
//!                  9 pong                4.7K
//!                 10 ping                5.2K uart
//!                 11 idle                0.1K
//!    i2c buses => 1 controller, 1 bus
//!                 C PORT MODE NAME          DESCRIPTION
//!                 2 F    init -             -
//! ```
//!
//! `humility manifest` can operate on either an archive or on a dump.

use anyhow::Result;
use clap::{CommandFactory, Parser};
use humility::hubris::*;
use humility_cmd::{Archive, Command, CommandKind};
use std::collections::HashSet;

#[derive(Parser, Debug)]
#[clap(name = "manifest", about = env!("CARGO_PKG_DESCRIPTION"))]
struct ManifestArgs {}

#[allow(clippy::print_literal)]
fn manifestcmd(context: &mut humility::ExecutionContext) -> Result<()> {
    let hubris = context.archive.as_ref().unwrap();
    let manifest = &hubris.manifest;

    let print = |what, val| {
        println!("{:>12} => {}", what, val);
    };

    let size = |task| hubris.lookup_module(task).unwrap().memsize;

    print(
        "version",
        match &manifest.version {
            Some(s) => s,
            None => "<unknown>",
        },
    );

    print(
        "git rev",
        match &manifest.gitrev {
            Some(s) => s,
            None => "<unknown>",
        },
    );

    println!(
        "{:>12} => {}",
        "image id",
        match &hubris.imageid {
            Some(s) => {
                format!("{:x?}", s.1)
            }
            None => "<none>".to_string(),
        },
    );

    print(
        "board",
        match &manifest.board {
            Some(s) => s,
            None => "<unknown>",
        },
    );

    print(
        "name",
        match &manifest.name {
            Some(s) => s,
            None => "<unknown>",
        },
    );

    print(
        "target",
        match &manifest.target {
            Some(s) => s,
            None => "<unknown>",
        },
    );

    print("features", &manifest.features.join(", "));

    let ttl = hubris.modules().fold(0, |ttl, m| ttl + m.memsize);

    println!("{:>12} => {}K", "total size", ttl / 1024);
    println!("{:>12} => {}K", "kernel size", size(HubrisTask::Kernel) / 1024);
    println!("{:>12} => {}", "tasks", hubris.ntasks());
    println!("{:>18} {:18} {:>5} {}", "ID", "TASK", "SIZE", "FEATURES");

    let mut id = 0;

    for module in hubris.modules() {
        if module.task == HubrisTask::Kernel {
            continue;
        }

        let features = manifest.task_features.get(&module.name);

        println!(
            "{:>18} {:18} {:>4.1}K {}",
            id,
            module.name,
            module.memsize as f64 / 1024_f64,
            if let Some(f) = features { f.join(", ") } else { "".to_string() }
        );

        id += 1;
    }

    if !manifest.i2c_buses.is_empty() {
        let mut controllers = HashSet::new();

        for bus in &manifest.i2c_buses {
            controllers.insert(bus.controller);
        }

        println!(
            "{:>12} => {} controller{}, {} bus{}",
            "i2c buses",
            controllers.len(),
            if controllers.len() != 1 { "s" } else { "" },
            manifest.i2c_buses.len(),
            if manifest.i2c_buses.len() != 1 { "es" } else { "" },
        );

        println!(
            "{:>17} {} {} {:13} {}",
            "C", "PORT", "MODE", "NAME", "DESCRIPTION"
        );

        for bus in &manifest.i2c_buses {
            println!(
                "{:>17} {:4} {:4} {:13} {}",
                bus.controller,
                bus.port.name,
                if bus.target { "trgt" } else { "init" },
                bus.name.as_ref().unwrap_or(&"-".to_string()),
                bus.description.as_ref().unwrap_or(&"-".to_string()),
            );
        }
    }

    if !manifest.i2c_devices.is_empty() {
        println!(
            "{:>12} => {} device{}",
            "i2c devices",
            manifest.i2c_devices.len(),
            if manifest.i2c_devices.len() != 1 { "s" } else { "" }
        );

        println!(
            "{:>19} {:2} {:2} {} {} {:13} {}",
            "ID", "C", "P", "MUX", "ADDR", "DEVICE", "DESCRIPTION"
        );

        for (ndx, device) in manifest.i2c_devices.iter().enumerate() {
            let mux = match (device.mux, device.segment) {
                (Some(m), Some(s)) => format!("{}:{}", m, s),
                (None, None) => "-".to_string(),
                (_, _) => "?:?".to_string(),
            };

            println!(
                "{:>19} {:2} {:2} {:3} 0x{:02x} {:13} {}",
                ndx,
                device.controller,
                device.port.name,
                mux,
                device.address,
                device.device,
                device.description
            );
        }
    }

    if let Some(auxflash) = manifest.auxflash.as_ref() {
        const ONE_MIB: usize = 1024 * 1024;

        print!("{:>12} => {} bytes", "aux flash", auxflash.memory_size);

        if auxflash.memory_size % ONE_MIB == 0 {
            print!(" ({} MiB)", auxflash.memory_size / ONE_MIB);
        }

        print!(", {} slots", auxflash.slot_count);

        let bytes_per_slot = auxflash.memory_size / auxflash.slot_count;
        if bytes_per_slot % ONE_MIB == 0 {
            println!(" ({} MiB/slot)", bytes_per_slot / ONE_MIB);
        } else {
            println!(" ({} bytes/slot)", bytes_per_slot);
        }
    }

    if !manifest.sensors.is_empty() {
        println!(
            "{:>12} => {} sensor{}",
            "sensors",
            manifest.sensors.len(),
            if manifest.sensors.len() > 1 { "s" } else { "" }
        );
        println!("{:>19} {:23} {:11} {}", "ID", "NAME", "DEVICE", "KIND");

        for (ndx, s) in manifest.sensors.iter().enumerate() {
            let device = match &s.device {
                HubrisSensorDevice::I2c(ndx) => format!("i2c id={}", ndx),
                HubrisSensorDevice::Other(dev, _) => dev.to_string(),
            };
            println!(
                "                {:3} {:23} {:11} {}",
                ndx,
                s.name,
                device,
                s.kind.to_string()
            );
        }
    }

    Ok(())
}

pub fn init() -> Command {
    Command {
        app: ManifestArgs::command(),
        name: "manifest",
        run: manifestcmd,
        kind: CommandKind::Unattached { archive: Archive::Required },
    }
}
