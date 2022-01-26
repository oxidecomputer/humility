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
use clap::Command as ClapCommand;
use clap::{CommandFactory, Parser};
use humility_cmd::{Archive, Command};

#[derive(Parser, Debug)]
#[clap(name = "manifest", about = env!("CARGO_PKG_DESCRIPTION"))]
struct ManifestArgs {}

fn manifestcmd(context: &mut humility::ExecutionContext) -> Result<()> {
    let hubris = context.archive.as_ref().unwrap();
    hubris.manifest()?;
    Ok(())
}

pub fn init() -> (Command, ClapCommand<'static>) {
    (
        Command::Unattached {
            name: "manifest",
            archive: Archive::Required,
            run: manifestcmd,
        },
        ManifestArgs::command(),
    )
}
