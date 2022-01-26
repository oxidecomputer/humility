// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use humility::core::Core;
use humility::hubris::*;
use humility_cmd::hiffy::*;
use humility_cmd::i2c::I2cArgs;
use humility_cmd::{Archive, Args, Attach, Command, Validate};

use anyhow::{bail, Result};
use clap::{App, IntoApp, Parser};
use hif::*;
use indicatif::{ProgressBar, ProgressStyle};
use std::collections::HashMap;
use std::fs::{self, OpenOptions};
use std::io::Write;

#[macro_use]
extern crate log;

#[derive(Parser, Debug)]
#[clap(name = "renbb", about = env!("CARGO_PKG_DESCRIPTION"))]
struct RenbbArgs {
    /// sets timeout
    #[clap(
        long, short, default_value = "5000", value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// specifies an I2C bus by name
    #[clap(long, short, value_name = "bus",
        conflicts_with_all = &["port", "controller"]
    )]
    bus: Option<String>,

    /// specifies an I2C controller
    #[clap(long, short, value_name = "controller",
        parse(try_from_str = parse_int::parse),
    )]
    controller: Option<u8>,

    /// specifies an I2C controller port
    #[clap(long, short, value_name = "port")]
    port: Option<String>,

    /// specifies I2C multiplexer and segment
    #[clap(long, short, value_name = "mux:segment")]
    mux: Option<String>,

    /// specifies an I2C device address
    #[clap(long, short = 'd', value_name = "address")]
    device: Option<String>,

    /// specifies a device by rail name
    #[clap(long, short = 'r', value_name = "rail")]
    rail: Option<String>,

    /// dump all device memory
    #[clap(long, short = 'D')]
    dump: bool,
}

fn all_commands(
    device: pmbus::Device,
) -> HashMap<String, (u8, pmbus::Operation, pmbus::Operation)> {
    let mut all = HashMap::new();

    for i in 0..=255u8 {
        device.command(i, |cmd| {
            all.insert(
                cmd.name().to_string(),
                (i, cmd.read_op(), cmd.write_op()),
            );
        });
    }

    all
}

fn renbb(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    _args: &Args,
    subargs: &[String],
) -> Result<()> {
    let subargs = RenbbArgs::try_parse_from(subargs)?;

    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;
    let funcs = context.functions()?;
    let i2c_read = funcs.get("I2cRead", 7)?;
    let i2c_write = funcs.get("I2cWrite", 8)?;

    let hargs = match (&subargs.rail, &subargs.device) {
        (Some(rail), None) => {
            let mut found = None;

            for device in &hubris.manifest.i2c_devices {
                if let HubrisI2cDeviceClass::Pmbus { rails } = &device.class {
                    for r in rails {
                        if rail == r {
                            found = match found {
                                Some(_) => {
                                    bail!("multiple devices match {}", rail);
                                }
                                None => Some(device),
                            }
                        }
                    }
                }
            }

            match found {
                None => {
                    bail!("rail {} not found", rail);
                }
                Some(device) => I2cArgs::from_device(device),
            }
        }

        (_, _) => I2cArgs::parse(
            hubris,
            &subargs.bus,
            subargs.controller,
            &subargs.port,
            &subargs.mux,
            &subargs.device,
        )?,
    };

    let device = match hargs.device {
        Some(device) => match pmbus::Device::from_str(&device) {
            Some(device) => device,
            None => {
                bail!("no driver found for {}", device);
            }
        },
        None => {
            bail!("no PMBus device found");
        }
    };

    let all = all_commands(device);

    let dmaaddr = match all.get("DMAADDR") {
        Some((code, _, write)) => {
            if *write != pmbus::Operation::WriteWord {
                bail!("DMAADDR mismatch: found {:?}", write);
            }
            *code
        }
        _ => {
            bail!("no DMAADDR command found; is this a Renesas device?");
        }
    };

    let dmaseq = match all.get("DMASEQ") {
        Some((code, read, _)) => {
            if *read != pmbus::Operation::ReadWord32 {
                bail!("DMASEQ mismatch: found {:?}", read);
            }
            *code
        }
        _ => {
            bail!("no DMASEQ command found; is this a Renesas device?");
        }
    };

    let mut base = vec![Op::Push(hargs.controller), Op::Push(hargs.port.index)];

    if let Some(mux) = hargs.mux {
        base.push(Op::Push(mux.0));
        base.push(Op::Push(mux.1));
    } else {
        base.push(Op::PushNone);
        base.push(Op::PushNone);
    }

    if let Some(address) = hargs.address {
        base.push(Op::Push(address));
    } else {
        bail!("expected device");
    }

    if subargs.dump {
        let blocksize = 128u8;
        let nblocks = 8;
        let memsize = 256 * 1024usize;
        let laps = memsize / (blocksize as usize * nblocks);
        let mut addr = 0;

        let bar = ProgressBar::new(memsize as u64);

        let mut filename;
        let mut i = 0;

        let filename = loop {
            filename = format!("hubris.renbb.dump.{}", i);

            if let Ok(_f) = fs::File::open(&filename) {
                i += 1;
                continue;
            }

            break filename;
        };

        let mut file =
            OpenOptions::new().write(true).create_new(true).open(&filename)?;

        info!("dumping device memory to {}", filename);

        bar.set_style(ProgressStyle::default_bar().template(
            "humility: dumping device memory \
                          [{bar:30}] {bytes}/{total_bytes}",
        ));

        for lap in 0..laps {
            let mut ops = base.clone();

            //
            // If this is our first lap through, set our address to be 0
            //
            if lap == 0 {
                ops.push(Op::Push(dmaaddr));
                ops.push(Op::Push(0));
                ops.push(Op::Push(0));
                ops.push(Op::Push(2));
                ops.push(Op::Call(i2c_write.id));
                ops.push(Op::DropN(4));
            }

            ops.push(Op::Push(dmaseq));
            ops.push(Op::Push(blocksize));

            //
            // Unspeakably lazy, but also much less complicated:  we just
            // unroll our loop here.
            //
            for _ in 0..nblocks {
                ops.push(Op::Call(i2c_read.id));
            }

            //
            // Kick it off
            //
            ops.push(Op::Done);

            let results = context.run(core, ops.as_slice(), None)?;

            let start = if lap == 0 {
                match results[0] {
                    Err(err) => {
                        bail!(
                            "failed to set address: {}",
                            i2c_write.strerror(err)
                        )
                    }
                    Ok(_) => 1,
                }
            } else {
                0
            };

            for result in &results[start..] {
                match result {
                    Ok(val) => {
                        file.write_all(val)?;
                        addr += val.len();
                        bar.set_position(addr as u64);
                    }
                    Err(err) => {
                        bail!("{:?}", err);
                    }
                }
            }
        }
    }

    Ok(())
}

pub fn init() -> (Command, App<'static>) {
    (
        Command::Attached {
            name: "renbb",
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
            run: renbb,
        },
        RenbbArgs::into_app(),
    )
}
