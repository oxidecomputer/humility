// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility validate`
//!
//! Validates your parking.
//!

use anyhow::{Context, Result};
use clap::Command as ClapCommand;
use clap::{CommandFactory, Parser};
use hif::*;
use humility::core::Core;
use humility::hubris::*;
use humility_cmd::hiffy::*;
use humility_cmd::idol;
use humility_cmd::{Archive, Args, Attach, Command, Validate};
use humility_cmd::i2c::I2cArgs;

#[derive(Parser, Debug)]
#[clap(name = "sensors", about = env!("CARGO_PKG_DESCRIPTION"))]
struct ValidateArgs {
    /// sets timeout
    #[clap(
        long, short = 'T', default_value = "5000", value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// list all devices to be validated
    #[clap(long, short)]
    list: bool,

    /// specifies an I2C controller to validate
    #[clap(long, short, value_name = "controller",
        parse(try_from_str = parse_int::parse),
    )]
    controller: Option<u8>,

    /// specifies an I2C bus by name to validate
    #[clap(long, short, value_name = "bus",
        conflicts_with_all = &["port", "controller"]
    )]
    bus: Option<String>,

    /// specifies an I2C controller port to validate
    #[clap(long, short, value_name = "port", requires = "controller")]
    port: Option<String>,

    /// specifies I2C multiplexer and segment to validate
    #[clap(long, short, value_name = "mux:segment", requires = "controller")]
    mux: Option<String>,

    /// specifies I2C multiplexer and segment to validate
    #[clap(long, short, value_name = "index", 
        conflicts_with_all = &["port", "controller", "bus", "device"]
    )]
    index: Option<usize>,

    /// specifies a device name to validate
    #[clap(long, short = 'd', value_name = "device")]
    device: Option<String>,
}

fn list(
    hubris: &HubrisArchive,
    hargs: &Option<I2cArgs>,
) -> Result<()> {
    println!(
        "{:2} {:<4} {:>2} {:2} {:3} {:4} {:13}",
        "ID", "TYPE", "C", "P", "MUX", "ADDR", "DEVICE"
    );

    for (ndx, device) in hubris.manifest.i2c_devices.iter().enumerate() {
        if let Some(hargs) = hargs {
            if !hargs.matches_device(device) {
                continue;
            }
        }

        let mux = match (device.mux, device.segment) {
            (Some(m), Some(s)) => format!("{}:{}", m, s),
            (None, None) => "-".to_string(),
            (_, _) => "?:?".to_string(),
        };

        println!(
            "{:2}  i2c {:2} {:2} {:3} 0x{:02x} {:13}",
            ndx,
            device.controller,
            device.port.name,
            mux,
            device.address,
            device.device,
        );
    }

    Ok(())
}

fn validate(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    _args: &Args,
    subargs: &[String],
) -> Result<()> {
    let subargs = ValidateArgs::try_parse_from(subargs)?;

    let hargs = if subargs.bus.is_some() || subargs.controller.is_some() {
        Some(I2cArgs::parse(
            hubris,
            &subargs.bus,
            subargs.controller,
            &subargs.port,
            &subargs.mux,
            &None,
        )?)
    } else {
        None
    };

    if subargs.list {
        list(hubris, &hargs)?;
        return Ok(());
    }

    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;
    let funcs = context.functions()?;
    let op = idol::IdolOperation::new(hubris, "Validate", "validate_i2c", None)
        .context("is the 'validate' task present?")?;
    let mut ops = vec![];

    let mut devices = vec![];

    for (ndx, device) in hubris.manifest.i2c_devices.iter().enumerate() {
        if let Some(index) = subargs.index {
            if ndx != index {
                continue;
            }
        }

        if let Some(ref hargs) = hargs {
            if !hargs.matches_device(device) {
                continue;
            }
        }

        devices.push((ndx, device));

        let payload =
            op.payload(&[("index", idol::IdolArgument::Scalar(ndx as u64))])?;
        context.idol_call_ops(&funcs, &op, &payload, &mut ops)?;
    }

    ops.push(Op::Done);

    let results = context.run(core, ops.as_slice(), None)?;

    let fmt = HubrisPrintFormat {
        newline: false,
        hex: true,
        ..HubrisPrintFormat::default()
    };

    println!(
        "{:2} {:<4} {:>2} {:2} {:3} {:4} {:13} {}",
        "ID", "TYPE", "C", "P", "MUX", "ADDR", "DEVICE", "VALIDATION"
    );

    for (rndx, (ndx, device)) in devices.iter().enumerate() {
        let result = match &results[rndx] {
            Ok(val) => {
                hubris.printfmt(val, op.ok, &fmt)?
            }
            Err(e) => {
                match op.error.unwrap().lookup_variant(*e as u64) {
                    Some(variant) => format!("{}", variant.name),
                    None => format!("Err(0x{:x?})", e),
                }
            }
        };

        let mux = match (device.mux, device.segment) {
            (Some(m), Some(s)) => format!("{}:{}", m, s),
            (None, None) => "-".to_string(),
            (_, _) => "?:?".to_string(),
        };

        println!(
            "{:2}  i2c {:2} {:2} {:3} 0x{:02x} {:13} {}",
            ndx,
            device.controller,
            device.port.name,
            mux,
            device.address,
            device.device,
            result,
        );
    }

    Ok(())
}

pub fn init() -> (Command, ClapCommand<'static>) {
    (
        Command::Attached {
            name: "validate",
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
            run: validate,
        },
        ValidateArgs::command(),
    )
}
