// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility validate`
//!
//! `humility validate` uses the Hubris `validate` task to validate the
//! correct presence of devices as described by the application TOML.  To
//! view all devices, use the `--list` option; to validate them, run
//! without any additional arguments:
//!
//! ```console
//! % humility validate
//! humility: attached via ST-Link V3
//! ID VALIDATION   C P  MUX ADDR DEVICE        DESCRIPTION
//!  0 removed      2 F  -   0x48 tmp117        Southwest temperature sensor
//!  1 removed      2 F  -   0x49 tmp117        South temperature sensor
//!  2 removed      2 F  -   0x4a tmp117        Southeast temperature sensor
//!  3 present      2 F  -   0x70 pca9545       U.2 ABCD mux
//!  4 present      2 F  -   0x71 pca9545       U.2 EFGH mux
//!  5 present      2 F  -   0x72 pca9545       U.2 IJ/FRUID mux
//!  6 timeout      2 B  -   0x73 pca9545       M.2 mux
//!  7 timeout      2 B  1:4 0x4c tmp451        T6 temperature sensor
//!  8 validated    3 H  -   0x24 tps546b24a    A2 3.3V rail
//!  9 validated    3 H  -   0x26 tps546b24a    A0 3.3V rail
//! 10 validated    3 H  -   0x27 tps546b24a    A2 5V rail
//! 11 validated    3 H  -   0x29 tps546b24a    A2 1.8V rail
//! 12 present      3 H  -   0x3a max5970       M.2 hot plug controller
//! 13 absent       3 H  -   0x4c sbtsi         CPU temperature sensor
//! 14 present      3 H  -   0x58 idt8a34003    Clock generator
//! 15 validated    3 H  -   0x5a raa229618     CPU power controller
//! 16 validated    3 H  -   0x5b raa229618     SoC power controller
//! 17 validated    3 H  -   0x5c isl68224      DIMM/SP3 1.8V A0 power controller
//! 18 validated    4 F  -   0x10 adm1272       Fan hot swap controller
//! 19 validated    4 F  -   0x14 adm1272       Sled hot swap controller
//! 20 validated    4 F  -   0x20 max31790      Fan controller
//! 21 validated    4 F  -   0x25 tps546b24a    T6 power controller
//! 22 removed      4 F  -   0x48 tmp117        Northeast temperature sensor
//! 23 removed      4 F  -   0x49 tmp117        North temperature sensor
//! 24 validated    4 F  -   0x4a tmp117        Northwest temperature sensor
//! 25 validated    4 F  -   0x67 bmr491        Intermediate bus converter
//! ```
//!

use anyhow::Result;
use clap::{CommandFactory, Parser};
use colored::Colorize;
use hif::*;
use humility::cli::Subcommand;
use humility::hubris::*;
use humility_cmd::hiffy::HiffyContext;
use humility_cmd::i2c::I2cArgs;
use humility_cmd::idol::{self, HubrisIdol};
use humility_cmd::{Archive, Attach, Command, CommandKind, Validate};

#[derive(Parser, Debug)]
#[clap(name = "validate", about = env!("CARGO_PKG_DESCRIPTION"))]
struct ValidateArgs {
    /// sets timeout
    #[clap(
        long, short = 'T', default_value_t = 5000, value_name = "timeout_ms",
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

    /// specifies a device name to validate
    #[clap(long, short = 'd', value_name = "device")]
    device: Option<String>,

    /// specifies a device identifier to validate
    #[clap(long, short = 'i', value_name = "id",
        conflicts_with_all = &["port", "controller", "bus", "device"]
    )]
    id: Option<usize>,
}

fn list(hubris: &HubrisArchive, hargs: &Option<I2cArgs>) -> Result<()> {
    println!(
        "{:2} {:>2} {:2} {:3} {:4} {:13} DESCRIPTION",
        "ID", "C", "P", "MUX", "ADDR", "DEVICE"
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
            "{:2} {:2} {:2} {:3} 0x{:02x} {:13} {}",
            ndx,
            device.controller,
            device.port.name,
            mux,
            device.address,
            device.device,
            device.description
        );
    }

    Ok(())
}
fn validate(context: &mut humility::ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let subargs = ValidateArgs::try_parse_from(subargs)?;
    let hubris = context.archive.as_ref().unwrap();

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
    let op = hubris.get_idol_command("Validate.validate_i2c")?;
    let mut ops = vec![];

    let mut devices = vec![];

    for (ndx, device) in hubris.manifest.i2c_devices.iter().enumerate() {
        if let Some(id) = subargs.id {
            if ndx != id {
                continue;
            }
        }

        if let Some(ref hargs) = hargs {
            if !hargs.matches_device(device) {
                continue;
            }
        } else if let Some(ref d) = subargs.device {
            if device.device != *d {
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
        "{:2} {:11} {:>2} {:2} {:3} {:4} {:13} DESCRIPTION",
        "ID", "VALIDATION", "C", "P", "MUX", "ADDR", "DEVICE"
    );

    let ok = hubris.lookup_enum(op.ok)?;

    for (rndx, (ndx, device)) in devices.iter().enumerate() {
        let result = match &results[rndx] {
            Ok(val) => {
                if let Some(variant) = ok.lookup_variant_by_tag(val[0].into()) {
                    match variant.name.as_str() {
                        "Present" => "present".yellow(),
                        "Validated" => "validated".green(),
                        _ => format!("<{}>", variant.name).cyan(),
                    }
                } else {
                    hubris.printfmt(val, op.ok, fmt)?.white()
                }
            }
            Err(e) => {
                match op.error.unwrap().lookup_variant_by_tag(*e as u64) {
                    Some(variant) => match variant.name.as_str() {
                        "NotPresent" => {
                            if device.removable {
                                "removed".blue()
                            } else {
                                "absent".red()
                            }
                        }
                        "BadValidation" => "failed".red(),
                        "DeviceTimeout" => "timeout".red(),
                        "DeviceError" => "error".red(),
                        "Unavailable" => "unavailable".yellow(),
                        _ => format!("<{}>", variant.name).red(),
                    },
                    None => format!("Err(0x{:x?})", e).red(),
                }
            }
        };

        let mux = match (device.mux, device.segment) {
            (Some(m), Some(s)) => format!("{}:{}", m, s),
            (None, None) => "-".to_string(),
            (_, _) => "?:?".to_string(),
        };

        println!(
            "{:2} {:11} {:2} {:2} {:3} 0x{:02x} {:13} {}",
            ndx,
            result,
            device.controller,
            device.port.name,
            mux,
            device.address,
            device.device,
            device.description
        );
    }

    Ok(())
}

pub fn init() -> Command {
    Command {
        app: ValidateArgs::command(),
        name: "validate",
        run: validate,
        kind: CommandKind::Attached {
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
        },
    }
}
