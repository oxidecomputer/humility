// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility powershelf`
//!
//! `humility powershelf` allows for remotely dumping the state of the PSC
//! power shelves, via the same network mechanism as `humility rpc`.
//!
//! This command has the same requirements as `humility rpc` and uses the same
//! underlying mechanisms: the Hubris `udprpc` task should be listening on port
//! 8 and the matching Hubris archive is required. See the `humility rpc`
//! documentation for more details.
//!
//! This command is currently hard-coded to support only the MWOCP68, and it
//! dumps 50+ properties described in the ACAN-114 application note. It will
//! only dump the properties from a single shelf+rail combination, so seeing
//! properties of all 6 shelves requires calling this command 6 time (with
//! indices 0 through 5).

use cmd_hiffy as humility_cmd_hiffy;

use anyhow::{anyhow, Context, Result};
use clap::IntoApp;
use clap::Parser;
use hif::*;
use humility::cli::Subcommand;
use humility::hubris::HubrisArchive;
use humility::hubris::HubrisEnum;
use humility_cmd::hiffy::*;
use humility_cmd::idol::{self, HubrisIdol};
use humility_cmd::CommandKind;
use humility_cmd::{Archive, Attach, Command, Validate};

#[derive(Parser, Debug)]
#[clap(name = "powershelf", about = env!("CARGO_PKG_DESCRIPTION"))]
struct PowershelfArgs {
    /// sets timeout
    #[clap(
        long, short = 'T', default_value_t = 5000, value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// index of the power shelf to inspect
    #[clap(long, default_value_t = 0)]
    index: u64,

    /// rail of the power shelf to inspect
    #[clap(long, default_value_t = 0)]
    rail: u64,

    /// verbose output
    #[clap(long, short)]
    verbose: bool,
}

// Find the `task_power_api::Operation` enum.
fn lookup_operation_enum(hubris: &HubrisArchive) -> Result<&HubrisEnum> {
    let power_task = hubris.lookup_task("power").ok_or_else(|| {
        anyhow!("missing `power` task - is this a PSC archive?")
    })?;

    let module = hubris.lookup_module(*power_task)?;

    // `Operation` matches multiple enums, and `task_power_api::Operation`
    // doesn't show up alone, but `Option<task_power_api::Operation>` does! Find
    // that, then drill down to get the enum we want out.
    let opt_operation = module
        .lookup_enum_byname(hubris, "Option<task_power_api::Operation>")
        .context("Could not look up task_power_api::Operation")?;
    let some_operation_goff = opt_operation
        .variants
        .iter()
        .find_map(|v| if v.name == "Some" { v.goff } else { None })
        .expect("Option<_> missing `Some(_)` variant");

    let some_operation = hubris.lookup_struct(some_operation_goff)?;
    let operation_goff = some_operation
        .newtype()
        .ok_or_else(|| anyhow!("Some(_) variant is not a newtype struct"))?;

    let operation = hubris.lookup_enum(operation_goff)?;

    Ok(operation)
}

fn interpret_raw_variant(variant: &str, payload: &[u8]) {
    use pmbus::{CommandCode, Device, VOutModeCommandData};

    // Slightly gross: Hard-code our device and VOUT_MODE for mwocp68. Should we
    // instead add VOUT_MODE to the list of operations? How would we know if we
    // were inspecting something other than an mwocp68?
    let device = Device::Mwocp68;
    let vout_mode = VOutModeCommandData::from_slice(&[0x17]).unwrap();

    let code = match variant {
        "FanConfig1_2" => CommandCode::FAN_CONFIG_1_2,
        "StatusByte" => CommandCode::STATUS_BYTE,
        "StatusWord" => CommandCode::STATUS_WORD,
        "StatusVout" => CommandCode::STATUS_VOUT,
        "StatusIout" => CommandCode::STATUS_IOUT,
        "StatusInput" => CommandCode::STATUS_INPUT,
        "StatusTemperature" => CommandCode::STATUS_TEMPERATURE,
        "StatusCml" => CommandCode::STATUS_CML,
        "StatusMfrSpecific" => CommandCode::STATUS_MFR_SPECIFIC,
        "StatusFans1_2" => CommandCode::STATUS_FANS_1_2,
        "PmbusRevision" => CommandCode::PMBUS_REVISION,
        _ => {
            println!("     | Missing command code mapping! Update humility");
            return;
        }
    };

    let result = device.interpret(
        code as u8,
        payload,
        || vout_mode,
        |field, value| {
            let (pos, width) = field.bits();

            let bits = if width.0 == 1 {
                format!("b{}", pos.0)
            } else {
                format!("b{}:{}", pos.0 + width.0 - 1, pos.0)
            };

            let value = format!("{}", value);

            println!("     | {:6} {:<34} <= {}", bits, value, field.name());
        },
    );

    if let Err(err) = result {
        println!("     | Error interpreting fields: {err:?}");
    }
}

fn powershelf_run(context: &mut humility::ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let hubris = context.archive.as_ref().unwrap();

    let subargs = PowershelfArgs::try_parse_from(subargs)?;

    let operation = lookup_operation_enum(hubris)?;

    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;
    let mut ops = vec![];
    let funcs = context.functions()?;

    let idol_cmd = hubris
        .get_idol_command("Power.pmbus_read")
        .context("missing `Power.pmbus_read` - is this a PSC archive?")?;

    // 3 out of 4 args are the same for every variant; build a vec of the 4 args
    // here, and we'll modify the first arg in the loop below.
    let mut args = vec![
        ("op", idol::IdolArgument::String("PLACEHOLDER")),
        ("dev", idol::IdolArgument::String("PowerShelf")),
        ("rail", idol::IdolArgument::Scalar(subargs.rail)),
        ("index", idol::IdolArgument::Scalar(subargs.index)),
    ];

    for variant in &operation.variants {
        args[0].1 = idol::IdolArgument::String(&variant.name);
        let payload = idol_cmd.payload(&args)?;
        context.idol_call_ops(&funcs, &idol_cmd, &payload, &mut ops)?;
    }

    ops.push(Op::Done);

    let results = context.run(core, ops.as_slice(), None)?;

    for (ndx, variant) in operation.variants.iter().enumerate() {
        let result = humility_cmd_hiffy::hiffy_decode(
            hubris,
            &idol_cmd,
            results[ndx].clone(),
        )?;

        println!(
            "{:<20} => {}",
            variant.name,
            humility_cmd_hiffy::hiffy_format_result(hubris, result.clone())
        );

        if subargs.verbose {
            // We only verbosely print successful results whose values are
            // `Raw8` or `Raw16`; all other value types are already sufficiently
            // explained.
            if let Ok(value) = result {
                let value = value.as_enum()?;

                // Helper enum to unwrap `value` if it is one of the `Raw8(u8)`
                // or `Raw16(u16)` enum variants: extract the contents as a
                // 1tuple then pull out the base value.
                let unwrap_to_base = || {
                    value
                        .contents()
                        .unwrap()
                        .as_1tuple()
                        .unwrap()
                        .as_base()
                        .unwrap()
                };

                match value.disc() {
                    "Raw8" => {
                        let x = unwrap_to_base().as_u8().unwrap();
                        interpret_raw_variant(&variant.name, &[x]);
                    }
                    "Raw16" => {
                        let x = unwrap_to_base().as_u16().unwrap();
                        interpret_raw_variant(&variant.name, &x.to_le_bytes());
                    }
                    _ => (),
                }
            }
        }
    }

    Ok(())
}

pub fn init() -> Command {
    Command {
        app: PowershelfArgs::command(),
        name: "powershelf",
        run: powershelf_run,
        kind: CommandKind::Attached {
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
        },
    }
}
