// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility pmbus`
//!
//! Operates on PMBus devices in the system.  To list all PMBus devices, use
//! `--list` (`-l`):
//!
//! ```console
//! $ humility pmbus --list
//! C P  MUX ADDR DEVICE        RAILS
//! 3 H  -   0x24 tps546b24a    V3P3_SP_A2
//! 3 H  -   0x26 tps546b24a    V3P3_SYS_A0
//! 3 H  -   0x27 tps546b24a    V5_SYS_A2
//! 3 H  -   0x29 tps546b24a    V1P8_SYS_A2
//! 3 H  -   0x5a raa229618     VDD_VCORE, VDD_MEM_ABCD
//! 3 H  -   0x5b raa229618     VDDCR_SOC, VDD_MEM_EFGH
//! 3 H  -   0x5c isl68224      VPP_ABCD, VPP_EFGH, V1P8_SP3
//! 4 F  -   0x10 adm1272       V54_FAN
//! 4 F  -   0x14 adm1272       V54_HS_OUTPUT
//! 4 F  -   0x25 tps546b24a    V0P96_NIC_VDD_A0HP
//! 4 F  -   0x67 bmr491        V12_SYS_A2
//! ```
//!
//! To query a particular device, specify the device by its I2C identity
//! (controller/port, segment, address):
//!
//! ```console
//! $ humility pmbus -d 0x67 -c 4 -p f
//! 0x01 OPERATION                 0x84
//! 0x02 ON_OFF_CONFIG             0x18
//! 0x10 WRITE_PROTECT             0x00
//! 0x19 CAPABILITY                0xb0
//! 0x20 VOUT_MODE                 0x15
//! 0x21 VOUT_COMMAND              0x6000 = 12.000V
//! 0x22 VOUT_TRIM                 0x0000 = 0.000V
//! 0x23 VOUT_CAL_OFFSET           0xffb4 = 31.963V
//! 0x24 VOUT_MAX                  0x7333 = 14.400V
//! 0x25 VOUT_MARGIN_HIGH          0x699a = 13.200V
//! 0x26 VOUT_MARGIN_LOW           0x5666 = 10.800V
//! 0x27 VOUT_TRANSITION_RATE      0x9b02 = 0.094V/ms
//! 0x28 VOUT_DROOP                0xe800 = 0.000mV/A
//! ...
//! ```
//!
//! In the unusual case that a device is unknown to the system (that is, it does
//! not appear in `humliity manifest`), you can force a particular PMBus driver
//! by using `--driver` (`-D`).
//!
//! For the common case of devices known to the system, you can specify a device
//! by name if it matches a single device in the system (e.g., `humility pmbus
//! -d bmr491`).  In lieu of specifying a device, you can specify a rail via
//! `--rail` (`-r`), e.g.:
//!
//! ```console
//! $ humility pmbus --rail VDD_MEM_EFGH
//! humility: attached via ST-Link V3
//! 0x00 PAGE                      0x01 = 1
//! 0x01 OPERATION                 0x48
//! 0x02 ON_OFF_CONFIG             0x16
//! 0x04 PHASE                     0x00 = 0
//! 0x07 ZONE_CONFIG               0xfefe
//! 0x08 ZONE_ACTIVE               0x0000
//! 0x10 WRITE_PROTECT             0x00
//! 0x19 CAPABILITY                0xd4
//! 0x20 VOUT_MODE                 0x40
//! 0x21 VOUT_COMMAND              0x04ce = 1.230V
//! 0x22 VOUT_TRIM                 0x0000 = 0.000V
//! 0x23 VOUT_CAL_OFFSET           0x0000 = 0.000V
//! 0x24 VOUT_MAX                  0x05dc = 1.500V
//! 0x25 VOUT_MARGIN_HIGH          0x03b1 = 0.945V
//! 0x26 VOUT_MARGIN_LOW           0x0357 = 0.855V
//! 0x27 VOUT_TRANSITION_RATE      0x09c4 = 0.025V/μs
//! 0x28 VOUT_DROOP                0x0014 = 0.200mV/A
//! ...
//! ```
//!
//! You can specify a command (or commands) to run with `--command` (`-C`);
//! this can be useful when paired with the `--verbose` (`-v`) flag to
//! deconstruct a particular command result:
//!
//! ```console
//! $ humility pmbus -r VDD_MEM_ABCD --command STATUS_WORD --verbose
//! humility: attached via ST-Link V3
//! 0x79 STATUS_WORD               0x0000
//!      |
//!      | b15    0b0 = no fault                 <= OutputVoltageFault
//!      | b14    0b0 = no fault                 <= OutputCurrentFault
//!      | b13    0b0 = no fault                 <= InputFault
//!      | b12    0b0 = no fault                 <= ManufacturerFault
//!      | b11    0b0 = POWER_GOOD set           <= PowerGoodStatus
//!      | b10    0b0 = no fault                 <= FanFault
//!      | b9     0b0 = no fault                 <= OtherFault
//!      | b8     0b0 = no fault                 <= UnknownFault
//!      | b7     0b0 = no fault                 <= Busy
//!      | b6     0b0 = power is not off         <= Off
//!      | b5     0b0 = no fault                 <= OutputOvervoltageFault
//!      | b4     0b0 = no fault                 <= OutputOvercurrentFault
//!      | b3     0b0 = no fault                 <= InputUndervoltageFault
//!      | b2     0b0 = no fault                 <= TemperatureFault
//!      | b1     0b0 = no fault                 <= CMLFault
//!      | b0     0b0 = no fault                 <= NoneOfTheAbove
//!      +-----------------------------------------------------------------------
//! ```
//!
//! You can also write a PMBus command with `--write` (`-w`), which allows for
//! for particular fields to be written, e.g.:
//!
//! ```console
//! $ humility pmbus -r VDD_VCORE -w OPERATION.MarginFaultResponse=ActUpon
//! humility: attached via ST-Link V3
//! humility: I2C3, port H, dev 0x5a, rail 0: successfully wrote OPERATION
//! ```
//!
//! It should go without saying that this should be done carefully!
//!
//! To get specific help on what fields may be written and the legal values for
//! those fields, use `--commandhelp` (`-H`):
//!
//! ```console
//! $ humility pmbus -r VDD_VCORE --commandhelp OPERATION
//! 0x01 OPERATION
//!      | b7     OnOffState                     <= On/off state
//!      |        0b0 = Off                      <- output off
//!      |        0b1 = On                       <- output on
//!      | b6     TurnOffBehavior                <= Power down behavior
//!      |        0b0 = Clear                    <- powers down immediately
//!      |        0b1 = Set                      <- powers down based on TOFF_DELAY
//!      | b5:4   VoltageCommandSource           <= Source of output voltage
//!      |        0b00 = VOUT_COMMAND            <- set by VOUT_COMMAND
//!      |        0b01 = VOUT_MARGIN_LOW         <- set by VOUT_MARGIN_LOW
//!      |        0b10 = VOUT_MARGIN_HIGH        <- set by VOUT_MARGIN_HIGH
//!      |        0b11 = AVS_VOUT_COMMAND        <- set by AVSBus
//!      | b3:2   MarginFaultResponse            <= Margin fault response
//!      |        0b01 = Ignore                  <- ignore margin faults
//!      |        0b10 = ActUpon                 <- act upon margin faults
//!      | b1     TransitionControl              <= Transition control
//!      |        0b0 = Clear                    <- not set
//!      |        0b1 = Set                      <- set
//!      +-----------------------------------------------------------------------
//! ```
//!
//! To get a summary of all PMBus rails in the system, use `--summarize` (`-s`):
//!
//! ```console
//! $ humility pmbus --summarize
//! humility: attached via ST-Link V3
//! DEVICE      RAIL               PG? #FLT       VIN      VOUT      IOUT    TEMP_1
//! tps546b24a  V3P3_SP_A2           Y    0   11.969V    3.311V    0.404A  33.500°C
//! tps546b24a  V3P3_SYS_A0          Y    0   11.969V    3.309V    1.457A  36.250°C
//! tps546b24a  V5_SYS_A2            Y    0   11.969V    4.982V    0.518A  36.000°C
//! tps546b24a  V1P8_SYS_A2          Y    0   11.984V    1.797V    3.316A  34.750°C
//! raa229618   VDD_VCORE            Y    0   11.990V    1.181V   53.200A  40.000°C
//! raa229618   VDD_MEM_ABCD         Y    0   11.990V    1.224V   28.600A  41.000°C
//! raa229618   VDDCR_SOC            Y    0   11.950V    0.890V   18.200A  42.000°C
//! raa229618   VDD_MEM_EFGH         Y    0   11.950V    1.223V   27.400A  43.000°C
//! isl68224    VPP_ABCD             Y    0   11.950V    2.500V    0.400A  40.000°C
//! isl68224    VPP_EFGH             Y    0   11.950V    2.499V    0.500A  30.000°C
//! isl68224    V1P8_SP3             Y    0   11.950V    1.796V    1.600A   0.000°C
//! adm1272     V54_FAN              Y    4    0x088c    0x088c    0x0810  30.452°C
//! adm1272     V54_HS_OUTPUT        Y    4    0x088c    0x088b    0x092e  30.690°C
//! tps546b24a  V0P96_NIC_VDD_A0HP   Y    0   11.984V    0.955V    3.074A  37.500°C
//! bmr491      V12_SYS_A2           Y    1   53.625V   11.995V   19.250A  35.750°C
//! ```
//!
//! Note that for some devices, it is not possible to get accurate voltage and
//! current readings from `pmbus` alone, as knowledge of how the device is
//! integrated into a larger system is required to interpret raw values.  For
//! these devices, the raw value is provided (as in the `adm1272` output,
//! above); to get the interpreted value, use `humility power` instead (which
//! has the added advantage of displaying all power rails in the systemn, not
//! just PMBus devices.)
//!

use colored::Colorize;
use humility::core::Core;
use humility::hubris::*;
use humility_cli::{ExecutionContext, Subcommand};
use humility_cmd::{Archive, Attach, Command, CommandKind, Validate};
use humility_hiffy::*;
use humility_i2c::I2cArgs;
use humility_idol::{HubrisIdol, IdolArgument, IdolOperation};

use anyhow::{anyhow, bail, Result};
use clap::{CommandFactory, Parser};
use hif::*;
use indexmap::IndexMap;
use pmbus::commands::*;
use pmbus::*;
use std::collections::{BTreeMap, HashMap};
use std::fmt::Write;

#[derive(Parser, Debug)]
#[clap(name = "pmbus", about = env!("CARGO_PKG_DESCRIPTION"))]
struct PmbusArgs {
    /// sets timeout
    #[clap(
        long, short, default_value_t = 5000, value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// list PMBus components
    #[clap(
        long, short,conflicts_with_all = &[
            "driver", "controller", "port", "bus", "summarize"
        ]
    )]
    list: bool,

    /// summarize PMBus components
    #[clap(
        long, short,
        conflicts_with_all = &["driver", "controller", "port", "bus"]
    )]
    summarize: bool,

    /// command-specific help
    #[clap(long, short = 'H', value_name = "command")]
    commandhelp: Option<Vec<String>>,

    /// verbose output
    #[clap(long, short)]
    verbose: bool,

    /// show errors
    #[clap(long, short)]
    errors: bool,

    /// dry-run; show commands instead of running them
    #[clap(long = "dry-run", short = 'n')]
    dryrun: bool,

    /// force unrecognized PMBus device
    #[clap(long, short = 'F')]
    force: bool,

    /// specifies a PMBus driver
    #[clap(long, short = 'D')]
    driver: Option<String>,

    /// specifies command(s) to run
    #[clap(
        long = "command",
        alias = "commands",
        short = 'C',
        conflicts_with = "writes",
        use_value_delimiter = true,
        value_name = "command"
    )]
    commands: Option<Vec<String>>,

    /// specifies writes to perform
    #[clap(long, short = 'w', use_value_delimiter = false)]
    writes: Option<Vec<String>>,

    /// specifies an I2C controller
    #[clap(long, short, value_name = "controller",
        parse(try_from_str = parse_int::parse),
    )]
    controller: Option<u8>,

    /// specifies an I2C bus by name
    #[clap(long, short, value_name = "bus",
        conflicts_with_all = &["port", "controller"]
    )]
    bus: Option<String>,

    /// specifies an I2C controller port
    #[clap(long, short, value_name = "port")]
    port: Option<String>,

    /// specifies I2C multiplexer and segment
    #[clap(long, short, value_name = "mux:segment")]
    mux: Option<String>,

    /// specifies an I2C device address
    #[clap(long, short = 'd', value_name = "address")]
    device: Option<String>,

    /// specifies a rail within the specified device
    #[clap(long, short = 'r', value_name = "rail", use_value_delimiter = true)]
    rail: Option<Vec<String>>,

    /// agent to use when executing PMBus operations
    #[clap(long, arg_enum, default_value_t=Agent::Auto)]
    agent: Agent,
}

#[derive(clap::ArgEnum, Clone, Debug)]
enum Agent {
    Auto,
    Idol,
    I2c,
}

fn all_commands(
    device: pmbus::Device,
) -> (HashMap<String, u8>, HashMap<u8, String>) {
    let mut all: HashMap<String, u8> = HashMap::new();
    let mut bycode: HashMap<u8, String> = HashMap::new();

    for i in 0..=255u8 {
        device.command(i, |cmd| {
            all.insert(cmd.name().to_string(), i);
            bycode.insert(i, cmd.name().to_string());
        });
    }

    (all, bycode)
}

fn print_command(
    device: pmbus::Device,
    code: u8,
    command: &dyn pmbus::Command,
) {
    use std::*;

    println!("0x{:02x} {}", code, command.name());

    let mut bitfields = false;

    let fields = |field: &dyn Field| {
        let bits = field.bits();
        let nbits = bits.1 .0 as usize;

        let b = if nbits == 1 {
            format!("b{}", bits.0 .0)
        } else {
            format!("b{}:{}", bits.0 .0 + bits.1 .0 - 1, bits.0 .0)
        };

        if field.bitfield() {
            bitfields = true;

            println!("     | {:6} {:30} <= {}", b, field.name(), field.desc());

            let mut last = None;

            let sentinels = |val: &dyn Value| {
                let v =
                    format!("0b{:0w$b} = {}", val.raw(), val.name(), w = nbits);

                println!("     | {:6} {:30} <- {}", "", v, val.desc());

                if let Some(last) = last {
                    if last >= val.raw() {
                        panic!("values are out of order");
                    }
                }

                last = Some(val.raw());
            };

            device.sentinels(code, field.bits().0, sentinels).unwrap();
        }
    };

    device.fields(code, fields).unwrap();

    if bitfields {
        println!(
            "     +------------------------------------------\
            -----------------------------\n"
        );
    }
}

#[rustfmt::skip::macros(println)]
fn print_result(
    subargs: &PmbusArgs,
    device: pmbus::Device,
    code: u8,
    mode: impl Fn() -> VOutModeCommandData,
    command: &dyn pmbus::Command,
    result: &Result<Vec<u8>, u32>,
    worker: &dyn PmbusWorker,
) -> Result<()> {
    let nbytes = match command.read_op() {
        pmbus::Operation::ReadByte => Some(1),
        pmbus::Operation::ReadWord => Some(2),
        pmbus::Operation::ReadWord32 => Some(4),
        pmbus::Operation::ReadBlock => None,
        _ => {
            unreachable!();
        }
    };

    let name = command.name();
    let cmdstr = format!("0x{:02x} {:<25}", code, name);

    fn printchar(val: u8) {
        let c = val as char;

        if c.is_ascii() && !c.is_ascii_control() {
            print!("{}", c);
        } else {
            print!(".");
        }
    }

    match result {
        Err(err) => {
            if subargs.errors {
                println!("{} Err({})", cmdstr, worker.decode_read_err(*err));
            }
        }

        Ok(val) => {
            if val.is_empty() && subargs.errors {
                println!("{} Timed out", cmdstr);
                return Ok(());
            }

            if let Some(nbytes) = nbytes {
                if val.len() != nbytes {
                    println!("{} Short read: {:x?}", cmdstr, val);
                    return Ok(());
                }
            }

            let mut printed = false;
            let mut interpreted = false;

            let printraw = |interpret: bool| {
                print!("{}", cmdstr);

                if nbytes.is_none() {
                    let w = 8;

                    for i in 0..val.len() {
                        if i > 0 && i % w == 0 {
                            print!(" |");

                            for &j in val[(i - w)..i].iter() {
                                printchar(j);
                            }

                            if !interpret {
                                print!("\n{:30}", "");
                            } else {
                                print!("\n     | {:22} ", "");
                            }
                        }

                        print!(" 0x{:02x}", val[i]);
                    }

                    let rem = val.len() % w;

                    if rem != 0 {
                        print!("{:width$} |", "", width = (w - rem) * 5);
                        let base = val.len() - rem;

                        for i in 0..rem {
                            printchar(val[base + i]);
                        }
                    }
                } else {
                    print!(" 0x");
                    for i in (0..val.len()).rev() {
                        print!("{:02x}", val[i]);
                    }
                }

                println!();
            };

            let err = device.interpret(code, val, mode, |field, value| {
                if !field.bitfield() {
                    let width = (field.bits().1 .0 / 4) as usize;

                    println!(
                       "{} 0x{:0width$x} = {}",
                       cmdstr, value.raw(), value, width = width
                    );

                    interpreted = true;
                    return;
                }

                if !subargs.verbose {
                    return;
                }

                if !interpreted {
                    printraw(true);
                    println!("     |");
                    interpreted = true;
                }

                let (pos, width) = field.bits();

                let bits = if width.0 == 1 {
                    format!("b{}", pos.0)
                } else {
                    format!("b{}:{}", pos.0 + width.0 - 1, pos.0)
                };

                let value = format!("{}", value);

                println!("     | {:6} {:<30} <= {}", bits, value, field.name());
                printed = true;
            });

            if err.is_err() && subargs.errors {
                println!("{} {:?}", cmdstr, err);
            }

            if !interpreted {
                printraw(false);
            }

            if printed {
                println!(
                    "     +------------------------------------------\
                    -----------------------------\n"
                );
            }
        }
    }

    Ok(())
}

fn prepare_write(
    device: pmbus::Device,
    code: u8,
    mode: impl Fn() -> VOutModeCommandData,
    command: &dyn pmbus::Command,
    payload: &[u8],
    writes: &[(Bitpos, Replacement)],
) -> Result<Vec<u8>> {
    let name = command.name();
    let mut rval = payload.to_vec();
    let mut replaced = vec![false; writes.len()];

    let err = device.mutate(code, &mut rval, mode, |field, _| {
        let pos = field.bits().0;

        for i in 0..writes.len() {
            if writes[i].0 == pos {
                replaced[i] = true;
                return Some(writes[i].1);
            }
        }

        None
    });

    if err.is_err() {
        bail!("failed to mutate {}: {:?}", name, err);
    }

    for i in 0..writes.len() {
        if !replaced[i] {
            bail!("failed to replace {} at position {}", name, writes[i].0 .0);
        }
    }

    Ok(rval.to_vec())
}

fn validate_write(
    device: pmbus::Device,
    cmd: &str,
    code: u8,
    field: Option<&str>,
    value: Option<&str>,
) -> Result<(Bitpos, Replacement)> {
    let value = match value {
        None => {
            bail!("write \"{}\" needs a value, e.g. COMMAND=value", cmd);
        }
        Some(value) => value,
    };

    if let Some(field) = field {
        //
        // Iterate over the fields for this command to make sure we have
        // the specified field
        //
        let mut all = vec![];
        let mut found = None;

        device
            .fields(code, |f| {
                if f.name() == field {
                    found = Some(f.bits());
                }

                all.push(f.name());
            })
            .unwrap();

        match found {
            None => {
                bail!(
                    "field {} not found in {}; expected one of: {}",
                    field,
                    cmd,
                    all.join(", ")
                );
            }
            Some(bits) => {
                let mut replacement: Option<Replacement> = None;
                let mut all = vec![];

                device
                    .sentinels(code, bits.0, |s| {
                        if s.name() == value {
                            replacement = Some(Replacement::Integer(s.raw()));
                        }

                        all.push(s.name());
                    })
                    .unwrap();

                match replacement {
                    Some(replacement) => Ok((bits.0, replacement)),

                    None => {
                        bail!(
                            "field {} of {} cannot be set to {}; \
                            expected one of: {}",
                            field,
                            cmd,
                            value,
                            all.join(", ")
                        )
                    }
                }
            }
        }
    } else {
        let mut bits = None;
        let mut bitfields = false;

        device
            .fields(code, |f| {
                if !f.bitfield() {
                    bits = Some(f.bits());
                } else {
                    bitfields = true;
                }
            })
            .unwrap();

        if let Some(bits) = bits {
            if let Ok(val) = parse_int::parse::<u32>(value) {
                Ok((bits.0, Replacement::Integer(val)))
            } else if let Ok(val) = value.parse::<f32>() {
                Ok((bits.0, Replacement::Float(val)))
            } else {
                bail!("illegal value: {}", value);
            }
        } else if bitfields {
            bail!("{} has bitfields which must be set explicitly", cmd);
        } else {
            bail!("can't write to {}: data has unknown type", cmd);
        }
    }
}

fn split_write(write: &str) -> Result<(&str, Option<&str>, Option<&str>)> {
    let expr: Vec<&str> = write.split('=').collect();

    if expr.len() > 2 {
        bail!("write \"{}\" has an ambiguous value", write);
    }

    if expr.len() < 2 {
        Ok((expr[0], None, None))
    } else {
        let field: Vec<&str> = expr[0].split('.').collect();

        if field.len() > 1 {
            if field.len() > 2 {
                bail!("write \"{}\" has too many field delimiters", write);
            }

            Ok((field[0], Some(field[1]), Some(expr[1])))
        } else {
            Ok((expr[0], None, Some(expr[1])))
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn summarize_rail(
    subargs: &PmbusArgs,
    device: &HubrisI2cDevice,
    driver: &pmbus::Device,
    rail: &str,
    calls: &[u8],
    results: &[Result<Vec<u8>, u32>],
    worker: &dyn PmbusWorker,
    width: usize,
) -> Result<()> {
    let mut base = 0;

    print!("{:11} {rail:18}", device.device);

    if calls[base] == CommandCode::PAGE as u8 {
        //
        // This is a selected rail -- we just want to be sure that it worked
        //
        if let Err(code) = results[base] {
            bail!("rail selection failed: {}", worker.decode_write_err(code));
        }

        base += 1;
    }

    let mode = if calls[base] == CommandCode::VOUT_MODE as u8 {
        match results[base] {
            Err(code) => {
                bail!("can't read VOUT_MODE: {}", worker.decode_read_err(code));
            }
            Ok(ref val) => {
                base += 1;
                Some(VOUT_MODE::CommandData::from_slice(val).unwrap())
            }
        }
    } else {
        None
    };

    let getmode = || match mode {
        Some(mode) => mode,
        None => {
            panic!("unexpected call to VOutMode");
        }
    };

    assert_eq!(calls[base], CommandCode::STATUS_WORD as u8);

    let status = match results[base] {
        Err(_) => None,
        Ok(ref val) => Some(STATUS_WORD::CommandData::from_slice(val).unwrap()),
    };

    print!(
        " {:>3}",
        match status {
            Some(status) => match status.get_power_good_status() {
                Some(STATUS_WORD::PowerGoodStatus::PowerGood) => "Y".green(),
                Some(STATUS_WORD::PowerGoodStatus::NoPowerGood) => "N".red(),
                None => "X".red(),
            },
            None => "-".yellow(),
        }
    );

    let mut faults = vec![];

    print!(
        " {:>4}",
        match status {
            Some(status) => {
                let _ = status.interpret(getmode, |field, value| {
                    if field.name().contains("Fault") && value.raw() != 0 {
                        faults.push(field.desc());
                    }
                });

                let str = format!("{}", faults.len());

                if !faults.is_empty() {
                    str.red()
                } else {
                    str.green()
                }
            }
            None => "-".yellow(),
        }
    );

    base += 1;

    for i in base..calls.len() {
        let code = calls[i];
        match results[i] {
            Err(_) => {
                print!(" {:>width$}", "-", width = width);
            }
            Ok(ref val) => {
                let mut interpreted = false;
                let mut str = String::new();

                let err =
                    driver.interpret(code, val, getmode, |field, value| {
                        if !field.bitfield() {
                            write!(&mut str, "{}", value).unwrap();
                            interpreted = true;
                        }
                    });

                if err.is_err() {
                    print!(" {:>width$?}", err, width = width);
                    continue;
                }

                if !interpreted {
                    write!(&mut str, "0x").unwrap();
                    for i in (0..val.len()).rev() {
                        write!(&mut str, "{:02x}", val[i]).unwrap();
                    }
                }

                print!(" {:>width$}", str, width = width);
            }
        }
    }

    println!();

    if subargs.verbose && !faults.is_empty() {
        println!("{:38}|", "");
        println!("{:38}+--- {}", "", faults[0]);

        for item in faults.iter().skip(1) {
            println!("{:38}     {}", "", item);
        }

        println!();
    }

    Ok(())
}

fn summarize(
    subargs: &PmbusArgs,
    hubris: &HubrisArchive,
    worker: &mut dyn PmbusWorker,
) -> Result<()> {
    let page = CommandCode::PAGE as u8;
    let (all, bycode) = all_commands(pmbus::Device::Common);

    let mut width = 9;
    let mut commands = vec![
        (CommandCode::VOUT_MODE as u8, None),
        (CommandCode::STATUS_WORD as u8, None),
    ];

    if let Some(ref cmds) = subargs.commands {
        for cmd in cmds {
            if let Some(code) = all.get(cmd) {
                commands.push((*code, Some(bycode.get(code).unwrap().as_str())))
            } else {
                bail!("unrecognized command {}", cmd);
            }
        }

        width = 15;
    } else {
        commands.extend_from_slice(&[
            (CommandCode::READ_VIN as u8, Some("VIN")),
            (CommandCode::READ_VOUT as u8, Some("VOUT")),
            (CommandCode::READ_IOUT as u8, Some("IOUT")),
            (CommandCode::READ_TEMPERATURE_1 as u8, Some("TEMP_1")),
        ]);
    }

    let mut work = vec![];

    for device in &hubris.manifest.i2c_devices {
        if let HubrisI2cDeviceClass::Pmbus { rails } = &device.class {
            let driver = match pmbus::Device::from_str(&device.device) {
                Some(device) => device,
                None => pmbus::Device::Common,
            };

            let harg = I2cArgs::from_device(device);
            worker.begin_device(&harg)?;

            //
            // We have the arguments for our device pushed.  Now iterate over
            // each rail, selecting it as needed...
            //
            for (rnum, rail) in rails.iter().enumerate() {
                let mut calls = vec![];

                if rails.len() > 1 {
                    worker.select_rail(rnum as u8);
                    calls.push(page);
                }

                //
                // For each of the commands that we need to run, add a call
                // for it
                //
                for &(code, _) in &commands {
                    driver.command(code, |cmd| {
                        let op = match cmd.read_op() {
                            v @ (pmbus::Operation::ReadByte
                            | pmbus::Operation::ReadWord
                            | pmbus::Operation::ReadWord32
                            | pmbus::Operation::ReadBlock) => v,
                            _ => {
                                return;
                            }
                        };
                        worker.read(code, op);
                        calls.push(code);
                    });
                }

                work.push((device, driver, rail, calls));
            }
            worker.end_device();
        }
    }

    let results = worker.run()?;
    let mut base = 0;

    print!(
        "{:11} {:18} {:3} {:4}",
        "DEVICE".bold(),
        "RAIL".bold(),
        "PG?".bold(),
        "#FLT".bold()
    );

    for (_, header) in commands.iter() {
        if let Some(header) = header {
            print!(" {:>width$}", header.bold(), width = width);
        }
    }

    println!();

    for (device, driver, rail, calls) in &work {
        if let Err(e) = summarize_rail(
            subargs,
            device,
            driver,
            &rail.name,
            calls,
            &results[base..base + calls.len()],
            worker,
            width,
        ) {
            println!(
                " {0}  {1} {2}  {0} ",
                "--".dimmed(),
                "error:".yellow(),
                e,
            );
        }

        base += calls.len();
    }

    Ok(())
}

fn find_rail<'a>(
    hubris: &'a HubrisArchive,
    rail: &str,
) -> Result<(I2cArgs<'a>, Option<u8>)> {
    let mut found = None;

    for device in &hubris.manifest.i2c_devices {
        if let HubrisI2cDeviceClass::Pmbus { rails } = &device.class {
            for (rnum, r) in rails.iter().enumerate() {
                if rail == r.name {
                    found = match found {
                        Some(_) => {
                            bail!("multiple devices match {}", rail);
                        }
                        None => Some((
                            device,
                            if rails.len() > 1 {
                                Some(rnum as u8)
                            } else {
                                None
                            },
                        )),
                    }
                }
            }
        }
    }

    match found {
        None => {
            bail!("rail {} not found", rail);
        }
        Some((device, rail)) => Ok((I2cArgs::from_device(device), rail)),
    }
}

#[derive(Debug)]
enum WriteOp {
    Modify(usize, Vec<(Bitpos, Replacement)>),
    SetBlock(Vec<u8>),
    SetByte(u8),
    SetWord(u16),
    SetWord32(u32),
    Set,
}

impl WriteOp {
    fn from_op(
        device: pmbus::Device,
        op: pmbus::Operation,
        cmd: &str,
        code: u8,
        field: Option<&str>,
        value: Option<&str>,
    ) -> Result<Self> {
        match op {
            pmbus::Operation::SendByte => {
                if value.is_some() {
                    bail!("write of {} cannot take a value", cmd);
                }

                Ok(WriteOp::Set)
            }

            pmbus::Operation::WriteBlock => {
                if field.is_some() {
                    bail!("write {} can only take raw bytes", cmd);
                }

                let bytes: Vec<&str> = match value {
                    None => {
                        bail!(
                            "write {} needs a byte stream, e.g. {}=0x1,0xde",
                            cmd,
                            cmd
                        );
                    }
                    Some(value) => value.split(',').collect(),
                };

                let mut payload = vec![];

                for byte in &bytes {
                    if let Ok(val) = parse_int::parse::<u8>(byte) {
                        payload.push(val);
                    } else {
                        bail!("invalid byte {}", byte)
                    }
                }

                Ok(WriteOp::SetBlock(payload))
            }

            pmbus::Operation::WriteByte
            | pmbus::Operation::WriteWord
            | pmbus::Operation::WriteWord32 => {
                let count = match op {
                    pmbus::Operation::WriteByte => 1,
                    pmbus::Operation::WriteWord => 2,
                    pmbus::Operation::WriteWord32 => 4,
                    _ => {
                        panic!("unexpected operation {:?}", op);
                    }
                };

                Ok(WriteOp::Modify(
                    count,
                    vec![validate_write(device, cmd, code, field, value)?],
                ))
            }

            _ => {
                bail!("{} cannot be written", cmd);
            }
        }
    }
}

fn validate_writes(
    writecmds: &[String],
    device: pmbus::Device,
) -> Result<IndexMap<u8, (String, WriteOp)>> {
    let mut rval = IndexMap::new();
    let mut all = HashMap::new();

    for i in 0..=255u8 {
        device.command(i, |cmd| {
            all.insert(cmd.name().to_string(), (i, cmd.write_op()));
        });
    }

    for write in writecmds {
        let (cmd, field, value) = split_write(write)?;

        if let Some((code, op)) = all.get(cmd) {
            match rval.get_mut(code) {
                Some((_, WriteOp::Modify(_, ref mut writes))) => {
                    assert!(*op != pmbus::Operation::SendByte);
                    writes.push(validate_write(
                        device, cmd, *code, field, value,
                    )?);
                }
                None => {
                    rval.insert(
                        *code,
                        (
                            cmd.to_string(),
                            WriteOp::from_op(
                                device, *op, cmd, *code, field, value,
                            )?,
                        ),
                    );
                }
                _ => {
                    bail!("{} cannot be written more than once", cmd);
                }
            }
        } else {
            bail!("unrecognized PMBus command {}", cmd);
        }
    }

    Ok(rval)
}

#[rustfmt::skip::macros(bail)]
fn writes(
    subargs: &PmbusArgs,
    hubris: &HubrisArchive,
    worker: &mut dyn PmbusWorker,
) -> Result<()> {
    let hargs = match (&subargs.rail, &subargs.device) {
        (Some(rails), None) => rails
            .iter()
            .map(|rail| find_rail(hubris, rail))
            .collect::<Result<Vec<(I2cArgs, Option<u8>)>>>(),

        (_, _) => Ok(vec![(
            I2cArgs::parse(
                hubris,
                &subargs.bus,
                subargs.controller,
                &subargs.port,
                &subargs.mux,
                &subargs.device,
            )?,
            None,
        )]),
    }?;

    //
    // Determine if we have a common device.
    //
    let device = hargs
        .iter()
        .fold(None, |dev, (harg, _)| {
            Some(match dev {
                None => match &harg.device {
                    Some(device) => match pmbus::Device::from_str(device) {
                        Some(device) => device,
                        None => pmbus::Device::Common,
                    },
                    None => pmbus::Device::Common,
                },
                Some(pmbus::Device::Common) => pmbus::Device::Common,
                Some(found) => match &harg.device {
                    Some(device) => match pmbus::Device::from_str(device) {
                        Some(device) if device == found => device,
                        _ => pmbus::Device::Common,
                    },
                    None => pmbus::Device::Common,
                },
            })
        })
        .unwrap();

    //
    // Now determine what we're actually going to write.
    //
    let writecmds = subargs.writes.as_ref().unwrap();
    let writes = validate_writes(writecmds, device)?;

    //
    // First up, we are going to do any reads that we need to perform, along
    // with any operations to set a command (SendByte) as well as set an
    // entire block (WriteBlock).
    //
    for (harg, rail) in &hargs {
        worker.begin_device(harg)?;

        //
        // If we have a rail, select it.
        //
        if let Some(rnum) = rail {
            worker.select_rail(*rnum);
        }

        //
        // Now our VOUT_MODE
        //
        worker.read(CommandCode::VOUT_MODE as u8, pmbus::Operation::ReadByte);

        for (&code, (_cmd, op)) in &writes {
            match op {
                WriteOp::Modify(size, _) => {
                    let op = match size {
                        1 => pmbus::Operation::ReadByte,
                        2 => pmbus::Operation::ReadWord,
                        4 => pmbus::Operation::ReadWord32,
                        _ => bail!("invalid size for WriteOp::Modify: {size}"),
                    };
                    worker.read(code, op);
                }
                _ => worker.write(code, op),
            }
        }

        worker.end_device();
    }

    //
    // Now go back through our devices checking results -- and creating our
    // next batch of work, if any.
    //
    let results = worker.run()?;
    let mut ndx = 0;
    let mut additional = false;

    let success = |harg, rail: &Option<u8>, cmd| {
        if let Some(rnum) = *rail {
            humility::msg!("{harg}, rail {rnum}: successfully wrote {cmd}",);
        } else {
            humility::msg!("{harg}: successfully wrote {cmd}");
        }
    };

    for (harg, rail) in &hargs {
        if let Some(rnum) = rail {
            if let Err(code) = results[ndx] {
                bail!("{harg}: failed to set rail {rnum}: {code}");
            }

            ndx += 1;
        }

        //
        // Step over the mode.
        //
        ndx += 1;

        for (&_code, (cmd, op)) in &writes {
            match op {
                WriteOp::Modify(_, _) => {
                    additional = true;
                }
                WriteOp::Set
                | WriteOp::SetBlock(..)
                | WriteOp::SetByte(..)
                | WriteOp::SetWord(..)
                | WriteOp::SetWord32(..) => match results[ndx] {
                    Err(code) => {
                        bail!(
                            "{harg}: failed to set {cmd}: {}",
                            worker.decode_write_err(code)
                        )
                    }
                    Ok(_) => {
                        success(harg, rail, cmd);
                    }
                },
            }
            ndx += 1;
        }
    }

    if !additional {
        return Ok(());
    }

    //
    // If we're here, we have addtional work to do.
    //
    let mut ndx = 0;

    for (harg, rail) in &hargs {
        worker.begin_device(harg)?;

        //
        // If we have a rail, select it.
        //
        if let Some(rnum) = rail {
            worker.select_rail(*rnum);
            ndx += 1;
        }

        let mode = match results[ndx] {
            Err(code) => {
                bail!("bad VOUT_MODE on {harg}: {}", worker.decode_read_err(code));
            }
            Ok(ref val) => VOUT_MODE::CommandData::from_slice(val).unwrap(),
        };

        ndx += 1;

        let getmode = || mode;

        for (&code, (cmd, op)) in &writes {
            if let WriteOp::Modify(size, set) = op {
                let payload = match results[ndx] {
                    Err(code) => {
                        bail!("failed to read {cmd}: {}", worker.decode_read_err(code));
                    }
                    Ok(ref val) => val,
                };

                let mut r = None;

                if payload.len() != *size {
                    bail!(
                        "mismatch on {cmd}: expected {size}, found {}",
                        payload.len()
                    );
                }

                device.command(code, |cmd| {
                    r = Some(prepare_write(
                        device, code, getmode, cmd, payload, set,
                    ));
                });

                let v = r.unwrap()?;
                assert_eq!(*size, v.len());

                worker.write(
                    code,
                    &match size {
                        1 => WriteOp::SetByte(v[0]),
                        2 => WriteOp::SetWord(u16::from_le_bytes(
                            v[..2].try_into().unwrap(),
                        )),
                        4 => WriteOp::SetWord32(u32::from_le_bytes(
                            v[..4].try_into().unwrap(),
                        )),
                        _ => bail!("invalid modify size: {size}"),
                    },
                )
            }

            ndx += 1;
        }

        worker.end_device();
    }

    let results = worker.run()?;
    let mut ndx = 0;

    //
    // Now take one final lap through our results, reporting any errors
    // that we find.
    //
    for (harg, rail) in &hargs {
        if let Some(rnum) = rail {
            if let Err(code) = results[ndx] {
                bail!("failed to set rail {rnum} on {harg}: Err({code})");
            }

            ndx += 1;
        }

        for (&_code, (cmd, op)) in &writes {
            if let WriteOp::Modify(_, _) = op {
                if let Err(code) = results[ndx] {
                    bail!(
                        "{harg}: failed to write {cmd}: {}",
                        worker.decode_write_err(code)
                    );
                } else {
                    success(harg, rail, cmd);
                }

                ndx += 1;
            }
        }
    }

    Ok(())
}

/// Functions necessary to perform PMBus work
trait PmbusWorker {
    /// Called to set up function calls to point to the given device
    ///
    /// Returns an error if there is no device specified
    fn begin_device(&mut self, harg: &I2cArgs) -> Result<()>;

    /// Ends usage of the current device
    fn end_device(&mut self);

    /// Selects a particular rail
    fn select_rail(&mut self, rail: u8);

    /// Performs the given read operation
    fn read(&mut self, code: u8, op: pmbus::Operation);

    /// Performs the given write operation
    fn write(&mut self, code: u8, op: &WriteOp);

    /// Executes queued-up commands, returning the result
    fn run(&mut self) -> Result<Vec<Result<Vec<u8>, u32>>>;

    fn decode_read_err(&self, code: u32) -> String;
    fn decode_write_err(&self, code: u32) -> String;
}

struct I2cWorker<'a> {
    core: &'a mut dyn Core,
    read_func: HiffyFunction,
    write_func: HiffyFunction,
    context: HiffyContext<'a>,
    ops: Vec<Op>,
}

impl<'a> I2cWorker<'a> {
    fn new(
        hubris: &'a HubrisArchive,
        core: &'a mut dyn Core,
        timeout: u32,
    ) -> Result<Self> {
        let context = HiffyContext::new(hubris, core, timeout)?;
        let read_func = context.get_function("I2cRead", 7)?;
        let write_func = context.get_function("I2cWrite", 8)?;
        Ok(Self { core, context, read_func, write_func, ops: vec![] })
    }
}

impl PmbusWorker for I2cWorker<'_> {
    fn begin_device(&mut self, harg: &I2cArgs) -> Result<()> {
        self.ops.push(Op::Push(harg.controller));
        self.ops.push(Op::Push(harg.port.index));

        if let Some(mux) = harg.mux {
            self.ops.push(Op::Push(mux.0));
            self.ops.push(Op::Push(mux.1));
        } else {
            self.ops.push(Op::PushNone);
            self.ops.push(Op::PushNone);
        }

        if let Some(address) = harg.address {
            self.ops.push(Op::Push(address));
        } else {
            bail!("no device specified");
        }
        Ok(())
    }

    fn select_rail(&mut self, rail: u8) {
        let page = CommandCode::PAGE as u8;
        self.ops.push(Op::Push(page));
        self.ops.push(Op::Push(rail));
        self.ops.push(Op::Push(1));
        self.ops.push(Op::Call(self.write_func.id));
        self.ops.push(Op::DropN(3));
    }

    fn read(&mut self, code: u8, op: pmbus::Operation) {
        let op = match op {
            pmbus::Operation::ReadByte => Op::Push(1),
            pmbus::Operation::ReadWord => Op::Push(2),
            pmbus::Operation::ReadWord32 => Op::Push(4),
            pmbus::Operation::ReadBlock => Op::PushNone,
            _ => panic!("not a read operation"),
        };

        self.ops.push(Op::Push(code));
        self.ops.push(op);
        self.ops.push(Op::Call(self.read_func.id));
        self.ops.push(Op::DropN(2));
    }

    fn end_device(&mut self) {
        self.ops.push(Op::DropN(5));
    }

    fn run(&mut self) -> Result<Vec<Result<Vec<u8>, u32>>> {
        self.ops.push(Op::Done);
        let ops = std::mem::take(&mut self.ops);
        self.context.run(self.core, &ops, None)
    }

    fn decode_read_err(&self, code: u32) -> String {
        self.read_func.strerror(code)
    }

    fn decode_write_err(&self, code: u32) -> String {
        self.read_func.strerror(code)
    }

    fn write(&mut self, code: u8, op: &WriteOp) {
        match op {
            WriteOp::SetBlock(payload) => {
                self.ops.push(Op::Push(code));

                //
                // For WriteBlock operations, we must write the size
                // followed by the payload.
                //
                self.ops.push(Op::Push(payload.len() as u8));

                for &byte in payload {
                    self.ops.push(Op::Push(byte));
                }

                self.ops.push(Op::Push(payload.len() as u8));
                self.ops.push(Op::Call(self.write_func.id));
                self.ops.push(Op::DropN(payload.len() as u8 + 2));
            }
            WriteOp::Set => {
                //
                // For SendByte operations, we issue a 1-byte raw write
                // that is the command by indicating the register to be
                // None.
                //
                self.ops.push(Op::PushNone);
                self.ops.push(Op::Push(code));
                self.ops.push(Op::Push(1));
                self.ops.push(Op::Call(self.write_func.id));
                self.ops.push(Op::DropN(3));
            }
            WriteOp::SetByte(b) => {
                self.ops.push(Op::Push(code));
                self.ops.push(Op::Push(*b));
                self.ops.push(Op::Push(1));
                self.ops.push(Op::Call(self.write_func.id));
                self.ops.push(Op::DropN(3));
            }
            WriteOp::SetWord(w) => {
                self.ops.push(Op::Push(code));
                for b in w.to_le_bytes() {
                    self.ops.push(Op::Push(b));
                }
                self.ops.push(Op::Push(2));
                self.ops.push(Op::Call(self.write_func.id));
                self.ops.push(Op::DropN(4));
            }
            WriteOp::SetWord32(w) => {
                self.ops.push(Op::Push(code));
                for b in w.to_le_bytes() {
                    self.ops.push(Op::Push(b));
                }
                self.ops.push(Op::Push(6));
                self.ops.push(Op::Call(self.write_func.id));
                self.ops.push(Op::DropN(6));
            }
            op => panic!("invalid op {op:?}"),
        }
    }
}

/// PMBus worker which uses Idol APIs
struct IdolWorker<'a> {
    hubris: &'a HubrisArchive,
    core: &'a mut dyn Core,
    context: HiffyContext<'a>,
    ops: Vec<Op>,

    write_set: IdolOperation<'a>,
    write_byte: IdolOperation<'a>,
    write_word: IdolOperation<'a>,
    write_word32: IdolOperation<'a>,

    read_byte: IdolOperation<'a>,
    read_word: IdolOperation<'a>,
    read_word32: IdolOperation<'a>,
    read_block: IdolOperation<'a>,

    /// Records whether each command (a single Idol operation) is a block read.
    ///
    /// Block read operations are serialized in a special way and must be
    /// post-processed afterwards to end up in the form expected by the caller.
    command_is_block_read: Vec<bool>,

    /// Records whether each command sets the rail.
    ///
    /// The Idol implementation sets the rail intrinsically as part of each
    /// command, but the caller expects a separate rail selection in the result
    /// array; we track this and inject synthetic results where needed.
    command_has_rail: Vec<bool>,
    just_set_rail: bool,

    /// Index of the selected device within `HubrisManifest::i2c_devices`
    dev_index: Option<usize>,

    /// Index of the specific rail within this archive's `CONTROLLER_CONFIG`
    rail_index: Option<u32>,

    sensor_id_to_index: BTreeMap<u32, usize>,
}

impl<'a> IdolWorker<'a> {
    fn new(
        hubris: &'a HubrisArchive,
        core: &'a mut dyn Core,
        timeout: u32,
    ) -> Result<Self> {
        let context = HiffyContext::new(hubris, core, timeout)?;
        let write_set = hubris.get_idol_command("Power.raw_pmbus_set")?;
        let write_byte =
            hubris.get_idol_command("Power.raw_pmbus_write_byte")?;
        let write_word =
            hubris.get_idol_command("Power.raw_pmbus_write_word")?;
        let write_word32 =
            hubris.get_idol_command("Power.raw_pmbus_write_word32")?;
        let read_byte = hubris.get_idol_command("Power.raw_pmbus_read_byte")?;
        let read_word = hubris.get_idol_command("Power.raw_pmbus_read_word")?;
        let read_word32 =
            hubris.get_idol_command("Power.raw_pmbus_read_word32")?;
        let read_block =
            hubris.get_idol_command("Power.raw_pmbus_read_block")?;

        // Read CONTROLLER_CONFIG from the core, which we'll use to compute
        // indices for rails when using raw PMBus functions.
        let (_name, var) = hubris
            .qualified_variables()
            .find(|&(n, _v)| n == "task_power::bsp::CONTROLLER_CONFIG")
            .ok_or_else(|| anyhow!("could not find CONTROLLER_CONFIG"))?;
        let mut buf: Vec<u8> = vec![0u8; var.size];
        core.halt()?;
        core.read_8(var.addr, buf.as_mut_slice())?;
        core.run()?;
        let controller_config = humility::reflect::load_value(
            hubris,
            &buf,
            hubris.lookup_type(var.goff)?,
            0,
        )?;

        // Destructure CONTROLLER_CONFIG to build a map of voltage sensor IDs
        // (which we can easily compute) to index in the CONTROLLER_CONFIG
        // array.
        use humility::reflect::{Base, Value};
        let Value::Array(array) = controller_config else {
            bail!("invalid shape for CONTROLLER_CONFIG: {controller_config:?}");
        };
        let mut sensor_id_to_index = BTreeMap::new();
        for (i, cfg) in array.iter().enumerate() {
            let Value::Struct(s) = cfg else {
                bail!("invalid shape for CONTROLLER_CONFIG[{i}]: {cfg:?}");
            };
            let v = &s["voltage"];
            let Value::Tuple(t) = v else {
                bail!("could not get 'voltage': {v:?}");
            };
            let Value::Base(b) = &t[0] else {
                bail!("could not get sensor ID from 'voltage': {v:?}");
            };
            let Base::U32(sensor_id) = b else {
                bail!("bad sensor id type: {b:?}");
            };
            sensor_id_to_index.insert(*sensor_id, i);
        }

        Ok(Self {
            hubris,
            core,
            context,
            ops: vec![],
            write_set,
            write_byte,
            write_word,
            write_word32,
            read_byte,
            read_word,
            read_word32,
            read_block,
            dev_index: None,
            rail_index: None,
            sensor_id_to_index,

            command_is_block_read: vec![],
            command_has_rail: vec![],
            just_set_rail: false,
        })
    }

    fn find_rail(&self, rail: u8) -> u32 {
        // Okay, this is the fun part.
        //
        // We've got an I2C device, which is uniquely defined with
        // `self.dev_index` as an index into the archive's `i2c_devices`.
        //
        // What we *need* is an index into the SP's `CONTROLLER_CONFIG`, which
        // is a data structure defining a bunch of rails.
        //
        // When constructing this struct, we previously built a map from a
        // voltage SensorId to an index in CONTROLLER_CONFIG.  We'll use that
        // map here to get our index.
        let device = self.dev_index.expect("cannot select rail without device");

        // Each rail has a single Voltage sensor, and they are in order; this
        // means we can pick the nth voltage sensor as our target SensorId.
        let sensor_id = self
            .hubris
            .manifest
            .sensors
            .iter()
            .enumerate()
            .filter(|(_, s)| {
                s.device == HubrisSensorDevice::I2c(device)
                    && s.kind == HubrisSensorKind::Voltage
            })
            .nth(rail as usize)
            .expect("could not get sensor id")
            .0;

        *self
            .sensor_id_to_index
            .get(&(sensor_id as u32))
            .expect("could not get rail") as u32
    }
}

impl PmbusWorker for IdolWorker<'_> {
    fn begin_device(&mut self, harg: &I2cArgs) -> Result<()> {
        assert!(self.dev_index.is_none());
        // Do a reverse search through our I2C devices to pick one out that has
        // the correct port + controller + mux
        let dev_index = self
            .hubris
            .manifest
            .i2c_devices
            .iter()
            .enumerate()
            .find(|(_i, dev)| {
                &dev.port == harg.port
                    && dev.controller == harg.controller
                    && Some(dev.address) == harg.address
                    && dev.mux.and_then(|mux| dev.segment.map(|s| (mux, s)))
                        == harg.mux
            })
            .ok_or_else(|| anyhow!("could not find device matching {harg:?}"))?
            .0;
        self.dev_index = Some(dev_index);
        self.rail_index = None;
        Ok(())
    }

    fn end_device(&mut self) {
        self.dev_index = None;
        self.rail_index = None;
    }

    fn select_rail(&mut self, rail: u8) {
        self.rail_index = Some(self.find_rail(rail));
        self.just_set_rail = true;
    }

    fn read(&mut self, code: u8, op: pmbus::Operation) {
        let (index, has_rail) = match self.rail_index {
            None => (self.find_rail(0), false),
            Some(r) => (r, true),
        };
        let index = IdolArgument::Scalar(index as u64);
        let has_rail = IdolArgument::Scalar(has_rail as u64);
        let code = IdolArgument::Scalar(code as u64);
        let args = [("index", index), ("has_rail", has_rail), ("op", code)];
        match op {
            pmbus::Operation::ReadBlock => {
                let payload = self.read_block.payload(&args).unwrap();
                self.context
                    .idol_call_ops(&self.read_block, &payload, &mut self.ops)
                    .unwrap();
                self.command_is_block_read.push(true);
            }
            pmbus::Operation::ReadByte => {
                let payload = self.read_byte.payload(&args).unwrap();
                self.context
                    .idol_call_ops(&self.read_byte, &payload, &mut self.ops)
                    .unwrap();
                self.command_is_block_read.push(false);
            }
            pmbus::Operation::ReadWord => {
                let payload = self.read_word.payload(&args).unwrap();
                self.context
                    .idol_call_ops(&self.read_word, &payload, &mut self.ops)
                    .unwrap();
                self.command_is_block_read.push(false);
            }
            pmbus::Operation::ReadWord32 => {
                let payload = self.read_word32.payload(&args).unwrap();
                self.context
                    .idol_call_ops(&self.read_word32, &payload, &mut self.ops)
                    .unwrap();
                self.command_is_block_read.push(false);
            }
            _ => panic!("not a read operation: {op:?}"),
        };
        self.command_has_rail.push(self.just_set_rail);
        self.just_set_rail = false;
    }

    fn write(&mut self, code: u8, op: &WriteOp) {
        let (index, has_rail) = match self.rail_index {
            None => (self.find_rail(0), false),
            Some(r) => (r, true),
        };
        let index = IdolArgument::Scalar(index as u64);
        let has_rail = IdolArgument::Scalar(has_rail as u64);
        let code = IdolArgument::Scalar(code as u64);
        let mut args =
            vec![("index", index), ("has_rail", has_rail), ("op", code)];
        match op {
            WriteOp::SetByte(b) => {
                args.push(("data", IdolArgument::Scalar(*b as u64)));
                let payload = self.write_byte.payload(&args).unwrap();
                self.context
                    .idol_call_ops(&self.write_byte, &payload, &mut self.ops)
                    .unwrap();
                self.command_is_block_read.push(false);
            }
            WriteOp::Set => {
                let payload = self.write_set.payload(&args).unwrap();
                self.context
                    .idol_call_ops(&self.write_set, &payload, &mut self.ops)
                    .unwrap();
                self.command_is_block_read.push(false);
            }
            WriteOp::SetWord(b) => {
                args.push(("data", IdolArgument::Scalar(*b as u64)));
                let payload = self.write_word.payload(&args).unwrap();
                self.context
                    .idol_call_ops(&self.write_word, &payload, &mut self.ops)
                    .unwrap();
                self.command_is_block_read.push(false);
            }
            WriteOp::SetWord32(b) => {
                args.push(("data", IdolArgument::Scalar(*b as u64)));
                let payload = self.write_word32.payload(&args).unwrap();
                self.context
                    .idol_call_ops(&self.write_word32, &payload, &mut self.ops)
                    .unwrap();
                self.command_is_block_read.push(false);
            }
            WriteOp::SetBlock(..) => {
                unimplemented!("cannot encode argument for SetBlock");
            }
            op => panic!("not a write op: {op:?}"),
        };
        self.command_has_rail.push(self.just_set_rail);
        self.just_set_rail = false;
    }

    fn decode_read_err(&self, code: u32) -> String {
        // All read operations share an error code
        self.read_byte.strerror(code)
    }

    fn decode_write_err(&self, code: u32) -> String {
        // All write operations share an error code
        self.write_byte.strerror(code)
    }

    fn run(&mut self) -> Result<Vec<Result<Vec<u8>, u32>>> {
        self.ops.push(Op::Done);
        let ops = std::mem::take(&mut self.ops);
        let mut out = self.context.run(self.core, &ops, None)?;

        // Block reads return a RawPmbusBlock, which is an active length
        // followed by a max-length array.  We convert from that type to a raw
        // data array here.
        //
        // Everything else is deserialized as-is, so we don't need to do
        // anything fancy to post-process it.
        assert_eq!(out.len(), self.command_has_rail.len());
        for out in out
            .iter_mut()
            .zip(std::mem::take(&mut self.command_is_block_read))
            .filter(|(_, b)| *b)
            .filter_map(|(o, _)| o.as_mut().ok())
        {
            let len = out[0] as usize;
            out.remove(0);
            out.resize(len, 0u8);
        }

        // Inject synthetic rail selection results into the result array, since
        // that's expected by the caller.
        let mut out_with_rail = vec![];
        assert_eq!(out.len(), self.command_has_rail.len());
        for (out, b) in
            out.into_iter().zip(std::mem::take(&mut self.command_has_rail))
        {
            if b {
                out_with_rail.push(Ok(vec![0]));
            }
            out_with_rail.push(out);
        }
        Ok(out_with_rail)
    }
}

#[allow(clippy::print_literal)]
fn pmbus(context: &mut ExecutionContext) -> Result<()> {
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let hubris = context.archive.as_ref().unwrap();
    let subargs = PmbusArgs::try_parse_from(subargs)?;

    if subargs.list {
        println!(
            "{} {:2} {} {} {:13} {}",
            "C", "P", "MUX", "ADDR", "DEVICE", "RAILS"
        );

        for device in &hubris.manifest.i2c_devices {
            if let HubrisI2cDeviceClass::Pmbus { rails } = &device.class {
                let mux = match (device.mux, device.segment) {
                    (Some(m), Some(s)) => format!("{}:{}", m, s),
                    (None, None) => "-".to_string(),
                    (_, _) => "?:?".to_string(),
                };

                println!(
                    "{} {:2} {:3} 0x{:02x} {:13} {}",
                    device.controller,
                    device.port.name,
                    mux,
                    device.address,
                    device.device,
                    rails
                        .iter()
                        .map(|r| r.name.clone())
                        .collect::<Vec<_>>()
                        .join(", "),
                )
            }
        }

        return Ok(());
    }

    let core = &mut **context.core.as_mut().unwrap();

    if core.is_dump() {
        bail!("can only list PMBus devices on a dump");
    }

    let timeout = subargs.timeout;

    // Pick an implementation based on our flags and core state
    let mut worker: Box<dyn PmbusWorker> = match subargs.agent {
        Agent::Auto => {
            if core.is_net() {
                Box::new(IdolWorker::new(hubris, core, timeout)?)
            } else if let Ok(a) = IdolWorker::new(hubris, core, timeout) {
                Box::new(a)
            } else {
                Box::new(I2cWorker::new(hubris, core, timeout)?)
            }
        }
        Agent::I2c => {
            if core.is_net() {
                bail!("cannot use I2C agent over the network");
            } else {
                Box::new(I2cWorker::new(hubris, core, timeout)?)
            }
        }
        Agent::Idol => Box::new(IdolWorker::new(hubris, core, timeout)?),
    };

    if subargs.summarize {
        summarize(&subargs, hubris, worker.as_mut())?;
        return Ok(());
    }

    if subargs.writes.is_some() {
        writes(&subargs, hubris, worker.as_mut())?;
        return Ok(());
    }

    pmbus_main(&subargs, hubris, worker.as_mut())
}

fn pmbus_main(
    subargs: &PmbusArgs,
    hubris: &HubrisArchive,
    worker: &mut dyn PmbusWorker,
) -> Result<()> {
    let hargs = match (&subargs.rail, &subargs.device) {
        (Some(rails), None) => {
            if rails.len() > 1 {
                bail!("cannot specify more than one rail");
            }

            find_rail(hubris, &rails[0])?.0
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

    let device = if let Some(driver) = &subargs.driver {
        match pmbus::Device::from_str(driver) {
            Some(device) => device,
            None => {
                bail!("unknown device \"{}\"", driver);
            }
        }
    } else if let Some(driver) = &hargs.device {
        match pmbus::Device::from_str(driver) {
            Some(device) => device,
            None => pmbus::Device::Common,
        }
    } else {
        pmbus::Device::Common
    };

    let (all, _) = all_commands(device);

    if let Some(ref commands) = subargs.commandhelp {
        if commands.is_empty() || commands[0] == "all" {
            for code in 0..0xffu8 {
                device.command(code, |cmd| {
                    print_command(device, code, cmd);
                });
            }

            return Ok(());
        }

        for cmd in commands {
            let code = match all.get(cmd) {
                Some(code) => *code,
                None => match parse_int::parse::<u8>(cmd) {
                    Ok(code) => code,
                    Err(_) => {
                        bail!(
                            "unrecognized PMBus command {}; \
                              use 'all' for all commands",
                            cmd
                        );
                    }
                },
            };

            device.command(code, |cmd| {
                print_command(device, code, cmd);
            });
        }

        return Ok(());
    }

    let mut cmds = vec![];

    worker.begin_device(&hargs)?;

    let rails = match hargs.class {
        HubrisI2cDeviceClass::Pmbus { rails } => Some(rails),
        _ => {
            if !subargs.force {
                bail!("not a recognized PMBus device; -F to force");
            } else {
                None
            }
        }
    };

    let mut run = [true; 256];

    if let Some(ref commands) = subargs.commands {
        if commands.is_empty() {
            bail!("expected a command");
        }

        run.fill(false);

        for cmd in commands {
            if let Some(code) = all.get(cmd) {
                run[*code as usize] = true;
            } else if let Ok(code) = parse_int::parse::<u8>(cmd) {
                run[code as usize] = true;
            } else {
                bail!(
                    "unrecognized PMBus command {}; \
                     use -H for command help",
                    cmd
                );
            }
        }
    }

    let mut setrail = false;

    //
    // If we have a rail specified, we want to set that first.
    //
    if let Some(railargs) = &subargs.rail {
        if railargs.len() != 1 {
            bail!("rails length?!");
        }

        let rail = &railargs[0];

        let rails = match rails {
            Some(rails) => rails,
            None => bail!("rail specified, but device has unknown rails"),
        };

        if rails.is_empty() {
            bail!("rail specified, but device has no defined rails");
        }

        //
        // We want to allow our rail to be specified by number or by name.
        //
        let rnum = match parse_int::parse::<u8>(rail) {
            Ok(rnum) => {
                if rails.len() == 1 {
                    bail!("rail specified, but device only has one rail");
                }

                if rnum as usize >= rails.len() {
                    bail!("invalid rail number {}", rnum);
                }
                rnum as usize
            }
            _ => match rails.iter().position(|r| &r.name == rail) {
                Some(rnum) => rnum,
                None => {
                    bail!(
                        "invalid rail; expected one of: {}",
                        rails
                            .iter()
                            .map(|r| r.name.clone())
                            .collect::<Vec<_>>()
                            .join(", ")
                    );
                }
            },
        };

        let page = pmbus::commands::CommandCode::PAGE as u8;

        if rails.len() > 1 {
            worker.select_rail(rnum as u8);
            cmds.push(page);
            setrail = true;
        }
    }

    let mut addcmd = |cmd: &dyn pmbus::Command, code| {
        let op = match cmd.read_op() {
            v @ (pmbus::Operation::ReadByte
            | pmbus::Operation::ReadWord
            | pmbus::Operation::ReadWord32
            | pmbus::Operation::ReadBlock) => v,
            _ => {
                return;
            }
        };

        if subargs.dryrun {
            println!("0x{:02x} {:?}", code, cmd);
        }

        worker.read(code, op);
        cmds.push(code);
    };

    let vout = pmbus::commands::CommandCode::VOUT_MODE as u8;
    device.command(vout, |cmd| addcmd(cmd, vout));

    for i in 0..=255u8 {
        if run[i as usize] {
            device.command(i, |cmd| addcmd(cmd, i));
        }
    }

    if subargs.dryrun {
        return Ok(());
    }

    if cmds.is_empty() {
        bail!("no command to run");
    }

    let results = worker.run()?;

    let base = if setrail {
        match results[0] {
            Err(code) => {
                bail!("couldn't set rail: {}", worker.decode_write_err(code));
            }
            Ok(_) => 1,
        }
    } else {
        0
    };

    let (mode, ndx) = if cmds[base] == vout {
        let mode = match results[base] {
            Err(code) => {
                bail!("can't read VOUT_MODE: {}", worker.decode_read_err(code));
            }
            Ok(ref val) => VOUT_MODE::CommandData::from_slice(val).unwrap(),
        };

        (Some(mode), base + 1)
    } else {
        (None, base)
    };

    let getmode = || match mode {
        Some(mode) => mode,
        None => {
            panic!("unexpected call to get VOutMode");
        }
    };

    for i in ndx..results.len() {
        let mut r = Ok(());

        device.command(cmds[i], |cmd| {
            r = print_result(
                subargs,
                device,
                cmds[i],
                getmode,
                cmd,
                &results[i],
                worker,
            );
        });

        r?;
    }

    Ok(())
}

pub fn init() -> Command {
    Command {
        app: PmbusArgs::command(),
        name: "pmbus",
        run: pmbus,
        kind: CommandKind::Attached {
            archive: Archive::Required,
            attach: Attach::Any,
            validate: Validate::Booted,
        },
    }
}
