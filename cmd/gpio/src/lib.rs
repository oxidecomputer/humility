// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility gpio`
//!
//! `humility gpio` allows for GPIO pins to be set, reset, queried or
//! configured on STM32 targets.  Commands:
//!
//! - `--set` (`-s`): Sets a pin (sets it high)
//! - `--reset` (`-r`): Resets a pin (sets it low)
//! - `--toggle` (`-t`): Toggles a pin (sets it high if low, low if high)
//! - `--input` (`-i`): Queries the state of a pin (or all pins if no pin
//!   is specified)
//! - `--configure` (`-c`): Configures a pin
//!
//! ### Set, reset, toggle
//!
//! To change the state of a pin (or pins), specify the pin (or pins) and
//! the desired command.  For example, to toggle the state on pin 14 on
//! port B:
//!
//! ```console
//! $ humility gpio --toggle --pins B:14
//! humility: attached via ST-Link V3
//! [Ok([])]
//! ```
//!
//! To set pins B:0, B:14 and E:1:
//!
//! ```console
//! $ humility gpio --set --pins B:0,B:14,E:1
//! humility: attached via ST-Link V3
//! [Ok([]), Ok([]), Ok([])]
//! ```
//!
//! To reset pin E:1:
//!
//! ```console
//! $ humility gpio --reset --pins E:1
//! humility: attached via ST-Link V3
//! [Ok([])]
//! ```
//!
//! ### Input
//!
//! To get input values for a particular pin:
//!
//! ```console
//! $ humility gpio --input --pins B:0,B:14,E:1
//! humility: attached via ST-Link V3
//! B:0  = 1
//! B:14 = 1
//! E:1  = 0
//! ```
//!
//! To get input values for all pins, leave the pin unspecified:
//!
//! ```console
//! $ humility gpio --input
//! humility: attached via ST-Link V3
//! Chip: STM32H753ZITx
//! Pin       0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15
//! -----------------------------------------------------------------------
//! Port A    0   0   1   0   0   0   0   0   0   0   0   0   0   1   1   1
//! Port B    1   0   0   0   1   0   0   0   0   0   0   0   0   0   1   0
//! Port C    0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
//! Port D    0   0   0   0   0   0   0   0   1   1   0   0   0   0   1   0
//! Port E    0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
//! Port F    1   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0
//! Port G    0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
//! Port H    0   0   .   .   .   .   .   .   .   .   .   .   .   .   .   .
//! ```
//!
//! Note that the chip name is displayed, and GPIO groups are filtered based on
//! the specific chip variant. For partially populated GPIO groups (like Port H
//! above with only 2 pins), non-existent pins are shown as `.`
//!
//! If a chip name is not in the internal database of chip names, then the
//! old behavior of this program is used; no GPIO config info will be shown
//! and it will be assumed that the part has 176 GPIO pins in groups A to K.
//!
//! To print input values with STM32H7 series GPIO configuration settings,
//! use the `--with-config` or `-w` flag with `--input`:
//!
//! ```console
//! $ humility gpio --input --with-config --pins B:0,B:14,E:1
//! humility: attached to 0483:3754:002F00174741500820383733 via ST-Link V3
//! Chip: STM32H753ZITx
//! B:0  = 0 Analog:OutPushPull:LowSpeed:NoPull:AF0:Unlocked:InZero:OutZero
//! B:14 = 1 Input:OutPushPull:HighSpeed:NoPull:AF5:Unlocked:InOne:OutZero
//! E:1  = 0 Alternate:OutPushPull:VeryHighSpeed:NoPull:AF12:Unlocked:InZero:OutZero
//! ```
//!
//! If you query pins that don't exist on the chip variant, they will show as `None`:
//!
//! ```console
//! $ humility gpio --input --pins J:4,K:9
//! humility: attached via ST-Link V3
//! Chip: STM32H753ZITx
//! J:4  = None
//! K:9  = None
//! ```
//!
//! With `--with-config`, non-existent pins or groups show `None` for configuration:
//!
//! ```console
//! $ humility gpio --input --with-config --pins H:5,J:4
//! humility: attached via ST-Link V3
//! Chip: STM32H753ZITx
//! H:5  = 0 None
//! J:4  = 0 None
//! ```
//!
//! If no pins are specified with `--with-config`, then the pin names, values
//! and config settings are printed one per line for all available GPIO pins on
//! the chip variant.
//!
//! ### Configure
//!
//! To configure a pin, the configuration should be specified as a
//! colon-delimited 5-tuple consisting of:
//!
//! - Mode: `Input`, `Output`, `Alternate`, or `Analog`
//! - Output type: `PushPull` or `OpenDrain`
//! - Speed: `Low`, `Medium`, `High`, or `VeryHigh`
//! - Pull direction: `None`, `Up`, or `Down`
//! - Alternate function: one of `AF0` through `AF15`
//!
//! For example, to configure pin 5 on port A as a push-pull output:
//!
//! ```console
//! $ humility gpio -c Output:PushPull:High:None:AF0 -p A:5
//! ```
//!

mod config_cache;
mod stm32h7;
mod stm32h7_parts;
#[cfg(test)]
mod stm32h7_testdata;

use humility_cli::{ExecutionContext, Subcommand};
use humility_cmd::{Archive, Attach, Command, CommandKind, Validate};
use humility_hiffy::HiffyContext;

use anyhow::{Result, anyhow, bail};
use clap::{CommandFactory, Parser};
use hif::*;
use humility_hiffy::IpcError;

fn show_gpio_with_config(
    context: &mut ExecutionContext,
    gpio_input: &humility_hiffy::HiffyFunction,
    args: &[(u16, Option<u8>, String)],
    results: &[Result<Vec<u8>, IpcError>],
) -> Result<()> {
    let hubris = context
        .archive
        .as_ref()
        .ok_or_else(|| anyhow!("Cannot get GPIO config: archive not loaded"))?;
    if let Some(chip) = hubris.chip() {
        if chip.to_ascii_lowercase().starts_with("stm32h7") {
            // Call the stm32h7-specific function
            stm32h7::show_gpio_with_config(
                context,
                gpio_input,
                chip.as_str(),
                args,
                results,
            )
        } else {
            Err(anyhow!(
                "GPIO `--with-config` is not supported for chip '{chip}'"
            ))
        }
    } else {
        Err(anyhow!(
            "GPIO `--with-config` is not supported when chip is not specified"
        ))
    }
}

use std::convert::TryInto;

#[derive(Parser, Debug)]
#[clap(name = "gpio", about = env!("CARGO_PKG_DESCRIPTION"))]
struct GpioArgs {
    /// sets timeout
    #[clap(
        long, short = 'T', default_value_t = 5000, value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// shows the state of an input pin (or all pins if pin is unspecified)
    #[clap(
        long, short,
        conflicts_with_all = &["toggle", "set", "reset", "configure"]
    )]
    input: bool,

    /// show configuration of pins along with their values
    #[clap(short, long, requires = "input")]
    with_config: bool,

    /// toggle specified pins
    #[clap(
        long, short, requires = "pins",
        conflicts_with_all = &["set", "reset", "configure"]
    )]
    toggle: bool,

    /// sets specified pins
    #[clap(
        long, short, requires = "pins",
        conflicts_with_all = &["reset", "configure"]
    )]
    set: bool,

    /// resets specified pins
    #[clap(
        long, short, requires = "pins",
         conflicts_with_all = &["configure"])]
    reset: bool,

    /// configures specified pins
    #[clap(long, short, requires = "pins")]
    configure: Option<String>,

    /// specifies GPIO pins on which to operate
    #[clap(long, short, value_name = "pins", use_value_delimiter = true)]
    pins: Option<Vec<String>>,
}

fn gpio(context: &mut ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let hubris = context.archive.as_ref().unwrap();
    let subargs = GpioArgs::try_parse_from(subargs)?;
    let mut hiffy_context = HiffyContext::new(hubris, core, subargs.timeout)?;

    let gpio_toggle = hiffy_context.get_function("GpioToggle", 2)?;
    let gpio_set = hiffy_context.get_function("GpioSet", 2)?;
    let gpio_reset = hiffy_context.get_function("GpioReset", 2)?;
    let gpio_input = hiffy_context.get_function("GpioInput", 1)?;
    let gpio_configure = hiffy_context.get_function("GpioConfigure", 7)?;
    let mut configure_args = vec![];

    let target = if subargs.toggle {
        gpio_toggle.id
    } else if subargs.set {
        gpio_set.id
    } else if subargs.reset {
        gpio_reset.id
    } else if let Some(ref configure) = subargs.configure {
        let params: Vec<&str> = configure.split(':').collect();
        let args = ["Mode", "OutputType", "Speed", "Pull", "Alternate"];

        if params.len() != args.len() {
            bail!("expected {}", args.join(":"));
        }

        for i in 0..args.len() {
            configure_args.push(gpio_configure.lookup_argument(
                hubris,
                args[i],
                2 + i,
                params[i],
            )?);
        }

        gpio_configure.id
    } else if subargs.input {
        gpio_input.id
    } else {
        bail!(
            "expected one of configure, set, \
            reset, toggle, or input to be specified"
        );
    };

    let mut args: Vec<(u16, Option<u8>, String)> = vec![];

    if let Some(ref pins) = subargs.pins {
        for pin in pins {
            let p: Vec<&str> = pin.split(':').collect();

            if p.len() != 2 {
                bail!("expected both a port and a pin number");
            }

            let port = gpio_toggle.lookup_argument(hubris, "port", 0, p[0])?;
            let pin = match parse_int::parse::<u8>(p[1]) {
                Ok(pin) if pin < 16 => pin,
                _ => {
                    bail!("invalid pin {}", p[1]);
                }
            };

            args.push((port, Some(pin), p[0].to_string()));
        }
    }

    let mut ops = vec![];

    if subargs.input {
        if args.is_empty() {
            let variants = gpio_input.argument_variants(hubris, 0)?;

            for v in &variants {
                args.push((v.1, None, v.0.clone()));
            }
        }

        for arg in &args {
            ops.push(Op::Push16(arg.0));
            ops.push(Op::Call(target));
            ops.push(Op::DropN(1));
        }
    } else if subargs.configure.is_some() {
        for arg in &args {
            ops.push(Op::Push16(arg.0));
            ops.push(Op::Push(arg.1.unwrap()));

            for configure_arg in &configure_args {
                ops.push(Op::Push16(*configure_arg));
            }

            ops.push(Op::Call(target));
            ops.push(Op::DropN(7));
        }
    } else {
        for arg in &args {
            ops.push(Op::Push16(arg.0));
            ops.push(Op::Push(arg.1.unwrap()));
            ops.push(Op::Call(target));
            ops.push(Op::DropN(2));
        }
    }

    ops.push(Op::Done);

    let results = hiffy_context.run(core, ops.as_slice(), None)?;

    // Get chip info for filtering and display
    let chip_name = hubris.chip();
    let (max_group, chip_part) = if let Some(chip) = &chip_name {
        if chip.to_ascii_lowercase().starts_with("stm32h7") {
            use crate::stm32h7_parts::Stm32H7Series;
            let part = Stm32H7Series::find_part(chip, true);

            // Warn about unknown chips
            if !part.is_known() {
                eprintln!(
                    "Warning: Unknown STM32H7 chip '{}'. Using default GPIO layout (groups A-K).",
                    chip
                );
                eprintln!(
                    "         Configuration display (--with-config) is not available for unknown chips."
                );
            }

            (Some(part.max_group()), Some(part))
        } else {
            (None, None)
        }
    } else {
        (None, None)
    };

    if subargs.input {
        let mut header = false;

        if subargs.with_config {
            // Print chip name
            if let Some(chip) = &chip_name {
                println!("Chip: {}", chip);
            }
            show_gpio_with_config(context, &gpio_input, &args, &results)?;
        } else {
            // Print chip name for grid display too
            if let Some(chip) = &chip_name {
                println!("Chip: {}", chip);
            }

            for (ndx, arg) in args.iter().enumerate() {
                // Check if this group exists on the chip
                let group = arg.2.chars().next();
                let group_exists =
                    if let (Some(g), Some(max_g)) = (group, max_group) {
                        g >= 'A' && g <= max_g
                    } else {
                        true // If we can't determine, assume it exists
                    };

                match arg.1 {
                    Some(pin) => {
                        // Check if pin exists (group exists and pin within range)
                        let pin_exists =
                            if let (Some(g), Some(part)) = (group, chip_part) {
                                g >= 'A'
                                    && g <= part.max_group()
                                    && pin < part.pins_in_group(g)
                            } else {
                                group_exists // Fall back to group-level check
                            };

                        if !pin_exists {
                            // Pin doesn't exist on this chip
                            println!("{}:{:<2} = None", arg.2, pin);
                        } else {
                            println!(
                                "{}:{:<2} = {}",
                                arg.2,
                                pin,
                                match results[ndx] {
                                    Err(code) => {
                                        gpio_input.strerror(code)
                                    }
                                    Ok(ref val) => {
                                        let arr: &[u8; 2] =
                                            val[0..2].try_into()?;
                                        let v = u16::from_le_bytes(*arr);
                                        format!("{}", (v >> pin) & 1)
                                    }
                                }
                            );
                        }
                    }

                    None => {
                        // Skip ports that don't exist on this chip variant
                        if !group_exists {
                            continue;
                        }

                        if !header {
                            print!("{:7}", "Pin");

                            for i in 0..16 {
                                print!("{:4}", i);
                            }

                            println!();

                            print!("-------");

                            for _i in 0..16 {
                                print!("----");
                            }

                            println!();
                            header = true;
                        }

                        print!("Port {} ", arg.2);
                        match results[ndx] {
                            Err(code) => {
                                println!("{}", gpio_input.strerror(code))
                            }
                            Ok(ref val) => {
                                let arr: &[u8; 2] = val[0..2].try_into()?;
                                let v = u16::from_le_bytes(*arr);

                                // Determine how many pins exist in this group
                                let max_pin = if let (Some(g), Some(part)) =
                                    (group, chip_part)
                                {
                                    part.pins_in_group(g)
                                } else {
                                    16 // Default to all 16 if we can't determine
                                };

                                for i in 0..16 {
                                    if i < max_pin {
                                        print!("{:4}", (v >> i) & 1)
                                    } else {
                                        print!("   .")
                                    }
                                }
                                println!();
                            }
                        }
                    }
                }
            }
        }
    } else {
        println!("{:?}", results);
    }

    Ok(())
}

pub fn init() -> Command {
    Command {
        app: GpioArgs::command(),
        name: "gpio",
        run: gpio,
        kind: CommandKind::Attached {
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
        },
    }
}
