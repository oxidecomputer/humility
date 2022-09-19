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
//! % humility gpio --toggle --pins B:14
//! humility: attached via ST-Link V3
//! [Ok([])]
//! ```
//!
//! To set pins B:0, B:14 and E:1:
//!
//! ```console
//! % humility gpio --set --pins B:0,B:14,E:1
//! humility: attached via ST-Link V3
//! [Ok([]), Ok([]), Ok([])]
//! ```
//!
//! To reset pin E:1:
//!
//! ```console
//! % humility gpio --reset --pins E:1
//! humility: attached via ST-Link V3
//! [Ok([])]
//! ```
//!
//! ### Input
//!
//! To get input values for a particular pin:
//!
//! ```console
//! % humility gpio --input --pins B:0,B:14,E:1
//! humility: attached via ST-Link V3
//! B:0  = 1
//! B:14 = 1
//! E:1  = 0
//! ```
//!
//! To get input values for all pins, leave the pin unspecified:
//!
//! ```console
//! % humility gpio --input
//! humility: attached via ST-Link V3
//! Pin       0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15
//! -----------------------------------------------------------------------
//! Port A    0   0   1   0   0   0   0   0   0   0   0   0   0   1   1   1
//! Port B    1   0   0   0   1   0   0   0   0   0   0   0   0   0   1   0
//! Port C    0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
//! Port D    0   0   0   0   0   0   0   0   1   1   0   0   0   0   1   0
//! Port E    0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
//! Port F    1   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0
//! Port G    0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
//! Port H    0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
//! Port I    0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
//! Port J    0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
//! Port K    0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
//! ```
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
//! % humility gpio -c Output:PushPull:High:None:AF0 -p A:5
//! ```
//!

use humility::cli::Subcommand;
use humility_cmd::hiffy::HiffyContext;
use humility_cmd::{Archive, Attach, Command, CommandKind, Validate};
use std::str;

use anyhow::{bail, Result};
use clap::{CommandFactory, Parser};
use hif::*;

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

fn gpio(context: &mut humility::ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let hubris = context.archive.as_ref().unwrap();
    let subargs = GpioArgs::try_parse_from(subargs)?;
    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;
    let funcs = context.functions()?;

    let gpio_toggle = funcs.get("GpioToggle", 2)?;
    let gpio_set = funcs.get("GpioSet", 2)?;
    let gpio_reset = funcs.get("GpioReset", 2)?;
    let gpio_input = funcs.get("GpioInput", 1)?;
    let gpio_configure = funcs.get("GpioConfigure", 7)?;
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

    let results = context.run(core, ops.as_slice(), None)?;

    if subargs.input {
        let mut header = false;

        for (ndx, arg) in args.iter().enumerate() {
            match arg.1 {
                Some(pin) => {
                    println!(
                        "{}:{:<2} = {}",
                        arg.2,
                        pin,
                        match results[ndx] {
                            Err(code) => {
                                gpio_input.strerror(code)
                            }
                            Ok(ref val) => {
                                let arr: &[u8; 2] = val[0..2].try_into()?;
                                let v = u16::from_le_bytes(*arr);
                                format!("{}", (v >> pin) & 1)
                            }
                        }
                    );
                }

                None => {
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

                            for i in 0..16 {
                                print!("{:4}", (v >> i) & 1)
                            }
                            println!();
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
