// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use humility::cli::Subcommand;
use humility_cmd::{hiffy::*, CommandKind};
use humility_cmd::{Archive, Attach, Command, Validate};
use std::str;

use anyhow::{bail, Result};
use clap::{CommandFactory, Parser};
use hif::*;

use std::convert::TryInto;

#[derive(Parser, Debug)]
#[clap(name = "lpc55gpio", about = "GPIO pin manipulation (lpc55 variant)")]
struct GpioArgs {
    /// sets timeout
    #[clap(
        long, short = 'T', default_value_t = 5000, value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// toggle specified pins
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

    /// configures specified pins
    #[clap(long, short, requires = "pins")]
    direction: Option<String>,

    /// specifies GPIO pins on which to operate
    #[clap(long, short, value_name = "pins")]
    pins: Option<Vec<String>>,
}

fn gpio(context: &mut humility::ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let hubris = context.archive.as_ref().unwrap();

    let subargs = GpioArgs::try_parse_from(subargs)?;
    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;
    let funcs = context.functions()?;

    let gpio_toggle = funcs.get("GpioToggle", 1)?;
    let gpio_set = funcs.get("GpioSet", 1)?;
    let gpio_reset = funcs.get("GpioReset", 1)?;
    let gpio_input = funcs.get("GpioInput", 1)?;
    let gpio_configure = funcs.get("GpioConfigure", 7)?;
    let gpio_direction = funcs.get("GpioDirection", 2)?;
    let mut configure_args = vec![];
    let mut direction_args = vec![];

    let target = if subargs.toggle {
        gpio_toggle.id
    } else if subargs.set {
        gpio_set.id
    } else if subargs.reset {
        gpio_reset.id
    } else if let Some(ref configure) = subargs.configure {
        let params: Vec<&str> = configure.split(':').collect();
        let args = ["AltFn", "Mode", "Slew", "Invert", "Digimode", "Opendrain"];

        if params.len() != args.len() {
            bail!("expected {}", args.join(":"));
        }

        for i in 0..args.len() {
            configure_args.push(gpio_configure.lookup_argument(
                hubris,
                args[i],
                1 + i,
                params[i],
            )?);
        }

        gpio_configure.id
    } else if let Some(ref dir) = subargs.direction {
        let params: Vec<&str> = dir.split(':').collect();
        let args = ["Direction"];

        if params.len() != args.len() {
            bail!("expected {}", args.join(":"));
        }

        for i in 0..args.len() {
            direction_args.push(gpio_direction.lookup_argument(
                hubris,
                args[i],
                1 + i,
                params[i],
            )?);
        }

        gpio_direction.id
    } else if subargs.input {
        gpio_input.id
    } else {
        bail!("expected one of input, toggle, set, or reset to be specified");
    };

    let mut args: Vec<(u16, String)> = vec![];

    if let Some(ref pins) = subargs.pins {
        for pin in pins {
            let pin = gpio_toggle.lookup_argument(hubris, "pin", 0, pin)?;

            args.push((pin, pin.to_string()));
        }
    }

    let mut ops = vec![];

    if subargs.input {
        if args.is_empty() {
            let pins = gpio_input.argument_variants(hubris, 0)?;

            for pin in &pins {
                args.push((pin.1, pin.0.clone()));
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

            for configure_arg in &configure_args {
                ops.push(Op::Push16(*configure_arg));
            }

            ops.push(Op::Call(target));
            ops.push(Op::DropN(7));
        }
    } else if subargs.direction.is_some() {
        for arg in &args {
            ops.push(Op::Push16(arg.0));

            for direction_arg in &direction_args {
                ops.push(Op::Push16(*direction_arg));
            }

            ops.push(Op::Call(target));
            ops.push(Op::DropN(2));
        }
    } else {
        for arg in &args {
            ops.push(Op::Push16(arg.0));
            ops.push(Op::Call(target));
            ops.push(Op::DropN(1));
        }
    }

    ops.push(Op::Done);

    let results = context.run(core, ops.as_slice(), None)?;

    if subargs.input {
        for (ndx, arg) in args.iter().enumerate() {
            println!(
                "{} = {}",
                arg.1,
                match results[ndx] {
                    Err(code) => {
                        gpio_input.strerror(code)
                    }
                    Ok(ref val) => {
                        let arr: &[u8; 2] = val[0..2].try_into()?;
                        let v = u16::from_le_bytes(*arr);
                        format!("{}", v)
                    }
                }
            );
        }
    } else {
        println!("{:?}", results);
    }

    Ok(())
}

pub fn init() -> Command {
    Command {
        app: GpioArgs::command(),
        name: "lpc55gpio",
        run: gpio,
        kind: CommandKind::Attached {
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
        },
    }
}
