/*
 * Copyright 2021 Oxide Computer Company
 */

use crate::cmd::{Archive, Attach, Validate};
use crate::core::Core;
use crate::hiffy::*;
use crate::hubris::*;
use crate::Args;
use std::str;
use std::thread;

use anyhow::{anyhow, bail, Result};
use hif::*;
use std::time::Duration;
use structopt::clap::App;
use structopt::StructOpt;

use std::convert::TryInto;

#[derive(StructOpt, Debug)]
#[structopt(
    name = "lpc55gpio",
    about = "GPIO pin manipulation (lpc55 variant)"
)]
struct GpioArgs {
    /// sets timeout
    #[structopt(
        long, short = "T", default_value = "5000", value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// verbose output
    #[structopt(long, short)]
    verbose: bool,

    /// toggle specified pins
    #[structopt(
        long, short,
        conflicts_with_all = &["togget", "set", "reset", "configure"]
    )]
    input: bool,

    /// toggle specified pins
    #[structopt(
        long, short, requires = "pins",
        conflicts_with_all = &["set", "reset", "configure"]
    )]
    toggle: bool,

    /// sets specified pins
    #[structopt(
        long, short, requires = "pins",
        conflicts_with_all = &["reset", "configure"]
    )]
    set: bool,

    /// resets specified pins
    #[structopt(
        long, short, requires = "pins",
         conflicts_with_all = &["configure"])]
    reset: bool,

    /// configures specified pins
    #[structopt(long, short, requires = "pins")]
    configure: Option<String>,

    /// configures specified pins
    #[structopt(long, short, requires = "pins")]
    direction: Option<String>,

    /// specifies GPIO pins on which to operate
    #[structopt(long, short, value_name = "pins")]
    pins: Option<Vec<String>>,
}

fn gpio(
    hubris: &mut HubrisArchive,
    core: &mut dyn Core,
    _args: &Args,
    subargs: &Vec<String>,
) -> Result<()> {
    let subargs = GpioArgs::from_iter_safe(subargs)?;
    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;
    let funcs = context.functions()?;

    let func = |name, nargs| {
        let f = funcs
            .get(name)
            .ok_or_else(|| anyhow!("did not find {} function", name))?;

        if f.args.len() != nargs {
            println!("{} {:?}", name, f);
            bail!(
                "mismatched function signature on {} {} {}",
                name,
                f.args.len(),
                nargs
            );
        }

        Ok(f)
    };

    let gpio_toggle = func("GpioToggle", 1)?;
    let gpio_set = func("GpioSet", 1)?;
    let gpio_reset = func("GpioReset", 1)?;
    let gpio_input = func("GpioInput", 1)?;
    let gpio_configure = func("GpioConfigure", 7)?;
    let gpio_direction = func("GpioDirection", 2)?;
    let mut configure_args = vec![];
    let mut direction_args = vec![];

    let target = if subargs.toggle {
        gpio_toggle.id
    } else if subargs.set {
        gpio_set.id
    } else if subargs.reset {
        gpio_reset.id
    } else if let Some(ref configure) = subargs.configure {
        let params: Vec<&str> = configure.split(":").collect();
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
        let params: Vec<&str> = dir.split(":").collect();
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
        if args.len() == 0 {
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

    context.execute(core, ops.as_slice(), None)?;

    loop {
        if context.done(core)? {
            break;
        }

        thread::sleep(Duration::from_millis(100));
    }

    let results = context.results(core)?;

    if subargs.input {
        let mut ndx = 0;
        for arg in &args {
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

            ndx += 1;
        }
    } else {
        println!("{:?}", results);
    }

    Ok(())
}

pub fn init<'a, 'b>() -> (crate::cmd::Command, App<'a, 'b>) {
    (
        crate::cmd::Command::Attached {
            name: "lpc55gpio",
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
            run: gpio,
        },
        GpioArgs::clap(),
    )
}
