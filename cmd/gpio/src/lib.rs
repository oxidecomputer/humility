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
//! To get input values with GPIO configuration settings,
//! use the --with-config or -w flag with --input:
//!
//! ```console
//! $ humility gpio --input -with-config --pins B:0,B:14,E:1
//! humility: attached to 0483:3754:002F00174741500820383733 via ST-Link V3
//! B:0  = 0 Analog:OutPushPull:LowSpeed:NoPull:AF0:Unlocked:InZero:OutZero
//! B:14 = 1 Input:OutPushPull:HighSpeed:NoPull:AF5:Unlocked:InOne:OutZero
//! E:1  = 0 Alternate:OutPushPull:VeryHighSpeed:NoPull:AF12:Unlocked:InZero:OutZero
//! ```
//!
//! If no pins are specified, then the pin names, values and config settings
//! are printeed one per line for all GPIO pins.
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

use humility_cli::{ExecutionContext, Subcommand};
use humility_cmd::{Archive, Attach, Command, CommandKind, Validate};
use humility_hiffy::{HiffyContext, IpcError};
use std::mem::MaybeUninit;
use std::str;

use anyhow::{anyhow, bail, Result};
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

use device::gpioa::RegisterBlock;
use humility_hiffy::HiffyFunction;
use lazy_static::lazy_static;
use std::collections::BTreeMap;
use std::ops::RangeInclusive;
use stm32h7::stm32h753 as device;

lazy_static! {
    static ref GPIO_REGISTERS: BTreeMap<&'static str, RangeInclusive<u32>> = {
        let mut map = BTreeMap::new();
        // Section 11.4: GPIO registers
        map.insert("A", 0x58020000..=0x580203FF);
        map.insert("B", 0x58020400..=0x580207FF);
        map.insert("C", 0x58020800..=0x58020BFF);
        map.insert("D", 0x58020C00..=0x58020FFF);
        map.insert("E", 0x58021000..=0x580213FF);
        map.insert("F", 0x58021400..=0x580217FF);
        map.insert("G", 0x58021800..=0x58021BFF);
        map.insert("H", 0x58021C00..=0x58021FFF);
        map.insert("I", 0x58022000..=0x580223FF);
        map.insert("J", 0x58022400..=0x580227FF);
        map.insert("K", 0x58022800..=0x58022BFF);
        map
    };
}

// This is a hack.
struct PinConfig {
    mode: String,
    otype: String,
    speed: String,
    pull: String,
    input: String,
    output: String,
    lock: String,
    alternate: String,
}

use std::fmt;

impl fmt::Display for PinConfig {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Use the write! macro to format the output to the formatter 'f'
        write!(
            f,
            "{}:{}:{}:{}:{}:{}:{}:{}",
            self.mode,
            self.otype,
            self.speed,
            self.pull,
            self.alternate,
            self.lock,
            self.input,
            self.output
        )
    }
}

struct ConfigCache {
    cache: BTreeMap<String, RegisterBlock>,
}

impl ConfigCache {
    fn new() -> ConfigCache {
        let cache: BTreeMap<String, RegisterBlock> = BTreeMap::new();
        Self { cache }
    }

    fn get_pin_config(
        &mut self,
        context: &mut ExecutionContext,
        group: &str,
        pin: u32,
    ) -> Result<PinConfig> {
        let rb = match self.cache.get(group) {
            None => {
                let rb = read_gpio_register_block(context, group)?;
                self.cache.insert(group.to_string(), rb);
                self.cache.get(group).unwrap()
            }
            Some(rb) => rb,
        };

        let reg = rb.moder.read().bits();
        let mode = match (reg >> (pin * 2)) & 3 {
            0 => "Input",
            1 => "Output",
            2 => "Alternate",
            3 => "Analog",
            _ => unreachable!(),
        };

        let reg = rb.otyper.read().bits();
        let otype = match (reg >> pin) & 1 {
            0 => "OutPushPull",
            1 => "OutOpenDrain",
            _ => unreachable!(),
        };

        let reg = rb.ospeedr.read().bits();
        let speed = match (reg >> (pin * 2)) & 3 {
            0 => "LowSpeed",
            1 => "MediumSpeed",
            2 => "HighSpeed",
            3 => "VeryHighSpeed",
            _ => unreachable!(),
        };

        let reg = rb.pupdr.read().bits();
        let pull = match (reg >> (pin * 2)) & 3 {
            0 => "NoPull",
            1 => "PullUp",
            2 => "PullDown",
            3 => "ReservedPull",
            _ => unreachable!(),
        };

        let reg = rb.idr.read().bits();
        let input = match (reg >> pin) & 1 {
            0 => "InZero",
            1 => "InOne",
            _ => unreachable!(),
        };

        let reg = rb.odr.read().bits();
        let output = match (reg >> pin) & 1 {
            0 => "OutZero",
            1 => "OutOne",
            _ => unreachable!(),
        };

        // Note: Bit 16 indicates that the register is locked or unlocked.
        let reg = rb.lckr.read().bits();
        let lock = match (reg >> pin) & 1 {
            0 => "Unlocked",
            1 => "Locked",
            _ => unreachable!(),
        };

        let alternate = format!(
            "AF{}",
            if pin < 8 {
                let reg = rb.afrl.read().bits();
                (reg >> (pin * 4)) & 15
            } else {
                let reg = rb.afrh.read().bits();
                (reg >> ((pin - 8) * 4)) & 15
            }
        );

        Ok(PinConfig {
            mode: mode.to_string(),
            otype: otype.to_string(),
            speed: speed.to_string(),
            pull: pull.to_string(),
            input: input.to_string(),
            output: output.to_string(),
            lock: lock.to_string(),
            alternate,
        })
    }
}

fn read_gpio_register_block(
    context: &mut ExecutionContext,
    group: &str,
) -> Result<RegisterBlock> {
    let core = &mut **context.core.as_mut().unwrap();
    let span = GPIO_REGISTERS
        .get(group)
        .ok_or(anyhow!("no address for GPIO group {}", group))?;
    let mut buffer = vec![0u8; std::mem::size_of::<RegisterBlock>()];
    core.read_8(*span.start(), &mut buffer)?;

    let mut rb: MaybeUninit<RegisterBlock> = MaybeUninit::uninit();
    // let ptr: *mut u8 = rb.as_mut_ptr() as *mut u8;
    unsafe {
        // Get a mutable pointer to the allocated memory
        let dest_ptr = rb.as_mut_ptr() as *mut u8;

        // Copy the raw bytes into the struct's memory location
        // Ensure the size of raw_bytes matches the size of MyStruct
        // to avoid out-of-bounds access.
        std::ptr::copy_nonoverlapping(buffer.as_ptr(), dest_ptr, buffer.len());

        Ok(rb.assume_init())
    }
}

fn show_gpio_with_config(
    context: &mut ExecutionContext,
    gpio_input: &HiffyFunction,
    args: &[(u16, Option<u8>, String)],
    results: &[Result<Vec<u8>, IpcError>],
) -> Result<()> {
    let mut config_cache = ConfigCache::new();
    for (ndx, arg) in args.iter().enumerate() {
        match arg.1 {
            Some(pin) => {
                let config =
                    config_cache.get_pin_config(context, &arg.2, pin as u32)?;
                println!(
                    "{}:{:<2} = {} {}",
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
                    },
                    config
                );
            }

            None => match results[ndx] {
                Err(code) => {
                    println!("Port {}: {}", arg.2, gpio_input.strerror(code))
                }
                Ok(ref val) => {
                    let arr: &[u8; 2] = val[0..2].try_into()?;
                    let v = u16::from_le_bytes(*arr);

                    for i in 0..16 {
                        let config = config_cache
                            .get_pin_config(context, &arg.2, i as u32)?;
                        println!(
                            "{}:{:<2} = {} ({})",
                            arg.2,
                            i,
                            (v >> i) & 1,
                            config
                        );
                    }
                }
            },
        }
    }
    Ok(())
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

    if subargs.input {
        let mut header = false;

        if subargs.with_config {
            let _ =
                show_gpio_with_config(context, &gpio_input, &args, &results);
        } else {
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
