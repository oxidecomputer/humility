// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility registers`
//!

use anyhow::{bail, Result};
use clap::Command as ClapCommand;
use clap::{CommandFactory, Parser};
use humility::arch::{ARMRegister, ARMRegisterField};
use humility::core::Core;
use humility::hubris::*;
use humility_cmd::{Archive, Args, Attach, Command, Validate};
use humility_cortex::debug::*;
use num_traits::FromPrimitive;
use std::collections::BTreeMap;

#[derive(Parser, Debug)]
#[clap(name = "registers", about = env!("CARGO_PKG_DESCRIPTION"))]
struct RegistersArgs {
    /// show stack backtrace
    #[clap(long, short)]
    stack: bool,

    /// show line number information with stack backtrace
    #[clap(long, short, requires = "stack")]
    line: bool,

    /// show floating point registers
    #[clap(long = "floating-point", short)]
    fp: bool,
}

fn print_reg(reg: ARMRegister, val: u32, fields: &[ARMRegisterField]) {
    print!("{:>5} = 0x{:08x} <- ", reg, val);
    let indent = 5 + "= 0x00000000 <- ".len();

    for i in (0..32).step_by(4).rev() {
        print!("{:04b}", (val >> i) & 0b1111);

        if i != 0 {
            print!("_");
        } else {
            println!();
        }
    }

    fn print_bars(f: &[ARMRegisterField], elbow: bool) {
        let mut pos = 32;

        for i in 0..f.len() {
            while pos > f[i].lowbit {
                print!(" ");

                if pos % 4 == 0 && pos != 32 {
                    print!(" ");
                }

                pos -= 1;
            }

            if elbow && i == f.len() - 1 {
                print!("+");

                while pos > 0 {
                    print!("-");
                    if pos % 4 == 0 && pos != 32 {
                        print!("-");
                    }

                    pos -= 1;
                }

                print!(" ");
                break;
            }

            print!("|");

            if pos == 0 {
                break;
            }

            if pos % 4 == 0 && pos != 32 {
                print!(" ");
            }
            pos -= 1;
        }
    }

    print!("{:indent$}", "");
    print_bars(fields, false);
    println!();

    for (ndx, field) in fields.iter().enumerate().rev() {
        print!("{:indent$}", "");
        print_bars(&fields[0..=ndx], true);

        let mask = ((1u64 << (field.highbit - field.lowbit + 1)) - 1) as u32;

        if mask == 1 {
            println!("{} = {}", field.name, (val >> field.lowbit) & mask);
        } else {
            println!("{} = 0x{:x}", field.name, (val >> field.lowbit) & mask);
        }
    }

    println!();
}

fn registers(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    _args: &Args,
    subargs: &[String],
) -> Result<()> {
    let subargs = RegistersArgs::try_parse_from(subargs)?;
    let mut regs = BTreeMap::new();

    if subargs.fp && !core.is_dump() {
        let mvfr = MVFR0::read(core)?;

        if mvfr.simd_registers() != 1 {
            bail!("microcontroller does not support floating point");
        }
    }

    core.halt()?;

    let regions = match hubris.regions(core) {
        Ok(regions) => regions,
        Err(err) => {
            //
            // If we can't ascertain our memory regions, we will drive on.  (If
            // we were provided a dump/archive, we will also print a message to
            // indicate that regions can't be loadwed.)
            //
            if hubris.loaded() {
                humility::msg!("failed to determine memory regions: {}", err);
            }

            BTreeMap::new()
        }
    };

    //
    // Read all of our registers first...
    //
    for i in 0..=ARMRegister::max() {
        let reg = match ARMRegister::from_u16(i) {
            Some(r) => r,
            None => {
                continue;
            }
        };

        if reg.is_floating_point() && !subargs.fp {
            continue;
        }

        let val = match core.read_reg(reg) {
            Ok(val) => val,
            Err(_) => {
                continue;
            }
        };

        regs.insert(reg, val);
    }

    let printer = humility_cmd::stack::StackPrinter {
        indent: 8,
        line: subargs.line,
        ..Default::default()
    };

    for (reg, val) in regs.iter() {
        let val = *val;

        if let Some(fields) = reg.fields() {
            print_reg(*reg, val, &fields);
            continue;
        }

        println!(
            "{:>5} = 0x{:08x}{}",
            reg,
            val,
            if !reg.is_floating_point() {
                match hubris.explain(&regions, val) {
                    Some(explain) => format!(" <- {}", explain),
                    None => "".to_string(),
                }
            } else {
                "".to_string()
            }
        );

        if subargs.stack && *reg == ARMRegister::SP {
            if let Some((_, region)) = regions.range(..=val).next_back() {
                let task = if region.tasks.len() == 1 {
                    region.tasks[0]
                } else {
                    humility::msg!(
                        "multiple tasks map 0x{:x}: {:?}",
                        val,
                        region.tasks
                    );
                    continue;
                };

                match hubris.stack(core, task, region.base + region.size, &regs)
                {
                    Ok(stack) => printer.print(hubris, &stack),
                    Err(e) => {
                        //
                        // If this a kernel stack and it's a dump, it's quite
                        // likely that this dump pre-dates our dumping of
                        // kernel stacks; in classic Humility fashion, phrase
                        // our hunch in the form of a question.
                        //
                        if core.is_dump() && task == HubrisTask::Kernel {
                            humility::msg!(
                                "kernel stack missing; \
                                does the dump pre-date dumped kernel stacks?"
                            );
                        } else {
                            humility::msg!("stack unwind failed: {:?} ", e);
                        }

                        continue;
                    }
                }
            } else {
                humility::msg!("unknown region for SP 0x{:08x}", val);
            }
        }
    }

    core.run()?;

    Ok(())
}

pub fn init() -> (Command, ClapCommand<'static>) {
    (
        Command::Attached {
            name: "registers",
            archive: Archive::Optional,
            attach: Attach::Any,
            validate: Validate::None,
            run: registers,
        },
        RegistersArgs::command(),
    )
}
