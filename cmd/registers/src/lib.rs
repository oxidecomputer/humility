// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility registers`
//!
//! `humility registers` displays the registers from either a live system
//! or a dump, e.g.:
//!
//! ```console
//! % humility registers
//! humility: attached via ST-Link V3
//!    R0 = 0x00000000
//!    R1 = 0x0000000a
//!    R2 = 0x80000000
//!    R3 = 0x00000000
//!    R4 = 0x00000000
//!    R5 = 0x0000f406
//!    R6 = 0x00002711
//!    R7 = 0x20000310
//!    R8 = 0x00000000
//!    R9 = 0x00000000
//!   R10 = 0x20000f68
//!   R11 = 0x00000001
//!   R12 = 0x200002b4
//!    SP = 0x200002e8
//!    LR = 0x0800414f
//!    PC = 0x08004236
//!   PSR = 0x4100000f <- 0100_0001_0000_0000_0000_0000_0000_1111
//!                       |||| | ||         |       |           |
//!                       |||| | ||         |       |           + Exception = 0xf
//!                       |||| | ||         |       +------------ IC/IT = 0x0
//!                       |||| | ||         +-------------------- GE = 0x0
//!                       |||| | |+------------------------------ T = 1
//!                       |||| | +------------------------------- IC/IT = 0x0
//!                       |||| +--------------------------------- Q = 0
//!                       |||+----------------------------------- V = 0
//!                       ||+------------------------------------ C = 0
//!                       |+------------------------------------- Z = 1
//!                       +-------------------------------------- N = 0
//!
//!   MSP = 0x200002e8
//!   PSP = 0x20011ab0
//!   SPR = 0x01000001 <- 0000_0001_0000_0000_0000_0000_0000_0001
//!                             |||         |         |         |
//!                             |||         |         |         + PRIMASK = 1
//!                             |||         |         +---------- BASEPRI = 0x0
//!                             |||         +-------------------- FAULTMASK = 0
//!                             ||+------------------------------ CONTROL.nPRIV = 1
//!                             |+------------------------------- CONTROL.SPSEL = 0
//!                             +-------------------------------- CONTROL.FPCA = 0
//!
//! FPSCR = 0x00000000
//! ```
//!
//! If an archive is provided or if displaying registers from a dump, the
//! symbol that corresponds to register's value (if any) is displayed, e.g.:
//!
//! ```console
//! % humility -d ./hubris.core.81 registers
//! humility: attached to dump
//!    R0 = 0x00000000
//!    R1 = 0x0000000a
//!    R2 = 0x80000000
//!    R3 = 0x00000000
//!    R4 = 0x00000000
//!    R5 = 0x0000f406
//!    R6 = 0x00002711
//!    R7 = 0x20000310 <- kernel: 0x20000000+0x310
//!    R8 = 0x00000000
//!    R9 = 0x00000000
//!   R10 = 0x20000f68 <- kernel: DEVICE_PERIPHERALS+0x0
//!   R11 = 0x00000001
//!   R12 = 0x200002b4 <- kernel: 0x20000000+0x2b4
//!    SP = 0x200002e8 <- kernel: 0x20000000+0x2e8
//!    LR = 0x0800414f <- kernel: write_str<cortex_m::itm::Port>+0xd
//!    PC = 0x08004236 <- kernel: panic+0x36
//!   PSR = 0x4100000f <- 0100_0001_0000_0000_0000_0000_0000_1111
//!                       |||| | ||         |       |           |
//!                       |||| | ||         |       |           + Exception = 0xf
//!                       |||| | ||         |       +------------ IC/IT = 0x0
//!                       |||| | ||         +-------------------- GE = 0x0
//!                       |||| | |+------------------------------ T = 1
//!                       |||| | +------------------------------- IC/IT = 0x0
//!                       |||| +--------------------------------- Q = 0
//!                       |||+----------------------------------- V = 0
//!                       ||+------------------------------------ C = 0
//!                       |+------------------------------------- Z = 1
//!                       +-------------------------------------- N = 0
//!
//!   MSP = 0x200002e8 <- kernel: 0x20000000+0x2e8
//!   PSP = 0x20011ab0 <- pong: 0x20011800+0x2b0
//!   SPR = 0x01000001 <- 0000_0001_0000_0000_0000_0000_0000_0001
//!                             |||         |         |         |
//!                             |||         |         |         + PRIMASK = 1
//!                             |||         |         +---------- BASEPRI = 0x0
//!                             |||         +-------------------- FAULTMASK = 0
//!                             ||+------------------------------ CONTROL.nPRIV = 1
//!                             |+------------------------------- CONTROL.SPSEL = 0
//!                             +-------------------------------- CONTROL.FPCA = 0
//!
//! ```
//!
//! To display a stack backtrace, use the `--stack` (`-s`) option:
//!
//! ```console
//! % humility -d ./hubris.core.81 registers --stack
//! humility: attached to dump
//!    R0 = 0x00000000
//! ...
//!   R10 = 0x20000f68 <- kernel: DEVICE_PERIPHERALS+0x0
//!   R11 = 0x00000001
//!   R12 = 0x200002b4 <- kernel: 0x20000000+0x2b4
//!    SP = 0x200002e8 <- kernel: 0x20000000+0x2e8
//!         |
//!         +--->  0x20000318 0x08004236 rust_begin_unwind
//!                0x20000330 0x08000558 core::panicking::panic_fmt
//!                0x20000358 0x08000ad8 core::panicking::panic
//!                0x20000390 0x08003ba6 kern::arch::arm_m::safe_sys_tick_handler
//!                0x20000390 0x08003ba6 kern::arch::arm_m::SysTick::{{closure}}
//!                0x20000390 0x08003ba6 kern::arch::arm_m::with_task_table
//!                0x20000390 0x08003bb0 SysTick
//!
//!    LR = 0x0800414f <- kernel: write_str<cortex_m::itm::Port>+0xd
//! ...
//! ```
//!
//! To additionally display floating point registers on platforms that support
//! floating point, use the `--floating-point` (`-f`) option.
//!

use anyhow::{bail, Result};
use clap::{CommandFactory, Parser};
use humility::arch::{ARMRegister, ARMRegisterField};
use humility::cli::Subcommand;
use humility::hubris::*;
use humility_cmd::{Archive, Attach, Command, CommandKind, Validate};
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

fn registers(context: &mut humility::ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let subargs = RegistersArgs::try_parse_from(subargs)?;
    let mut regs = BTreeMap::new();
    let hubris = context.archive.as_ref().unwrap();

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

pub fn init() -> Command {
    Command {
        app: RegistersArgs::command(),
        name: "registers",
        run: registers,
        kind: CommandKind::Attached {
            archive: Archive::Optional,
            attach: Attach::Any,
            validate: Validate::None,
        },
    }
}
