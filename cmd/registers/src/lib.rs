// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility registers`
//!

use anyhow::{anyhow, bail, Result};
use clap::Command as ClapCommand;
use clap::{CommandFactory, Parser};
use humility::arch::ARMRegister;
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

fn print_stack(
    hubris: &HubrisArchive,
    stack: &[HubrisStackFrame],
    subargs: &RegistersArgs,
) {
    for frame in stack {
        let pc = frame.registers.get(&ARMRegister::PC).unwrap();

        if let Some(ref inlined) = frame.inlined {
            for inline in inlined {
                println!(
                    "0x{:08x} 0x{:08x} {}",
                    frame.cfa, inline.addr, inline.name
                );

                if subargs.line {
                    if let Some(src) = hubris.lookup_src(inline.origin) {
                        println!("{:11}@ {}:{}", "", src.fullpath(), src.line);
                    }
                }
            }
        }

        if let Some(sym) = frame.sym {
            println!(
                "0x{:08x} 0x{:08x} {}",
                frame.cfa, *pc, sym.demangled_name
            );

            if subargs.line {
                if let Some(src) = hubris.lookup_src(sym.goff) {
                    println!("{:11}@ {}:{}", "", src.fullpath(), src.line);
                }
            }
        } else {
            println!("0x{:08x} 0x{:08x}", frame.cfa, *pc);
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

    if subargs.fp {
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

        println!(
            "{:>5} = 0x{:08x}{}",
            format!("{:?}", reg),
            val,
            if reg.is_general_purpose() {
                match hubris.explain(&regions, val) {
                    Some(explain) => format!("  <- {}", explain),
                    None => "".to_string(),
                }
            } else {
                "".to_string()
            }
        );
    }

    if subargs.stack {
        //
        // Determine the task that contains the SP
        //
        let sp = regs.get(&ARMRegister::SP).ok_or_else(|| {
            let _ = core.run();
            anyhow!("can't print stack: SP missing")
        })?;

        if let Some((_, region)) = regions.range(..=sp).next_back() {
            let task = if region.tasks.len() == 1 {
                region.tasks[0]
            } else {
                core.run()?;
                bail!("multiple tasks map 0x{:x}: {:?}", sp, region.tasks);
            };

            match hubris.stack(core, task, region.base + region.size, &regs) {
                Ok(stack) => print_stack(hubris, &stack, &subargs),
                Err(e) => {
                    eprintln!("   stack unwind failed: {:?} ", e);
                }
            }
        } else {
            core.run()?;
            bail!("unknown region for SP 0x{:08x}", sp);
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
