// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility break`
//!
//! Gosh writing some things here would be swell!
//!
use anyhow::Result;
use clap::Command as ClapCommand;
use clap::{CommandFactory, Parser};
use humility::cli::Subcommand;
use humility_cmd::{Archive, Attach, Command, Validate};

#[derive(Parser, Debug)]
#[clap(name = "subcmd")]
enum BreakCmd {
    /// List breakpoints
    List,
    /// Set breakpoint
    SetByAddr {
        #[clap(parse(try_from_str = parse_int::parse))]
        addr: u32,
    },
    /// Clear breakpoint
    ClearByAddr {
        #[clap(parse(try_from_str = parse_int::parse))]
        addr: u32,
    },
    SetByName {
        task: String,
        name: String,
    },
    SetByFile {
        task: String,
        path: String,
        line: usize,
    },
    Continue,
    Step,
}

#[derive(Parser, Debug)]
#[clap(name = "break", about = env!("CARGO_PKG_DESCRIPTION"))]
struct BreakArgs {
    #[clap(subcommand)]
    cmd: BreakCmd,
}

fn break_cmd(context: &mut humility::ExecutionContext) -> Result<()> {
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let subargs = BreakArgs::try_parse_from(subargs)?;
    let core = &mut **context.core.as_mut().unwrap();
    let hubris = context.archive.as_ref().unwrap();

    match subargs.cmd {
        BreakCmd::List => {
            let regions = hubris.regions(core).unwrap_or_default();
            for (i, b) in core.list_breakpoints()?.iter().enumerate() {
                let out = match b {
                    None => "(unset)".to_string(),
                    Some(v) => match hubris.explain(&regions, *v) {
                        Some(explain) => format!(" {:x} <- {}", v, explain),
                        None => format!(" {:x} <- (Not in hubris)", v),
                    },
                };
                println!("Break {}: {}", i, out);
            }
        }
        BreakCmd::SetByAddr { addr } => {
            core.set_breakpoint(addr)?;
            println!("Set breakpoint at {:x}", addr);
        }
        BreakCmd::ClearByAddr { addr } => {
            core.clear_breakpoint(addr)?;
            println!("Clear breakpoint at {:x}", addr);
        }
        BreakCmd::SetByName { task, name } => {
            let module = hubris.lookup_module_by_name(&task)?;
            let addr = hubris.lookup_function_addr(&module, &name)?;
            core.set_breakpoint(addr)?;
            println!("Breakpoint for {}:{} @ {:x}", task, name, addr);
        }
        BreakCmd::SetByFile { task, path, line } => {
            let module = hubris.lookup_module_by_name(&task)?;
            let (addr, exact) =
                hubris.get_addr_from_line(&module, &path, line as u64)?;

            if exact {
                println!(
                    "Found exact match for {}:{}:{} @ {:x}",
                    task, path, line, addr
                );
            } else {
                println!(
                    "Found inexact match for {}:{}:{} @ {:x}",
                    task, path, line, addr
                );
            }
            core.set_breakpoint(addr as u32)?;
        }
        BreakCmd::Continue => {
            core.run()?;
        }
        BreakCmd::Step => {
            core.step()?;
        }
    }

    Ok(())
}

pub fn init() -> (Command, ClapCommand<'static>) {
    (
        Command::Attached {
            name: "break",
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
            run: break_cmd,
        },
        BreakArgs::command(),
    )
}
