// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility sbrmi`
//!

use anyhow::{anyhow, Result};
use clap::{CommandFactory, Parser};
use hif::*;
use humility::cli::Subcommand;
use humility::core::Core;
use humility::hubris::*;
use humility::reflect;
use humility_cmd::hiffy::*;
use humility_cmd::idol::{self, HubrisIdol};
use humility_cmd::CommandKind;
use humility_cmd::{Archive, Attach, Command, Validate};
use raw_cpuid::{CpuId, CpuIdResult};

//
// We pull in more or less verbatim the `cpuid` program from the raw-cpuid
// crate.  Skip formatting it and running Clippy against it to minimize
// our differences with upstream should we wish to update it over time.
//
#[rustfmt::skip]
#[allow(clippy::all)]
mod cpuid;

use std::cell::RefCell;

#[derive(Parser, Debug)]
#[clap(
    name = "sbrmi", about = env!("CARGO_PKG_DESCRIPTION"),
)]
struct SbrmiArgs {
    /// sets timeout
    #[clap(
        long, short = 'T', default_value_t = 5000, value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// thread to operate upon
    #[clap(
        long, short, parse(try_from_str = parse_int::parse),
        default_value = "0"
    )]
    thread: u8,

    /// CPUID operations on target thread
    #[clap(long, short)]
    cpuid: bool,
}

fn call_cpuid(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
    thread: u8,
    eax: u32,
    ecx: u32,
) -> Result<CpuIdResult> {
    let op = hubris.get_idol_command("Sbrmi.cpuid")?;
    let funcs = context.functions()?;
    let mut ops = vec![];

    let payload = op.payload(&[
        ("thread", idol::IdolArgument::Scalar(thread as u64)),
        ("eax", idol::IdolArgument::Scalar(eax as u64)),
        ("ecx", idol::IdolArgument::Scalar(ecx as u64)),
    ])?;

    context.idol_call_ops(&funcs, &op, &payload, &mut ops)?;
    ops.push(Op::Done);

    let results = context.run(core, ops.as_slice(), None)?;
    let result = context.idol_result(&op, &results[0])?;
    let registers = result.as_struct()?["value"].as_array()?;

    let register = |ndx| {
        let b: &reflect::Value = &registers[ndx];
        b.as_base()?
            .as_u32()
            .ok_or_else(|| anyhow!("couldn't decode index {ndx} as u32"))
    };

    Ok(CpuIdResult {
        eax: register(0)?,
        ebx: register(1)?,
        ecx: register(2)?,
        edx: register(3)?,
    })
}

fn cpuid(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
    thread: u8,
) -> Result<()> {
    struct SbrmiWorkspace {
        hubris: Option<std::ptr::NonNull<HubrisArchive>>,
        core: Option<std::ptr::NonNull<dyn Core>>,
        context: Option<std::ptr::NonNull<HiffyContext<'static>>>,
        thread: Option<u8>,
    }

    thread_local! {
        static SBRMI_WORKSPACE: RefCell<SbrmiWorkspace> =
            RefCell::new(
                SbrmiWorkspace {
                    hubris: None,
                    core: None,
                    context: None,
                    thread: None,
                });
    }

    fn cb(eax: u32, ecx: u32) -> CpuIdResult {
        SBRMI_WORKSPACE.with(|workspace| {
            let workspace = workspace.borrow_mut();
            let (hubris, core, context) = {
                // Hold Matt's beer
                unsafe {
                    (
                        workspace.hubris.unwrap().as_ref(),
                        workspace.core.unwrap().as_mut(),
                        workspace.context.unwrap().as_mut(),
                    )
                }
            };

            let thread = workspace.thread.unwrap();

            match call_cpuid(hubris, core, context, thread, eax, ecx) {
                Err(e) => {
                    //
                    // We unfortunately have no ability to fail, so we
                    // just bail all the way out.
                    //
                    eprintln!("humility sbrmi failed: {:?}", e);
                    std::process::exit(1);
                }
                Ok(result) => result,
            }
        })
    }

    SBRMI_WORKSPACE.with(|workspace| {
        let mut workspace = workspace.borrow_mut();
        *workspace = SbrmiWorkspace {
            hubris: Some(std::ptr::NonNull::from(hubris)),
            core: Some(
                std::ptr::NonNull::new(unsafe {
                    std::mem::transmute::<
                        *mut (dyn Core + '_),
                        *mut (dyn Core + 'static),
                    >(core as *mut dyn Core)
                })
                .unwrap(),
            ),
            context: Some(
                std::ptr::NonNull::new(unsafe {
                    std::mem::transmute::<
                        &mut HiffyContext<'_>,
                        &mut HiffyContext<'static>,
                    >(context)
                })
                .unwrap(),
            ),
            thread: Some(thread),
        };
    });

    let cpuid = CpuId::with_cpuid_fn(cb);
    crate::cpuid::markdown(&cpuid);

    Ok(())
}

fn sbrmi(context: &mut humility::ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let subargs = SbrmiArgs::try_parse_from(subargs)?;
    let hubris = context.archive.as_ref().unwrap();
    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;

    if subargs.cpuid {
        return cpuid(hubris, core, &mut context, subargs.thread);
    }

    Ok(())
}

pub fn init() -> Command {
    Command {
        app: SbrmiArgs::command(),
        name: "sbrmi",
        run: sbrmi,
        kind: CommandKind::Attached {
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
        },
    }
}
