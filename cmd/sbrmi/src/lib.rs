// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility sbrmi`
//!

use anyhow::{anyhow, Result};
use clap::{CommandFactory, Parser};
use colored::Colorize;
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
use std::collections::HashSet;

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
    //
    // We want to call into the raw_cpuid crate to pull and display CPUID
    // information, but it unfortunately takes only a function, not a
    // closure.  (It -- understndably -- didn't forsee the CPUID instruction
    // being run via a sidechannel I2C interface through an intermediary
    // operating at a distance!)  We resort to the same technique used by the
    // Hiffy RPC engine for its client-side HIF emulation:  we create a
    // structure to hold the pointers to things that we need, and fill it with
    // more or less raw pointers, transmuting away all lifetimes in the
    // process.  We dispense with some of the precaution taken in that code
    // because this isn't callable by anyone else, and the stakes here are
    // generally low: we're going to call into the raw_cpuid code to
    // prett-print the results of the proxied CPUID, and then we're going to
    // exit.
    //
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

        // Cough, cough.
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

fn threadmap(arr: &reflect::Array) -> Result<HashSet<u8>> {
    let mut rval = HashSet::new();

    for (base, elem) in arr.iter().enumerate() {
        let val = elem.as_base()?.as_u8().unwrap();

        for bit in 0..8 {
            if val & (1 << bit) != 0 {
                rval.insert(((base * 8) + bit) as u8);
            }
        }
    }

    Ok(rval)
}

#[allow(non_camel_case_types, dead_code)]
#[derive(Copy, Clone, Debug)]
enum Msr {
    MCG_CAP,
    MCG_STATUS,
    MCA_CTL(u8),
    MCA_STATUS(u8),
    MCA_ADDR(u8),
    MCA_MISC(u8),
    MCA_CONFIG(u8),
    MCA_SYND(u8),
    MCA_IPID(u8),
    MCA_DESTAT(u8),
    MCA_DEADDR(u8),
}

impl From<Msr> for u32 {
    fn from(msr: Msr) -> Self {
        let banked = |bank, offs| 0xc000_2000 + (bank as u32 * 0x10) + offs;

        match msr {
            Msr::MCG_CAP => 0x0000_0179,
            Msr::MCG_STATUS => 0x0000_017a,
            Msr::MCA_CTL(bank) => banked(bank, 0),
            Msr::MCA_STATUS(bank) => banked(bank, 1),
            Msr::MCA_ADDR(bank) => banked(bank, 2),
            Msr::MCA_MISC(bank) => banked(bank, 3),
            Msr::MCA_CONFIG(bank) => banked(bank, 4),
            Msr::MCA_IPID(bank) => banked(bank, 5),
            Msr::MCA_SYND(bank) => banked(bank, 6),
            Msr::MCA_DESTAT(bank) => banked(bank, 8),
            Msr::MCA_DEADDR(bank) => banked(bank, 9),
        }
    }
}

impl From<Msr> for u64 {
    fn from(msr: Msr) -> Self {
        u32::from(msr) as u64
    }
}

struct Field {
    highbit: u16,
    lowbit: u16,
    name: &'static str,
    mneumonic: Option<&'static str>,
}

impl Field {
    fn field(
        highbit: u16,
        lowbit: u16,
        name: &'static str,
        mneumonic: Option<&'static str>,
    ) -> Self {
        Self { highbit, lowbit, name, mneumonic }
    }

    fn bit(
        bit: u16,
        name: &'static str,
        mneumonic: Option<&'static str>,
    ) -> Self {
        Self { highbit: bit, lowbit: bit, name, mneumonic }
    }
}

impl Msr {
    fn fields(&self) -> Option<Vec<Field>> {
        match self {
            Msr::MCA_STATUS(_) => Some(vec![
                Field::bit(63, "Valid", Some("VAL")),
                Field::bit(62, "Overflow", Some("OVER")),
                Field::bit(61, "Uncorrected error", Some("UC")),
                Field::bit(60, "Error condition enabled", Some("EN")),
                Field::bit(59, "Misc error register valid", Some("MISCV")),
                Field::bit(58, "Address register valid", Some("ADDRV")),
                Field::bit(57, "Processor context corrupt", Some("PCC")),
                Field::bit(55, "Task content corrupt", Some("TCC")),
                Field::bit(53, "Syndrome register valid", Some("SYNDV")),
                Field::bit(44, "Deferred error", Some("Deferred")),
                Field::bit(43, "Poisoned data consumed", Some("Poison")),
                Field::field(31, 16, "Extended Error Code", None),
                Field::field(15, 0, "MCA Error Code", None),
            ]),
            Msr::MCA_CONFIG(_) => Some(vec![
                Field::bit(40, "Interrupt Enable", Some("IntEn")),
                Field::field(38, 37, "Deferred error type", None),
                Field::bit(32, "MCAX enable", Some("McaxEn")),
                Field::bit(9, "MCA FRU text supported", None),
                Field::bit(8, "Address LSB in MCA_STATUS", None),
                Field::bit(7, "Deferred error status supported", None),
                Field::bit(6, "System fatal error event supported", None),
                Field::bit(5, "Deferred interrupt type supported", None),
                Field::bit(2, "Deferred error logging supported", None),
                Field::bit(0, "MCAX capable", None),
            ]),
            _ => None,
        }
    }
}

fn mce(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
    thread: u8,
    mcg_cap: u64,
) -> Result<()> {
    //
    // This is all a little dirty.
    //
    let nbanks = (mcg_cap & 0xff) as u8;
    let op = hubris.get_idol_command("Sbrmi.rdmsr64")?;
    let mut ops = vec![];
    let funcs = context.functions()?;

    for bank in 0..nbanks {
        let payload = op.payload(&[
            ("thread", idol::IdolArgument::Scalar(thread as u64)),
            ("msr", idol::IdolArgument::Scalar(Msr::MCA_STATUS(bank).into())),
        ])?;

        context.idol_call_ops(&funcs, &op, &payload, &mut ops)?;
    }

    ops.push(Op::Done);

    let results = context.run(core, ops.as_slice(), None)?;

    for (bank, r) in results.iter().enumerate() {
        let status = context.idol_result(&op, r)?.as_base()?.as_u64().unwrap();

        if status == 0 {
            continue;
        }

        println!("=== Bank {}", bank);

        ops = vec![];

        let bank = bank as u8;

        let allregs = vec![
            Msr::MCA_CTL(bank),
            Msr::MCA_STATUS(bank),
            Msr::MCA_ADDR(bank),
            Msr::MCA_MISC(bank),
            Msr::MCA_CONFIG(bank),
            Msr::MCA_SYND(bank),
            Msr::MCA_IPID(bank),
            Msr::MCA_DESTAT(bank),
            Msr::MCA_DEADDR(bank),
        ];

        for reg in &allregs {
            let r = *reg;

            let payload = op.payload(&[
                ("thread", idol::IdolArgument::Scalar(thread as u64)),
                ("msr", idol::IdolArgument::Scalar(r.into())),
            ])?;

            context.idol_call_ops(&funcs, &op, &payload, &mut ops)?;
        }

        ops.push(Op::Done);

        let reg_results = context.run(core, ops.as_slice(), None)?;

        for (ndx, reg) in reg_results.iter().enumerate() {
            let v = context.idol_result(&op, reg)?.as_base()?.as_u64().unwrap();

            let name = format!("{:?}", allregs[ndx]);
            println!("    {name:17} 0x{v:016x}");

            if let Some(fields) = allregs[ndx].fields() {
                let blank = "";
                print!("{blank:22}|\n{blank:22}+---> ");

                for (i, field) in fields.iter().enumerate() {
                    let value = if field.highbit == field.lowbit {
                        if v & (1 << field.highbit) != 0 {
                            "true".to_string()
                        } else {
                            "false".to_string()
                        }
                    } else {
                        let mask =
                            (1u64 << (field.highbit - field.lowbit + 1)) - 1;
                        let f = (v >> field.lowbit) & mask;

                        if f >= 10 {
                            format!("{:#x} ({})", f, f)
                        } else {
                            format!("{}", f)
                        }
                    };

                    let name = if let Some(mneumonic) = field.mneumonic {
                        format!("{} ({mneumonic})", field.name)
                    } else {
                        format!("{}", field.name)
                    };

                    if i == 0 {
                        println!("{name:35} = {value}");
                    } else {
                        println!("{blank:28}{name:35} = {value}");
                    }
                }

                println!();
            }
        }
    }

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

    let mut ops = vec![];
    let funcs = context.functions()?;

    let nthreads = hubris.get_idol_command("Sbrmi.nthreads")?;
    context.idol_call_ops(&funcs, &nthreads, &[], &mut ops)?;

    let enabled = hubris.get_idol_command("Sbrmi.enabled")?;
    context.idol_call_ops(&funcs, &enabled, &[], &mut ops)?;

    let alert = hubris.get_idol_command("Sbrmi.alert")?;
    context.idol_call_ops(&funcs, &alert, &[], &mut ops)?;

    let mcg_cap = hubris.get_idol_command("Sbrmi.rdmsr64")?;

    let payload = mcg_cap.payload(&[
        ("thread", idol::IdolArgument::Scalar(0 as u64)),
        ("msr", idol::IdolArgument::Scalar(Msr::MCG_CAP.into())),
    ])?;

    context.idol_call_ops(&funcs, &mcg_cap, &payload, &mut ops)?;

    ops.push(Op::Done);

    let results = context.run(core, ops.as_slice(), None)?;

    let nthreads = context
        .idol_result(&nthreads, &results[0])?
        .as_base()?
        .as_u8()
        .unwrap();

    let result = context.idol_result(&enabled, &results[1])?;
    let enabled = threadmap(result.as_struct()?["value"].as_array()?)?;

    let result = context.idol_result(&alert, &results[2])?;
    let alert = threadmap(result.as_struct()?["value"].as_array()?)?;

    print!(" THR");

    for row in 0..16 {
        print!(" 0x{:1x}", row);
    }

    println!();

    for row in 0..8 {
        print!("0x{:02x}", (row * 16));

        for col in 0..16 {
            let thread = (row * 16) + col as u8;

            if thread < nthreads {
                if alert.contains(&thread) {
                    print!("{:>4}", "MCE".red());
                } else if enabled.contains(&thread) {
                    print!("{:>4}", "ok".green());
                } else {
                    print!("{:>4}", "?".blue());
                }
            } else {
                print!("{:>4}", "-");
            }
        }

        println!();
    }

    println!();

    let mcg_cap = context
        .idol_result(&mcg_cap, &results[3])?
        .as_base()?
        .as_u64()
        .unwrap();

    for thread in alert {
        println!("MCE on 0x{thread:x} ({thread}):");
        mce(hubris, core, &mut context, thread, mcg_cap)?;
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
