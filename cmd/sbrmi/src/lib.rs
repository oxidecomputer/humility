// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility sbrmi`
//!
//! Print out information retrieved via AMD's sideband remote management
//! interface (SB-RMI).  This interface is somewhat limited in its utility,
//! but can sometimes yield information not available via other means.
//!
//! When run without arguments, this will indicate the presence of each
//! thread; if all are present and not in alert status, one will see
//! output like:
//!
//! ```console
//!  THR 0x0 0x1 0x2 0x3 0x4 0x5 0x6 0x7 0x8 0x9 0xa 0xb 0xc 0xd 0xe 0xf
//! 0x00  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok
//! 0x10  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok
//! 0x20  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok
//! 0x30  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok
//! 0x40  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok
//! 0x50  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok
//! 0x60  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok
//! 0x70  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok
//! ```
//!
//! If any CPU has its alert bit set, it will be marked as `MCE`, and the
//! machine check exception information will be displayed, e.g.:
//!
//! ```console
//!  THR 0x0 0x1 0x2 0x3 0x4 0x5 0x6 0x7 0x8 0x9 0xa 0xb 0xc 0xd 0xe 0xf
//! 0x00  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok
//! 0x10  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok
//! 0x20  ok  ok  ok  ok  ok MCE  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok
//! 0x30  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok
//! 0x40  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok
//! 0x50  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok
//! 0x60  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok
//! 0x70  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok  ok
//!
//! === MCE on thread 0x25 (37), bank 0 ===
//!
//!     MCA_CTL(0)     0x0000000000ffffff
//!     MCA_STATUS(0)  0xb420000001130839
//!                    |
//!                    +---> Valid (VAL)                         = true
//!                          Overflow (OVER)                     = false
//!                          Uncorrected error (UC)              = true
//!                          Error condition enabled (EN)        = true
//!                          Misc error register valid (MISCV)   = false
//!                          Address register valid (ADDRV)      = true
//!                          Processor context corrupt (PCC)     = false
//!                          Task content corrupt (TCC)          = false
//!                          Syndrome register valid (SYNDV)     = true
//!                          Deferred error (Deferred)           = false
//!                          Poisoned data consumed (Poison)     = false
//!                          Extended Error Code                 = 0x113
//!                          MCA Error Code                      = 0x839
//!
//!     MCA_ADDR(0)    0x000ffffe14700008
//!     MCA_MISC(0)    0xd010000000000000
//!     MCA_CONFIG(0)  0x00000007000001fd
//!                    |
//!                    +---> Interrupt Enable (IntEn)            = false
//!                          Deferred error type                 = 0x0
//!                          MCAX enable (McaxEn)                = true
//!                          MCA FRU text supported              = false
//!                          Address LSB in MCA_STATUS           = true
//!                          Deferred error status supported     = true
//!                          System fatal error event supported  = true
//!                          Deferred interrupt type supported   = true
//!                          Deferred error logging supported    = true
//!                          MCAX capable                        = true
//!
//!     MCA_SYND(0)    0x000000005c000002
//!     MCA_IPID(0)    0x001000b000000000
//!                    |
//!                    +---> MCA bank type                       = 0x10
//!                          Instance ID (high)                  = 0x0
//!                          Hardware ID                         = 0xb0
//!                          Instance ID (low)                   = 0x0
//!
//!     MCA_DESTAT(0)  0x0000000000000000
//!     MCA_DEADDR(0)  0x0000000000000000
//! ```
//!
//! CPU identification information as provided by the `cpuid` instruction
//! can be retrieved by using the `--cpuid` option and specifying a
//! desired target thread; full MCA information can similarly be retrieved
//! using the `--mca` option and specifyin a desired thread.

use anyhow::{anyhow, Result};
use clap::{ArgGroup, CommandFactory, Parser};
use colored::Colorize;
use hif::*;
use humility::core::Core;
use humility::hubris::*;
use humility::reflect;
use humility_cli::{ExecutionContext, Subcommand};
use humility_cmd::CommandKind;
use humility_cmd::{Archive, Attach, Command, Validate};
use humility_hiffy::*;
use humility_idol::{self as idol, HubrisIdol};
use raw_cpuid::{CpuId, CpuIdResult};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

#[derive(Parser, Debug)]
#[clap(
    name = "sbrmi", about = env!("CARGO_PKG_DESCRIPTION"),
    group = ArgGroup::new("command").multiple(false).required(false)
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
        requires = "command",
    )]
    thread: Option<u8>,

    /// run/interpret CPUID on target thread
    #[clap(long, short, group = "command")]
    cpuid: bool,

    /// display MCA registers for target thread
    #[clap(long, short, group = "command", requires = "thread")]
    mca: bool,
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
    let mut ops = vec![];

    let payload = op.payload(&[
        ("thread", idol::IdolArgument::Scalar(thread as u64)),
        ("eax", idol::IdolArgument::Scalar(eax as u64)),
        ("ecx", idol::IdolArgument::Scalar(ecx as u64)),
    ])?;

    context.idol_call_ops(&op, &payload, &mut ops)?;
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
    thread: Option<u8>,
) -> Result<()> {
    let thread = thread.unwrap_or(0);

    struct SbrmiWorkspace<'a, 'b> {
        hubris: &'a HubrisArchive,
        core: &'a mut dyn Core,
        context: &'a mut HiffyContext<'b>,
        thread: u8,
    }
    impl<'a, 'b> SbrmiWorkspace<'a, 'b> {
        fn cpuid2(&mut self, eax: u32, ecx: u32) -> raw_cpuid::CpuIdResult {
            match call_cpuid(
                self.hubris,
                self.core,
                self.context,
                self.thread,
                eax,
                ecx,
            ) {
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
        }
    }
    #[derive(Clone)]
    struct WorkspaceRef<'a, 'b> {
        cell: Rc<RefCell<SbrmiWorkspace<'a, 'b>>>,
    }
    impl<'a, 'b> raw_cpuid::CpuIdReader for WorkspaceRef<'a, 'b> {
        fn cpuid2(&self, eax: u32, ecx: u32) -> raw_cpuid::CpuIdResult {
            let mut r = self.cell.borrow_mut();
            r.cpuid2(eax, ecx)
        }
    }
    let workspace = WorkspaceRef {
        cell: Rc::new(RefCell::new(SbrmiWorkspace {
            hubris,
            core,
            context,
            thread,
        })),
    };

    let cpuid = CpuId::with_cpuid_fn(workspace);
    raw_cpuid::display::markdown(cpuid);

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
#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
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
    #[allow(clippy::self_named_constructors)]
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
            Msr::MCA_IPID(_) => Some(vec![
                Field::field(63, 48, "MCA bank type", None),
                Field::field(47, 44, "Instance ID (high)", None),
                Field::field(43, 32, "Hardware ID", None),
                Field::field(31, 0, "Instance ID (low)", None),
            ]),
            _ => None,
        }
    }
}

fn mca(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
    thread: u8,
    mcg_cap: u64,
    all_mca: bool,
) -> Result<()> {
    //
    // This is all a little dirty.
    //
    let nbanks = (mcg_cap & 0xff) as u8;
    let op = hubris.get_idol_command("Sbrmi.rdmsr64")?;
    let mut ops = vec![];
    let thread_name = format!("thread 0x{thread:x} ({thread})");

    for bank in 0..nbanks {
        let payload = op.payload(&[
            ("thread", idol::IdolArgument::Scalar(thread as u64)),
            ("msr", idol::IdolArgument::Scalar(Msr::MCA_STATUS(bank).into())),
        ])?;

        context.idol_call_ops(&op, &payload, &mut ops)?;
    }

    for bank in 0..nbanks {
        let payload = op.payload(&[
            ("thread", idol::IdolArgument::Scalar(thread as u64)),
            ("msr", idol::IdolArgument::Scalar(Msr::MCA_IPID(bank).into())),
        ])?;

        context.idol_call_ops(&op, &payload, &mut ops)?;
    }

    ops.push(Op::Done);

    let results = context.run(core, ops.as_slice(), None)?;

    if all_mca {
        println!("MCA registers for {thread_name}:\n");
    }

    let ipid_results = &results[nbanks as usize..];

    for (bank, r) in results[..nbanks as usize].iter().enumerate() {
        let status = context.idol_result(&op, r)?.as_base()?.as_u64().unwrap();
        let ipid = context
            .idol_result(&op, &ipid_results[bank])?
            .as_base()?
            .as_u64()
            .unwrap();

        if !all_mca {
            if status == 0 {
                continue;
            } else {
                println!("=== MCE on {thread_name}, bank {bank} ===\n");
            }
        }

        if ipid == 0 {
            continue;
        }

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

            context.idol_call_ops(&op, &payload, &mut ops)?;
        }

        ops.push(Op::Done);

        let reg_results = context.run(core, ops.as_slice(), None)?;
        let mut values = HashMap::new();

        for (ndx, reg) in reg_results.iter().enumerate() {
            let v = context.idol_result(&op, reg)?.as_base()?.as_u64().unwrap();
            values.insert(allregs[ndx], v);
        }

        if *values.get(&Msr::MCA_IPID(bank)).unwrap() == 0 {
            continue;
        }

        for reg in &allregs {
            let v = *(values.get(reg).unwrap());

            let name = format!("{:?}", reg);
            println!("    {name:14} 0x{v:016x}");

            if let Msr::MCA_STATUS(_) = reg {
                if v == 0 {
                    continue;
                }
            }

            if let Some(fields) = reg.fields() {
                let blank = "";
                print!("{blank:19}|\n{blank:19}+---> ");

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
                        format!("{:#x}", (v >> field.lowbit) & mask)
                    };

                    let name = if let Some(mneumonic) = field.mneumonic {
                        format!("{} ({mneumonic})", field.name)
                    } else {
                        field.name.to_string()
                    };

                    if i == 0 {
                        println!("{name:35} = {value}");
                    } else {
                        println!("{blank:25}{name:35} = {value}");
                    }
                }

                println!();
            }
        }

        println!();
    }

    Ok(())
}

fn sbrmi(context: &mut ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let subargs = SbrmiArgs::try_parse_from(subargs)?;
    let hubris = context.archive.as_ref().unwrap();
    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;

    if subargs.cpuid {
        return cpuid(hubris, core, &mut context, subargs.thread);
    }

    let mut ops = vec![];

    let nthreads = hubris.get_idol_command("Sbrmi.nthreads")?;
    context.idol_call_ops(&nthreads, &[], &mut ops)?;

    let enabled = hubris.get_idol_command("Sbrmi.enabled")?;
    context.idol_call_ops(&enabled, &[], &mut ops)?;

    let alert = hubris.get_idol_command("Sbrmi.alert")?;
    context.idol_call_ops(&alert, &[], &mut ops)?;

    let mcg_cap = hubris.get_idol_command("Sbrmi.rdmsr64")?;

    let payload = mcg_cap.payload(&[
        ("thread", idol::IdolArgument::Scalar(0_u64)),
        ("msr", idol::IdolArgument::Scalar(Msr::MCG_CAP.into())),
    ])?;

    context.idol_call_ops(&mcg_cap, &payload, &mut ops)?;

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

    let mcg_cap = context
        .idol_result(&mcg_cap, &results[3])?
        .as_base()?
        .as_u64()
        .unwrap();

    if subargs.mca {
        let thread = subargs.thread.unwrap();
        mca(hubris, core, &mut context, thread, mcg_cap, true)?;
        return Ok(());
    }

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

    for thread in alert {
        mca(hubris, core, &mut context, thread, mcg_cap, false)?;
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
