// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility test`
//!
//! When run against a test archive, `humility test` kicks off the test suite.
//! Humility is responsible for getting the number of tests and then running
//! each one in succession.
//!
//! ```console
//! humility: attached via CMSIS-DAP
//! Total test cases: 50
//! humility: running test_send ...ok
//! humility: running test_recv_reply ...ok
//! humility: running test_recv_reply_fault ...ok
//! humility: running test_floating_point_lowregs ...ok
//! humility: running test_floating_point_highregs ...ok
//! humility: running test_floating_point_fault ...ok
//! humility: running test_fault_badmem ...ok
//! humility: running test_fault_stackoverflow ...ok
//! humility: running test_fault_execdata ...ok
//! humility: running test_fault_illop ...ok
//! humility: running test_fault_nullexec ...ok
//! humility: running test_fault_textoob ...ok
//! humility: running test_fault_stackoob ...ok
//! humility: running test_fault_buserror ...ok
//! humility: running test_fault_illinst ...ok
//! humility: running test_fault_divzero ...ok
//! humility: running test_fault_maxstatus ...ok
//! humility: running test_fault_badstatus ...ok
//! humility: running test_fault_maxrestart ...ok
//! humility: running test_fault_badrestart ...ok
//! humility: running test_fault_maxinjection ...ok
//! humility: running test_fault_badinjection ...ok
//! humility: running test_fault_superinjection ...ok
//! humility: running test_fault_selfinjection ...ok
//! humility: running test_panic ...ok
//! humility: running test_restart ...ok
//! humility: running test_restart_taskgen ...ok
//! humility: running test_borrow_info ...ok
//! humility: running test_borrow_read ...ok
//! humility: running test_borrow_write ...ok
//! humility: running test_borrow_without_peer_waiting ...ok
//! humility: running test_supervisor_fault_notification ...ok
//! humility: running test_timer_advance ...ok
//! humility: running test_timer_notify ...ok
//! humility: running test_timer_notify_past ...ok
//! humility: running test_task_config ...ok
//! humility: running test_task_status ...ok
//! humility: running test_task_fault_injection ...ok
//! humility: running test_refresh_task_id_basic ...ok
//! humility: running test_refresh_task_id_off_by_one ...ok
//! humility: running test_refresh_task_id_off_by_many ...ok
//! humility: running test_post ...ok
//! humility: running test_idol_basic ...ok
//! humility: running test_idol_bool_arg ...ok
//! humility: running test_idol_bool_ret ...ok
//! humility: running test_idol_bool_xor ...ok
//! humility: running test_idol_err_ret ...ok
//! humility: running test_idol_ssmarshal ...ok
//! humility: running test_idol_ssmarshal_multiarg ...ok
//! humility: running test_idol_ssmarshal_multiarg_enum ...ok
//! Ran a total of 50 cases
//! ```
//!
//! All tests will produce an output file. This contains information about
//! the hubris archive as well as task state after each test run. Search
//! for "fail" to see any failed tests.
//!
//! ```console
//! $ cat hubris.testout.15
//! ...
//! ==== Test results
//! ...
//! ==== Test test_task_status result: "ok"
//! ==== Task state
//! system time = 27941
//! ID TASK                       GEN PRI STATE
//! 0 runner                       0   0 recv, notif: bit0 bit1 bit2 bit3 bit4 bit5 bit6 bit7 bit8 bit9 bit10 bit11 bit12 bit13 bit14 bit15 bit16 bit17 bit18 bit19 bit20 bit21 bit22 bit23 bit24 bit25 bit26 bit27 bit28 bit29 bit30 bit31
//! 1 suite                       33   2 recv
//! 2 assist                      37   1 FAULT: in syscall: used bogus task index (was: ready)
//! 3 idol                         0   1 recv
//! 4 hiffy                        0   3 notif: bit31(T+8)
//! 5 idle                         0   4 RUNNING
//! ```
//!
//! Older versions of the test suite gave streaming output. This is no longer
//! available. If the information in the output is not enough to debug, the
//! recommendation is to extend the state that is captured.
//!

use anyhow::{Context, Result, bail};
use clap::{CommandFactory, Parser};
use colored::Colorize;
use hif::*;
use humility::hubris::*;
use humility_cli::{ExecutionContext, Subcommand};
use humility_cmd::CommandKind;
use humility_cmd::{Archive, Attach, Command, Validate};
use humility_hiffy::*;
use std::fmt;
use std::fs::OpenOptions;
use std::io::{BufWriter, Write};

#[derive(Parser, Debug)]
#[clap(name = "test", about = env!("CARGO_PKG_DESCRIPTION"))]
struct TestArgs {
    /// sets timeout
    #[clap(
        long, short = 'T', default_value_t = 3000, value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// dump full report even on success
    #[clap(long, short)]
    dumpalways: bool,
    /// sets the output file
    #[clap(long, short, value_name = "filename")]
    output: Option<String>,
    /// Run a single test
    #[clap(long, short, value_name = "single")]
    single: Option<String>,
}

#[derive(Clone, Debug, PartialEq)]
enum TestResult {
    Ok,
    Fail,
    Unknown(String),
}

impl TestResult {
    fn to_str_no_color(&self) -> String {
        match self {
            TestResult::Ok => "ok".to_string(),
            TestResult::Fail => "fail".to_string(),
            TestResult::Unknown(s) => format!("unknown: {}", s),
        }
    }
}

impl From<&str> for TestResult {
    fn from(input: &str) -> Self {
        match input {
            "ok" => TestResult::Ok,
            "FAIL" => TestResult::Fail,
            _ => TestResult::Unknown(input.to_owned()),
        }
    }
}

#[rustfmt::skip::macros(write)]
impl fmt::Display for TestResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            TestResult::Ok => "ok".green(),
            TestResult::Fail => "fail".red(),
            TestResult::Unknown(..) => "unknown".bold(),
        })
    }
}

fn test(context: &mut ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let hubris = context.archive.as_ref().unwrap();

    let subargs = TestArgs::try_parse_from(subargs)?;

    hubris.validate(core, HubrisValidate::Booted)?;

    // This type is &[(&str, &(dyn Fn() + Send + Sync))]
    let test_slice = hubris
        .lookup_variable("TESTS")
        .context("This does not look to be a test archive")?;

    let filename = match subargs.output {
        Some(filename) => filename,
        None => {
            let mut filename;
            let mut i = 0;

            loop {
                filename = format!("hubris.testout.{}", i);

                if let Ok(_f) = std::fs::File::open(&filename) {
                    i += 1;
                    continue;
                }

                break;
            }

            filename
        }
    };

    let file =
        OpenOptions::new().write(true).create_new(true).open(&filename)?;
    let mut out = BufWriter::new(&file);

    writeln!(out, "==== Test archive details")?;
    writeln!(out, "{:#?}", hubris.manifest)?;

    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;

    let run_test = context.get_function("RunTest", 1)?;

    // Slices are (not so) secretly structs with members "length" and "data_ptr"
    let test_struct = hubris.lookup_struct(test_slice.goff)?;
    let len_data = test_struct.lookup_member("length")?;
    let data_ptr_data = test_struct.lookup_member("data_ptr")?;
    let test_len =
        core.read_word_32(test_slice.addr + (len_data.offset as u32))?;
    let test_array =
        core.read_word_32(test_slice.addr + (data_ptr_data.offset as u32))?;

    println!("Total test cases: {}", test_len);
    // This the (&str, &(dyn Fn() + Send + Sync)) which is also secretly a struct
    // We only care about the first entry (&str)
    let array_entry_ptr = hubris.lookup_ptrtype(data_ptr_data.goff)?;
    let array_entry = hubris.lookup_struct(array_entry_ptr)?;
    let test_name_str =
        hubris.lookup_struct(array_entry.lookup_member("__0")?.goff)?;

    let mut ran_cases = 0;
    for i in 0..test_len {
        let base = test_array + i * (array_entry.size as u32);
        let str_len = core.read_word_32(
            base + (test_name_str.lookup_member("length")?.offset as u32),
        )?;
        let mut bytes = vec![0u8; str_len as usize];
        let str_addr = core.read_word_32(
            base + (test_name_str.lookup_member("data_ptr")?.offset as u32),
        )?;
        core.read_8(str_addr, &mut bytes)?;

        let test_name =
            std::str::from_utf8(&bytes).unwrap_or("<test name unknown>");

        if let Some(ref expected) = subargs.single {
            if expected != test_name {
                println!("skipping {}", test_name);
                continue;
            }
        }
        print!("humility: running {} ...", test_name);
        ran_cases += 1;

        let ops =
            vec![Op::Push32(i), Op::Call(run_test.id), Op::Drop, Op::Done];

        let results = context.run(core, ops.as_slice(), None)?;
        if results.is_empty() {
            bail!("Bad return");
        }

        let result = match &results[0] {
            Ok(s) => {
                if s[0] == 1 {
                    TestResult::Ok
                } else {
                    TestResult::Fail
                }
            }
            Err(e) => TestResult::Unknown(format!("{}", e)),
        };

        println!("{:#}", result);
        writeln!(
            out,
            "==== Test {} result: {:?}",
            test_name,
            result.to_str_no_color()
        )?;
        writeln!(out, "==== Task state")?;

        cmd_tasks::print_tasks(
            &mut out, core, hubris, false, false, false, false, false, false,
            None,
        )?;
    }
    println!("Ran a total of {} cases", ran_cases);
    println!("Wrote test output to {}", filename);

    Ok(())
}

pub fn init() -> Command {
    Command {
        app: TestArgs::command(),
        name: "test",
        run: test,
        kind: CommandKind::Attached {
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
        },
    }
}
