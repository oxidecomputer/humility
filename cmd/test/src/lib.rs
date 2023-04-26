// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility test`
//!
//! When run against a test archive, `humility test` kicks off the test suite
//! and parses its results via ITM.
//!
//! ```console
//! humility: attached via ST-Link
//! humility: ITM synchronization packet found at offset 6
//! humility: expecting 22 cases
//! humility: running test_send ... ok
//! humility: running test_recv_reply ... ok
//! humility: running test_fault_badmem ... ok
//! humility: running test_fault_stackoverflow ... ok
//! humility: running test_fault_execdata ... ok
//! humility: running test_fault_illop ... ok
//! humility: running test_fault_nullexec ... ok
//! humility: running test_fault_textoob ... ok
//! humility: running test_fault_stackoob ... ok
//! humility: running test_fault_buserror ... ok
//! humility: running test_fault_illinst ... ok
//! humility: running test_fault_divzero ... ok
//! humility: running test_panic ... ok
//! humility: running test_restart ... ok
//! humility: running test_restart_taskgen ... ok
//! humility: running test_borrow_info ... ok
//! humility: running test_borrow_read ... ok
//! humility: running test_borrow_write ... ok
//! humility: running test_supervisor_fault_notification ... ok
//! humility: running test_timer_advance ... ok
//! humility: running test_timer_notify ... ok
//! humility: running test_timer_notify_past ... ok
//! humility: tests completed: pass
//! ```
//!
//! If a test fails, this will also create a complete report, e.g.:
//!
//! ```console
//! humility: attached via ST-Link
//! humility: ITM synchronization packet found at offset 6
//! humility: expecting 22 cases
//! humility: running test_send ... ok
//! humility: running test_recv_reply ... fail
//! humility: running test_fault_badmem ... ok
//! ...
//! humility: running test_timer_notify_past ... ok
//! humility: tests completed: fail
//! humility: test output dumped to hubris.testout.15
//! ```
//!
//! This output file will have (among other things) a section that has
//! complete test run information.  For details on a failing test, look
//! for `result: Fail`:
//!
//! ```console
//! $ cat hubris.testout.15
//! ...
//! ==== Test results
//! [
//!     ...
//!     TestCompletion {
//!         case: "test_recv_reply",
//!         result: Fail,
//!         log: [
//!             (
//!                 UserLog,
//!                 "assistant starting",
//!             ),
//!             (
//!                 KernelLog,
//!                 "task @1 panicked: panicked at \'assertion failed: false\', test/test-suite/src/main.rs:124:5",
//!             ),
//!             (
//!                 UserLog,
//!                 "Task #1 Panic!",
//!             ),
//!             (
//!                 UserLog,
//!                 "assistant starting",
//!             ),
//!         ],
//!     },
//!     ...
//! ```
//!
//! This shows the sequential ordering of all log messages while running the
//! test.  The test report can also be useful even when tests pass; to always
//! dump a test report, use the `-d` option to `humility test`.
//!
//! Note that `humility test` relies on the ability to keep up with ITM data,
//! which can be lossy.  In the event ITM data is lost, the failure mode is
//! unlikely to be a failing test, but rather a fatal error due to a misframed
//! packet:
//!
//! ```console
//! humility: running test_fault_nullexec ... ok
//! humility: running test_fault_textoob ... ok
//! humility: running test_fault_stackoob ... humility: test output dumped to hubris.testout.21
//! humility: test failed: malformed datum: 0x74
//! Error: test failed
//! ```
//!
//! All received packet data will be dumped to the resulting output file,
//! allowing these transient failures to be differentiated from deeper issues.
//!

use anyhow::{bail, Context, Result};
use clap::{CommandFactory, Parser};
use colored::Colorize;
use humility::core::Core;
use humility::hubris::*;
use humility_cli::{ExecutionContext, Subcommand};
use humility_cmd::CommandKind;
use humility_cmd::{Archive, Attach, Command, Validate};
use humility_cortex::itm::*;
use std::cell::RefCell;
use std::collections::VecDeque;
use std::fmt;
use std::fs;
use std::fs::OpenOptions;
use std::io::BufWriter;
use std::io::Write;
use std::time::Instant;

#[derive(Parser, Debug)]
#[clap(name = "test", about = env!("CARGO_PKG_DESCRIPTION"))]
struct TestArgs {
    /// dump full report even on success
    #[clap(long, short)]
    dumpalways: bool,
    /// sets the output file
    #[clap(long, short, value_name = "filename")]
    output: Option<String>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TestSource {
    KernelLog,
    UserLog,
    Suite,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum TestToken {
    Meta,
    Expect,
    Case,
    Run,
    Start,
    Finish,
    Done,
    None,
    Unknown(String),
}

impl From<&str> for TestToken {
    fn from(input: &str) -> Self {
        match input {
            "meta" => TestToken::Meta,
            "expect" => TestToken::Expect,
            "case" => TestToken::Case,
            "run" => TestToken::Run,
            "start" => TestToken::Start,
            "finish" => TestToken::Finish,
            "done" => TestToken::Done,
            _ => TestToken::Unknown(input.to_owned()),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
enum TestResult {
    Ok,
    Fail,
    Unknown(String),
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
            TestResult::Unknown(ref _str) => "unknown".bold(),
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
enum TestRunResult {
    Pass,
    Fail,
    Unknown(String),
}

impl From<&str> for TestRunResult {
    fn from(input: &str) -> Self {
        match input {
            "pass" => TestRunResult::Pass,
            "FAIL" => TestRunResult::Fail,
            _ => TestRunResult::Unknown(input.to_owned()),
        }
    }
}

#[rustfmt::skip::macros(write)]
impl fmt::Display for TestRunResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            TestRunResult::Pass => "pass".green(),
            TestRunResult::Fail => "fail".red(),
            TestRunResult::Unknown(ref _str) => "unknown".bold(),
        })
    }
}

fn nargs(token: &TestToken) -> usize {
    match token {
        TestToken::Meta | TestToken::Run => 0,
        TestToken::Expect | TestToken::Case | TestToken::Start => 1,
        TestToken::Done => 1,
        TestToken::Finish => 2,
        _ => 0,
    }
}

#[derive(Debug)]
#[allow(dead_code)]
struct TestCompletion {
    case: String,
    result: TestResult,
    log: Vec<(TestSource, String)>,
}

pub struct TestRun<'a> {
    hubris: &'a HubrisArchive,
    log: Vec<(char, TestSource)>,
    raw: Vec<char>,
    buffer: Vec<char>,
    case: usize,
    cases: Vec<String>,
    expected: TestToken,
    ncases: Option<usize>,
    result: Option<TestRunResult>,
    results: Vec<TestCompletion>,
}

#[rustfmt::skip::macros(bail)]
impl<'a> TestRun<'a> {
    pub fn new(hubris: &'a HubrisArchive) -> TestRun<'a> {
        Self {
            hubris,
            log: Vec::new(),
            raw: Vec::new(),
            buffer: Vec::new(),
            expected: TestToken::Meta,
            case: 0,
            cases: Vec::new(),
            ncases: None,
            result: None,
            results: Vec::new(),
        }
    }

    fn parse(&mut self) -> Result<()> {
        let s: String = self.buffer.iter().collect();
        let tokens: Vec<&str> = s.split(' ').collect();

        if tokens.is_empty() {
            bail!("expected {:?} token, found blank line", self.expected);
        }

        let token = TestToken::from(tokens[0]);

        if token != self.expected {
            bail!("expected {:?} token, found {:?}", self.expected, token);
        }

        let nargs = nargs(&token);

        if tokens.len() - 1 != nargs {
            bail!("for {:?}, expected {} args, found {}",
                token, nargs, tokens.len() - 1);
        }

        self.expected = match token {
            TestToken::Meta => TestToken::Expect,

            TestToken::Expect => {
                self.ncases = match tokens[1].parse::<usize>() {
                    Ok(val) => Some(val),
                    Err(e) => bail!("invalid number of cases: {:?}", e),
                };
                TestToken::Case
            }

            TestToken::Case => {
                self.cases.push(tokens[1].to_string());

                if self.cases.len() == self.ncases.unwrap() {
                    TestToken::Run
                } else {
                    TestToken::Case
                }
            }

            TestToken::Run => {
                println!("humility: expecting {} cases", self.cases.len());
                TestToken::Start
            }

            TestToken::Start => {
                if tokens[1] != self.cases[self.case] {
                    bail!("starting case {}: expected case {}, found case {}",
                        self.case, self.cases[self.case], tokens[1]);
                }

                print!("humility: running {} ... ", self.cases[self.case]);
                std::io::stdout().flush().unwrap();

                TestToken::Finish
            }

            TestToken::Finish => {
                if tokens[2] != self.cases[self.case] {
                    bail!("finishing case {}: expected case {}, found case {}",
                        self.case, self.cases[self.case], tokens[2]);
                }

                let mut log = vec![];
                let mut last = None;
                let mut buf = vec![];

                for (datum, source) in &self.log {
                    if let Some(l) = last {
                        if l != *source || *datum == '\n' {
                            let s: String = buf.iter().collect();
                            log.push((l, s));
                            buf.truncate(0);
                        }
                    }

                    if *datum == '\n' {
                        last = None;
                        continue;
                    }

                    buf.push(*datum);
                    last = Some(*source);
                }

                if let Some(l) = last {
                    let s: String = buf.iter().collect();
                    log.push((l, s));
                }

                let completion = TestCompletion {
                    case: self.cases[self.case].clone(),
                    result: TestResult::from(tokens[1]),
                    log,
                };

                println!("{}", completion.result);
                self.results.push(completion);

                self.log.truncate(0);
                self.case += 1;

                if self.case < self.ncases.unwrap() {
                    TestToken::Start
                } else {
                    TestToken::Done
                }
            }

            TestToken::Done => {
                let result = TestRunResult::from(tokens[1]);
                humility::msg!("tests completed: {result}");
                self.result = Some(result);
                TestToken::None
            }

            _ => {
                println!("{:#?}", self.results);
                bail!("unhandled token {:?}", token);
            }
        };

        self.buffer.truncate(0);

        Ok(())
    }

    pub fn consume(&mut self, source: TestSource, datum: char) -> Result<()> {
        match source {
            TestSource::Suite => {
                self.raw.push(datum);

                if datum == '\n' {
                    self.parse()?;
                } else {
                    self.buffer.push(datum);
                }
            }

            _ => {
                self.log.push((datum, source));
            }
        }

        Ok(())
    }

    pub fn report(
        &mut self,
        output: Option<&String>,
        wire: &[(u8, f64, f64)],
        err: Option<&anyhow::Error>,
    ) -> Result<()> {
        let filename = match output {
            Some(filename) => filename.clone(),
            None => {
                let mut filename;
                let mut i = 0;

                loop {
                    filename = format!("hubris.testout.{}", i);

                    if let Ok(_f) = fs::File::open(&filename) {
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
        writeln!(out, "{:#?}", self.hubris.manifest)?;

        writeln!(out, "==== Test result")?;

        match &self.result {
            None => match err {
                Some(err) => {
                    writeln!(out, "result=aborted due to error: {:?}", err)?;
                }
                None => {
                    writeln!(out, "result=incomplete")?;
                }
            },
            Some(result) => {
                writeln!(out, "result={:?}", result)?;
            }
        }

        writeln!(out, "==== Raw SWO output")?;
        for (i, w) in wire.iter().enumerate() {
            writeln!(out, "swo,{},{},{},0x{:02x},,", i, w.1, w.2, w.0)?;
        }

        writeln!(out, "==== Test output")?;
        writeln!(out, "{}", self.raw.iter().collect::<String>())?;
        writeln!(out, "==== Test results")?;
        writeln!(out, "{:#?}", self.results)?;

        humility::msg!("test output dumped to {filename}");

        Ok(())
    }

    pub fn completed(&mut self) -> bool {
        self.result.is_some()
    }

    pub fn failed(&mut self) -> bool {
        self.result == Some(TestRunResult::Fail)
    }
}

fn test_ingest(
    core: &mut dyn Core,
    subargs: &TestArgs,
    hubris: &HubrisArchive,
    traceid: Option<u8>,
) -> Result<()> {
    let mut bufs: VecDeque<(Vec<u8>, f64)> = VecDeque::new();
    let mut ndx = 0;
    let mut current = None;

    let v = hubris
        .lookup_variable("TEST_KICK")
        .context("does not appear to be a test archive")?;

    if v.size != 4 {
        bail!("expected TEST_KICK to be of size 4; found {}", v.size);
    }

    let start = Instant::now();

    let mut testrun = TestRun::new(hubris);
    let mut kicked = false;

    let shared = RefCell::new(core);

    let wirebuf = vec![];
    let wire = RefCell::new(wirebuf);

    let output = subargs.output.as_ref();
    let timeout = 30;

    let rval = itm_ingest(
        traceid,
        || {
            loop {
                if start.elapsed().as_secs() > timeout {
                    bail!("timed out after {} seconds", timeout);
                }

                //
                // We will keep reading until we have a zero byte read, at
                // which time we will kick out and process one byte.
                //
                let buf = shared.borrow_mut().read_swv()?;

                if !buf.is_empty() {
                    bufs.push_back((buf, start.elapsed().as_secs_f64()));
                    continue;
                }

                match current {
                    None => {
                        current = bufs.pop_front();
                        ndx = 0;
                    }

                    Some((ref buf, _)) => {
                        if ndx == buf.len() {
                            current = bufs.pop_front();
                            ndx = 0;
                        }
                    }
                }

                if current.is_none() {
                    continue;
                }

                break;
            }

            let (buf, pulled) = current.as_ref().unwrap();
            ndx += 1;

            let datum = (buf[ndx - 1], start.elapsed().as_secs_f64());
            wire.borrow_mut().push((datum.0, *pulled, datum.1));

            Ok(Some(datum))
        },
        |packet| match &packet.payload {
            ITMPayload::Instrumentation { payload, port } => {
                let source = match *port {
                    0 => TestSource::KernelLog,
                    1 => TestSource::UserLog,
                    8 => TestSource::Suite,
                    _ => {
                        bail!("spurious data on port {}: {:x?}", port, payload);
                    }
                };

                for p in payload {
                    match testrun.consume(source, *p as char) {
                        Ok(_) => {}
                        Err(err) => {
                            testrun.report(
                                output,
                                &wire.borrow(),
                                Some(&err),
                            )?;
                            return Err(err);
                        }
                    }

                    if testrun.completed() {
                        if testrun.failed() {
                            testrun.report(output, &wire.borrow(), None)?;
                            std::process::exit(1);
                        }

                        if subargs.dumpalways {
                            testrun.report(output, &wire.borrow(), None)?;
                        }

                        std::process::exit(0);
                    }
                }

                Ok(())
            }
            ITMPayload::None => {
                match packet.header {
                    ITMHeader::Sync => {
                        if !kicked {
                            shared.borrow_mut().halt()?;
                            shared.borrow_mut().write_word_32(v.addr, 1)?;
                            shared.borrow_mut().run()?;
                            kicked = true;
                        }
                    }
                    ITMHeader::Malformed(datum) => {
                        bail!("malformed datum: 0x{:x}", datum);
                    }
                    _ => {}
                }

                Ok(())
            }
            _ => {
                bail!("unknown packet: {:x?}", packet);
            }
        },
    );

    match rval {
        Ok(_) => rval,
        Err(err) => {
            testrun.report(output, &wire.borrow(), Some(&err))?;
            Err(err)
        }
    }
}

fn test(context: &mut ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let hubris = context.archive.as_ref().unwrap();

    let subargs = TestArgs::try_parse_from(subargs)?;

    hubris.validate(core, HubrisValidate::Booted)?;

    let stim = 0x0000_ffff;
    let traceid = itm_enable_ingest(core, hubris, stim)?;
    test_ingest(core, &subargs, hubris, traceid)?;

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
