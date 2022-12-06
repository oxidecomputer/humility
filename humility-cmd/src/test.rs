// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::{bail, Result};
use colored::Colorize;
use humility::hubris::*;
use std::fmt;
use std::fs;
use std::fs::OpenOptions;
use std::io::BufWriter;
use std::io::Write;

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
                humility::msg!("tests completed: {}", result);
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

        humility::msg!("test output dumped to {}", filename);

        Ok(())
    }

    pub fn completed(&mut self) -> bool {
        self.result.is_some()
    }

    pub fn failed(&mut self) -> bool {
        self.result == Some(TestRunResult::Fail)
    }
}
