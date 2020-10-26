/*
 * Copyright 2020 Oxide Computer Company
 */

use anyhow::{bail, Result};

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum TestSource {
    KernelLog,
    UserLog,
    Suite,
}

#[derive(Clone, Debug, PartialEq)]
enum TestToken {
    Meta,
    Expect,
    Case,
    Run,
    Start,
    Finish,
    Done,
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
struct TestCompletion {
    case: String,
    result: TestResult,
    log: Vec<(TestSource, String)>,
}

pub struct TestRun {
    log: Vec<(char, TestSource)>,
    buffer: Vec<char>,
    case: usize,
    cases: Vec<String>,
    expected: TestToken,
    ncases: Option<usize>,
    results: Vec<TestCompletion>,
}

#[rustfmt::skip::macros(bail)]
impl TestRun {
    pub fn new() -> TestRun {
        Self {
            log: Vec::new(),
            buffer: Vec::new(),
            expected: TestToken::Meta,
            case: 0,
            cases: Vec::new(),
            ncases: None,
            results: Vec::new(),
        }
    }

    fn parse(&mut self) -> Result<()> {
        let s: String = self.buffer.iter().collect();
        let tokens: Vec<&str> = s.split(' ').collect();

        if tokens.len() == 0 {
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
                println!("{:#?}", self.cases);
                TestToken::Start
            }

            TestToken::Start => {
                if tokens[1] != self.cases[self.case] {
                    bail!("starting case {}: expected case {}, found case {}",
                        self.case, self.cases[self.case], tokens[1]);
                }

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

                self.results.push(TestCompletion {
                    case: self.cases[self.case].clone(),
                    result: TestResult::from(tokens[1]),
                    log: log,
                });

                self.log.truncate(0);
                self.case += 1;

                if self.case < self.ncases.unwrap() {
                    TestToken::Start
                } else {
                    TestToken::Done
                }
            }

            TestToken::Done => {
                println!("{:#?}", self.results);
                std::process::exit(0);
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
}
