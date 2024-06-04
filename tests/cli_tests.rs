// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::{Context, Result};
use std::fmt::Write;
use std::fs::{self, File};
use std::path::Path;

#[derive(PartialEq)]
enum Kind {
    Postmortem,
    Archive,
    All,
}

impl Kind {
    fn path(&self) -> &'static str {
        match self {
            Kind::Postmortem => "cores",
            Kind::Archive => "archives",
            Kind::All => panic!(),
        }
    }

    fn prefix(&self) -> &'static str {
        match self {
            Kind::Postmortem => "hubris.core.",
            Kind::Archive => "build-",
            Kind::All => panic!(),
        }
    }

    fn options(&self) -> &'static str {
        match self {
            Kind::Postmortem => "-d",
            Kind::Archive => "-p archive -a",
            Kind::All => panic!(),
        }
    }
}

struct Test {
    kind: Kind,
    name: &'static str,
    cmd: &'static str,
    arg: Option<&'static str>,
}

impl Test {
    fn basic(kind: Kind, name: &'static str) -> Self {
        Test { kind, name, cmd: name, arg: None }
    }

    fn witharg(
        kind: Kind,
        name: &'static str,
        cmd: &'static str,
        arg: &'static str,
    ) -> Self {
        Test { kind, name, cmd, arg: Some(arg) }
    }
}

fn make_tests(tests: &[Test], kind: Kind) -> Result<()> {
    let path = format!("./tests/cmd/{}", kind.path());
    let dir = Path::new(&path);
    let mut input = vec![];

    for entry in
        fs::read_dir(dir).with_context(|| format!("couldn't read {path}"))?
    {
        let entry = entry?;

        let path = entry.path();

        if let Some(f) = path.file_name() {
            if let Some(s) = f.to_str() {
                if let Some(name) = s.strip_prefix(&kind.prefix()) {
                    input.push((name.to_string(), s.to_string()));
                }
            }
        }
    }

    for test in tests {
        let dirpath = format!("./tests/cmd/{}", test.name);
        let dir = Path::new(&dirpath);

        fs::create_dir_all(dir)?;

        if test.kind != Kind::All && test.kind != kind {
            continue;
        }

        for (input, filename) in &input {
            let base = format!("{}.{input}", test.name);
            let toml = format!("{base}.toml");
            let stdout = format!("{base}.stdout");
            let stderr = format!("{base}.stderr");
            let succeeds = format!("{base}.stdout.succeeds");
            let fails = format!("{base}.stderr.fails");

            let testcase = dir.join(toml);
            let succeeds = dir.join(succeeds);
            let fails = dir.join(fails);

            if !testcase.exists() || succeeds.exists() || fails.exists() {
                let mut testfile = File::create(testcase)?;
                let mut contents = String::new();

                let testcmd = if let Some(arg) = test.arg {
                    format!("{} {arg}", test.cmd)
                } else {
                    format!("{}", test.cmd)
                };

                writeln!(
                    &mut contents,
                    r##"
#
# This test case has been automatically created, but can be edited and
# should be checked in.  Should it ever be regenerated, simply delete
# it and re-run "cargo test"
#
fs.base = "../{}"
bin.name = "humility"
args = "{} {filename} {testcmd}"
"##,
                    kind.path(),
                    kind.options(),
                )?;

                let stderr = dir.join(stderr);
                let stdout = dir.join(stdout);

                let fails = if succeeds.exists() {
                    fs::remove_file(succeeds)?;
                    false
                } else if fails.exists() {
                    fs::remove_file(fails)?;
                    true
                } else {
                    input.ends_with(".fails")
                };

                if fails {
                    writeln!(&mut contents, "status.code = 1")?;
                }

                File::create(stderr)?;
                File::create(stdout)?;

                use std::io::Write;
                testfile.write_all(contents.as_bytes())?;
            }
        }
    }

    Ok(())
}

fn make_all_tests() -> Result<()> {
    let tests = [
        Test::witharg(Kind::All, "extract", "extract", "app.toml"),
        Test::witharg(Kind::All, "extract-list", "extract", "--list"),
        Test::witharg(Kind::All, "hiffy-list", "hiffy", "--list"),
        Test::basic(Kind::All, "manifest"),
        Test::basic(Kind::Postmortem, "spd"),
        Test::basic(Kind::All, "map"),
        Test::basic(Kind::Postmortem, "registers"),
        Test::witharg(Kind::Postmortem, "registers-s", "registers", "-s"),
        Test::basic(Kind::Postmortem, "ringbuf"),
        Test::witharg(Kind::Postmortem, "ringbuf-arg", "ringbuf", "i2c"),
        Test::witharg(
            Kind::Postmortem,
            "ringbuf-full-totals",
            "ringbuf",
            "--full-totals",
        ),
        Test::witharg(Kind::All, "readvar-list", "readvar", "-l"),
        Test::witharg(Kind::Postmortem, "readvar-ticks", "readvar", "TICKS"),
        Test::witharg(
            Kind::Archive,
            "readvar-tasks",
            "readvar",
            "HUBRIS_TASK_DESCS",
        ),
        Test::witharg(Kind::All, "sensors", "sensors", "--list"),
        Test::witharg(Kind::Postmortem, "sensors-read", "sensors", ""),
        Test::basic(Kind::Postmortem, "stackmargin"),
        Test::basic(Kind::Postmortem, "tasks"),
        Test::witharg(Kind::Postmortem, "tasks-slvr", "tasks", "-slvr"),
        Test::basic(Kind::Postmortem, "counters"),
        Test::witharg(Kind::Postmortem, "counters-list", "counters", "list"),
        Test::witharg(Kind::Postmortem, "counters-full", "counters", "--full"),
        Test::witharg(Kind::Postmortem, "counters-ipc", "counters", "ipc"),
        Test::witharg(
            Kind::Postmortem,
            "counters-ipc-full",
            "counters",
            "ipc --full",
        ),
        Test::witharg(
            Kind::Postmortem,
            "counters-ipc-filtered",
            "counters",
            "ipc --client gimlet_seq --client net",
        ),
        Test::witharg(
            Kind::Postmortem,
            "counters-arg",
            "counters",
            "gimlet_seq",
        ),
        Test::witharg(
            Kind::Postmortem,
            "counters-csv",
            "counters",
            "--output csv",
        ),
        Test::witharg(
            Kind::Postmortem,
            "counters-csv-full",
            "counters",
            "--output csv --full",
        ),
        Test::witharg(
            Kind::Postmortem,
            "counters-json",
            "counters",
            "--output json",
        ),
    ];

    make_tests(&tests, Kind::Postmortem)?;
    make_tests(&tests, Kind::Archive)?;

    Ok(())
}

#[test]
fn cli_tests() {
    if let Err(err) = make_all_tests() {
        panic!("make_tests() failed: {:?}", err);
    }

    match std::env::var_os("TRYCMD_TEST") {
        Some(case) => {
            trycmd::TestCases::new().case(case);
        }
        None => {
            trycmd::TestCases::new()
                .case("tests/cmd/*.trycmd")
                .case("tests/cmd/*.toml")
                .case("tests/cmd/*/*.toml");
        }
    }
}
