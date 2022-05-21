// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::Result;
use std::fmt::Write;
use std::fs::{self, File};
use std::path::Path;

struct Test {
    name: &'static str,
    cmd: &'static str,
    arg: Option<&'static str>,
}

impl Test {
    fn basic(name: &'static str) -> Self {
        Test { name, cmd: name, arg: None }
    }

    fn witharg(
        name: &'static str,
        cmd: &'static str,
        arg: &'static str,
    ) -> Self {
        Test { name, cmd, arg: Some(arg) }
    }
}

fn make_tests() -> Result<()> {
    let postmortem = [
        Test::witharg("extract", "extract", "app.toml"),
        Test::witharg("extract-list", "extract", "--list"),
        Test::basic("manifest"),
        Test::basic("spd"),
        Test::basic("map"),
        Test::basic("registers"),
        Test::witharg("registers-s", "registers", "-s"),
        Test::basic("ringbuf"),
        Test::witharg("ringbuf-arg", "ringbuf", "i2c"),
        Test::witharg("readvar-list", "readvar", "-l"),
        Test::witharg("readvar-ticks", "readvar", "TICKS"),
        Test::basic("stackmargin"),
        Test::basic("tasks"),
        Test::witharg("tasks-slvr", "tasks", "-slvr"),
    ];

    let mut cores = vec![];

    let dir = Path::new("./tests/cmd/cores");

    for entry in fs::read_dir(dir)? {
        let entry = entry?;

        let path = entry.path();

        if let Some(f) = path.file_name() {
            if let Some(s) = f.to_str() {
                let prefix = "hubris.core.";

                if let Some(name) = s.strip_prefix(&prefix) {
                    cores.push((name.to_string(), s.to_string()));
                }
            }
        }
    }

    for test in &postmortem {
        let dirpath = format!("./tests/cmd/{}", test.name);
        let dir = Path::new(&dirpath);

        fs::create_dir_all(dir)?;

        for (core, filename) in &cores {
            let base = format!("{}.{}", test.name, core);
            let toml = format!("{}.toml", base);
            let stdout = format!("{}.stdout", base);
            let stderr = format!("{}.stderr", base);
            let succeeds = format!("{}.stdout.succeeds", base);
            let fails = format!("{}.stderr.fails", base);

            let testcase = dir.join(toml);
            let succeeds = dir.join(succeeds);
            let fails = dir.join(fails);

            if !testcase.exists() || succeeds.exists() || fails.exists() {
                let mut testfile = File::create(testcase)?;
                let mut contents = String::new();

                let testcmd = if let Some(arg) = test.arg {
                    format!("{} {}", test.cmd, arg)
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
fs.base = "../cores"
bin.name = "humility"
args = "-d {} {}"
"##,
                    filename, testcmd,
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
                    core.ends_with(".fails")
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

#[test]
fn cli_tests() {
    if let Err(err) = make_tests() {
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
