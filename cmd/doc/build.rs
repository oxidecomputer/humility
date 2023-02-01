// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::{bail, Result};
use std::collections::BTreeMap;
use std::env;
use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::process::Command;

fn main() -> Result<()> {
    use cargo_metadata::MetadataCommand;
    let mut cmds = BTreeMap::new();

    // MetadataCommand doesn't emit this, so we should
    println!("cargo:rerun-if-changed=Cargo.toml",);

    let metadata =
        MetadataCommand::new().manifest_path("./Cargo.toml").exec().unwrap();

    let packages: Vec<_> = metadata
        .workspace_members
        .iter()
        .map(|id| {
            metadata.packages.iter().find(|p| &p.id == id).unwrap().clone()
        })
        .collect();

    for package in packages {
        if let Some(cmd) = package.name.strip_prefix("humility-cmd-") {
            let description = match package.description {
                Some(description) => description,
                None => {
                    bail!("{} is missing \"description\" in manifest", cmd);
                }
            };

            cmds.insert(cmd.to_string(), (description, package.manifest_path));
        }
    }

    let out_dir = env::var("OUT_DIR")?;
    let dest_path = Path::new(&out_dir).join("docs.rs");
    let mut output = File::create(&dest_path)?;

    write!(
        output,
        r##"
fn cmd_docs(lookup: &str) -> Option<&'static str> {{
    let mut m = HashMap::new();
"##
    )?;

    for (cmd, (_, path)) in &cmds {
        println!(
            "cargo:rerun-if-changed={}",
            path.parent().unwrap().join("src/lib.rs").display()
        );

        let mut gencmd = Command::new("cargo");
        gencmd.arg("readme");
        gencmd.arg("--no-title");
        gencmd.arg("-r");
        gencmd.arg(path.parent().unwrap());

        let contents = gencmd.output()?;

        if !contents.status.success() {
            bail!(
                "\"cargo readme\" command failed for {}: {:?}; \
                have you run \"cargo install cargo-readme\"?",
                cmd,
                contents
            );
        }

        write!(output, "        m.insert(\"{}\", r##\"", cmd)?;

        let header = format!("### `humility {}`\n", cmd);

        if contents.stdout.len() == 1 {
            output.write_all(&header.as_bytes()[1..])?;
            writeln!(
                output,
                r##"
Welp, no additional documentation for {} -- but there obviously should be!
Mind opening an issue on that if one isn't open already?
"##,
                cmd
            )?;
        } else {
            if !contents.stdout.starts_with(header.as_bytes()) {
                bail!("malformed documentation for {}", cmd);
            }

            output.write_all(&contents.stdout[1..])?;

            //
            // If we don't end on a blank line, insert one.
            //
            if !contents.stdout.ends_with("\n\n".as_bytes()) {
                writeln!(output)?;
            }
        }

        writeln!(output, "\"##);\n")?;
    }

    writeln!(
        output,
        r##"

    m.get(lookup).as_ref().map(|result| result as _)
}}

fn docs() -> &'static str {{
    r##""##
    )?;

    let root = metadata.workspace_root;
    let input = std::fs::read(root.join("README.md.in"))?;

    output.write_all(&input)?;

    writeln!(
        output,
        r##"## Commands

Run `humility doc` with the specified command name for details on each command.
"##
    )?;

    for (cmd, (description, _)) in &cmds {
        writeln!(output, "- `humility {}`: {}", cmd, description)?;
    }

    writeln!(output, "\n\"##\n}}")?;

    Ok(())
}
