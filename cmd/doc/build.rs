// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::{anyhow, bail, ensure, Context, Result};
use std::collections::BTreeMap;
use std::env;
use std::fs::File;
use std::io::Write;
use std::path::Path;

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
#[allow(clippy::needless_raw_string_hashes)]
fn cmd_docs(lookup: &str) -> Option<&'static str> {{
    let mut m = HashMap::new();
"##
    )?;

    for (cmd, (_, path)) in &cmds {
        println!(
            "cargo:rerun-if-changed={}",
            path.parent().unwrap().join("src/lib.rs").display()
        );

        let cmd_path = path.parent().unwrap();
        let mut lib_path = cmd_path.join("src");
        lib_path.push("lib.rs");
        let mut file = File::open(&lib_path).with_context(|| {
            format!("failed to open {}", lib_path.display())
        })?;
        let contents = cargo_readme::generate_readme(
            cmd_path, &mut file, None, false, true, true, true,
        )
        .map_err(|error| {
            anyhow!("failed to generate README for {cmd}: {error}")
        })?;

        //
        // We are prescriptive about what we expect this output to look like.
        //
        let header = format!("### `humility {}`\n", cmd);
        ensure!(
            contents.starts_with(&header),
            "documentation for {cmd} is malformed: \
            must begin with '{header}'",
        );
        ensure!(contents.len() > 1, "no documentation for {cmd}");

        write!(output, "        m.insert(\"{}\", r##\"", cmd)?;

        output.write_all(&contents.as_bytes()[1..])?;

        //
        // If we don't end on a blank line, insert one.
        //
        if !contents.ends_with("\n\n") {
            writeln!(output)?;
        }

        writeln!(output, "\"##);\n")?;
    }

    writeln!(
        output,
        r##"

    m.get(lookup).as_ref().map(|result| result as _)
}}

#[allow(clippy::needless_raw_string_hashes)]
fn docs() -> &'static str {{
    r##""##
    )?;

    let root = metadata.workspace_root;
    println!("cargo:rerun-if-changed={}", root.join("README.md.in").display());
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
