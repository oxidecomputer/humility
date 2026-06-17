// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::Result;
use heck::*;
use std::collections::BTreeSet;
use std::env;
use std::fs::File;
use std::io::Write;
use std::path::Path;

fn main() -> Result<()> {
    use cargo_metadata::MetadataCommand;
    let mut cmds = BTreeSet::new();

    // MetadataCommand doesn't emit this, so we should
    println!("cargo:rerun-if-changed=Cargo.toml");

    let metadata =
        MetadataCommand::new().manifest_path("./Cargo.toml").exec().unwrap();

    let out_dir = env::var("OUT_DIR")?;
    let dest_path = Path::new(&out_dir).join("cmds.rs");
    let mut output = File::create(&dest_path)?;

    write!(
        output,
        r##"
#[derive(::clap::Subcommand, Debug)]
pub enum Subcommand {{
"##
    )?;

    let (banned_deps, disabled_cmds) =
        if std::env::var_os("CARGO_FEATURE_PROBES").is_none() {
            // These are everything that can potentially pull in libusb
            let banned_deps = ["probe-rs", "humility-probes-core", "rusb"]
                .into_iter()
                .collect();
            // We also look at which commands are enabled by the `probes`
            // feature on the `humility-bin` crate, then remove them.
            let disabled_cmds = metadata
                .workspace_members
                .iter()
                .find(|p| metadata[p].name == "humility-bin")
                .map(|p| metadata[p].clone())
                .expect(
                    "could not find package named `humility-bin` while \
                     executing its own build script; has it been renamed?",
                )
                .features["probes"]
                .iter()
                .flat_map(|p| {
                    p.strip_prefix("cmd-").map(|p| format!("humility-cmd-{p}"))
                })
                .collect::<BTreeSet<_>>();
            (banned_deps, disabled_cmds)
        } else {
            (BTreeSet::new(), BTreeSet::new())
        };

    for id in &metadata.workspace_members {
        let package = metadata[id].clone();

        if let Some(cmd) = package.name.strip_prefix("humility-cmd-")
            && !package
                .dependencies
                .iter()
                .any(|d| banned_deps.contains(&*d.name))
            && !disabled_cmds.contains(package.name.as_str())
        {
            cmds.insert(cmd.to_string());
        }
    }

    for cmd in cmds.iter() {
        writeln!(
            output,
            "    {}(cmd_{}::Args),",
            cmd.to_upper_camel_case(),
            cmd.to_snake_case()
        )?;
    }

    writeln!(
        output,
        "}}

pub fn dispatch(c: Subcommand, ctx: &mut ::humility_cli::ExecutionContext) -> anyhow::Result<()> {{
    match c {{"
    )?;
    for cmd in cmds.iter() {
        writeln!(
            output,
            "        Subcommand::{}(args) => cmd_{}::Args::run(args, ctx),",
            cmd.to_upper_camel_case(),
            cmd.to_snake_case(),
        )?;
    }
    writeln!(
        output,
        "    }}
}}

pub fn name(c: &Subcommand) -> String {{
    use ::clap::{{CommandFactory}};
    match c {{
    "
    )?;

    for cmd in cmds.iter() {
        writeln!(
            output,
            "        Subcommand::{}(_) => cmd_{}::Args::command().get_name().to_owned(),",
            cmd.to_upper_camel_case(),
            cmd.to_snake_case(),
        )?;
    }
    writeln!(
        output,
        "    }}
}}"
    )?;

    Ok(())
}
