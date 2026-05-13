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
#[derive(::clap::Parser, Debug)]
pub enum Subcommand {{
"##
    )?;

    //
    let banned = if std::env::var_os("CARGO_FEATURE_PROBES").is_none() {
        // These are everything that can potentially pull in libusb
        ["probe-rs", "humility-probes-core", "rusb"].into_iter().collect()
    } else {
        BTreeSet::new()
    };
    for id in &metadata.workspace_members {
        let package =
            metadata.packages.iter().find(|p| &p.id == id).unwrap().clone();

        if let Some(cmd) = package.name.strip_prefix("humility-cmd-")
            && !package.dependencies.iter().any(|d| banned.contains(&*d.name))
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
    use ::humility_cli::HumilitySubcommand;
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
