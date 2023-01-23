// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::Result;
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

//
// Our generated command description.
//
struct CommandDescription {{
    init: fn() -> Command,
    docmsg: &'static str,
}}

fn dcmds() -> Vec<CommandDescription> {{
    vec![
"##
    )?;

    for id in &metadata.workspace_members {
        let package =
            metadata.packages.iter().find(|p| &p.id == id).unwrap().clone();

        if let Some(cmd) = package.name.strip_prefix("humility-cmd-") {
            cmds.insert(cmd.to_string().replace('-', "_"));
        }
    }

    for cmd in cmds.iter() {
        writeln!(
            output,
            r##"        CommandDescription {{
            init: cmd_{}::init,
            docmsg: "For additional documentation, run \"humility doc {}\"."
        }},"##,
            cmd, cmd
        )?;
    }

    write!(output, "    ]\n}}")?;

    Ok(())
}
