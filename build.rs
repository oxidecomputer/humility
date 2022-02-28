use anyhow::Result;
use std::collections::BTreeSet;
use std::env;
use std::fs::File;
use std::io::Write;
use std::path::Path;

fn main() -> Result<()> {
    use cargo_metadata::MetadataCommand;
    let mut cmds = BTreeSet::new();

    let metadata =
        MetadataCommand::new().manifest_path("./Cargo.toml").exec().unwrap();

    let out_dir = env::var("OUT_DIR")?;
    let dest_path = Path::new(&out_dir).join("cmds.rs");
    let mut output = File::create(&dest_path)?;

    write!(
        output,
        r##"

struct CommandDescription {{
    init: fn() -> (Command, App<'static>),
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
            cmds.insert(cmd.to_string());
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
