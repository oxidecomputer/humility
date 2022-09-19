// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility exec`
//!
//! `humility exec` executes a command for a target within the specified
//! environment.  The environment is specified to Humility via an argument
//! (`--environment`) or an environment variable (`HUMILITY_ENVIRONMENT`);
//! the target is similarly specified via an argument (`--target`) or
//! an environment variable (`HUMILITY_TARGET`).  If specified via an
//! argument, note that both the environment and target must occur before
//! any subcommand, e.g.:
//!
//! ```console
//! $ humility -e /path/to/env.json -t my-target exec power.on
//! ```
//!
//! To list available commands, use the `--list` option.
//!
//! For more details, see the Humliity documenation on environments.
//!

use anyhow::{bail, Result};
use clap::{CommandFactory, Parser};
use humility::cli::Subcommand;
use humility::ExecutionContext;
use humility_cmd::{Archive, Command, CommandKind};
use serde_json::Value;
use std::collections::BTreeMap;

#[derive(Parser, Debug)]
#[clap(name = "exec", about = env!("CARGO_PKG_DESCRIPTION"))]
struct ExecArgs {
    /// list possible commands
    #[clap(long, short)]
    list: bool,

    /// Command to execute
    #[clap(conflicts_with = "list")]
    cmd: Option<String>,
}

fn load_cmds<'a>(
    target: &str,
    cmds: &'a Value,
    stack: &mut Vec<&'a str>,
    rval: &mut BTreeMap<String, String>,
) -> Result<()> {
    match cmds {
        Value::Object(obj) => {
            for (key, value) in obj.iter() {
                stack.push(key);
                load_cmds(target, value, stack, rval)?;
                stack.pop();
            }
        }
        Value::String(cmd) => {
            rval.insert(stack.join("."), cmd.to_string());
        }
        _ => {
            bail!(
                "illegal command for target {} at {}: {:?}",
                target,
                stack.join("."),
                cmds
            );
        }
    }

    Ok(())
}

fn exec(context: &mut ExecutionContext) -> Result<()> {
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let subargs = ExecArgs::try_parse_from(subargs)?;
    let env = context.environment.as_ref();

    let env = match env {
        None => {
            bail!("must provide a target within environment");
        }
        Some(env) => env,
    };

    //
    // If we have an environment, we must also have a target.
    //
    let target = context.cli.target.as_ref().unwrap();

    let cmds = match env.cmds {
        Some(ref cmds) => cmds,
        None => {
            bail!("target {} has no defined commands", target);
        }
    };

    let mut avail = BTreeMap::new();
    let mut stack = Vec::new();
    load_cmds(target, cmds, &mut stack, &mut avail)?;

    if subargs.list {
        let printcmd = |target, cmd| {
            println!("{:14} {}", target, cmd);
        };

        printcmd("NAME", "COMMAND");

        for (name, cmd) in &avail {
            printcmd(name, cmd);
        }
    } else {
        let cmd = match subargs.cmd {
            Some(cmd) => cmd,
            None => {
                bail!("must provide command; --list for commands");
            }
        };

        if let Some(cmdline) = avail.get(&cmd) {
            let args = splitty::split_unquoted_char(cmdline, ' ')
                .unwrap_quotes(true)
                .collect::<Vec<_>>();

            humility::msg!("{} {}: executing: '{}' ...", target, cmd, cmdline);

            let status = std::process::Command::new(args[0])
                .args(&args[1..])
                .status()?;

            humility::msg!(
                "{} {}: done ({})",
                target,
                cmd,
                match status.code() {
                    Some(code) => format!("status code {code}"),
                    None => "terminated by signal".to_string(),
                }
            );
        } else {
            bail!("unknown command \"{}\"; --list for commands", cmd);
        }
    }

    Ok(())
}

pub fn init() -> Command {
    Command {
        app: ExecArgs::command(),
        name: "exec",
        run: exec,
        kind: CommandKind::Unattached { archive: Archive::Ignored },
    }
}
