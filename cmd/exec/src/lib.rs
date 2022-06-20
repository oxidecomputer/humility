// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility exec`
//!
//! `humility exec` executes a command within the named environment.
//! This is useful when managing many Hubris targets from a single
//! machine.
//!

use anyhow::{bail, Result};
use clap::Command as ClapCommand;
use clap::{CommandFactory, Parser};
use humility::hubris::HubrisArchive;
use humility_cmd::{Archive, Args, Command, Environment, RunUnattached};

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

fn parse_cmd<'a>(
    cmd: &str,
    cmds: &'a humility_cmd::env::Commands,
) -> Result<Vec<&'a str>> {
    let c = cmd.split(".").collect::<Vec<_>>();

    let spawn = match c[0] {
        "console" if c.len() == 1 => {
            if let Some(console) = &cmds.console {
                console
            } else {
                bail!("console command not present; --list for commands");
            }
        }
        "power" if c.len() == 2 => {
            if let Some(power) = &cmds.power {
                match c[1] {
                    "on" => &power.on,
                    "off" => &power.off,
                    _ => {
                        bail!("unknown power command \"{}\"", c[1]);
                    }
                }
            } else {
                bail!("power commands not present; --list for commands");
            }
        }
        _ => {
            bail!("unknown command \"{}\"; --list for commands", cmd);
        }
    };

    Ok(spawn.split_whitespace().collect::<Vec<_>>())
}

fn exec(
    _hubris: &mut HubrisArchive,
    args: &Args,
    subargs: &[String],
    env: Option<&Environment>,
) -> Result<()> {
    let subargs = ExecArgs::try_parse_from(subargs)?;
    let name = args.name.as_ref().unwrap();

    let env = match env {
        None => {
            bail!("must provide a name within environment");
        }
        Some(env) => env,
    };

    if subargs.list {
        let printcmd = |name, cmd| {
            println!("{:14} {}", name, cmd);
        };

        printcmd("NAME", "COMMAND");

        if let Some(cmds) = &env.cmds {
            if let Some(console) = &cmds.console {
                printcmd("console", console);
            }

            if let Some(power) = &cmds.power {
                printcmd("power.on", &power.on);
                printcmd("power.off", &power.off);
            }
        }
    } else {
        let cmds = match &env.cmds {
            Some(cmds) => cmds,
            None => {
                bail!("no commands for {}", name);
            }
        };

        let cmd = match subargs.cmd {
            Some(cmd) => cmd,
            None => {
                bail!("must provide command name; --list for commands");
            }
        };

        let args = parse_cmd(&cmd, &cmds)?;
        println!("{:?}", args);
    }

    Ok(())
}

pub fn init() -> (Command, ClapCommand<'static>) {
    (
        Command::Unattached {
            name: "exec",
            archive: Archive::Ignored,
            run: RunUnattached::Environment(exec),
        },
        ExecArgs::command(),
    )
}
