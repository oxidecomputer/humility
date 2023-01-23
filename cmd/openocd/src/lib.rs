// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility openocd`
//!
//! This command launches OpenOCD based on the config file in a build archive,
//! which then allows one to connect with either GDB or directly via telnet.
//! If the intention is to only run GDB, note that `humility gdb --run-openocd`
//! will both run OpenOCD and run a foreground GDB that is connected to it.
//!

use std::process;

use humility::cli::{Cli, Subcommand};
use humility_cmd::{Archive, Command, CommandKind};

use anyhow::{bail, Context, Result};
use clap::{CommandFactory, Parser};

#[derive(Parser, Debug)]
#[clap(
    name = "openocd", about = env!("CARGO_PKG_DESCRIPTION"),
)]
struct OcdArgs {
    /// specifies the `openocd` executable to run
    #[clap(short, long)]
    exec: Option<String>,

    /// specifies the probe serial number to use with OpenOCD
    #[clap(long)]
    serial: Option<String>,

    /// Extra options to pass to `openocd`
    #[clap(last = true)]
    extra_options: Vec<String>,
}

/// Extracts and returns the probe serial name for use in OpenOCD
///
/// We can get the serial name from two different places:
/// - If the global `--probe` argument is of the form `vid:pid:serial`, then we
///   can extract the serial name.  This is also the case if we're using an
///   environment file, which populates `args.probe` automatically.
/// - If the `--serial` argument is given in this command's subarguments,
///   then we use it directly.
///
/// This function checks both sources, returns an error if they conflict, and
/// returns the (optional) serial name otherwise.
pub fn get_probe_serial(
    args: &Cli,
    subargs_serial: Option<String>,
) -> Result<Option<String>> {
    match &args.probe {
        Some(probe) => {
            let re = regex::Regex::new(
                r"^([[:xdigit:]]+):([[:xdigit:]]+):([[:xdigit:]]+)$",
            )
            .unwrap();
            if let Some(cap) = re.captures(probe) {
                if subargs_serial.is_some() {
                    if args.target.is_some() {
                        bail!(
                            "Cannot specify probe serial number with both \
                             environment and `--serial`"
                        );
                    } else {
                        bail!(
                            "Cannot specify probe serial number with both \
                             `--probe` and `--serial`"
                        );
                    }
                }
                Ok(Some(cap.get(3).unwrap().as_str().to_string()))
            } else {
                bail!(
                    "Cannot specify `--probe {}` with `openocd` subcommand \
                     (must be of the form vid:pid:serial)",
                    probe
                );
            }
        }
        None => Ok(subargs_serial),
    }
}

fn openocd(context: &mut humility::ExecutionContext) -> Result<()> {
    if context.cli.probe.is_some() {
        bail!("Cannot specify --probe with `openocd` subcommand");
    }

    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();

    let subargs = OcdArgs::try_parse_from(subargs)?;
    let serial = get_probe_serial(&context.cli, subargs.serial.clone())?;

    let hubris = context.archive.as_ref().unwrap();

    let work_dir = tempfile::tempdir()?;
    hubris
        .extract_file_to(
            "debug/openocd.cfg",
            &work_dir.path().join("openocd.cfg"),
        )
        .context("OpenOCD config missing. Is your Hubris build too old?")?;
    let mut cmd = process::Command::new(
        subargs.exec.unwrap_or_else(|| "openocd".to_string()),
    );
    cmd.arg("-f").arg("openocd.cfg");
    if let Some(serial) = serial {
        cmd.arg("-c")
            .arg("interface hla")
            .arg("-c")
            .arg(format!("hla_serial {}", serial));
    }
    cmd.current_dir(work_dir.path());

    for opt in subargs.extra_options {
        cmd.arg(opt);
    }

    // Run OpenOCD, ignoring Ctrl-C (so it can handle them)
    ctrlc::set_handler(|| {}).expect("Error setting Ctrl-C handler");
    let status = cmd.status()?;

    // Then, check on the OpenOCD status.
    //
    // If OpenOCD is killed by Ctrl-C (which is typical), `status.success()`
    // will be `false`, but `status.code()` will be `None` (based on
    // `ExitStatus` docs), so this saves us from printing a spurious error
    // message.
    if !status.success() && status.code().is_some() {
        bail!(
            "command failed ({:?}), see output for details",
            status.code().unwrap()
        );
    }
    Ok(())
}

pub fn init() -> Command {
    Command {
        app: OcdArgs::command(),
        name: "openocd",
        run: openocd,
        kind: CommandKind::Unattached { archive: Archive::Required },
    }
}
