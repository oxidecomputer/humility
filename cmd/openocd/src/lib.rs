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

use humility_cli::{ExecutionContext, Subcommand};
use humility_cmd::{Archive, Command, CommandKind};

use anyhow::{Context, Result, bail};
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

fn openocd(context: &mut ExecutionContext) -> Result<()> {
    if context.cli.probe.is_some() {
        bail!("Cannot specify --probe with `openocd` subcommand");
    }

    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();

    let subargs = OcdArgs::try_parse_from(subargs)?;
    let serial = context.cli.get_probe_serial(subargs.serial.as_deref())?;

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
