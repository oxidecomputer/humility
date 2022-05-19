// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility openocd`
//!
//! This command launches OpenOCD based on the config file in a build archive
//!

use std::process::Command;

use humility::hubris::*;
use humility_cmd::{Archive, Args, Attach, Command as HumilityCmd, Validate};

use anyhow::{Context, Result};
use clap::{Command as ClapCommand, CommandFactory, Parser};

#[derive(Parser, Debug)]
#[clap(
    name = "openocd", about = env!("CARGO_PKG_DESCRIPTION"),
)]
struct OcdArgs {
    /// specifies the `openocd` executable to run
    #[clap(short, long)]
    exec: Option<String>,

    /// Extra options to pass to `openocd`
    #[clap(last = true)]
    extra_options: Vec<String>,
}

fn openocd(
    hubris: &mut HubrisArchive,
    args: &Args,
    subargs: &[String],
) -> Result<()> {
    let subargs = OcdArgs::try_parse_from(subargs)?;

    // Do a dummy attach to confirm that the image matches
    humility_cmd::attach(
        hubris,
        args,
        Attach::LiveOnly,
        Validate::Match,
        |_, _| Ok(()),
    )?;

    let work_dir = tempfile::tempdir()?;
    hubris
        .extract_file_to(
            "debug/openocd.cfg",
            &work_dir.path().join("openocd.cfg"),
        )
        .context("OpenOCD config missing. Is your Hubris build too old?")?;
    let mut cmd =
        Command::new(subargs.exec.unwrap_or_else(|| "openocd".to_string()));
    cmd.arg("-f").arg("openocd.cfg");
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
        anyhow::bail!(
            "command failed ({:?}), see output for details",
            status.code().unwrap()
        );
    }
    Ok(())
}

pub fn init() -> (HumilityCmd, ClapCommand<'static>) {
    (
        HumilityCmd::Unattached {
            name: "openocd",
            archive: Archive::Required,
            run: openocd,
        },
        OcdArgs::command(),
    )
}
