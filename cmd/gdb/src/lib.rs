// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility gdb`
//!
//! This command launches GDB and attaches to a running device.
//!

use std::process::Command;

use humility::hubris::*;
use humility_cmd::{Archive, Args, Attach, Command as HumilityCmd, Validate};

use anyhow::{bail, Result};
use clap::{Command as ClapCommand, CommandFactory, Parser};

#[derive(Parser, Debug)]
#[clap(
    name = "gdb", about = env!("CARGO_PKG_DESCRIPTION"),
)]
struct GdbArgs {
    /// when set, calls `load` and `stepi` upon attaching
    #[clap(long, short)]
    load: bool,
}

fn gdb(
    hubris: &mut HubrisArchive,
    args: &Args,
    subargs: &[String],
) -> Result<()> {
    let subargs = GdbArgs::try_parse_from(subargs)?;

    // Do a dummy attach to confirm that the image matches
    humility_cmd::attach(
        hubris,
        args,
        Attach::LiveOnly,
        Validate::Match,
        |_, _| Ok(()),
    )?;

    let work_dir = tempfile::tempdir()?;
    let name = match &hubris.manifest.name {
        Some(name) => name,
        None => bail!("Could not get app name from manifest"),
    };
    let elf_dir = work_dir.path().join("target").join(name).join("dist");
    std::fs::create_dir_all(&elf_dir)?;
    hubris.extract_elfs_to(&elf_dir)?;

    hubris.extract_file_to(
        "debug/openocd.gdb",
        &work_dir.path().join("openocd.gdb"),
    )?;
    hubris.extract_file_to(
        "debug/script.gdb",
        &work_dir.path().join("script.gdb"),
    )?;
    hubris
        .extract_file_to("img/final.elf", &work_dir.path().join("final.elf"))?;

    let mut cmd = None;

    const GDB_NAMES: [&str; 2] = ["arm-none-eabi-gdb", "gdb-multiarch"];
    for candidate in &GDB_NAMES {
        if Command::new(candidate).arg("--version").status().is_ok() {
            cmd = Some(Command::new(candidate));
            break;
        }
    }

    let mut cmd = cmd.ok_or_else(|| {
        anyhow::anyhow!("GDB not found.  Tried: {:?}", GDB_NAMES)
    })?;

    cmd.arg("-q").arg("-x").arg("script.gdb").arg("-x").arg("openocd.gdb");

    if subargs.load {
        // start the process but immediately halt the processor
        cmd.arg("-ex").arg("load").arg("-ex").arg("stepi");
    }
    cmd.arg("final.elf");
    cmd.current_dir(work_dir.path());
    println!("{:?}", cmd);

    ctrlc::set_handler(|| {}).expect("Error setting Ctrl-C handler");
    let status = cmd.status()?;
    if !status.success() {
        anyhow::bail!("command failed, see output for details");
    }
    Ok(())
}

pub fn init() -> (HumilityCmd, ClapCommand<'static>) {
    (
        HumilityCmd::Unattached {
            name: "gdb",
            archive: Archive::Required,
            run: gdb,
        },
        GdbArgs::command(),
    )
}
