// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility gdb`
//!
//! This command launches GDB and attaches to a running device.
//!
//! By default, the user must be running `openocd` or `pyocd` in a separate
//! terminal.
//!
//! The `--run-openocd` option automatically launches `openocd` based on the
//! `openocd.cfg` file included in the build archive.
//!
//! When using `pyocd`, it must be launched with the `--persist` option,
//! because `humility gdb` connects to it multiple times (once to check the
//! app id, then again to run the console).
//!

use std::process::{Command, Stdio};

use cmd_openocd::get_probe_serial;

use humility::cli::Subcommand;
use humility_cmd::{Archive, Command as HumilityCmd, CommandKind};

use anyhow::{bail, Context, Result};
use clap::{CommandFactory, Parser};

#[derive(Parser, Debug)]
#[clap(
    name = "gdb", about = env!("CARGO_PKG_DESCRIPTION"),
)]
struct GdbArgs {
    /// when set, calls `load` and `stepi` upon attaching
    #[clap(long, short)]
    load: bool,

    /// when set, runs an OpenOCD process before starting GDB
    #[clap(long, group = "run_openocd")]
    run_openocd: bool,

    /// specifies the `openocd` executable to run
    #[clap(long, requires = "run_openocd")]
    openocd: Option<String>,

    /// specifies the probe serial number to use with OpenOCD
    #[clap(long, requires = "run_openocd")]
    serial: Option<String>,
}

fn gdb(context: &mut humility::ExecutionContext) -> Result<()> {
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let hubris = context.archive.as_ref().unwrap();

    if context.cli.probe.is_some() {
        bail!("Cannot specify --probe with `gdb` subcommand");
    }

    let subargs = GdbArgs::try_parse_from(subargs)?;
    let serial = get_probe_serial(&context.cli, subargs.serial.clone())?;

    let work_dir = tempfile::tempdir()?;
    let name = match &hubris.manifest.name {
        Some(name) => name,
        None => bail!("Could not get app name from manifest"),
    };
    let elf_dir = work_dir.path().join("target").join(name).join("dist");
    std::fs::create_dir_all(&elf_dir)?;
    hubris.extract_elfs_to(&elf_dir)?;

    hubris
        .extract_file_to(
            "debug/openocd.gdb",
            &work_dir.path().join("openocd.gdb"),
        )
        .context("GDB config missing. Is your Hubris build too old?")?;
    hubris
        .extract_file_to(
            "debug/script.gdb",
            &work_dir.path().join("script.gdb"),
        )
        .context("GDB script missing. Is your Hubris build too old?")?;
    hubris
        .extract_file_to("img/final.elf", &work_dir.path().join("final.elf"))?;

    let mut gdb_cmd = None;

    const GDB_NAMES: [&str; 2] = ["arm-none-eabi-gdb", "gdb-multiarch"];
    for candidate in &GDB_NAMES {
        if Command::new(candidate)
            .arg("--version")
            .stdout(Stdio::piped())
            .status()
            .is_ok()
        {
            gdb_cmd = Some(candidate);
            break;
        }
    }

    // Select the GDB command
    let gdb_cmd = gdb_cmd.ok_or_else(|| {
        anyhow::anyhow!("GDB not found.  Tried: {:?}", GDB_NAMES)
    })?;

    // If OpenOCD is requested, then run it in a subprocess here, with an RAII
    // handle to ensure that it's killed before the program exits.
    struct OpenOcdRunner(std::process::Child);
    impl Drop for OpenOcdRunner {
        fn drop(&mut self) {
            self.0.kill().expect("Could not kill `openocd`")
        }
    }
    let _openocd = if subargs.run_openocd {
        hubris
            .extract_file_to(
                "debug/openocd.cfg",
                &work_dir.path().join("openocd.cfg"),
            )
            .context("openocd config missing. Is your Hubris build too old?")?;
        let mut cmd = Command::new(
            subargs.openocd.unwrap_or_else(|| "openocd".to_string()),
        );
        cmd.arg("-f").arg("openocd.cfg");
        if let Some(serial) = serial {
            cmd.arg("-c")
                .arg("interface hla")
                .arg("-c")
                .arg(format!("hla_serial {}", serial));
        }
        cmd.current_dir(work_dir.path());
        cmd.stdin(Stdio::piped());
        Some(OpenOcdRunner(cmd.spawn().context("Could not start `openocd`")?))
    } else {
        None
    };

    // Alright, here's where it gets awkward.  We are either
    // - Running OpenOCD, launched by the block above
    // - Running OpenOCD in a separate terminal, through humility openocd
    //   or manually
    // - Running PyOCD in a separate terminal
    //
    // If we aren't loading new firmware, then we want to check that the
    // running firmware matches the image.  However, we can't use
    // humility_cmd::attach like normal, because that's not compatible with
    // PyOCD (which doesn't expose the TCL port).
    //
    // Instead, we fall back to the one interface that we _know_ these three
    // cases have in common: GDB.  Also, GDB is _terrible_: `print/x` doesn't
    // work if we're attached to the target and have all of our sections
    // loaded.
    if !subargs.load {
        let mut cmd = Command::new(gdb_cmd);
        let image_id_addr = hubris.image_id_addr().unwrap();
        let image_id = hubris.image_id().unwrap();
        cmd.arg("-q")
            .arg("-x")
            .arg("openocd.gdb")
            .arg("-ex")
            .arg(format!(
                "dump binary memory image_id {} {}",
                image_id_addr,
                image_id_addr as usize + image_id.len(),
            ))
            .arg("-ex")
            .arg("set confirm off")
            .arg("-ex")
            .arg("quit");
        cmd.current_dir(work_dir.path());
        let status = cmd.status()?;
        if !status.success() {
            anyhow::bail!("could not get image_id, see output for details");
        }
        let image_id_actual = std::fs::read(work_dir.path().join("image_id"))?;
        if image_id_actual != image_id {
            bail!(
                "Invalid image ID: expected {:?}, got {:?}",
                image_id,
                image_id_actual
            );
        }
    }

    let mut cmd = Command::new(gdb_cmd);
    cmd.arg("-q").arg("-x").arg("script.gdb").arg("-x").arg("openocd.gdb");
    if subargs.load {
        // start the process but immediately halt the processor
        cmd.arg("-ex").arg("load").arg("-ex").arg("stepi");
    }
    cmd.arg("final.elf");
    cmd.current_dir(work_dir.path());

    // Run GDB, ignoring Ctrl-C (so it can handle them)
    ctrlc::set_handler(|| {}).expect("Error setting Ctrl-C handler");
    let status = cmd.status()?;
    if !status.success() {
        anyhow::bail!("command failed, see output for details");
    }
    Ok(())
}

pub fn init() -> HumilityCmd {
    HumilityCmd {
        app: GdbArgs::command(),
        name: "gdb",
        run: gdb,
        kind: CommandKind::Unattached { archive: Archive::Required },
    }
}
