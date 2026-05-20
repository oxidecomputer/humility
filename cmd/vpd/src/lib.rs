// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility vpd`
//!
//! Reads from (or writes to) EEPROMs that contain vital product data (VPD).
//! To list all eligible devices, use `--list`:
//!
//! ```console
//! $ humility vpd --list
//! humility: attached via ST-Link V3
//! ID  C P  MUX ADDR DEVICE        DESCRIPTION               LOCKED
//!  0  1 B  1:1 0x50 at24csw080    Sharkfin VPD              locked
//!  1  1 B  1:2 0x50 at24csw080    Gimlet Fan VPD            unlocked
//!  2  1 B  1:3 0x50 at24csw080    Sidecar Fan VPD           unlocked
//! ```
//!
//! To read from all devices, combine `--list` with `--read`.  To read from a
//! particular device, use `--read` alone, and specify the device by either id
//! (`--id`) or by some (case-insensitive) substring of its description
//! (`--device`):
//!
//! ```console
//! $ humility vpd --read --id 0
//! humility: attached via ST-Link V3
//! [
//!    ("FRU0", [
//!        ("BARC", [
//!            "OXC11R00241",
//!        ]),
//!    ]),
//! ]
//! ```
//!
//! In this example, this could also be phrased as:
//!
//! ```console
//! $ humility vpd --read --device sharkfin
//! humility: attached via ST-Link V3
//! [
//!    ("FRU0", [
//!        ("BARC", [
//!            "OXC11R00241",
//!        ]),
//!    ]),
//! ]
//! ```
//!
//! You can also use the `--raw` flag to `--read` to see the raw bytes:
//!
//! ```console
//! $ humility vpd --read -i 10 --raw
//! humility: attached via ST-Link V3
//!             \/  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f
//! 0x00000000 | 46 52 55 30 4c 00 00 00 fd c6 3b db 42 41 52 43 | FRU0L.....;.BARC
//! 0x00000010 | 1f 00 00 00 ce 3d d7 f7 30 58 56 31 3a 39 31 33 | .....=..0XV1:913
//! 0x00000020 | 30 30 30 30 30 31 39 3a 30 30 36 3a 42 52 4d 34 | 0000019:006:BRM4
//! 0x00000030 | 32 32 32 30 30 32 33 00 b9 1f 7f e3 4d 41 43 30 | 2220023.....MAC0
//! 0x00000040 | 09 00 00 00 61 64 d1 7e a8 40 25 04 01 00 08 00 | ....ad.~.@%.....
//! 0x00000050 | 08 00 00 00 26 27 3d 9d ee a4 f9 bb ff ff ff ff | ....&'=.........
//! ```
//!
//! Note that this will fail if the description matches more than one
//! device, e.g.:
//!
//! ```console
//! $ humility vpd --read --device fan
//! humility: attached via ST-Link V3
//! humility vpd failed: multiple devices match description "fan"
//! ```
//!
//! To write VPD data, pass a filename of a file that contains a RON
//! description of a valid TLV-C payload, e.g.:
//!
//! ```console
//! $ cat vpd.in
//! [("BARC", [
//!    ("FOOB", [ [8, 6, 7, 5, 3, 0, 9] ]),
//!    ("QUUX", []),
//! ])]
//! $ humility vpd --write ./vpd.in --device sharkfin
//! humility: attached via ST-Link V3
//! humility: successfully wrote 56 bytes of VPD
//! ```
//!
//! You can also use a file as a loopback device via `--loopback`, allowing
//! you to, e.g., read binary data and format it (i.e., via `--read`).
//!
//! To lock a VPD device, use the `--lock` command.  This will lock the VPD
//! permanently and cannot be undone; subsequent attempts to write to (or
//! lock) a locked VPD device will result in an error.  The lock status of
//! each device is shown in `--list`.
//!
//! To lock all VPD devices, use the `--lock-all` command.
//!
//! - This will exit successfully if all devices were successfully locked or are
//!   already locked
//! - If a device is missing or returns an error when its lock status is
//!   initially checked, the command returns a *non-zero* exit status without
//!   locking other devices.  Use `--allow-missing` to disable this behavior.
//! - If all devices are present but some devices fail to lock, other devices
//!   will be locked and the command will return with a *non-zero* exit status.

use anyhow::{Result, bail};
use clap::{ArgGroup, Parser};
use humility::core::Core;
use humility::hubris::*;
use humility_cli::{ExecutionContext, HumilitySubcommand};
use humility_hexdump::Dumper;
use humility_vpd_lib::VpdTarget;
use std::fs;
use std::io::Write;
use std::time::Duration;

#[derive(Parser, Debug)]
#[clap(
    name = "vpd", about = env!("CARGO_PKG_DESCRIPTION"),
    group = ArgGroup::new("command").multiple(false),
)]
pub struct VpdArgs {
    /// sets timeout
    #[clap(
        long, short = 'T', default_value_t = 5000, value_name = "timeout_ms",
        value_parser = parse_int::parse::<u64>
    )]
    timeout: u64,

    /// list all devices that have VPD (can be combined with --read)
    #[clap(long, short, conflicts_with_all = &[
        "device", "id", "lock", "lock_all", "erase", "raw", "binary", "loopback"
    ])]
    list: bool,

    /// specify device by ID
    #[clap(long, short = 'i', value_name = "id", conflicts_with = "device")]
    id: Option<usize>,

    /// specifies a device by its description
    #[clap(long, short, value_name = "device")]
    device: Option<String>,

    /// write the contents of the specified file into the designated VPD
    #[clap(long, short, value_name = "filename", group = "command")]
    write: Option<String>,

    /// read the contents of the designated VPD (or of all with --list)
    #[clap(long, short, group = "command")]
    read: bool,

    /// erase the designated VPD
    #[clap(long, short, group = "command")]
    erase: bool,

    /// raw output
    #[clap(long, requires = "read")]
    raw: bool,

    /// binary output to file
    #[clap(long, requires = "read", conflicts_with = "raw")]
    binary: Option<String>,

    /// specify binary file to act as a loopback device
    #[clap(
        long, value_name = "file",
        conflicts_with_all = &["device", "id"]
    )]
    loopback: Option<String>,

    /// permanently lock VPD (cannot be undone!)
    #[clap(long, group = "command")]
    lock: bool,

    /// permanently locks all VPDs (cannot be undone!)
    #[clap(long, group = "command",
        conflicts_with_all = &["device", "id"]
    )]
    lock_all: bool,

    #[clap(long, requires = "lock_all")]
    allow_missing: bool,
}

fn vpd_devices(
    hubris: &HubrisArchive,
) -> impl Iterator<Item = &HubrisI2cDevice> {
    hubris
        .manifest
        .i2c_devices
        .iter()
        .filter(|device| device.device == "at24csw080")
}

pub fn list(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    timeout: Duration,
    read: bool,
) -> Result<()> {
    let results = humility_vpd_lib::vpd_list(hubris, core, timeout)?;

    println!(
        "{:2} {:>2} {:2} {:3} {:4} {:13} {:25} LOCKED",
        "ID", "C", "P", "MUX", "ADDR", "DEVICE", "DESCRIPTION",
    );

    for device in results {
        let mux = match (device.device.mux, device.device.segment) {
            (Some(m), Some(s)) => format!("{}:{}", m, s),
            (None, None) => "-".to_string(),
            (_, _) => "?:?".to_string(),
        };

        let locked = match device.locked {
            Ok(true) => "locked".to_string(),
            Ok(false) => "unlocked".to_string(),
            Err(s) => format!("<{s}>"),
        };

        println!(
            "{:2} {:2} {:2} {:3} 0x{:02x} {:13} {:25} {}",
            device.ndx,
            device.device.controller,
            device.device.port.name,
            mux,
            device.device.address,
            device.device.device,
            device.device.description,
            locked
        );

        if read {
            print!(" |\n +--> ");

            match device.data {
                Ok(vpd) => {
                    match tlvc::TlvcReader::begin(&vpd[..]) {
                        Ok(reader) => {
                            let p = tlvc_text::dump(reader);
                            println!("{}", ron::ser::to_string(&p)?);
                        }

                        Err(err) => {
                            println!("<{err:?}>");
                        }
                    };
                }
                Err(msg) => {
                    println!("<{msg}>");
                }
            }

            println!();
        }
    }

    Ok(())
}

fn target(hubris: &HubrisArchive, subargs: &VpdArgs) -> Result<VpdTarget> {
    let mut rval = None;

    if let Some(ref description) = subargs.device {
        let m = description.to_lowercase();

        for (ndx, device) in vpd_devices(hubris).enumerate() {
            if device.description.to_lowercase().contains(&m) {
                rval = match rval {
                    Some(_) => {
                        bail!(
                            "multiple devices match description \"{}\"",
                            description
                        );
                    }
                    None => Some(ndx),
                };
            }
        }

        match rval {
            Some(ndx) => Ok(VpdTarget::Device(ndx)),
            None => {
                bail!("no device matches description \"{}\"", description)
            }
        }
    } else if let Some(id) = subargs.id {
        let count = vpd_devices(hubris).count();

        if id < count {
            Ok(VpdTarget::Device(id))
        } else {
            bail!("device index {} invalid; --list to list", id)
        }
    } else if subargs.loopback.is_some() {
        bail!("loopback support is deprecated");
    } else {
        bail!("must specify either device ID or device description");
    }
}

enum OutputOption {
    Tlvc,
    Raw,
    Binary(String),
}

fn vpd_read(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    target: VpdTarget,
    timeout: Duration,
    output: OutputOption,
) -> Result<()> {
    let vpd = humility_vpd_lib::vpd_read(hubris, core, target, timeout)?;

    //
    // Now we should have the whole thing!
    //
    let reader = match tlvc::TlvcReader::begin(&vpd[..]) {
        Ok(reader) => reader,
        Err(err) => {
            bail!("{:?}", err);
        }
    };

    match output {
        OutputOption::Raw => {
            let dumper = Dumper::new();
            dumper.dump(&vpd, 0);
        }
        OutputOption::Binary(output) => {
            let mut file = fs::File::create(output)?;
            file.write_all(&vpd)?;
        }
        OutputOption::Tlvc => {
            let p = tlvc_text::dump(reader);
            tlvc_text::save(std::io::stdout(), &p)?;
            println!();
        }
    }

    Ok(())
}

fn vpd(subargs: VpdArgs, context: &mut ExecutionContext) -> Result<()> {
    let hubris = &context.cli.archive()?;
    let core = &mut *context.cli.attach_live_booted(hubris)?;
    let target = target(hubris, &subargs)?;
    let timeout = Duration::from_millis(subargs.timeout);

    if subargs.list {
        list(hubris, core, timeout, subargs.read)?;
    } else if let Some(path) = subargs.write {
        humility_vpd_lib::vpd_write(
            hubris,
            core,
            target,
            timeout,
            path.into(),
        )?;
    } else if subargs.erase {
        humility_vpd_lib::vpd_erase(hubris, core, target, timeout)?;
    } else if subargs.read {
        let options = if subargs.raw {
            OutputOption::Raw
        } else if let Some(s) = subargs.binary {
            OutputOption::Binary(s)
        } else {
            OutputOption::Tlvc
        };
        vpd_read(hubris, core, target, timeout, options)?;
    } else if subargs.lock {
        humility_vpd_lib::vpd_lock(hubris, core, target, timeout)?;
    } else if subargs.lock_all {
        humility_vpd_lib::vpd_lock_all(
            hubris,
            core,
            timeout,
            subargs.allow_missing,
        )?;
    } else {
        bail!("expected a command");
    }

    Ok(())
}

pub type Args = VpdArgs;
impl HumilitySubcommand for Args {
    fn run(args: Args, context: &mut ExecutionContext) -> Result<()> {
        vpd(args, context)
    }
}
