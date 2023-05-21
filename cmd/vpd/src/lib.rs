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
//! To read from a device, specify it by either id (`--id`) or by some
//! (case-insensitive) substring of its description (`--device`):
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
//! To lock a VPD device, use the `--lock` command.  This cannot be undone;
//! subsequent attempts to write to (or lock) a locked VPD device will result
//! in an error.  The lock status of each device is shown in `--list`.
//!

use anyhow::{bail, Context, Result};
use clap::{ArgGroup, CommandFactory, Parser};
use hif::*;
use humility::core::Core;
use humility::hubris::*;
use humility::reflect;
use humility_cli::{ExecutionContext, Subcommand};
use humility_cmd::CommandKind;
use humility_cmd::{Archive, Attach, Command, Dumper, Validate};
use humility_hiffy::*;
use humility_idol::{self as idol, HubrisIdol};
use indicatif::{ProgressBar, ProgressStyle};
use std::fs;
use std::io::{Read, Seek, SeekFrom, Write};

#[derive(Parser, Debug)]
#[clap(
    name = "vpd", about = env!("CARGO_PKG_DESCRIPTION"),
    group = ArgGroup::new("command").multiple(false).required(true)
)]
struct VpdArgs {
    /// sets timeout
    #[clap(
        long, short = 'T', default_value_t = 5000, value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// list all devices that have VPD
    #[clap(long, short, group = "command")]
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

    /// read the contents of the designated VPD
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

    /// lock VPD
    #[clap(long, group = "command")]
    lock: bool,
}

enum VpdTarget {
    Device(usize),
    Loopback(fs::File),
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

fn list(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    subargs: &VpdArgs,
) -> Result<()> {
    let devices = vpd_devices(hubris).collect::<Vec<_>>();
    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;
    let op = hubris.get_idol_command("Vpd.is_locked")?;

    let mut ops = vec![];

    for ndx in 0..devices.len() {
        let payload =
            op.payload(&[("index", idol::IdolArgument::Scalar(ndx as u64))])?;

        context.idol_call_ops(&op, &payload, &mut ops)?;
    }

    ops.push(Op::Done);

    let results = context.run(core, ops.as_slice(), None)?;

    println!(
        "{:2} {:>2} {:2} {:3} {:4} {:13} {:25} LOCKED",
        "ID", "C", "P", "MUX", "ADDR", "DEVICE", "DESCRIPTION",
    );

    for ((ndx, device), r) in devices.iter().enumerate().zip(results.iter()) {
        use humility::reflect::Base::Bool;
        use humility::reflect::Value::Base;

        let mux = match (device.mux, device.segment) {
            (Some(m), Some(s)) => format!("{}:{}", m, s),
            (None, None) => "-".to_string(),
            (_, _) => "?:?".to_string(),
        };

        let result = context.idol_result(&op, r);

        println!(
            "{:2} {:2} {:2} {:3} 0x{:02x} {:13} {:25} {}",
            ndx,
            device.controller,
            device.port.name,
            mux,
            device.address,
            device.device,
            device.description,
            match result {
                Ok(Base(Bool(val))) => match val {
                    false => "unlocked",
                    true => "locked",
                }
                .to_string(),
                Ok(r) => format!("<{r:?}>").to_string(),
                Err(err) => format!("<{}>", err).to_string(),
            },
        );
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
    } else if let Some(loopback) = &subargs.loopback {
        if subargs.write.is_some() {
            Ok(VpdTarget::Loopback(fs::File::create(loopback)?))
        } else {
            Ok(VpdTarget::Loopback(fs::File::open(loopback)?))
        }
    } else {
        bail!("must specify either device ID or device description");
    }
}

fn vpd_write(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    subargs: &VpdArgs,
) -> Result<()> {
    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;
    let op = hubris.get_idol_command("Vpd.write")?;
    let target = target(hubris, subargs)?;

    let bytes = if let Some(ref filename) = subargs.write {
        let file = fs::File::open(filename)?;

        let p = tlvc_text::load(file).with_context(|| {
            format!("failed to parse {} as VPD input", filename)
        })?;

        tlvc_text::pack(&p)
    } else {
        vec![0xffu8; 1024]
    };

    let target = match target {
        VpdTarget::Device(target) => target,
        VpdTarget::Loopback(mut file) => {
            file.write_all(&bytes)?;
            return Ok(());
        }
    };

    let mut all_ops = vec![];

    for (offset, b) in bytes.iter().enumerate() {
        let mut ops = vec![];
        let payload = op.payload(&[
            ("index", idol::IdolArgument::Scalar(target as u64)),
            ("offset", idol::IdolArgument::Scalar(offset as u64)),
            ("contents", idol::IdolArgument::Scalar(*b as u64)),
        ])?;

        context.idol_call_ops(&op, &payload, &mut ops)?;
        all_ops.push(ops);
    }

    let nops = (context.text_size() / context.ops_size(&all_ops[0])?) - 1;

    if nops == 0 {
        bail!("text size is too small for a single write!");
    }

    let mut offset = 0;

    let bar = ProgressBar::new(bytes.len() as u64);

    bar.set_style(ProgressStyle::default_bar().template(if subargs.erase {
        "humility: erasing VPD [{bar:30}] {bytes}/{total_bytes}"
    } else {
        "humility: writing VPD [{bar:30}] {bytes}/{total_bytes}"
    }));

    for chunk in all_ops.chunks(nops) {
        let mut ops = chunk.iter().flatten().copied().collect::<Vec<Op>>();
        ops.push(Op::Done);

        let results = context.run(core, ops.as_slice(), None)?;

        for (o, result) in results.iter().enumerate() {
            if let Err(err) = context.idol_result(&op, result) {
                bail!(
                    "failed to write VPD at offset {}: {:?}",
                    offset + o,
                    err
                );
            }
        }

        offset += results.len();

        bar.set_position(offset as u64);
    }

    bar.finish_and_clear();

    if subargs.erase {
        humility::msg!("successfully erased VPD");
    } else {
        humility::msg!("successfully wrote {offset} bytes of VPD");
    }

    Ok(())
}

fn vpd_read_at(
    core: &mut dyn Core,
    context: &mut HiffyContext,
    op: &idol::IdolOperation,
    target: &mut VpdTarget,
    offset: usize,
) -> Result<Vec<u8>> {
    let target = match target {
        VpdTarget::Device(target) => *target,
        VpdTarget::Loopback(ref mut file) => {
            let mut buffer = vec![];
            file.seek(SeekFrom::Start(offset as u64))?;
            file.read_to_end(&mut buffer)?;
            return Ok(buffer);
        }
    };

    let payload = op.payload(&[
        ("index", idol::IdolArgument::Scalar(target as u64)),
        ("offset", idol::IdolArgument::Scalar(offset as u64)),
    ])?;

    let mut ops = vec![];

    context.idol_call_ops(op, &payload, &mut ops)?;
    ops.push(Op::Done);

    let results = context.run(core, ops.as_slice(), None)?;

    let r = context
        .idol_result(op, &results[0])
        .with_context(|| format!("failed to read at offset {offset}"))?;
    let contents = r.as_struct()?["value"].as_array()?;
    let mut rval = vec![];

    for b in contents.iter() {
        if let reflect::Base::U8(val) = b.as_base()? {
            rval.push(*val);
        } else {
            bail!("expected array of U8; found {:?}", contents);
        }
    }

    Ok(rval)
}

fn vpd_read(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    subargs: &VpdArgs,
) -> Result<()> {
    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;
    let op = hubris.get_idol_command("Vpd.read")?;
    let mut target = target(hubris, subargs)?;

    //
    // First, read in enough to read just the header.
    //
    let mut vpd = vpd_read_at(core, &mut context, &op, &mut target, 0)?;

    let reader = match tlvc::TlvcReader::begin(&vpd[..]) {
        Ok(reader) => reader,
        Err(err) => bail!("{:?}", err),
    };

    //
    // If this isn't a header, see if it's all 0xff -- in which case we
    // will suggest that the part is unprogrammed.
    //
    let header = match reader.read_header() {
        Ok(header) => header,
        Err(err) => match vpd.iter().find(|&b| *b != 0xffu8) {
            Some(_) => {
                bail!("bad header: {:x?}", err);
            }
            None => {
                bail!("VPD appears to be unprogrammed");
            }
        },
    };

    //
    // And now go back and read everything.
    //
    let total = header.total_len_in_bytes();

    while vpd.len() < total {
        vpd.extend(
            vpd_read_at(core, &mut context, &op, &mut target, vpd.len())
                .with_context(|| format!("failed to read {total} bytes"))?,
        );
    }

    //
    // Now we should have the whole thing!
    //
    let reader = match tlvc::TlvcReader::begin(&vpd[..total]) {
        Ok(reader) => reader,
        Err(err) => {
            bail!("{:?}", err);
        }
    };

    if subargs.lock {
        //
        // We can only get here because we are doing the read as part of
        // a `--lock` operation.
        //
        assert!(!subargs.read);
    } else if subargs.raw {
        let dumper = Dumper::new();
        dumper.dump(&vpd, 0);
    } else if let Some(output) = &subargs.binary {
        let mut file = fs::File::create(output)?;
        file.write_all(&vpd)?;
    } else {
        let p = tlvc_text::dump(reader);
        tlvc_text::save(std::io::stdout(), &p)?;
        println!();
    }

    Ok(())
}

fn vpd_lock(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    subargs: &VpdArgs,
) -> Result<()> {
    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;

    let op = hubris.get_idol_command("Vpd.lock")?;
    let index = match target(hubris, subargs)? {
        VpdTarget::Device(index) => index,
        _ => {
            bail!("can only lock a physical device");
        }
    };

    if let Err(err) = vpd_read(hubris, core, subargs) {
        bail!("can't lock VPD: {}", err);
    }

    let payload =
        op.payload(&[("index", idol::IdolArgument::Scalar(index as u64))])?;

    let mut ops = vec![];

    context.idol_call_ops(&op, &payload, &mut ops)?;
    ops.push(Op::Done);

    let results = context.run(core, ops.as_slice(), None)?;

    if let Err(err) = context.idol_result(&op, &results[0]) {
        bail!("failed to lock {}: {:?}", index, err);
    }

    humility::msg!("successfully locked VPD");

    Ok(())
}

fn vpd(context: &mut ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let subargs = VpdArgs::try_parse_from(subargs)?;
    let hubris = context.archive.as_ref().unwrap();

    if subargs.list {
        list(hubris, core, &subargs)?;
    } else if subargs.write.is_some() || subargs.erase {
        vpd_write(hubris, core, &subargs)?;
    } else if subargs.read {
        vpd_read(hubris, core, &subargs)?;
    } else if subargs.lock {
        vpd_lock(hubris, core, &subargs)?;
    } else {
        // Clap should prevent us from getting here
        panic!();
    }

    Ok(())
}

pub fn init() -> Command {
    Command {
        app: VpdArgs::command(),
        name: "vpd",
        run: vpd,
        kind: CommandKind::Attached {
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
        },
    }
}
