// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility vpd`
//!
//! Reads from (or writes to) EEPROMs that contain vital product data (VPD).
//! To list all eligible devices, use `--list`:
//!
//! ```console
//! % humility vpd --list
//! humility: attached via ST-Link V3
//! ID  C P  MUX ADDR DEVICE        DESCRIPTION
//!  0  1 B  1:1 0x50 at24csw080    Sharkfin VPD
//!  1  1 B  1:2 0x50 at24csw080    Gimlet Fan VPD
//!  2  1 B  1:3 0x50 at24csw080    Sidecar Fan VPD
//! ```
//!
//! To read from a device, specify it by either id (`--id`) or by some
//! (case-insensitive) substring of its description (`--device`):
//!
//! ```console
//! % humility vpd --read --id 0
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
//! % humility vpd --read --device sharkfin
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
//! Note that this will fail if the description matches more than one
//! device, e.g.:
//!
//! ```console
//! % humility vpd --read --device fan
//! humility: attached via ST-Link V3
//! humility vpd failed: multiple devices match description "fan"
//! ```
//!
//! To write VPD data, pass a filename of a file that contains a RON
//! description of a valid TLV-C payload, e.g.:
//!
//! ```console
//! % cat vpd.in
//! [("BARC", [
//!    ("FOOB", [ [8, 6, 7, 5, 3, 0, 9] ]),
//!    ("QUUX", []),
//! ])]
//! % humility vpd --write ./vpd.in --device sharkfin
//! humility: attached via ST-Link V3
//! humility: successfully wrote 56 bytes of VPD
//! ```
//!

use anyhow::{anyhow, bail, Context, Result};
use clap::{ArgGroup, CommandFactory, Parser};
use hif::*;
use humility::cli::Subcommand;
use humility::core::Core;
use humility::hubris::*;
use humility::reflect;
use humility_cmd::hiffy::*;
use humility_cmd::idol::{self, HubrisIdol};
use humility_cmd::CommandKind;
use humility_cmd::{Archive, Attach, Command, Validate};
use indicatif::{ProgressBar, ProgressStyle};
use std::fs;

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

fn list(hubris: &HubrisArchive) -> Result<()> {
    println!(
        "{:2} {:>2} {:2} {:3} {:4} {:13} DESCRIPTION",
        "ID", "C", "P", "MUX", "ADDR", "DEVICE"
    );

    for (ndx, device) in vpd_devices(hubris).enumerate() {
        let mux = match (device.mux, device.segment) {
            (Some(m), Some(s)) => format!("{}:{}", m, s),
            (None, None) => "-".to_string(),
            (_, _) => "?:?".to_string(),
        };

        println!(
            "{:2} {:2} {:2} {:3} 0x{:02x} {:13} {}",
            ndx,
            device.controller,
            device.port.name,
            mux,
            device.address,
            device.device,
            device.description
        );
    }

    Ok(())
}

fn target(hubris: &HubrisArchive, subargs: &VpdArgs) -> Result<usize> {
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

        rval.ok_or_else(|| {
            anyhow!("no device matches description \"{}\"", description)
        })
    } else if let Some(id) = subargs.id {
        let count = vpd_devices(hubris).count();

        if id < count {
            Ok(id)
        } else {
            bail!("device index {} invalid; --list to list", id)
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
    let funcs = context.functions()?;
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

    let mut all_ops = vec![];

    for (offset, b) in bytes.iter().enumerate() {
        let mut ops = vec![];
        let payload = op.payload(&[
            ("index", idol::IdolArgument::Scalar(target as u64)),
            ("offset", idol::IdolArgument::Scalar(offset as u64)),
            ("contents", idol::IdolArgument::Scalar(*b as u64)),
        ])?;

        context.idol_call_ops(&funcs, &op, &payload, &mut ops)?;
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
        humility::msg!("successfully wrote {} bytes of VPD", offset);
    }

    Ok(())
}

fn vpd_read_at(
    core: &mut dyn Core,
    context: &mut HiffyContext,
    funcs: &HiffyFunctions,
    op: &idol::IdolOperation,
    target: usize,
    offset: usize,
) -> Result<Vec<u8>> {
    let payload = op.payload(&[
        ("index", idol::IdolArgument::Scalar(target as u64)),
        ("offset", idol::IdolArgument::Scalar(offset as u64)),
    ])?;

    let mut ops = vec![];

    context.idol_call_ops(funcs, op, &payload, &mut ops)?;
    ops.push(Op::Done);

    let results = context.run(core, ops.as_slice(), None)?;

    let r = context.idol_result(op, &results[0])?;
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
    let funcs = context.functions()?;
    let op = hubris.get_idol_command("Vpd.read")?;
    let target = target(hubris, subargs)?;

    //
    // First, read in enough to read just the header.
    //
    let mut vpd = vpd_read_at(core, &mut context, &funcs, &op, target, 0)?;

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
        vpd.extend(vpd_read_at(
            core,
            &mut context,
            &funcs,
            &op,
            target,
            vpd.len(),
        )?);
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

    let p = tlvc_text::dump(reader);
    tlvc_text::save(std::io::stdout(), &p)?;
    println!();

    Ok(())
}

fn vpd(context: &mut humility::ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let subargs = VpdArgs::try_parse_from(subargs)?;
    let hubris = context.archive.as_ref().unwrap();

    if subargs.list {
        list(hubris)?;
    } else if subargs.write.is_some() || subargs.erase {
        vpd_write(hubris, core, &subargs)?;
    } else if subargs.read {
        vpd_read(hubris, core, &subargs)?;
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
