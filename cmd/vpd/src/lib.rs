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
use clap::Command as ClapCommand;
use clap::{ArgGroup, CommandFactory, Parser};
use hif::*;
use humility::core::Core;
use humility::hubris::*;
use humility::reflect;
use humility_cmd::hiffy::*;
use humility_cmd::idol;
use humility_cmd::{Archive, Attach, Command, Run, Validate};
use indicatif::{ProgressBar, ProgressStyle};
use std::fs;
use tlvc_text::Piece;
use zerocopy::AsBytes;

#[derive(Parser, Debug)]
#[clap(
    name = "vpd", about = env!("CARGO_PKG_DESCRIPTION"),
    group = ArgGroup::new("command").multiple(false).required(true)
)]
struct VpdArgs {
    /// sets timeout
    #[clap(
        long, short = 'T', default_value = "5000", value_name = "timeout_ms",
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

fn pack(piece: &Piece) -> Vec<u8> {
    match piece {
        Piece::Bytes(b) => b.to_vec(),
        Piece::Chunk(tag, body) => {
            let mut out = vec![];

            let mut header = tlvc::ChunkHeader {
                tag: (*tag).into(),
                len: 0.into(),
                header_checksum: 0.into(),
            };

            out.extend(header.as_bytes());

            let c = tlvc::begin_body_crc();
            let mut c = c.digest();
            for p in body {
                let segment = pack(p);
                c.update(&segment);
                out.extend(segment);
            }
            let body_len = out.len() - std::mem::size_of::<tlvc::ChunkHeader>();
            let body_len = u32::try_from(body_len).unwrap();
            while out.len() & 0b11 != 0 {
                out.push(0);
            }
            out.extend(c.finalize().to_le_bytes());

            // Update the header.
            header.len.set(body_len);
            header.header_checksum.set(header.compute_checksum());

            out[..std::mem::size_of::<tlvc::ChunkHeader>()]
                .copy_from_slice(header.as_bytes());
            out
        }
        Piece::String(s) => s.as_bytes().to_vec(),
    }
}

fn dump<R>(mut src: tlvc::TlvcReader<R>) -> Vec<Piece>
where
    R: tlvc::TlvcRead,
{
    let mut pieces = vec![];
    loop {
        match src.next() {
            Ok(Some(chunk)) => {
                let mut tmp = [0; 512];
                if chunk.check_body_checksum(&mut tmp).is_ok() {
                    pieces.push(Piece::Chunk(
                        tlvc_text::Tag::new(chunk.header().tag),
                        dump(chunk.read_as_chunks()),
                    ));
                } else {
                    let bytes = remaining_bytes(
                        src,
                        chunk.header().total_len_in_bytes(),
                    );

                    if let Ok(s) = std::str::from_utf8(&bytes) {
                        pieces.push(Piece::String(s.to_string()));
                    } else {
                        pieces.push(Piece::Bytes(bytes));
                    }

                    break;
                }
            }
            Ok(None) => break,
            Err(_) => {
                let bytes = remaining_bytes(src, 0);

                if let Ok(s) = std::str::from_utf8(&bytes) {
                    pieces.push(Piece::String(s.to_string()));
                } else {
                    pieces.push(Piece::Bytes(bytes));
                }

                break;
            }
        }
    }
    pieces
}

fn remaining_bytes<R>(src: tlvc::TlvcReader<R>, rewind: usize) -> Vec<u8>
where
    R: tlvc::TlvcRead,
{
    let (src, start, end) = src.into_inner();
    let start = start as usize - rewind;
    let mut bytes = vec![0; end as usize - start];
    src.read_exact(start as u64, &mut bytes).unwrap();
    bytes
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
    let op = idol::IdolOperation::new(hubris, "Vpd", "write", None)
        .context("is the 'vpd' task present?")?;
    let target = target(hubris, subargs)?;

    let mut bytes = vec![];

    if let Some(ref filename) = subargs.write {
        let file = fs::File::open(filename)?;

        let p = tlvc_text::load(file).with_context(|| {
            format!("failed to parse {} as VPD input", filename)
        })?;

        for piece in p {
            bytes.extend(pack(&piece));
        }
    } else {
        bytes.resize_with(1024, || 0xffu8);
    }

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
    let op = idol::IdolOperation::new(hubris, "Vpd", "read", None)
        .context("is the 'vpd' task present?")?;
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

    let p = dump(reader);
    tlvc_text::save(std::io::stdout(), &p)?;
    println!();

    Ok(())
}

fn vpd(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    subargs: &[String],
) -> Result<()> {
    let subargs = VpdArgs::try_parse_from(subargs)?;

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

pub fn init() -> (Command, ClapCommand<'static>) {
    (
        Command::Attached {
            name: "vpd",
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
            run: Run::Subargs(vpd),
        },
        VpdArgs::command(),
    )
}
