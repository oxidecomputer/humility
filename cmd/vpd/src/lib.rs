// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility vpd`
//!

use anyhow::{anyhow, bail, Context, Result};
use clap::Command as ClapCommand;
use clap::{CommandFactory, Parser};
use hif::*;
use humility::core::Core;
use humility::hubris::*;
use humility_cmd::hiffy::*;
use humility_cmd::idol;
use humility_cmd::{Archive, Attach, Command, Run, Validate};
use indicatif::{ProgressBar, ProgressStyle};
use std::fs;
use tlvc_text::Piece;
use zerocopy::AsBytes;

#[derive(Parser, Debug)]
#[clap(name = "vpd", about = env!("CARGO_PKG_DESCRIPTION"))]
struct VpdArgs {
    /// sets timeout
    #[clap(
        long, short = 'T', default_value = "5000", value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// list all devices that have VPD
    #[clap(long, short, conflicts_with_all = &["read", "write"])]
    list: bool,

    /// specify device by ID
    #[clap(long, short = 'i', value_name = "id", conflicts_with = "device")]
    id: Option<usize>,

    /// specifies a device by its description
    #[clap(long, short, value_name = "device")]
    device: Option<String>,

    #[clap(long, short, value_name = "filename", conflicts_with = "read")]
    write: Option<String>,

    #[clap(long, short)]
    read: bool,
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
    }
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
    let target = target(hubris, &subargs)?;

    let filename = subargs.write.as_ref().unwrap();
    let file = fs::File::open(filename)?;

    let p = tlvc_text::load(file).with_context(|| {
        format!("failed to parse {} as VPD input", filename)
    })?;

    let mut bytes = vec![];

    for piece in p {
        bytes.extend(pack(&piece));
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

    if nops <= 0 {
        bail!("text size is too small for a single write!");
    }

    let mut offset = 0;

    let bar = ProgressBar::new(bytes.len() as u64);

    bar.set_style(
        ProgressStyle::default_bar()
            .template("humility: writing VPD [{bar:30}] {bytes}/{total_bytes}"),
    );

    for chunk in all_ops.chunks(nops) {
        let mut ops =
            chunk.into_iter().flatten().map(|m| *m).collect::<Vec<Op>>();
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
    humility::msg!("successfully wrote {} bytes of VPD", offset);

    Ok(())
}

fn vpd_read(
    _hubris: &HubrisArchive,
    _core: &mut dyn Core,
    _subargs: &VpdArgs,
) -> Result<()> {
    todo!();
}

fn vpd(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    subargs: &[String],
) -> Result<()> {
    let subargs = VpdArgs::try_parse_from(subargs)?;

    if subargs.list {
        list(hubris)?;
    } else if subargs.write.is_some() {
        vpd_write(hubris, core, &subargs)?;
    } else if subargs.read {
        vpd_read(hubris, core, &subargs)?;
    } else {
        bail!("must specify either write (--write) or read (--read)");
    }

    Ok(())

    /*
    let ndx = find_target(

    for (offset, b) in bytes.iter().enumerate() {
        let payload = op.payload(&[
            ("index", idol::IdolArgument::Scalar(ndx as u64)),
            ("offset", idol::IdolArgument::Scalar(offset as u64)),
            ("contents", idol::IdolArgument::Scalar(b)),
        ]);

        context.idol_call_ops(&funcs, &op, &payload, &mut ops)?;
    }

    println!("{:#x?}", ops);
    println!("{} ops", ops.len());

    ops.push(Op::Done);

    let results = context.run(core, ops.as_slice(), None)?;

    let fmt = HubrisPrintFormat {
        newline: false,
        hex: true,
        ..HubrisPrintFormat::default()
    };

    println!(
        "{:2} {:11} {:>2} {:2} {:3} {:4} {:13} DESCRIPTION",
        "ID", "VALIDATION", "C", "P", "MUX", "ADDR", "DEVICE"
    );

    let ok = hubris.lookup_enum(op.ok)?;

    for (rndx, (ndx, device)) in devices.iter().enumerate() {
        let result = match &results[rndx] {
            Ok(val) => {
                if let Some(variant) = ok.lookup_variant(val[0].into()) {
                    match variant.name.as_str() {
                        "Present" => "present".yellow(),
                        "Validated" => "validated".green(),
                        _ => format!("<{}>", variant.name).cyan(),
                    }
                } else {
                    hubris.printfmt(val, op.ok, fmt)?.white()
                }
            }
            Err(e) => match op.error.unwrap().lookup_variant(*e as u64) {
                Some(variant) => match variant.name.as_str() {
                    "NotPresent" => {
                        if device.removable {
                            "removed".blue()
                        } else {
                            "absent".red()
                        }
                    }
                    "BadValidation" => "failed".red(),
                    "DeviceTimeout" => "timeout".red(),
                    "DeviceError" => "error".red(),
                    _ => format!("<{}>", variant.name).red(),
                },
                None => format!("Err(0x{:x?})", e).red(),
            },
        };

        let mux = match (device.mux, device.segment) {
            (Some(m), Some(s)) => format!("{}:{}", m, s),
            (None, None) => "-".to_string(),
            (_, _) => "?:?".to_string(),
        };

        println!(
            "{:2} {:11} {:2} {:2} {:3} 0x{:02x} {:13} {}",
            ndx,
            result,
            device.controller,
            device.port.name,
            mux,
            device.address,
            device.device,
            device.description
        );
    }


    Ok(())
    */
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
