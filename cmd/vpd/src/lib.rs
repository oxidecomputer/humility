// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility vpd`
//!

use anyhow::{Context, Result};
use clap::Command as ClapCommand;
use clap::{CommandFactory, Parser};
use hif::*;
use humility::core::Core;
use humility::hubris::*;
use humility_cmd::hiffy::*;
use humility_cmd::i2c::I2cArgs;
use humility_cmd::idol;
use humility_cmd::{Archive, Attach, Command, Run, Validate};
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

    #[clap(long, short, value_name = "filename")]
    read: bool,
}

fn is_vpd(device: &HubrisI2cDevice) -> bool {
    device.device == "at24csw080"
}

fn list(hubris: &HubrisArchive) -> Result<()> {
    println!(
        "{:2} {:>2} {:2} {:3} {:4} {:13} DESCRIPTION",
        "ID", "C", "P", "MUX", "ADDR", "DEVICE"
    );

    for (ndx, device) in hubris.manifest.i2c_devices.iter().enumerate() {
        if !is_vpd(device) {
            continue;
        }

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

fn vpd(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    subargs: &[String],
) -> Result<()> {
    let subargs = VpdArgs::try_parse_from(subargs)?;

    /*
        let hargs = if subargs.bus.is_some() || subargs.controller.is_some() {
            Some(I2cArgs::parse(
                hubris,
                &subargs.bus,
                subargs.controller,
                &subargs.port,
                &subargs.mux,
                &None,
            )?)
        } else {
            None
        };
    */

    if subargs.list {
        list(hubris)?;
        return Ok(());
    }

    if let Some(ref file) = subargs.write {
        let file = fs::File::open(file)?;

        let p = tlvc_text::load(file)?;

        let mut bytes = vec![];
        for piece in p {
            bytes.extend(pack(&piece));
        }

        println!("{:x?}", bytes);
        return Ok(());
    }

    /*
    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;
    let funcs = context.functions()?;
    let op = idol::IdolOperation::new(hubris, "Validate", "validate_i2c", None)
        .context("is the 'validate' task present?")?;
    let mut ops = vec![];

    let mut devices = vec![];

    for (ndx, device) in hubris.manifest.i2c_devices.iter().enumerate() {
        if let Some(id) = subargs.id {
            if ndx != id {
                continue;
            }
        }

        if let Some(ref hargs) = hargs {
            if !hargs.matches_device(device) {
                continue;
            }
        } else if let Some(ref d) = subargs.device {
            if device.device != *d {
                continue;
            }
        }

        devices.push((ndx, device));

        let payload =
            op.payload(&[("index", idol::IdolArgument::Scalar(ndx as u64))])?;
        context.idol_call_ops(&funcs, &op, &payload, &mut ops)?;
    }

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

    */

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
