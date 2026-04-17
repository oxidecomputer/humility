// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility ereport`
//! `humility ereport` lets you examine the ereport buffer in an image.

use humility::reflect::Value;
use humility::{
    core::Core,
    hubris::{HubrisArchive, HubrisTask},
};
use humility_cli::{ExecutionContext, Subcommand};
use humility_cmd::{Archive, Attach, Command, CommandKind, Validate};

use anyhow::{Context, Result, anyhow};
use std::fmt::Write;
use zerocopy::FromBytes;

use clap::{CommandFactory, Parser};

#[derive(Parser, Debug)]
#[clap(name = "ereport", about = env!("CARGO_PKG_DESCRIPTION"))]
struct EreportArgs {
    #[clap(subcommand)]
    cmd: EreportCmd,
}

#[derive(Parser, Debug)]
enum EreportCmd {
    /// Print the contents of the ereport buffer
    Dump {
        #[clap(short, long)]
        verbose: bool,
    },
}

/// Reasons a server might cite when using the `REPLY_FAULT` syscall.
#[derive(Debug, FromBytes)]
#[repr(C)]
pub struct EreportHeader {
    data_len: zerocopy::byteorder::little_endian::U16,
    task: zerocopy::byteorder::little_endian::U16,
    timestamp: zerocopy::byteorder::little_endian::U64,
}

fn ereport_dump(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    verbose: bool,
) -> Result<()> {
    static PACKRAT_BUF_NAME: &str = "task_packrat::main::BUFS";
    let buf_ty = hubris
        .lookup_qualified_variable(PACKRAT_BUF_NAME)
        .with_context(|| format!("could not find `{PACKRAT_BUF_NAME}`"))?;

    let buf: Value = humility::reflect::load_variable(hubris, core, buf_ty)?;
    let storage: Value =
        buf.field("cell.value.ereport_bufs.storage.storage")?;
    let array: Vec<humility_doppel::MaybeUninit<u8>> =
        storage.field("buffer")?;
    let front = storage.field::<u32>("front")? as usize;
    let back = storage.field::<u32>("back")? as usize;

    if verbose {
        println!(
            "buffer has {} bytes; front: {front}, back: {back}",
            array.len()
        );
    }

    // Read data from the circular buffer
    let mut buf_data = Vec::with_capacity(array.len());
    let mut i = front;
    while i != back {
        buf_data.push(array[i].value);
        i = (i + 1) % array.len();
    }
    let mut buf_data = buf_data.as_slice();
    while !buf_data.is_empty() {
        let header =
            EreportHeader::read_from_prefix(buf_data).ok_or_else(|| {
                anyhow!("could not get ereport header from buffer")
            })?;
        buf_data = &buf_data[std::mem::size_of::<EreportHeader>()..];
        let (ereport_data, next) =
            buf_data.split_at(usize::from(header.data_len));
        let task = hubris.lookup_module(HubrisTask::Task(header.task.into()));
        println!(
            "task: {} ({})",
            task.map(|t| t.name.as_str()).unwrap_or("<unknown>"),
            header.task
        );
        println!("timestamp: {}", header.timestamp);
        if verbose {
            println!("  {header:?}, {ereport_data:x?}");
        }
        println!("{}", pretty_print_cbor(ereport_data)?);
        buf_data = next;
    }

    Ok(())
}

fn pretty_print_cbor(
    data: &[u8],
) -> Result<String, ciborium::de::Error<std::io::Error>> {
    let value: ciborium::Value = ciborium::from_reader(data)?;
    let mut output = String::new();
    pretty_print_value(&value, &mut output, 0, false);
    Ok(output)
}

fn pretty_print_value(
    value: &ciborium::Value,
    out: &mut String,
    indent: usize,
    is_key: bool,
) {
    let pad = "  ".repeat(indent);

    match value {
        ciborium::Value::Null => write!(out, "null").unwrap(),
        ciborium::Value::Bool(b) => write!(out, "{b}").unwrap(),
        ciborium::Value::Integer(n) => {
            // ciborium::value::Integer can be converted to i128
            let n: i128 = (*n).into();
            write!(out, "{n}").unwrap();
        }
        ciborium::Value::Float(f) => write!(out, "{f}").unwrap(),
        ciborium::Value::Text(s) => {
            if is_key {
                write!(out, "{s}").unwrap()
            } else {
                write!(out, "\"{s}\"").unwrap()
            }
        }
        ciborium::Value::Bytes(b) => {
            write!(out, "h'{}'", hex::encode(b)).unwrap()
        }

        ciborium::Value::Array(items) => {
            if items.is_empty() {
                write!(out, "[]").unwrap();
                return;
            }
            writeln!(out, "[").unwrap();
            for (i, item) in items.iter().enumerate() {
                write!(out, "{pad}  ").unwrap();
                pretty_print_value(item, out, indent + 1, false);
                if i + 1 < items.len() {
                    writeln!(out, ",").unwrap();
                } else {
                    writeln!(out).unwrap();
                }
            }
            write!(out, "{pad}]").unwrap();
        }

        ciborium::Value::Map(entries) => {
            if entries.is_empty() {
                write!(out, "{pad}{{}}").unwrap();
                return;
            }
            if indent > 0 {
                writeln!(out, "{pad}{{").unwrap();
            }
            for (i, (k, v)) in entries.iter().enumerate() {
                write!(out, "{pad}  ").unwrap();
                pretty_print_value(k, out, indent + 1, true);
                write!(out, ": ").unwrap();
                // Special-case handling for the `k`, which is an ereport name
                // and is printed without quotes
                let is_ereport_key = match k {
                    ciborium::Value::Text(t) => t == "k",
                    _ => false,
                };
                pretty_print_value(v, out, indent + 1, is_ereport_key);
                if i + 1 < entries.len() {
                    writeln!(out, ",").unwrap();
                } else {
                    writeln!(out).unwrap();
                }
            }
            if indent > 0 {
                write!(out, "{pad}}}").unwrap();
            }
        }

        ciborium::Value::Tag(tag, inner) => {
            write!(out, "tag({tag}) ").unwrap();
            pretty_print_value(inner, out, indent, false);
        }

        _ => write!(out, "<unknown>").unwrap(),
    }
}

fn ereport(context: &mut ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let hubris = context.archive.as_ref().unwrap();

    let subargs = EreportArgs::try_parse_from(subargs)?;

    match subargs.cmd {
        EreportCmd::Dump { verbose } => ereport_dump(hubris, core, verbose),
    }
}

pub fn init() -> Command {
    Command {
        app: EreportArgs::command(),
        name: "ereport",
        run: ereport,
        kind: CommandKind::Attached {
            archive: Archive::Required,
            attach: Attach::Any,
            validate: Validate::Booted,
        },
    }
}
