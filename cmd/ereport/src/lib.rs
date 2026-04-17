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
use std::collections::VecDeque;
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
        #[clap(flatten)]
        flags: Flags,
    },
}

#[derive(Parser, Debug)]
struct Flags {
    /// Print verbose info while processing ereports
    #[clap(short, long, conflicts_with = "json")]
    verbose: bool,
    /// Print ereports in JSON format
    #[clap(short, long)]
    json: bool,
    /// Recover previously-sent ereports from the buffer
    #[clap(short, long)]
    recover: bool,
}

/// Reasons a server might cite when using the `REPLY_FAULT` syscall.
#[derive(Debug, FromBytes)]
#[repr(C)]
struct EreportHeader {
    data_len: zerocopy::byteorder::little_endian::U16,
    task: zerocopy::byteorder::little_endian::U16,
    timestamp: zerocopy::byteorder::little_endian::U64,
}

const HEADER_SIZE: usize = std::mem::size_of::<EreportHeader>();

#[derive(serde::Serialize)]
struct Ereport {
    task_name: Option<String>,
    task_index: u16,
    timestamp: u64,
    recovered: bool,
    contents: ciborium::Value,
}

fn ereport_dump(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    flags: Flags,
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

    if flags.verbose {
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
    let mut ereports = vec![];
    let mut buf_data = buf_data.as_slice();
    while !buf_data.is_empty() {
        let header =
            EreportHeader::read_from_prefix(buf_data).ok_or_else(|| {
                anyhow!("could not get ereport header from buffer")
            })?;
        buf_data = &buf_data[HEADER_SIZE..];
        let (ereport_data, next) =
            buf_data.split_at(usize::from(header.data_len));
        let task = hubris.lookup_module(HubrisTask::Task(header.task.into()));
        let contents = ciborium::from_reader(ereport_data)?;
        ereports.push(Ereport {
            task_name: task.map(|t| t.name.clone()).ok(),
            task_index: header.task.into(),
            timestamp: header.timestamp.into(),
            recovered: false,
            contents,
        });
        buf_data = next;
    }

    if flags.recover {
        let mut recovered = vec![];
        let mut buf = VecDeque::new();
        let mut i = front;
        while i != back {
            i = i.checked_sub(1).unwrap_or_else(|| array.len() - 1);
            buf.push_front(array[i].value);
            if let Some(e) = try_recover_ereport(hubris, &mut buf) {
                assert!(e.recovered);
                recovered.push(e);
                buf.clear();
            }
        }
        // ereports are recovered in reverse order (since we iterate backwards
        // through the buffer), so we'll reverse their order then prepend them
        // to the list of normal ereports.
        recovered.reverse();
        std::mem::swap(&mut ereports, &mut recovered);
        ereports.extend(recovered);
    }

    if flags.json {
        println!("{}", serde_json::ser::to_string_pretty(&ereports)?);
    } else {
        for (i, e) in ereports.iter().enumerate() {
            if i > 0 {
                println!();
            }
            println!(
                "task: {} ({})",
                e.task_name.as_deref().unwrap_or("<unknown>"),
                e.task_index,
            );
            println!("timestamp: {}", e.timestamp);
            if e.recovered || flags.recover {
                println!("recovered: {}", e.recovered);
            }
            pretty_print_value(&e.contents, 0, false);
        }
    }

    Ok(())
}

/// Tries to recover an ereport from the given buffer
fn try_recover_ereport(
    hubris: &HubrisArchive,
    buf: &mut VecDeque<u8>,
) -> Option<Ereport> {
    // Cheapest possible check: we must have enough room for the header and at
    // least one byte of payload.
    if buf.len() <= HEADER_SIZE {
        return None;
    }
    // Second-cheapest check: the header must imply the correct size.  We do
    // this manually to avoid needing a contiguous buffer, which is a more
    // expensive operation (shuffling data around).
    let data_len = u16::from_le_bytes([buf[0], buf[1]]);
    if data_len == 0 || usize::from(data_len) + HEADER_SIZE != buf.len() {
        return None;
    }
    let buf = &*buf.make_contiguous();

    let header = EreportHeader::read_from_prefix(buf).unwrap(); // size checked
    let task =
        hubris.lookup_module(HubrisTask::Task(header.task.into())).ok()?;
    let contents = ciborium::from_reader(&buf[HEADER_SIZE..]).ok()?;
    Some(Ereport {
        task_name: Some(task.name.clone()),
        task_index: header.task.into(),
        timestamp: header.timestamp.into(),
        recovered: true,
        contents,
    })
}

/// Recursively pretty-print a CBOR value
fn pretty_print_value(value: &ciborium::Value, indent: usize, is_key: bool) {
    let pad = "  ".repeat(indent);

    match value {
        ciborium::Value::Null => print!("null"),
        ciborium::Value::Bool(b) => print!("{b}"),
        ciborium::Value::Integer(n) => {
            // ciborium::value::Integer can be converted to i128
            let n: i128 = (*n).into();
            print!("{n}");
        }
        ciborium::Value::Float(f) => print!("{f}"),
        ciborium::Value::Text(s) => {
            if is_key {
                print!("{s}")
            } else {
                print!("\"{s}\"")
            }
        }
        ciborium::Value::Bytes(b) => {
            print!("h'{}'", hex::encode(b))
        }

        ciborium::Value::Array(items) => {
            if items.is_empty() {
                print!("[]");
                return;
            }
            println!("[");
            for (i, item) in items.iter().enumerate() {
                print!("{pad}  ");
                pretty_print_value(item, indent + 1, false);
                if i + 1 < items.len() {
                    println!(",");
                } else {
                    println!();
                }
            }
            print!("{pad}]");
        }

        ciborium::Value::Map(entries) => {
            if entries.is_empty() {
                print!("{pad}{{}}");
                return;
            }
            if indent > 0 {
                println!("{pad}{{");
            }
            for (i, (k, v)) in entries.iter().enumerate() {
                print!("{pad}  ");
                pretty_print_value(k, indent + 1, true);
                print!(": ");
                // Special-case handling for the `k`, which is an ereport name
                // and is printed without quotes
                let is_ereport_key = match k {
                    ciborium::Value::Text(t) => t == "k",
                    _ => false,
                };
                pretty_print_value(v, indent + 1, is_ereport_key);
                if i + 1 < entries.len() {
                    println!(",");
                } else {
                    println!();
                }
            }
            if indent > 0 {
                print!("{pad}}}");
            }
        }

        ciborium::Value::Tag(tag, inner) => {
            print!("tag({tag}) ");
            pretty_print_value(inner, indent, false);
        }

        _ => print!("<unknown>"),
    }
}

fn ereport(context: &mut ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let hubris = context.archive.as_ref().unwrap();

    let subargs = EreportArgs::try_parse_from(subargs)?;

    match subargs.cmd {
        EreportCmd::Dump { flags } => ereport_dump(hubris, core, flags),
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
