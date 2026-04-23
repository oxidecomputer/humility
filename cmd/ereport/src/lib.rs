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
    /// Print ereports in JSON format
    #[clap(short, long)]
    json: bool,

    /// Recover previously-sent ereports from the buffer
    ///
    /// The circular buffer may contain previously-sent ereports outside of its
    /// valid range, i.e. before the `front` index.  When this flag is set, we
    /// examine those bytes to find valid-looking ereports and print them (with
    /// `recovered: true`).
    #[clap(short, long)]
    recover: bool,
}

/// Raw ereport header
///
/// This must agree with the SP's implementation in `snitch_core::Store`
#[derive(Debug, FromBytes)]
#[repr(C)]
struct EreportHeader {
    data_len: zerocopy::byteorder::little_endian::U16,
    task: zerocopy::byteorder::little_endian::U16,
    timestamp: zerocopy::byteorder::little_endian::U64,
}

const HEADER_SIZE: usize = std::mem::size_of::<EreportHeader>();

#[derive(serde::Serialize)]
pub struct Ereport {
    task_name: Option<String>,
    task_index: u16,
    timestamp: u64,
    recovered: bool,
    ena: u64,
    contents: ciborium::Value,
}

/// Read ereports from the given system
///
/// If `recover` is `true`, then we attempt to recover ereports from outside the
/// valid range of the circular buffer; ereports which are recovered have
/// [`Ereport::recovered`] set to `true`.
pub fn ereport_dump(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    recover: bool,
) -> Result<Vec<Ereport>> {
    static PACKRAT_BUF_NAME: &str = "task_packrat::main::BUFS";
    let buf_ty = hubris
        .lookup_qualified_variable(PACKRAT_BUF_NAME)
        .with_context(|| format!("could not find `{PACKRAT_BUF_NAME}`"))?;

    let buf: Value = humility::reflect::read_variable(hubris, core, buf_ty)?;
    let outer_storage: Value = buf.field("cell.value.ereport_bufs.storage")?;
    let inner_storage: Value = outer_storage.field("storage")?;
    let array: Vec<humility_doppel::MaybeUninit<u8>> =
        inner_storage.field("buffer")?;
    let front = inner_storage.field::<u32>("front")? as usize;
    let back = inner_storage.field::<u32>("back")? as usize;
    let earliest_ena = outer_storage.field::<u64>("earliest_ena")?;
    let insert_state =
        outer_storage.field::<humility::reflect::Enum>("insert_state")?;
    if insert_state.disc() != "Collecting" {
        humility::warn!(
            "ereports are being lost; `insert_state` is {insert_state:#?}"
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
    let mut current_ena = earliest_ena;
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
            ena: current_ena,
            contents,
        });
        buf_data = next;
        let Some(next_ena) = current_ena.checked_add(1) else {
            humility::warn!("ena overflow; breaking out of loop");
            break;
        };
        current_ena = next_ena;
    }

    if recover {
        let mut recovered = vec![];
        let mut buf = VecDeque::new();
        let mut current_ena = earliest_ena;
        let mut i = front;
        while i != back {
            i = i.checked_sub(1).unwrap_or_else(|| array.len() - 1);
            buf.push_front(array[i].value);
            if let Some(e) =
                try_recover_ereport(hubris, &mut buf, &mut current_ena)
            {
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

    // Go through and substitute "k" -> "class" and "v" -> "version"
    for e in &mut ereports {
        let Some(m) = e.contents.as_map_mut() else {
            continue;
        };
        for (k, _v) in m.iter_mut() {
            if let Some(s) = k.as_text_mut() {
                if s == "k" {
                    *s = "class".to_owned()
                } else if s == "v" {
                    *s = "version".to_owned()
                }
            }
        }
    }

    Ok(ereports)
}

fn ereport_print(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    flags: Flags,
) -> Result<()> {
    let ereports = ereport_dump(hubris, core, flags.recover)?;
    if flags.json {
        println!("{}", serde_json::ser::to_string_pretty(&ereports)?);
    } else {
        for (i, e) in ereports.iter().enumerate() {
            if i > 0 {
                println!();
            }
            println!(
                "task:      {} ({})",
                e.task_name.as_deref().unwrap_or("<unknown>"),
                e.task_index,
            );
            println!("timestamp: {}", e.timestamp);
            println!("ena:       {}", e.ena);
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
    ena: &mut u64,
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

    // Step the ENA backwards; the ereport is invalid if it wraps or hits 0
    *ena = ena.checked_sub(1)?;
    if *ena == 0 {
        return None;
    }

    Some(Ereport {
        task_name: Some(task.name.clone()),
        task_index: header.task.into(),
        timestamp: header.timestamp.into(),
        recovered: true,
        ena: *ena,
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
                // Special-case handling for the class, which is an ereport name
                // and is printed without quotes
                let is_ereport_key = match k {
                    ciborium::Value::Text(t) => t == "class",
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
        EreportCmd::Dump { flags } => ereport_print(hubris, core, flags),
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
