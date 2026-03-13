// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility cosmo`
//! Hello there

use anyhow::{Context, Result, bail};
use clap::{CommandFactory, Parser};
use humility::core::Core;
use humility::hubris::*;
use humility::reflect::{self, Base, Format, Load, Value};
use humility_cli::{ExecutionContext, Subcommand};
use humility_cmd::{Archive, Attach, Command, CommandKind, Validate};
use humility_doppel::{CountedRingbuf, Ringbuf, StaticCell};
use std::collections::HashMap;

#[derive(Parser, Debug)]
#[clap(name = "cosmo", about = env!("CARGO_PKG_DESCRIPTION"))]
struct CosmoArgs {
    #[clap(subcommand)]
    cmd: CosmoSubcommand,
}

#[derive(Parser, Debug)]
enum CosmoSubcommand {
    /// Prints the most recent POST code
    Sequencer {
        #[clap(subcommand)]
        cmd: SequencerCommand,
    },
}

#[derive(Parser, Debug)]
enum SequencerCommand {
    Diagnose,
}

fn cosmo(context: &mut ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let hubris = context.archive.as_ref().unwrap();

    let subargs = CosmoArgs::try_parse_from(subargs)?;
    match subargs.cmd {
        CosmoSubcommand::Sequencer { cmd: SequencerCommand::Diagnose } => {
            cosmo_seq_diagnose(hubris, core)
        }
    }
}

fn cosmo_seq_diagnose(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
) -> Result<()> {
    const DIAGNOSE_RINGBUF: &str = "drv_cosmo_seq_server::diagnose::__RINGBUF";
    const RAW_RINGBUF: &str = "drv_cosmo_seq_server::diagnose::RAW";

    let mut diagnose_ringbuf = None;
    let mut raw_ringbuf = None;
    for v in hubris.qualified_variables() {
        let def = match hubris.lookup_struct(v.1.goff) {
            Ok(s) => {
                // Skip variables whose type does not indicate they contain a
                // ringbuf; this check is imprecise but probably good enough
                if s.name.contains("Ringbuf") {
                    s
                } else {
                    continue;
                }
            }
            Err(_) => continue,
        };

        if v.0 == DIAGNOSE_RINGBUF {
            diagnose_ringbuf = Some((v.1, def));
        }
        if v.0 == RAW_RINGBUF {
            raw_ringbuf = Some((v.1, def));
        }
    }

    let mut read_var = |var: &HubrisVariable, def| -> Result<Value> {
        let mut buf: Vec<u8> = vec![];
        buf.resize_with(var.size, Default::default);

        core.halt()?;
        core.read_8(var.addr, buf.as_mut_slice())?;
        core.run()?;

        let val: Value =
            Value::Struct(reflect::load_struct(hubris, &buf, def, 0)?);
        Ok(val)
    };

    let mut diagnose_ts = None;
    if let Some((var, def)) = diagnose_ringbuf {
        let val = read_var(var, def)?;
        match print_counted_ringbuf(hubris, &val) {
            Ok(v) => diagnose_ts = v,
            Err(e) => {
                humility::warn!("error when processing diagnose ringbuf: {e:#}")
            }
        }
    } else {
        humility::warn!("can't find diagnosis ringbuf");
    }
    if let Some((var, def)) = raw_ringbuf {
        let val = read_var(var, def)?;
        if let Err(e) = print_raw_registers(&val, diagnose_ts) {
            humility::warn!("error when processing diagnose ringbuf: {e:#}")
        }
    } else {
        humility::warn!("Can't find raw register ringbuf");
    }

    Ok(())
}

fn print_counted_ringbuf(
    hubris: &HubrisArchive,
    val: &Value,
) -> Result<Option<u64>> {
    let r = CountedRingbuf::from_value(val)
        .context("can't decode value as CountedRingbuf")?;
    let Some(r) = r.ringbuf else {
        bail!("ringbuf member is empty");
    };

    let mut diagnose_ts = None;
    let Some(ndx) = r.last else {
        bail!("nothing in ringbuf");
    };

    let entry = &r.buffer[ndx as usize];
    let Value::Enum(e) = &entry.payload else {
        bail!("payload is not an enum");
    };
    let Some(Value::Struct(d)) = e.contents() else {
        bail!("payload contents is not a struct");
    };

    println!("Diagnosis details:");
    if let Some(t) = d.get("now")
        && let Value::Base(Base::U64(t)) = t
    {
        diagnose_ts = Some(*t);
        println!("  timestamp: {t} (ms since boot)");
    } else {
        humility::warn!("  can't get timestamp");
    }

    if let Some(d) = d.get("details") {
        let mut dumped = vec![];
        let fmt = HubrisPrintFormat::default();
        d.format(hubris, fmt, &mut dumped)?;
        let dumped = String::from_utf8(dumped)?;
        println!("  details: {dumped}");
    } else {
        humility::warn!("  can't get details");
    }

    Ok(diagnose_ts)
}

fn print_raw_registers(val: &Value, diagnose_ts: Option<u64>) -> Result<()> {
    let cell = StaticCell::from_value(val)
        .context("could not decode value as StaticCell")?;
    let r = Ringbuf::from_value(&cell.cell.value)
        .context("could not decode cell as Ringbuf")?;
    let Some(ndx) = r.last else {
        bail!("raw register ringbuf is empty");
    };

    println!("\nRaw register values:");
    let entry = &r.buffer[ndx as usize];
    let Value::Enum(e) = &entry.payload else {
        bail!("can't get ringbuf entry payload as Enum");
    };
    let Some(Value::Struct(d)) = e.contents() else {
        bail!("can't get ringbuf entry payload enum as Struct");
    };
    if let Some(t) = d.get("now")
        && let Value::Base(Base::U64(t)) = t
    {
        if let Some(prev) = diagnose_ts
            && *t != prev
        {
            humility::warn!(
                "raw register readings have a different timestamp \
                 than diagnosis ringbuf ({t} != {prev})"
            );
        }
        println!("  timestamp: {t} (ms since boot)");
    } else {
        humility::warn!("  can't get timestamp");
    }
    if let Some(d) = d.get("values")
        && let Value::Struct(d) = d
    {
        let get_tuple_name = |name, field| {
            if let Some(Value::Struct(s)) = d.get(name)
                && let Some(Value::Enum(e)) = s.get(field)
                && let Some(Value::Tuple(t)) = e.contents()
                && let Some(Value::Enum(t)) = t.first()
            {
                Some(t.disc())
            } else {
                None
            }
        };
        let get_table = |name: &str| {
            let mut out = Vec::new();
            if let Some(Value::Struct(s)) = d.get(name) {
                for (field, m) in s.iter() {
                    if let Value::Base(Base::Bool(b)) = m {
                        out.push((field, *b));
                    } else {
                        humility::warn!("  can't get {name}.{field}");
                    }
                }
            } else {
                humility::warn!("  can't get {name}");
            }
            out
        };
        let print_table = |name: &str| {
            let out = get_table(name);
            println!("  {name}");
            for (field, b) in out {
                println!("  | {field:>30} | {b:>5} |",);
            }
        };

        // Known / expected names
        const SEQ_API_STATUS_NAME: &str = "seq_api_status";
        const SEQ_RAW_STATUS_NAME: &str = "seq_raw_status";

        if let Some(s) = get_tuple_name(SEQ_API_STATUS_NAME, "a0_sm") {
            println!("  A0 seq state: {s}");
        } else {
            humility::warn!("  can't get a0_sm");
        }
        if let Some(s) = get_tuple_name(SEQ_RAW_STATUS_NAME, "hw_sm") {
            println!("  A0 raw state: {s}");
        } else {
            humility::warn!("  can't get hw_sm");
        }
        let rail_enables = get_table("rail_enables");
        let rail_pgs =
            get_table("rail_pgs").into_iter().collect::<HashMap<_, _>>();
        let rail_pgs_max_hold = get_table("rail_pgs_max_hold")
            .into_iter()
            .collect::<HashMap<_, _>>();
        let field_names = d.iter().map(|(name, _v)| name).filter(|s| {
            *s != SEQ_API_STATUS_NAME
                && *s != SEQ_RAW_STATUS_NAME
                && !s.starts_with("rail_")
        });
        for f in field_names {
            print_table(f);
            println!("  |-----------------------------------------");
        }
        println!("\n  Rail state");
        println!(
            "{:>20} | {:>8} | {:>10} | {:>8}",
            "rail", "enable", "power good", "max hold",
        );
        println!("---------------------|----------|------------|-----------");
        for (name, enable) in rail_enables.iter() {
            let pg = rail_pgs
                .get(*name)
                .map(|b| format!("{b}"))
                .unwrap_or_else(|| "???".to_owned());
            let pg_max_hold = rail_pgs_max_hold
                .get(*name)
                .map(|b| format!("{b}"))
                .unwrap_or_else(|| "???".to_owned());
            println!("{name:>20} | {enable:>8} | {pg:>10} | {pg_max_hold:>8}");
        }
    }
    Ok(())
}

pub fn init() -> Command {
    Command {
        app: CosmoArgs::command(),
        name: "cosmo",
        run: cosmo,
        kind: CommandKind::Attached {
            archive: Archive::Required,
            attach: Attach::Any,
            validate: Validate::Match,
        },
    }
}
