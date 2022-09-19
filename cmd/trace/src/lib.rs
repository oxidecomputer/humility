// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::{anyhow, Result};
use clap::{CommandFactory, Parser};
use humility::cli::Subcommand;
use humility::core::Core;
use humility::hubris::*;
use humility_cmd::{Archive, Attach, Command, CommandKind, Validate};
use humility_cortex::itm::*;
use std::collections::HashMap;
use std::convert::TryInto;
use std::time::Instant;
use std::time::SystemTime;

#[derive(Parser, Debug)]
#[clap(name = "trace", about = env!("CARGO_PKG_DESCRIPTION"))]
struct TraceArgs {
    /// provide statemap-ready output
    #[clap(long, short)]
    statemap: bool,
}

#[rustfmt::skip::macros(println)]

fn tracecmd_ingest(
    core: &mut dyn Core,
    subargs: &TraceArgs,
    hubris: &HubrisArchive,
    tasks: &HashMap<u32, String>,
    traceid: Option<u8>,
) -> Result<()> {
    let mut bytes: Vec<u8> = vec![];
    let mut ndx = 0;

    let start = Instant::now();
    let mut ts: f64 = 0.0;
    let mut time = 0;

    let mut states: HashMap<String, i32> = HashMap::new();

    if subargs.statemap {
        let t = SystemTime::now().duration_since(SystemTime::UNIX_EPOCH)?;

        let colors = [
            "#ed441f", "#ef5b3b", "#f27357", "#f48a73", "#f6a28f", "#f8b9ab",
            "#fad0c7", "#fde8e3",
        ];

        println!("{{");
        println!("\t\"start\": [ {}, {} ],", t.as_secs(), t.subsec_nanos());
        println!("\t\"title\": \"Hubris tasks\",");
        println!("\t\"entityKind\": \"Task\",");

        println!("\t\"states\": {{");
        println!("\t\t\"Running\": \
            {{ \"value\": 0, \"color\": \"#DAF7A6\" }},");
        println!("\t\t\"Runnable\": \
            {{ \"value\": 1, \"color\": \"#9BC362\" }},");
        println!("\t\t\"InRecv\": \
            {{ \"value\": 2, \"color\": \"#e0e0e0\" }},");

        states.insert("Runnable".to_string(), 1);
        states.insert("InRecv(None)".to_string(), 2);

        // TODO: this algorithm is broken for images with more than eight tasks,
        // which makes using iterators hard.
        #[allow(clippy::needless_range_loop)]
        for i in 0..tasks.len() {
            let name = tasks.get(&(i as u32)).unwrap();
            let s = format!("InReply(({}))", i);
            let state = 3 + i;

            states.insert(s, state as i32);
            println!("\t\t\"InReply({})\": {{ \"value\": {}, \
                \"color\": \"{}\" }}{}", name, state, colors[i],
                if i < tasks.len() - 1 { "," } else { "" });
        }

        println!("\t}}");
        println!("}}");

        for i in 0..tasks.len() {
            let name = tasks.get(&(i as u32)).unwrap();
            println!("{{ \"entity\": \"{}\", \"description\": \"{}\" }}",
                i, name);
        }
    }

    let tstruct = hubris.lookup_struct_byname("Task")?;
    let state = tstruct.lookup_member("state")?;
    let state_enum = hubris.lookup_enum(state.goff)?;
    let healthy = state_enum.lookup_variant_byname("Healthy")?;
    let hh = hubris.lookup_struct(
        healthy.goff.ok_or_else(|| anyhow!("incomplete Healthy structure"))?,
    )?;

    let schedstate = hubris.lookup_enum(hh.lookup_member("__0")?.goff)?;
    let mut spayload = Vec::with_capacity(schedstate.size);
    let mut task = 0;
    let mut newtask = None;

    itm_ingest(
        traceid,
        || {
            while ndx == bytes.len() {
                bytes = core.read_swv().unwrap();
                ts = start.elapsed().as_secs_f64();
                ndx = 0;
            }
            ndx += 1;
            Ok(Some((bytes[ndx - 1], ts)))
        },
        |packet| {
            match &packet.payload {
                ITMPayload::Instrumentation { payload, port } => {
                    if *port == 30 {
                        newtask = Some(payload[0] as u32);
                        return Ok(());
                    }

                    if *port == 31 {
                        if payload.len() == 1 {
                            task = payload[0] as u32;
                            spayload.truncate(0);
                        } else {
                            for p in payload {
                                spayload.push(*p);
                            }
                        }

                        if spayload.len() < schedstate.size {
                            return Ok(());
                        }

                        if !subargs.statemap {
                            println!(
                            "{:.9} {} ({}): {}",
                            time as f64 / 16_000_000_f64,
                            task,
                            tasks.get(&task).unwrap_or(&"<invalid>".to_string()),
                            hubris.print(&spayload[..], schedstate.goff)?,
                        );
                            return Ok(());
                        }

                        let state =
                            hubris.print(&spayload[..], schedstate.goff)?;

                        println!("{{ \"time\": \"{}\", \"entity\": \"{}\", \
                        \"state\": {} }}",
                        ((time as f64 / 16_000_000_f64) *
                        1_000_000_000_f64) as u64,
                        task, states.get(&state).unwrap_or(&-1)
                    );

                        return Ok(());
                    }

                    if !subargs.statemap {
                        for p in payload {
                            print!("{}", *p as char);
                        }
                    }
                }

                ITMPayload::LocalTimestamp {
                    timedelta,
                    delayed: _,
                    early: _,
                } => {
                    time += timedelta;

                    if let Some(task) = newtask {
                        if subargs.statemap {
                            println!("{{ \"time\": \"{}\", \"entity\": \"{}\", \
                            \"state\": 0 }}",
                            ((time as f64 / 16_000_000_f64) *
                            1_000_000_000_f64) as u64,
                            task
                        );
                        } else {
                            println!(
                            "{:.9} {} ({}): Running",
                            time as f64 / 16_000_000_f64,
                            task,
                            tasks.get(&task).unwrap_or(&"<invalid>".to_string())
                        );
                        }

                        newtask = None;
                    }
                }
                _ => {}
            }

            Ok(())
        },
    )
}

fn tracecmd(context: &mut humility::ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let hubris = context.archive.as_ref().unwrap();

    let subargs = &TraceArgs::try_parse_from(subargs)?;
    let mut tasks: HashMap<u32, String> = HashMap::new();

    //
    // First, read the task block to get a mapping of IDs to names.
    //
    let (base, size) = hubris.task_table(core)?;

    let task = hubris.lookup_struct_byname("Task")?;
    let descriptor = task.lookup_member("descriptor")?.offset as u32;

    let taskdesc = hubris.lookup_struct_byname("TaskDesc")?;
    let entry_point = taskdesc.lookup_member("entry_point")?.offset as u32;

    let mut taskblock: Vec<u8> = vec![];
    taskblock.resize_with(task.size * size as usize, Default::default);
    core.read_8(base, taskblock.as_mut_slice())?;

    let taskblock32 =
        |o| u32::from_le_bytes(taskblock[o..o + 4].try_into().unwrap());

    for i in 0..size {
        let offs = i as usize * task.size;
        let daddr = taskblock32(offs + descriptor as usize);
        let entry = core.read_word_32(daddr + entry_point)?;
        let module = hubris.instr_mod(entry).unwrap_or("<unknown>");

        tasks.insert(i, module.to_string());
    }

    //
    // Now enable ITM and ingest.
    //
    let traceid = itm_enable_ingest(core, hubris, 0xf000_0000)?;
    tracecmd_ingest(core, subargs, hubris, &tasks, traceid)?;

    Ok(())
}

pub fn init() -> Command {
    Command {
        app: TraceArgs::command(),
        name: "trace",
        run: tracecmd,
        kind: CommandKind::Attached {
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Match,
        },
    }
}
