/*
 * Copyright 2020 Oxide Computer Company
 */

use crate::attach;
use crate::cmd::{Archive, HumilityCommand};
use crate::core::Core;
use crate::hubris::*;
use crate::itm::*;
use crate::Args;
use anyhow::Result;
use std::collections::HashMap;
use std::convert::TryInto;
use std::time::Instant;
use structopt::clap::App;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(name = "log", about = "get logging over ITM")]
struct LogArgs {
    #[structopt(long, short)]
    verbose: bool,
}

#[rustfmt::skip::macros(println)]

fn logcmd_ingest(
    core: &mut dyn Core,
    _subargs: &LogArgs,
    hubris: &HubrisArchive,
    _tasks: &HashMap<u32, String>,
    traceid: Option<u8>,
) -> Result<()> {
    let mut bytes: Vec<u8> = vec![];
    let mut ndx = 0;

    let start = Instant::now();
    let mut ts: f64 = 0.0;

    let mut frames = vec![];

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
            if let ITMPayload::Instrumentation { payload, port: _ } =
                &packet.payload
            {
                //
                // XXX: We will want to look at the port here, and if it's
                // not a defmt-encoded port, display it accordingly
                //

                frames.extend_from_slice(&payload);

                // find the first byte of our frame, add two since it's two
                // bytes long
                if let Some(position) =
                    frames.windows(2).position(|bytes| bytes == [0xde, 0x01])
                {
                    let position = position + 2;

                    // do we have a task id + frame length? we need two bytes
                    // for each of them, four total
                    if frames.len() > position + 4 {
                        // these are u16, but we want to use them as usize, so
                        // we cast after constructing it
                        let task_id = u16::from_le_bytes([
                            frames[position],
                            frames[position + 1],
                        ]);

                        // this is duplicated from hubris, probably want to
                        // de-duplicate someday
                        let task_index = task_id & (1 << 10) - 1;

                        let frame_len = u16::from_le_bytes([
                            frames[position + 2],
                            frames[position + 3],
                        ]) as usize;

                        // have we got all the data yet?
                        let total_end_position = position + 4 + frame_len;

                        if frames.len() >= total_end_position {
                            let start = position + 4;
                            let end = start + frame_len;

                            let table = {
                                let table =
                                    hubris.task_elf_map().get(&task_index);

                                if table.is_none() {
                                    println!("warning: couldn't find task #{}, continuing", task_index);
                                    return Ok(());
                                }

                                let table = table.unwrap().as_ref();

                                if table.is_none() {
                                    println!("warning: couldn't find log messages for task #{}, continuing", task_index);
                                    return Ok(());
                                }

                                table.unwrap()
                            };

                            if let Ok((frame, consumed)) = defmt_decoder::decode(
                                &frames[start..end],
                                &table,
                            ) {
                                // just to be on the safe side
                                assert_eq!(frame_len, consumed);

                                println!(
                                    "#{}: {}",
                                    task_index,
                                    frame.display(true)
                                );

                                let len = frames.len();
                                frames.rotate_left(end);
                                frames.truncate(len - end);
                            }
                        }
                    }
                }
            }
            Ok(())
        },
    )
}

fn logcmd(
    hubris: &mut HubrisArchive,
    args: &Args,
    subargs: &Vec<String>,
) -> Result<()> {
    let subargs = &LogArgs::from_iter_safe(subargs)?;
    let mut tasks: HashMap<u32, String> = HashMap::new();

    let mut c = attach(args)?;
    let core = c.as_mut();

    // make sure to validate
    hubris.validate(core, HubrisValidate::Booted)?;

    //
    // First, read the task block to get a mapping of IDs to names.
    //
    let base = core.read_word_32(hubris.lookup_symword("TASK_TABLE_BASE")?)?;
    let size = core.read_word_32(hubris.lookup_symword("TASK_TABLE_SIZE")?)?;

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
    let traceid = itm_enable_ingest(core, hubris, 0x0000_000f)?;
    logcmd_ingest(core, subargs, hubris, &tasks, traceid)?;

    Ok(())
}

pub fn init<'a, 'b>() -> (HumilityCommand, App<'a, 'b>) {
    (
        HumilityCommand {
            name: "log",
            archive: Archive::Required,
            run: logcmd,
        },
        LogArgs::clap(),
    )
}
