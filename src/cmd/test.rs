/*
 * Copyright 2020 Oxide Computer Company
 */

use crate::attach_live;
use crate::cmd::{Archive, HumilityCommand};
use crate::core::Core;
use crate::hubris::HubrisArchive;
use crate::itm::*;
use crate::test::*;
use crate::Args;
use anyhow::{bail, Context, Result};
use std::cell::RefCell;
use std::collections::VecDeque;
use std::time::Instant;
use structopt::clap::App;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(name = "test", about = "run Hubristest suite and parse results")]
struct TestArgs {
    /// dump full report even on success
    #[structopt(long, short)]
    dumpalways: bool,
    /// sets the output file
    #[structopt(long, short, value_name = "filename")]
    output: Option<String>,
}

fn test_ingest(
    core: &mut dyn Core,
    subargs: &TestArgs,
    hubris: &HubrisArchive,
    traceid: Option<u8>,
) -> Result<()> {
    let mut bufs: VecDeque<(Vec<u8>, f64)> = VecDeque::new();
    let mut ndx = 0;
    let mut current = None;

    let v = hubris
        .lookup_variable("TEST_KICK")
        .context("does not appear to be a test archive")?;

    if v.size != 4 {
        bail!("expected TEST_KICK to be of size 4; found {}", v.size);
    }

    let start = Instant::now();

    let mut testrun = TestRun::new(hubris);
    let mut kicked = false;

    let shared = RefCell::new(core);

    let wirebuf = vec![];
    let wire = RefCell::new(wirebuf);

    let output = subargs.output.as_ref();
    let timeout = 30;

    let rval = itm_ingest(
        traceid,
        || {
            loop {
                if start.elapsed().as_secs() > timeout {
                    bail!("timed out after {} seconds", timeout);
                }

                /*
                 * We will keep reading until we have a zero byte read, at
                 * which time we will kick out and process one byte.
                 */
                let buf = shared.borrow_mut().read_swv()?;

                if buf.len() != 0 {
                    bufs.push_back((buf, start.elapsed().as_secs_f64()));
                    continue;
                }

                match current {
                    None => {
                        current = bufs.pop_front();
                        ndx = 0;
                    }

                    Some((ref buf, _)) => {
                        if ndx == buf.len() {
                            current = bufs.pop_front();
                            ndx = 0;
                        }
                    }
                }

                if current.is_none() {
                    continue;
                }

                break;
            }

            let (buf, pulled) = current.as_ref().unwrap();
            ndx += 1;

            let datum = (buf[ndx - 1], start.elapsed().as_secs_f64());
            wire.borrow_mut().push((datum.0, *pulled, datum.1));

            Ok(Some(datum))
        },
        |packet| match &packet.payload {
            ITMPayload::Instrumentation { payload, port } => {
                let source = match *port {
                    0 => TestSource::KernelLog,
                    1 => TestSource::UserLog,
                    8 => TestSource::Suite,
                    _ => {
                        bail!("spurious data on port {}: {:x?}", port, payload);
                    }
                };

                for p in payload {
                    match testrun.consume(source, *p as char) {
                        Ok(_) => {}
                        Err(err) => {
                            testrun.report(
                                output,
                                &wire.borrow(),
                                Some(&err),
                            )?;
                            return Err(err);
                        }
                    }

                    if testrun.completed() {
                        if testrun.failed() {
                            testrun.report(output, &wire.borrow(), None)?;
                            std::process::exit(1);
                        }

                        if subargs.dumpalways {
                            testrun.report(output, &wire.borrow(), None)?;
                        }

                        std::process::exit(0);
                    }
                }

                Ok(())
            }
            ITMPayload::None => {
                match packet.header {
                    ITMHeader::Sync => {
                        if !kicked {
                            shared.borrow_mut().halt()?;
                            shared.borrow_mut().write_word_32(v.addr, 1)?;
                            shared.borrow_mut().run()?;
                            kicked = true;
                        }
                    }
                    ITMHeader::Malformed(datum) => {
                        bail!("malformed datum: 0x{:x}", datum);
                    }
                    _ => {}
                }

                Ok(())
            }
            _ => {
                bail!("unknown packet: {:x?}", packet);
            }
        },
    );

    match rval {
        Ok(_) => rval,
        Err(err) => {
            testrun.report(output, &wire.borrow(), Some(&err))?;
            Err(err)
        }
    }
}

fn test(
    hubris: &mut HubrisArchive,
    args: &Args,
    subargs: &Vec<String>,
) -> Result<()> {
    let subargs = TestArgs::from_iter_safe(subargs)?;
    let mut c = attach_live(args)?;
    let core = c.as_mut();

    hubris.validate(core)?;

    let stim = 0x0000_ffff;
    let traceid = itm_enable_ingest(core, hubris, stim)?;
    test_ingest(core, &subargs, hubris, traceid)?;

    Ok(())
}

pub fn init<'a, 'b>() -> (HumilityCommand, App<'a, 'b>) {
    (
        HumilityCommand { name: "test", archive: Archive::Required, run: test },
        TestArgs::clap(),
    )
}
