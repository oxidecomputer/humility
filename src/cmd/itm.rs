/*
 * Copyright 2020 Oxide Computer Company
 */

use crate::attach;
use crate::cmd::{Archive, HumilityCommand};
use crate::core::Core;
use crate::debug::*;
use crate::dwt::*;
use crate::hubris::*;
use crate::itm::*;
use crate::scs::*;
use crate::tpiu::*;
use crate::Args;
use anyhow::{bail, Result};
use std::fs::File;
use std::io::Read;
use std::time::Instant;
use structopt::clap::App;
use structopt::StructOpt;

const ITM_TRACEID_MAX: u8 = 0x7f;

#[derive(StructOpt, Debug)]
#[structopt(
    name = "itm",
    about = "commands for ARM's Instrumentation Trace Macrocell (ITM)"
)]
struct ItmArgs {
    /// probe for ITM capability on attached device
    #[structopt(
        long, short, conflicts_with_all = &["enable", "disable", "ingest"]
    )]
    probe: bool,
    /// enable ITM on attached device
    #[structopt(long, short, conflicts_with_all = &["disable", "ingest"])]
    enable: bool,
    /// disable ITM on attached device
    #[structopt(long, short)]
    disable: bool,
    /// sets ITM trace identifier
    #[structopt(
        long, short, default_value = "0x3a", value_name = "identifier",
        parse(try_from_str = parse_int::parse)
    )]
    traceid: u8,
    /// ingest ITM data as CSV
    #[structopt(long, short, value_name = "filename")]
    ingest: Option<String>,
    /// ingest directly from attached device
    #[structopt(long, short, conflicts_with_all = &["disable", "ingest"])]
    attach: bool,
    /// assume bypassed TPIU in ingested file
    #[structopt(long, short, requires = "ingest")]
    bypass: bool,
    /// sets the value of SWOSCALER
    #[structopt(long, short, value_name = "scaler", requires = "enable",
        parse(try_from_str = parse_int::parse),
    )]
    clockscaler: Option<u16>,
}

fn itmcmd_probe(core: &mut dyn Core, coreinfo: &CoreInfo) -> Result<()> {
    info!("{:#x?}", TPIU_ACPR::read(core)?);
    info!("{:#x?}", TPIU_SPPR::read(core)?);
    info!("{:#x?}", TPIU_FFCR::read(core)?);

    info!("{:#x?}", ITM_LSR::read(core)?);
    info!("{:#x?}", ITM_TCR::read(core)?);
    info!("{:#x?}", ITM_TER::read(core)?);
    info!("{:#x?}", ITM_TPR::read(core)?);

    info!("{:#x?}", DWT_CTRL::read(core)?);

    info!("{:#x?}", DEMCR::read(core)?);

    match (coreinfo.vendor, coreinfo.part) {
        (Vendor::ST, ARMCore::CortexM4) => {
            info!("{:#x?}", STM32F4_DBGMCU_CR::read(core)?);
        }
        (Vendor::ST, ARMCore::CortexM7) => {
            info!("{:#x?}", STM32H7_DBGMCU_CR::read(core)?);
        }
        _ => {}
    }

    Ok(())
}

fn itmcmd_disable(core: &mut dyn Core) -> Result<()> {
    /*
     * Unlock the ITM.
     */
    ITM_LAR::unlock(core)?;

    /*
     * Disable the ITM.
     */
    let mut tcr = ITM_TCR::read(core)?;
    tcr.set_itm_enable(false);
    tcr.write(core)?;

    /*
     * Now disable TRCENA in the DEMCR.
     */
    let mut val = DEMCR::read(core)?;
    val.set_trcena(false);
    val.write(core)?;

    info!("ITM disabled");

    Ok(())
}

fn itmcmd_ingest(subargs: &ItmArgs, filename: &str) -> Result<()> {
    let file = File::open(filename)?;
    let traceid = if subargs.bypass { None } else { Some(subargs.traceid) };

    let process = |packet: &ITMPacket| -> Result<()> {
        if let ITMPayload::Instrumentation { payload, .. } = &packet.payload {
            for p in payload {
                print!("{}", *p as char);
            }
        }

        Ok(())
    };

    let mut rdr = csv::Reader::from_reader(file);

    match rdr.headers() {
        Ok(_hdr) => {
            type SaleaeTraceRecord = (f64, u8, Option<String>, Option<String>);
            let mut iter = rdr.deserialize();

            itm_ingest(
                traceid,
                || {
                    if let Some(line) = iter.next() {
                        let record: SaleaeTraceRecord = line?;
                        Ok(Some((record.1, record.0)))
                    } else {
                        Ok(None)
                    }
                },
                process,
            )
        }
        Err(_) => {
            info!("not a Saleae trace file; assuming raw input");

            let mut file = File::open(filename)?;
            let mut buffer = [0; 1];

            itm_ingest(
                traceid,
                || {
                    let nbytes = file.read(&mut buffer)?;

                    match nbytes {
                        1 => Ok(Some((buffer[0], 0.0))),
                        0 => Ok(None),
                        _ => {
                            panic!("illegal read");
                        }
                    }
                },
                process,
            )
        }
    }
}

fn itmcmd_ingest_attached(
    core: &mut dyn Core,
    coreinfo: &CoreInfo,
    subargs: &ItmArgs,
) -> Result<()> {
    let mut bytes: Vec<u8> = vec![];
    let mut ndx = 0;

    let traceid = if coreinfo.address(CoreSightComponent::SWO).is_some() {
        None
    } else {
        Some(subargs.traceid)
    };

    let start = Instant::now();

    itm_ingest(
        traceid,
        || {
            while ndx == bytes.len() {
                bytes = core.read_swv()?;
                ndx = 0;
            }
            ndx += 1;
            Ok(Some((bytes[ndx - 1], start.elapsed().as_secs_f64())))
        },
        |packet| {
            match &packet.payload {
                ITMPayload::Instrumentation { payload, port } => {
                    if *port > 1 {
                        println!("{:x?}", payload);
                        return Ok(());
                    }

                    for p in payload {
                        print!("{}", *p as char);
                    }
                }
                _ => {}
            }

            Ok(())
        },
    )
}

fn itmcmd(
    hubris: &mut HubrisArchive,
    args: &Args,
    subargs: &Vec<String>,
) -> Result<()> {
    let subargs = &ItmArgs::from_iter_safe(subargs)?;
    let mut rval = Ok(());

    let traceid = subargs.traceid;

    if traceid >= ITM_TRACEID_MAX {
        bail!("traceid has a maximum value of {:x}", ITM_TRACEID_MAX);
    }

    if let Some(ingest) = &subargs.ingest {
        match itmcmd_ingest(subargs, ingest) {
            Err(e) => {
                bail!("failed to ingest {}: {}", ingest, e);
            }
            _ => {
                return Ok(());
            }
        }
    }

    /*
     * For all of the other commands, we need to actually attach to the chip.
     */
    let mut c = attach(args)?;
    let core = c.as_mut();
    let coreinfo = CoreInfo::read(core)?;

    let _info = core.halt();
    info!("core halted");

    if subargs.probe {
        rval = itmcmd_probe(core, &coreinfo);
    }

    if subargs.disable {
        rval = itmcmd_disable(core);
    }

    if subargs.enable {
        if subargs.attach {
            core.init_swv()?;
        }

        /*
         * By default, we enable all logging (ports 0-7).
         */
        let stim = 0x0000_000f;
        let clockscaler = match subargs.clockscaler {
            Some(value) => value,
            None => swoscaler(hubris, core)?,
        };

        rval = itm_enable_explicit(core, &coreinfo, clockscaler, traceid, stim);
    }

    core.run()?;
    info!("core resumed");

    if rval.is_ok() && subargs.attach {
        match itmcmd_ingest_attached(core, &coreinfo, subargs) {
            Err(e) => {
                bail!("failed to ingest from attached device: {}", e);
            }
            _ => {
                return Ok(());
            }
        }
    }

    rval
}

pub fn init<'a, 'b>() -> (HumilityCommand, App<'a, 'b>) {
    (
        HumilityCommand {
            name: "itm",
            archive: Archive::Optional,
            run: itmcmd,
        },
        ItmArgs::clap(),
    )
}
