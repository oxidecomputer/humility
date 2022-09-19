// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility itm`
//!
//! `humility itm` consumes data from the Instrumentation Trace Macrocell
//! (ITM) present in many ARM Cortex-M variants.  ITM is problematic in many
//! dimensions: it is lossy; it requires knowledge of the target's clocking to
//! configure properly; it relies on functionality (SWO/SWV) that is often
//! buggy in chip debuggers; it isn't present everywhere (Cortex-M0+ in
//! particular doesn't have ITM).  So in general, ITM isn't what Hubris
//! programmers should be looking for:  those developing code and wishing to
//! see if and how that code is executed should prefer ring buffers to
//! ITM-based instrumentation.  (See the documentation for `humility ringbuf`
//! for details.)
//!
//! That said, ITM remains the best way to get certain messages from the
//! Hubris kernel (e.g., boot and panic messages); use `humility itm -ea` to
//! enable ITM and attach to the connected device.  For example, if running
//! with the `ping` task, one will see messages from `jefe` restarting it:
//!
//! ```console
//! % humility -a /path/to/my/hubris-archive.zip itm -ea
//! humility: attached via ST-Link
//! humility: core halted
//! humility: core resumed
//! humility: ITM synchronization packet found at offset 6
//! Task #7 Divide-by-zero
//! Task #7 Memory fault at address 0x0
//! Task #7 Divide-by-zero
//! ```
//!

use anyhow::{bail, Context, Result};
use clap::{CommandFactory, Parser};
use humility::cli::Subcommand;
use humility::core::Core;
use humility::hubris::*;
use humility_cmd::{attach_live, CommandKind};
use humility_cmd::{Archive, Command};
use humility_cortex::debug::*;
use humility_cortex::dwt::*;
use humility_cortex::itm::*;
use humility_cortex::scs::*;
use humility_cortex::tpiu::*;
use std::fs::File;
use std::io::Read;
use std::time::Instant;

const ITM_TRACEID_MAX: u8 = 0x7f;

#[derive(Parser, Debug)]
#[clap(name = "itm", about = env!("CARGO_PKG_DESCRIPTION"))]
struct ItmArgs {
    /// probe for ITM capability on attached device
    #[clap(
        long, short, conflicts_with_all = &["enable", "disable", "ingest"]
    )]
    probe: bool,

    /// enable ITM on attached device
    #[clap(long, short, conflicts_with_all = &["disable", "ingest"])]
    enable: bool,

    /// disable ITM on attached device
    #[clap(long, short)]
    disable: bool,

    /// sets ITM trace identifier
    #[clap(
        long, short, default_value_t = 0x3a, value_name = "identifier",
        parse(try_from_str = parse_int::parse)
    )]
    traceid: u8,

    /// ingest ITM data as CSV
    #[clap(long, short, value_name = "filename")]
    ingest: Option<String>,

    /// ingest directly from attached device
    #[clap(long, short, conflicts_with_all = &["disable", "ingest"])]
    attach: bool,

    /// assume bypassed TPIU in ingested file
    #[clap(long, short, requires = "ingest")]
    bypass: bool,

    /// sets the value of SWOSCALER
    #[clap(long, short, value_name = "scaler", requires = "enable",
        parse(try_from_str = parse_int::parse),
    )]
    clockscaler: Option<u16>,

    /// reset target
    #[clap(long, short, requires = "attach")]
    reset: bool,
}

fn itmcmd_probe(core: &mut dyn Core, coreinfo: &CoreInfo) -> Result<()> {
    humility::msg!("{:#x?}", TPIU_ACPR::read(core)?);
    humility::msg!("{:#x?}", TPIU_SPPR::read(core)?);
    humility::msg!("{:#x?}", TPIU_FFCR::read(core)?);

    humility::msg!("{:#x?}", ITM_LSR::read(core)?);
    humility::msg!("{:#x?}", ITM_TCR::read(core)?);
    humility::msg!("{:#x?}", ITM_TER::read(core)?);
    humility::msg!("{:#x?}", ITM_TPR::read(core)?);

    humility::msg!("{:#x?}", DWT_CTRL::read(core)?);

    humility::msg!("{:#x?}", DEMCR::read(core)?);

    match (coreinfo.vendor, coreinfo.part) {
        (Vendor::ST, ARMCore::CortexM4) => {
            humility::msg!("{:#x?}", STM32F4_DBGMCU_CR::read(core)?);
        }
        (Vendor::ST, ARMCore::CortexM7) => {
            humility::msg!("{:#x?}", STM32H7_DBGMCU_CR::read(core)?);
        }
        _ => {}
    }

    Ok(())
}

fn itmcmd_disable(core: &mut dyn Core) -> Result<()> {
    //
    // Unlock the ITM.
    //
    ITM_LAR::unlock(core)?;

    //
    // Disable the ITM.
    //
    let mut tcr = ITM_TCR::read(core)?;
    tcr.set_itm_enable(false);
    tcr.write(core)?;

    //
    // Now disable TRCENA in the DEMCR.
    //
    let mut val = DEMCR::read(core)?;
    val.set_trcena(false);
    val.write(core)?;

    humility::msg!("ITM disabled");

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
            humility::msg!("not a Saleae trace file; assuming raw input");

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
            if let ITMPayload::Instrumentation { payload, port } =
                &packet.payload
            {
                if *port > 1 {
                    println!("{:x?}", payload);
                    return Ok(());
                }

                for p in payload {
                    print!("{}", *p as char);
                }
            }

            Ok(())
        },
    )
}

fn itmcmd(context: &mut humility::ExecutionContext) -> Result<()> {
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let hubris = context.archive.as_ref().unwrap();

    let subargs = &ItmArgs::try_parse_from(subargs)?;
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

    //
    // For all of the other commands, we need to actually attach to the chip.
    //
    let mut c = attach_live(&context.cli, hubris)?;
    let core = c.as_mut();
    hubris.validate(core, HubrisValidate::ArchiveMatch)?;

    let coreinfo = CoreInfo::read(core)?;

    let _info = core.halt();
    humility::msg!("core halted");

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

        //
        // By default, we enable all logging (ports 0-7).
        //
        let stim = 0x0000_000f;
        let clockscaler = match subargs.clockscaler {
            Some(value) => value,
            None => {
                if !hubris.loaded() {
                    core.run()?;
                    bail!("must provide an archive");
                }

                swoscaler(hubris, core).with_context(|| {
                    "CPU frequency cannot be determined; the clock scaler \
                    must be set manually. To determine the clock scaler, \
                    take the CPU frequency in megahertz divide by 2, and \
                    subtract 1 (e.g., 400 MHz yields a clock scaler of 199), \
                    and specify via \"-c\" (e.g. \"-c 199\")"
                })?
            }
        };

        rval = itm_enable_explicit(core, &coreinfo, clockscaler, traceid, stim);
    }

    core.run()?;
    humility::msg!("core resumed");

    if subargs.reset {
        core.reset()?;
        humility::msg!("core reset");
    }

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

pub fn init() -> Command {
    Command {
        app: ItmArgs::command(),
        name: "itm",
        run: itmcmd,
        kind: CommandKind::Unattached { archive: Archive::Optional },
    }
}
