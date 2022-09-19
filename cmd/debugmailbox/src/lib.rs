// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility debugmailbox`
//!
//! The LPC55 includes an extra access port referred to as the Debug Mailbox.
//! This allows for running a fixed set of commands to do useful things such
//! as forcing SWD enablement and putting the chip into ISP mode without
//! needing to touch an external pin
//!
//! ```console
//! $ humility debugmailbox debug
//! Looks like a plausible debug mailbox
//! Reset chip successfully!
//! entering debug
//!
//! $ humility debugmailbox isp
//! Looks like a plausible debug mailbox
//! Reset chip successfully!
//! entered ISP mode!
//! ```

use anyhow::{bail, Result};
use clap::{CommandFactory, Parser};
use humility::cli::Subcommand;
use humility_cmd::{Archive, Command, CommandKind};
use probe_rs::{
    architecture::arm::{ApAddress, ArmProbeInterface, DpAddress},
    Probe,
};

// The debug mailbox registers
// See 51.5.5.1 of Rev 2.4 of the LPC55 manual
const CSW: u8 = 0x0;
const REQUEST: u8 = 0x4;
const RETURN: u8 = 0x8;
const IDR: u8 = 0xFC;

// See 51.5.5.1.4 of Rev 2.4 of the LPC55 manual
const DM_ID: u32 = 0x002a_0000;

// See 51.5.7.3 of Rev 2.4 of the LPC55 manual
const ACK_TOKEN: u32 = 0xa5a5;

// See 51.5.7.1.1 of Rev 2.4 of the LPC55 manual
#[repr(u8)]
pub enum DMCommand {
    //StartDM = 0x1,
    BulkErase = 0x3,
    //ExitDM = 0x4,
    ISPMode = 0x5,
    //FAMode = 0x6,
    StartDebug = 0x7,
    //DebugChallenge = 0x10,
    //DebugResponse = 0x11,
}

#[derive(Parser, Debug)]
#[clap(name = "subcmd")]
enum DebugMailboxCmd {
    /// Force the device into a mode to attach SWD
    /// This will not work if you have secure boot enabled!
    Debug,
    /// Force the device into ISP mode
    Isp,
}

fn alive<'a>(
    probe: &mut Box<dyn ArmProbeInterface + 'a>,
    addr: &ApAddress,
) -> Result<()> {
    probe.write_raw_ap_register(*addr, CSW, 0x21)?;

    let mut timeout = 100;
    loop {
        std::thread::sleep(std::time::Duration::from_millis(10));
        if let Ok(val) = probe.read_raw_ap_register(*addr, CSW) {
            if val == 0 {
                break;
            }
        }

        if timeout == 0 {
            break;
        } else {
            timeout -= -1;
        }
    }

    if timeout == 0 {
        bail!("Timed out waiting for reset. Chip is unlikely to be alive!");
    } else {
        println!("Reset chip successfully!");
        Ok(())
    }
}

fn write_request_reg<'a>(
    probe: &mut Box<dyn ArmProbeInterface + 'a>,
    addr: &ApAddress,
    val: u32,
) -> Result<()> {
    probe.write_raw_ap_register(*addr, REQUEST, val)?;

    let mut timeout = 100;
    loop {
        std::thread::sleep(std::time::Duration::from_millis(10));
        if let Ok(val) = probe.read_raw_ap_register(*addr, CSW) {
            if val == 0 {
                break;
            } else if val & 0x4 == 0x4 {
                bail!("debug overrun");
            } else if val & 0x8 == 0x8 {
                bail!("AHB overrun!");
            }
        }

        if timeout == 0 {
            break;
        } else {
            timeout -= 1;
        }
    }

    if timeout == 0 {
        bail!("Timed out waiting for request, chip may not be alive!");
    }

    Ok(())
}

fn write_req<'a>(
    probe: &mut Box<dyn ArmProbeInterface + 'a>,
    addr: &ApAddress,
    command: DMCommand,
    args: &[u32],
) -> Result<Vec<u32>> {
    let val = (command as u32) | ((args.len() as u32) << 16);

    write_request_reg(probe, addr, val)?;

    for (i, c) in args.iter().enumerate() {
        let ret = read_return(probe, addr)?;

        if ret & 0xffff != ACK_TOKEN {
            bail!("Bad return {:x}", ret);
        }

        if ((ret >> 16) & 0xffff) != ((args.len() - i) as u32) {
            bail!("Parameter length mismatch!");
        }

        write_request_reg(probe, addr, *c)?;
    }

    let b = read_return(probe, addr)?;

    let mut response_len = (b >> 16) & 0x7fff;

    let ret_val = b & 0xffff;

    if ret_val != 0 {
        bail!("request fail {:x}", ret_val);
    }

    let mut response: Vec<u32> = Vec::new();

    if response_len == 0 {
        return Ok(response);
    }

    while response_len > 0 {
        write_request_reg(probe, addr, response_len << 16 | ACK_TOKEN)?;

        let b = read_return(probe, addr)?;

        response.push(b);

        response_len -= 1;
    }

    write_request_reg(probe, addr, ACK_TOKEN)?;

    Ok(response)
}

fn read_return<'a>(
    probe: &mut Box<dyn ArmProbeInterface + 'a>,
    addr: &ApAddress,
) -> Result<u32> {
    let mut timeout = 100;
    loop {
        std::thread::sleep(std::time::Duration::from_millis(10));
        if let Ok(val) = probe.read_raw_ap_register(*addr, RETURN) {
            return Ok(val);
        }

        if timeout == 0 {
            break;
        } else {
            timeout -= 1;
        }
    }

    bail!("Timed out reading return!");
}

fn debugmailboxcmd(context: &mut humility::ExecutionContext) -> Result<()> {
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let subargs = DebugMailboxArgs::try_parse_from(subargs)?;

    // Get a list of all available debug probes.
    let probes = Probe::list_all();

    if probes.is_empty() {
        bail!("No probes found!");
    }

    let num = subargs.probe_num.unwrap_or(0);

    if num > probes.len() {
        bail!("Invalid probe number {}", num);
    }

    // Use the specified probe or the first one found.
    let mut probe = probes[num].open()?;

    probe.attach_to_unspecified()?;
    let mut iface = probe
        .try_into_arm_interface()
        .unwrap()
        .initialize_unspecified()
        .unwrap();

    let dm_port = ApAddress { dp: DpAddress::Default, ap: 2 };

    // Check if this is a debug mailbox. This is based on the sequence from
    // the LPC55 Debug Mailbox user manual
    //
    // The manual presumes we have to bank select manually but probe-rs
    // handles this for us

    // Check the IDR
    let val = iface.read_raw_ap_register(dm_port, IDR)?;

    if val != DM_ID {
        bail!("IDR incorrect: {:x}", val);
    }
    {
        println!("Looks like a plausible debug mailbox");
    }

    alive(&mut iface, &dm_port)?;

    match subargs.cmd {
        DebugMailboxCmd::Debug => {
            let _ =
                write_req(&mut iface, &dm_port, DMCommand::StartDebug, &[])?;

            println!("entering debug");
        }
        DebugMailboxCmd::Isp => {
            // The argument here 0x1 = UART.
            let _ =
                write_req(&mut iface, &dm_port, DMCommand::ISPMode, &[0x1])?;

            println!("entered ISP mode!");
        }
    };

    Ok(())
}
#[derive(Parser, Debug)]
#[clap(name = "debugmailbox", about = env!("CARGO_PKG_DESCRIPTION"))]
struct DebugMailboxArgs {
    /// Which probe to connect
    probe_num: Option<usize>,

    #[clap(subcommand)]
    cmd: DebugMailboxCmd,
}

pub fn init() -> Command {
    Command {
        app: DebugMailboxArgs::command(),
        name: "debugmailbox",
        run: debugmailboxcmd,
        kind: CommandKind::Unattached { archive: Archive::Ignored },
    }
}
