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

use std::{
    fs::File,
    path::PathBuf,
    process::{Command as Process, Stdio},
    thread::spawn,
};

use anyhow::{bail, Context, Result};
use byteorder::{ByteOrder, LittleEndian, WriteBytesExt};
use clap::{CommandFactory, Parser};
use humility_cli::{ExecutionContext, Subcommand};
use humility_cmd::{Archive, Command, CommandKind};
use probe_rs::{
    architecture::arm::{ApAddress, ArmProbeInterface, DapError, DpAddress},
    DebugProbeError, Probe,
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
#[derive(Clone, Copy, Debug)]
#[repr(u8)]
pub enum DMCommand {
    //StartDM = 0x1,
    BulkErase = 0x3,
    ExitDM = 0x4,
    ISPMode = 0x5,
    //FAMode = 0x6,
    StartDebug = 0x7,
    DebugChallenge = 0x10,
    DebugResponse = 0x11,
}

#[derive(Parser, Debug)]
#[clap(name = "subcmd")]
enum DebugMailboxCmd {
    /// Force the device into a mode to attach SWD
    /// This will not work if you have secure boot enabled!
    Debug {
        /// When true, the target will be reset via SYSREQRESET before sending
        /// the DebugStart command to the debug mailbox.  This defaults to true
        /// as most user applications will not have handlers for debug mailbox
        /// operations and attempt to send a debug mailbox command will timeout.
        /// If you are certain your target is still executing its ROM, set this
        /// to false to enable the debug access without losing the current
        /// execution state.
        #[clap(long, action=clap::ArgAction::Set, default_value_t = true)]
        reset_target: bool,
    },
    /// Force the device into ISP mode
    Isp,
    /// Get a Debug Authentication Challenge
    AuthChallenge { out: PathBuf },
    /// Send a Debug Authentication Response
    AuthResponse { dar: PathBuf },
    /// Debug Authentication Challenge/Response with online signing via `permslip`
    AuthOnline {
        /// Debug credential key name (try `permslip list-keys -t`)
        key_name: String,
        /// Authentication beacon (UM11126 §51.7)
        #[clap(long, default_value_t = 0, parse(try_from_str = parse_int::parse))]
        beacon: u16,
    },
}

fn poll_raw_ap_register(
    probe: &mut Box<dyn ArmProbeInterface + '_>,
    ap: &ApAddress,
    addr: u8,
    mut f: impl FnMut(u32) -> Result<bool>,
    mut timeout_ms: usize,
) -> Result<u32> {
    loop {
        // Sleep at least once to provide a little delay from any previous
        // operation.  Really this should be done by the caller but it isn't and
        // fixing all those cases is more annoying than having a little extra
        // delay here.  Currently, read_return() will definitely fail Without
        // this initial delay.
        std::thread::sleep(std::time::Duration::from_millis(10));

        match probe.read_raw_ap_register(*ap, addr) {
            Ok(val) => match f(val) {
                Ok(true) => return Ok(val),
                Ok(false) => (),
                Err(e) => return Err(e),
            },
            Err(DebugProbeError::ArchitectureSpecific(arch_err)) => {
                match arch_err.downcast::<DapError>() {
                    Ok(dap_err) => match *dap_err {
                        DapError::WaitResponse => {}
                        e => {
                            return Err(DebugProbeError::ArchitectureSpecific(
                                e.into(),
                            )
                            .into())
                        }
                    },
                    Err(e) => {
                        return Err(
                            DebugProbeError::ArchitectureSpecific(e).into()
                        )
                    }
                }
            }
            Err(x) => return Err(x.into()),
        }

        timeout_ms = timeout_ms.saturating_sub(10);
        if timeout_ms == 0 {
            break;
        }
    }

    bail!("Timed out polling AP {ap:?} address 0x{addr:02x}")
}

fn alive<'a>(
    probe: &mut Box<dyn ArmProbeInterface + 'a>,
    addr: &ApAddress,
    reset: bool,
) -> Result<()> {
    if reset {
        probe.write_raw_ap_register(*addr, CSW, 0x21)?;
        println!("Resetting chip via SYSREQRESET!");
    }

    let _ = poll_raw_ap_register(probe, addr, CSW, |csw| Ok(csw == 0), 1000)
        .context("Waiting for debugmailbox to be ready for requests")?;

    Ok(())
}

fn write_request_reg<'a>(
    probe: &mut Box<dyn ArmProbeInterface + 'a>,
    addr: &ApAddress,
    val: u32,
) -> Result<()> {
    probe.write_raw_ap_register(*addr, REQUEST, val)?;

    let _ = poll_raw_ap_register(
        probe,
        addr,
        CSW,
        |csw| {
            if csw & 0x4 == 0x4 {
                bail!("debug overrun")
            } else if csw & 0x8 == 0x8 {
                bail!("AHB overrun")
            } else {
                Ok(csw == 0)
            }
        },
        1000,
    )
    .context("Waiting for debugmailbox request to complete")?;

    Ok(())
}

fn write_req<'a>(
    probe: &mut Box<dyn ArmProbeInterface + 'a>,
    addr: &ApAddress,
    command: DMCommand,
    args: &[u32],
) -> Result<Vec<u32>> {
    let val = (command as u32) | ((args.len() as u32) << 16);

    write_request_reg(probe, addr, val).with_context(|| {
        format!("Sending command {:?} with data length {}", command, args.len())
    })?;

    for (i, c) in args.iter().enumerate() {
        let ret =
            read_return(probe, addr).context("Reading request data ACK")?;

        if ret & 0xffff != ACK_TOKEN {
            bail!("Bad return {:x}", ret);
        }

        if ((ret >> 16) & 0xffff) != ((args.len() - i) as u32) {
            bail!("Parameter length mismatch!");
        }

        write_request_reg(probe, addr, *c).context("Writing request data")?;
    }

    let b = read_return(probe, addr).context("Reading request ACK")?;

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
        write_request_reg(probe, addr, response_len << 16 | ACK_TOKEN)
            .context("ACKing response data")?;

        let b = read_return(probe, addr).context("Reading response data")?;

        response.push(b);

        response_len -= 1;
    }

    write_request_reg(probe, addr, ACK_TOKEN)
        .context("Final response data ACK")?;

    Ok(response)
}

fn read_return<'a>(
    probe: &mut Box<dyn ArmProbeInterface + 'a>,
    addr: &ApAddress,
) -> Result<u32> {
    poll_raw_ap_register(probe, addr, RETURN, |_| Ok(true), 1000)
        .context("Reading debugmailbox RETURN register")
}

fn debugmailboxcmd(context: &mut ExecutionContext) -> Result<()> {
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

    match subargs.cmd {
        DebugMailboxCmd::Debug { reset_target } => {
            alive(&mut iface, &dm_port, reset_target)?;

            let _ =
                write_req(&mut iface, &dm_port, DMCommand::StartDebug, &[])?;

            println!("entering debug");
        }
        DebugMailboxCmd::Isp => {
            alive(&mut iface, &dm_port, true)?;

            // The argument here 0x1 = UART.
            let _ =
                write_req(&mut iface, &dm_port, DMCommand::ISPMode, &[0x1])?;

            println!("entered ISP mode!");
        }
        DebugMailboxCmd::AuthChallenge { out } => {
            alive(&mut iface, &dm_port, true)?;

            let challenge_bytes = write_req(
                &mut iface,
                &dm_port,
                DMCommand::DebugChallenge,
                &[],
            )?;

            let mut out_file = File::create(out)?;
            for word in challenge_bytes {
                out_file.write_u32::<LittleEndian>(word)?;
            }
        }
        DebugMailboxCmd::AuthResponse { dar } => {
            alive(&mut iface, &dm_port, false)?;

            let dar_bytes = std::fs::read(dar)?;
            if dar_bytes.len() % 4 != 0 {
                bail!("Debug Authentication Response is not an even number of words ({} bytes)", dar_bytes.len());
            }

            let mut dar_words = vec![0u32; dar_bytes.len() / 4];
            LittleEndian::read_u32_into(&dar_bytes, &mut dar_words);

            let _ = write_req(
                &mut iface,
                &dm_port,
                DMCommand::DebugResponse,
                dar_words.as_slice(),
            )?;

            let _ = write_req(&mut iface, &dm_port, DMCommand::ExitDM, &[])?;
        }
        DebugMailboxCmd::AuthOnline { key_name, beacon } => {
            // Get the challenge from the chip.
            alive(&mut iface, &dm_port, true)?;
            let dac = write_req(
                &mut iface,
                &dm_port,
                DMCommand::DebugChallenge,
                &[],
            )?;

            // Ask permission-slip to sign it.
            let mut permslip = Process::new("permslip")
                .arg("sign")
                .arg(key_name)
                .arg("--kind=debug-authn-challenge")
                .arg(format!("--beacon={beacon}"))
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .spawn()
                .context("Unable to execute `permslip`, is it in your PATH and executable?")?;

            // Send the challenge to permission-slip ...
            let mut input =
                permslip.stdin.take().context("Opening permslip stdin")?;
            spawn(move || -> Result<()> {
                for word in dac {
                    input.write_u32::<LittleEndian>(word)?;
                }
                Ok(())
            });

            // ... and read the response from it.
            let dar_bytes = permslip
                .wait_with_output()
                .context("Reading permslip stdout")?
                .stdout;
            if dar_bytes.len() % 4 != 0 {
                bail!("Debug Authentication Response is not an even number of words ({} bytes)", dar_bytes.len());
            }
            let mut dar_words = vec![0u32; dar_bytes.len() / 4];
            LittleEndian::read_u32_into(&dar_bytes, &mut dar_words);

            // Send the response to the chip.
            alive(&mut iface, &dm_port, false)?;
            let _ = write_req(
                &mut iface,
                &dm_port,
                DMCommand::DebugResponse,
                dar_words.as_slice(),
            )?;
            let _ = write_req(&mut iface, &dm_port, DMCommand::ExitDM, &[])?;
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
