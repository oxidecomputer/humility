// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility ibc`
//!
//! Tools to interact with the BMR491 IBC

use anyhow::{anyhow, bail, Result};
use clap::{CommandFactory, Parser};
use colored::Colorize;
use humility::cli::Subcommand;
use humility_cmd::CommandKind;

use cmd_hiffy as humility_cmd_hiffy;
use hif::*;

use humility::core::Core;
use humility::hubris::*;
use humility_cmd::hiffy::HiffyContext;
use humility_cmd::i2c::I2cArgs;
use humility_cmd::idol::{HubrisIdol, IdolArgument};
use humility_cmd::{Archive, Attach, Command, Validate};
use humility_cmd_hiffy::HiffyLease;

#[derive(Parser, Debug)]
#[clap(name = "ibc", about = env!("CARGO_PKG_DESCRIPTION"))]
struct IbcArgs {
    /// sets timeout
    #[clap(
        long, short = 'T', default_value_t = 15000, value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    #[clap(subcommand)]
    cmd: IbcSubcommand,
}

#[derive(Parser, Debug)]
enum IbcSubcommand {
    /// Prints the auxiliary flash status
    BlackBox {
        #[clap(long, short)]
        verbose: bool,
    },
}

pub struct IbcHandler<'a> {
    hubris: &'a HubrisArchive,
    device: &'a HubrisI2cDevice,
    core: &'a mut dyn Core,
    context: HiffyContext<'a>,
}

impl<'a> IbcHandler<'a> {
    pub fn new(
        hubris: &'a HubrisArchive,
        core: &'a mut dyn Core,
        hiffy_timeout: u32,
    ) -> Result<Self> {
        let device = hubris
            .manifest
            .i2c_devices
            .iter()
            .find(|d| d.device == "bmr491")
            .ok_or_else(|| anyhow!("could not find 'bmr491' device"))?;
        println!("{device:?}");
        let context = HiffyContext::new(hubris, core, hiffy_timeout)?;
        Ok(Self { hubris, device, core, context })
    }
    pub fn blackbox(&mut self, verbose: bool) -> Result<()> {
        if verbose {
            humility::msg!("hi");
        }

        let funcs = self.context.functions()?;
        let write_func = funcs.get("I2cWrite", 8)?;
        let read_func = funcs.get("I2cRead", 7)?;

        let hargs = I2cArgs::from_device(self.device);

        const EVENT_INDEX: u8 =
            pmbus::commands::bmr491::CommandCode::MFR_EVENT_INDEX as u8;
        const READ_EVENT: u8 =
            pmbus::commands::bmr491::CommandCode::MFR_READ_EVENT as u8;

        // We want to loop over the 48 slots (24 fault, 24 event)

        let mut ops = vec![];
        ops.push(Op::Push(hargs.controller));
        ops.push(Op::Push(hargs.port.index));
        if let Some(mux) = hargs.mux {
            ops.push(Op::Push(mux.0));
            ops.push(Op::Push(mux.1));
        } else {
            ops.push(Op::PushNone);
            ops.push(Op::PushNone);
        }
        ops.push(Op::Push(hargs.address.unwrap()));

        // Let's unroll the loop here to avoid having to think about it in HIF
        for i in 0..48 {
            ops.push(Op::Push(i)); // This is our actual index
            ops.push(Op::Push(EVENT_INDEX));
            ops.push(Op::Push(1)); // Number of words to write
            ops.push(Op::Call(write_func.id));
            ops.push(Op::DropN(3));

            ops.push(Op::Push(READ_EVENT));
            ops.push(Op::Push(24)); // Number of bytes to read
            ops.push(Op::Call(read_func.id));
            ops.push(Op::DropN(2));
        }

        ops.push(Op::DropN(5));
        ops.push(Op::Done);

        let results = self.context.run(self.core, ops.as_slice(), None)?;
        println!("{results:?}");

        Ok(())
    }
}

////////////////////////////////////////////////////////////////////////////////

/// Raw data stored in the `READ_EVENT` PMBus block
///
/// Byte-encoded data is packed into native machine types, but is not decoded
/// any further.
///
/// See page 19 of the BMR491 technical specification
struct IbcEvent {
    event_id: u16,
    timestamp: u32,
    status_word: u16,
    status_vout: u8,
    status_iout: u8,
    status_input: u8,
    status_temperature: u8,
    status_cml: u8,
    status_other: u8,
    status_mfr: u8,
    vin_value: u16,
    vout_value: u16,
    iout_value: u16,
    temperature_value: u16,
}

////////////////////////////////////////////////////////////////////////////////

fn ibc(context: &mut humility::ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let subargs = IbcArgs::try_parse_from(subargs)?;
    let hubris = context.archive.as_ref().unwrap();
    let mut worker = IbcHandler::new(hubris, core, subargs.timeout)?;

    match subargs.cmd {
        IbcSubcommand::BlackBox { verbose } => {
            worker.blackbox(verbose)?;
        }
    }
    Ok(())
}

pub fn init() -> Command {
    Command {
        app: IbcArgs::command(),
        name: "ibc",
        run: ibc,
        kind: CommandKind::Attached {
            archive: Archive::Required,
            attach: Attach::Any, // TODO
            validate: Validate::Booted,
        },
    }
}
