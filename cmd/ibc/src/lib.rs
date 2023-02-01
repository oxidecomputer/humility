// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility ibc`
//!
//! Interface to BMR491 power regulator
//!
//! This regulator is present on Gimlet and Sidecar PCAs.  Right now,
//! `humility ibc` only exposes one subcommand: `black-box`.
//!
//! `humility ibc black-box` allows you to read out the blackbox log from the
//! power converter.  This can be used to debug previous faults.  The log is
//! stored in non-volatile memory, so it should be persistent through power
//! loss.
//!
//! Here's an example:
//! ```console
//! matt@igor ~ (sn5) $ pfexec ./humility -tsn5 ibc black-box
//! humility: attached to 0483:374e:001B00083156501320323443 via ST-Link V3
//! FAULT EVENT
//!   EVENT_INDEX:        0
//!   TIMESTAMP           0x000001ca = 45.5 sec
//!   EVENT_ID            0x0000
//!   STATUS_WORD         0x0010
//!   STATUS_IOUT         0x0080
//!   V_IN                0xf869 = 52.50V
//!   V_OUT               0x5f00 = 11.88V
//!   I_OUT               0x004e = 78.00A
//!   TEMPERATURE         0x001c = 28.00°C
//! FAULT EVENT
//!   EVENT_INDEX:        1
//!   TIMESTAMP           0x000001d4 = 46.6 sec
//!   EVENT_ID            0x0001
//!   STATUS_WORD         0x0001
//!   STATUS_MFG          0x0001
//!     b0 = BOOT_EVENT
//!   V_IN                0xf83c = 30.00V
//!   V_OUT               0x0000 = 0.00V
//!   I_OUT               0x0000 = 0.00A
//!   TEMPERATURE         0x0000 = 0.00°C
//! FAULT EVENT
//!   EVENT_INDEX:        2
//!   TIMESTAMP           0x000002c4 = 1 min, 10.0 sec
//!   EVENT_ID            0x0002
//!   STATUS_WORD         0x0010
//!   STATUS_IOUT         0x0080
//!   V_IN                0xf877 = 59.50V
//!   V_OUT               0x5f00 = 11.88V
//!   I_OUT               0x0070 = 112.00A
//!   TEMPERATURE         0x002c = 44.00°C
//! FAULT EVENT
//!   EVENT_INDEX:        3
//!   TIMESTAMP           0x27ffb2c4 = 776 day, 16 hr, 48 min, 6.6 sec
//!   EVENT_ID            0x0003
//!   STATUS_WORD         0x0001
//!   STATUS_MFG          0x0002
//!     b1 = INPUT_LOW_EVENT
//!   V_IN                0xf86b = 53.50V
//!   V_OUT               0x6000 = 12.00V
//!   I_OUT               0x0000 = 0.00A
//!   TEMPERATURE         0x0016 = 22.00°C
//! ```
//!
//! The log doesn't appear to be _completely_ reliable, so take it with a grain
//! of salt and with the datasheet close at hand.  For example, the machine in
//! the example above had **not** be up for 776 days.

use anyhow::{anyhow, bail, Result};
use clap::{CommandFactory, Parser};
use colored::Colorize;
use humility::cli::Subcommand;
use humility_cmd::CommandKind;
use zerocopy::{
    byteorder::{BigEndian, U16, U32},
    AsBytes, FromBytes,
};

use hif::*;

use humility::core::Core;
use humility::hubris::*;
use humility_cmd::hiffy::HiffyContext;
use humility_cmd::i2c::I2cArgs;
use humility_cmd::{Archive, Attach, Command, Validate};

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
        Ok(Self { device, core, context })
    }
    pub fn blackbox(&mut self, verbose: bool) -> Result<()> {
        let funcs = self.context.functions()?;
        let write_func = funcs.get("I2cWrite", 8)?;
        let read_func = funcs.get("I2cRead", 7)?;

        let hargs = I2cArgs::from_device(self.device);

        const EVENT_INDEX: u8 =
            pmbus::commands::bmr491::CommandCode::MFR_EVENT_INDEX as u8;
        const READ_EVENT: u8 =
            pmbus::commands::bmr491::CommandCode::MFR_READ_EVENT as u8;

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

        // We must read VOUT_MODE to interpret later data
        ops.push(Op::Push(
            pmbus::commands::bmr491::CommandCode::VOUT_MODE as u8,
        ));
        ops.push(Op::Push(1)); // Number of bytes to read
        ops.push(Op::Call(read_func.id));
        ops.push(Op::DropN(2));

        // The IBC contains 48 slots in its black box.  The lower 24 are fault
        // slots, and the upper 24 are life-cycle events.  By writing special
        // indexes to EVENT_INDEX, we can read back the index of the newest
        // record in each section
        ops.push(Op::Push(EVENT_INDEX));
        ops.push(Op::Push(255)); // Special to get newest fault index
        ops.push(Op::Push(1)); // Number of words to write
        ops.push(Op::Call(write_func.id));
        ops.push(Op::DropN(2));

        ops.push(Op::Push(1)); // Number of bytes to read
        ops.push(Op::Call(read_func.id));
        ops.push(Op::DropN(1));

        ops.push(Op::Push(254)); // Special to get newest life-cycle index
        ops.push(Op::Push(1)); // Number of words to write
        ops.push(Op::Call(write_func.id));
        ops.push(Op::DropN(2));

        ops.push(Op::Push(1)); // Number of bytes to read
        ops.push(Op::Call(read_func.id));
        ops.push(Op::DropN(2));

        // Now, read all of the actual events!
        // We'll unroll the loop here to avoid having to think about it in HIF
        for i in 0..48 {
            ops.push(Op::Push(EVENT_INDEX));
            ops.push(Op::Push(i)); // This is our actual index
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

        let vout_mode = results[0].as_ref().unwrap()[0];

        // Results are alternating between writing EVENT_INDEX and reading
        // READ_EVENT
        for (i, r) in results.iter().skip(1).step_by(2).enumerate() {
            if let Err(e) = r {
                bail!("Failed to read event {i}: {e}");
            }
        }
        let newest_fault_event_index = results[2].as_ref().unwrap()[0];
        let newest_lifecycle_event_index = results[4].as_ref().unwrap()[0];

        let mut events = vec![];
        for (i, r) in results.iter().skip(6).step_by(2).enumerate() {
            match r {
                Ok(r) => {
                    events.push(IbcEvent::read_from(&r[1..]).ok_or_else(
                        || anyhow!("Failed to decode IbcEvent from {r:?}"),
                    )?);
                }
                Err(e) => bail!("Failed to read event {i}: {e}"),
            }
        }
        for i in 0..=newest_fault_event_index {
            let e = events[i as usize];
            println!("{}", "FAULT EVENT".red());
            println!("  EVENT_INDEX:        {i}");
            self.print_event(e, verbose, vout_mode);
        }
        for i in 24..=newest_lifecycle_event_index {
            let e = events[i as usize];
            println!("{}", "LIFECYCLE EVENT".green());
            println!("  EVENT_INDEX:        {i}");
            self.print_event(e, verbose, vout_mode);
        }

        Ok(())
    }

    fn print_event(&self, e: IbcEvent, verbose: bool, vout_mode: u8) {
        println!(
            "  TIMESTAMP           {:#010x} = {}",
            e.timestamp.get(),
            Self::format_time(e.timestamp.get())
        );
        println!("  EVENT_ID            {:#06x}", e.event_id.get());
        println!("  STATUS_WORD         {:#06x}", e.status_word.get());
        let statuses = [
            ("STATUS_VOUT", e.status_vout),
            ("STATUS_IOUT", e.status_iout),
            ("STATUS_INPUT", e.status_input),
            ("STATUS_TEMPERATURE", e.status_temperature),
            ("STATUS_CML", e.status_cml),
            ("STATUS_OTHER", e.status_other),
            ("STATUS_MFG", e.status_mfr),
        ];

        for (name, value) in statuses {
            if value == 0 && !verbose {
                continue;
            }
            print!("  {: <18}  ", name);
            let v = format!("{:#06x}", value);
            if value == 0 {
                println!("{}", v.dimmed());
            } else {
                println!("{v}");
            }
        }
        if e.status_word.get() == 0x0001 {
            for (i, flag) in [
                "BOOT_EVENT",
                "INPUT_LOW_EVENT",
                "CANCEL_EVENT",
                "ERASE_EVENT",
                "CLR_EVENT",
                "ERASE_OVFL_EVENT",
            ]
            .iter()
            .enumerate()
            {
                // TODO: some of these flags are only valid in the life-cycle
                // section
                if e.status_mfr & (1 << i) != 0 {
                    println!("    b{i} = {flag}");
                }
            }
        }

        let dev = pmbus::commands::Device::Bmr491;
        use pmbus::commands::bmr491::CommandCode;
        let vout_mode_cb =
            || pmbus::commands::VOUT_MODE::CommandData(vout_mode);

        let values = [
            ("V_IN", CommandCode::READ_VIN, e.vin_value.get()),
            ("V_OUT", CommandCode::READ_VOUT, e.vout_value.get()),
            ("I_OUT", CommandCode::READ_IOUT, e.iout_value.get()),
            (
                "TEMPERATURE",
                CommandCode::READ_TEMPERATURE_1,
                e.temperature_value.get(),
            ),
        ];

        for (name, command, value) in values {
            dev.interpret(
                command as u8,
                value.as_bytes(),
                vout_mode_cb,
                |_field, value| {
                    println!("  {: <18}  {:#06x} = {value}", name, value.raw())
                },
            )
            .unwrap();
        }
    }

    fn format_time(timestamp: u32) -> String {
        let time_sec = timestamp / 10;
        let seconds = time_sec % 60;
        let minutes = (time_sec / 60) % 60;
        let hours = (time_sec / 3600) % 24;
        let days = time_sec / (3600 * 24);
        let mut out = String::new();
        if days > 0 {
            out += &format!("{days} day, ");
        }
        if hours > 0 {
            out += &format!("{hours} hr, ");
        }
        if minutes > 0 {
            out += &format!("{minutes} min, ");
        }
        out += &format!("{seconds}.{} sec", time_sec % 10);

        out
    }
}

////////////////////////////////////////////////////////////////////////////////

/// Raw data stored in the `READ_EVENT` PMBus block
///
/// Byte-encoded data is packed into native machine types, but is not decoded
/// any further.
///
/// See page 19 of the BMR491 technical specification
#[derive(Copy, Clone, Debug, FromBytes)]
struct IbcEvent {
    event_id: U16<BigEndian>,
    timestamp: U32<BigEndian>,
    status_word: U16<BigEndian>,
    status_vout: u8,
    status_iout: u8,
    status_input: u8,
    status_temperature: u8,
    status_cml: u8,
    status_other: u8,
    status_mfr: u8,
    vin_value: U16<BigEndian>,
    vout_value: U16<BigEndian>,
    iout_value: U16<BigEndian>,
    temperature_value: U16<BigEndian>,
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
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
        },
    }
}
