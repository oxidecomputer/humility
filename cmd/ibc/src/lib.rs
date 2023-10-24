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
//! $ humility -tsn5 ibc black-box
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
use humility_cli::{ExecutionContext, Subcommand};
use humility_cmd::CommandKind;
use humility_idol::{self as idol, HubrisIdol};
use zerocopy::{
    byteorder::{BigEndian, U16, U32},
    AsBytes, FromBytes,
};

use hif::*;

use humility::core::Core;
use humility::hubris::*;
use humility_cmd::{Archive, Attach, Command, Validate};
use humility_hiffy::HiffyContext;

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
    core: &'a mut dyn Core,
    context: HiffyContext<'a>,
}

impl<'a> IbcHandler<'a> {
    pub fn new(
        hubris: &'a HubrisArchive,
        core: &'a mut dyn Core,
        hiffy_timeout: u32,
    ) -> Result<Self> {
        let context = HiffyContext::new(hubris, core, hiffy_timeout)?;
        Ok(Self { hubris, core, context })
    }

    pub fn blackbox(&mut self, verbose: bool) -> Result<()> {
        let bmr491_max_lifecycle_event_index = self
            .hubris
            .get_idol_command("Power.bmr491_max_lifecycle_event_index")?;
        let bmr491_max_fault_event_index = self
            .hubris
            .get_idol_command("Power.bmr491_max_fault_event_index")?;
        let read_mode = self.hubris.get_idol_command("Power.read_mode")?;
        let read_log =
            self.hubris.get_idol_command("Power.bmr491_event_log_read")?;

        let mut ops = vec![];

        // We must read VOUT_MODE to interpret later data
        let payload = read_mode.payload(&[
            ("dev", idol::IdolArgument::String("Bmr491")),
            ("rail", idol::IdolArgument::Scalar(0)),
            ("index", idol::IdolArgument::Scalar(0)),
        ])?;
        self.context.idol_call_ops(&read_mode, &payload, &mut ops)?;

        self.context.idol_call_ops(
            &bmr491_max_fault_event_index,
            &[],
            &mut ops,
        )?;
        self.context.idol_call_ops(
            &bmr491_max_lifecycle_event_index,
            &[],
            &mut ops,
        )?;
        for i in 0..48 {
            let payload = read_log
                .payload(&[("index", idol::IdolArgument::Scalar(i))])?;
            self.context.idol_call_ops(&read_log, &payload, &mut ops)?;
        }
        ops.push(Op::Done);
        let results = self.context.run(self.core, &ops, None)?;

        let vout_mode = results[0].as_ref().unwrap()[0];
        let newest_fault_event_index = results[1].as_ref().unwrap()[0];
        let newest_lifecycle_event_index = results[2].as_ref().unwrap()[0];

        let mut events = vec![];
        for (i, r) in results.iter().skip(3).enumerate() {
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

        // Decoding is extracted from revision A and B of the datasheet, which
        // contain non-overlapping sets of decoding information.  They don't
        // actively disagree with each other, though, which is good!
        //
        // 28701-BMR491 02 Rev A, April 2021
        // 28701-BMR491 10 Rev B, September 2022
        let status_word = e.status_word.get();
        println!("  STATUS_WORD         {:#06x}", status_word);
        for (bit, name) in [
            (0, "Unspecified fault (manufacturer?)"),
            (1, "CML fault"),
            (2, "Temperature fault or warning"),
            (3, "Input undervoltage fault"),
            (4, "Output overcurrent fault"),
            (5, "Output overvoltage fault"),
            (6, "Unit is not providing power"),
            (9, "A bit in Status-Other is set"),
            (11, "Power-Good signal, if present, is negated"),
            (12, "Manufacturer specific fault or warning"),
            (
                13,
                "input voltage, input current, or input power fault or warning",
            ),
            (14, "output current or output power fault or warning"),
            (15, "output voltage fault or warning"),
        ] {
            if status_word & (1 << bit) != 0 {
                println!("    {:#06x}: {name}", 1 << bit);
            }
        }
        let statuses = [
            (
                "STATUS_VOUT",
                e.status_vout,
                &[
                    (1, "Toff-Max Fault"),
                    (2, "Ton-Max Fault"),
                    (3, "Vout Max Warning"),
                    (4, "Vout Undervoltage Fault"),
                    (5, "Vout Undervoltage Warning"),
                    (6, "Vout Overvoltage Warning"),
                    (7, "Vout Overvoltage Fault"),
                ] as &[_],
            ),
            (
                "STATUS_IOUT",
                e.status_iout,
                &[
                    (4, "Iout undercurrent fault"),
                    (5, "Iout Overcurrent warning"),
                    (6, "Iout Overcurrent and low voltage fault"),
                    (7, "Iout Overcurrent Fault"),
                ],
            ),
            (
                "STATUS_INPUT",
                e.status_input,
                &[
                    (3, "Insufficient Vin"),
                    (4, "VIN undervoltage fault"),
                    (5, "VIN undervoltage warning"),
                    (6, "VIN overvoltage warning"),
                    (7, "VIN overvoltage fault"),
                ],
            ),
            (
                "STATUS_TEMPERATURE",
                e.status_temperature,
                &[
                    (4, "undertemperature fault"),
                    (5, "undertemperature warning"),
                    (6, "overtemperature warning"),
                    (7, "overtemperature fault"),
                ],
            ),
            (
                "STATUS_CML",
                e.status_cml,
                &[
                    (0, "Other memory or logic fault"),
                    (1, "Other communication fault"),
                    (4, "Memory Fault Detected"),
                    (5, "Packed Error Check Failed"),
                    (6, "Invalid Or Unsupported Data Received"),
                    (7, "Invalid Or Unsupported Command Received"),
                ],
            ),
            ("STATUS_OTHER", e.status_other, &[]),
            (
                "STATUS_MFG",
                e.status_mfr,
                &[
                    // TODO: some of these flags are only valid in the
                    // life-cycle section
                    (0, "BOOT_EVENT"),
                    (1, "INPUT_LOW_EVENT"),
                    (2, "CANCEL_EVENT"),
                    (3, "ERASE_EVENT"),
                    (4, "CLR_EVENT"),
                    (5, "ERASE_OVFL_EVENT*"),
                    (7, "Brown out detected (?)"),
                ],
            ),
        ];

        for (name, value, bits) in statuses {
            if value == 0 && !verbose {
                continue;
            }
            print!("  {: <18}  ", name);
            let v = format!("{:#04x}", value);
            if value == 0 {
                println!("{}", v.dimmed());
            } else {
                println!("{v}");
            }
            for (bit, name) in bits {
                if value & (1 << bit) != 0 {
                    println!("    {:#04x}: {name}", 1 << bit);
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
#[repr(C, packed)]
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

fn ibc(context: &mut ExecutionContext) -> Result<()> {
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
