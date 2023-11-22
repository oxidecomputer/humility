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
//! $ humility ibc black-box
//! humility: attached to 0483:374e:001B00083156501320323443 via ST-Link V3
//! FAULT EVENT
//!   EVENT_INDEX:        0
//!   TIMESTAMP           0x000a3747 = 18 hr, 35 min, 51.1 sec
//!   EVENT_ID            0x0000
//!   STATUS_WORD         0x0010
//!     0x0010: Output overcurrent fault
//!   STATUS_IOUT         0x80
//!     0x80: Iout Overcurrent Fault
//!   STATUS_MFR          0x00
//!   V_IN                0xf84f = 39.500V
//!   V_OUT               0x6000 = 12.000V
//!   I_OUT               0x0075 = 117.000A
//!   TEMPERATURE         0x0023 = 35.000°C
//! FAULT EVENT
//!   EVENT_INDEX:        1
//!   TIMESTAMP           0x000a3751 = 18 hr, 35 min, 52.1 sec
//!   EVENT_ID            0x0001
//!   STATUS_WORD         0x0001
//!     0x0001: System event
//!   STATUS_MFR          0x01
//!     0x01: INPUT_LOW_EVENT
//!   V_IN                0xf83c = 30.000V
//!   V_OUT               0x0000 = 0.000V
//!   I_OUT               0x0000 = 0.000A
//!   TEMPERATURE         0x0000 = 0.000°C
//! FAULT EVENT
//!   EVENT_INDEX:        2
//!   TIMESTAMP           0x2809e751 = 777 day, 11 hr, 22 min, 48.1 sec
//!   EVENT_ID            0x0002
//!   STATUS_WORD         0x0001
//!     0x0001: System event
//!   STATUS_MFR          0x02
//!     0x02: CANCEL_EVENT
//!   V_IN                0xf86b = 53.500V
//!   V_OUT               0x5f00 = 11.875V
//!   I_OUT               0x0000 = 0.000A
//!   TEMPERATURE         0x0012 = 18.000°C
//! FAULT EVENT
//!   EVENT_INDEX:        3
//!   TIMESTAMP           0x50099751 = 1554 day, 4 hr, 9 min, 44.1 sec
//!   EVENT_ID            0x0003
//!   STATUS_WORD         0x0001
//!     0x0001: System event
//!   STATUS_MFR          0x02
//!     0x02: CANCEL_EVENT
//!   V_IN                0xf86b = 53.500V
//!   V_OUT               0x6000 = 12.000V
//!   I_OUT               0x0000 = 0.000A
//!   TEMPERATURE         0x0020 = 32.000°C
//! LIFECYCLE EVENT
//!   EVENT_INDEX:        24
//!   TIMESTAMP           0xffef8ad0 = 4969 day, 18 hr, 41 min, 12.0 sec
//!   EVENT_ID            0x0680
//!   STATUS_WORD         0x0001
//!     0x0001: System event
//!   STATUS_MFR          0x04
//!     0x04: CLR_EVENT
//!   V_IN                0xf86b = 53.500V
//!   V_OUT               0x5f00 = 11.875V
//!   I_OUT               0x0000 = 0.000A
//!   TEMPERATURE         0x001a = 26.000°C
//! LIFECYCLE EVENT
//!   EVENT_INDEX:        25
//!   TIMESTAMP           0xffef8bd4 = 4969 day, 18 hr, 41 min, 38.0 sec
//!   EVENT_ID            0x0681
//!   STATUS_WORD         0x0001
//!     0x0001: System event
//!   STATUS_MFR          0x05
//!     0x05: ERASE_OVFL_EVENT
//!   V_IN                0xf860 = 48.000V
//!   V_OUT               0x5f00 = 11.875V
//!   I_OUT               0x0000 = 0.000A
//!   TEMPERATURE         0x001b = 27.000°C
//!
//! ```
//!
//! The log doesn't appear to be _completely_ reliable, especially with
//! respect to timestamps, so take it with a grain of salt and with the
//! datasheet close at hand.  For example, the machine in the example above
//! had **not** be up for 777 days (or, for that matter, for 4,969).
//!

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
        // In that case that a log contains no entries, the newest event index
        // reported will be the lowest slot for the log type, which will be
        // empty. Reading an empty event will return 0xFF in all bytes.
        if e.timestamp.get() == u32::MAX
            && e.event_id.get() == u16::MAX
            && e.status_word.get() == u16::MAX
        {
            println!("  EMPTY");
            return;
        }
        println!(
            "  TIMESTAMP           {:#010x} = {}",
            e.timestamp.get(),
            Self::format_time(e.timestamp.get())
        );
        println!("  EVENT_ID            {:#06x}", e.event_id.get());

        // Decoding is extracted from revision A and B of the datasheet, which
        // contain non-overlapping sets of decoding information.  They don't
        // actively disagree with each other, though, which is good!  They do,
        // however, both disagree (if slightly) with the application note that
        // covers the event recording present in the BMR491 (Application
        // Note 326, Event recorder for digital Intermediate Bus Converters),
        // specifically with respect to decoding STATUS_MFR.
        //
        // The three documents that we're working from:
        //
        //   28701-BMR491 02 Rev A, April 2021
        //   28701-BMR491 10 Rev B, September 2022
        //   28710-FGB 100 378 Rev A (Application Note 326), May 2023

        let status_word = e.status_word.get();
        println!("  STATUS_WORD         {:#06x}", status_word);
        for (bit, name) in [
            (0, "System event"),
            (1, "CML fault"),
            (2, "Temperature fault or warning"),
            (3, "Input undervoltage fault"),
            (4, "Output overcurrent fault"),
            (5, "Output overvoltage fault"),
            (6, "Unit is not providing power"),
            (9, "A bit in Status-Other is set"),
            (11, "Power-Good signal"),
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
        ];

        for (name, value, bits) in statuses {
            if value == 0 && !verbose {
                continue;
            }
            let v = format!("{:#04x}", value);
            print!("  {: <18}  ", name);
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

        // The STATUS_MFR depends on the value of STATUS_WORD.  See
        // Application Note 326 for details.
        let mfr = [
            (
                0x0001u16,
                &[
                    (0u8, "BOOT_EVENT"),
                    (1, "INPUT_LOW_EVENT"),
                    (2, "CANCEL_EVENT"),
                    (3, "ERASE_EVENT"),
                    (4, "CLR_EVENT"),
                    (5, "ERASE_OVFL_EVENT"),
                    (6, "TIME_ERASE_EVENT"),
                    (7, "Brown out detected (?)"),
                ] as &[(u8, _)],
            ),
            (
                0x0040,
                &[
                    (0, "Unit is off due to internal configuration"),
                    (1, "Unit is off due to primary remote control"),
                    (2, "Unit is off due to seconday remote control"),
                    (3, "Unit is off due to PMBus command"),
                    (4, "Unit is on due to internal configuration"),
                    (5, "Unit is on due to primary remote control"),
                    (6, "Unit is on due to seconday remote control"),
                    (7, "Unit is on due to PMBus command"),
                ],
            ),
            (0x0800, &[(0, "Power not good"), (1, "Power is good")]),
        ];

        // Always print the actual raw bits, even if they are 0.
        println!("  {: <18}  {:#04x}", "STATUS_MFR", e.status_mfr);

        if let Some(&(_, bits)) = mfr.iter().find(|&&(v, _)| v == status_word) {
            match bits.iter().find(|&&(v, _)| v == e.status_mfr) {
                Some(&(value, meaning)) => {
                    println!("    {value:#04x}: {meaning}");
                }
                None => {
                    println!("    {:#04x}: Unknown meaning!", e.status_mfr);
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
        out += &format!("{seconds}.{} sec", timestamp % 10);

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
