// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
use std::collections::BTreeMap;
use std::convert::TryInto;

use humility::core::Core;
use humility::hubris::*;
use humility_cmd::hiffy::HiffyContext;
use humility_cmd::idol::{IdolArgument, IdolOperation};
use humility_cmd::{Archive, Args, Attach, Validate};

use anyhow::{anyhow, bail, Result};
use clap::Command as ClapCommand;
use clap::{CommandFactory, Parser};
use hif::*;
use vsc7448_info::parse::{PhyRegister, TargetRegister};
use vsc7448_types::Field;

#[derive(Parser, Debug)]
#[clap(name = "monorail", about = env!("CARGO_PKG_DESCRIPTION"))]
struct MonorailArgs {
    /// sets timeout
    #[clap(
        long, short = 'T', default_value = "5000", value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    #[clap(subcommand)]
    cmd: Command,
}

impl MonorailArgs {
    /// Checks whether the given subcommand requires an attached target
    /// (i.e. a microcontroller running Hubris, connected through a debugger)
    fn requires_target(&self) -> bool {
        !matches!(
            &self.cmd,
            Command::Info { .. }
                | Command::Phy { cmd: PhyCommand::Info { .. }, .. }
        )
    }
}

#[derive(Parser, Debug)]
enum Command {
    Info {
        reg: String,
        #[clap(parse(try_from_str = parse_int::parse))]
        value: Option<u32>,
    },
    Read {
        reg: String,
    },
    Write {
        reg: String,
        #[clap(parse(try_from_str = parse_int::parse))]
        value: u32,
    },
    Phy {
        #[clap(subcommand)]
        cmd: PhyCommand,
    },
}

#[derive(Parser, Debug)]
enum PhyCommand {
    Info {
        reg: String,
    },
    Read {
        #[clap(long, short)]
        port: u8,
        reg: String,
    },
    Write {
        #[clap(long, short)]
        port: u8,
        reg: String,
        #[clap(parse(try_from_str = parse_int::parse))]
        value: u16,
    },
}

#[derive(Parser, Debug)]
struct PhyAddr {
    #[clap(help = "MIIM peripheral (0-2)")]
    miim: u8,
    #[clap(help = "PHY address")]
    phy: u8,
}

/// Parses either a register address (as an integer or hex literal), or a
/// string representing a register's name.
fn parse_reg_or_addr(s: &str) -> Result<TargetRegister> {
    let reg = if let Ok(addr) = parse_int::parse(s) {
        TargetRegister::from_addr(addr)?
    } else {
        s.parse()?
    };
    Ok(reg)
}

fn pretty_print_fields(value: u32, fields: &BTreeMap<String, Field<String>>) {
    let mut field_keys = fields.keys().collect::<Vec<_>>();
    if field_keys.is_empty() {
        return;
    }
    field_keys.sort_by(|a, b| fields[*b].lo.cmp(&fields[*a].lo));
    println!("  bits |    value   | field");
    for f in field_keys {
        let field = &fields[f];
        let bits = (value & ((1u64 << field.hi) - 1) as u32) >> field.lo;
        println!(" {:>2}:{:<2} | 0x{:<8x} | {}", field.hi, field.lo, bits, f);
    }
}

/// Helper struct to work with a connected VSC7448 ethernet switch IC
struct Monorail<'a> {
    hubris: &'a HubrisArchive,
    core: &'a mut dyn Core,
    context: HiffyContext<'a>,
}

impl<'a> Monorail<'a> {
    pub fn new(
        hubris: &'a HubrisArchive,
        core: &'a mut dyn Core,
        args: &MonorailArgs,
    ) -> Result<Self> {
        let mut context = HiffyContext::new(hubris, core, args.timeout)?;

        Ok(Self { hubris, core, context })
    }

    /// Writes a single 32-bit register
    fn write(&mut self, addr: u32, data: u32) -> Result<()> {
        todo!()
    }

    /// Reads a single 32-bit register
    fn read(&mut self, addr: u32) -> Result<Result<Vec<u8>, String>> {
        let op = IdolOperation::new(
            self.hubris,
            "Monorail",
            "read_vsc7448_reg",
            None, // TODO?
        )?;
        humility_cmd_hiffy::hiffy_call(
            self.hubris,
            self.core,
            &mut self.context,
            &op,
            &[("addr", IdolArgument::Scalar(u64::from(addr)))],
        )
    }

    fn phy_read(&mut self, port: u8, page: u16, reg: u8) -> Result<u16> {
        todo!()
    }

    fn phy_write(
        &mut self,
        port: u8,
        page: u16,
        reg: u8,
        data: u16,
    ) -> Result<()> {
        todo!()
    }
}

fn monorail(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    _args: &Args,
    subargs: &[String],
) -> Result<()> {
    let subargs = MonorailArgs::try_parse_from(subargs)?;
    let mut vsc = Monorail::new(hubris, core, &subargs)?;
    match subargs.cmd {
        Command::Read { reg } => {
            let reg = parse_reg_or_addr(&reg)?;
            let addr = reg.address();
            humility::msg!("Reading {} from 0x{:x}", reg, addr);
            let value = vsc.read(addr)?;
            match value {
                Ok(v) => {
                    assert_eq!(v.len(), 4);
                    let value = u32::from_le_bytes(v.try_into().unwrap());
                    println!("{} => 0x{:x}", reg, value);
                    if value == 0x88888888 {
                        log::warn!(
                            "0x88888888 typically indicates a communication issue!"
                        );
                    }
                    pretty_print_fields(value, reg.fields());
                }
                Err(e) => {
                    println!("Got error: {}", e);
                }
            }
        }
        Command::Info { .. } => panic!("Called monorail with info subcommand"),
        Command::Write { reg, value } => {
            let reg = parse_reg_or_addr(&reg)?;
            let addr = reg.address();
            humility::msg!("Writing 0x{:x} to {} at 0x{:x}", value, reg, addr);
            pretty_print_fields(value, reg.fields());

            vsc.write(addr, value)?;
        }
        Command::Phy { cmd } => {
            let dummy_fields = BTreeMap::new();
            let (reg, port) = match &cmd {
                PhyCommand::Write { reg, port, .. }
                | PhyCommand::Read { reg, port } => (reg, *port),
                _ => panic!("Invalid PHY command"),
            };

            // Attempt to parse to known registers, then do a last-chance parse
            // where we just attempt to read numbers, e.g. 5:13 (as page:reg).
            //
            // This is necessary because the SDK PHY headers don't necessarily
            // include every register in every PHY, and even lack some registers
            // in the Microchip PHY that's on the dev kit (!!)
            let parsed = reg.parse::<PhyRegister>();
            let (page, reg, name, fields) = if let Ok(reg) = parsed {
                (
                    reg.page_addr().try_into().unwrap(),
                    reg.reg_addr(),
                    format!("{}", reg),
                    reg.fields(),
                )
            } else if let Ok(nums) = reg
                .split(':')
                .map(|s| s.parse::<u16>())
                .collect::<Result<Vec<_>, _>>()
            {
                match nums.len() {
                    1 => (
                        0,
                        nums[0].try_into().unwrap(),
                        format!("0:{}", nums[0]),
                        &dummy_fields,
                    ),
                    2 => (
                        nums[0],
                        nums[1].try_into().unwrap(),
                        format!("{}:{}", nums[0], nums[1]),
                        &dummy_fields,
                    ),
                    _ => {
                        return Err(
                            vsc7448_info::parse::ParseError::TooManyItems
                                .into(),
                        );
                    }
                }
            } else {
                // If the last-change parse failed, then just return the
                // previous parse error.
                assert!(parsed.is_err());
                parsed?;
                unreachable!()
            };
            match cmd {
                PhyCommand::Write { value, .. } => {
                    println!(
                        "Writing 0x{:x} to port {} PHY, register {}",
                        value, port, name,
                    );
                    pretty_print_fields(value as u32, fields);
                    vsc.phy_write(port, page, reg, value)?;
                }
                PhyCommand::Read { .. } => {
                    println!(
                        "Reading from port {} PHY, register {}",
                        port, name,
                    );
                    let out = vsc.phy_read(port, page, reg)?;
                    println!("Got result 0x{:x}", out);
                    pretty_print_fields(out as u32, fields);
                }
                _ => panic!("Invalid PHY command"),
            }
        }
    };
    Ok(())
}

fn monorail_get_info(
    hubris: &mut HubrisArchive,
    _args: &Args,
    subargs: &[String],
) -> Result<()> {
    assert!(!hubris.loaded());
    let subargs = MonorailArgs::try_parse_from(subargs)?;
    match subargs.cmd {
        Command::Info { reg, value } => {
            let reg = parse_reg_or_addr(&reg)?;
            println!("Register {}", reg);
            println!("Register address: 0x{:x}", reg.address());

            if let Some(v) = value {
                println!("Register value: 0x{:x}", v);
                pretty_print_fields(v, reg.fields());
            } else {
                println!("  bits |    field");
                for (f, field) in reg.fields() {
                    println!(" {:>2}:{:<2} | {}", field.hi, field.lo, f);
                }
            }
        }
        Command::Phy { cmd: PhyCommand::Info { reg } } => {
            let reg: PhyRegister = reg.parse()?;
            println!("PHY register: {}", reg);
        }
        _ => panic!("Called monorail_get_info without info subcommand"),
    }
    Ok(())
}

pub fn init() -> (humility_cmd::Command, ClapCommand<'static>) {
    // We do a bonus parse of the command-line arguments here to see if we're
    // doing a `monorail info` subcommand, which doesn't require a Hubris image
    // or attached device; skipping those steps improves runtime (especially
    // in debug builds)
    let subcmd_attached = (
        humility_cmd::Command::Attached {
            name: "monorail",
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
            run: monorail,
        },
        MonorailArgs::command(),
    );
    let subcmd_unattached = (
        humility_cmd::Command::Unattached {
            name: "monorail",
            archive: Archive::Ignored,
            run: monorail_get_info,
        },
        MonorailArgs::command(),
    );

    // If there's a `monorail` subcommand, then attempt to parse the subcmd
    let mut args = std::env::args().skip_while(|a| a != "monorail").peekable();
    if args.peek().is_some() {
        if let Ok(args) = MonorailArgs::try_parse_from(args) {
            if !args.requires_target() {
                return subcmd_unattached;
            }
        } else {
            // If the argument parse failed, then return the faster subcommand
            // so that we don't have to attach to a device then fail parsing
            // again.
            return subcmd_unattached;
        }
    }
    subcmd_attached
}
