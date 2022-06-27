// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility monorail`
//! `humility monorail` exposes commands to interact with the management
//! network switch and PHYs.  It is for _management_ of the management network,
//! and can therefore only be run on two images:
//! - `app/sidecar/app.toml` running on Sidecar hardware
//! - `app/gimletlet/app-vsc7448.toml`, running on a Gimletlet which is
//!   attached to a VSC7448 dev kit (VSC5627EV) via SPI.  This setup is
//!   non-trivial; if you find yourself with a dev kit, talk to Matt about
//!   how to wire it up.
//!
//! Use `humility monorail -h` to see help, or `humility monorail status` for
//! a bird's-eye view of the ports.

use std::collections::{BTreeMap, BTreeSet};
use std::convert::TryInto;

use humility::core::Core;
use humility::hubris::*;
use humility::reflect::*;
use humility_cmd::hiffy::HiffyContext;
use humility_cmd::idol::{IdolArgument, IdolOperation};
use humility_cmd::{Archive, Attach, Run, RunUnattached, Validate};

use anyhow::{anyhow, bail, Result};
use clap::Command as ClapCommand;
use clap::{CommandFactory, Parser};
use colored::Colorize;
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
    /// Get info about a particular VSC7448 register
    Info {
        reg: String,
        #[clap(parse(try_from_str = parse_int::parse))]
        value: Option<u32>,
    },
    /// Print a table showing port status
    Status {
        /// arguments
        #[clap(long, short, use_value_delimiter = true)]
        ports: Vec<u8>,
    },
    /// Print or reset VSC7448 port counters
    Counters {
        #[clap(long, short)]
        port: u8,
        #[clap(long, short)]
        reset: bool,
    },
    /// Read a VSC7448 register
    Read { reg: String },
    /// Write to a VSC7448 register
    Write {
        reg: String,
        #[clap(parse(try_from_str = parse_int::parse))]
        value: u32,
    },
    /// Subcommand to control PHYs
    Phy {
        #[clap(subcommand)]
        cmd: PhyCommand,
    },
}

#[derive(Parser, Debug)]
enum PhyCommand {
    /// Get info about a particular PHY register
    Info { reg: String },
    /// Read from a register in the PHY associated with a particular VSC7448 port
    Read {
        #[clap(long, short)]
        port: u8,
        reg: String,
    },
    /// Write to a register in the PHY associated with a particular VSC7448 port
    Write {
        #[clap(long, short)]
        port: u8,
        reg: String,
        #[clap(parse(try_from_str = parse_int::parse))]
        value: u16,
    },
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

fn monorail_read(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
    reg: String,
) -> Result<()> {
    let reg = parse_reg_or_addr(&reg)?;
    let addr = reg.address();
    humility::msg!("Reading {} from 0x{:x}", reg, addr);

    let op = IdolOperation::new(hubris, "Monorail", "read_vsc7448_reg", None)?;
    let value = humility_cmd_hiffy::hiffy_call(
        hubris,
        core,
        context,
        &op,
        &[("addr", IdolArgument::Scalar(u64::from(addr)))],
    )?;
    match value {
        Ok(v) => {
            let value = if let Value::Base(Base::U32(v)) = v {
                v
            } else {
                bail!("Got bad reflected value: expected U32, got {:?}", v);
            };
            println!("{} => 0x{:x}", reg, value);

            // The VSC7448 is configured to return 0x88888888 if a register is
            // read too fast.  This should never happen, because the `monorail`
            // task configures appropriate padding bytes between setting the
            // target register and reading it back.
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
    Ok(())
}

fn monorail_write(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
    reg: String,
    value: u32,
) -> Result<()> {
    let reg = parse_reg_or_addr(&reg)?;
    let addr = reg.address();
    humility::msg!("Writing 0x{:x} to {} at 0x{:x}", value, reg, addr);
    pretty_print_fields(value, reg.fields());

    let op = IdolOperation::new(hubris, "Monorail", "write_vsc7448_reg", None)?;
    let value = humility_cmd_hiffy::hiffy_call(
        hubris,
        core,
        context,
        &op,
        &[
            ("addr", IdolArgument::Scalar(u64::from(addr))),
            ("value", IdolArgument::Scalar(u64::from(value))),
        ],
    )?;
    match value {
        Ok(v) => {
            if !matches!(v, Value::Base(Base::U0)) {
                bail!("Got unexpected value: expected (), got {:?}", v)
            }
        }
        Err(e) => {
            println!("Got error: {}", e);
        }
    }
    Ok(())
}

/// Helper struct for a fully decoded PHY register. This is analogous to
/// `vsc7448_info::parse::PhyRegister`, but flexibly allows for registers
/// that are unknown to the PAC.
struct ParsedPhyRegister {
    name: String,
    page: u16,
    reg: u8,
    fields: BTreeMap<String, Field<String>>,
}

/// Attempt to parse to known registers, then do a last-chance parse where we
/// just attempt to read numbers, e.g. 5:13 (as page:reg).
///
/// This is necessary because the SDK PHY headers don't necessarily include
/// every register in every PHY, and even lack some registers in the Microchip
/// PHY that's on the dev kit (!!)
fn parse_phy_register(reg: String) -> Result<ParsedPhyRegister> {
    let parsed = reg.parse::<PhyRegister>();
    if let Ok(reg) = parsed {
        return Ok(ParsedPhyRegister {
            page: reg.page_addr().try_into().unwrap(),
            reg: reg.reg_addr(),
            name: format!("{}", reg),
            fields: reg.fields().clone(),
        });
    }

    if let Ok(nums) =
        reg.split(':').map(|s| s.parse::<u16>()).collect::<Result<Vec<_>, _>>()
    {
        match nums.len() {
            1 => Ok(ParsedPhyRegister {
                page: 0,
                reg: nums[0].try_into().unwrap(),
                name: format!("0:{}", nums[0]),
                fields: BTreeMap::new(),
            }),
            2 => Ok(ParsedPhyRegister {
                page: nums[0],
                reg: nums[1].try_into().unwrap(),
                name: format!("{}:{}", nums[0], nums[1]),
                fields: BTreeMap::new(),
            }),
            _ => Err(vsc7448_info::parse::ParseError::TooManyItems.into()),
        }
    } else {
        // If the last-change parse failed, then just return the
        // previous parse error.
        Err(parsed.unwrap_err().into())
    }
}

fn monorail_phy_read(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
    port: u8,
    reg: String,
) -> Result<()> {
    let reg = parse_phy_register(reg)?;
    println!("Reading from port {} PHY, register {}", port, reg.name);
    let op = IdolOperation::new(hubris, "Monorail", "read_phy_reg", None)?;
    let value = humility_cmd_hiffy::hiffy_call(
        hubris,
        core,
        context,
        &op,
        &[
            ("port", IdolArgument::Scalar(u64::from(port))),
            ("page", IdolArgument::Scalar(u64::from(reg.page))),
            ("reg", IdolArgument::Scalar(u64::from(reg.reg))),
        ],
    )?;
    match value {
        Ok(v) => {
            let value = if let Value::Base(Base::U16(v)) = v {
                v
            } else {
                bail!("Got bad reflected value: expected U16, got {:?}", v);
            };
            println!("Got result 0x{:x}", value);
            pretty_print_fields(value as u32, &reg.fields);
        }
        Err(e) => {
            println!("Got error: {}", e);
        }
    }
    Ok(())
}

fn monorail_phy_write(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
    port: u8,
    reg: String,
    value: u16,
) -> Result<()> {
    let reg = parse_phy_register(reg)?;
    println!(
        "Writing 0x{:x} to port {} PHY, register {}",
        value, port, reg.name
    );
    pretty_print_fields(value as u32, &reg.fields);
    let op = IdolOperation::new(hubris, "Monorail", "write_phy_reg", None)?;
    let value = humility_cmd_hiffy::hiffy_call(
        hubris,
        core,
        context,
        &op,
        &[
            ("port", IdolArgument::Scalar(u64::from(port))),
            ("page", IdolArgument::Scalar(u64::from(reg.page))),
            ("reg", IdolArgument::Scalar(u64::from(reg.reg))),
            ("value", IdolArgument::Scalar(u64::from(value))),
        ],
    )?;
    match value {
        Ok(v) => {
            if !matches!(v, Value::Base(Base::U0)) {
                bail!("Got unexpected value: expected (), got {:?}", v)
            }
        }
        Err(e) => {
            println!("Got error: {}", e);
        }
    }
    Ok(())
}

fn monorail_status(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
    ports: &[u8],
) -> Result<()> {
    // We're going to get the status of *every* port, then just print the ones
    // that are specified by the caller; the time it takes to poll a single
    // port is dwarfed by the Hiffy call overhead, so no need to optimize.
    const NUM_PORTS: u8 = 53;
    let results = {
        let op_port =
            IdolOperation::new(hubris, "Monorail", "get_port_status", None)?;
        let op_phy =
            IdolOperation::new(hubris, "Monorail", "get_phy_status", None)?;
        use hif::*;
        let funcs = context.functions()?;
        let send = funcs.get("Send", 4)?;
        let mut ops = vec![];

        let label = Target(0);

        // We run two hiffy loops back to back, which have the same form:
        // - Monorail.get_port_status for every port
        // - Monorail.get_phy_status for every port
        //
        // They're placed back-to-back in Hiffy code, so we'll call the two
        // loops one after the other (i.e. values are not interleaved!)
        for op in [&op_port, &op_phy] {
            assert_eq!(op.args.members.len(), 1); // Sanity-check!
            let ret_size = hubris.typesize(op.ok)? as u32;

            // Here we go!
            ops.push(Op::Push32(op.task.task())); // Task id
            ops.push(Op::Push16(op.code)); // opcode
            ops.push(Op::Push(0)); // port (payload)
            ops.push(Op::Push(NUM_PORTS as u8)); // comparison target (dummy)
            ops.push(Op::Label(label));
            {
                ops.push(Op::Drop); // Drop comparison target
                ops.push(Op::Push(1)); // Payload length
                ops.push(Op::Push32(ret_size)); // Return size
                ops.push(Op::Call(send.id));
                ops.push(Op::DropN(2)); // Drop payload and return size
                ops.push(Op::Push(1)); // Increment by one
                ops.push(Op::Add); // port = port + 1
                ops.push(Op::Push(NUM_PORTS as u8)); // Comparison target
                ops.push(Op::BranchGreaterThan(label)); // Jump to loop start
            }
            ops.push(Op::DropN(4)); // Cleanup
        }
        ops.push(Op::Done); // Finish
                            //
        let mut results = context.run(core, ops.as_slice(), None)?;
        let phy_results = results.split_off(NUM_PORTS as usize);
        let port_results = results;
        assert_eq!(port_results.len(), NUM_PORTS as usize);
        assert_eq!(phy_results.len(), NUM_PORTS as usize);

        let port_results = port_results
            .into_iter()
            .map(move |r| humility_cmd_hiffy::hiffy_decode(hubris, &op_port, r))
            .collect::<Result<Vec<Result<_, _>>>>()?;
        let phy_results = phy_results
            .into_iter()
            .map(move |r| humility_cmd_hiffy::hiffy_decode(hubris, &op_phy, r))
            .collect::<Result<Vec<Result<_, _>>>>()?;

        // Decode the port and phy status values into reflect::Value
        port_results
            .into_iter()
            .zip(phy_results.into_iter())
            .collect::<Vec<_>>()
    };

    // Convert ports in a lookup-friendly structure
    let ports = ports.iter().collect::<BTreeSet<_>>();

    // Helper functions:
    let decode_mode = |value: &Value| match value {
        Value::Enum(m) => {
            let mode = m.disc().to_uppercase();
            let speed = m.contents().and_then(|speed| match speed {
                Value::Tuple(t) => t.get(0).map(|t| match t {
                    Value::Enum(t) => t.disc().replace("Speed", ""),
                    v => panic!("Expected enum, got {:?}", v),
                }),
                v => panic!("Expected tuple, got {:?}", v),
            });
            (mode, speed.unwrap_or_else(|| "--".to_owned()))
        }
        v => panic!("Expected enum, got {:?}", v),
    };
    // Extracts a device name from a reflected value, e.g. "DEV1G_0"
    let decode_dev = |value: &Value| match value {
        Value::Tuple(dev) => {
            let d = match &dev[0] {
                Value::Enum(d) => d.disc(),
                d => panic!("Could not get enum from {:?}", d),
            };
            let n = match &dev[1] {
                Value::Base(Base::U8(n)) => n,
                d => panic!("Could not get U8 from {:?}", d),
            };
            format!("{}_{}", d.to_uppercase(), n)
        }
        dev => panic!("Expected tuple, got {:?}", dev),
    };

    let fmt_link = |b| if b { "up".green() } else { "down".red() };
    println!("{}", "PORT | MODE    SPEED  DEV     SERDES  LINK |   PHY    MAC LINK  MEDIA LINK".bold());
    println!("{}", "-----|-------------------------------------|-------------------------------".bold());
    for (port, (port_value, phy_value)) in (0..NUM_PORTS).zip(results) {
        if !ports.is_empty() && !ports.contains(&port) {
            continue;
        }
        print!(" {:<3} | ", port);
        match port_value {
            Ok(v) => match v {
                Value::Struct(s) => {
                    assert_eq!(s.name(), "PortStatus");
                    let (dev, serdes, mode, speed) = match &s["cfg"] {
                        Value::Struct(cfg) => {
                            assert_eq!(cfg.name(), "PortConfig");
                            let dev = decode_dev(&cfg["dev"]);
                            let serdes = decode_dev(&cfg["serdes"]);
                            let (mode, speed) = decode_mode(&cfg["mode"]);
                            (
                                dev.replace("DEV", ""),
                                serdes.replace("SERDES", ""),
                                mode,
                                speed,
                            )
                        }
                        v => panic!("Expected Struct, got {:?}", v),
                    };
                    let link_up = match &s["link_up"] {
                        Value::Base(Base::Bool(b)) => b,
                        b => panic!("Could not get bool from {:?}", b),
                    };

                    let fmt_mode = match mode.as_str() {
                        "SGMII" => mode.cyan(),
                        "QSGMII" => mode.blue(),
                        "SFI" => mode.magenta(),
                        v => v.into(),
                    };

                    print!(
                        "{:<6}  {:<5}  {:<6}  {:<6}  {:<4}",
                        fmt_mode,
                        speed,
                        dev,
                        serdes,
                        fmt_link(*link_up)
                    )
                }
                v => panic!("Expected Struct, got {:?}", v),
            },
            Err(e) => {
                if e == "UnconfiguredPort" {
                    print!(
                        "{}",
                        "--      --     --      --      --  ".dimmed()
                    );
                } else {
                    println!("Got unexpected error {}", e);
                }
            }
        }
        print!(" | ");
        match phy_value {
            Ok(v) => match v {
                Value::Struct(s) => {
                    assert_eq!(s.name(), "PhyStatus");
                    let phy_ty = match &s["ty"] {
                        Value::Enum(e) => e.disc().to_uppercase(),
                        v => panic!("Expected struct, got {:?}", v),
                    };
                    let mac_link_up = match &s["mac_link_up"] {
                        Value::Base(Base::Bool(b)) => b,
                        b => panic!("Could not get bool from {:?}", b),
                    };
                    let media_link_up = match &s["media_link_up"] {
                        Value::Base(Base::Bool(b)) => b,
                        b => panic!("Could not get bool from {:?}", b),
                    };
                    println!(
                        "{:<6}  {:<8}  {:<10}",
                        phy_ty,
                        fmt_link(*mac_link_up),
                        fmt_link(*media_link_up),
                    )
                }
                v => panic!("Expected Struct, got {:?}", v),
            },
            Err(e) => {
                if e == "UnconfiguredPort" || e == "NoPhy" {
                    println!("{}", "--       --         --".dimmed());
                } else {
                    println!("Got unexpected error {}", e);
                }
            }
        }
    }
    Ok(())
}

fn monorail_reset_counters(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
    port: u8,
) -> Result<()> {
    let op =
        IdolOperation::new(hubris, "Monorail", "reset_port_counters", None)?;
    let value = humility_cmd_hiffy::hiffy_call(
        hubris,
        core,
        context,
        &op,
        &[("port", IdolArgument::Scalar(u64::from(port)))],
    )?;
    value.map(|_| ()).map_err(|err| anyhow!("Got error {}", err))
}

fn monorail_counters(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
    port: u8,
) -> Result<()> {
    let op = IdolOperation::new(hubris, "Monorail", "get_port_counters", None)?;
    let value = humility_cmd_hiffy::hiffy_call(
        hubris,
        core,
        context,
        &op,
        &[("port", IdolArgument::Scalar(u64::from(port)))],
    )?;
    let decode_count = |s: &Value| match s {
        Value::Struct(s) => {
            let mc = match &s["multicast"] {
                Value::Base(Base::U32(v)) => *v,
                v => panic!("Expected U32, got {:?}", v),
            };
            let uc = match &s["unicast"] {
                Value::Base(Base::U32(v)) => *v,
                v => panic!("Expected U32, got {:?}", v),
            };
            let bc = match &s["broadcast"] {
                Value::Base(Base::U32(v)) => *v,
                v => panic!("Expected U32, got {:?}", v),
            };
            (mc, uc, bc)
        }
        s => panic!("Expected Struct, got {:?}", s),
    };

    match value {
        Ok(v) => {
            match v {
                Value::Struct(s) => {
                    let (rx_mc, rx_uc, rx_bc) = decode_count(&s["rx"]);
                    let (tx_mc, tx_uc, tx_bc) = decode_count(&s["tx"]);
                    println!("{} (port {})", "Packet counters:".bold(), port);
                    println!("  Receive:");
                    println!("    Unicast:   {}", rx_uc);
                    println!("    Multicast: {}", rx_mc);
                    println!("    Broadcast:  {}", rx_bc);
                    println!("  Transmit:");
                    println!("    Unicast:   {}", tx_uc);
                    println!("    Multicast: {}", tx_mc);
                    println!("    Broadcast:  {}", tx_bc);
                }
                s => panic!("Expected struct, got {:?}", s),
            };
        }
        Err(e) => {
            println!("Got error: {}", e);
        }
    }
    Ok(())
}

fn monorail(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    subargs: &[String],
) -> Result<()> {
    let subargs = MonorailArgs::try_parse_from(subargs)?;
    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;
    match subargs.cmd {
        Command::Info { .. } => panic!("Called monorail with info subcommand"),
        Command::Status { ports } => {
            monorail_status(hubris, core, &mut context, &ports)?
        }
        Command::Counters { port, reset } => {
            if reset {
                monorail_reset_counters(hubris, core, &mut context, port)?
            } else {
                monorail_counters(hubris, core, &mut context, port)?
            }
        }

        Command::Read { reg } => {
            monorail_read(hubris, core, &mut context, reg)?
        }
        Command::Write { reg, value } => {
            monorail_write(hubris, core, &mut context, reg, value)?
        }

        Command::Phy { cmd } => match cmd {
            PhyCommand::Read { port, reg } => {
                monorail_phy_read(hubris, core, &mut context, port, reg)?
            }
            PhyCommand::Write { port, reg, value } => monorail_phy_write(
                hubris,
                core,
                &mut context,
                port,
                reg,
                value,
            )?,
            _ => panic!("Invalid PHY command"),
        },
    };
    Ok(())
}

fn monorail_get_info(
    hubris: &mut HubrisArchive,
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
            run: Run::Subargs(monorail),
        },
        MonorailArgs::command(),
    );
    let subcmd_unattached = (
        humility_cmd::Command::Unattached {
            name: "monorail",
            archive: Archive::Ignored,
            run: RunUnattached::Subargs(monorail_get_info),
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
