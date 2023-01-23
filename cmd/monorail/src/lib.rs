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
//!
//! #### `humility monorail read`
//! The `read` subcommand allows you to read a register from the VSC7448 switch
//! IC.  Registers can be specified in a few ways, but most of the time, you'll
//! want to execute a read by register name:
//!
//!```console
//! matt@niles ~ () $ export HUMILITY_TARGET=sidecar
//! matt@niles ~ (sidecar) $ pfexec humility monorail read DEV1G[0]:DEV_RST_CTRL
//! humility: attached to 0483:374f:002A001C4D46500F20373033 via ST-Link V3
//! humility: Reading DEV1G[0]:DEV_CFG_STATUS:DEV_RST_CTRL from 0x71040000
//! DEV1G[0]:DEV_CFG_STATUS:DEV_RST_CTRL => 0x100000
//!   bits |    value   | field
//!  21:20 | 0x1        | SPEED_SEL
//!     12 | 0x0        | PCS_TX_RST
//!      8 | 0x0        | PCS_RX_RST
//!      4 | 0x0        | MAC_TX_RST
//!      0 | 0x0        | MAC_RX_RST
//! ```
//! It's not necessary to use the fully qualified `TARGET:GROUP:REGISTER` form;
//! any unambiguous subset will work (e.g. in the example above, we used
//! `DEV1G[0]:DEV_RST_CTRL` instead of the full
//! `DEV1G[0]:DEV_CFG_STATUS:DEV_RST_CTRL`).  If your register name is not
//! unambiguous, an error will be printed.
//!
//! Register names can be found in the
//! [`vsc7448-pac` crate](https://github.com/oxidecomputer/vsc7448/tree/master/vsc7448-pac),
//! which is automatically generated from Microchip's C SDK header files.
//!
//! It's also possible to execute reads by raw address:
//! ```console
//! matt@niles ~ (sidecar) $ pfexec humility monorail read 0x71040000
//! humility: attached to 0483:374f:002A001C4D46500F20373033 via ST-Link V3
//! humility: Reading DEV1G[0]:DEV_CFG_STATUS:DEV_RST_CTRL from 0x71040000
//! DEV1G[0]:DEV_CFG_STATUS:DEV_RST_CTRL => 0x100000
//!   bits |    value   | field
//!  21:20 | 0x1        | SPEED_SEL
//!     12 | 0x0        | PCS_TX_RST
//!      8 | 0x0        | PCS_RX_RST
//!      4 | 0x0        | MAC_TX_RST
//!      0 | 0x0        | MAC_RX_RST
//! ```
//!
//! #### `humility monorail write`
//! Modifies a register in the VSC7448 switch.  Note that there is no checking
//! of read/write vs read-only registers; good luck!
//!
//! #### `humility monorail info`
//! The `info` subcommand looks up info on a register in the VSC7448 switch IC.
//! This command is offline, meaning it does not require an attached system.
//!
//! ```console
//! matt@niles ~ (sidecar) $ pfexec humility monorail info HW_QSGMII_CFG
//! Register HSIO:HW_CFGSTAT:HW_QSGMII_CFG
//! Register address: 0x71460170
//!   bits |    field
//!     13 | E_DET_ENA
//!   11:0 | FLIP_LANES
//!     14 | SHYST_DIS
//!     12 | USE_I1_ENA
//! ```
//!
//! If you provide a value as the final argument, it will decode the register
//! and pretty-print a table:
//! ```console
//! matt@niles ~ (sidecar) $ pfexec humility monorail info HW_QSGMII_CFG 0x2000
//! Register HSIO:HW_CFGSTAT:HW_QSGMII_CFG
//! Register address: 0x71460170
//! Register value: 0x2000
//!   bits |    value   | field
//!     14 | 0x0        | SHYST_DIS
//!     13 | 0x1        | E_DET_ENA
//!     12 | 0x0        | USE_I1_ENA
//!   11:0 | 0x0        | FLIP_LANES
//! ```
//!
//! #### `humility monorail status`
//! Prints a table showing the status of every port in the system, along with
//! their PHY (if present).  The `-p` argument allows you to specify a subset of
//! ports, e.g.
//! ```console
//! matt@niles ~ (sidecar) $ pfexec humility monorail status -p40,41,42,43,44,45
//! humility: attached to 0483:374f:002A001C4D46500F20373033 via ST-Link V3
//! PORT | MODE    SPEED  DEV     SERDES  LINK |   PHY    MAC LINK  MEDIA LINK
//! -----|-------------------------------------|-------------------------------
//!  40  | QSGMII  100M   1G_16   6G_14   up   | VSC8504  down      down
//!  41  | QSGMII  100M   1G_17   6G_14   up   | VSC8504  down      up
//!  42  | QSGMII  100M   1G_18   6G_14   up   | VSC8504  down      down
//!  43  | QSGMII  100M   1G_19   6G_14   up   | VSC8504  down      down
//!  44  | QSGMII  1G     1G_20   6G_15   up   | VSC8562  up        up
//!  45  | QSGMII  1G     1G_21   6G_15   up   | VSC8562  up        up
//!  ```
//!
//! #### `humility monorail dump`
//! Dumps an entire top-level target on the VSC7448, e.g. `DEV1G[0]`
//! ```console
//!
//! matt@niles ~ (sidecar) $ h monorail dump DEV1G[0]
//! humility: attached to 0483:374f:002A001C4D46500F20373033 via ST-Link V3
//! Dumping target DEV1G[0] (0x71040000 -> 0x710400a0)
//! DEV1G[0]:DEV_CFG_STATUS:DEV_RST_CTRL    0x00100000
//! DEV1G[0]:DEV_CFG_STATUS:DEV_STICKY    0x00004000
//! DEV1G[0]:DEV_CFG_STATUS:DEV_DBG_CFG    0x00000800
//! DEV1G[0]:DEV_CFG_STATUS:DEV_PORT_PROTECT    0x00000000
//! DEV1G[0]:DEV_CFG_STATUS:EEE_CFG    0x0011940a
//! DEV1G[0]:DEV_CFG_STATUS:PTP_CFG    0x00400000
//! DEV1G[0]:DEV_CFG_STATUS:PTP_EVENTS    0x00000000
//! ...etc
//! ```
//!
//! This subcommand is rather fragile; large targets may overflow the HIF return
//! stack, and it's possible to access invalid registers.
//!
//! #### `humility monorail mac`
//! Prints the MAC table of the VSC7448 switch.  This table shows which MAC
//! addresses have be learned on each port.
//! ```console
//! matt@niles ~ (sidecar) $ pfexec ./humility monorail mac
//! humility: attached to 0483:374f:002A001C4D46500F20373033 via ST-Link V3
//! Reading 3 MAC addresses...
//!  PORT |        MAC
//! ------|-------------------
//!    18 | 0e:1d:23:88:1a:3b
//!    41 | 0e:1d:7f:c3:07:31
//!    48 | 0e:1d:15:70:3d:bb
//! ```
//!
//! #### `humility monorail counters`
//! Prints or resets (with `-r`) counters for a port on the VSC7448.
//! ```console
//! matt@niles ~ (sidecar) $ pfexec humility monorail counters -p48
//! humility: attached to 0483:374f:002A001C4D46500F20373033 via ST-Link V3
//! Packet counters: (port 48)
//!   Receive:
//!     Unicast:   0
//!     Multicast: 1049
//!     Broadcast:  0
//!   Transmit:
//!     Unicast:   0
//!     Multicast: 2099
//!     Broadcast:  0
//! ```
//!
//! #### `humility monorail phy`
//! Subcommand to interact with PHYs on a per-port basis.  Supports `read`,
//! `write`, `info`, and `dump` sub-subcommands, which behave similarly to the
//! commands to interact with VSC7448 registers.
//!
//! PHY register names are also found in the
//! [`vsc7448-pac` crate](https://github.com/oxidecomputer/vsc7448/tree/master/vsc7448-pac/src/phy).

use std::collections::{BTreeMap, BTreeSet};
use std::convert::TryInto;

use cmd_hiffy as humility_cmd_hiffy;

use humility::cli::Subcommand;
use humility::core::Core;
use humility::reflect::*;
use humility::{hubris::*, ExecutionContext};
use humility_cmd::hiffy::HiffyContext;
use humility_cmd::idol::{HubrisIdol, IdolArgument};
use humility_cmd::{Archive, Attach, CommandKind, Validate};

use anyhow::{anyhow, bail, Result};

use clap::{CommandFactory, Parser};
use colored::Colorize;
use vsc7448_info::parse::{PhyRegister, TargetRegister};
use vsc7448_types::Field;

#[derive(Parser, Debug)]
#[clap(name = "monorail", about = env!("CARGO_PKG_DESCRIPTION"))]
struct MonorailArgs {
    /// sets timeout
    #[clap(
        long, short = 'T', default_value_t = 5000, value_name = "timeout_ms",
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
    /// Get information about a particular VSC7448 register group
    Dump { target: String },
    /// Dumps MAC tables in the switch
    Mac,
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
    /// Dumps all PHY registers on the given port
    Dump {
        #[clap(long, short)]
        port: u8,
        #[clap(long, short)]
        verbose: bool,
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

fn pretty_print_fields(
    value: u32,
    fields: &BTreeMap<String, Field<String>>,
    indent: usize,
) {
    let mut field_keys = fields.keys().collect::<Vec<_>>();
    if field_keys.is_empty() {
        return;
    }
    field_keys.sort_by(|a, b| fields[*b].lo.cmp(&fields[*a].lo));
    println!("{:indent$}  bits |    value   | field", "");
    for f in field_keys {
        let field = &fields[f];
        let bits = (value & ((1u64 << field.hi) - 1) as u32) >> field.lo;
        print!("{:indent$} ", "");
        let range = if field.hi > field.lo + 1 {
            format!("{}:{}", field.hi - 1, field.lo)
        } else {
            format!("{}", field.lo)
        };
        println!("{:>5} | 0x{:<8x} | {}", range, bits, f);
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
    humility::msg!("Reading {} from {:#x}", reg, addr);

    let op = hubris.get_idol_command("Monorail.read_vsc7448_reg")?;
    let value = humility_cmd_hiffy::hiffy_call(
        hubris,
        core,
        context,
        &op,
        &[("addr", IdolArgument::Scalar(u64::from(addr)))],
        None,
    )?;
    match value {
        Ok(v) => {
            let value = if let Value::Base(Base::U32(v)) = v {
                v
            } else {
                bail!("Got bad reflected value: expected U32, got {:?}", v);
            };
            println!("{} => {:#x}", reg, value);

            // The VSC7448 is configured to return 0x88888888 if a register is
            // read too fast.  This should never happen, because the `monorail`
            // task configures appropriate padding bytes between setting the
            // target register and reading it back.
            if value == 0x88888888 {
                log::warn!(
                    "0x88888888 typically indicates a communication issue!"
                );
            }
            pretty_print_fields(value, reg.fields(), 0);
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
    humility::msg!("Writing {:#x} to {} at {:#x}", value, reg, addr);
    pretty_print_fields(value, reg.fields(), 0);

    let op = hubris.get_idol_command("Monorail.write_vsc7448_reg")?;
    let value = humility_cmd_hiffy::hiffy_call(
        hubris,
        core,
        context,
        &op,
        &[
            ("addr", IdolArgument::Scalar(u64::from(addr))),
            ("value", IdolArgument::Scalar(u64::from(value))),
        ],
        None,
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
#[derive(Debug)]
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
fn parse_phy_register(reg: &str) -> Result<ParsedPhyRegister> {
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
    let reg = parse_phy_register(&reg)?;
    println!("Reading from port {} PHY, register {}", port, reg.name);
    let op = hubris.get_idol_command("Monorail.read_phy_reg")?;
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
        None,
    )?;
    match value {
        Ok(v) => {
            let value = v
                .as_base()?
                .as_u16()
                .ok_or_else(|| anyhow!("Could not get U16 from {:?}", v))?;
            println!("Got result {:#x}", value);
            pretty_print_fields(value as u32, &reg.fields, 0);
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
    let reg = parse_phy_register(&reg)?;
    println!(
        "Writing {:#x} to port {} PHY, register {}",
        value, port, reg.name
    );
    pretty_print_fields(value as u32, &reg.fields, 0);
    let op = hubris.get_idol_command("Monorail.write_phy_reg")?;
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
        None,
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

fn monorail_phy_dump(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
    port: u8,
    verbose: bool,
) -> Result<()> {
    use hif::*;
    let op = hubris.get_idol_command("Monorail.read_phy_reg")?;

    let funcs = context.functions()?;
    let send = funcs.get("Send", 4)?;
    let ret_size = hubris.typesize(op.ok)? as u32;
    assert_eq!(ret_size, 2);

    let mut ops = vec![];
    let label = Target(0);

    struct Page {
        name: &'static str,
        page: u16,
        reg_start: u8,
        reg_end: u8,
    }
    const PAGES: [Page; 5] = [
        Page { name: "STANDARD", page: 0, reg_start: 0, reg_end: 30 },
        Page { name: "EXTENDED_1", page: 1, reg_start: 16, reg_end: 30 },
        Page { name: "EXTENDED_2", page: 2, reg_start: 16, reg_end: 30 },
        Page { name: "EXTENDED_3", page: 3, reg_start: 16, reg_end: 30 },
        Page { name: "GPIO", page: 16, reg_start: 0, reg_end: 30 },
    ];
    // Loop over all page registers
    for page in &PAGES {
        assert!(page.reg_start < page.reg_end);
        // The function takes arguments in the order
        //      port: u8
        //      page: u16
        //      reg: u8
        ops.push(Op::Push32(op.task.task()));
        ops.push(Op::Push16(op.code));
        ops.push(Op::Push(port)); // Port and page don't change
        for b in page.page.to_le_bytes() {
            ops.push(Op::Push(b));
        }
        ops.push(Op::Push(page.reg_start)); // Initial register
        ops.push(Op::Push(page.reg_end)); // comparison target (dummy)
        ops.push(Op::Label(label));
        {
            ops.push(Op::Drop); // Drop comparison target
            ops.push(Op::Push(4)); // Payload length
            ops.push(Op::Push32(ret_size)); // Payload length
            ops.push(Op::Call(send.id));
            ops.push(Op::DropN(2)); // Drop payload and return size
            ops.push(Op::Push(1)); // Increment register by one
            ops.push(Op::Add); // reg = reg + 1
            ops.push(Op::Push(page.reg_end)); // Comparison target
            ops.push(Op::BranchGreaterThan(label));
        }
        ops.push(Op::DropN(6)); // Cleanup
    }
    ops.push(Op::Done);

    let results = context.run(core, ops.as_slice(), None)?;
    let mut iter = results.into_iter();
    for page in PAGES {
        println!("\n{}", page.name);
        for r in page.reg_start..page.reg_end {
            let v: Vec<u8> = iter.next().unwrap().unwrap();
            let value = u16::from_le_bytes([v[0], v[1]]);
            print!("  {:>2}: {:#06x}", r, value);

            let raw_name = format!("{}:{}", page.page, r);
            let parsed = parse_phy_register(&raw_name).unwrap();
            if parsed.name != raw_name {
                println!("  {}", parsed.name.split(':').nth(1).unwrap());
                if verbose {
                    pretty_print_fields(value as u32, &parsed.fields, 4);
                }
            } else {
                println!()
            }
        }
    }
    Ok(())
}

fn monorail_dump(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
    start_address: u32,
    end_address: u32,
) -> Result<()> {
    let results = {
        use hif::*;
        let op_read = hubris.get_idol_command("Monorail.read_vsc7448_reg")?;

        let funcs = context.functions()?;
        let send = funcs.get("Send", 4)?;
        let mut ops = vec![];

        let label = Target(0);
        let ret_size = hubris.typesize(op_read.ok)? as u32;
        ops.push(Op::Push32(op_read.task.task())); // task id
        ops.push(Op::Push16(op_read.code)); // opcode
        ops.push(Op::Push32(start_address)); // Starting register address
        ops.push(Op::Push32(0)); // Comparison target (dummy)
        ops.push(Op::Label(label));
        {
            ops.push(Op::Drop); // Drop comparison target

            // Expand u32 -> [u8; 4], since that's expected by `send`
            ops.push(Op::Expand32);
            ops.push(Op::Push(4)); // Payload size
            ops.push(Op::Push32(ret_size)); // Return size
            ops.push(Op::Call(send.id));
            ops.push(Op::DropN(2)); // Drop payload and return size
            ops.push(Op::Collect32);
            ops.push(Op::Push(4)); // Increment by four
            ops.push(Op::Add); // address = address + 4
            ops.push(Op::Push32(end_address)); // Comparison target
            ops.push(Op::BranchGreaterThan(label)); // Jump to loop start
        }
        ops.push(Op::DropN(4)); // Cleanup
        ops.push(Op::Done); // Finish

        let results = context.run(core, ops.as_slice(), None)?;
        results
            .into_iter()
            .map(move |r| humility_cmd_hiffy::hiffy_decode(hubris, &op_read, r))
            .collect::<Result<Vec<Result<_, _>>>>()?
    };
    for (i, v) in results.iter().enumerate() {
        let value = if let Ok(Value::Base(Base::U32(v))) = v {
            v
        } else {
            bail!("Got bad reflected value: expected U32, got {:?}", v);
        };
        let addr = format!("{}", start_address as usize + i * 4);
        // XXX this is inefficient
        let reg = parse_reg_or_addr(&addr)?;
        println!("{}    {:#010x}", reg, value);
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
        let op_port = hubris.get_idol_command("Monorail.get_port_status")?;
        let op_phy = hubris.get_idol_command("Monorail.get_phy_status")?;
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

    let fmt_link = |v: &Value| match v {
        Value::Base(Base::Bool(b)) => {
            if *b {
                "up".to_owned().green()
            } else {
                "down".to_owned().red()
            }
        }
        Value::Enum(e) => match e.disc() {
            "Up" => "up".to_owned().green(),
            "Down" => "down".to_owned().red(),
            "Error" => "err".to_owned().yellow(),
            s => panic!("Unknown LinkStatus variant {:?}", s),
        },
        b => panic!("Could not get bool or enum from {:?}", b),
    };

    println!("{}", "PORT | MODE    SPEED  DEV     SERDES  LINK |   PHY    MAC LINK  MEDIA LINK".bold());
    println!("{}", "-----|-------------------------------------|-------------------------------".bold());
    for (port, (port_value, phy_value)) in (0..NUM_PORTS).zip(results) {
        if !ports.is_empty() && !ports.contains(&port) {
            continue;
        }
        print!(" {:<3} | ", port);
        match port_value {
            Ok(v) => {
                let s = v.as_struct()?;
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
                let fmt_mode = match mode.as_str() {
                    "SGMII" => mode.cyan(),
                    "QSGMII" => mode.blue(),
                    "SFI" | "BASEKR" => mode.magenta(),
                    v => v.into(),
                };

                print!(
                    "{:<6}  {:<5}  {:<6}  {:<6}  {:<4}",
                    fmt_mode,
                    speed,
                    dev,
                    serdes,
                    fmt_link(&s["link_up"]),
                )
            }
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
            Ok(v) => {
                let s = v.as_struct()?;
                assert_eq!(s.name(), "PhyStatus");
                let phy_ty = match &s["ty"] {
                    Value::Enum(e) => e.disc().to_uppercase(),
                    v => panic!("Expected struct, got {:?}", v),
                };
                println!(
                    "{:<6}  {:<8}  {:<10}",
                    phy_ty,
                    fmt_link(&s["mac_link_up"]),
                    fmt_link(&s["media_link_up"]),
                )
            }
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

fn monorail_mac_table(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
) -> Result<()> {
    let op_mac_count =
        hubris.get_idol_command("Monorail.read_vsc7448_mac_count")?;

    // We need to make two HIF calls:
    // - Read the number of entries in the MAC table
    // - Loop over the table that many times, reading entries
    let value = humility_cmd_hiffy::hiffy_call(
        hubris,
        core,
        context,
        &op_mac_count,
        &[],
        None,
    )?;
    let mac_count = match value {
        Ok(v) => {
            if let Value::Base(Base::U32(v)) = v {
                v
            } else {
                bail!("Got bad reflected value: expected U32, got {:?}", v);
            }
        }
        Err(e) => {
            bail!("Got error: {}", e);
        }
    };

    println!("Reading {} MAC addresses...", mac_count);

    let op = hubris.get_idol_command("Monorail.read_vsc7448_next_mac")?;
    let funcs = context.functions()?;
    let send = funcs.get("Send", 4)?;

    use hif::*;
    let label = Target(0);

    let ops = vec![
        Op::Push32(mac_count),
        Op::Push32(0), // current loop iteration
        Op::Label(label),
        // BEGIN LOOP
        Op::Push32(op.task.task()),
        Op::Push16(op.code),
        Op::Push(0), // Payload length
        Op::Push(8), // Return size
        Op::Call(send.id),
        Op::DropN(4), // Drop all arguments
        // Loop iteration is now at the top of the stack
        Op::Push(1), // Prepare to increment
        Op::Add,     // i = i + 1
        Op::BranchLessThan(label),
        // END LOOP
        Op::DropN(2),
        Op::Done,
    ];

    let results = context.run(core, ops.as_slice(), None)?;
    let results = results
        .into_iter()
        .map(move |r| humility_cmd_hiffy::hiffy_decode(hubris, &op, r))
        .collect::<Result<Vec<Result<_, _>>>>()?;

    let mut mac_table: BTreeMap<u16, Vec<[u8; 6]>> = BTreeMap::new();
    for r in results {
        if let Ok(r) = r {
            let s = r.as_struct()?;
            assert_eq!(s.name(), "MacTableEntry");
            let port = s["port"].as_base().unwrap().as_u16().unwrap();
            let mut mac = [0; 6];
            for (i, m) in s["mac"].as_array().unwrap().iter().enumerate() {
                mac[i] = m.as_base().unwrap().as_u8().unwrap()
            }
            if mac == [0; 6] && port == 0xFFFF {
                println!("Skipping empty MAC address");
            } else {
                mac_table.entry(port).or_default().push(mac);
            }
        } else {
            // Log the error but keep going for other entries in the table
            println!("Got error result: {:?}", r);
        }
    }
    println!(" {} |        {}", "PORT".bold(), "MAC".bold());
    println!("------|-------------------");
    for (port, macs) in &mac_table {
        for (i, mac) in macs.iter().enumerate() {
            if i == 0 {
                print!("{:>5} | ", port);
            } else {
                print!("      | ");
            }
            for (i, m) in mac.iter().enumerate() {
                if i > 0 {
                    print!(":");
                }
                print!("{:02x}", m);
            }
            println!();
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
    let op = hubris.get_idol_command("Monorail.reset_port_counters")?;
    let value = humility_cmd_hiffy::hiffy_call(
        hubris,
        core,
        context,
        &op,
        &[("port", IdolArgument::Scalar(u64::from(port)))],
        None,
    )?;
    value.map(|_| ()).map_err(|err| anyhow!("Got error {}", err))
}

fn monorail_counters(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
    port: u8,
) -> Result<()> {
    let op = hubris.get_idol_command("Monorail.get_port_counters")?;
    let value = humility_cmd_hiffy::hiffy_call(
        hubris,
        core,
        context,
        &op,
        &[("port", IdolArgument::Scalar(u64::from(port)))],
        None,
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
            let s = v.as_struct()?;
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
        Err(e) => {
            println!("Got error: {}", e);
        }
    }
    Ok(())
}

fn monorail(context: &mut ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let hubris = context.archive.as_mut().unwrap();
    let subargs = MonorailArgs::try_parse_from(subargs)?;
    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;
    match subargs.cmd {
        Command::Info { .. } => panic!("Called monorail with info subcommand"),
        Command::Status { ports } => {
            monorail_status(hubris, core, &mut context, &ports)?
        }
        Command::Mac => monorail_mac_table(hubris, core, &mut context)?,
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
        Command::Dump { target } => {
            use regex::Regex;
            let re = Regex::new(r"^([A-Z_0-9]+)(\[([0-9]+)\])?$").unwrap();
            let cap = re
                .captures(&target)
                .ok_or_else(|| anyhow!("Could not parse {}", target))?;
            let name = &cap[1];
            let index: Option<u32> =
                cap.get(3).map(|i| i.as_str().parse().unwrap());
            let tgt = vsc7448_info::MEMORY_MAP
                .get(name)
                .ok_or_else(|| anyhow!("Could not find target {}", name))?;
            let tgt_name = &tgt.0;
            let start_addr: u32 = tgt
                .1
                .iter()
                .find(|(tgt_index, _addr)| *tgt_index == index)
                .map(|(_tgt_index, addr)| *addr)
                .ok_or_else(|| anyhow!("Could not find index {:?}", index))?;
            let tgt = vsc7448_info::TARGETS.get(tgt_name).unwrap();
            let tgt_size = tgt
                .groups
                .iter()
                .map(|g| g.1)
                .map(|g| g.addr.base + g.addr.width * g.addr.count)
                .max()
                .unwrap();
            let end_addr = start_addr + tgt_size * 4;

            println!(
                "Dumping target {} ({:#x} -> {:#x})",
                target, start_addr, end_addr
            );
            monorail_dump(hubris, core, &mut context, start_addr, end_addr)?
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
            PhyCommand::Dump { port, verbose } => {
                monorail_phy_dump(hubris, core, &mut context, port, verbose)?
            }
            _ => panic!("Invalid PHY command"),
        },
    };
    Ok(())
}

fn monorail_get_info(context: &mut ExecutionContext) -> Result<()> {
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let hubris = context.archive.as_ref().unwrap();
    assert!(!hubris.loaded());
    let subargs = MonorailArgs::try_parse_from(subargs)?;
    match subargs.cmd {
        Command::Info { reg, value } => {
            let reg = parse_reg_or_addr(&reg)?;
            println!("Register {}", reg);
            println!("Register address: {:#x}", reg.address());

            if let Some(v) = value {
                println!("Register value: {:#x}", v);
                pretty_print_fields(v, reg.fields(), 0);
            } else {
                println!("  bits |    field");
                for (f, field) in reg.fields() {
                    let range = if field.hi > field.lo + 1 {
                        format!(" {}:{}", field.hi - 1, field.lo)
                    } else {
                        format!("{}", field.lo)
                    };
                    println!(" {:>5} | {}", range, f);
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

pub fn init() -> humility_cmd::Command {
    // We do a bonus parse of the command-line arguments here to see if we're
    // doing a `monorail info` subcommand, which doesn't require a Hubris image
    // or attached device; skipping those steps improves runtime (especially
    // in debug builds)

    let subcmd_attached = humility_cmd::Command {
        app: MonorailArgs::command(),
        name: "monorail",
        run: monorail,
        kind: CommandKind::Attached {
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
        },
    };

    let subcmd_unattached = humility_cmd::Command {
        app: MonorailArgs::command(),
        name: "monorail",
        run: monorail_get_info,
        kind: CommandKind::Unattached { archive: Archive::Ignored },
    };

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
