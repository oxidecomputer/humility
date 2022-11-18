// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
use std::collections::BTreeMap;
use std::convert::TryInto;

use humility::cli::Subcommand;
use humility::core::Core;
use humility::hubris::*;
use humility_cmd::hiffy::{HiffyContext, HiffyFunctions};
use humility_cmd::{Archive, Attach, Validate};
use humility_cmd_spi::spi_task;

use anyhow::{anyhow, bail, Result};
use clap::Command as ClapCommand;
use clap::{CommandFactory, Parser};
use hif::*;
use vsc7448_info::parse::{PhyRegister, TargetRegister};
use vsc7448_types::Field;

#[derive(Parser, Debug)]
#[clap(name = "vsc7448", about = env!("CARGO_PKG_DESCRIPTION"))]
struct Vsc7448Args {
    /// sets timeout
    #[clap(
        long, short = 'T', default_value_t = 5000, value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// Disables initialization, which saves a few SPI transactions if you
    /// know that the initial VSC7448 configuration has already been done.
    #[clap(long, short)]
    noinit: bool,

    /// SPI peripheral on which to operate
    #[clap(long, short, value_name = "peripheral")]
    peripheral: Option<u8>,

    #[clap(subcommand)]
    cmd: Command,
}

impl Vsc7448Args {
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
        #[clap(flatten)]
        addr: PhyAddr,
        reg: String,
    },
    Write {
        #[clap(flatten)]
        addr: PhyAddr,
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
struct Vsc7448<'a> {
    core: &'a mut dyn Core,
    context: HiffyContext<'a>,

    task: HubrisTask,
    funcs: HiffyFunctions,
}

impl<'a> Vsc7448<'a> {
    pub fn new(
        hubris: &'a HubrisArchive,
        core: &'a mut dyn Core,
        args: &Vsc7448Args,
    ) -> Result<Self> {
        let mut context = HiffyContext::new(hubris, core, args.timeout)?;
        let funcs = context.functions()?;

        let task = spi_task(hubris, args.peripheral)?;
        Ok(Self { core, context, task, funcs })
    }

    /// Writes a single 32-bit register
    fn write(&mut self, addr: u32, data: u32) -> Result<()> {
        let spi_write = self.funcs.get("SpiWrite", 3)?;

        // Write 7 bytes from the data array over SPI
        let ops = [
            Op::Push32(self.task.task()),
            Op::Push(0),     // Device 0
            Op::Push32(0x7), // Write size
            Op::Call(spi_write.id),
            Op::Done,
        ];

        if !(0x71000000..=0x72000000).contains(&addr) {
            bail!("Address must be in Switch Core Register Bus (0x71000000)");
        }
        let addr = (addr & 0x00FFFFFF) >> 2;
        let data = [
            // 24-bit address, with high bit set to mark write
            ((addr >> 16) & 0xFF) as u8 | 0x80,
            ((addr >> 8) & 0xFF) as u8,
            (addr & 0xFF) as u8,
            // 32-bit data word
            ((data >> 24) & 0xFF) as u8,
            ((data >> 16) & 0xFF) as u8,
            ((data >> 8) & 0xFF) as u8,
            (data & 0xFF) as u8,
        ];
        self.context.run(self.core, &ops, Some(&data))?;
        Ok(())
    }

    /// Reads a single 32-bit register
    fn read(&mut self, addr: u32) -> Result<u32> {
        let spi_read = self.funcs.get("SpiRead", 4)?;

        // Write 3 bytes of address, and read 8 bytes back in total
        // (3 address bytes, 1 padding byte, 4 bytes of result)
        let ops = [
            Op::Push32(self.task.task()),
            Op::Push(0),     // Device 0
            Op::Push32(0x3), // Write size
            Op::Push32(0x8), // Read size
            Op::Call(spi_read.id),
            Op::Done,
        ];
        if !(0x71000000..=0x72000000).contains(&addr) {
            bail!("Address must be in Switch Core Register Bus (0x71000000)");
        }
        let addr = (addr & 0x00FFFFFF) >> 2;
        let data = [
            // 24-bit address, with high bit cleared for a read
            ((addr >> 16) & 0xFF) as u8,
            ((addr >> 8) & 0xFF) as u8,
            (addr & 0xFF) as u8,
        ];
        let results = self.context.run(self.core, &ops, Some(&data))?;
        let r = results[0]
            .as_ref()
            .map_err(|e| anyhow!("Got error code: {}", e))?;
        if r.len() != 8 {
            bail!("wrong length read: {:x?}", r);
        }
        Ok(u32::from_be_bytes(r[4..].try_into().unwrap()))
    }

    /// Initialize the VSC7448, checking that it returns the correct chip ID.
    ///
    /// This is idempotent, so you can run it before doing anything with the
    /// chip if its state is unknown.
    fn init(&mut self) -> Result<()> {
        // Set DEVCPU_ORG:DEVCPU_ORG:IF_CTRL to 1
        // (using a special write pattern to be unambiguous regardless of
        // endianness or bit order; see the register manual for details)
        self.write(0x71000000, 0x81818181)?;

        // Set IF_CFGSTAT = 1 (to add one padding byte to read outputs)
        self.write(0x71000004, 0x1)?;

        // Read DEVCPU_GCB:CHIP_REGS:CHIP_ID to confirm we're happy
        let chip_id_reg = self.read(0x71010000)?;
        if chip_id_reg != 0x374680E9 {
            bail!("Invalid chip ID register: {}", chip_id_reg);
        }
        Ok(())
    }

    // Configures GPIOs as MIIM alternate functions (see Table 270 in VSC7448
    // datasheet for details)
    fn set_miim_gpios(&mut self, miim: u8) -> Result<()> {
        humility::msg!("Configuring MIIM{} alt gpios", miim);
        let gpio_reg = "GPIO_ALT1[0]".parse::<TargetRegister>()?.address();
        match miim {
            0 => Ok(()),
            1 => self.write(gpio_reg, 0x3000000),
            2 => self.write(gpio_reg, 0xC000000),
            _ => bail!("Invalid MIIM peripheral (must be 0-2)"),
        }
    }

    fn miim_read(&mut self, phy: &PhyAddr, page: u16, reg: u8) -> Result<u16> {
        if phy.phy >= 32 {
            bail!("Invalid phy address {} (must be < 32)", phy.phy);
        }
        if reg >= 32 {
            bail!("Invalid register address {} (must be < 32)", reg);
        }
        // Switch pages every time, since it's cheap and easier than restoring
        // the page when done _or_ whenever exiting early
        humility::msg!("Switching to page {}", page);
        self.miim_write_inner(
            phy, 0,  // STANDARD
            31, // PAGE
            page, false,
        )?;
        let data = (1 << 31) | // MIIM_CMD_VLD
            ((phy.phy as u32) << 25) | // MIIM_CMD_PHYAD
            ((reg as u32) << 20) | // MIIM_CMD_PHYAD
            (0b10 << 1); // MIIM_CMD_OPR_FIELD (read)

        let mii_cmd =
            format!("MIIM[{}]:MII_CMD", phy.miim).parse::<TargetRegister>()?;
        self.write(mii_cmd.address(), data)?;

        let mii_data =
            format!("MIIM[{}]:MII_DATA", phy.miim).parse::<TargetRegister>()?;
        let out = self.read(mii_data.address())?;

        if ((out >> 16) & 3) == 3 {
            bail!("Read returned error in MIIM_DATA_SUCCESS");
        }
        Ok((out & 0xFFFF) as u16)
    }

    fn miim_write_inner(
        &mut self,
        phy: &PhyAddr,
        page: u16,
        reg: u8,
        data: u16,
        switch_page: bool,
    ) -> Result<()> {
        if phy.phy >= 32 {
            bail!("Invalid phy address {} (must be < 32)", phy.phy);
        }
        if reg >= 32 {
            bail!("Invalid register address {} (must be < 32)", reg);
        }
        // Switch pages every time, since it's cheap and easier than restoring
        // the page when done _or_ whenever exiting early
        if switch_page {
            humility::msg!("Switching to page {}", page);
            self.miim_write_inner(
                phy, 0,  // STANDARD
                31, // PAGE
                page, false,
            )?;
        }
        let data = (1 << 31) | // MIIM_CMD_VLD
            ((phy.phy as u32) << 25) | // MIIM_CMD_PHYAD
            ((reg as u32) << 20) | // MIIM_CMD_PHYAD
            ((data as u32) << 4) | // MIIM_CMD_WRDATA
            (0b01 << 1); // MIIM_CMD_OPR_FIELD (write)

        let mii_cmd =
            format!("MIIM[{}]:MII_CMD", phy.miim).parse::<TargetRegister>()?;
        self.write(mii_cmd.address(), data)
    }

    fn miim_write(
        &mut self,
        phy: &PhyAddr,
        page: u16,
        reg: u8,
        data: u16,
    ) -> Result<()> {
        self.miim_write_inner(phy, page, reg, data, true)
    }
}

fn vsc7448(context: &mut humility::ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let hubris = context.archive.as_ref().unwrap();

    let subargs = Vsc7448Args::try_parse_from(subargs)?;
    let mut vsc = Vsc7448::new(hubris, core, &subargs)?;
    if !subargs.noinit {
        vsc.init()?;
    }
    match subargs.cmd {
        Command::Read { reg } => {
            let reg = parse_reg_or_addr(&reg)?;
            let addr = reg.address();
            humility::msg!("Reading {} from 0x{:x}", reg, addr);
            let value = vsc.read(addr)?;
            println!("{} => 0x{:x}", reg, value);
            if value == 0x88888888 {
                log::warn!(
                    "0x88888888 typically indicates a communication issue!"
                );
            }
            pretty_print_fields(value, reg.fields());
        }
        Command::Info { .. } => panic!("Called vsc7448 with info subcommand"),
        Command::Write { reg, value } => {
            let reg = parse_reg_or_addr(&reg)?;
            let addr = reg.address();
            humility::msg!("Writing 0x{:x} to {} at 0x{:x}", value, reg, addr);
            pretty_print_fields(value, reg.fields());

            vsc.write(addr, value)?;
        }
        Command::Phy { cmd } => {
            let dummy_fields = BTreeMap::new();
            let reg = match &cmd {
                PhyCommand::Write { reg, .. }
                | PhyCommand::Read { reg, .. } => reg,
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
                PhyCommand::Write { addr, value, .. } => {
                    println!(
                        "Writing 0x{:x} to {} at MIIM{}:{}",
                        value, name, addr.miim, addr.phy
                    );
                    pretty_print_fields(value as u32, fields);
                    vsc.set_miim_gpios(addr.miim)?;
                    vsc.miim_write(&addr, page, reg, value)?;
                }
                PhyCommand::Read { addr, .. } => {
                    println!(
                        "Reading from {} at MIIM{}:{}",
                        name, addr.miim, addr.phy
                    );
                    vsc.set_miim_gpios(addr.miim)?;
                    let out = vsc.miim_read(&addr, page, reg)?;
                    println!("Got result 0x{:x}", out);
                    pretty_print_fields(out as u32, fields);
                }
                _ => panic!("Invalid PHY command"),
            }
        }
    };
    Ok(())
}

fn vsc7448_get_info(context: &mut humility::ExecutionContext) -> Result<()> {
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let hubris = context.archive.as_ref().unwrap();
    assert!(!hubris.loaded());
    let subargs = Vsc7448Args::try_parse_from(subargs)?;
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
        _ => panic!("Called vsc7448_get_info without info subcommand"),
    }
    Ok(())
}

pub fn init() -> (humility_cmd::Command, ClapCommand<'static>) {
    // We do a bonus parse of the command-line arguments here to see if we're
    // doing a `vsc7448 info` subcommand, which doesn't require a Hubris image
    // or attached device; skipping those steps improves runtime (especially
    // in debug builds)
    let subcmd_attached = (
        humility_cmd::Command::Attached {
            name: "vsc7448",
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
            run: vsc7448,
        },
        Vsc7448Args::command(),
    );
    let subcmd_unattached = (
        humility_cmd::Command::Unattached {
            name: "vsc7448",
            archive: Archive::Ignored,
            run: vsc7448_get_info,
        },
        Vsc7448Args::command(),
    );

    // If there's a `vsc7448` subcommand, then attempt to parse the subcmd
    let mut args = std::env::args().skip_while(|a| a != "vsc7448").peekable();
    if args.peek().is_some() {
        if let Ok(args) = Vsc7448Args::try_parse_from(args) {
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
