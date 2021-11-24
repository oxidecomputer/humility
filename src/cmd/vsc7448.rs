/*
 * Copyright 2021 Oxide Computer Company
 */
use std::convert::TryInto;

use crate::cmd::{Archive, Attach, Validate};
use humility::core::Core;
use humility_cmd::hiffy::{HiffyContext, HiffyFunctions};
use humility::hubris::*;
use cmd_spi::spi_task;
use crate::Args;

use anyhow::{anyhow, bail, Context, Result};
use hif::*;
use structopt::clap::App;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(name = "spi", about = "SPI reading and writing")]
struct Vsc7448Args {
    /// sets timeout
    #[structopt(
        long, short = "T", default_value = "5000", value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// SPI peripheral on which to operate
    #[structopt(long, short, value_name = "peripheral")]
    peripheral: Option<u8>,

    #[structopt(subcommand)]
    cmd: Command,
}

#[derive(StructOpt, Debug)]
enum Command {
    Info { reg: String },
    Read { reg: String },
    Write { reg: String, value: u32 },
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
        let spi_write = self.funcs.get("SpiWrite", 2)?;

        // Write 7 bytes from the data array over SPI
        let ops = [
            Op::Push32(self.task.task()),
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
        self.context.execute_blocking(self.core, &ops, Some(&data))?;
        Ok(())
    }

    /// Reads a single 32-bit register
    fn read(&mut self, addr: u32) -> Result<u32> {
        let spi_read = self.funcs.get("SpiRead", 3)?;

        // Write 3 bytes of address, and read 8 bytes back in total
        // (3 address bytes, 1 padding byte, 4 bytes of result)
        let ops = [
            Op::Push32(self.task.task()),
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
        let results =
            self.context.execute_blocking(self.core, &ops, Some(&data))?;
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
}

fn vsc7448(
    hubris: &mut HubrisArchive,
    core: &mut dyn Core,
    _args: &Args,
    subargs: &Vec<String>,
) -> Result<()> {
    let subargs = Vsc7448Args::from_iter_safe(subargs)?;
    let mut vsc = Vsc7448::new(hubris, core, &subargs)?;
    vsc.init()?;
    match subargs.cmd {
        Command::Read { reg } => {
            let reg = QualifiedRegister::new(&reg)?;
            let addr = reg.address()?;
            log::info!("Reading {} from 0x{:x}", reg, addr);
            let value = vsc.read(addr)?;
            println!("{} => 0x{:x}", reg, value);
            if value == 0x88888888 {
                log::warn!("0x88888888 typically indicates a communication issue!");
            }
            let fields = &vsc7448_info::TARGETS[vsc7448_info::MEMORY_MAP[reg.target.name].0]
                .groups[reg.group.name]
                .regs[reg.reg.name]
                .fields;
            let mut field_keys = fields.keys().collect::<Vec<_>>();
            field_keys.sort_by(|a, b| fields[*b].lo.cmp(&fields[*a].lo));
            println!("  bits |    value   | field");
            for f in field_keys {
                let field = &fields[*f];
                let bits = (value & ((1u64 << field.hi) - 1) as u32) >> field.lo;
                println!(" {:>2}:{:<2} | 0x{:<8x} | {}", field.hi, field.lo, bits, f);
            }
        },
        Command::Info { .. } => panic!("Called vsc7448 with info subcommand"),
        Command::Write { .. } => unimplemented!(),
    };
    Ok(())
}

fn vsc7448_get_info(
    hubris: &mut HubrisArchive,
    _args: &Args,
    subargs: &Vec<String>,
) -> Result<()> {
    assert!(!hubris.loaded());
    let subargs = Vsc7448Args::from_iter_safe(subargs)?;
    if let Command::Info { reg } = subargs.cmd {
        let reg = QualifiedRegister::new(&reg)?;
        println!("Register address: {:x}", reg.address()?);
    } else {
        panic!("Called vsc7448_get_info without info subcommand");
    }
    Ok(())
}

pub fn init<'a, 'b>() -> (crate::cmd::Command, App<'a, 'b>) {
    // We do a bonus parse of the command-line arguments here to see if we're
    // doing a `vsc7448 info` subcommand, which doesn't require a Hubris image
    // or attached device; skipping those steps improves runtime (especially
    // in debug builds)
    let subcmd_attached = (
        crate::cmd::Command::Attached {
            name: "vsc7448",
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
            run: vsc7448,
        },
        Vsc7448Args::clap(),
    );
    let subcmd_unattached = (
        crate::cmd::Command::Unattached {
            name: "vsc7448",
            archive: Archive::Ignored,
            run: vsc7448_get_info,
        },
        Vsc7448Args::clap(),
    );

    let mut args = std::env::args().skip_while(|a| a != "vsc7448").peekable();
    if args.peek().is_some() {
        if let Ok(args) = Vsc7448Args::from_iter_safe(args) {
            if let Command::Info { .. } = args.cmd {
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

////////////////////////////////////////////////////////////////////////////////
/// TODO: this can probably move to the `vsc7448-info` crate

#[derive(Debug)]
struct Indexed<'a> {
    name: &'a str,
    index: Option<usize>,
}
impl<'a> std::fmt::Display for Indexed<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.index {
            None => write!(f, "{}", self.name),
            Some(i) => write!(f, "{}[{}]", self.name, i)
        }
    }
}

#[derive(Debug)]
struct QualifiedRegister<'a> {
    target: Indexed<'a>,
    group: Indexed<'a>,
    reg: Indexed<'a>,
}

impl<'a> QualifiedRegister<'a> {
    fn new(s: &'a str) -> Result<Self> {
        // TODO: make this parser much smarter, so it can properly recognize
        // indexes and disambiguate registers

        let mut out = None;
        for (i, target) in vsc7448_info::MEMORY_MAP.iter() {
            for (j, reg_group) in vsc7448_info::TARGETS[target.0].groups.iter() {
                if reg_group.regs.contains_key(s) {
                    if out.is_some() {
                        warn!("Ambiguous register name");
                    }
                    out = Some(QualifiedRegister {
                        target: Indexed { name: i, index: None },
                        group: Indexed { name: j, index: None },
                        reg: Indexed { name: s, index: None },
                    })
                }
            }
        }
        out.ok_or_else(|| anyhow!("Could not find {}", s))
    }

    fn address(&self) -> Result<u32> {
        let target = &vsc7448_info::MEMORY_MAP[self.target.name];
        let (_, mut addr) = target.1.iter().find(|t| t.0 == self.target.index)
            .ok_or_else(|| anyhow!("Could not find target index {:?}", self.target.index))?;

        let target = &vsc7448_info::TARGETS[target.0];
        let group = &target.groups[self.group.name];
        match (group.addr.count, self.group.index) {
            (1, Some(_)) => bail!("Register group has only one instance"),
            (i, None) if i != 1 => bail!("Index required for register group"),
            (i, Some(j)) if j >= i => bail!("Index is too high"),
            _ => (),
        }
        addr += (group.addr.base + group.addr.width * self.group.index.unwrap_or(0)) * 4;

        let reg = &group.regs[self.reg.name];
        match (reg.addr.count, self.reg.index) {
            (1, Some(_)) => bail!("Register has only one instance"),
            (i, None) if i != 1 => bail!("Index required for register"),
            (i, Some(j)) if j >= i => bail!("Index is too high"),
            _ => (),
        }
        addr += (reg.addr.base + reg.addr.width * self.reg.index.unwrap_or(0)) * 4;

        addr.try_into().context("Address is too large")
    }

}

impl<'a> std::fmt::Display for QualifiedRegister<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}:{}", self.target, self.group, self.reg)
    }
}
