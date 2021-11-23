/*
 * Copyright 2021 Oxide Computer Company
 */
use crate::cmd::{Archive, Attach, Validate};
use humility::core::Core;
use humility_cmd::hiffy::{HiffyContext, HiffyFunctions};
use humility::hubris::*;
use cmd_spi::spi_task;
use crate::Args;

use anyhow::{anyhow, bail, Result};
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

    #[structopt(long, short, parse(try_from_str = parse_int::parse), value_name = "addr")]
    addr: u32,
}

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
        args: Vsc7448Args,
    ) -> Result<Self> {
        let mut context = HiffyContext::new(hubris, core, args.timeout)?;
        let funcs = context.functions()?;

        let task = spi_task(hubris, args.peripheral)?;
        Ok(Self { core, context, task, funcs })
    }

    // Writes a single 32-bit register
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

    // Reads a single 32-bit register
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
        Ok(((r[4] as u32) << 24)
            | ((r[5] as u32) << 16)
            | ((r[6] as u32) << 8)
            | r[7] as u32)
    }

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
    let mut vsc = Vsc7448::new(hubris, core, subargs)?;
    vsc.init()?;
    Ok(())
}

pub fn init<'a, 'b>() -> (crate::cmd::Command, App<'a, 'b>) {
    (
        crate::cmd::Command::Attached {
            name: "vsc7448",
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
            run: vsc7448,
        },
        Vsc7448Args::clap(),
    )
}
