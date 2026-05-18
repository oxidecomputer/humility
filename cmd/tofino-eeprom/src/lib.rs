// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility tofino-eeprom`
//!
//! Tools to interact with the Tofino EEPROM

use anyhow::{Result, bail};
use clap::Parser;
use humility_cli::{ExecutionContext, HumilitySubcommand};
use indicatif::{ProgressBar, ProgressStyle};

use humility::core::Core;
use humility::hubris::*;
use humility_hiffy::HiffyContext;
use humility_idol::{HubrisIdol, IdolArgument};

// Limited to 128 bytes due to the write buffer in the EEPROM
const READ_CHUNK_SIZE: usize = 128;
const WRITE_CHUNK_SIZE: usize = 128;

const EEPROM_SIZE_BYTES: usize = 65536;

#[derive(Parser, Debug)]
#[clap(name = "tofino-eeprom", about = env!("CARGO_PKG_DESCRIPTION"))]
pub struct EepromArgs {
    /// sets timeout
    #[clap(
        long, short = 'T', default_value_t = 15000, value_name = "timeout_ms",
        value_parser = parse_int::parse::<u32>
    )]
    timeout: u32,

    #[clap(subcommand)]
    cmd: EepromCommand,
}

#[derive(Parser, Debug)]
enum EepromCommand {
    /// Reads from the EEPROM, saving data to a file
    Read {
        /// Number of bytes to read (defaults to EEPROM size)
        #[clap(long, short)]
        count: Option<usize>,

        /// Filename of output
        output: String,
    },
    /// Writes a file to the EEPROM
    Write {
        /// Filename of output
        input: String,
    },
}

pub struct EepromHandler<'a> {
    hubris: &'a HubrisArchive,
    core: &'a mut dyn Core,
    context: HiffyContext<'a>,
}

impl<'a> EepromHandler<'a> {
    pub fn new(
        hubris: &'a HubrisArchive,
        core: &'a mut dyn Core,
        hiffy_timeout: u32,
    ) -> Result<Self> {
        let context = HiffyContext::new(hubris, core, hiffy_timeout)?;
        Ok(Self { hubris, core, context })
    }

    fn eeprom_read(&mut self, count: Option<usize>) -> Result<Vec<u8>> {
        let op =
            self.hubris.get_idol_command("Sequencer.read_spi_eeprom_bytes")?;

        let mut out = vec![0u8; count.unwrap_or(EEPROM_SIZE_BYTES)];
        let bar = ProgressBar::new(0);
        bar.set_style(
            ProgressStyle::default_bar().template(
                "humility: reading [{bar:30}] {bytes}/{total_bytes}",
            )?,
        );
        bar.set_length(out.len() as u64);
        for (i, chunk) in out.chunks_mut(READ_CHUNK_SIZE).enumerate() {
            let offset = i * READ_CHUNK_SIZE;
            humility_hiffy::hiffy_call::<()>(
                self.hubris,
                self.core,
                &mut self.context,
                &op,
                &[("offset", IdolArgument::Scalar(offset as u64))],
                None,
                Some(chunk),
            )?;
            bar.set_position(offset as u64);
        }
        bar.finish_and_clear();
        Ok(out)
    }

    pub fn eeprom_write(&mut self, data: &[u8]) -> Result<()> {
        if data.len() > EEPROM_SIZE_BYTES {
            bail!(
                "Data is too large ({} bytes, eeprom size is {} bytes)",
                data.len(),
                EEPROM_SIZE_BYTES
            );
        }
        let op =
            self.hubris.get_idol_command("Sequencer.write_spi_eeprom_bytes")?;

        let bar = ProgressBar::new(0);
        bar.set_style(
            ProgressStyle::default_bar().template(
                "humility: writing [{bar:30}] {bytes}/{total_bytes}",
            )?,
        );
        bar.set_length(data.len() as u64);
        for (i, chunk) in data.chunks(WRITE_CHUNK_SIZE).enumerate() {
            let offset = i * WRITE_CHUNK_SIZE;
            humility_hiffy::hiffy_call::<()>(
                self.hubris,
                self.core,
                &mut self.context,
                &op,
                &[("offset", IdolArgument::Scalar(offset as u64))],
                Some(chunk),
                None,
            )?;
            bar.set_position(offset as u64);
        }
        bar.set_position(data.len() as u64);
        humility::msg!("done");
        Ok(())
    }
}

////////////////////////////////////////////////////////////////////////////////

fn eeprom(subargs: EepromArgs, context: &mut ExecutionContext) -> Result<()> {
    let hubris = &context.cli.archive()?;
    let core = &mut *context.cli.attach_live_booted(hubris)?;
    let mut worker = EepromHandler::new(hubris, core, subargs.timeout)?;

    match subargs.cmd {
        EepromCommand::Read { output, count } => {
            let data = worker.eeprom_read(count)?;
            std::fs::write(output, data)?;
        }
        EepromCommand::Write { input } => {
            let data = std::fs::read(input)?;
            worker.eeprom_write(&data)?;
        }
    }
    Ok(())
}

pub type Args = EepromArgs;
impl HumilitySubcommand for Args {
    fn run(args: Args, context: &mut ExecutionContext) -> Result<()> {
        eeprom(args, context)
    }
}
