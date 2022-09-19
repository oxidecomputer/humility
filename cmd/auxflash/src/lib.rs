// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility auxflash`
//!
//! Tools to interact with the auxiliary flash, described in RFD 311.
//!
//! This subcommand should be rarely used; `humility flash` will automatically
//! program auxiliary flash when needed.

use anyhow::{anyhow, bail, Result};
use clap::{CommandFactory, Parser};
use colored::Colorize;
use humility::cli::Subcommand;
use humility_cmd::CommandKind;
use indicatif::{ProgressBar, ProgressStyle};

use cmd_hiffy as humility_cmd_hiffy;

use humility::core::Core;
use humility::hubris::*;
use humility_cmd::hiffy::HiffyContext;
use humility_cmd::idol::{HubrisIdol, IdolArgument};
use humility_cmd::{Archive, Attach, Command, Validate};
use humility_cmd_hiffy::HiffyLease;

const DEFAULT_SLOT_SIZE_BYTES: usize = 2 * 1024 * 1024;
const READ_CHUNK_SIZE: usize = 256; // limited by HIFFY_SCRATCH_SIZE

#[derive(Parser, Debug)]
#[clap(name = "auxflash", about = env!("CARGO_PKG_DESCRIPTION"))]
struct AuxFlashArgs {
    /// sets timeout
    #[clap(
        long, short = 'T', default_value_t = 15000, value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    #[clap(subcommand)]
    cmd: AuxFlashCommand,
}

#[derive(Parser, Debug)]
enum AuxFlashCommand {
    /// Prints the auxiliary flash status
    Status {
        #[clap(long, short)]
        verbose: bool,
    },
    Erase {
        #[clap(long, short)]
        slot: u32,
    },
    Read {
        #[clap(long, short)]
        slot: u32,

        /// Number of bytes to read (defaults to 1M)
        #[clap(long, short)]
        count: Option<usize>,
        output: String,
    },
    Write {
        #[clap(long, short)]
        slot: u32,
        #[clap(long, short = 'F')]
        force: bool,
        input: Option<String>,
    },
}

pub struct AuxFlashHandler<'a> {
    hubris: &'a HubrisArchive,
    core: &'a mut dyn Core,
    context: HiffyContext<'a>,
}

impl<'a> AuxFlashHandler<'a> {
    pub fn new(
        hubris: &'a HubrisArchive,
        core: &'a mut dyn Core,
        hiffy_timeout: u32,
    ) -> Result<Self> {
        let context = HiffyContext::new(hubris, core, hiffy_timeout)?;
        Ok(Self { hubris, core, context })
    }

    pub fn slot_size_bytes(&self) -> Result<usize> {
        self.hubris
            .manifest
            .auxflash
            .as_ref()
            .map(|i| i.slot_size_bytes())
            .unwrap_or(Ok(DEFAULT_SLOT_SIZE_BYTES))
    }

    pub fn slot_count(&mut self) -> Result<u32> {
        let op = self.hubris.get_idol_command("AuxFlash.slot_count")?;
        let value = humility_cmd_hiffy::hiffy_call(
            self.hubris,
            self.core,
            &mut self.context,
            &op,
            &[],
            None,
        )?;
        let v = match value {
            Ok(v) => v,
            Err(e) => bail!("Got Hiffy error: {}", e),
        };
        let v = v.as_base()?;
        v.as_u32().ok_or_else(|| anyhow!("Couldn't get U32"))
    }

    /// Returns the active slot, or `None` if there is no active slot
    pub fn active_slot(&mut self) -> Result<Option<u32>> {
        let op = self
            .hubris
            .get_idol_command("AuxFlash.scan_and_get_active_slot")?;
        let value = humility_cmd_hiffy::hiffy_call(
            self.hubris,
            self.core,
            &mut self.context,
            &op,
            &[],
            None,
        )?;
        let v = match value {
            Ok(v) => v,
            Err(e) if e == "NoActiveSlot" => {
                return Ok(None);
            }
            Err(e) => bail!("Got Hiffy error: {}", e),
        };
        let v = v.as_base()?;
        v.as_u32().ok_or_else(|| anyhow!("Couldn't get U32")).map(Some)
    }

    fn slot_erase(&mut self, slot: u32) -> Result<()> {
        let op = self.hubris.get_idol_command("AuxFlash.erase_slot")?;
        let value = humility_cmd_hiffy::hiffy_call(
            self.hubris,
            self.core,
            &mut self.context,
            &op,
            &[("slot", IdolArgument::Scalar(u64::from(slot)))],
            None,
        )?;
        match value {
            Ok(..) => Ok(()),
            Err(e) => bail!("Got Hiffy error: {}", e),
        }
    }

    pub fn slot_status(&mut self, slot: u32) -> Result<Option<[u8; 32]>> {
        let op = self.hubris.get_idol_command("AuxFlash.read_slot_chck")?;
        let value = humility_cmd_hiffy::hiffy_call(
            self.hubris,
            self.core,
            &mut self.context,
            &op,
            &[("slot", IdolArgument::Scalar(u64::from(slot)))],
            None,
        )?;
        let v = match value {
            Ok(v) => v,
            Err(e) if e == "MissingChck" => return Ok(None),
            Err(e) => bail!("{}", e),
        };
        let array = v.as_1tuple().unwrap().as_array().unwrap();
        assert_eq!(array.len(), 32);
        let mut out = [0u8; 32];
        for (o, v) in out
            .iter_mut()
            .zip(array.iter().map(|i| i.as_base().unwrap().as_u8().unwrap()))
        {
            *o = v;
        }

        Ok(Some(out))
    }

    fn auxflash_status(&mut self, verbose: bool) -> Result<()> {
        let slot_count = self.slot_count()?;
        let active_slot = self.active_slot()?;
        println!(" {} | {}", "slot".bold(), "status".bold());
        println!("------|----------------------------");
        for i in 0..slot_count {
            print!("  {:>3} | ", i);
            match self.slot_status(i) {
                Err(e) => {
                    println!("Error: {}", e.to_string().red());
                }
                Ok(None) => println!("{}", "Missing checksum".yellow()),
                Ok(Some(v)) => {
                    if active_slot == Some(i) {
                        print!("{} (", "Active".green());
                    } else {
                        print!("{} (", "Valid ".blue());
                    }
                    if verbose {
                        for byte in v {
                            print!("{:0>2x}", byte);
                        }
                    } else {
                        for byte in &v[0..4] {
                            print!("{:0>2x}", byte);
                        }
                        print!("...");
                    }
                    println!(")");
                }
            }
        }
        Ok(())
    }

    fn auxflash_read(
        &mut self,
        slot: u32,
        count: Option<usize>,
    ) -> Result<Vec<u8>> {
        let op =
            self.hubris.get_idol_command("AuxFlash.read_slot_with_offset")?;
        let slot_size = self.slot_size_bytes()?;

        let mut out = vec![0u8; count.unwrap_or(slot_size)];
        let bar = ProgressBar::new(0);
        bar.set_style(
            ProgressStyle::default_bar()
                .template("humility: reading [{bar:30}] {bytes}/{total_bytes}"),
        );
        bar.set_length(out.len() as u64);
        for (i, chunk) in out.chunks_mut(READ_CHUNK_SIZE).enumerate() {
            let offset = i * READ_CHUNK_SIZE;
            let value = humility_cmd_hiffy::hiffy_call(
                self.hubris,
                self.core,
                &mut self.context,
                &op,
                &[
                    ("slot", IdolArgument::Scalar(slot as u64)),
                    ("offset", IdolArgument::Scalar(offset as u64)),
                ],
                Some(HiffyLease::Read(chunk)),
            )?;
            if let Err(e) = value {
                bail!("Got Hubris error: {:?}", e);
            }
            bar.set_position(offset as u64);
        }
        bar.set_position(out.len() as u64);

        Ok(out)
    }

    pub fn auxflash_write(
        &mut self,
        slot: u32,
        data: &[u8],
        force: bool,
    ) -> Result<()> {
        // If the input data can be parsed as TLV-C, then check against
        // the slot checksum currently loaded in auxiliary flash, skipping
        // flashing if they match.
        let mut reader = tlvc::TlvcReader::begin(data)
            .map_err(|e| anyhow!("TlvcReader::begin failed: {:?}", e))?;
        let mut chck_data = None;
        loop {
            match reader.next() {
                Ok(Some(chunk)) => {
                    if &chunk.header().tag == b"CHCK" {
                        assert_eq!(chunk.len(), 32);
                        let mut out = [0; 32];
                        chunk.read_exact(0, &mut out).map_err(|e| {
                            anyhow!("Failed to read chunk: {:?}", e)
                        })?;
                        chck_data = Some(out);
                        break;
                    }
                }
                Ok(None) => break,
                Err(e) => {
                    humility::msg!(
                        "Failed to load data as TLV-C ({:?}); \
                         skipping reflash check",
                        e
                    );
                    break;
                }
            }
        }
        if let Some(chck_data) = chck_data {
            if let Ok(Some(chck_slot)) = self.slot_status(slot) {
                if chck_data == chck_slot {
                    humility::msg!(
                        "Slot {} is already programmed with our data",
                        slot
                    );
                    if force {
                        humility::msg!("Reprogramming it anyways!");
                    } else {
                        humility::msg!("Skipping reprogramming.");
                        return Ok(());
                    }
                }
            }
        }

        humility::msg!("erasing slot {}", slot);
        self.slot_erase(slot)?;

        let slot_size = self.slot_size_bytes()?;
        if data.len() > slot_size {
            bail!(
                "Data is too large ({} bytes, slot size is {} bytes)",
                data.len(),
                slot_size
            );
        }
        let op =
            self.hubris.get_idol_command("AuxFlash.write_slot_with_offset")?;

        let bar = ProgressBar::new(0);
        bar.set_style(
            ProgressStyle::default_bar()
                .template("humility: writing [{bar:30}] {bytes}/{total_bytes}"),
        );
        bar.set_length(data.len() as u64);
        for (i, chunk) in data.chunks(self.context.data_size()).enumerate() {
            let offset = i * self.context.data_size();
            let value = humility_cmd_hiffy::hiffy_call(
                self.hubris,
                self.core,
                &mut self.context,
                &op,
                &[
                    ("slot", IdolArgument::Scalar(slot as u64)),
                    ("offset", IdolArgument::Scalar(offset as u64)),
                ],
                Some(HiffyLease::Write(chunk)),
            )?;
            if let Err(e) = value {
                bail!("Got Hubris error: {:?}", e);
            }
            bar.set_position(offset as u64);
        }
        bar.set_position(data.len() as u64);
        humility::msg!("done");
        Ok(())
    }

    pub fn reset(&mut self) -> Result<()> {
        self.core.reset()
    }

    pub fn auxflash_write_from_archive(
        &mut self,
        slot: u32,
        force: bool,
    ) -> Result<()> {
        let data = self.hubris.read_auxflash_data()?.ok_or_else(|| {
            anyhow!(
                "Could not find auxiliary data in the archive. \
                     Does this Hubris app include auxiliary blobs?"
            )
        })?;
        humility::msg!("Flashing auxiliary data from the Hubris archive");
        self.auxflash_write(slot, &data, force)
    }
}

////////////////////////////////////////////////////////////////////////////////

fn auxflash(context: &mut humility::ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let subargs = AuxFlashArgs::try_parse_from(subargs)?;
    let hubris = context.archive.as_ref().unwrap();
    let mut worker = AuxFlashHandler::new(hubris, core, subargs.timeout)?;

    match subargs.cmd {
        AuxFlashCommand::Status { verbose } => {
            worker.auxflash_status(verbose)?;
        }
        AuxFlashCommand::Erase { slot } => {
            worker.slot_erase(slot)?;
            humility::msg!("done erasing slot {}", slot);
        }
        AuxFlashCommand::Read { slot, output, count } => {
            let data = worker.auxflash_read(slot, count)?;
            std::fs::write(&output, &data)?;
        }
        AuxFlashCommand::Write { slot, input, force } => match input {
            Some(input) => {
                let data = std::fs::read(&input)?;
                worker.auxflash_write(slot, &data, force)?;
            }
            None => {
                // If the user didn't specify an image to flash on the
                // command line, then attempt to pull it from the image.
                worker.auxflash_write_from_archive(slot, force)?;
            }
        },
    }
    Ok(())
}

pub fn init() -> Command {
    Command {
        app: AuxFlashArgs::command(),
        name: "auxflash",
        run: auxflash,
        kind: CommandKind::Attached {
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
        },
    }
}
