// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility fpga`
//!
//! This command allows interacting with FPGAs connected to a MCU running
//! Hubris.
//!

use std::time::Instant;
use std::{fs, path::Path};

use anyhow::{bail, Result};
use clap::{CommandFactory, Parser};
use hif::Op;
use humility::core::Core;
use humility::hubris::*;
use humility_cli::{ExecutionContext, Subcommand};
use humility_cmd::CommandKind;
use humility_cmd::{Archive, Attach, Command, Validate};
use humility_hiffy::HiffyContext;
use humility_idol::{self as idol, HubrisIdol};
use indicatif::{HumanBytes, HumanDuration};
use indicatif::{ProgressBar, ProgressStyle};

#[derive(Parser, Debug)]
#[clap(name = "fpga", about = env!("CARGO_PKG_DESCRIPTION"))]
struct FpgaArgs {
    /// sets timeout
    #[clap(
        long, short = 'T', default_value = "15000", value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    #[clap(subcommand)]
    cmd: FpgaCommand,
}

#[derive(Parser, Debug)]
enum FpgaCommand {
    State,
    Reset,
    Id,
    LoadBitstream { file_path: String },
}

pub struct FpgaHandler<'a> {
    hubris: &'a HubrisArchive,
    core: &'a mut dyn Core,
    context: HiffyContext<'a>,
}

impl<'a> FpgaHandler<'a> {
    pub fn new(
        hubris: &'a HubrisArchive,
        core: &'a mut dyn Core,
        hiffy_timeout: u32,
    ) -> Result<Self> {
        let context = HiffyContext::new(hubris, core, hiffy_timeout)?;
        Ok(Self { hubris, core, context })
    }

    fn reset(&mut self) -> Result<()> {
        let reset = self.hubris.get_idol_command("Fpga.reset")?;
        let payload = reset
            .payload(&[("device_index", idol::IdolArgument::Scalar(0))])?;
        let mut ops = vec![];

        self.context.idol_call_ops(&reset, &payload, &mut ops)?;
        ops.push(Op::Done);

        let _ = self.context.run(self.core, &ops, None)?;
        Ok(())
    }

    fn state(&mut self) -> Result<()> {
        let state = self.hubris.get_idol_command("Fpga.state")?;
        let payload = state
            .payload(&[("device_index", idol::IdolArgument::Scalar(0))])?;
        let mut ops = vec![];

        self.context.idol_call_ops(&state, &payload, &mut ops)?;
        ops.push(Op::Done);

        for r in self.context.run(self.core, &ops, None)? {
            match r {
                Ok(val) => println!(
                    "{}",
                    match val[0] {
                        1 => "Disabled",
                        2 => "AwaitingBitstream",
                        3 => "RunningUserDesign",
                        4 => "Error",
                        _ => "Unknown",
                    }
                ),
                Err(e) => println!("Failed to get `state`: {:x}", e),
            }
        }

        Ok(())
    }

    fn id(&mut self) -> Result<()> {
        let id = self.hubris.get_idol_command("Fpga.state")?;
        let payload =
            id.payload(&[("device_index", idol::IdolArgument::Scalar(0))])?;
        let mut ops = vec![];

        self.context.idol_call_ops(&id, &payload, &mut ops)?;
        ops.push(Op::Done);

        match &self.context.run(self.core, &ops, None)?[0] {
            Ok(val) => println!("{}", val[0]),
            Err(e) => println!("Failed to get `id`: {:x}", e),
        }

        Ok(())
    }

    fn load_bitstream(
        &mut self,
        file_name: &str,
        bitstream: &[u8],
    ) -> Result<()> {
        let ecp5_write_bitstream_chunk =
            self.context.get_function("FpgaWriteBitstreamChunk", 0)?;

        let start_bitstream_load =
            self.hubris.get_idol_command("Fpga.start_bitstream_load")?;
        let payload = start_bitstream_load.payload(&[
            ("device_index", idol::IdolArgument::Scalar(0)),
            ("bitstream_type", idol::IdolArgument::String("Uncompressed")),
        ])?;
        let mut ops = vec![];

        self.context.idol_call_ops(
            &start_bitstream_load,
            &payload,
            &mut ops,
        )?;
        ops.push(Op::Done);

        if let Err(e) = &self.context.run(self.core, &ops, None)?[0] {
            bail!("Failed to initiate bitstream load: {:x}", e);
        }

        let bar = ProgressBar::new(bitstream.len() as u64);
        bar.set_style(
            ProgressStyle::default_bar()
                .template("humility: xfer [{bar:30}] {bytes}/{total_bytes}"),
        );

        // Set up the call to `ecp5_load_bitstream_chunk`, which in turn calls
        // `Ecp5::continue_bitstream_load(..)`. Transferring data between
        // Humility and Hiffy is likely the bottleneck, so use a significant
        // chunk size. `ecp5_load_bitstream_chunk` will divide further divide in
        // smaller chunks as it sends data across task boundaries.
        let started = Instant::now();
        let mut bytes_transferred = 0;

        for chunk in bitstream.chunks(2048) {
            let ops = vec![
                Op::Push32(chunk.len().try_into().unwrap()),
                Op::Call(ecp5_write_bitstream_chunk.id),
                Op::Done,
            ];
            let results = self.context.run(self.core, &ops, Some(chunk))?;

            if let Err(e) = &results[0] {
                bar.finish_and_clear();
                bail!("Error while loading bitstream chunk: {:x}", e);
            }

            bytes_transferred += chunk.len();
            bar.set_position(bytes_transferred.try_into().unwrap());
        }

        bar.finish_and_clear();

        humility::msg!(
            "xfer of {} ({}) in {}",
            file_name,
            HumanBytes(bitstream.len() as u64),
            HumanDuration(started.elapsed())
        );

        let start_bitstream_load =
            self.hubris.get_idol_command("Fpga.finish_bitstream_load")?;
        let payload = start_bitstream_load
            .payload(&[("device_index", idol::IdolArgument::Scalar(0))])?;
        let mut ops = vec![];

        self.context.idol_call_ops(
            &start_bitstream_load,
            &payload,
            &mut ops,
        )?;
        ops.push(Op::Done);

        if let Err(e) = &self.context.run(self.core, &ops, None)?[0] {
            bail!("Failed to finalize bitstream load: {:x}", e);
        }

        Ok(())
    }
}

fn fpga(context: &mut ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let subargs = FpgaArgs::try_parse_from(subargs)?;
    let hubris = context.archive.as_ref().unwrap();
    let mut worker = FpgaHandler::new(hubris, core, subargs.timeout)?;

    match subargs.cmd {
        FpgaCommand::Reset => {
            worker.reset()?;
        }
        FpgaCommand::State => {
            worker.state()?;
        }
        FpgaCommand::Id => {
            worker.id()?;
        }
        FpgaCommand::LoadBitstream { file_path } => {
            let file_path = Path::new(&file_path);
            let file_name = file_path.file_name().unwrap().to_str().unwrap();
            let bitstream = fs::read(file_path)?;

            worker.reset()?;
            worker.load_bitstream(file_name, &bitstream)?;
        }
    }
    Ok(())
}

pub fn init() -> Command {
    Command {
        app: FpgaArgs::command(),
        name: "fpga",
        run: fpga,
        kind: CommandKind::Attached {
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
        },
    }
}
