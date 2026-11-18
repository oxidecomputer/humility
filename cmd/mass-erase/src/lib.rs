// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility mass-erase`
//!
//! Erase all nonvolatile memory on the attached target. What qualifies
//! as "nonvolatile" is going to vary from target to target but the
//! expectation is that after running this command there will be no
//! running hubris image on target.
//!
//! Note that a hubris archive is required to determine the specific
//! chip to erase.

use anyhow::{Result, bail};
use clap::Parser;
use humility::log::info;
use humility_cli::{ExecutionContext, humility_cmd};
use indicatif::{MultiProgress, ProgressBar};
use probe_rs::flashing::ProgressEvent;

#[derive(Parser, Debug)]
#[clap(
    name = "mass-erase", about = env!("CARGO_PKG_DESCRIPTION"),
)]
pub struct MassEraseArgs {
    /// Yes you really want to erase.
    #[clap(long, short)]
    yes: bool,
}

fn mass_erase(
    subargs: MassEraseArgs,
    context: &mut ExecutionContext,
) -> Result<()> {
    if !subargs.yes {
        bail!(
            "Pass --yes to this command to confirm you really want to mass erase"
        );
    }

    let hubris = &context.cli.archive()?;
    let log = context.log();

    let probe = match &context.cli.probe {
        Some(p) => p,
        None => "auto",
    };

    let chip = hubris.chip()?;
    info!(log, "attaching with chip set to {chip:x?}");
    let core = &mut humility_probes_core::attach_for_flashing(
        probe,
        &chip,
        context.cli.speed,
        log,
    )?;

    let multi_progress = MultiProgress::new();
    let mut progress_bars = humility_probes_core::ProgressBars::default();

    let mut progress =
        probe_rs::flashing::FlashProgress::new(move |event| match event {
            ProgressEvent::FlashLayoutReady { .. } => {}

            ProgressEvent::AddProgressBar { operation, total } => {
                let bar = multi_progress.add(if let Some(total) = total {
                    // We were promised a length, but in this implementation it
                    // may come later in the Started message. Set to at least 1
                    // to avoid progress bars starting from 100%
                    ProgressBar::new(total.max(1))
                } else {
                    ProgressBar::no_length()
                });
                progress_bars.get_mut(operation).add(bar);
            }
            ProgressEvent::Started(operation) => {
                progress_bars.get_mut(operation).mark_start_now();
            }
            ProgressEvent::Progress { operation, size, time: _ } => {
                progress_bars.get_mut(operation).inc(size);
            }
            ProgressEvent::Failed(operation) => {
                progress_bars.get_mut(operation).abandon();
            }
            ProgressEvent::Finished(operation) => {
                progress_bars.get_mut(operation).finish();
            }
            ProgressEvent::DiagnosticMessage { message } => {
                println!("{}", message);
            }
        });

    // o7
    probe_rs::flashing::erase_all(&mut core.session, &mut progress, false)?;
    Ok(())
}

humility_cmd!(MassEraseArgs, mass_erase);
