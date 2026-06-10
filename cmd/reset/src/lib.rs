// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility reset`
//!
//! `humility reset` will hard reset the system using the debug pin
//! or using software reset with the appropriate flag

use anyhow::Result;
use clap::Parser;
use humility::log::info;
use humility_cli::{ExecutionContext, humility_cmd};

#[derive(Parser, Debug)]
#[clap(name = "reset", about = env!("CARGO_PKG_DESCRIPTION"))]
pub struct ResetArgs {
    /// Use a software reset instead of pin reset
    #[clap(long, conflicts_with_all = &["halt"])]
    soft_reset: bool,
    /// Reset and halt instead of continuing
    #[clap(long, conflicts_with_all = &["soft_reset"])]
    halt: bool,
    /// Use measurement token handoff (usually decided automatically)
    #[clap(long, conflicts_with_all = &["soft_reset", "halt"])]
    use_token: Option<bool>,
}

fn reset(subargs: ResetArgs, context: &mut ExecutionContext) -> Result<()> {
    let hubris = context.cli.try_archive()?;
    let log = context.log();

    let probe = match &context.cli.probe {
        Some(p) => p,
        None => "auto",
    };

    enum Behavior<'a> {
        Halt,
        ResetWithHandoff(&'a humility::hubris::HubrisArchive),
        Reset,
    }

    let behavior = if subargs.halt {
        Behavior::Halt
    } else {
        match subargs.use_token {
            None => {
                // Detect bogus archives by looking at the chip member
                if let Some(hubris) = &hubris
                    && hubris.wants_reset_handoff_token()
                {
                    Behavior::ResetWithHandoff(hubris)
                } else {
                    Behavior::Reset
                }
            }
            Some(false) => Behavior::Reset,
            Some(true) => {
                if let Some(hubris) = &hubris {
                    if !hubris.wants_reset_handoff_token() {
                        anyhow::bail!(
                            "--use-token=true was specified, but the archive \
                             does not have the relevant kernel feature"
                        );
                    }
                    Behavior::ResetWithHandoff(hubris)
                } else {
                    anyhow::bail!(
                        "Need a Hubris archive to use measurement token handoff"
                    )
                }
            }
        }
    };

    let r = if subargs.soft_reset
        || matches!(behavior, Behavior::Halt | Behavior::ResetWithHandoff(..))
    {
        let chip = hubris.as_ref().map(|h| h.chip()).transpose()?.ok_or_else(
            || {
                anyhow::anyhow!(
                    "Need an archive and chip to do a soft reset, \
                     halt after reset, or handoff"
                )
            },
        )?;
        let mut c = humility_probes_core::attach_to_chip(
            probe,
            Some(&chip),
            context.cli.speed,
            log,
        )?;
        match behavior {
            Behavior::Halt => {
                c.reset_and_halt(std::time::Duration::from_secs(2))
            }
            Behavior::ResetWithHandoff(archive) => {
                c.reset_with_handoff(archive, log)
            }
            Behavior::Reset => c.reset(),
        }
    } else {
        let mut probe = humility_probes_core::attach_to_probe(
            probe,
            context.cli.speed,
            log,
        )?;
        match behavior {
            Behavior::Halt | Behavior::ResetWithHandoff(..) => {
                unreachable!()
            }
            Behavior::Reset => {
                probe
                    .target_reset_assert()
                    .map_err(anyhow::Error::from)
                    .and_then(|()| {
                        // The closest available documentation on hold time is
                        // a comment giving a timeout
                        // https://open-cmsis-pack.github.io/Open-CMSIS-Pack-Spec/main/html/debug_description.html#resetHardwareDeassert
                        std::thread::sleep(std::time::Duration::from_millis(
                            1000,
                        ));

                        probe.target_reset_deassert()?;
                        Ok(())
                    })
            }
        }
    };

    if r.is_err() {
        info!(
            log,
            "There was an error when resetting. \
            The chip may be in an unknown state!"
        );
        info!(log, "Full error: {r:x?}");
    }

    Ok(())
}

humility_cmd!(ResetArgs, reset);
