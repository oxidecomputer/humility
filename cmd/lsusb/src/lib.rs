// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility lsusb`
//!
//! `humility lsusb` will show you Humility's view of the USB devices available
//! on the system, to help you choose probes and/or diagnose permissions issues.

use anyhow::{anyhow, Context, Result};
use clap::{CommandFactory, Parser};
use humility_cli::{ExecutionContext, Subcommand};
use humility_cmd::{Archive, Command, CommandKind};
use std::collections::HashMap;
use std::time::Duration;

#[derive(Parser, Debug)]
#[clap(name = "lsusb", about = env!("CARGO_PKG_DESCRIPTION"))]
struct Args {
    // None as yet
}

fn lsusb(context: &mut ExecutionContext) -> Result<()> {
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let _subargs = Args::try_parse_from(subargs)?;
    let mut targets = if let Some(ref env) = context.cli.environment {
        humility_cli::env::Environment::read(env)
            .with_context(|| {
                format!("failed to read environment from '{env}'")
            })?
            .into_iter()
            .map(|(name, target)| (target.probe, name))
            .collect::<HashMap<_, _>>()
    } else {
        HashMap::new()
    };

    let devices = rusb::devices()?;
    let mut successes = vec![];
    let mut failures = vec![];
    for dev in devices.iter() {
        match list1(&dev) {
            Ok(summary) => successes.push(summary),
            Err(e) => {
                failures.push((
                    dev.bus_number(),
                    dev.address(),
                    dev.port_number(),
                    e,
                ));
            }
        }
    }

    humility::msg!(
        "USB device scan, {} successful and {} failed",
        successes.len(),
        failures.len()
    );

    if !successes.is_empty() {
        successes.sort();
        humility::msg!("--- successfully opened devices ---");
        humility::msg!(
            "format: VID:PID:SERIAL, then manufacturer name, \
            then product name"
        );
        for (ident, desc) in successes {
            if let Some(target) = targets.remove(&ident) {
                humility::msg!("{ident}\t{desc} (target: {target})");
            } else {
                humility::msg!("{ident}\t{desc}");
            }
        }
    }

    if !failures.is_empty() {
        failures.sort_by_key(|(b, a, p, _)| (*b, *a, *p));
        humility::msg!("--- failures ---");
        humility::msg!("could not access {} devices:", failures.len());
        for (bus, addr, port, e) in failures {
            humility::msg!("bus {bus}, addr {addr}, port {port}: {e}");
        }
    }

    if !targets.is_empty() {
        let env =
            context.cli.environment.as_ref().expect(
                "if `targets` is non-empty, `environment` must be Some",
            );
        humility::warn!(
            "--- could not find {} probes declared in HUMILITY_ENVIRONMENT ---",
            targets.len()
        );
        let target_len = targets
            .values()
            .map(String::len)
            .max()
            .expect("must be `Some`, as `targets` is not empty");
        humility::warn!("HUMILITY_ENVIRONMENT={env}");
        humility::warn!("{:<target_len$} PROBE", "TARGET");
        for (probe, target) in targets {
            humility::warn!("{target:<target_len$} {probe}");
        }
    }

    Ok(())
}

fn list1(
    dev: &rusb::Device<impl rusb::UsbContext>,
) -> Result<(String, String)> {
    const TIMEOUT: Duration = Duration::from_secs(1);

    let desc = dev.device_descriptor()?;
    let vid = desc.vendor_id();
    let pid = desc.product_id();

    let handle = match dev.open() {
        Ok(handle) => handle,
        Err(e) => {
            return Err(anyhow!("{vid:04x}:{pid:04x}:???\topen failed: {e}"));
        }
    };
    let lang = *handle
        .read_languages(TIMEOUT)?
        .iter()
        .find(|lang| lang.primary_language() == rusb::PrimaryLanguage::English)
        .ok_or_else(|| anyhow!("can't find English strings"))?;

    let man = handle
        .read_manufacturer_string(lang, &desc, TIMEOUT)
        .unwrap_or_else(|_| "(manufacturer unknown)".to_string());
    let prod = handle
        .read_product_string(lang, &desc, TIMEOUT)
        .unwrap_or_else(|_| "(product unknown)".to_string());
    let serial = handle
        .read_serial_number_string(lang, &desc, TIMEOUT)
        .unwrap_or_else(|_| "(serial unknown)".to_string());

    Ok((format!("{vid:04x}:{pid:04x}:{serial}"), format!("{man}\t{prod}")))
}

pub fn init() -> Command {
    Command {
        app: Args::command(),
        name: "lsusb",
        run: lsusb,
        kind: CommandKind::Unattached { archive: Archive::Ignored },
    }
}
