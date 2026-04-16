// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility lsusb`
//!
//! `humility lsusb` will show you Humility's view of the USB devices available
//! on the system, to help you choose probes and/or diagnose permissions issues.

use anyhow::{Context, Result};
use clap::Parser;
use humility_cli::{ExecutionContext, humility_cmd};
use humility_log::{info, warn};
use nusb::{MaybeFuture, descriptors::language_id::US_ENGLISH};
use std::collections::HashMap;
use std::time::Duration;

#[derive(Parser, Debug)]
#[clap(name = "lsusb", about = env!("CARGO_PKG_DESCRIPTION"))]
pub struct LsUsbArgs {
    // None as yet
}

fn lsusb(_args: LsUsbArgs, context: &mut ExecutionContext) -> Result<()> {
    let log = context.log();
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

    let devices = nusb::list_devices().wait()?;
    let mut successes = vec![];
    let mut failures = vec![];
    for dev in devices {
        match list1(&dev) {
            Ok(summary) => successes.push(summary),
            Err(e) => {
                failures.push((
                    // `bus_id` as an &'str is common across all OSes
                    dev.bus_id().to_owned(),
                    dev.device_address(),
                    dev.port_chain()[0],
                    e,
                ));
            }
        }
    }

    info!(
        log,
        "USB device scan, {} successful and {} failed",
        successes.len(),
        failures.len()
    );

    if !successes.is_empty() {
        successes.sort();
        info!(log, "--- successfully opened devices ---");
        info!(
            log,
            "format: VID:PID:SERIAL, then manufacturer name, \
            then product name"
        );
        for (ident, desc) in successes {
            if let Some(target) = targets.remove(&ident) {
                info!(log, "{ident}\t{desc} (target: {target})");
            } else {
                info!(log, "{ident}\t{desc}");
            }
        }
    }

    if !failures.is_empty() {
        failures.sort_by(|lhs, rhs| {
            (&lhs.0, lhs.1, lhs.2).cmp(&(&rhs.0, rhs.1, rhs.2))
        });
        info!(log, "--- failures ---");
        info!(log, "could not access {} devices:", failures.len());
        for (bus, addr, port, e) in failures {
            info!(log, "bus {bus}, addr {addr}, port {port}: {e:#}");
        }
    }

    if !targets.is_empty() {
        let env =
            context.cli.environment.as_ref().expect(
                "if `targets` is non-empty, `environment` must be Some",
            );
        warn!(
            log,
            "--- could not find {} probes declared in HUMILITY_ENVIRONMENT ---",
            targets.len()
        );
        let target_len = targets
            .values()
            .map(String::len)
            .max()
            .expect("must be `Some`, as `targets` is not empty");
        warn!(log, "HUMILITY_ENVIRONMENT={env}");
        warn!(log, "{:<target_len$} PROBE", "TARGET");
        for (probe, target) in targets {
            warn!(log, "{target:<target_len$} {probe}");
        }
    }

    Ok(())
}

fn list1(dev: &nusb::DeviceInfo) -> Result<(String, String)> {
    let timeout = Duration::from_millis(100);
    let vid = dev.vendor_id();
    let pid = dev.product_id();

    let dev = dev
        .open()
        .wait()
        .with_context(|| format!("open failed for {vid:04x}:{pid:04x}:???"))?;
    let dev_descriptor = dev.device_descriptor();

    let languages: Vec<u16> = dev
        .get_string_descriptor_supported_languages(timeout)
        .wait()
        .map(|i| i.collect())
        .unwrap_or_default();

    let language = languages.first().copied().unwrap_or(US_ENGLISH);

    let man = dev_descriptor
        .manufacturer_string_index()
        .and_then(|i| {
            dev.get_string_descriptor(i, language, timeout).wait().ok()
        })
        .unwrap_or_else(|| "(manufacturer unknown)".to_string());

    let prod = dev_descriptor
        .product_string_index()
        .and_then(|i| {
            dev.get_string_descriptor(i, language, timeout).wait().ok()
        })
        .unwrap_or_else(|| "(product unknown)".to_string());

    let serial = dev_descriptor
        .serial_number_string_index()
        .and_then(|i| {
            dev.get_string_descriptor(i, language, timeout).wait().ok()
        })
        .unwrap_or_else(|| "(serial unknown)".to_string());

    Ok((format!("{vid:04x}:{pid:04x}:{serial}"), format!("{man}\t{prod}")))
}

humility_cmd!(LsUsbArgs, lsusb);
