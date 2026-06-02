// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use ::probe_rs::{
    DebugProbeError, DebugProbeInfo, DebugProbeSelector, Probe,
    ProbeCreationError,
};
use humility::core::{Core, ProbeError};

use anyhow::{Result, anyhow, bail};
use humility::msg;

mod probe_rs;
mod unattached;

pub use probe_rs::ProbeCore;
pub use unattached::UnattachedCore;

fn parse_probe(probe: &str) -> (&str, Option<usize>) {
    if probe.contains('-') {
        let str = probe.to_owned();
        let pieces: Vec<&str> = str.split('-').collect();

        if pieces[0] == "usb" && pieces.len() == 2 {
            if let Ok(val) = pieces[1].parse::<usize>() {
                ("usb", Some(val))
            } else {
                (probe, None)
            }
        } else {
            (probe, None)
        }
    } else {
        (probe, None)
    }
}

fn get_usb_probe(index: Option<usize>) -> Result<DebugProbeInfo> {
    let probes = Probe::list_all();

    if probes.is_empty() {
        return Err(ProbeError::NoProbeFound.into());
    }

    if let Some(index) = index {
        if index < probes.len() {
            Ok(probes[index].clone())
        } else {
            bail!(
                "index ({}) exceeds max probe index ({})",
                index,
                probes.len() - 1
            );
        }
    } else if probes.len() == 1 {
        Ok(probes[0].clone())
    } else {
        bail!(
            "multiple USB probes detected; must \
            explicitly append index (e.g., \"-p usb-0\")"
        );
    }
}

/// [`probe_rs::Probe::open`] with specialized error messages and speed
/// configuration
fn open_probe<T: Into<DebugProbeSelector> + Clone>(
    selector: T,
    speed_khz: Option<u32>,
) -> Result<Probe> {
    let probe_selector: DebugProbeSelector = selector.clone().into();
    let res = Probe::open(selector);

    // the following error customizations could be a match statement but until
    // if let guards stabilize it would be a kludge

    if let Err(DebugProbeError::ProbeCouldNotBeCreated(
        ProbeCreationError::NotFound,
    )) = res
    {
        if probe_selector.serial_number.is_some() {
            bail!(
                "Could not find probe {}.\n\
                \n\
                Because a serial number is present, this may be due to not \
                running humility with permission to read USB device serial \
                numbers; if not root already, run again as root?",
                probe_selector
            );
        } else {
            bail!("Could not find probe {}.", probe_selector);
        }
    }

    if let Err(DebugProbeError::Usb(Some(ref err))) = res
        && let Some(rcode) = err.downcast_ref::<rusb::Error>()
        && *rcode == rusb::Error::Busy
    {
        bail!("USB link in use; is OpenOCD or another debugger running?");
    }

    let mut probe = res?;

    if let Some(speed) = speed_khz {
        probe.set_speed(speed)?;
    };

    Ok(probe)
}

#[rustfmt::skip::macros(anyhow, bail)]
pub fn attach_to_probe(
    probe: &str,
    speed_khz: Option<u32>,
) -> Result<Box<dyn Core>> {
    let (probe, index) = parse_probe(probe);

    match probe {
        "usb" => {
            let probe_info = get_usb_probe(index)?;

            let probe = open_probe(&probe_info, speed_khz)?;

            crate::msg!("Opened probe {}", probe_info.identifier);
            Ok(Box::new(unattached::UnattachedCore::new(
                probe,
                probe_info.identifier.clone(),
                probe_info.vendor_id,
                probe_info.product_id,
                probe_info.serial_number,
            )))
        }
        "auto" => attach_to_probe("usb", speed_khz),
        _ => match TryInto::<DebugProbeSelector>::try_into(probe) {
            Ok(selector) => {
                let vidpid = probe;
                let vid = selector.vendor_id;
                let pid = selector.product_id;
                let serial = selector.serial_number.clone();
                let probe = open_probe(selector, speed_khz)?;
                let name = probe.get_name();

                crate::msg!("Opened {vidpid} via {name}");
                Ok(Box::new(unattached::UnattachedCore::new(
                    probe, name, vid, pid, serial,
                )))
            }
            Err(_) => Err(anyhow!("unrecognized probe: {}", probe)),
        },
    }
}

#[rustfmt::skip::macros(anyhow, bail)]
pub fn attach_to_chip(
    probe: &str,
    chip: Option<&str>,
    speed_khz: Option<u32>,
) -> Result<probe_rs::ProbeCore> {
    let (probe, index) = parse_probe(probe);

    match probe {
        "usb" => {
            let probe_info = get_usb_probe(index)?;

            let probe = open_probe(&probe_info, speed_khz)?;

            let name = probe.get_name();
            //
            // probe-rs needs us to specify a chip that it knows about -- but
            // it only really uses this information for flashing the part.  If
            // we are attaching to the part for not pusposes of flashing, we
            // specify a generic ARMv7-M (but then we also indicate that can't
            // flash to assure that we can fail explicitly should flashing be
            // attempted).
            //
            let (session, can_flash) = match chip {
                Some(chip) => (probe.attach(chip)?, true),
                None => (probe.attach("armv7m")?, false),
            };

            crate::msg!("attached via {name}");

            Ok(probe_rs::ProbeCore::new(
                session,
                probe_info.identifier.clone(),
                probe_info.vendor_id,
                probe_info.product_id,
                probe_info.serial_number,
                can_flash,
            ))
        }
        "auto" => attach_to_chip("usb", chip, speed_khz),

        _ => match TryInto::<DebugProbeSelector>::try_into(probe) {
            Ok(selector) => {
                let vidpid = probe;

                let vid = selector.vendor_id;
                let pid = selector.product_id;
                let serial = selector.serial_number.clone();

                let probe = open_probe(selector, speed_khz)?;
                let name = probe.get_name();

                //
                // See the block comment in the generic "usb" attach for
                // why we use armv7m here.
                //
                let (session, can_flash) = match chip {
                    Some(chip) => (probe.attach(chip)?, true),
                    None => (probe.attach("armv7m")?, false),
                };

                crate::msg!("attached to {vidpid} via {name}");

                Ok(probe_rs::ProbeCore::new(
                    session, name, vid, pid, serial, can_flash,
                ))
            }
            Err(_) => Err(anyhow!("unrecognized probe: {probe}")),
        },
    }
}

pub fn attach_for_flashing(
    probe: &str,
    chip: &str,
    speed_khz: Option<u32>,
) -> Result<probe_rs::ProbeCore> {
    attach_to_chip(probe, Some(chip), speed_khz)
}

/// Trait to make it easier for libraries to attach to
/// a probe via a hubris archive. This may gain more
/// functions as we find more ways users want to attach
/// to probes.
pub trait HubrisAttach {
    /// Attach to the specified probe using the chip in the
    /// hubris archive and that the archive matches the image
    /// id present on target. If no chip is is present in the archive
    /// this will still attach.
    fn attach_probe(&self, probe: &str) -> Result<probe_rs::ProbeCore>;
}

impl HubrisAttach for humility::hubris::HubrisArchive {
    fn attach_probe(&self, probe: &str) -> Result<probe_rs::ProbeCore> {
        let mut core = attach_to_chip(probe, self.chip().as_deref(), None)?;

        self.validate(
            &mut core,
            humility::hubris::HubrisValidate::ArchiveMatch,
        )?;

        Ok(core)
    }
}
