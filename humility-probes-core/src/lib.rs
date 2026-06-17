// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use ::probe_rs::{
    Permissions,
    probe::{
        DebugProbeError, DebugProbeInfo, DebugProbeSelector, Probe,
        ProbeCreationError,
    },
};
use humility::{
    core::ProbeError,
    log::{Logger, info},
};

use anyhow::{Result, anyhow, bail};

mod probe_rs;

pub use probe_rs::ProbeCore;

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
    let lister = ::probe_rs::probe::list::Lister::new();
    let probes = lister.list_all();

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

fn open_probe_from_selector(
    selector: DebugProbeSelector,
    speed_khz: Option<u32>,
) -> Result<Probe> {
    let lister = ::probe_rs::probe::list::Lister::new();
    let mut probe = match lister.open(selector.clone()) {
        Ok(p) => p,
        Err(DebugProbeError::ProbeCouldNotBeCreated(
            ProbeCreationError::NotFound,
        )) => {
            if selector.serial_number.is_some() {
                bail!(
                    "Could not find probe {selector}.\n\
                    \n\
                    Because a serial number is present, this may be due to not \
                    running humility with permission to read USB device serial \
                    numbers; if not root already, run again as root?",
                );
            } else {
                bail!("Could not find probe {}.", selector);
            }
        }
        Err(DebugProbeError::Usb(err)) => {
            let msg = format!("Usb Error: {err:?}");
            if let Ok(rcode) = err.downcast::<nusb::Error>()
                && rcode.kind() == nusb::ErrorKind::Busy
            {
                bail!(
                    "USB link in use; is OpenOCD or another debugger running?"
                );
            } else {
                bail!(msg);
            }
        }
        Err(e) => bail!("{e:?}"),
    };

    if let Some(speed) = speed_khz {
        probe.set_speed(speed)?;
    };

    Ok(probe)
}

fn attach_err(e: ::probe_rs::Error) -> anyhow::Error {
    match e {
        ::probe_rs::Error::Arm(
            ::probe_rs::architecture::arm::ArmError::WrongApType,
        ) => {
            anyhow!(
                "Could not find the expected AP type. This has some common causes:\n \
                1) This is an RoT that has been locked and can no longer be accessed via probes.\n \
                2) This is an RoT that is being forced into ISP mode\n \
                3) This is an RoT that needs to be programmed via embootleby\n\
                4) The probe is attached to an RoT when you are trying to flash an SP or vice versa\n
                "
            )
        }
        e => anyhow::Error::new(e),
    }
}

#[rustfmt::skip::macros(anyhow, bail)]
pub fn attach_to_probe(
    probe: &str,
    speed_khz: Option<u32>,
    log: &Logger,
) -> Result<::probe_rs::probe::Probe> {
    let (probe, index) = parse_probe(probe);

    match probe {
        "usb" => {
            let probe_info = get_usb_probe(index)?;

            let mut probe = probe_info.open()?;
            if let Some(speed) = speed_khz {
                probe.set_speed(speed)?;
            }

            info!(log, "Opened probe {}", probe_info.identifier);
            Ok(probe)
        }
        "auto" => attach_to_probe("usb", speed_khz, log),
        _ => match probe.parse::<DebugProbeSelector>() {
            Ok(selector) => {
                let vidpid = probe;
                let probe = open_probe_from_selector(selector, speed_khz)?;

                info!(log, "Opened {vidpid}");
                Ok(probe)
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
    log: &Logger,
) -> Result<probe_rs::ProbeCore> {
    let (probe, index) = parse_probe(probe);

    match probe {
        "usb" => {
            let probe_info = get_usb_probe(index)?;

            let mut probe = probe_info.open()?;
            if let Some(speed) = speed_khz {
                probe.set_speed(speed)?;
            }

            //
            // probe-rs needs us to specify a chip that it knows about -- but
            // it only really uses this information for flashing the part.  If
            // we are attaching to the part for not pusposes of flashing, we

            // specify a generic Cortex-M3 (but then we also indicate that can't
            // flash to assure that we can fail explicitly should flashing be
            // attempted).
            //
            let (session, can_flash) = match chip {
                Some(chip) => (
                    probe
                        .attach(chip, Permissions::new().allow_erase_all())
                        .map_err(attach_err)?,
                    true,
                ),
                None => (
                    probe
                        .attach("Cortex-M3", Permissions::new())
                        .map_err(attach_err)?,
                    false,
                ),
            };

            info!(
                log,
                "attached via {:x}:{:x}:{:?} {}",
                probe_info.vendor_id,
                probe_info.product_id,
                probe_info.serial_number,
                probe_info.identifier
            );

            Ok(probe_rs::ProbeCore::new(
                session,
                probe_info.identifier.clone(),
                probe_info.vendor_id,
                probe_info.product_id,
                probe_info.serial_number,
                can_flash,
                log,
            ))
        }
        "auto" => attach_to_chip("usb", chip, speed_khz, log),

        _ => match probe.parse::<DebugProbeSelector>() {
            Ok(selector) => {
                let vidpid = probe;

                let vid = selector.vendor_id;
                let pid = selector.product_id;
                let serial = selector.serial_number.clone();

                let probe = open_probe_from_selector(selector, speed_khz)?;
                let name = probe.get_name();

                //
                // See the block comment in the generic "usb" attach for
                // why we use Cortex-M3 here.
                //
                let (session, can_flash) = match chip {
                    Some(chip) => (
                        probe
                            .attach(chip, Permissions::new().allow_erase_all())
                            .map_err(attach_err)?,
                        true,
                    ),
                    None => {
                        (probe.attach("Cortex-M3", Permissions::new())?, false)
                    }
                };

                info!(log, "attached to {vidpid} via {name}");

                Ok(probe_rs::ProbeCore::new(
                    session, name, vid, pid, serial, can_flash, log,
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
    log: &Logger,
) -> Result<probe_rs::ProbeCore> {
    attach_to_chip(probe, Some(chip), speed_khz, log)
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
    fn attach_probe(
        &self,
        probe: &str,
        log: &Logger,
    ) -> Result<probe_rs::ProbeCore>;
}

impl HubrisAttach for humility::hubris::HubrisArchive {
    fn attach_probe(
        &self,
        probe: &str,
        log: &Logger,
    ) -> Result<probe_rs::ProbeCore> {
        let mut core =
            attach_to_chip(probe, self.chip().as_deref(), None, log)?;

        self.validate(
            &mut core,
            humility::hubris::HubrisValidate::ArchiveMatch,
        )?;

        Ok(core)
    }
}
