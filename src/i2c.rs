// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::hubris::*;
use anyhow::{bail, Context, Result};
use std::fmt;

pub struct I2cArgs<'a> {
    pub controller: u8,
    pub port: &'a HubrisI2cPort,
    pub mux: Option<(u8, u8)>,
    pub device: Option<String>,
    pub address: Option<u8>,
    pub class: &'a HubrisI2cDeviceClass,
}

impl fmt::Display for I2cArgs<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "I2C{}, port {}", self.controller, self.port.name)?;

        if let Some((mux, segment)) = self.mux {
            write!(f, ", seg {}:{}", mux, segment)?;
        }

        if let Some(address) = self.address {
            write!(f, ", dev 0x{:02x}", address)?;
        }

        Ok(())
    }
}

impl<'a> I2cArgs<'a> {
    pub fn from_device(device: &'a HubrisI2cDevice) -> Self {
        Self {
            controller: device.controller,
            port: &device.port,
            mux: match (device.mux, device.segment) {
                (Some(m), Some(s)) => Some((m, s)),
                (None, None) => None,
                _ => {
                    panic!("bad mux/segment on {}", device.device);
                }
            },
            address: Some(device.address),
            device: Some(device.device.clone()),
            class: &device.class,
        }
    }

    pub fn parse(
        hubris: &'a HubrisArchive,
        bus: &Option<String>,
        controller: Option<u8>,
        port: &Option<String>,
        mux: &Option<String>,
        device: &Option<String>,
    ) -> Result<Self> {
        //
        // A helper function to translate a found device into something we can
        // return, checking that we have found exactly one device that matches
        // the criteria.
        //
        fn translate_device<'a, T>(
            device: &str,
            mut found: T,
        ) -> Result<I2cArgs<'a>>
        where
            T: Iterator<Item = &'a HubrisI2cDevice>,
        {
            match found.next() {
                None => {
                    bail!("unknown device {}", device);
                }
                Some(d) => {
                    if let Some(_) = found.next() {
                        bail!("multiple {} devices found", device);
                    }

                    Ok(I2cArgs {
                        controller: d.controller,
                        port: &d.port,
                        mux: match (d.mux, d.segment) {
                            (Some(m), Some(s)) => Some((m, s)),
                            (None, None) => None,
                            _ => {
                                bail!("{}: bad mux/segment", device);
                            }
                        },
                        address: Some(d.address),
                        device: Some(device.to_string()),
                        class: &d.class,
                    })
                }
            }
        }

        //
        // If we have no I2C controllers, give an explicit error message.
        //
        if hubris.manifest.i2c_buses.len() == 0 {
            bail!("no I2C buses found; is this an old Hubris image?");
        }

        //
        // If we were given a bus, that will guide us to our controller and
        // port
        //
        let (controller, port) = if let Some(bus) = bus {
            if controller.is_some() {
                bail!("cannot specify both a bus and a controller");
            }

            if port.is_some() {
                bail!("cannot specity both a bus and a port");
            }

            let bus = hubris.lookup_i2c_bus(bus)?;
            (bus.controller, &bus.port)
        } else {
            match (controller, port) {
                (None, Some(_)) => {
                    bail!(
                        "cannot specify a port alone; need a controller or bus"
                    );
                }
                (None, None) => {
                    //
                    // If we haven't been given a controller or a bus, but we've
                    // been given a device, see if we can
                    // find exactly one of those devices.
                    //
                    if let Some(device) = device {
                        let found = hubris
                            .manifest
                            .i2c_devices
                            .iter()
                            .filter(|d| d.device == *device);

                        return translate_device(device, found);
                    } else {
                        bail!("must specify either a controller or a bus");
                    }
                }
                (Some(controller), Some(port)) => {
                    //
                    // We have a controller and a port; that should be an exact
                    // match.
                    //
                    (controller, hubris.lookup_i2c_port(controller, &port)?)
                }
                (Some(controller), None) => {
                    //
                    // We have been given a controller but no port; if there is
                    // exactly one bus for this controller, we will use its
                    // port, otherwise we will bail.
                    //
                    (controller, hubris.i2c_port(controller)?)
                }
            }
        };

        let mux = if let Some(mux) = &mux {
            let s = mux
                .split(':')
                .map(|v| parse_int::parse::<u8>(v))
                .collect::<Result<Vec<_>, _>>()
                .context("expected multiplexer and segment to be integers")?;

            if s.len() == 2 {
                Some((s[0], s[1]))
            } else if s.len() == 1 {
                Some((0, s[0]))
            } else {
                bail!("expected only multiplexer and segment identifiers");
            }
        } else {
            None
        };

        let (device, address) = match device {
            None => (None, None),
            Some(device) => {
                if let Ok(val) = parse_int::parse::<u8>(device) {
                    (None, Some(val))
                } else {
                    let found = hubris
                        .manifest
                        .i2c_devices
                        .iter()
                        .filter(|d| d.device == *device)
                        .filter(|d| d.controller == controller)
                        .filter(|d| d.port.index == port.index);

                    return translate_device(device, found);
                }
            }
        };

        Ok(Self {
            controller: controller,
            port: port,
            mux: mux,
            device: device,
            address: address,
            class: &HubrisI2cDeviceClass::Unknown,
        })
    }
}
