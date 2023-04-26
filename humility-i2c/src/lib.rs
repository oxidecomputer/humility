// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::{bail, Context, Result};
use humility::hubris::*;
use std::fmt;

#[derive(Debug)]
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

    pub fn matches_device(&self, device: &'a HubrisI2cDevice) -> bool {
        if device.controller != self.controller {
            return false;
        }

        if device.port.index != self.port.index {
            return false;
        }

        if let Some(address) = self.address {
            if address != device.address {
                return false;
            }
        }

        if let Some((m, s)) = self.mux {
            match (device.mux, device.segment) {
                (Some(mux), Some(segment)) => {
                    if mux != m || segment != s {
                        return false;
                    }
                }
                (_, _) => {
                    return false;
                }
            }
        }

        true
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
                    if found.next().is_some() {
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
        if hubris.manifest.i2c_buses.is_empty() {
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
                    // been given a device; if that device hasn't been passed
                    // as an address, see if we can find exactly one of those
                    // devices.
                    //
                    if let Some(device) = device {
                        if parse_int::parse::<u8>(device).is_ok() {
                            bail!(
                                "need a controller or bus in addition to \
                                a device address"
                            );
                        }

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
                    (controller, hubris.lookup_i2c_port(controller, port)?)
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
                .map(parse_int::parse::<u8>)
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
                if let Ok(address) = parse_int::parse::<u8>(device) {
                    //
                    // We have been provided a controller, port, mux/segment
                    // and address.  If we actually have a device in the
                    // manifest that matches this, we want to return its class
                    // and driver.  (If we don't, we fall into the Unknown
                    // class to allow for I2C operations to continue, albeit
                    // without any additional device information.)
                    //
                    let mut found = hubris
                        .manifest
                        .i2c_devices
                        .iter()
                        .filter(|d| d.controller == controller)
                        .filter(|d| d.port.index == port.index)
                        .filter(|d| d.address == address)
                        .filter(|d| match (mux, d.mux, d.segment) {
                            (None, None, None) => true,
                            (Some((m, s)), Some(dm), Some(ds)) => {
                                m == dm && s == ds
                            }
                            _ => false,
                        });

                    if let Some(d) = found.next() {
                        if let Some(n) = found.next() {
                            //
                            // We really don't expect this to happen: it means
                            // that there are multiple device definitions that
                            // have the same controller, port, address and
                            // mux/segment.  (This is a bail!() instead of a
                            // panic!() because it is possible with bad Hubris
                            // TOML.)
                            //
                            bail!("devices {:?} and {:?} conflict!", d, n);
                        }

                        return Ok(Self {
                            controller,
                            port,
                            mux,
                            address: Some(address),
                            device: Some(d.device.to_string()),
                            class: &d.class,
                        });
                    }

                    (None, Some(address))
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

        let class = &HubrisI2cDeviceClass::Unknown;

        Ok(Self { controller, port, mux, device, address, class })
    }
}
