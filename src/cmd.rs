/*
 * Copyright 2020 Oxide Computer Company
 */

mod apptable;
mod diagnose;
mod dump;
mod etm;
mod gpio;
mod hiffy;
mod i2c;
mod itm;
mod jefe;
mod manifest;
mod map;
mod pmbus;
mod probe;
mod qspi;
mod readmem;
mod readvar;
mod rencm;
mod ringbuf;
mod spd;
mod spi;
mod stackmargin;
mod stmsecure;
mod tasks;
mod test;
mod trace;

use crate::core::Core;
use crate::hubris::*;
use crate::Args;
use crate::{attach_dump, attach_live};
use anyhow::{bail, Context, Result};
use std::collections::HashMap;
use std::convert::TryFrom;
use std::convert::TryInto;
use structopt::clap::App;

#[allow(dead_code)]
#[derive(Copy, Clone, Debug)]
pub enum Archive {
    Required,
    Optional,
    Prohibited,
}

#[allow(dead_code)]
#[derive(Copy, Clone, Debug)]
pub enum Attach {
    LiveOnly,
    DumpOnly,
    Any,
}

#[allow(dead_code)]
#[derive(Copy, Clone, Debug)]
pub enum Validate {
    Match,
    Booted,
    None,
}

pub enum Command {
    Attached {
        name: &'static str,
        archive: Archive,
        attach: Attach,
        validate: Validate,
        run: fn(
            &mut HubrisArchive,
            &mut dyn Core,
            &Args,
            &Vec<String>,
        ) -> Result<()>,
    },
    Unattached {
        name: &'static str,
        archive: Archive,
        run: fn(&mut HubrisArchive, &Args, &Vec<String>) -> Result<()>,
    },
}

pub fn init<'a, 'b>(
    app: App<'a, 'b>,
) -> (HashMap<&'static str, Command>, App<'a, 'b>) {
    let mut cmds = HashMap::new();
    let mut rval = app;

    let dcmds = [
        apptable::init,
        diagnose::init,
        dump::init,
        etm::init,
        gpio::init,
        hiffy::init,
        i2c::init,
        itm::init,
        jefe::init,
        manifest::init,
        map::init,
        pmbus::init,
        probe::init,
        qspi::init,
        readmem::init,
        readvar::init,
        rencm::init,
        ringbuf::init,
        spd::init,
        spi::init,
        stackmargin::init,
        tasks::init,
        test::init,
        trace::init,
        stmsecure::init,
    ];

    for dcmd in &dcmds {
        let (cmd, subcmd) = dcmd();

        let name = match cmd {
            Command::Attached { name, .. } => name,
            Command::Unattached { name, .. } => name,
        };

        cmds.insert(name, cmd);
        rval = rval.subcommand(subcmd);
    }

    (cmds, rval)
}

pub fn subcommand(
    commands: &HashMap<&'static str, Command>,
    hubris: &mut HubrisArchive,
    args: &Args,
    subargs: &Vec<String>,
) -> Result<()> {
    if let Some(command) = commands.get(&subargs[0].as_str()) {
        let archive = match command {
            Command::Attached { archive, .. } => archive,
            Command::Unattached { archive, .. } => archive,
        };

        match (archive, hubris.loaded()) {
            (Archive::Required, false) => {
                bail!("must provide a Hubris archive or dump");
            }

            (Archive::Prohibited, true) => {
                bail!("does not operate on a Hubris archive or dump");
            }

            (_, _) => {}
        }

        match command {
            Command::Attached { run, attach, validate, .. } => {
                let mut c = match attach {
                    Attach::LiveOnly => attach_live(args),
                    Attach::DumpOnly => attach_dump(args, hubris),
                    Attach::Any => {
                        if args.dump.is_some() {
                            attach_dump(args, hubris)
                        } else {
                            attach_live(args)
                        }
                    }
                }?;

                let core = c.as_mut();

                match validate {
                    Validate::Booted => {
                        hubris.validate(core, HubrisValidate::Booted)?;
                    }
                    Validate::Match => {
                        hubris.validate(core, HubrisValidate::ArchiveMatch)?;
                    }
                    Validate::None => {}
                }

                (run)(hubris, core, args, subargs)
            }
            Command::Unattached { run, .. } => (run)(hubris, args, subargs),
        }
    } else {
        bail!("command {} not found", subargs[0]);
    }
}

fn printmem(bytes: &[u8], addr: u32, size: usize, width: usize) {
    let mut addr = addr;

    let print = |line: &[u8], addr, offs| {
        print!("0x{:08x} | ", addr);

        for i in (0..width).step_by(size) {
            if i < offs || i - offs >= line.len() {
                print!(" {:width$}", "", width = size * 2);
                continue;
            }

            let slice = &line[i - offs..i - offs + size];

            print!(
                "{:0width$x} ",
                match size {
                    1 => line[i - offs] as u32,
                    2 => u16::from_le_bytes(slice.try_into().unwrap()) as u32,
                    4 => u32::from_le_bytes(slice.try_into().unwrap()) as u32,
                    _ => {
                        panic!("invalid size");
                    }
                },
                width = size * 2
            );
        }

        print!("| ");

        for i in 0..width {
            if i < offs || i - offs >= line.len() {
                print!(" ");
            } else {
                let c = line[i - offs] as char;

                if c.is_ascii() && !c.is_ascii_control() {
                    print!("{}", c);
                } else {
                    print!(".");
                }
            }
        }

        println!();
    };

    let offs = (addr & (width - 1) as u32) as usize;
    addr -= offs as u32;

    /*
     * Print out header line, OpenBoot PROM style
     */
    print!("  {:8}  ", "");

    for i in (0..width).step_by(size) {
        if i == offs {
            print!(" {:>width$}", "\\/", width = size * 2);
        } else {
            print!(" {:>width$x}", i, width = size * 2);
        }
    }

    println!();

    /*
     * Print our first line.
     */
    let lim = std::cmp::min(width - offs, bytes.len());
    print(&bytes[0..lim], addr, offs);

    if lim < bytes.len() {
        let lines = bytes[lim..].chunks(width);

        for line in lines {
            addr += width as u32;
            print(line, addr, 0);
        }
    }
}

struct HiffyI2cArgs<'a> {
    controller: u8,
    port: u8,
    mux: Option<(u8, u8)>,
    device: Option<String>,
    address: Option<u8>,
    class: &'a HubrisI2cDeviceClass,
}

///
/// A routine to convert from a found device into a [`HiffyI2cArg`]
///
fn hiffy_i2c_arg<'a>(
    hubris: &'a HubrisArchive,
    goff: HubrisGoff,
    device: &'a HubrisI2cDevice,
) -> Result<HiffyI2cArgs<'a>> {
    let p = hubris.lookup_enum(goff).context("expected port to be an enum")?;

    if p.size != 1 {
        bail!("expected port to be a 1-byte enum");
    }

    let val = p
        .variants
        .iter()
        .filter(|v| v.name == device.port)
        .map(|v| v.tag.unwrap() as u8)
        .next()
        .unwrap();

    Ok(HiffyI2cArgs {
        controller: device.controller,
        port: val,
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
    })
}

///
/// A routine to help commands parse I2C-related command-line arguments.
///
fn hiffy_i2c_args<'a>(
    hubris: &'a HubrisArchive,
    goff: HubrisGoff,
    bus: &Option<String>,
    controller: Option<u8>,
    port: &Option<String>,
    mux: &Option<String>,
    device: &Option<String>,
) -> Result<HiffyI2cArgs<'a>> {
    let p = hubris.lookup_enum(goff).context("expected port to be an enum")?;

    if p.size != 1 {
        bail!("expected port to be a 1-byte enum");
    }

    //
    // A private function to look a port string up, given the enum.
    //
    fn lookup_port(port: &str, p: &HubrisEnum) -> Result<u8> {
        for variant in &p.variants {
            if variant.name.eq_ignore_ascii_case(port) {
                return Ok(u8::try_from(variant.tag.unwrap())?);
            }
        }

        let mut vals: Vec<String> = vec![];

        for variant in &p.variants {
            vals.push(variant.name.to_string());
        }

        bail!(
            "invalid port \"{}\" (must be one of: {})",
            port,
            vals.join(", ")
        );
    }

    //
    // A wrapper for ports found internally (i.e., in device configuration)
    // for which we expect the lookup_port() to succeed (tht is, failure would
    // denote a port in the I2C configuration that isn't in the enum).
    //
    fn translate_port(port: &str, p: &HubrisEnum) -> u8 {
        match lookup_port(port, p) {
            Ok(result) => result,
            Err(err) => {
                panic!("misconfig: port {} is not in enum: {}", port, err);
            }
        }
    }

    //
    // A helper function to translate a found device into something we can
    // return, checking that we have found exactly one device that matches
    // the criteria.
    //
    fn translate_device<'a, T>(
        device: &str,
        p: &HubrisEnum,
        mut found: T,
    ) -> Result<HiffyI2cArgs<'a>>
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

                Ok(HiffyI2cArgs {
                    controller: d.controller,
                    port: translate_port(&d.port, p),
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

        let found = hubris
            .manifest
            .i2c_buses
            .iter()
            .find(|&b| b.name == Some(bus.to_string()));

        match found {
            None => {
                bail!("bus {} not found", bus);
            }
            Some(bus) => (bus.controller, translate_port(&bus.port, p)),
        }
    } else {
        match (controller, port) {
            (None, Some(_)) => {
                bail!("cannot specify a port alone; need a controller or bus");
            }
            (None, None) => {
                //
                // If we haven't been given a controller or a bus, but we've been
                // given a device, see if we can find exactly one of
                // those devices.
                //
                if let Some(device) = device {
                    let found = hubris
                        .manifest
                        .i2c_devices
                        .iter()
                        .filter(|d| d.device == *device);

                    return translate_device(device, p, found);
                } else {
                    bail!("must specify either a controller or a bus");
                }
            }
            (Some(controller), Some(port)) => {
                (controller, lookup_port(&port, p)?)
            }
            (Some(controller), None) => {
                //
                // We have been given a controller but no port; if there is
                // exactly one bus for this controller, we will use its port,
                // otherwise we will bail.
                //
                let mut found = hubris
                    .manifest
                    .i2c_buses
                    .iter()
                    .filter(|bus| bus.controller == controller)
                    .map(|bus| bus.port.clone());

                let all: Vec<_> = found.clone().collect();

                match found.next() {
                    None => {
                        bail!("unknown I2C controller {}", controller);
                    }
                    Some(port) => {
                        if let Some(_) = found.next() {
                            bail!(
                                "I2C{} has multiple ports; expected one of: {}",
                                controller,
                                all.join(", ")
                            )
                        }

                        (controller, translate_port(&port, p))
                    }
                }
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
                    .filter(|d| translate_port(&d.port, p) == port);

                return translate_device(device, p, found);
            }
        }
    };

    Ok(HiffyI2cArgs {
        controller: controller,
        port: port,
        mux: mux,
        device: device,
        address: address,
        class: &HubrisI2cDeviceClass::Unknown,
    })
}
