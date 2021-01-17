/*
 * Copyright 2020 Oxide Computer Company
 */

use crate::attach_live;
use crate::cmd::{Archive, HumilityCommand};
use crate::core::Core;
use crate::hubris::*;
use crate::itm::*;
use crate::Args;
use anyhow::{bail, Context, Result};
use std::cell::RefCell;
use std::convert::TryInto;
use std::thread;
use std::time::Duration;
use std::time::Instant;
use structopt::clap::App;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(name = "i2c", about = "scan for and read I2C devices")]
struct I2cArgs {
    /// sets timeout
    #[structopt(
        long, short, default_value = "5000", value_name = "timeout_ms`",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// enables ITM logging
    #[structopt(long, short)]
    log: bool,

    /// scan a controller for devices or a device for registers
    #[structopt(long, short, conflicts_with = "read")]
    scan: bool,

    /// specifies an I2C bus
    #[structopt(long, short, value_name = "controller",
        parse(try_from_str = parse_int::parse),
    )]
    controller: u8,

    /// specifies an I2C controller port
    #[structopt(long, short, value_name = "port")]
    port: Option<String>,

    /// specifies an I2C device address
    #[structopt(long, short, value_name = "address",
        parse(try_from_str = parse_int::parse),
    )]
    device: Option<u8>,

    /// read a register
    #[structopt(long, short, value_name = "register",
        parse(try_from_str = parse_int::parse),
    )]
    read: Option<u8>,

    /// read as halfwords instead of as bytes
    #[structopt(long, short, conflicts_with = "word")]
    halfword: bool,

    /// read as words instead of as bytes
    #[structopt(long, short)]
    word: bool,
}

#[derive(Debug)]
struct I2cVariables<'a> {
    hubris: &'a HubrisArchive,
    ready: &'a HubrisVariable,
    kick: &'a HubrisVariable,
    controller: &'a HubrisVariable,
    port: &'a HubrisVariable,
    mux: &'a HubrisVariable,
    segment: &'a HubrisVariable,
    device: &'a HubrisVariable,
    register: &'a HubrisVariable,
    requests: &'a HubrisVariable,
    errors: &'a HubrisVariable,
    results: &'a HubrisVariable,
    cached: Option<(u32, u32)>,
    kicked: Option<Instant>,
    timeout: u32,
    timedout: bool,
}

impl<'a> I2cVariables<'a> {
    fn variable(
        hubris: &'a HubrisArchive,
        name: &str,
        wordsize: bool,
    ) -> Result<&'a HubrisVariable> {
        let v = hubris
            .lookup_variable(name)
            .context("expected i2c debugging interface not found")?;

        if wordsize && v.size != 4 {
            bail!("expected {} to be size 4, found {}", name, v.size);
        }

        Ok(v)
    }

    pub fn new(
        hubris: &'a HubrisArchive,
        timeout: u32,
    ) -> Result<I2cVariables> {
        Ok(Self {
            hubris: hubris,
            ready: Self::variable(hubris, "I2C_DEBUG_READY", true)?,
            kick: Self::variable(hubris, "I2C_DEBUG_KICK", true)?,
            controller: Self::variable(hubris, "I2C_DEBUG_CONTROLLER", true)?,
            port: Self::variable(hubris, "I2C_DEBUG_PORT", false)?,
            mux: Self::variable(hubris, "I2C_DEBUG_MUX", true)?,
            segment: Self::variable(hubris, "I2C_DEBUG_SEGMENT", true)?,
            device: Self::variable(hubris, "I2C_DEBUG_DEVICE", true)?,
            register: Self::variable(hubris, "I2C_DEBUG_REGISTER", true)?,
            requests: Self::variable(hubris, "I2C_DEBUG_REQUESTS", true)?,
            errors: Self::variable(hubris, "I2C_DEBUG_ERRORS", true)?,
            results: Self::variable(hubris, "I2C_DEBUG_RESULTS", false)?,
            cached: None,
            kicked: None,
            timeout: timeout,
            timedout: false,
        })
    }

    fn kickit(&mut self, core: &mut dyn Core, subargs: &I2cArgs) -> Result<()> {
        let mut port = None;

        if core.read_word_32(self.ready.addr)? != 1 {
            bail!("i2c debugging facility unavailable");
        }

        if let Some(ref portarg) = subargs.port {
            let p = self
                .hubris
                .lookup_enum(self.port.goff)
                .context("expected port to be an enum")?;

            if p.size != 1 {
                bail!("expected port to be a 1-byte enum");
            }

            for variant in &p.variants {
                if variant.name.eq_ignore_ascii_case(&portarg) {
                    port = variant.tag;
                    break;
                }
            }

            if port.is_none() {
                let mut vals: Vec<String> = vec![];

                for variant in &p.variants {
                    vals.push(variant.name.to_string());
                }

                bail!(
                    "invalid port \"{}\" (must be one of: {})",
                    portarg,
                    vals.join(", ")
                );
            }
        }

        core.halt()?;
        core.write_word_32(self.controller.addr, subargs.controller as u32)?;

        if let Some(device) = subargs.device {
            core.write_word_32(self.device.addr, device as u32)?;
        }

        if let Some(register) = subargs.read {
            core.write_word_32(self.register.addr, register as u32)?;
        }

        if let Some(port) = port {
            core.write_8(self.port.addr, port as u8)?;
        }

        core.write_word_32(self.kick.addr, 1)?;

        self.cached = Some((
            core.read_word_32(self.requests.addr)?,
            core.read_word_32(self.errors.addr)?,
        ));

        self.kicked = Some(Instant::now());

        core.run()?;
        Ok(())
    }

    fn done(&mut self, core: &mut dyn Core) -> Result<bool> {
        core.halt()?;

        let vars = Some((
            core.read_word_32(self.requests.addr)?,
            core.read_word_32(self.errors.addr)?,
        ));

        core.run()?;

        if let Some(kicked) = self.kicked {
            if kicked.elapsed().as_millis() > self.timeout.into() {
                self.timedout = true;

                return Ok(true);
            }
        }

        Ok(self.cached.is_some() && vars != self.cached)
    }

    fn error(&self, core: &mut dyn Core) -> Result<bool> {
        if let Some((_, errors)) = self.cached {
            core.halt()?;
            let rval = core.read_word_32(self.errors.addr)? > errors;
            core.run()?;
            Ok(rval)
        } else {
            Ok(false)
        }
    }
}

fn i2c_results<'a>(
    hubris: &'a HubrisArchive,
    vars: &I2cVariables,
    buf: &[u8],
) -> Result<Vec<Option<Result<u32, &'a HubrisEnumVariant>>>> {
    let variant_enum = |variant: &HubrisEnumVariant| {
        let t = match variant.goff {
            None => bail!("expected tuple"),
            Some(goff) => hubris.lookup_struct(goff)?.lookup_member("__0")?,
        };

        Ok((hubris.lookup_enum(t.goff)?, t.offset))
    };

    let variant_basetype = |variant: &HubrisEnumVariant| {
        let t = match variant.goff {
            None => bail!("expected tuple"),
            Some(goff) => hubris.lookup_struct(goff)?.lookup_member("__0")?,
        };

        Ok((hubris.lookup_basetype(t.goff)?, t.offset))
    };

    let array = hubris
        .lookup_array(vars.results.goff)
        .context("expected results to be an array")?;

    let option = hubris
        .lookup_enum(array.goff)
        .context("expected results to be an array of Option")?;

    let some = option.lookup_variant_byname("Some")?;
    let result = variant_enum(some)?;

    let ok = result.0.lookup_variant_byname("Ok")?;
    let err = result.0.lookup_variant_byname("Err")?;

    let err_payload = variant_enum(err)?;
    let ok_payload = variant_basetype(ok)?;

    let mut results: Vec<Option<Result<u32, &HubrisEnumVariant>>> = vec![];

    for i in 0..array.count {
        let offs = i * option.size;
        let b = &buf[offs..];

        let variant = option.determine_variant(hubris, b)?;

        if variant.goff == some.goff {
            let r =
                result.0.determine_variant(hubris, &buf[offs + result.1..])?;

            if r.goff == err.goff {
                let details = err_payload
                    .0
                    .determine_variant(hubris, &buf[offs + err_payload.1..])?;

                results.push(Some(Err(details)));
            } else {
                let o = offs + ok_payload.1;

                results.push(Some(Ok(u32::from_le_bytes(
                    buf[o..o + 4].try_into()?,
                ))));
            }
        } else {
            results.push(None);
        }
    }

    Ok(results)
}

fn i2c_done(
    core: &mut dyn Core,
    subargs: &I2cArgs,
    hubris: &HubrisArchive,
    vars: &I2cVariables,
) -> Result<()> {
    if vars.error(core)? {
        if subargs.log {
            bail!("i2c command failed on target");
        } else {
            bail!("i2c command failed on target; run with -l for more detail");
        }
    }

    let mut buf: Vec<u8> = vec![];
    buf.resize_with(vars.results.size, Default::default);
    core.read_8(vars.results.addr, buf.as_mut_slice())?;

    let results = i2c_results(hubris, vars, &buf)?;

    if vars.timedout {
        warn!("operation timed out");
    }

    if subargs.scan && subargs.device.is_none() {
        println!("\nDevice scan on controller I2C{}:\n", subargs.controller);

        println!(
            "    R = Reserved   - = No device   \
            \\o/ = Device found   X = Timed out\n"
        );

        print!("{:<8}", "ADDR");

        for i in 0..16 {
            print!(" 0x{:x}", i);
        }

        println!("");

        for i in 0..128 {
            if i % 16 == 0 {
                print!("0x{:02x}    ", i);
            }

            print!(
                "{:>4}",
                match results[i] {
                    None => {
                        "X"
                    }
                    Some(Ok(_)) => {
                        "\\o/"
                    }
                    Some(Err(err)) => {
                        if err.name == "NoDevice" {
                            "-"
                        } else if err.name == "ReservedAddress" {
                            "R"
                        } else {
                            "Err"
                        }
                    }
                }
            );

            if i % 16 == 15 {
                println!("");
            }
        }
    } else if subargs.scan && subargs.device.is_some() {
        println!(
            "\nRegister scan for device 0x{:x} on I2C{}:\n",
            subargs.device.unwrap(),
            subargs.controller
        );

        println!(
            "      - = No register        ! = No device        X = Timed out\n"
        );

        print!("{:<5}", "ADDR");

        for i in 0..16 {
            print!(" 0x{:x}", i);
        }

        println!("");

        for i in 0..256 {
            if i % 16 == 0 {
                print!("0x{:02x} ", i);
            }

            match results[i] {
                Some(Ok(val)) => {
                    print!("  {:02x}", val);
                }
                None => {
                    print!("{:>4}", "X");
                }
                Some(Err(err)) => {
                    print!(
                        "{:>4}",
                        if err.name == "NoRegister" {
                            "-"
                        } else if err.name == "NoDevice" {
                            "!"
                        } else {
                            "Err"
                        }
                    );
                }
            }

            if i % 16 == 15 {
                println!("");
            }
        }
    } else {
        println!(
            "Controller I2C{}, device 0x{:x}, register 0x{:x} = {:x?}",
            subargs.controller,
            subargs.device.unwrap(),
            subargs.read.unwrap(),
            results[0]
        );
    }

    std::process::exit(0);
}

fn i2c_ingest(
    core: &mut dyn Core,
    subargs: &I2cArgs,
    hubris: &HubrisArchive,
    vars: &mut I2cVariables,
    traceid: Option<u8>,
) -> Result<()> {
    let shared = RefCell::new(core);
    let vars = RefCell::new(vars);
    let mut bytes: Vec<u8> = vec![];
    let mut ndx = 0;
    let mut kicked = false;
    let start = Instant::now();
    let mut last = Instant::now();
    let check_ms = 100;

    itm_ingest(
        traceid,
        || {
            while ndx == bytes.len() {
                if ndx == 0 && last.elapsed().as_millis() > check_ms {
                    last = Instant::now();

                    if vars.borrow_mut().done(*shared.borrow_mut())? {
                        i2c_done(
                            *shared.borrow_mut(),
                            subargs,
                            hubris,
                            *vars.borrow(),
                        )?;
                    }
                }
                bytes = shared.borrow_mut().read_swv()?;
                ndx = 0;
            }
            ndx += 1;
            Ok(Some((bytes[ndx - 1], start.elapsed().as_secs_f64())))
        },
        |packet| {
            if packet.header == ITMHeader::Sync {
                if !kicked {
                    vars.borrow_mut().kickit(*shared.borrow_mut(), subargs)?;
                    kicked = true;
                }
            }

            match &packet.payload {
                ITMPayload::Instrumentation { payload, port } => {
                    if *port > 1 {
                        println!("{:x?}", payload);
                        return Ok(());
                    }

                    for p in payload {
                        print!("{}", *p as char);
                    }
                }
                _ => {}
            }

            Ok(())
        },
    )
}

fn i2c(
    hubris: &mut HubrisArchive,
    args: &Args,
    subargs: &Vec<String>,
) -> Result<()> {
    let subargs = I2cArgs::from_iter_safe(subargs)?;
    let mut c = attach_live(args)?;
    let core = c.as_mut();
    hubris.validate(core, HubrisValidate::Booted)?;

    if !subargs.scan && subargs.read.is_none() {
        bail!("must specify either 'scan' or 'read'");
    }

    let mut vars = I2cVariables::new(hubris, subargs.timeout)?;

    if !subargs.log {
        vars.kickit(core, &subargs)?;

        loop {
            thread::sleep(Duration::from_millis(100));
            if vars.done(core)? {
                i2c_done(core, &subargs, hubris, &vars)?;
            }
        }
    }

    let traceid = itm_enable_ingest(core, hubris, 0x0000_000f)?;
    i2c_ingest(core, &subargs, hubris, &mut vars, traceid)?;

    Ok(())
}

pub fn init<'a, 'b>() -> (HumilityCommand, App<'a, 'b>) {
    (
        HumilityCommand { name: "i2c", archive: Archive::Required, run: i2c },
        I2cArgs::clap(),
    )
}
