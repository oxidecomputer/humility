/*
 * Copyright 2021 Oxide Computer Company
 */

use crate::cmd::{Archive, Attach, Validate};
use crate::core::Core;
use crate::hiffy::*;
use crate::hubris::*;
use crate::Args;
use std::convert::TryFrom;
use std::thread;

use itertools::Itertools;

use anyhow::{anyhow, bail, Context, Result};
use hif::*;
use std::collections::HashMap;
use std::time::Duration;
use structopt::clap::App;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(name = "rencm", about = "query Renesas 8A3400X ClockMatrix parts")]
struct RencmArgs {
    /// sets timeout
    #[structopt(
        long, short, default_value = "5000", value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// specifies an I2C controller
    #[structopt(long, short, value_name = "controller",
        parse(try_from_str = parse_int::parse),
    )]
    controller: u8,

    /// specifies an I2C controller port
    #[structopt(long, short, value_name = "port")]
    port: Option<String>,

    /// specifies I2C multiplexer and segment
    #[structopt(long, short, value_name = "mux:segment")]
    mux: Option<String>,

    /// scans all registers
    #[structopt(long, short, conflicts_with_all = &[ "register", "module" ])]
    scan: bool,

    /// specifies register(s) to read
    #[structopt(
        long,
        short,
        value_name = "register",
        conflicts_with = "module"
    )]
    register: Option<Vec<String>>,

    /// specifies module(s) to read
    #[structopt(long, short = "M", value_name = "module")]
    module: Option<Vec<String>>,

    /// specifies an I2C device address
    #[structopt(long, short = "d", value_name = "address",
        parse(try_from_str = parse_int::parse),
    )]
    device: u8,
}

fn rencm(
    hubris: &mut HubrisArchive,
    core: &mut dyn Core,
    _args: &Args,
    subargs: &Vec<String>,
) -> Result<()> {
    use idt8a3xxxx::*;

    let subargs = RencmArgs::from_iter_safe(subargs)?;

    let modules = modules();
    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;
    let funcs = context.functions()?;
    let read_func = funcs
        .get("I2cRead")
        .ok_or_else(|| anyhow!("did not find I2cRead function"))?;

    if read_func.args.len() != 7 {
        bail!("mismatched function signature on I2cRead");
    }

    let write_func = funcs
        .get("I2cWrite")
        .ok_or_else(|| anyhow!("did not find I2cWrite function"))?;

    if write_func.args.len() != 8 {
        bail!("mismatched function signature on I2cWrite");
    }

    let mut port = None;

    if let Some(ref portarg) = subargs.port {
        let p = hubris
            .lookup_enum(read_func.args[1])
            .context("expected port to be an enum")?;

        if p.size != 1 {
            bail!("expected port to be a 1-byte enum");
        }

        for variant in &p.variants {
            if variant.name.eq_ignore_ascii_case(portarg) {
                port = Some(u8::try_from(variant.tag.unwrap())?);
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

    let mux = if let Some(mux) = &subargs.mux {
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

    let mut modnames = HashMap::new();
    let mut regmods = HashMap::new();

    //
    // Spin through our modules
    //
    for module in modules {
        if module.base.len() > 1 {
            for i in 0..module.base.len() {
                modnames.insert(
                    format!("{}_{}", module.name, i),
                    (module, Some(i)),
                );
            }
        }

        modnames.insert(module.name.to_string(), (module, None));

        for register in module.registers {
            match regmods.insert(register.name, (module, None)) {
                None => {}
                Some(_) => {
                    bail!("duplicate register {}!", register.name);
                }
            }
        }
    }

    let mut work = vec![];

    if let Some(registers) = &subargs.register {
        for register in registers {
            let val = register.split('=').collect::<Vec<&str>>();

            let write = if val.len() > 1 {
                if val.len() != 2 {
                    bail!("illegal register set value");
                }

                if let Ok(val) = parse_int::parse::<u64>(val[1]) {
                    Some(val)
                } else {
                    bail!("invalid register value {}", val[1]);
                }
            } else {
                None
            };

            let s = val[0].split('.').collect::<Vec<&str>>();

            let (module, register) = if s.len() == 2 {
                //
                // We have been provided a module explicitly.  Verify that
                // it's valid.
                //
                match modnames.get(s[0]) {
                    None => {
                        bail!("{} is not a valid module name", s[0]);
                    }
                    Some(module) => (module, s[1]),
                }
            } else if s.len() == 1 {
                //
                // We have just been provdided a register; find the module
                // that contains it.
                //
                match regmods.get(s[0]) {
                    None => {
                        bail!("{} is not a known register", s[0]);
                    }
                    Some(module) => (module, s[0]),
                }
            } else {
                bail!("expected module.register");
            };

            //
            // Verify that our module contains our register, and then
            // enqueue the work to do.
            //
            match module.0.registers.iter().find(|r| r.name == register) {
                Some(r) => match module.1 {
                    Some(ndx) => {
                        work.push((module.0, r, ndx, write));
                    }
                    None => {
                        for ndx in 0..module.0.base.len() {
                            work.push((module.0, r, ndx, write));
                        }
                    }
                },
                None => {
                    let registers = module.0.registers;

                    bail!(
                        "register {} is not found in {}; expected one of: {}",
                        register,
                        module.0.name,
                        registers.iter().map(|r| r.name).join(", ")
                    );
                }
            }
        }
    }

    if let Some(modules) = &subargs.module {
        for module in modules {
            match modnames.get(module) {
                None => {
                    bail!("{} is not a valid module name", module);
                }
                Some(module) => match module.1 {
                    Some(ndx) => {
                        for r in module.0.registers {
                            work.push((module.0, r, ndx, None));
                        }
                    }
                    None => {
                        for ndx in 0..module.0.base.len() {
                            for r in module.0.registers {
                                work.push((module.0, r, ndx, None));
                            }
                        }
                    }
                },
            }
        }
    }

    if subargs.scan {
        for module in modules {
            for ndx in 0..module.base.len() {
                for r in module.registers {
                    work.push((module, r, ndx, None));
                }
            }
        }
    }

    info!("{} registers found", work.len());

    let mut ndx = 0;
    let maxops = 1000;

    if work.len() == 0 {
        bail!("must specify -s, -M, or -r");
    }

    let jobname = |job: &(&Module, &Register, usize, Option<u64>)| {
        if job.0.base.len() == 1 {
            format!("{}.{}", job.0.name, job.1.name)
        } else {
            format!("{}_{}.{}", job.0.name, job.2, job.1.name)
        }
    };

    let jobaddr = |job: &(&Module, &Register, usize, Option<u64>)| {
        job.0.base[job.2] + job.1.offset
    };

    loop {
        let mut ops = vec![];
        let mut current = None;
        let mut calls = vec![];

        ops.push(Op::Push(subargs.controller));

        if let Some(port) = port {
            ops.push(Op::Push(port));
        } else {
            ops.push(Op::PushNone);
        }

        if let Some(mux) = mux {
            ops.push(Op::Push(mux.0));
            ops.push(Op::Push(mux.1));
        } else {
            ops.push(Op::PushNone);
            ops.push(Op::PushNone);
        }

        ops.push(Op::Push(subargs.device));

        while ndx < work.len() && ops.len() < maxops {
            let job = work[ndx];
            let addr = job.0.base[job.2] + job.1.offset;
            let page = idt8a3xxxx::page(addr);

            match current {
                Some(current) if current == page => {}
                _ => {
                    //
                    // If our page doesn't match our current page (or if we
                    // don't have a current page), we need to write our page
                    // address.
                    //
                    ops.push(Op::Push(idt8a3xxxx::PAGE_ADDR));
                    ops.push(Op::Push(page));
                    ops.push(Op::Push(1));
                    ops.push(Op::Call(write_func.id));
                    ops.push(Op::DropN(3));
                    current = Some(page);
                    calls.push(None);
                }
            }

            ops.push(Op::Push(idt8a3xxxx::offset(addr)));

            if let Some(write) = job.3 {
                let mut buf = vec![0; 16];
                let size = job.1.contents.size();

                let payload = match Payload::into_slice(
                    job.1.contents,
                    write,
                    buf.as_mut_slice(),
                ) {
                    Some(payload) => payload,
                    None => {
                        let name = jobname(&job);
                        bail!("value {} exceeds size for {}", write, name);
                    }
                };

                for i in 0..payload.data.len() {
                    ops.push(Op::Push(payload.data[i]));
                }

                ops.push(Op::Push(size));
                ops.push(Op::Call(write_func.id));
                ops.push(Op::DropN(2 + size));
            } else {
                ops.push(Op::Push(job.1.contents.size()));
                ops.push(Op::Call(read_func.id));
                ops.push(Op::DropN(2));
            }

            calls.push(Some(ndx));
            ndx += 1;
        }

        //
        // Now we have our work to do! Kick it off.
        //
        ops.push(Op::Done);

        context.execute(core, ops.as_slice(), None)?;

        loop {
            if context.done(core)? {
                break;
            }

            thread::sleep(Duration::from_millis(100));
        }

        let results = context.results(core)?;

        if results.len() != calls.len() {
            bail!(
                "short results; expected {}, found {}: {:?}",
                calls.len(),
                results.len(),
                results
            );
        }

        for rndx in 0..results.len() {
            match &results[rndx] {
                Ok(r) => {
                    if r.is_empty() {
                        if let Some(ndx) = calls[rndx] {
                            let job = work[ndx];

                            if let Some(write) = job.3 {
                                info!(
                                    "successfully wrote {} to {} at 0x{:x}",
                                    write,
                                    jobname(&job),
                                    jobaddr(&job),
                                );
                            } else {
                                bail!(
                                    "missing result at {}: {:?}",
                                    rndx,
                                    results
                                );
                            }
                        }

                        continue;
                    }

                    let job = match calls[rndx] {
                        Some(ndx) => work[ndx],
                        None => {
                            bail!("spurious result at {}: {:?}", rndx, results);
                        }
                    };

                    let payload = match idt8a3xxxx::Payload::from_slice(
                        job.1.contents,
                        r.as_slice(),
                    ) {
                        Some(payload) => payload,
                        None => {
                            bail!("short read for {:?} at {:?}", job, results)
                        }
                    };

                    let value = payload.value();

                    println!(
                        "0x{:04x} {:50} = 0x{:x} ({})",
                        jobaddr(&job),
                        jobname(&job),
                        value,
                        value
                    );
                }

                Err(code) => {
                    if let Some(ndx) = calls[rndx] {
                        let job = work[ndx];

                        let err = match job.3 {
                            None => ("read", read_func.strerror(*code)),
                            Some(_) => ("write", write_func.strerror(*code)),
                        };

                        bail!(
                            "failed to {} {} at 0x{:x}: {}: {:?}",
                            err.0,
                            jobname(&job),
                            jobaddr(&job),
                            err.1,
                            results
                        );
                    } else {
                        bail!(
                            "failed to page write: {}: {:?}",
                            write_func.strerror(*code),
                            results,
                        );
                    }
                }
            }
        }

        if ndx == work.len() {
            break;
        }
    }

    Ok(())
}

pub fn init<'a, 'b>() -> (crate::cmd::Command, App<'a, 'b>) {
    (
        crate::cmd::Command::Attached {
            name: "rencm",
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
            run: rencm,
        },
        RencmArgs::clap(),
    )
}
