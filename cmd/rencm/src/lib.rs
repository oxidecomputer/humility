// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility rencm`
//!
//! Query the Renesas 8A3400X ClockMatrix part -- or process a trace from
//! Renesas configuration software.
//!

use humility::cli::Subcommand;
use humility::core::Core;
use humility::hubris::*;
use humility_cmd::i2c::I2cArgs;
use humility_cmd::{attach, Archive, Attach, Command, Dumper, Validate};
use humility_cmd::{hiffy::*, CommandKind};

use itertools::Itertools;

use anyhow::{bail, Result};
use clap::{CommandFactory, Parser};
use hif::*;
use idt8a3xxxx::*;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::fs;

use serde::Deserialize;
use serde_xml_rs::from_str;

#[derive(Parser, Debug)]
#[clap(name = "rencm", about = env!("CARGO_PKG_DESCRIPTION"))]
struct RencmArgs {
    /// sets timeout
    #[clap(
        long, short, default_value_t = 5000, value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// verbose output
    #[clap(long, short)]
    verbose: bool,

    /// specifies an I2C bus by name
    #[clap(long, short, value_name = "bus",
        conflicts_with_all = &["port", "controller"]
    )]
    bus: Option<String>,

    /// specifies an I2C controller
    #[clap(long, short, value_name = "controller",
        parse(try_from_str = parse_int::parse),
    )]
    controller: Option<u8>,

    /// specifies an I2C controller port
    #[clap(long, short, value_name = "port")]
    port: Option<String>,

    /// specifies I2C multiplexer and segment
    #[clap(long, short, value_name = "mux:segment")]
    mux: Option<String>,

    /// scans all registers
    #[clap(long, short, conflicts_with_all = &[ "register", "module" ])]
    scan: bool,

    /// specifies register(s) to read
    #[clap(long, short, value_name = "register", conflicts_with = "module")]
    register: Option<Vec<String>>,

    /// specifies module(s) to read
    #[clap(long, short = 'M', value_name = "module")]
    module: Option<Vec<String>>,

    /// specifies an I2C device address
    #[clap(long, short = 'd', value_name = "address")]
    device: Option<String>,

    /// ingest an Aardvark data file or Saleae trace
    #[clap(
        long,
        short = 'i',
        value_name = "filename",
        conflicts_with_all = &[
            "register", "module", "scan", "bus",
        ]
    )]
    ingest: Option<String>,

    /// Generage a Rust payload definition to the specified file
    #[clap(long, short, requires = "ingest")]
    generate: bool,
}

fn rencm_attached(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    subargs: &RencmArgs,
    modules: &[Module],
) -> Result<()> {
    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;
    let funcs = context.functions()?;
    let read_func = funcs.get("I2cRead", 7)?;
    let write_func = funcs.get("I2cWrite", 8)?;

    let hargs = I2cArgs::parse(
        hubris,
        &subargs.bus,
        subargs.controller,
        &subargs.port,
        &subargs.mux,
        &subargs.device,
    )?;

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

    humility::msg!("{} registers found", work.len());

    let mut ndx = 0;
    let maxops = 1000;

    if work.is_empty() {
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

        ops.push(Op::Push(hargs.controller));

        ops.push(Op::Push(hargs.port.index));

        if let Some(mux) = hargs.mux {
            ops.push(Op::Push(mux.0));
            ops.push(Op::Push(mux.1));
        } else {
            ops.push(Op::PushNone);
            ops.push(Op::PushNone);
        }

        if let Some(address) = hargs.address {
            ops.push(Op::Push(address));
        } else {
            bail!("expected device");
        }

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
                    ops.push(Op::Push(idt8a3xxxx::PAGE_ADDR_15_8));
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

        let results = context.run(core, ops.as_slice(), None)?;

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
                                humility::msg!(
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

fn rencm_dump(
    input: Input,
    subargs: &RencmArgs,
    ops: &[Vec<u8>],
    modules: &[Module],
) -> Result<()> {
    let mut base = 0u16;
    let mut byaddr = BTreeMap::new();

    if subargs.generate {
        #[rustfmt::skip]
        println!(
r##"// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

///
/// Iterate over a configuration payload for a Renesas 8A3XXXX clock
/// generator.  This code was generated by "humility rencm -g" given
/// {} running Renesas configuration software.
///
#[rustfmt::skip]
pub fn idt8a3xxxx_payload<E>(
    mut func: impl FnMut(&[u8]) -> Result<(), E>
) -> Result<(), E> {{

    const PAYLOAD: &[&[u8]] = &["##,
            match input {
                Input::Aardvark => "Aardvark output generated by",
                Input::Saleae => "a Salea trace gathered while",
            }
        );
    }

    for module in modules {
        for (ndx, addr) in module.base.iter().enumerate() {
            for register in module.registers.iter() {
                byaddr.insert(addr + register.offset, (module, ndx, register));
            }
        }
    }

    let mut dumper = Dumper::new();
    dumper.header = false;
    dumper.indent = 5;
    dumper.hanging = true;
    dumper.addrsize = 2;
    dumper.ascii = false;

    let preprocess = |windex, bytes| {
        if !subargs.generate {
            print!("{:>3}: ", windex);
            dumper.dump(bytes, 0);
        }
    };

    let postprocess = |bytes: &[u8], unknown: bool, addr: u16| {
        if subargs.generate {
            if unknown {
                println!("        // Unknown write to 0x{:04x}", addr);
            }

            print!("        &[");

            for (ndx, val) in bytes.iter().enumerate() {
                if ndx % 8 == 0 {
                    print!("\n            ");
                } else {
                    print!(" ");
                }

                print!("0x{:02x},", val);
            }

            println!("\n        ],\n");
        } else {
            println!();
        }
    };

    let print_attr = |attr, val| {
        if subargs.verbose {
            println!("{:5}{:12}: {}", "", attr, val);
        }
    };

    let print_value = |attr, val| {
        if !subargs.generate {
            println!("{:5}{} = {}", "", attr, val);
        } else {
            println!("        // {} = {}", attr, val);
        }
    };

    for (windex, bytes) in ops.iter().enumerate() {
        let mut boffs = 0_usize;

        preprocess(windex, bytes);

        if bytes[0] == PAGE_ADDR_7_0 {
            //
            // We expect at least two bytes of write here.
            //
            if bytes.len() < 3 {
                bail!("{}: short write to page adress: {:?}", windex, bytes);
            }

            base = (bytes[2] as u16) << 8;
            print_value("PAGE_ADDR".to_string(), format!("0x{:04x}", base));

            //
            // We might have more data here...
            //
            boffs = 5;

            if boffs >= bytes.len() {
                postprocess(bytes, false, base);
                continue;
            }
        }

        //
        // Our first byte contains our offset within the page
        //
        let addr = base + bytes[boffs] as u16;
        boffs += 1;

        print_attr("base", format!("0x{:04x}", base));
        print_attr("addr", format!("0x{:04x}", addr));

        let mut current = addr;
        let mut unknown = true;

        loop {
            if let Some((_, (module, ndx, register))) =
                byaddr.range(..=current).next_back()
            {
                let offset = current - module.base[*ndx];

                let which = if module.base.len() > 1 {
                    format!("[{}]", ndx)
                } else {
                    "".to_string()
                };

                print_attr("module", format!("{}{}", module.name, which));
                print_attr("offset", format!("0x{:x}", offset));
                print_attr("boffs", format!("0x{:x}", boffs));
                print_attr("len", format!("0x{:x}", bytes.len()));

                let size = register.contents.size() as u16;

                if offset - register.offset >= size {
                    print_attr("register", "<Unknown>".to_string());
                    print_attr("contents", format!("{:x?}", &bytes[boffs..]));
                    break;
                } else {
                    let roffset = offset - register.offset;
                    print_attr("size", format!("0x{:x}", size));
                    print_attr("roffset", format!("0x{:x}", roffset));

                    let size = (size - (offset - register.offset)) as usize;

                    let r = if roffset != 0 {
                        format!(
                            "{}{}.{}+0x{:x}",
                            module.name, which, register.name, roffset
                        )
                    } else {
                        format!("{}{}.{}", module.name, which, register.name)
                    };

                    let lim = if boffs + size < bytes.len() {
                        boffs + size
                    } else {
                        bytes.len()
                    };

                    let val = format!("{:x?}", &bytes[boffs..lim]);
                    print_value(r, val);

                    unknown = false;
                    boffs += size;
                    current += size as u16;
                }
            }

            if boffs >= bytes.len() {
                break;
            }
        }

        postprocess(bytes, unknown, addr);
    }

    if subargs.generate {
        println!(
            r##"    ];

    for chunk in PAYLOAD {{
        func(chunk)?;
    }}

    Ok(())
}}"##
        );
    }

    Ok(())
}

enum Input {
    Aardvark,
    Saleae,
}

#[derive(Debug, Deserialize, PartialEq)]
#[serde(rename_all = "kebab-case")]
struct Aardvark {
    #[serde(rename = "i2c_write")]
    writes: Vec<AardvarkWrite>,
}

#[derive(Debug, Deserialize, PartialEq)]
#[serde(rename_all = "kebab-case")]
struct AardvarkWrite {
    #[serde(rename = "$value")]
    value: String,
}

fn rencm_aardvark(subargs: &RencmArgs, modules: &[Module]) -> Result<()> {
    let contents =
        String::from_utf8(fs::read(subargs.ingest.as_ref().unwrap())?)?;
    let mut ops = vec![];

    let payload = match contents.find("<aardvark>") {
        Some(offset) => &contents[offset..],
        None => {
            bail!("input is not an Aardvark file");
        }
    };

    let aardvark: Aardvark = from_str(payload)?;

    for (windex, w) in aardvark.writes.iter().enumerate() {
        let mut bytes = vec![];

        for v in w.value.split(' ') {
            if let Ok(val) = u8::from_str_radix(v, 16) {
                bytes.push(val);
            } else {
                bail!("{}: bad write line: {:?}", windex, w);
            }
        }

        ops.push(bytes)
    }

    rencm_dump(Input::Aardvark, subargs, &ops, modules)?;

    Ok(())
}

#[derive(Debug, Deserialize)]
#[allow(dead_code)]
struct SaleaeTraceRecord {
    #[serde(rename = "Time [s]")]
    time: f64,

    #[serde(rename = "Packet ID")]
    id: u8,

    #[serde(rename = "Address")]
    address: String,

    #[serde(rename = "Data")]
    data: String,

    #[serde(rename = "Read/Write")]
    rw: String,

    #[serde(rename = "ACK/NAK")]
    acknak: String,
}

fn rencm_ingest(subargs: &RencmArgs, modules: &[Module]) -> Result<()> {
    let file = fs::File::open(subargs.ingest.as_ref().unwrap())?;

    let mut rdr = csv::Reader::from_reader(file);

    match rdr.headers() {
        Ok(hdr) if hdr.len() == 6 => {
            let mut register = None;

            let mut ops = vec![];
            let mut bytes = vec![];

            for result in rdr.deserialize() {
                let record: SaleaeTraceRecord = result?;

                if record.rw == "Read" {
                    if !bytes.is_empty() {
                        ops.push(bytes);
                        bytes = vec![];
                    }

                    register = None;
                }

                if record.rw == "Write" {
                    let datum = match parse_int::parse::<u8>(&record.data) {
                        Ok(val) => val,
                        Err(_) => {
                            bail!("invalid record {:?}", record)
                        }
                    };

                    if let Some(val) = register {
                        bytes.push(val);
                    }

                    register = Some(datum);
                }
            }

            rencm_dump(Input::Saleae, subargs, &ops, modules)?;
        }
        _ => {
            humility::msg!("not a Saleae trace file; assuming Aardvark input");
            rencm_aardvark(subargs, modules)?;
        }
    }

    Ok(())
}

fn rencm(context: &mut humility::ExecutionContext) -> Result<()> {
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let subargs = RencmArgs::try_parse_from(subargs)?;
    let modules = modules();

    if subargs.ingest.is_some() {
        return rencm_ingest(&subargs, modules);
    }

    attach(context, Attach::LiveOnly, Validate::Booted, |context| {
        let core = context.core.as_mut().unwrap();
        let hubris = context.archive.as_ref().unwrap();
        rencm_attached(hubris, &mut **core, &subargs, modules)
    })
}

pub fn init() -> Command {
    Command {
        app: RencmArgs::command(),
        name: "rencm",
        run: rencm,
        kind: CommandKind::Unattached { archive: Archive::Optional },
    }
}
