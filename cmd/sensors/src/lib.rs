// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility sensors`
//!
//! `humility sensors` communicates with the `sensor` Hubris task via its
//! `Sensor` Idol interface to get sensor data.  If there is no `sensor` task
//! or if there are no sensors defined in the in Hubris application
//! description, this command will not provide any meaningful output. To list
//! all available sensors, use `-l` (`--list`).  To constrain sensors by type,
//! use the `-t` (`--types`) option; to constrain sensors by device, use the
//! `-d` (`--devices`) option; to constrain sensors by name, use the `-n`
//! (`--named`) option.  Within each option, multiple specifications serve as
//! a logical OR (that is, (`-d raa229618,tmp117` would yield all sensors from
//! either device), but if multiple kinds of specifications are present, they
//! serve as a logical AND (e.g., `-t thermal -d raa229618,tmp117` would yield
//! all thermal sensors from either device).  Alternatively, sensors can be
//! listed or queried by specifying the ID (or IDs) via `-i` (`--id`).
//!
//! By default, `humility sensors` displays the value of each specified sensor
//! and exits; to read values once per second, use the `-s` (`--sleep`)
//! option.  To print values as a table with individual sensors as columns,
//! `--tabular`.  In its default output (with one sensor per row), error
//! counts are also displayed.

use anyhow::{anyhow, bail, Result};
use clap::{CommandFactory, Parser};
use hif::*;
use humility::core::Core;
use humility::hubris::*;
use humility_cli::{ExecutionContext, Subcommand};
use humility_cmd::{Archive, Attach, Command, CommandKind, Validate};
use humility_hiffy::*;
use humility_idol::{self as idol, HubrisIdol};
use itertools::izip;
use std::collections::HashSet;
use std::thread;
use std::time::{Duration, Instant};

#[derive(Parser, Debug)]
#[clap(name = "sensors", about = env!("CARGO_PKG_DESCRIPTION"))]
struct SensorsArgs {
    /// sets timeout
    #[clap(
        long, short = 'T', default_value_t = 5000, value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// list all sensors
    #[clap(long, short)]
    list: bool,

    /// use the given backend to read sensor data
    ///
    /// [default: `hiffy` if present, otherwise `readmem`]
    #[clap(long, short, value_enum)]
    backend: Option<Backend>,

    /// print sensors every second
    #[clap(long, short, conflicts_with = "list")]
    sleep: bool,

    /// print results as a table
    #[clap(long, conflicts_with = "list")]
    tabular: bool,

    /// restrict sensors by type of sensor
    #[clap(
        long,
        short,
        value_name = "sensor type",
        use_value_delimiter = true
    )]
    types: Option<Vec<String>>,

    /// restrict sensors by device
    #[clap(long, short, value_name = "device", use_value_delimiter = true)]
    devices: Option<Vec<String>>,

    /// restrict sensors by name
    #[clap(
        long,
        short,
        value_name = "sensor name",
        use_value_delimiter = true
    )]
    named: Option<Vec<String>>,

    /// indicate sensors by ID
    #[clap(
        long, short, value_name = "id", use_value_delimiter = true,
        parse(try_from_str = parse_int::parse),
        conflicts_with_all = &["types", "devices", "named"],
    )]
    id: Option<Vec<usize>>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, clap::ValueEnum)]
#[clap(rename_all = "kebab-case")]
enum Backend {
    /// Use the Hiffy task to read sensor data
    Hiffy,
    /// Use the RAM-reading primitive to read sensor data
    Readmem,
}

enum SensorSpecification<'a> {
    Params {
        types: &'a Option<HashSet<HubrisSensorKind>>,
        devices: &'a Option<HashSet<&'a String>>,
        named: &'a Option<HashSet<&'a String>>,
    },
    Id(HashSet<usize>),
}

impl SensorSpecification<'_> {
    fn matches(
        &self,
        hubris: &HubrisArchive,
        s: &HubrisSensor,
        ndx: usize,
    ) -> bool {
        match self {
            SensorSpecification::Params { types, devices, named } => {
                if let Some(types) = types {
                    if !types.contains(&s.kind) {
                        return false;
                    }
                }
                if let Some(devices) = devices {
                    if let HubrisSensorDevice::I2c(device) = s.device {
                        let device = &hubris.manifest.i2c_devices[device];
                        if !devices.contains(&device.device) {
                            return false;
                        }
                    }
                }
                if let Some(named) = named {
                    if !named.contains(&s.name) {
                        return false;
                    }
                }
                true
            }
            SensorSpecification::Id(ids) => ids.contains(&ndx),
        }
    }
}

fn list(hubris: &HubrisArchive, spec: &SensorSpecification) -> Result<()> {
    println!(
        "{:3} {:5} {:<13} {:>2} {:>2} {:3} {:4} {:13} {:4}",
        "ID", "HEXID", "KIND", "C", "P", "MUX", "ADDR", "DEVICE", "NAME"
    );

    for (ndx, s) in hubris.manifest.sensors.iter().enumerate() {
        if !spec.matches(hubris, s, ndx) {
            continue;
        }

        match &s.device {
            HubrisSensorDevice::I2c(device) => {
                let device = &hubris.manifest.i2c_devices[*device];

                let mux = match (device.mux, device.segment) {
                    (Some(m), Some(s)) => format!("{}:{}", m, s),
                    (None, None) => "-".to_string(),
                    (_, _) => "?:?".to_string(),
                };

                println!(
                    "{:3} {:#5x} {:13} {:>2} {:>2} {:>3} {:#04x} {:13} {:<1}",
                    ndx,
                    ndx,
                    s.kind.to_string(),
                    device.controller,
                    device.port.name,
                    mux,
                    device.address,
                    device.device,
                    s.name,
                );
            }
            HubrisSensorDevice::Other(device, _) => {
                println!(
                    "{:3} {:#5x} {:7} {:>2} {:>2} {:>3} {:>4} {:13} {:<1}",
                    ndx,
                    ndx,
                    s.kind.to_string(),
                    "-",
                    "-",
                    "-",
                    "-",
                    device,
                    s.name,
                );
            }
        }
    }

    Ok(())
}

struct SensorData {
    /// Last reading value (from `Sensor.get`), or `None` if it was an error
    values: Vec<Option<f32>>,

    /// Error counters from `Sensor.get_nerrors`
    errors: Vec<Option<u32>>,
}

trait SensorReader {
    /// Reads sensor data, returning a tuple of `values, errors`
    fn run(
        &mut self,
        hubris: &HubrisArchive,
        core: &mut dyn Core,
    ) -> Result<SensorData>;
}

struct HiffySensorReader<'a> {
    context: HiffyContext<'a>,
    all_ops: Vec<Vec<Op>>,
    err_ops: Vec<Vec<Op>>,
}

const NUM_ERR_BITS: usize = 32;

impl<'a> HiffySensorReader<'a> {
    fn new(
        hubris: &HubrisArchive,
        sensors: &[(usize, HubrisSensor)],
        context: HiffyContext<'a>,
    ) -> Result<Self> {
        let op = hubris.get_idol_command("Sensor.get")?;
        let ok = hubris.lookup_basetype(op.ok)?;

        if (ok.encoding, ok.size) != (HubrisEncoding::Float, 4) {
            bail!("expected return value of Sensor.get() to be a float");
        }

        let mut all_ops = vec![];
        let mut err_ops = vec![];

        for s in sensors.chunks(100) {
            let mut ops = vec![];

            for (i, _) in s {
                let payload = op.payload(&[(
                    "id",
                    idol::IdolArgument::Scalar(*i as u64),
                )])?;
                context.idol_call_ops(&op, &payload, &mut ops)?;
            }

            ops.push(Op::Done);
            all_ops.push(ops);
        }

        if let Ok(errop) = hubris.get_idol_command("Sensor.get_nerrors") {
            let ok = hubris.lookup_basetype(errop.ok)?;

            if (ok.encoding, ok.size)
                != (HubrisEncoding::Unsigned, NUM_ERR_BITS / 8)
            {
                bail!("expected return value of get_nerrors() to be a u32");
            }

            for s in sensors.chunks(100) {
                let mut ops = vec![];

                for (i, _) in s {
                    let payload = errop.payload(&[(
                        "id",
                        idol::IdolArgument::Scalar(*i as u64),
                    )])?;
                    context.idol_call_ops(&errop, &payload, &mut ops)?;
                }

                ops.push(Op::Done);
                err_ops.push(ops);
            }
        }
        Ok(Self { context, all_ops, err_ops })
    }
}

impl<'a> SensorReader for HiffySensorReader<'a> {
    fn run(
        &mut self,
        _hubris: &HubrisArchive,
        core: &mut dyn Core,
    ) -> Result<SensorData> {
        let mut rval = vec![];
        let mut errs = vec![];

        for ops in &self.all_ops {
            let results = self.context.run(core, ops.as_slice(), None)?;

            for r in results {
                if let Ok(val) = r {
                    rval.push(Some(f32::from_le_bytes(val[0..4].try_into()?)));
                } else {
                    rval.push(None);
                }
            }
        }

        for ops in &self.err_ops {
            let results = self.context.run(core, ops.as_slice(), None)?;

            for r in results {
                if let Ok(val) = r {
                    errs.push(Some(u32::from_le_bytes(val[0..4].try_into()?)));
                } else {
                    errs.push(None);
                }
            }
        }

        if errs.is_empty() {
            errs = vec![None; rval.len()];
        }

        Ok(SensorData { values: rval, errors: errs })
    }
}

/// Reader for sensor data that pulls it out of RAM directly
struct RamSensorReader {
    /// Position of `LAST_READING` in global RAM
    last_reading: HubrisVariable,
    last_reading_buf: Vec<u8>,

    /// Position of `DATA_VALUE` in global RAM
    data_value: HubrisVariable,
    data_value_buf: Vec<u8>,

    /// Position of `NERRORS` in global RAM
    nerrors: HubrisVariable,
    nerrors_buf: Vec<u8>,

    /// Indexes of sensors that we care about
    ///
    /// We'll read the entire array (for simplicity) then filter just the ones
    /// that matter
    sensors: Vec<usize>,
}

impl RamSensorReader {
    fn new(
        hubris: &HubrisArchive,
        sensors: &[(usize, HubrisSensor)],
    ) -> Result<Self> {
        let find_var = |v| {
            hubris
                .qualified_variables()
                .find(|&(n, _v)| n == format!("task_sensor::main::{v}"))
                .ok_or_else(|| anyhow!("could not find DATA_VALUE"))
                .map(|(_name, v)| *v)
        };
        let last_reading = find_var("LAST_READING")?;
        let data_value = find_var("DATA_VALUE")?;
        let nerrors = find_var("NERRORS")?;

        Ok(Self {
            last_reading,
            last_reading_buf: vec![0u8; last_reading.size],
            data_value,
            data_value_buf: vec![0u8; data_value.size],
            nerrors,
            nerrors_buf: vec![0u8; nerrors.size],
            sensors: sensors.iter().map(|(i, _)| *i).collect(),
        })
    }
}

impl SensorReader for RamSensorReader {
    fn run(
        &mut self,
        hubris: &HubrisArchive,
        core: &mut dyn Core,
    ) -> Result<SensorData> {
        use humility::reflect::{Base, Value};

        core.read_8(self.data_value.addr, &mut self.data_value_buf)?;
        core.read_8(self.last_reading.addr, &mut self.last_reading_buf)?;
        core.read_8(self.nerrors.addr, &mut self.nerrors_buf)?;

        let data_values = humility::reflect::load_value(
            hubris,
            &self.data_value_buf,
            hubris.lookup_type(self.data_value.goff)?,
            0,
        )?;
        let Value::Struct(s) = data_values else {
            bail!("expected DATA_VALUES to be a struct, not {data_values:?}");
        };
        let Value::Array(data_values) =
            s.get("value").ok_or_else(|| anyhow!("missing `value` member"))?
        else {
            bail!("expected DATA_VALUES to contain an array not {s:?}");
        };
        let data_values = self
            .sensors
            .iter()
            .map(|i| {
                let Value::Base(Base::F32(f)) = &data_values[*i] else {
                    bail!("expected an f32");
                };
                Ok(*f)
            })
            .collect::<Result<Vec<f32>>>()?;
        println!("data values:\n{data_values:?}");

        let Value::Array(last_reading) = humility::reflect::load_value(
            hubris,
            &self.last_reading_buf,
            hubris.lookup_type(self.last_reading.goff)?,
            0,
        )?
        else {
            bail!("expected LAST_READING to be an array");
        };
        println!("last reading:\n{last_reading:?}");
        // TODO clear data_values which aren't bla bla bla

        let Value::Array(nerrors) = humility::reflect::load_value(
            hubris,
            &self.nerrors_buf,
            hubris.lookup_type(self.nerrors.goff)?,
            0,
        )?
        else {
            bail!("expected NERRORS to be an array");
        };
        let nerrors = self
            .sensors
            .iter()
            .map(|i| {
                let Value::Base(Base::U32(e)) = &nerrors[*i] else {
                    bail!("expected NERRORS member to be u32");
                };
                Ok(Some(*e))
            })
            .collect::<Result<Vec<Option<u32>>>>()?;
        let nerrors = self.sensors.iter().map(|i| nerrors[*i]).collect();
        println!("nerrors:\n{nerrors:?}");

        Ok(SensorData {
            values: data_values.into_iter().map(Option::Some).collect(), // TODO replace errors
            errors: nerrors,
        })
    }
}

fn print(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    sensors: &[(usize, HubrisSensor)],
    subargs: &SensorsArgs,
    reader: &mut dyn SensorReader,
) -> Result<()> {
    if subargs.tabular {
        for (_, s) in sensors {
            print!(" {:>12}", s.name.to_uppercase());
        }

        println!();

        for (_, s) in sensors {
            print!(" {:>12}", s.kind.to_string().to_uppercase());
        }

        println!();
    }

    loop {
        let start = Instant::now();
        let SensorData { values: rval, errors: errs } =
            reader.run(hubris, core)?;

        if subargs.tabular {
            for val in rval {
                if let Some(val) = val {
                    print!(" {:>12.2}", val);
                } else {
                    print!(" {:>12}", "-");
                }
            }

            println!();
        } else {
            let etypes = ["UNPWR", "ERR", "MSSNG", "UNAVL", "TMOUT"];

            print!("{:20} {:13} {:>13}", "NAME", "KIND", "VALUE");

            for e in etypes {
                print!(" {:>5}", e);
            }

            println!();

            for ((_, s), val, err) in izip!(sensors, &rval, &errs) {
                print!("{:20} {:13} ", s.name, s.kind.to_string());

                if let Some(val) = val {
                    print!(" {:12.2}", val);
                } else {
                    print!(" {:>12}", "-");
                }

                let nfields = etypes.len();

                if let Some(err) = err {
                    //
                    // Okay, this is quick-and-dirty (perhaps with emphasis on
                    // the latter): we are encoding the kinds of `NoData` --
                    // along with the knowledge that all counters are encoded
                    // in the number of bits for error counts.
                    //
                    let nbits = NUM_ERR_BITS / etypes.len();
                    let mask = (1 << nbits) - 1;

                    for i in 0..nfields {
                        let v = (err >> (i * nbits)) & mask;
                        if v < mask {
                            print!(" {:>5}", v);
                        } else {
                            print!(" {:>4}+", mask);
                        }
                    }
                } else {
                    for _ in 0..nfields {
                        print!(" {:>5}", "-");
                    }
                }

                println!();
            }
        }

        if !subargs.sleep {
            break;
        }

        let elapsed = start.elapsed().as_millis() as u64;

        if elapsed < 1000 {
            thread::sleep(Duration::from_millis(1000 - elapsed));
        }
    }

    Ok(())
}

fn sensors(context: &mut ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let hubris = context.archive.as_ref().unwrap();

    let subargs = SensorsArgs::try_parse_from(subargs)?;

    let types = if let Some(ref types) = subargs.types {
        let mut rval = HashSet::new();

        for t in types {
            match HubrisSensorKind::from_string(t) {
                Some(kind) => {
                    rval.insert(kind);
                }
                None => {
                    bail!("unrecognized sensor kind \"{}\"", t);
                }
            }
        }

        Some(rval)
    } else {
        None
    };

    let devices = if let Some(ref devices) = subargs.devices {
        let mut rval = HashSet::new();
        let mut all = HashSet::new();

        for d in hubris.manifest.i2c_devices.iter() {
            all.insert(&d.device);
        }

        for d in devices {
            match all.get(&d) {
                Some(_) => {
                    rval.insert(d);
                }
                None => {
                    bail!("unrecognized device {}", d);
                }
            }
        }

        Some(rval)
    } else {
        None
    };

    let named = if let Some(ref named) = subargs.named {
        let mut all = HashSet::new();
        let mut rval = HashSet::new();

        for s in hubris.manifest.sensors.iter() {
            all.insert(&s.name);
        }

        for d in named {
            match all.get(&d) {
                Some(_) => {
                    rval.insert(d);
                }
                None => {
                    bail!("unrecognized sensor name {}", d);
                }
            }
        }

        Some(rval)
    } else {
        None
    };

    let specification = if let Some(ref id) = subargs.id {
        SensorSpecification::Id(HashSet::from_iter(id.iter().cloned()))
    } else {
        SensorSpecification::Params {
            types: &types,
            devices: &devices,
            named: &named,
        }
    };

    if subargs.list {
        list(hubris, &specification)?;
        return Ok(());
    }

    if core.is_dump() {
        bail!("cannot query sensor data from a dump");
    }

    if hubris.manifest.sensors.is_empty() {
        bail!("no sensors found");
    }

    let mut sensors = vec![];
    for (i, s) in hubris.manifest.sensors.iter().enumerate() {
        if !specification.matches(hubris, s, i) {
            continue;
        }
        sensors.push((i, s.clone()));
    }

    let mut reader: Box<dyn SensorReader> = match subargs.backend {
        Some(Backend::Hiffy) => {
            let context = HiffyContext::new(hubris, core, subargs.timeout)?;
            Box::new(HiffySensorReader::new(hubris, &sensors, context)?)
        }
        Some(Backend::Readmem) => {
            Box::new(RamSensorReader::new(hubris, &sensors)?)
        }
        None => match HiffyContext::new(hubris, core, subargs.timeout) {
            Ok(ctx) => Box::new(HiffySensorReader::new(hubris, &sensors, ctx)?),
            Err(_) => Box::new(RamSensorReader::new(hubris, &sensors)?),
        },
    };
    print(hubris, core, &sensors, &subargs, &mut *reader)?;

    Ok(())
}

pub fn init() -> Command {
    Command {
        app: SensorsArgs::command(),
        name: "sensors",
        run: sensors,
        kind: CommandKind::Attached {
            archive: Archive::Required,
            attach: Attach::Any,
            validate: Validate::Booted,
        },
    }
}
