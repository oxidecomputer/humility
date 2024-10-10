// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use capstone::prelude::*;
use humility_arch_arm::{presyscall_pushes, ARMRegister};
use indexmap::IndexMap;
use serde::{Deserialize, Serialize};
use std::io::prelude::*;

use std::borrow::Cow;
use std::collections::{btree_map, BTreeMap, BTreeSet, HashMap, HashSet};
use std::convert::TryInto;
use std::fmt::{self, Write};
use std::fs::{self, OpenOptions};
use std::io::Cursor;
use std::mem::size_of;
use std::num::TryFromIntError;
use std::path::Path;
use std::str::{self, FromStr};
use std::time::Instant;

use crate::{msg, warn};
use anyhow::{anyhow, bail, ensure, Context, Result};
use capstone::InsnGroupType;
use fallible_iterator::FallibleIterator;
use gimli::UnwindSection;
use goblin::elf::Elf;
use humpty::DumpTask;
use idol::syntax::Interface;
use multimap::MultiMap;
use num_traits::FromPrimitive;
use rustc_demangle::demangle;
use scroll::{IOwrite, Pwrite};
use zerocopy::{AsBytes, FromBytes};

const OXIDE_NT_NAME: &str = "Oxide Computer Company";
const OXIDE_NT_BASE: u32 = 0x1de << 20;
const OXIDE_NT_HUBRIS_ARCHIVE: u32 = OXIDE_NT_BASE + 1;
const OXIDE_NT_HUBRIS_REGISTERS: u32 = OXIDE_NT_BASE + 2;
const OXIDE_NT_HUBRIS_TASK: u32 = OXIDE_NT_BASE + 3;

const MAX_HUBRIS_VERSION: u32 = 9;

#[derive(Default, Debug, Serialize)]
pub struct HubrisManifest {
    pub version: Option<String>,
    pub gitrev: Option<String>,
    pub features: Vec<String>,
    pub board: Option<String>,
    pub image: Option<String>,
    pub name: Option<String>,
    pub target: Option<String>,
    pub task_features: HashMap<String, Vec<String>>,
    pub task_irqs: HashMap<String, Vec<(u32, u32)>>,
    pub task_notifications: HashMap<String, Vec<String>>,
    pub peripherals: BTreeMap<String, u32>,
    pub peripherals_byaddr: BTreeMap<u32, String>,
    pub i2c_devices: Vec<HubrisI2cDevice>,
    pub i2c_buses: Vec<HubrisI2cBus>,
    pub sensors: Vec<HubrisSensor>,
    pub auxflash: Option<HubrisConfigAuxflash>,
}

//
// This structure (and the structures that it refers to) contain everything
// that we might want to pull out of the config TOML -- which will be a subset
// of the entire config.  Unless it is known that the field has always existed
// (like `target` and `board`), the fields should generally be `Option`s to
// allow a new Humility to work on an old Hubris.
//
#[derive(Clone, Debug, Deserialize)]
struct HubrisConfig {
    target: String,
    name: String,
    board: String,
    kernel: HubrisConfigKernel,
    tasks: IndexMap<String, HubrisConfigTask>,
    peripherals: Option<IndexMap<String, HubrisConfigPeripheral>>,
    chip: Option<String>,
    config: Option<HubrisConfigConfig>,
}

#[derive(Clone, Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct HubrisConfigPatches {
    name: String,
    features: IndexMap<String, Vec<String>>,
}

/// Deserialization `struct` for a memory region in the `memory.toml` file
///
/// The file contains other fields associated with each region (e.g. size, name,
/// whether it's read / write / execute); we skip those fields because they
/// aren't used below.
#[derive(Clone, Debug, Deserialize)]
struct HubrisMemoryRegion {
    address: u32,
}

#[derive(Clone, Debug, Deserialize)]
struct HubrisConfigKernel {
    features: Option<Vec<String>>,
}

#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
struct HubrisConfigTask {
    features: Option<Vec<String>>,
    extern_regions: Option<Vec<String>>,
    #[serde(default)]
    notifications: Vec<String>,
    interrupts: Option<IndexMap<String, HubrisTaskInterrupt>>,
}

#[derive(Clone, Debug, Deserialize)]
#[serde(untagged)]
enum HubrisTaskInterrupt {
    Mask(u32),
    Named(String),
}

#[derive(Clone, Debug, Deserialize)]
#[allow(dead_code)]
struct HubrisConfigPeripheral {
    address: u32,
    size: u32,
    interrupts: Option<IndexMap<String, u32>>,
}

#[derive(Clone, Debug, Deserialize)]
struct HubrisConfigI2cPort {
    name: Option<String>,
    description: Option<String>,
}

#[derive(Clone, Debug, Deserialize)]
struct HubrisConfigI2cController {
    controller: u8,
    ports: BTreeMap<String, HubrisConfigI2cPort>,
    target: Option<bool>,
}

///
/// This is a legacy way of expressing PMBus devices that is no longer
/// present in new archives -- but we retain our ability to read it.
///
#[derive(Clone, Debug, Deserialize, Serialize)]
struct HubrisConfigI2cPmbus {
    rails: Option<Vec<String>>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
struct HubrisConfigI2cPower {
    rails: Option<Vec<String>>,
    phases: Option<Vec<Vec<u8>>>,
    #[serde(default = "HubrisConfigI2cPower::default_pmbus")]
    pmbus: bool,

    /// Lists which sensor types have a one-to-one association with power rails
    ///
    /// When `None`, we assume that all sensor types are mapped one-to-one with
    /// rails.  Otherwise, *only* the listed sensor types are associated with
    /// rails (which is the case in systems with independent temperature sensor
    /// and power rails).
    sensors: Option<Vec<HubrisSensorKind>>,
}

impl HubrisConfigI2cPower {
    fn default_pmbus() -> bool {
        true
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
struct HubrisConfigI2cSensors {
    #[serde(default)]
    temperature: usize,

    #[serde(default)]
    power: usize,

    #[serde(default)]
    current: usize,

    #[serde(default)]
    voltage: usize,

    #[serde(default)]
    input_current: usize,

    #[serde(default)]
    input_voltage: usize,

    #[serde(default)]
    speed: usize,

    names: Option<Vec<String>>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
struct HubrisConfigI2cDevice {
    device: String,
    name: Option<String>,
    controller: Option<u8>,
    bus: Option<String>,
    address: u8,
    port: Option<String>,
    mux: Option<u8>,
    segment: Option<u8>,
    description: String,
    pmbus: Option<HubrisConfigI2cPmbus>,
    power: Option<HubrisConfigI2cPower>,
    sensors: Option<HubrisConfigI2cSensors>,
    removable: Option<bool>,
}

#[derive(Clone, Debug, Deserialize)]
struct HubrisConfigI2c {
    controllers: Option<Vec<HubrisConfigI2cController>>,
    devices: Option<Vec<HubrisConfigI2cDevice>>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields, rename_all = "kebab-case")]
pub struct HubrisConfigAuxflash {
    pub memory_size: usize,
    pub slot_count: usize,
}

impl HubrisConfigAuxflash {
    pub fn slot_size_bytes(&self) -> Result<usize> {
        if self.memory_size % self.slot_count != 0 {
            bail!("Cannot evenly divide auxflash into slots");
        }
        Ok(self.memory_size / self.slot_count)
    }
}

#[derive(Clone, Debug, Deserialize)]
struct HubrisConfigConfig {
    i2c: Option<HubrisConfigI2c>,
    sensor: Option<HubrisConfigSensor>,
    auxflash: Option<HubrisConfigAuxflash>,
}

#[derive(Clone, Debug, Deserialize)]
pub struct HubrisConfigSensor {
    devices: Vec<HubrisConfigSensorSensor>,
}

#[derive(Clone, Debug, Deserialize)]
pub struct HubrisConfigSensorSensor {
    name: String,
    device: String,
    sensors: BTreeMap<String, usize>,
}

#[derive(Clone, Debug, Serialize, Eq, PartialEq)]
pub struct HubrisI2cPort {
    pub name: String,
    pub index: u8,
}

#[derive(Clone, Debug, Serialize)]
pub struct HubrisI2cBus {
    pub controller: u8,
    pub port: HubrisI2cPort,
    pub name: Option<String>,
    pub description: Option<String>,
    pub target: bool,
}

#[derive(Clone, Debug, Serialize)]
pub struct HubrisPmbusRail {
    pub name: String,
    pub phases: Option<Vec<u8>>,
}

#[derive(Clone, Debug, Serialize)]
pub enum HubrisI2cDeviceClass {
    Pmbus { rails: Vec<HubrisPmbusRail> },
    Unspecified,
    Unknown,
}

impl HubrisI2cDeviceClass {
    fn from(device: &HubrisConfigI2cDevice) -> Self {
        fn rails_phases(
            rails: &Option<Vec<String>>,
            phases: &Option<Vec<Vec<u8>>>,
        ) -> Vec<HubrisPmbusRail> {
            match (rails, phases) {
                (Some(rails), None) => rails
                    .iter()
                    .map(|r| HubrisPmbusRail { name: r.clone(), phases: None })
                    .collect::<_>(),
                (Some(r), Some(p)) => r
                    .iter()
                    .zip(p.iter())
                    .map(|(r, p)| HubrisPmbusRail {
                        name: r.clone(),
                        phases: Some(p.clone()),
                    })
                    .collect::<_>(),
                _ => vec![],
            }
        }

        if let Some(pmbus) = &device.pmbus {
            HubrisI2cDeviceClass::Pmbus {
                rails: rails_phases(&pmbus.rails, &None),
            }
        } else if let Some(power) = &device.power {
            if power.pmbus {
                HubrisI2cDeviceClass::Pmbus {
                    rails: rails_phases(&power.rails, &power.phases),
                }
            } else {
                HubrisI2cDeviceClass::Unspecified
            }
        } else {
            HubrisI2cDeviceClass::Unspecified
        }
    }
}

#[derive(Clone, Debug, Serialize)]
pub struct HubrisI2cDevice {
    pub device: String,
    pub name: Option<String>,
    pub controller: u8,
    pub port: HubrisI2cPort,
    pub mux: Option<u8>,
    pub segment: Option<u8>,
    pub address: u8,
    pub description: String,
    pub class: HubrisI2cDeviceClass,
    pub removable: bool,
}

#[derive(Copy, Clone, Deserialize, Debug, PartialEq, Eq, Hash, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum HubrisSensorKind {
    Temperature,
    Power,
    Current,
    Voltage,
    InputCurrent,
    InputVoltage,
    Speed,
}

#[derive(Clone, Debug, PartialOrd, Ord, Eq, PartialEq, Serialize)]
pub enum HubrisSensorDevice {
    I2c(usize),
    Other(String, usize),
}

#[derive(Clone, Debug, Serialize)]
pub struct HubrisSensor {
    pub name: String,
    pub kind: HubrisSensorKind,
    pub device: HubrisSensorDevice,
}

impl HubrisSensorKind {
    pub fn to_string(&self) -> &str {
        match self {
            HubrisSensorKind::Temperature => "temp",
            HubrisSensorKind::Power => "power",
            HubrisSensorKind::Current => "current",
            HubrisSensorKind::Voltage => "voltage",
            HubrisSensorKind::InputCurrent => "input-current",
            HubrisSensorKind::InputVoltage => "input-voltage",
            HubrisSensorKind::Speed => "speed",
        }
    }

    pub fn from_string(kind: &str) -> Option<Self> {
        match kind {
            "temp" | "temperature" => Some(HubrisSensorKind::Temperature),
            "power" => Some(HubrisSensorKind::Power),
            "current" => Some(HubrisSensorKind::Current),
            "voltage" => Some(HubrisSensorKind::Voltage),
            "input-current" => Some(HubrisSensorKind::InputCurrent),
            "input-voltage" => Some(HubrisSensorKind::InputVoltage),
            "speed" => Some(HubrisSensorKind::Speed),
            _ => None,
        }
    }
}

#[derive(Clone)]
pub struct HubrisFlashMap {
    /// Linear map of all flash memory
    pub contents: Vec<u8>,

    /// Regions within that memory as offset + size tuples, indexed by address
    pub regions: BTreeMap<u32, (u32, usize)>,
}

impl HubrisFlashMap {
    /// # Errors
    ///
    /// (Non-exhaustive list added when surprising error conditions were
    /// discovered:)
    ///
    /// This will fail if the `HubrisArchive` is fake, i.e. contains zero bytes.
    pub fn new(hubris: &HubrisArchive) -> Result<Self> {
        if hubris.archive().is_empty() {
            bail!("archive is required for network use but was not provided");
        }
        //
        // We want to read in the "final.elf" from our archive and use that
        // to determine the memory that constitutes flash.
        //
        let cursor = Cursor::new(hubris.archive());
        let mut archive = zip::ZipArchive::new(cursor)?;
        let mut file = archive
            .by_name("img/final.elf")
            .map_err(|e| anyhow!("failed to find final.elf: {}", e))?;

        let mut contents = Vec::new();
        file.read_to_end(&mut contents)?;

        let elf = Elf::parse(&contents).map_err(|e| {
            anyhow!("failed to parse final.elf as an ELF file: {}", e)
        })?;

        let regions = elf
            .section_headers
            .iter()
            .filter(|shdr| {
                shdr.sh_type == goblin::elf::section_header::SHT_PROGBITS
            })
            .map(|shdr| {
                (
                    shdr.sh_addr as u32,
                    (shdr.sh_size as u32, shdr.sh_offset as usize),
                )
            })
            .collect();

        Ok(Self { contents, regions })
    }

    pub fn read(&self, addr: u32, data: &mut [u8]) -> Option<()> {
        if let Some((&base, &(size, offset))) =
            self.regions.range(..=addr).next_back()
        {
            if base <= addr && base + size > addr {
                let start = (addr - base) as usize;
                let roffs = offset + start;

                if start + data.len() <= size as usize {
                    data.copy_from_slice(
                        &self.contents[roffs..roffs + data.len()],
                    );

                    return Some(());
                }

                let len = (size as usize) - start;
                data[..len].copy_from_slice(&self.contents[roffs..roffs + len]);

                return self.read(addr + len as u32, &mut data[len..]);
            }
        }

        None
    }
}

//
// This is the Hubris definition
//
#[derive(Debug, Deserialize)]
pub enum FlashProgram {
    PyOcd(Vec<FlashArgument>),
    OpenOcd(FlashProgramConfig),
}

#[derive(Debug, Deserialize)]
pub enum FlashProgramConfig {
    Path(Vec<String>),
    Payload(String),
}

#[derive(Debug, Deserialize)]
pub enum FlashArgument {
    Direct(String),
    Payload,
    FormattedPayload(String, String),
    Config,
}

#[derive(Debug, Deserialize)]
pub struct HubrisFlashMeta {
    /// Legacy flash program. Not included in new archives.
    pub program: Option<FlashProgram>,
    /// Arguments for legacy flash program, or empty if not used.
    #[serde(default)]
    pub args: Vec<FlashArgument>,
    /// Chip name used by probe-rs.
    pub chip: Option<String>,
}

//
// Flash information pulled from the archive
//
pub struct HubrisFlashConfig {
    pub metadata: HubrisFlashMeta,
    pub elf: Vec<u8>,
    pub chip: Option<String>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum HubrisArchiveDoneness {
    /// Fully load archive
    Cook,
    /// Load archive into memory, but do not otherwise process
    Raw,
}

#[derive(Copy, Clone, Debug)]
struct NamespaceId(usize);

#[derive(Debug)]
struct Namespaces(Vec<NamespaceComponent>);

#[derive(Debug)]
struct NamespaceComponent {
    name: String,
    parent: Option<NamespaceId>,
}

impl Namespaces {
    fn new() -> Self {
        Namespaces(Vec::new())
    }

    fn allocate(
        &mut self,
        name: &str,
        parent: Option<NamespaceId>,
    ) -> NamespaceId {
        let id = NamespaceId(self.0.len());

        self.0.push(NamespaceComponent { name: name.to_string(), parent });

        id
    }

    //
    // For a namespace identifer, compose and return the entire namespace
    // vector as strings.
    //
    fn to_full(&self, id: Option<NamespaceId>) -> Result<Vec<&str>> {
        let mut rval = vec![];
        let mut current = id;

        while let Some(id) = current {
            let component = self
                .0
                .get(id.0)
                .ok_or_else(|| anyhow!("namespace id {} is invalid", id.0))?;

            rval.push(component.name.as_str());
            current = component.parent;
        }

        rval.reverse();

        Ok(rval)
    }

    //
    // A convenience routine to take a namespace identifier and a name,
    // and return the entire, ::-delimited name.
    //
    fn to_full_name(
        &self,
        id: Option<NamespaceId>,
        name: &String,
    ) -> Result<Option<String>> {
        let mut n = self.to_full(id)?;

        if !n.is_empty() {
            n.push(name);
            Ok(Some(n.join("::")))
        } else {
            Ok(None)
        }
    }
}

#[derive(Debug)]
pub struct HubrisArchive {
    // the entire archive
    archive: Vec<u8>,

    // constructed manifest
    pub manifest: HubrisManifest,

    // app table
    apptable: Option<(u32, Vec<u8>)>,

    // image ID
    pub imageid: Option<(u32, Vec<u8>)>,

    // loaded regions
    loaded: BTreeMap<u32, HubrisRegion>,

    // current object
    current: u32,

    // non-None if a dump of a single task
    task_dump: Option<DumpTask>,

    // Instructions: address to bytes/target tuple. The target will be None if
    // the instruction did not decode as some kind of jump/branch/call.
    instrs: HashMap<u32, (Vec<u8>, Option<HubrisTarget>)>,

    // Manual stack pushes before a syscall
    syscall_pushes: HashMap<u32, Option<Vec<ARMRegister>>>,

    // Current registers (if a dump)
    registers: HashMap<ARMRegister, u32>,

    // Modules: text address to module
    modules: BTreeMap<u32, HubrisModule>,

    // Tasks: name of task to ID
    tasks: HashMap<String, HubrisTask>,

    // DWARF call frame debugging sections: task to raw bytes
    frames: HashMap<HubrisTask, Vec<u8>>,

    // DWARF source code: goff to file/line
    src: HashMap<HubrisGoff, HubrisSrc>,

    // DWARF symbols: address to HubrisSymbol
    dsyms: BTreeMap<u32, HubrisSymbol>,

    // ELF symbols: address to name/length tuple
    esyms: BTreeMap<u32, (String, u32)>,

    // ELF symbols: name to value/length
    esyms_byname: MultiMap<String, (u32, u32)>,

    // Inlined: address/nesting tuple to length/goff/origin tuple
    inlined: BTreeMap<(u32, isize), (u32, HubrisGoff, HubrisGoff)>,

    // Subprograms: goff to demangled name
    subprograms: HashMap<HubrisGoff, String>,

    // Base types: goff to size
    basetypes: HashMap<HubrisGoff, HubrisBasetype>,

    // Base types: name to goff
    basetypes_byname: HashMap<String, HubrisGoff>,

    // Base types: goff to underlying type
    ptrtypes: HashMap<HubrisGoff, (String, HubrisGoff)>,

    // Structures: goff to struct
    structs: HashMap<HubrisGoff, HubrisStruct>,

    // Structures: name to goff
    structs_byname: MultiMap<String, HubrisGoff>,

    // Enums: goff to enum
    enums: HashMap<HubrisGoff, HubrisEnum>,

    // Enums: name to goff
    enums_byname: MultiMap<String, HubrisGoff>,

    // Arrays: goff to array
    arrays: HashMap<HubrisGoff, HubrisArray>,

    // Variables: name to goff/address/size tuple. Note that variables declared
    // in different modules will appear to have the same name here. Consider
    // using qualified_variables instead.
    variables: MultiMap<String, HubrisVariable>,

    // Qualified Variables: fully qualified Rust demangled name to
    // goff/address/size tuple.
    qualified_variables: MultiMap<String, HubrisVariable>,

    // Unions: goff to union
    unions: HashMap<HubrisGoff, HubrisUnion>,

    // Definitions: name to goff
    definitions: MultiMap<String, HubrisGoff>,

    // Space of all namespaces -- but namespacesspace seems needlessly cruel
    namespaces: Namespaces,

    // Extern regions in this archive
    extern_regions: ExternRegions,
}

#[rustfmt::skip::macros(anyhow, bail)]
impl HubrisArchive {
    pub fn new() -> Result<HubrisArchive> {
        Ok(Self {
            archive: Vec::new(),
            apptable: None,
            imageid: None,
            manifest: Default::default(),
            loaded: BTreeMap::new(),
            current: 0,
            task_dump: None,
            instrs: HashMap::new(),
            syscall_pushes: HashMap::new(),
            registers: HashMap::new(),
            modules: BTreeMap::new(),
            tasks: HashMap::new(),
            frames: HashMap::new(),
            src: HashMap::new(),
            dsyms: BTreeMap::new(),
            esyms: BTreeMap::new(),
            esyms_byname: MultiMap::new(),
            inlined: BTreeMap::new(),
            subprograms: HashMap::new(),
            basetypes: HashMap::new(),
            basetypes_byname: HashMap::new(),
            ptrtypes: HashMap::new(),
            structs: HashMap::new(),
            structs_byname: MultiMap::new(),
            enums: HashMap::new(),
            enums_byname: MultiMap::new(),
            arrays: HashMap::new(),
            variables: MultiMap::new(),
            qualified_variables: MultiMap::new(),
            unions: HashMap::new(),
            definitions: MultiMap::new(),
            namespaces: Namespaces::new(),
            extern_regions: ExternRegions::new(),
        })
    }

    pub fn instr_len(&self, addr: u32) -> Option<u32> {
        self.instrs.get(&addr).map(|instr| instr.0.len() as u32)
    }

    /// Looks up the jump target type of the previously-disassembled instruction
    /// at `addr`. Returns `None` if the instruction was did not affect control
    /// flow.
    ///
    /// TODO: this also returns `None` if `addr` is not an instruction boundary,
    /// which is probably wrong but we haven't totally thought it through yet.
    pub fn instr_target(&self, addr: u32) -> Option<HubrisTarget> {
        self.instrs.get(&addr).and_then(|&(_, target)| target)
    }

    pub fn instr_mod(&self, addr: u32) -> Option<&str> {
        if let Some(module) = self.modules.range(..=addr).next_back() {
            if addr < *module.0 + module.1.textsize {
                Some(&module.1.name)
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn instr_sym(&self, addr: u32) -> Option<(&str, u32)> {
        //
        // First, check our DWARF symbols.
        //
        let sym = match self.dsyms.range(..=addr).next_back() {
            Some((_, sym)) if addr < sym.addr + sym.size => {
                Some((sym.name.as_str(), sym.addr))
            }
            _ => None,
        };

        //
        // Fallback to our ELF symbols.
        //
        sym.or_else(|| match self.esyms.range(..=addr).next_back() {
            Some((&sym_addr, (name, sym_len))) if addr < sym_addr + sym_len => {
                Some((name, sym_addr))
            }
            _ => None,
        })
    }

    pub fn instr_inlined(&self, pc: u32, base: u32) -> Vec<HubrisInlined> {
        let mut inlined: Vec<HubrisInlined> = vec![];

        //
        // We find our stack of inlined functions by searching backwards from
        // our address (which we know must be greater than or equal to all
        // inlined functions that it is in).  This yields a vector that
        // starts from the greatest depth and ends with the least
        // depth -- so we reverse it before we return it.  We know
        // that our search is over when the address plus the length
        // is less than our base.
        //
        for (&(addr, _depth), (len, goff, origin)) in
            self.inlined.range(..=(pc, isize::MAX)).rev()
        {
            if addr + len < base {
                break;
            }

            if addr + len <= pc {
                continue;
            }

            if let Some(func) = self.subprograms.get(origin) {
                inlined.push(HubrisInlined {
                    addr,
                    name: func,
                    id: *goff,
                    origin: *origin,
                });
            }
        }

        inlined.reverse();
        inlined
    }

    fn load_sensor_config(
        &mut self,
        sensor: &HubrisConfigSensor,
    ) -> Result<()> {
        for device in &sensor.devices {
            for (kind, &count) in &device.sensors {
                for i in 0..count {
                    self.manifest.sensors.push(HubrisSensor {
                        name: device.name.clone(),
                        kind: HubrisSensorKind::from_string(kind).ok_or_else(
                            || anyhow!("Unknown sensor kind {kind}"),
                        )?,
                        device: HubrisSensorDevice::Other(
                            device.device.clone(),
                            i,
                        ),
                    });
                }
            }
        }
        Ok(())
    }

    fn load_i2c_config(&mut self, i2c: &HubrisConfigI2c) -> Result<()> {
        let mut buses = HashMap::new();

        if let Some(ref controllers) = i2c.controllers {
            for controller in controllers {
                for (index, (name, port)) in controller.ports.iter().enumerate()
                {
                    self.manifest.i2c_buses.push(HubrisI2cBus {
                        controller: controller.controller,
                        port: HubrisI2cPort {
                            name: name.clone(),
                            index: index as u8,
                        },
                        name: port.name.as_ref().cloned(),
                        description: port.description.as_ref().cloned(),
                        target: controller.target.unwrap_or(false),
                    });
                }
            }
        }

        for bus in &self.manifest.i2c_buses {
            if let Some(ref name) = bus.name {
                buses.insert(name, bus);
            }
        }

        let sensor_name = |d: &HubrisConfigI2cDevice,
                           idx: usize,
                           kind: HubrisSensorKind|
         -> Result<String> {
            if let Some(pmbus) = &d.pmbus {
                if let Some(rails) = &pmbus.rails {
                    if idx < rails.len() {
                        return Ok(rails[idx].clone());
                    } else {
                        bail!("sensor count exceeds rails for {:?}", d);
                    }
                }
            } else if d.power.is_some()
                && d.power
                    .as_ref()
                    .unwrap()
                    .sensors
                    .as_ref()
                    .map_or(true, |s| s.contains(&kind))
            {
                if let Some(rails) = &d.power.as_ref().unwrap().rails {
                    if idx < rails.len() {
                        return Ok(rails[idx].clone());
                    } else {
                        bail!("sensor count exceeds rails for {:?}", d);
                    }
                }
            }

            if let Some(names) = &d.sensors.as_ref().unwrap().names {
                if idx >= names.len() {
                    bail!(
                        "name array is too short ({}) for sensor index ({})",
                        names.len(),
                        idx
                    );
                } else {
                    Ok(names[idx].clone())
                }
            } else if let Some(name) = &d.name {
                if idx == 0 {
                    Ok(name.clone())
                } else {
                    Ok(format!("{}#{}", name, idx))
                }
            } else if idx == 0 {
                Ok(d.device.clone())
            } else {
                Ok(format!("{}#{}", d.device, idx))
            }
        };
        let get_sensor = |d: &HubrisConfigI2cDevice,
                          i: usize,
                          ndx: usize,
                          kind: HubrisSensorKind|
         -> Result<HubrisSensor> {
            let name = sensor_name(d, i, kind)?;
            Ok(HubrisSensor {
                name,
                kind,
                device: HubrisSensorDevice::I2c(ndx),
            })
        };

        if let Some(ref devices) = i2c.devices {
            for device in devices {
                let name = &device.device;

                let (controller, port) = match &device.bus {
                    Some(bus) => match buses.get(&bus) {
                        Some(bus) => (bus.controller, &bus.port),
                        None => {
                            //
                            // This really shouldn't happen: we have
                            // a bus that doesn't exist.
                            //
                            bail!("{}: unknown bus {}", name, bus);
                        }
                    },
                    None => match (device.controller, &device.port) {
                        (Some(controller), Some(port)) => (
                            controller,
                            self.lookup_i2c_port(controller, port)?,
                        ),
                        (None, _) => {
                            bail!("{}: missing controller", name);
                        }
                        (Some(controller), None) => {
                            (controller, self.i2c_port(controller)?)
                        }
                    },
                };

                let port = port.clone();

                if let Some(sensors) = &device.sensors {
                    let ndx = self.manifest.i2c_devices.len();

                    for i in 0..sensors.temperature {
                        self.manifest.sensors.push(get_sensor(
                            device,
                            i,
                            ndx,
                            HubrisSensorKind::Temperature,
                        )?);
                    }

                    for i in 0..sensors.power {
                        self.manifest.sensors.push(get_sensor(
                            device,
                            i,
                            ndx,
                            HubrisSensorKind::Power,
                        )?);
                    }
                    for i in 0..sensors.current {
                        self.manifest.sensors.push(get_sensor(
                            device,
                            i,
                            ndx,
                            HubrisSensorKind::Current,
                        )?);
                    }
                    for i in 0..sensors.voltage {
                        self.manifest.sensors.push(get_sensor(
                            device,
                            i,
                            ndx,
                            HubrisSensorKind::Voltage,
                        )?);
                    }
                    for i in 0..sensors.input_current {
                        self.manifest.sensors.push(get_sensor(
                            device,
                            i,
                            ndx,
                            HubrisSensorKind::InputCurrent,
                        )?);
                    }
                    for i in 0..sensors.input_voltage {
                        self.manifest.sensors.push(get_sensor(
                            device,
                            i,
                            ndx,
                            HubrisSensorKind::InputVoltage,
                        )?);
                    }

                    for i in 0..sensors.speed {
                        self.manifest.sensors.push(get_sensor(
                            device,
                            i,
                            ndx,
                            HubrisSensorKind::Speed,
                        )?);
                    }
                }

                self.manifest.i2c_devices.push(HubrisI2cDevice {
                    device: device.device.clone(),
                    name: device.name.clone(),
                    controller,
                    port,
                    mux: device.mux,
                    segment: device.segment,
                    address: device.address,
                    description: device.description.clone(),
                    class: HubrisI2cDeviceClass::from(device),
                    removable: device.removable.unwrap_or(false),
                });
            }
        }

        Ok(())
    }

    fn load_config(
        &mut self,
        config: &HubrisConfig,
        peripherals: Option<&IndexMap<String, HubrisConfigPeripheral>>,
    ) -> Result<()> {
        self.manifest.board = Some(config.board.clone());
        self.manifest.name = Some(config.name.clone());
        self.manifest.target = Some(config.target.clone());
        self.manifest.features = match config.kernel.features {
            Some(ref features) => features.clone(),
            None => vec![],
        };
        self.manifest.auxflash =
            config.config.as_ref().and_then(|c| c.auxflash.clone());

        let mut named_interrupts = HashMap::new();

        if let Some(peripherals) = peripherals {
            for (name, p) in peripherals {
                self.manifest.peripherals.insert(name.clone(), p.address);
                self.manifest
                    .peripherals_byaddr
                    .insert(p.address, name.clone());

                if let Some(ref interrupts) = p.interrupts {
                    for (interrupt, irq) in interrupts {
                        named_interrupts
                            .insert(format!("{}.{}", name, interrupt), *irq);
                    }
                }
            }
        }

        for (name, task) in &config.tasks {
            if let Some(ref features) = task.features {
                self.manifest
                    .task_features
                    .insert(name.clone(), features.clone());
            }

            if let Some(ref interrupts) = task.interrupts {
                let mut task_irqs = vec![];

                for (irq_str, notification) in interrupts {
                    let irq = match irq_str.parse::<u32>() {
                        Ok(irq_num) => irq_num,
                        Err(_) => {
                            //
                            // If our IRQ number doesn't parse, it may be
                            // because it's named; look it up before failing.
                            //
                            match named_interrupts.get(irq_str) {
                                Some(irq_num) => *irq_num,
                                None => {
                                    bail!(
                                        "unrecognized irq {} on task {}",
                                        irq_str, name
                                    )
                                }
                            }
                        }
                    };
                    let notification = match notification {
                        HubrisTaskInterrupt::Mask(i) => *i,
                        HubrisTaskInterrupt::Named(name) => match task
                            .notifications
                            .iter()
                            .position(|n| n == name)
                        {
                            Some(i) => 1 << i,
                            None => bail!(
                                "could not find notification '{name}' \
                                 (options are {:?})",
                                task.notifications),
                        },
                    };

                    task_irqs.push((notification, irq));
                }

                self.manifest.task_irqs.insert(name.clone(), task_irqs);
            }

            self.manifest
                .task_notifications
                .insert(name.clone(), task.notifications.clone());
        }

        if let Some(ref config) = config.config {
            if let Some(i2c) = config.i2c.as_ref() {
                self.load_i2c_config(i2c)?;
            }
            if let Some(sensor) = config.sensor.as_ref() {
                self.load_sensor_config(sensor)?;
            }
        }

        Ok(())
    }

    fn load_archive(&mut self, archive: &[u8]) -> Result<()> {
        let cursor = Cursor::new(archive);
        let mut archive = zip::ZipArchive::new(cursor)?;
        let manifest = &mut self.manifest;

        macro_rules! byname {
            ($name:tt) => {
                archive
                    .by_name($name)
                    .map_err(|e| anyhow!("failed to find \"{}\": {}", $name, e))
            };
        }

        //
        // First, we'll load aspects of configuration.
        //
        manifest.version = Some(str::from_utf8(archive.comment())?.to_string());

        if let Ok(mut file) = archive.by_name("git-rev") {
            let mut gitrev = String::new();
            file.read_to_string(&mut gitrev)
                .context("failed reading `git-rev`")?;
            manifest.gitrev = Some(gitrev);
        }

        if let Ok(mut file) = archive.by_name("image-name") {
            let mut image = String::new();
            file.read_to_string(&mut image)
                .context("failed reading `image-name`")?;
            manifest.image = Some(image);
        }

        let mut app = String::new();
        byname!("app.toml")?.read_to_string(&mut app)?;

        let mut config: HubrisConfig = toml::from_slice(app.as_bytes())?;

        // Apply TOML patches, if `patches.toml` is present in the archive.
        if let Ok(mut patches) = byname!("patches.toml") {
            let mut patch_str = String::new();
            patches.read_to_string(&mut patch_str)?;
            let patches: HubrisConfigPatches =
                toml::from_slice(patch_str.as_bytes())?;
            config.name = patches.name;
            for (task, features) in patches.features {
                config
                    .tasks
                    .get_mut(&task)
                    .unwrap()
                    .features
                    .get_or_insert_with(Default::default)
                    .extend(features.into_iter());
            }
        }

        let config = config; // remove mutability

        //
        // Before we load our config, we need to find where our peripherals
        // are located (if we have any).  If they are hanging off our config,
        // use that -- but if we have a newer archive that contains a referred
        // chip, pull in the TOML that it points to instead.
        //
        if let Some(ref peripherals) = config.peripherals {
            self.load_config(&config, Some(peripherals))?;
        } else if let Some(ref chip) = config.chip {
            //
            // Paths are relative, so we always pull the basename -- and
            // paths within a ZIP archive always use the forward slash
            // as a separator.
            //
            let path = match chip.rsplit('/').next() {
                // Newer archive files may include the chip peripherals with
                // the generic name "chip.toml" instead of something like
                // "stm32h7.toml".  This is the case when the `chip` parameter
                // in the app config is a path rather than a filename ending
                // in ".toml"
                Some(p) if p.ends_with(".toml") => p,
                Some(_) => "chip.toml",
                None => chip,
            };

            let mut chip = String::new();

            //
            // Ideally, we would hard-fail if we didn't find this file in
            // the archive -- but there was a small window of time in which
            // the chip TOML was not properly in the archive.  This is still
            // recoverable -- but anything that relies on the presence of
            // peripherals in the TOML will fail.
            //
            if let Ok(mut file) = archive.by_name(path) {
                file.read_to_string(&mut chip)?;

                let peripherals: IndexMap<String, HubrisConfigPeripheral> =
                    toml::from_slice(chip.as_bytes())?;

                self.load_config(&config, Some(&peripherals))?;
            } else {
                self.load_config(&config, None)?;
            }
        } else {
            self.load_config(&config, None)?;
        }

        //
        // Next up is the kernel.  Note that we refer to it explicitly with a
        // forward slash: regardless of platform, paths within a ZIP archive
        // use the forward slash as a separator.
        //
        let mut buffer = Vec::new();
        byname!("elf/kernel")?.read_to_end(&mut buffer)?;
        let mut loader = HubrisObjectLoader::new(self.current)?;
        loader.load_object("kernel", HubrisTask::Kernel, &buffer)?;
        self.merge(loader)?;

        //
        // Find and unzip tasks in parallel.  Note that into_par_iter discards
        // ordering, so we store an additional index as the first element in the
        // resulting tuple for later sorting.
        //
        use rayon::prelude::*;
        let mut objects = (0..archive.len())
            .into_par_iter()
            .map(|i| -> Result<Option<(usize, String, Vec<u8>)>> {
                // ZipArchive is cheap to clone since the backing is cheap
                let mut archive = archive.clone();
                let mut file = archive.by_index(i)?;
                let path = Path::new(file.name());
                let pieces = path.iter().collect::<Vec<_>>();

                //
                // If the second-to-last element of our path is "task", we have
                // a winner!
                //
                if pieces.len() < 2 || pieces[pieces.len() - 2] != "task" {
                    return Ok(None);
                }

                let mut buffer = Vec::new();
                file.read_to_end(&mut buffer)?;
                let filename = Path::new(file.name());
                Ok(Some((
                    i,
                    filename.file_name().unwrap().to_str().unwrap().to_owned(),
                    buffer,
                )))
            })
            .filter_map(|f| f.transpose())
            .collect::<Result<Vec<_>>>()?;

        //
        // Sort by file index then convert from file index -> task index.
        //
        // This depends on the fact that tasks are stored in task ID order in
        // the archive!
        //
        objects.sort();
        for (i, o) in objects.iter_mut().enumerate() {
            o.0 = i;
        }

        // Load each task using a HubrisObjectLoader, which can run
        // independently in a thread.
        let files = objects
            .into_par_iter()
            .map(|(id, name, buf)| {
                let id: u32 = id.try_into().unwrap();
                let mut loader = HubrisObjectLoader::new(self.current + id)?;
                loader.load_object(&name, HubrisTask::Task(id), &buf)?;
                Ok(loader)
            })
            .collect::<Result<Vec<_>>>()?;

        for loader in files {
            self.merge(loader)?;
        }
        assert_eq!(self.current as usize, self.tasks.len());

        //
        // Now that we have loaded our tasks, load our extern regions.
        //
        self.extern_regions = ExternRegions::load(self, &mut archive, &config)?;

        //
        // Post-process our enums and structs to add their fully scoped names.
        //
        let mut work = BTreeSet::new();

        for (name, enums) in self.enums_byname.iter_all() {
            for goff in enums.iter() {
                let n = self.enums.get(goff).unwrap().namespace;

                if let Some(full) = self.namespaces.to_full_name(n, name)? {
                    work.insert((full, *goff));
                }
            }
        }

        for (name, goff) in work.iter() {
            self.enums_byname.insert(name.clone(), *goff);
        }

        let mut work = BTreeSet::new();

        for (name, structs) in self.structs_byname.iter_all() {
            for goff in structs.iter() {
                let n = self.structs.get(goff).unwrap().namespace;

                if let Some(full) = self.namespaces.to_full_name(n, name)? {
                    work.insert((full, *goff));
                }
            }
        }

        for (name, goff) in work.iter() {
            self.structs_byname.insert(name.clone(), *goff);
        }

        Ok(())
    }

    fn for_each_task<F: FnMut(&Path, &[u8]) -> Result<()>>(
        archive: &mut zip::ZipArchive<Cursor<&[u8]>>,
        mut f: F,
    ) -> Result<()> {
        use rayon::prelude::*;
        let files = (0..archive.len())
            .into_par_iter()
            .map(|i| {
                // ZipArchive is cheap to clone since the backing is cheap
                let mut archive = archive.clone();
                let mut file = archive.by_index(i)?;
                let path = Path::new(file.name());
                let pieces = path.iter().collect::<Vec<_>>();

                //
                // If the second-to-last element of our path is "task", we have
                // a winner!
                //
                if pieces.len() < 2 || pieces[pieces.len() - 2] != "task" {
                    return Ok(None);
                }

                let mut buffer = Vec::new();
                file.read_to_end(&mut buffer)?;
                Ok(Some((file.name().to_owned(), buffer)))
            })
            .filter_map(|f| f.transpose())
            .collect::<Result<Vec<(String, Vec<u8>)>, anyhow::Error>>()?;

        for (file_name, buffer) in files {
            f(Path::new(&file_name), &buffer)?;
        }
        Ok(())
    }

    fn merge(&mut self, loader: HubrisObjectLoader) -> Result<()> {
        if loader.imageid.is_some() {
            self.imageid = loader.imageid;
        }
        if loader.apptable.is_some() {
            self.apptable = loader.apptable;
        }
        self.esyms_byname.extend(loader.esyms_byname);

        self.esyms.extend(loader.esyms);
        self.tasks.extend(loader.tasks);
        self.modules.extend(loader.modules);
        self.frames.extend(loader.frames);
        self.loaded.extend(loader.loaded);
        self.instrs.extend(loader.instrs);
        self.syscall_pushes.extend(loader.syscall_pushes);
        self.unions.extend(loader.unions);
        self.src.extend(loader.src);
        self.enums_byname.extend(loader.enums_byname);
        self.structs_byname.extend(loader.structs_byname);
        self.arrays.extend(loader.arrays);
        self.basetypes.extend(loader.basetypes);
        self.basetypes_byname.extend(loader.basetypes_byname);
        self.ptrtypes.extend(loader.ptrtypes);
        self.inlined.extend(loader.inlined);
        self.subprograms.extend(loader.subprograms);
        self.dsyms.extend(loader.dsyms);
        self.variables.extend(loader.variables);
        self.qualified_variables.extend(loader.qualified_variables);
        self.definitions.extend(loader.definitions);

        // Namespaces need to be shifted when merging into the global namespaces
        // vec.  This change applies to the `namespace` member in structs and
        // enums, as well as the `namespaces` table itself.
        let ns_offset = self.namespaces.0.len();

        self.namespaces.0.extend(loader.namespaces.0.into_iter().map(
            |mut v| {
                if let Some(n) = &mut v.parent {
                    n.0 += ns_offset;
                }
                v
            },
        ));
        self.structs.extend(loader.structs.into_iter().map(|(goff, mut s)| {
            if let Some(n) = &mut s.namespace {
                n.0 += ns_offset;
            }
            (goff, s)
        }));
        self.enums.extend(loader.enums.into_iter().map(|(goff, mut s)| {
            if let Some(n) = &mut s.namespace {
                n.0 += ns_offset;
            }
            (goff, s)
        }));

        self.current += 1;
        Ok(())
    }

    pub fn load(
        &mut self,
        archive: &str,
        doneness: HubrisArchiveDoneness,
    ) -> Result<()> {
        let metadata = fs::metadata(archive)?;

        if metadata.is_dir() {
            bail!("a directory as an archive is deprecated; \
                use archive instead");
        }

        //
        // We read the entire archive into memory (and hold onto it) -- we
        // are going to need most of it anyway, and we want to have
        // the entire archive in memory to be able to write it out to
        // any generated dump.
        //
        let contents = fs::read(archive)?;

        let cursor = Cursor::new(&contents);
        let archive = zip::ZipArchive::new(cursor)?;
        let comment = str::from_utf8(archive.comment())
            .context("Failed to decode comment string")?;
        Self::check_version(comment)?;

        if doneness == HubrisArchiveDoneness::Cook {
            self.load_archive(&contents)?;
        }

        self.archive = contents;
        Ok(())
    }

    fn check_version(comment: &str) -> Result<()> {
        match comment.strip_prefix("hubris build archive v") {
            Some(v) => {
                let archive_version = match v {
                    // Special-case for older archives
                    "1.0.0" => 1,
                    // There was no v1, just v1.0.0
                    "1" => bail!("Invalid archive version 'v1'"),
                    // Otherwise, expect an integer
                    v => v.parse().with_context(|| {
                        format!("Failed to parse version string {}", v)
                    })?,
                };
                if archive_version > MAX_HUBRIS_VERSION {
                    bail!("\
                        Hubris archive version is unsupported.\n\
                        Humility supports v{} and earlier; archive is v{}.\n\
                        Please update Humility.",
                        MAX_HUBRIS_VERSION, v)
                }
            }
            None => {
                bail!(
                    "Could not parse hubris archive version from '{}'",
                    comment)
            }
        }
        Ok(())
    }

    pub fn load_flash_config(&self) -> Result<HubrisFlashConfig> {
        let cursor = Cursor::new(&self.archive);
        let mut archive = zip::ZipArchive::new(cursor)?;

        macro_rules! slurp {
            ($name:tt) => {{
                let mut buffer = Vec::new();
                archive
                    .by_name($name)
                    .map_err(|e| {
                        anyhow!("failed to find \"{}\": {}", $name, e)
                    })?
                    .read_to_end(&mut buffer)?;
                buffer
            }};
        }

        let flash_ron = archive.by_name("img/flash.ron").map_err(|_| {
            anyhow!(
                "could not find img/flash.ron in archive; \
                does archive pre-date addition of flash information?"
            )
        })?;

        let config: HubrisFlashMeta = ron::de::from_reader(flash_ron)?;

        // This is incredibly ugly! It also gives us backwards compatibility!
        let chip: Option<String> = match config.chip {
            Some(ref chip) => Some(chip.to_string()),
            None => match &config.program {
                Some(FlashProgram::PyOcd(args)) => {
                    let s69 = regex::Regex::new(r"lpc55s69").unwrap();
                    let s28 = regex::Regex::new(r"lpc55s28").unwrap();
                    let mut c: Option<String> = None;
                    for arg in args {
                        c = match arg {
                            FlashArgument::Direct(s) => {
                                if s69.is_match(s) {
                                    Some("LPC55S69JBD100".to_string())
                                } else if s28.is_match(s) {
                                    Some("LPC55S28JBD64".to_string())
                                } else {
                                    None
                                }
                            }
                            _ => None,
                        };
                        if c.is_some() {
                            break;
                        }
                    }
                    c
                }
                Some(FlashProgram::OpenOcd(ref a)) => match a {
                    FlashProgramConfig::Payload(d) => {
                        let h7 =
                            regex::Regex::new(r"find target/stm32h7").unwrap();
                        let f3 =
                            regex::Regex::new(r"find target/stm32f3").unwrap();
                        let f4 =
                            regex::Regex::new(r"find target/stm32f4").unwrap();
                        let g0 =
                            regex::Regex::new(r"find target/stm32g0").unwrap();

                        let mut c: Option<String> = None;

                        for s in d.split('\n') {
                            if h7.is_match(s) {
                                c = Some("STM32H753ZITx".to_string());
                                break;
                            }
                            if f3.is_match(s) {
                                c = Some("STM32F301C6Tx".to_string());
                                break;
                            }
                            if f4.is_match(s) {
                                c = Some("STM32F401CBUx".to_string());
                                break;
                            }
                            if g0.is_match(s) {
                                c = Some("STM32G030C6Tx".to_string());
                                break;
                            }
                        }
                        c
                    }
                    _ => bail!("Unexpected config?"),
                },
                None => {
                    bail!("archive flash.ron is missing both probe-rs chip \
                        name and legacy flash config");
                }
            },
        };

        Ok(HubrisFlashConfig {
            metadata: config,
            elf: slurp!("img/final.elf"),
            chip,
        })
    }

    pub fn chip(&self) -> Option<String> {
        // It turns out the easiest way right now to get the chip is via the
        // flash config. Long term we may want to fix this
        //

        let flash = match self.load_flash_config() {
            Ok(f) => f,
            Err(_) => return None,
        };
        flash.chip
    }

    fn load_registers(&mut self, r: &[u8]) -> Result<()> {
        if r.len() % 8 != 0 {
            bail!("bad length {} in registers note", r.len());
        }

        for (i, chunk) in r.chunks_exact(8).enumerate() {
            let (id, val) = chunk.split_at(4);
            // We unwrap here because it can only fail if the length is wrong,
            // but we've explicitly broken a chunk of 8 into two chunks of 4,
            // so a failure here would mean this code has been changed.
            let id = u32::from_le_bytes(id.try_into().unwrap());
            let val = u32::from_le_bytes(val.try_into().unwrap());

            let reg = match ARMRegister::from_u32(id) {
                Some(r) => r,
                None => {
                    // This can totally happen if we encounter a future coredump
                    // where we decided to store, say, additional MSRs or a
                    // floating point register. Since this version of Humility
                    // doesn't understand them, we'll just skip it.
                    continue;
                }
            };

            if self.registers.insert(reg, val).is_some() {
                bail!("duplicate register {} ({}) at offset {}", reg, id, i * 8);
            }
        }

        Ok(())
    }

    pub fn load_dump(
        &mut self,
        dumpfile: &str,
        doneness: HubrisArchiveDoneness,
    ) -> Result<()> {
        //
        // We expect the dump to be an ELF core dump.
        //
        let contents = fs::read(dumpfile)?;
        let elf = Elf::parse(&contents).map_err(|e| {
            anyhow!("failed to parse {} as an ELF file: {}", dumpfile, e)
        })?;

        if let Some(notes) = elf.iter_note_headers(&contents) {
            for note in notes {
                match note {
                    Ok(note) => {
                        if note.name != OXIDE_NT_NAME {
                            continue;
                        }

                        match note.n_type {
                            OXIDE_NT_HUBRIS_ARCHIVE => {
                                if doneness == HubrisArchiveDoneness::Cook {
                                    self.load_archive(note.desc)?;
                                }

                                self.archive = note.desc.to_vec();
                            }
                            OXIDE_NT_HUBRIS_REGISTERS => {
                                self.load_registers(note.desc)?;
                            }
                            OXIDE_NT_HUBRIS_TASK => {
                                match DumpTask::read_from_prefix(note.desc) {
                                    Some(task) => {
                                        self.task_dump = Some(task);
                                    }
                                    None => {
                                        bail!(
                                            "unrecognized task {:?}", note.desc
                                        );
                                    }
                                }
                            }
                            _ => {
                                bail!("unrecognized note 0x{:x}", note.n_type);
                            }
                        }
                    }
                    Err(e) => {
                        bail!("failed to parse note: {}", e);
                    }
                }
            }
        }

        Ok(())
    }

    /// Load *only* a kernel -- which itself is only useful when operating on
    /// a file and not an archive.  This will fail if an archive has already
    /// been loaded.
    pub fn load_kernel(&mut self, kernel: &str) -> Result<()> {
        if !self.modules.is_empty() {
            bail!("cannot specify both an archive and a kernel");
        }

        let contents = fs::read(kernel)?;

        let mut loader = HubrisObjectLoader::new(self.current)?;
        loader.load_object("kernel", HubrisTask::Kernel, &contents)?;
        self.merge(loader)?;

        Ok(())
    }

    pub fn loaded(&self) -> bool {
        !self.modules.is_empty()
    }

    ///
    /// Takes a list of potentially similar types and deduplicates the list.
    ///
    fn dedup<'a, I: Iterator<Item = &'a HubrisGoff>>(
        &self,
        goffs: I,
    ) -> Result<Vec<HubrisGoff>> {
        let mut cmp = Vec::new();
        let mut out = Vec::new();

        for &goff in goffs {
            cmp.push(goff);
        }

        for (i, lhs) in cmp.iter().enumerate() {
            let mut dup = false;
            for rhs in &cmp[i + 1..] {
                if !self.differ(*lhs, *rhs)? {
                    dup = true;
                    break;
                }
            }
            if !dup {
                out.push(*lhs);
            }
        }

        Ok(out)
    }

    ///
    /// Looks up the specified structure.  This returns a Result and not an
    /// Option because the assumption is that the structure is needed to be
    /// present, and be present exactly once.  If needed structures begin
    /// having their names duplicated in modules, we may need to support
    /// proper namespacing -- or kludgey namespacing...
    pub fn lookup_struct_byname(&self, name: &str) -> Result<&HubrisStruct> {
        match self.structs_byname.get_vec(name) {
            Some(v) => {
                let m = self.dedup(v.iter())?;

                if m.len() > 1 {
                    Err(anyhow!("{} matches more than one structure", name))
                } else {
                    Ok(self.structs.get(&m[0]).unwrap())
                }
            }
            _ => Err(anyhow!("expected structure {} not found", name)),
        }
    }

    pub fn lookup_basetype_byname(&self, name: &str) -> Result<&HubrisGoff> {
        match self.basetypes_byname.get(name) {
            Some(goff) => Ok(goff),
            None => Err(anyhow!("expected {} to be a basetype", name)),
        }
    }

    pub fn lookup_type(&self, goff: HubrisGoff) -> Result<HubrisType> {
        let r = self
            .lookup_struct(goff)
            .map(HubrisType::Struct)
            .or_else(|_| self.lookup_enum(goff).map(HubrisType::Enum))
            .or_else(|_| self.lookup_array(goff).map(HubrisType::Array))
            .or_else(|_| self.lookup_union(goff).map(HubrisType::Union))
            .or_else(|_| self.lookup_basetype(goff).map(HubrisType::Base))
            .or_else(|_| {
                if self.ptrtypes.contains_key(&goff) {
                    Ok(HubrisType::Ptr(goff))
                } else {
                    bail!("no entry found for goff: {:x?}", goff);
                }
            })?;
        Ok(r)
    }

    pub fn lookup_struct(&self, goff: HubrisGoff) -> Result<&HubrisStruct> {
        match self.structs.get(&goff) {
            Some(s) => Ok(s),
            None => Err(anyhow!("expected struct {} not found", goff)),
        }
    }

    pub fn lookup_enum(&self, goff: HubrisGoff) -> Result<&HubrisEnum> {
        match self.enums.get(&goff) {
            Some(union) => Ok(union),
            None => Err(anyhow!("expected enum {} not found", goff)),
        }
    }

    pub fn lookup_union(&self, goff: HubrisGoff) -> Result<&HubrisUnion> {
        match self.unions.get(&goff) {
            Some(union) => Ok(union),
            None => Err(anyhow!("expected union {} not found", goff)),
        }
    }

    pub fn lookup_array(&self, goff: HubrisGoff) -> Result<&HubrisArray> {
        match self.arrays.get(&goff) {
            Some(array) => Ok(array),
            None => Err(anyhow!("expected {} to be an array", goff)),
        }
    }

    pub fn lookup_basetype(&self, goff: HubrisGoff) -> Result<&HubrisBasetype> {
        match self.basetypes.get(&goff) {
            Some(basetype) => Ok(basetype),
            None => Err(anyhow!("expected {} to be a basetype", goff)),
        }
    }

    pub fn lookup_ptrtype(&self, goff: HubrisGoff) -> Result<HubrisGoff> {
        match self.ptrtypes.get(&goff) {
            Some((_name, ptr)) => Ok(*ptr),
            None => Err(anyhow!("pointer type {} not found", goff)),
        }
    }

    ///
    /// Looks up the specified symbol.  This is more of a convenience routine
    /// that turns an Option into a Result.
    pub fn lookup_symword(&self, name: &str) -> Result<u32> {
        match self.esyms_byname.get(name) {
            Some(sym) => {
                if sym.1 != 4 {
                    Err(anyhow!("symbol {} is not word-sized", name))
                } else {
                    Ok(sym.0)
                }
            }
            None => Err(anyhow!("expected symbol {} not found", name)),
        }
    }

    pub fn lookup_variable(&self, name: &str) -> Result<&HubrisVariable> {
        match self.variables.get(name) {
            Some(variable) => Ok(variable),
            None => Err(anyhow!("variable {} not found", name)),
        }
    }

    pub fn lookup_qualified_variable(
        &self,
        name: &str,
    ) -> Result<&HubrisVariable> {
        match self.qualified_variables.get(name) {
            Some(variable) => Ok(variable),
            None => Err(anyhow!("variable {} not found", name)),
        }
    }

    pub fn lookup_variables(&self, name: &str) -> Result<&Vec<HubrisVariable>> {
        match self.variables.get_vec(name) {
            None => Err(anyhow!("variable {} not found", name)),
            Some(variables) => Ok(variables),
        }
    }

    pub fn lookup_definition(&self, name: &str) -> Result<&HubrisGoff> {
        match self.definitions.get(name) {
            Some(goff) => Ok(goff),
            None => Err(anyhow!("definition {} not found", name)),
        }
    }

    pub fn qualified_variables(
        &self,
    ) -> impl Iterator<Item = (&str, &HubrisVariable)> {
        let task_dump = self.task_dump();

        self.qualified_variables.iter_all().flat_map(move |(n, v)| {
            v.iter()
                .filter(|&v| match task_dump {
                    None => true,
                    Some(t) => t == HubrisTask::from(v.goff),
                })
                .map(|e| (n.as_str(), e))
                .collect::<Vec<(&str, &HubrisVariable)>>()
        })
    }

    pub fn lookup_module(&self, task: HubrisTask) -> Result<&HubrisModule> {
        match self.modules.values().find(|m| m.task == task) {
            Some(module) => Ok(module),
            None => Err(anyhow!("no such task: {}", task)),
        }
    }

    pub fn lookup_module_by_iface(&self, name: &str) -> Option<&HubrisModule> {
        (0..self.ntasks())
            .map(|t| self.lookup_module(HubrisTask::Task(t as u32)).unwrap())
            .find(|t| t.iface.as_ref().map(|i| i.name == name).unwrap_or(false))
    }

    pub fn modules(&self) -> impl Iterator<Item = &HubrisModule> {
        self.modules.values()
    }

    pub fn lookup_task(&self, name: &str) -> Option<&HubrisTask> {
        self.tasks.get(name)
    }

    pub fn task_name(&self, index: usize) -> Option<&str> {
        let index = HubrisTask::Task(index as u32);
        // TODO this is super gross but we don't have the inverse of the tasks
        // mapping at the moment.
        self.tasks.iter().find(|(_, &i)| i == index).map(|(name, _)| &**name)
    }

    pub fn task_table(
        &self,
        core: &mut dyn crate::core::Core,
    ) -> Result<(u32, u32)> {
        //
        // On older kernels, we expect to find the task table through an
        // indirect pointer (TASK_TABLE_BASE); on newer kernels, it's entirely
        // statically allocated (HUBRIS_TASK_TABLE_SPACE).
        //
        if let Ok(base) = self.lookup_symword("TASK_TABLE_BASE") {
            let size = core
                .read_word_32(self.lookup_symword("TASK_TABLE_SIZE")?)
                .context("failed to read TASK_TABLE_SIZE")?;

            let base = core
                .read_word_32(base)
                .context("failed to read TASK_TABLE_BASE")?;

            Ok((base, size))
        } else if let Ok(t) = self.lookup_variable("HUBRIS_TASK_TABLE_SPACE") {
            let task = self.lookup_struct_byname("Task")?;
            Ok((t.addr, (t.size / task.size) as u32))
        } else {
            bail!(
                "could not find task table as \
                TASK_TABLE_BASE or HUBRIS_TASK_TABLE_SPACE"
            )
        }
    }

    pub fn lookup_src(&self, goff: HubrisGoff) -> Option<&HubrisSrc> {
        self.src.get(&goff)
    }

    pub fn ntasks(&self) -> usize {
        if self.current >= 1 {
            self.current as usize - 1
        } else {
            0
        }
    }

    /// If this is a dump from a single task, returns that task -- or None
    /// otherwise.
    pub fn task_dump(&self) -> Option<HubrisTask> {
        self.task_dump.map(|task| HubrisTask::Task(task.id.into()))
    }

    pub fn current_task(
        &self,
        core: &mut dyn crate::core::Core,
    ) -> Result<Option<HubrisTask>> {
        //
        // If this is a dump and it only contains a single task, there is
        // no current task.  If this is an online task, then we can't read
        // kernel memory remotely, so we can't tell.
        //
        if self.task_dump.is_some() || core.is_net() || core.is_archive() {
            return Ok(None);
        }

        let cur =
            core.read_word_32(self.lookup_symword("CURRENT_TASK_PTR")?)?;
        let (base, task_count) = self.task_table(core)?;
        let task_t = self.lookup_struct_byname("Task")?;
        let size = task_t.size;

        if cur < base || cur >= base + (task_count * size as u32) {
            bail!(
                "CURRENT_TASK_PTR ({cur:#x}) does not appear to point into \
                the task table ({base:#x}, {task_count} tasks, {size} \
                bytes per task)"
            );
        }

        if (cur - base) % size as u32 != 0 {
            bail!(
                "CURRENT_TASK_PTR ({cur:#x}) - base ({base:#x}) \
                is not an even multiple of task size ({size})"
            );
        }

        let ndx = (cur - base) / task_t.size as u32;

        if ndx >= task_count {
            bail!("current task ({ndx}) exceeds max ({task_count})");
        }

        Ok(Some(HubrisTask::Task(ndx)))
    }

    pub fn ticks(&self, core: &mut dyn crate::core::Core) -> Result<u64> {
        match self.task_dump {
            Some(task) => Ok(task.time),
            None => core.read_word_64(self.lookup_variable("TICKS")?.addr),
        }
    }

    //
    // If the kernel has died and has left an epitaph, this will return it
    // as a string.  If the kernel is not dead -- or if this kernel pre-dates
    // the epitaph -- this will return Ok(None).
    //
    pub fn epitaph(
        &self,
        core: &mut dyn crate::core::Core,
    ) -> Result<Option<String>> {
        match (
            self.lookup_variable("KERNEL_HAS_FAILED"),
            self.lookup_variable("KERNEL_EPITAPH"),
        ) {
            (Ok(failed), Ok(epitaph)) => {
                if failed.size != 1 {
                    bail!("KERNEL_HAS_FAILED exists, but is of the wrong size?");
                }

                let mut buf: Vec<u8> = vec![];
                buf.resize_with(failed.size, Default::default);
                core.read_8(failed.addr, buf.as_mut_slice())?;

                match buf[0] {
                    0 => Ok(None),

                    1 => {
                        buf.resize_with(epitaph.size, Default::default);
                        core.read_8(epitaph.addr, buf.as_mut_slice())?;

                        let fmt = HubrisPrintFormat {
                            newline: false,
                            interpret_as_c_string: true,
                            ..HubrisPrintFormat::default()
                        };

                        Ok(Some(self.printfmt(&buf, epitaph.goff, fmt)?))
                    }

                    _ => {
                        bail!("illegal KERNEL_HAS_FAILED value {}", buf[0]);
                    }
                }
            }
            (_, _) => Ok(None),
        }
    }

    pub fn validate(
        &self,
        core: &mut dyn crate::core::Core,
        criteria: HubrisValidate,
    ) -> Result<()> {
        let ntasks = self.ntasks();

        if self.current == 0 {
            //
            // If we have no objects, we were never loaded -- and we consider
            // this to be validated because it will give no answers rather
            // than wrong ones.
            //
            return Ok(());
        }

        if core.is_net() || core.is_archive() {
            return Ok(());
        }

        //
        // To validate that what we're running on the target matches what
        // we have in the archive, we are going to check the image ID, an
        // identifer created for this purpose.  If we don't have an image ID,
        // we check the legacy mechanism of the .hubris_app_table; if we
        // don't have either of these, we don't have a way of validating the
        // archive and we fail.
        //
        if let Some(imageid) = &self.imageid {
            let addr = imageid.0;
            let nbytes = imageid.1.len();
            assert!(nbytes > 0);

            let mut id = vec![0; nbytes];
            core.read_8(addr, &mut id[0..nbytes]).context(format!(
                "failed to read image ID at 0x{:x}; board mismatch?",
                addr
            ))?;

            let deltas = id
                .iter()
                .zip(imageid.1.iter())
                .filter(|&(lhs, rhs)| lhs != rhs)
                .count();

            if deltas > 0 || id.len() != imageid.1.len() {
                bail!(
                    "image ID in archive ({:x?}) does not equal \
                    ID at 0x{:x} ({:x?})",
                    imageid.1, imageid.0, id,
                );
            }
        } else if let Some(archive) = &self.apptable {
            let addr = archive.0;
            let nbytes = archive.1.len();
            assert!(nbytes > 0);

            let mut apptable = vec![0; nbytes];
            core.read_8(addr, &mut apptable[0..nbytes]).context(format!(
                "failed to read .hubris_app_table at 0x{:x}; board mismatch?",
                addr
            ))?;

            let deltas = apptable
                .iter()
                .zip(archive.1.iter())
                .filter(|&(lhs, rhs)| lhs != rhs)
                .count();

            if deltas > 0 || apptable.len() != archive.1.len() {
                bail!(
                    "apptable at 0x{:x} does not match archive apptable",
                    addr
                );
            }
        } else {
            bail!("could not find HUBRIS_IMAGE_ID or .hubris_app_table");
        }

        if criteria == HubrisValidate::ArchiveMatch {
            return Ok(());
        }

        if self.task_dump().is_some() {
            return Ok(());
        }

        let (_, n) = self.task_table(core)?;

        if n == ntasks as u32 {
            //
            // Check the CURRENT_TASK_PTR; it's non-zero iff we have booted
            //
            let ptr = core
                .read_word_32(self.lookup_symword("CURRENT_TASK_PTR")?)
                .context("failed to read CURRENT_TASK_PTR")?;

            if ptr != 0 {
                return Ok(());
            }
        }

        //
        // We're not booted -- let's see if we've panicked.
        //
        if let Some(epitaph) = self.epitaph(core)? {
            bail!("kernel has panicked on boot: {}", epitaph);
        }

        //
        // Let's see if we're actually in Reset itself -- or if we aren't in a
        // known module at all (in which case we might actually be executing in
        // the ROM).
        //
        if let Some(sym) = self.esyms_byname.get("Reset") {
            core.halt()?;

            if let Ok(pc) = core.read_reg(ARMRegister::PC) {
                core.run()?;

                if pc >= sym.0 && pc < sym.0 + sym.1 {
                    bail!("target is not yet booted (currently in Reset)");
                }

                //
                // Check against the PC not being in any known module
                //
                if self.instr_mod(pc).is_none() {
                    bail!(
                        "target does not appear booted and PC 0x{:x} is \
                        unknown; is system executing boot ROM or other \
                        program?",
                        pc
                    );
                }
            } else {
                core.run()?;
            }
        }

        bail!(
            "target does not appear to be booted and may be panicking on \
            boot; run \"humility registers -s\" for a kernel stack trace"
        );
    }

    pub fn verify(&self, core: &mut dyn crate::core::Core) -> Result<()> {
        use indicatif::{HumanBytes, HumanDuration};
        use indicatif::{ProgressBar, ProgressStyle};

        // The verification logic we use is:
        //
        // - We find program headers (PHDRs) in the post-signing combined ELF
        //   object.
        //
        // - We find the subset of those that are type PT_LOAD, meaning they
        //   contain real data that is used by the program, as opposed to (say)
        //   stack metadata.
        //
        // - We find the subset of _those_ that should be in flash, by looking
        //   for non-zero filesz, meaning they contain at least one byte of
        //   initialized data (as opposed to, say, BSS, which contains zero).
        //
        // - We check the target to see if filesz bytes starting at the PHDR's
        //   physical address (paddr) match the ELF object. The paddr is used
        //   because of the DATA section initialization image, which is loaded
        //   into flash at paddr but then copied into RAM on startup to its
        //   vaddr.
        //
        // Note that, at the time of this writing, the post-signing final ELF
        // object (final.elf) contains a _single_ PHDR. However, there's no
        // reason that needs to remain true in the future, so this code is
        // written to be general and check all PHDRs.

        // We don't use final.elf for much and don't keep it around in a
        // convenient buffer. So, go get it out of the archive.
        let cursor = Cursor::new(self.archive());
        let mut archive = zip::ZipArchive::new(cursor)?;
        let mut elf_file = archive
            .by_name("img/final.elf")
            .context("could not find final.elf in archive!")?;
        let mut file_contents = vec![];
        elf_file.read_to_end(&mut file_contents)?;

        let elf =
            Elf::parse(&file_contents).context("busted final ELF object")?;

        // First pass: Find all the PHDRs that are relevant for this algorithm.
        //
        // This vec is (paddr, expected bytes)
        let phdrs: Vec<(u32, &[u8])> = elf
            .program_headers
            .iter()
            // Only check loaded data
            .filter(|h| h.p_type == goblin::elf::program_header::PT_LOAD)
            // Only check data with initialization (not, e.g., BSS)
            .filter(|h| h.p_filesz > 0)
            .map(|h| {
                let offset = h.p_offset as usize;
                let sz = h.p_filesz as usize;
                let chunk = &file_contents[offset..offset + sz];
                (h.p_paddr as u32, chunk)
            })
            .collect();

        // Second pass: figure out how large they all are, so we can start
        // displaying a progress bar.
        let total: usize = phdrs.iter().map(|(_a, chunk)| chunk.len()).sum();

        let mut verified = 0;
        let mut buffer = vec![0; 1024];
        let mut problems = 0;

        let started = Instant::now();
        let bar = ProgressBar::new(total as u64);
        bar.set_style(
            ProgressStyle::default_bar().template(
                "humility: verifying [{bar:30}] {buffer}/{total_bytes}",
            ),
        );

        // Third and final pass: read out the actual ranges from the target and
        // see if they match!
        for (paddr, mut expected_bytes) in phdrs {
            log::info!(
                "verifying {} bytes at {:#x}",
                expected_bytes.len(),
                paddr
            );
            let mut addr = paddr;

            while !expected_bytes.is_empty() {
                let nbytes = usize::min(expected_bytes.len(), buffer.len());

                core.read_8(addr, &mut buffer[0..nbytes])?;

                #[allow(clippy::needless_range_loop)]
                for i in 0..nbytes {
                    if expected_bytes[i] != buffer[i] {
                        bar.finish_and_clear();

                        log::error!(
                            "differs at addr 0x{:x}:
                                found 0x{:x}, expected 0x{:x}",
                            addr + i as u32,
                            buffer[i],
                            expected_bytes[i]
                        );
                        problems += 1;
                    }
                }

                expected_bytes = &expected_bytes[nbytes..];
                addr += nbytes as u32;
                verified += nbytes;
                bar.set_position(verified as u64);
            }
        }

        bar.finish_and_clear();

        if problems > 0 {
            bail!("found {problems} problems!");
        } else {
            msg!(
                "verified {} in {}",
                HumanBytes(verified as u64),
                HumanDuration(started.elapsed())
            );
        }

        Ok(())
    }

    pub fn image_id_addr(&self) -> Option<u32> {
        self.imageid.as_ref().map(|i| i.0)
    }

    pub fn image_id(&self) -> Option<&[u8]> {
        self.imageid.as_ref().map(|i| i.1.as_slice())
    }

    pub fn member_offset(
        &self,
        structure: &HubrisStruct,
        member: &str,
    ) -> Result<u32> {
        let mut s = structure;
        let mut offset = 0;

        let fields: Vec<&str> = member.split('.').collect();

        for i in 0..fields.len() {
            let field = fields[i];

            let m = match s.lookup_member(field) {
                Ok(member) => member,
                _ => {
                    return Err(anyhow!("struct {} ({}) doesn't contain {}",
                        s.name, s.goff, field))
                }
            };

            offset += m.offset;

            if i == fields.len() - 1 {
                //
                // We want to make sure that this is a 32-bit basetype or a
                // ptrtype.
                //
                if let Some(v) = self.basetypes.get(&m.goff) {
                    if v.size != 4 {
                        return Err(anyhow!(
                            "expected {} in struct {} ({}) to \
                            be 4 bytes, found to be {} bytes",
                            member, structure.name, structure.goff, v.size
                        ));
                    }
                } else if self.ptrtypes.contains_key(&m.goff) {
                    break;
                } else {
                    return Err(anyhow!(
                        "expected {} in struct {} ({}) to \
                        be 4 byte type, found to be {}",
                        member, structure.name, structure.goff, m.goff
                    ));
                }

                break;
            }

            //
            // We need to descend -- make sure that this is a structure!
            //
            s = match self.lookup_struct(m.goff) {
                Ok(structure) => structure,
                Err(_) => {
                    return Err(anyhow!(
                        "struct {} ({}) doesn't contain {}: \
                        non-structure at {} ({})",
                        structure.name, structure.goff, member, field, m.goff
                    ));
                }
            }
        }

        Ok(offset as u32)
    }

    //
    // Returns a vector of all region descriptor addresses for all tasks.
    //
    fn task_region_descs(
        &self,
        core: &mut dyn crate::core::Core,
    ) -> Result<Vec<Vec<u32>>> {
        let mut rval = vec![];

        //
        // If we have HUBRIS_TASK_DESCS and HUBRIS_REGION_DESCS (that is,
        // if our kernel post-dates the addition of those variables), we
        // would much rather get our region descriptors there:  if the
        // flash and RAM don't match -- e.g., because of a missing reset --
        // this will at least be self-consistent.  If we don't have these
        // variables, we will go fishing off the task structures in RAM,
        // and hope for the best.
        //
        match (
            self.lookup_variable("HUBRIS_TASK_DESCS"),
            self.lookup_variable("HUBRIS_REGION_DESCS"),
        ) {
            (Ok(tdescs), Ok(rdescs)) => {
                let tdesc = self.lookup_struct_byname("TaskDesc")?;
                let rdesc = self.lookup_struct_byname("RegionDesc")?;
                let regions = tdesc.lookup_member("regions")?;
                let roffs = regions.offset;

                //
                // We expect this to be an array of either indices or references
                // into the RegionDesc table
                //
                let (count, size) = match self.lookup_type(regions.goff)? {
                    HubrisType::Array(a) => {
                        let size = self.lookup_type(a.goff)?.size(self)?;
                        if size != 1 && size != 4 {
                            bail!("expected array of single-byte indices \
                                   or references for TaskDesc.regions");
                        }
                        (a.count, size)
                    }
                    _ => {
                        bail!("expected array for TaskDesc.regions");
                    }
                };

                let mut indices: Vec<u8> = vec![];
                indices.resize_with(count * size, Default::default);

                for i in 0..self.ntasks() {
                    let mut r = vec![];

                    let taddr = tdescs.addr + ((i * tdesc.size) + roffs) as u32;

                    if taddr + count as u32 > tdescs.addr + tdescs.size as u32 {
                        bail!("task {} has bad regions addr 0x{:x}", i, taddr);
                    }

                    core.read_8(taddr, &mut indices).context(format!(
                        "failed to read region desriptors for task {} at 0x{:x}",
                        i, taddr)
                    )?;

                    if size == 1 {
                        for ndx in &indices {
                            let ndx = *ndx as usize;

                            if ndx == 0 {
                                continue;
                            }

                            if ndx * rdesc.size > rdescs.size {
                                bail!("task {i} has bad region index {ndx}");
                            }

                            r.push(rdescs.addr + (ndx * rdesc.size) as u32);
                        }
                    } else if size == 4 {
                        for ndx in indices.chunks(4) {
                            let ndx =
                                u32::from_le_bytes(ndx.try_into().unwrap());

                            // Check that the reference is properly aligned for
                            // the RegionDesc type.
                            let offset = ndx - rdescs.addr;
                            if offset as usize % rdesc.size != 0 {
                                bail!("task {i} has misaligned reference at \
                                      {ndx:#x}");
                            }
                            r.push(ndx);
                        }
                    } else {
                        panic!("Invalid size: {size}");
                    }

                    rval.push(r);
                }
            }

            (_, _) => {
                let task = self.lookup_struct_byname("Task")?;
                let (base, _) = self.task_table(core)?;
                let poffs =
                    self.member_offset(task, "region_table.data_ptr")?;
                let loffs = self.member_offset(task, "region_table.length")?;

                for i in 0..self.ntasks() {
                    let mut r = vec![];

                    let addr = base + i as u32 * task.size as u32;
                    let ptr = core.read_word_32(addr + poffs)?;
                    let len = core.read_word_32(addr + loffs)?;

                    for j in 0..len {
                        r.push(core.read_word_32(ptr + j * 4)?);
                    }

                    rval.push(r);
                }
            }
        }

        Ok(rval)
    }

    pub fn regions(
        &self,
        core: &mut dyn crate::core::Core,
    ) -> Result<BTreeMap<u32, HubrisRegion>> {
        let desc = self.lookup_struct_byname("RegionDesc")?;

        let base_offs = self.member_offset(desc, "base")?;
        let size_offs = self.member_offset(desc, "size")?;

        // bitflags 1.x versus 2.x encode bitfields differently
        let attr_offs = self
            .member_offset(desc, "attributes.bits")
            .or_else(|_| self.member_offset(desc, "attributes.__0"))
            .context("could not find attributes.bits")?;

        //
        // Regrettably copied out of Hubris -- there isn't DWARF for this.
        //
        const READ: u32 = 1 << 0;
        const WRITE: u32 = 1 << 1;
        const EXECUTE: u32 = 1 << 2;
        const DEVICE: u32 = 1 << 3;
        const DMA: u32 = 1 << 4;

        let mut regions: BTreeMap<u32, HubrisRegion> = BTreeMap::new();

        //
        // Add our loaded kernel regions, which don't otherwise have
        // descriptors.
        //
        for region in self.loaded.values() {
            if region.tasks[0] == HubrisTask::Kernel {
                regions.insert(region.base, region.clone());
            }
        }

        //
        // Add a region for our kernel heap+bss, for which we don't have a
        // descriptor.
        //
        for module in
            self.modules.values().filter(|m| m.task == HubrisTask::Kernel)
        {
            if let (Some(sheapbss), Some(eheapbss)) = module.heapbss {
                regions.insert(
                    sheapbss,
                    HubrisRegion {
                        daddr: None,
                        base: sheapbss,
                        size: eheapbss - sheapbss,
                        attr: HubrisRegionAttr {
                            read: true,
                            write: true,
                            execute: false,
                            device: false,
                            dma: false,
                            external: false,
                        },
                        tasks: vec![HubrisTask::Kernel],
                    },
                );
            }
        }

        //
        // Iterate over all tasks, reading their region descriptors.
        //
        for (i, daddrs) in self.task_region_descs(core)?.iter().enumerate() {
            for daddr in daddrs {
                let base = core.read_word_32(daddr + base_offs)?;
                let size = core.read_word_32(daddr + size_offs)?;
                let attr = core.read_word_32(daddr + attr_offs)?;

                if base == 0 {
                    continue;
                }

                let task = HubrisTask::Task(i as u32);
                let dma = attr & DMA != 0;

                let region = HubrisRegion {
                    daddr: Some(*daddr),
                    base,
                    size,
                    attr: HubrisRegionAttr {
                        read: attr & READ != 0,
                        write: attr & WRITE != 0,
                        execute: attr & EXECUTE != 0,
                        device: attr & DEVICE != 0,
                        dma,
                        external: self.extern_regions.external(base, task, dma),
                    },
                    tasks: vec![HubrisTask::Task(i as u32)],
                };

                if let Some(existing) = regions.get_mut(&base) {
                    existing.tasks.push(HubrisTask::Task(i as u32));
                } else {
                    regions.insert(base, region);
                }
            }
        }

        Ok(regions)
    }

    pub fn dump_registers(&self) -> HashMap<ARMRegister, u32> {
        self.registers.clone()
    }

    pub fn registers(
        &self,
        core: &mut dyn crate::core::Core,
        t: HubrisTask,
    ) -> Result<BTreeMap<ARMRegister, u32>> {
        let (base, _) = self.task_table(core)?;
        let cur = self.current_task(core)?;

        let module = self.lookup_module(t)?;
        let mut rval = BTreeMap::new();

        let ndx = match module.task {
            HubrisTask::Task(ndx) => ndx,
            _ => {
                bail!("must provide a user task")
            }
        };

        let task = self.lookup_struct_byname("Task")?;
        let save = task.lookup_member("save")?.offset as u32;
        let state = self.lookup_struct_byname("SavedState")?;

        let mut regs: Vec<u8> = vec![];
        regs.resize_with(state.size, Default::default);

        let offset = base + (ndx * task.size as u32) + save;
        core.read_8(offset, regs.as_mut_slice())?;

        //
        // If this is the current task, we want to pull the current PC.
        //
        if cur == Some(t) {
            let pc = core.read_reg(ARMRegister::PC)?;

            //
            // If the PC falls within the task, then we are at user-level,
            // and we should take our register state directly rather than
            // from the stack.
            //
            let userland =
                if let Some(module) = self.modules.range(..=pc).next_back() {
                    pc < *module.0 + module.1.textsize && module.1.task == t
                } else {
                    false
                };

            if userland {
                for i in 0..=31 {
                    let reg = match ARMRegister::from_u16(i) {
                        Some(r) => r,
                        None => {
                            continue;
                        }
                    };

                    let val = core.read_reg(reg)?;
                    rval.insert(reg, val);
                }

                return Ok(rval);
            }
        };

        let readreg = |rname| -> Result<u32> {
            let o = state.lookup_member(rname)?.offset;
            Ok(u32::from_le_bytes(regs[o..o + 4].try_into().unwrap()))
        };

        //
        // R4-R11 are found in the structure.
        //
        for r in 4..=11 {
            let rname = format!("r{}", r);
            let o = state.lookup_member(&rname)?.offset;
            let val = u32::from_le_bytes(regs[o..o + 4].try_into().unwrap());

            rval.insert(ARMRegister::from_usize(r).unwrap(), val);
        }

        let sp = readreg("psp")?;

        const NREGS_CORE: usize = 8;

        let mut stack: Vec<u8> = vec![];
        stack.resize_with(NREGS_CORE * 4, Default::default);
        core.read_8(sp, stack.as_mut_slice())?;

        //
        // R0-R3, and then R12, LR and the PSR are found on the stack
        //
        for r in 0..NREGS_CORE {
            let o = r * 4;
            let val = u32::from_le_bytes(stack[o..o + 4].try_into().unwrap());

            let reg = match r {
                0..=3 => ARMRegister::from_usize(r).unwrap(),
                4 => ARMRegister::R12,
                5 => ARMRegister::LR,
                6 => ARMRegister::PC,
                7 => ARMRegister::PSR,
                _ => panic!("bad register value"),
            };

            rval.insert(reg, val);
        }

        //
        // Not all architectures have floating point -- and ARMv6 never has
        // it.  (Note that that the FP contents pushed onto the stack is
        // always 8-byte aligned; if we have our 17 floating point registers
        // here, we also have an unstored pad.)
        //
        let (nregs_fp, align) =
            if self.manifest.target.as_ref().unwrap() == "thumbv6m-none-eabi" {
                (0, 0)
            } else {
                (17, 1)
            };

        let nregs_frame: usize = NREGS_CORE + nregs_fp + align;

        //
        // We manually adjust our stack pointer to peel off the entire frame,
        // plus any needed re-alignment.
        //
        let adjust = (nregs_frame as u32) * 4
            + humility_arch_arm::exception_stack_realign(&rval);

        rval.insert(ARMRegister::SP, sp + adjust);

        Ok(rval)
    }

    pub fn stack(
        &self,
        core: &mut dyn crate::core::Core,
        task: HubrisTask,
        limit: u32,
        regs: &BTreeMap<ARMRegister, u32>,
    ) -> Result<Vec<HubrisStackFrame>> {
        let regions = self.regions(core)?;
        let sp = regs
            .get(&ARMRegister::SP)
            .ok_or_else(|| anyhow!("SP missing from regs map"))?;
        let pc = regs
            .get(&ARMRegister::PC)
            .ok_or_else(|| anyhow!("PC missing from regs map"))?;

        let mut rval: Vec<HubrisStackFrame> = Vec::new();
        let mut frameregs = regs.clone();

        //
        // First, find the region that contains our stack pointer.  We want
        // to read that entire region.
        //
        let (_, region) = regions.range(..=sp).last().ok_or_else(|| {
            anyhow!("could not find memory region containing sp 0x{:x}", sp)
        })?;

        let mut buf: Vec<u8> = vec![];
        buf.resize_with(region.size as usize, Default::default);
        core.read_8(region.base, buf.as_mut_slice())?;

        let readval = |addr| {
            if addr < region.base {
                bail!("address (0x{:x}) below range ({:x?})", addr, region);
            }

            if addr + 4 > region.base + region.size {
                bail!("address (0x{:x}) above range ({:x?})", addr, region);
            }

            let o = (addr - region.base) as usize;
            Ok(u32::from_le_bytes(buf[o..o + 4].try_into().unwrap()))
        };

        //
        // If our PC is in a system call (highly likely), we need to determine
        // what has been pushed on our stack via asm!().
        //
        if let Some(Some(pushed)) = self.syscall_pushes.get(pc) {
            for (i, &p) in pushed.iter().enumerate() {
                let val = readval(sp + (i * 4) as u32)?;
                frameregs.insert(p, val);
            }

            frameregs.insert(ARMRegister::SP, sp + (pushed.len() * 4) as u32);
        }

        let frames = self
            .frames
            .get(&task)
            .ok_or_else(|| anyhow!("task {:?} not present in image", task))?;
        let frame = gimli::DebugFrame::new(frames, gimli::LittleEndian);

        let mut prev = None;

        loop {
            let bases = gimli::BaseAddresses::default();
            let mut ctx = gimli::UninitializedUnwindContext::new();
            let pc = *frameregs.get(&ARMRegister::PC).unwrap();

            //
            // Now we want to iterate up our frames
            //
            let unwind_info = frame.unwind_info_for_address(
                &bases,
                &mut ctx,
                pc as u64,
                gimli::DebugFrame::cie_from_offset,
            )?;

            //
            // Determine the CFA (Canonical Frame Address)
            //
            let cfa = match unwind_info.cfa() {
                gimli::CfaRule::RegisterAndOffset { register, offset } => {
                    if let Some(reg) = ARMRegister::from_u16(register.0) {
                        *frameregs.get(&reg).unwrap() + *offset as u32
                    } else {
                        // A register we don't model -- that's OK.
                        continue;
                    }
                }
                _ => {
                    panic!("unimplemented CFA rule");
                }
            };

            //
            // Now iterate over all of our register rules to transform
            // our registers.
            //
            for (register, rule) in unwind_info.registers() {
                let val = match rule {
                    gimli::RegisterRule::Offset(offset) => readval(
                        (i64::from(cfa) + offset) as u32,
                    )
                    .with_context(|| {
                        format!(
                            "failed to read cfa 0x{:x}, offset 0x{:x}: {:x?}",
                            cfa, offset, rval
                        )
                    })?,
                    _ => {
                        panic!("unimplemented register rule");
                    }
                };

                if let Some(reg) = ARMRegister::from_u16(register.0) {
                    frameregs.insert(reg, val);
                } else {
                    // Skip register we don't model.
                    continue;
                }
            }

            frameregs.insert(ARMRegister::SP, cfa);

            //
            // Lookup the DWARF symbol associated with our PC
            //
            let sym = match self.dsyms.range(..=pc).next_back() {
                Some((addr, sym)) if pc < *addr + sym.size => {
                    Some(HubrisStackSymbol {
                        addr: sym.addr,
                        name: &sym.demangled_name,
                        goff: Some(sym.goff),
                    })
                }
                _ => match self.esyms.range(..=pc).next_back() {
                    Some((addr, (name, len))) if pc < *addr + *len => {
                        Some(HubrisStackSymbol {
                            addr: *addr,
                            name,
                            goff: None,
                        })
                    }
                    _ => None,
                },
            };

            //
            // Determine if there is, in fact, an inlined stack here.
            //
            let inlined = match &sym {
                Some(sym) => {
                    let mut inlined = self.instr_inlined(pc, sym.addr);
                    inlined.reverse();
                    Some(inlined)
                }
                None => None,
            };

            //
            // Our frame is complete -- push it and continue!
            //
            rval.push(HubrisStackFrame {
                cfa,
                sym,
                inlined,
                registers: frameregs.clone(),
            });

            let lr = *frameregs.get(&ARMRegister::LR).unwrap();

            //
            // If this is a kernel stack and we have hit an EXC_RETURN, we're
            // done.
            //
            if task == HubrisTask::Kernel && (lr >> 28 == 0xf) {
                break;
            }

            //
            // Make sure that the low (Thumb) bit is clear
            //
            let lr = lr & !1;

            frameregs.insert(ARMRegister::PC, lr);

            if cfa >= limit {
                break;
            }

            if let Some(prev) = prev {
                if prev == cfa {
                    //
                    // Our previous frame and our next frame are the same;
                    // break out.
                    //
                    break;
                }
            }

            prev = Some(cfa);
        }

        Ok(rval)
    }

    pub fn typesize(&self, goff: HubrisGoff) -> Result<usize> {
        if let Some(v) = self.structs.get(&goff) {
            return Ok(v.size);
        }

        if let Some(v) = self.basetypes.get(&goff) {
            return Ok(v.size);
        }

        if self.ptrtypes.contains_key(&goff) {
            return Ok(4);
        }

        if let Some(v) = self.enums.get(&goff) {
            return Ok(v.size);
        }

        if let Some(union) = self.unions.get(&goff) {
            return Ok(union.size);
        }

        if let Some(v) = self.arrays.get(&goff) {
            return Ok(self.typesize(v.goff)? * v.count);
        }

        Err(anyhow!("unknown size for type {}", goff))
    }

    pub fn hubpack_serialized_maxsize(
        &self,
        goff: HubrisGoff,
    ) -> Result<usize> {
        if let Some(v) = self.structs.get(&goff) {
            let mut total = 0;
            for m in &v.members {
                total += self.hubpack_serialized_maxsize(m.goff)?;
            }
            return Ok(total);
        }

        if let Some(v) = self.basetypes.get(&goff) {
            return Ok(v.size);
        }

        if let Some(v) = self.enums.get(&goff) {
            let mut max = 0; // largest variant
            for variant in &v.variants {
                if let Some(goff) = variant.goff {
                    max = max.max(self.hubpack_serialized_maxsize(goff)?);
                }
            }
            return Ok(max + 1); // the extra byte is for the hubpack tag
        }

        if let Some(v) = self.arrays.get(&goff) {
            return Ok(self.hubpack_serialized_maxsize(v.goff)? * v.count);
        }

        Err(anyhow!("unknown size for type {}", goff))
    }

    pub fn printfmt(
        &self,
        buf: &[u8],
        goff: HubrisGoff,
        fmt: HubrisPrintFormat,
    ) -> Result<String> {
        use crate::reflect::Format;
        let mut rval = vec![];
        crate::reflect::load_value(
            self,
            buf,
            self.lookup_type(goff).unwrap(),
            0,
        )?
        .format(self, fmt, &mut rval)?;

        let out = std::str::from_utf8(&rval)?.to_owned();
        Ok(out)
    }

    pub fn print(&self, buf: &[u8], goff: HubrisGoff) -> Result<String> {
        self.printfmt(
            buf,
            goff,
            HubrisPrintFormat { hex: true, ..HubrisPrintFormat::default() },
        )
    }

    pub fn explain(
        &self,
        regions: &BTreeMap<u32, HubrisRegion>,
        val: u32,
    ) -> Option<String> {
        //
        // Find the region for this value.
        //
        let (_, region) = regions.range(..=val).next_back()?;
        let offset = val - region.base;

        if offset > region.size {
            return None;
        }

        Some(if region.attr.device {
            if let Some(p) = self.lookup_peripheral_byaddr(region.base) {
                format!("[{}]+0x{:x}", p, offset)
            } else {
                format!("[0x{:x}]+0x{:x}", region.base, offset)
            }
        } else if region.tasks.len() != 1 {
            format!("<{:x?}>", region)
        } else if let Some(sval) = self.instr_sym(val) {
            format!(
                "{}: {}+0x{:x}",
                self.lookup_module(region.tasks[0]).ok()?.name,
                sval.0,
                val - sval.1
            )
        } else {
            format!(
                "{}: 0x{:x}+0x{:x}",
                self.lookup_module(region.tasks[0]).ok()?.name,
                region.base,
                offset
            )
        })
    }

    /// Returns a set of `(start, size)` dump segments
    pub fn dump_segments(
        &self,
        core: &mut dyn crate::core::Core,
        task: Option<DumpTask>,
        include_nonwritable: bool,
    ) -> Result<Vec<(u32, u32)>> {
        let regions = self.regions(core)?;

        Ok(match task {
            None => regions
                .values()
                .filter(|&r| !r.attr.device && !r.attr.external)
                .filter(|&r| include_nonwritable || r.attr.write)
                .map(|r| (r.base, r.size))
                .collect::<Vec<_>>(),
            Some(task) => {
                let t = HubrisTask::Task(task.id as u32);

                //
                // In an act of selfless charity, we want to accommodate
                // running on a Hubris that predates the functionality to
                // allow for Jefe to dump DMA regions for a task -- which we
                // approximate by the absence of memory metadata.
                //
                let exclude_dma =
                    matches!(self.extern_regions, ExternRegions::ByTask(_));

                let mut segments = regions
                    .values()
                    .filter(|&r| !r.attr.device && !r.attr.external)
                    .filter(|&r| !exclude_dma || !r.attr.dma)
                    .filter(|&r| {
                        r.tasks.contains(&t)
                            || (include_nonwritable && !r.attr.write)
                    })
                    .map(|r| (r.base, r.size))
                    .collect::<Vec<_>>();

                let (base, _) = self.task_table(core)?;
                let ndx = task.id as usize;
                let task_t = self.lookup_struct_byname("Task")?;
                segments.push((
                    base + (ndx * task_t.size) as u32,
                    task_t.size as u32,
                ));

                segments
            }
        })
    }

    pub fn dump(
        &self,
        core: &mut dyn crate::core::Core,
        task: Option<DumpTask>,
        dumpfile: Option<&str>,
        started: Option<Instant>,
    ) -> Result<()> {
        use indicatif::{HumanBytes, HumanDuration};
        use indicatif::{ProgressBar, ProgressStyle};
        use std::io::Write;

        let segments = self.dump_segments(core, task, true)?;
        let nsegs = segments.len();

        macro_rules! pad {
            ($size:expr) => {
                ((4 - ($size & 0b11)) & 0b11) as u32
            };
        }

        let pad = [0u8; 4];

        let ctx = goblin::container::Ctx::new(
            goblin::container::Container::Little,
            goblin::container::Endian::Little,
        );

        let oxide = String::from(OXIDE_NT_NAME);

        let notesz = |note: &goblin::elf::note::Nhdr32| {
            size_of::<goblin::elf::note::Nhdr32>() as u32
                + note.n_namesz
                + pad!(note.n_namesz)
                + note.n_descsz
                + pad!(note.n_descsz)
        };

        let mut notes = vec![];
        let mut regs = vec![];

        match task {
            Some(_) => {
                notes.push(goblin::elf::note::Nhdr32 {
                    n_namesz: (oxide.len() + 1) as u32,
                    n_descsz: std::mem::size_of::<DumpTask>() as u32,
                    n_type: OXIDE_NT_HUBRIS_TASK,
                });
            }

            None => {
                for i in 0..31 {
                    if let Some(reg) = ARMRegister::from_u16(i) {
                        let val = core.read_reg(reg)?;
                        regs.push((i, val));
                    }
                }

                notes.push(goblin::elf::note::Nhdr32 {
                    n_namesz: (oxide.len() + 1) as u32,
                    n_descsz: regs.len() as u32 * 8,
                    n_type: OXIDE_NT_HUBRIS_REGISTERS,
                });
            }
        }

        notes.push(goblin::elf::note::Nhdr32 {
            n_namesz: (oxide.len() + 1) as u32,
            n_descsz: self.archive.len() as u32,
            n_type: OXIDE_NT_HUBRIS_ARCHIVE,
        });

        let mut header = goblin::elf::header::Header::new(ctx);
        header.e_machine = goblin::elf::header::EM_ARM;
        header.e_type = goblin::elf::header::ET_CORE;
        header.e_phoff = header.e_ehsize as u64;
        header.e_phnum = (notes.len() + nsegs) as u16;

        let mut offset = header.e_phoff as u32
            + (header.e_phentsize * header.e_phnum) as u32;

        let filename = match dumpfile {
            Some(filename) => filename.to_owned(),
            None => {
                let prefix = match task {
                    Some(task) => {
                        let t = HubrisTask::Task(task.id as u32);
                        format!("hubris.core.{}.", self.lookup_module(t)?.name)
                    }
                    None => "hubris.core.".to_string(),
                };

                (0..)
                    .map(|i| format!("{prefix}{i}"))
                    .find(|f| std::fs::File::open(f).is_err())
                    .unwrap()
            }
        };

        //
        // Write our ELF header
        //
        let mut file =
            OpenOptions::new().write(true).create_new(true).open(&filename)?;

        msg!("dumping to {filename}");

        file.iowrite_with(header, ctx)?;

        let mut bytes = [0x0u8; goblin::elf32::program_header::SIZEOF_PHDR];

        //
        // Write our program headers, starting with our note headers.
        //
        for note in &notes {
            let size = notesz(note);

            let phdr = goblin::elf32::program_header::ProgramHeader {
                p_type: goblin::elf::program_header::PT_NOTE,
                p_flags: goblin::elf::program_header::PF_R,
                p_offset: offset,
                p_filesz: size,
                ..Default::default()
            };

            bytes.pwrite_with(phdr, 0, ctx.le)?;
            file.write_all(&bytes)?;

            offset += size;
        }

        let mut total = 0;

        for (base, size) in &segments {
            let seg_phdr = goblin::elf32::program_header::ProgramHeader {
                p_type: goblin::elf::program_header::PT_LOAD,
                p_flags: goblin::elf::program_header::PF_R,
                p_offset: offset,
                p_vaddr: *base,
                p_filesz: *size,
                p_memsz: *size,
                ..Default::default()
            };

            bytes.pwrite_with(seg_phdr, 0, ctx.le)?;
            file.write_all(&bytes)?;

            offset += *size + pad!(*size);
            total += *size;
        }

        for note in &notes {
            //
            // Now write our note section, starting with our note header...
            //
            let mut bytes = [0x0u8; size_of::<goblin::elf::note::Nhdr32>()];
            bytes.pwrite_with(note, 0, ctx.le)?;
            file.write_all(&bytes)?;

            //
            // ...and our note name
            //
            let bytes = oxide.as_bytes();
            file.write_all(bytes)?;
            let npad = 1 + pad!(note.n_namesz) as usize;
            file.write_all(&pad[0..npad])?;

            //
            // ...and finally, the note itself.
            //
            match note.n_type {
                OXIDE_NT_HUBRIS_REGISTERS => {
                    let mut bytes = [0x0u8; 8];

                    for (reg, val) in regs.iter() {
                        bytes.pwrite_with(reg, 0, ctx.le)?;
                        bytes.pwrite_with(val, 4, ctx.le)?;
                        file.write_all(&bytes)?;
                    }
                }

                OXIDE_NT_HUBRIS_ARCHIVE => {
                    file.write_all(&self.archive)?;
                }

                OXIDE_NT_HUBRIS_TASK => {
                    file.write_all(task.unwrap().as_bytes())?;
                }

                _ => {
                    panic!("unimplemented note");
                }
            }

            let npad = pad!(note.n_descsz) as usize;
            file.write_all(&pad[0..npad])?;
        }

        //
        // And now we write our segments.  This takes a little while, so
        // we're going to indicate our progress as we go.
        //
        let mut written = 0;

        let started = started.unwrap_or_else(Instant::now);

        let bar = ProgressBar::new(total as u64);
        bar.set_style(
            ProgressStyle::default_bar()
                .template("humility: dumping [{bar:30}] {bytes}/{total_bytes}"),
        );

        for (base, size) in &segments {
            let mut remain = *size as usize;
            let mut bytes = vec![0; 1024];
            let mut addr = *base;

            while remain > 0 {
                let nbytes =
                    if remain > bytes.len() { bytes.len() } else { remain };

                core.read_8(addr, &mut bytes[0..nbytes])?;
                file.write_all(&bytes[0..nbytes])?;
                remain -= nbytes;
                written += nbytes;
                addr += nbytes as u32;
                bar.set_position(written as u64);
            }

            let npad = pad!(*size) as usize;
            file.write_all(&pad[0..npad])?;
        }

        bar.finish_and_clear();

        msg!(
            "dumped {} in {}",
            HumanBytes(written as u64),
            HumanDuration(started.elapsed())
        );

        Ok(())
    }

    pub fn extract_file_to(&self, filename: &str, target: &Path) -> Result<()> {
        let cursor = Cursor::new(self.archive.as_slice());
        let mut archive = zip::ZipArchive::new(cursor)?;
        let mut file = archive
            .by_name(filename)
            .map_err(|e| anyhow!("failed to find '{}': {}", filename, e))?;
        let mut buffer = Vec::new();
        file.read_to_end(&mut buffer)?;

        std::fs::write(target, &buffer).map_err(Into::into)
    }

    /// Copies the kernel and every task ELF file to the given directory.
    pub fn extract_elfs_to(&self, p: &Path) -> Result<()> {
        self.extract_file_to("elf/kernel", &p.join("kernel"))?;

        // `stage0` is only present in some cases, so we ignore errors here
        let _ = self.extract_file_to("img/stage0", &p.join("stage0"));

        let cursor = Cursor::new(self.archive.as_slice());
        let mut archive = zip::ZipArchive::new(cursor)?;
        Self::for_each_task(&mut archive, |path, buffer| {
            let file_name = p.join(path.file_name().unwrap());
            std::fs::write(file_name, buffer)?;
            Ok(())
        })?;
        Ok(())
    }

    pub fn lookup_feature(&self, feature: &str) -> Result<Vec<HubrisTask>> {
        let mut rval = vec![];

        ensure!(
            !self.modules.is_empty(),
            "Hubris archive required specify a task feature"
        );

        for module in self.modules.values() {
            if module.task == HubrisTask::Kernel {
                continue;
            }

            if let Some(features) =
                self.manifest.task_features.get(&module.name)
            {
                for f in features {
                    if f == feature {
                        rval.push(module.task);
                    }
                }
            }
        }

        Ok(rval)
    }

    pub fn does_task_have_feature(
        &self,
        task: HubrisTask,
        feature: &str,
    ) -> Result<bool> {
        let name = &self.lookup_module(task)?.name;
        Ok(self
            .manifest
            .task_features
            .get(name)
            .map(|f| f.contains(&feature.to_string()))
            .unwrap_or(false))
    }

    pub fn lookup_peripheral(&self, name: &str) -> Result<u32> {
        ensure!(
            !self.modules.is_empty(),
            "Hubris archive required to specify a peripheral"
        );

        if let Some(addr) = self.manifest.peripherals.get(name) {
            Ok(*addr)
        } else {
            let peripherals: Vec<&str> =
                self.manifest.peripherals.keys().map(String::as_str).collect();

            bail!("{} does not correspond to a peripheral; \
                expected one of: {}", name, peripherals.join(", "));
        }
    }

    pub fn lookup_peripheral_byaddr(&self, addr: u32) -> Option<&String> {
        self.manifest.peripherals_byaddr.get(&addr)
    }

    pub fn lookup_external_byaddr(&self, addr: u32) -> Option<&String> {
        self.extern_regions.lookup_byaddr(addr)
    }

    pub fn lookup_i2c_bus(&self, bus: &str) -> Result<&HubrisI2cBus> {
        self.manifest
            .i2c_buses
            .iter()
            .find(|&b| b.name == Some(bus.to_string()))
            .ok_or_else(|| anyhow!("couldn't find bus {}", bus))
    }

    ///
    /// For a given controller and port name, return the matching port
    /// (if any)
    pub fn lookup_i2c_port(
        &self,
        controller: u8,
        port: &str,
    ) -> Result<&HubrisI2cPort> {
        let mut found = false;

        for bus in &self.manifest.i2c_buses {
            if bus.controller != controller {
                continue;
            }

            found = true;

            if bus.port.name.eq_ignore_ascii_case(port) {
                return Ok(&bus.port);
            }
        }

        if !found {
            bail!("unknown I2C controller {}", controller);
        }

        let ports = self
            .manifest
            .i2c_buses
            .iter()
            .filter(|bus| bus.controller == controller)
            .map(|bus| bus.port.name.clone())
            .collect::<Vec<_>>()
            .join(", ");

        bail!("invalid port \"{}\" (must be one of: {})", port, ports);
    }

    ///
    /// For a given controller, return its port if there is only one, failing
    /// if there is more than one port.
    pub fn i2c_port(&self, controller: u8) -> Result<&HubrisI2cPort> {
        let found = self
            .manifest
            .i2c_buses
            .iter()
            .filter(|bus| bus.controller == controller)
            .collect::<Vec<_>>();

        if found.is_empty() {
            bail!("unknown I2C controller {}", controller);
        } else if found.len() == 1 {
            Ok(&found[0].port)
        } else {
            let ports = found
                .iter()
                .map(|bus| bus.port.name.clone())
                .collect::<Vec<_>>()
                .join(", ");

            bail!(
                "I2C{} has multiple ports; expected one of: {}",
                controller, ports
            )
        }
    }

    pub fn clock(
        &self,
        core: &mut dyn crate::core::Core,
    ) -> Result<Option<u32>> {
        let name = "CLOCK_FREQ_KHZ";

        log::trace!("determining clock requency via {}", name);

        match self.variables.get(name) {
            Some(variable) => {
                if variable.size != 4 {
                    Err(anyhow!(
                        "{} has wrong size (expected 4, found {})",
                        name, variable.size
                    ))
                } else {
                    let clock = core.read_word_32(variable.addr)?;

                    if clock == 0 {
                        Err(anyhow!(
                            "clock is reading 0; target may not be booted?"
                        ))
                    } else {
                        Ok(Some(clock))
                    }
                }
            }

            None => Ok(None),
        }
    }

    pub fn archive(&self) -> &[u8] {
        &self.archive
    }

    pub fn apptable(&self) -> Option<&[u8]> {
        match &self.apptable {
            None => None,
            Some(apptable) => Some(&apptable.1),
        }
    }

    pub fn unhalted_reads(&self) -> bool {
        if let Some(ref target) = self.manifest.target {
            target != "thumbv6m-none-eabi"
        } else {
            false
        }
    }

    /// Reads the auxiliary flash data from a Hubris archive
    ///
    /// Returns `Ok(Some(...))` if the data is loaded, `Ok(None)` if the file
    /// is missing, or `Err(...)` if a zip file error occurred.
    pub fn read_file(&self, name: &str) -> Result<Option<Vec<u8>>> {
        let archive = self.archive();
        let cursor = Cursor::new(archive);
        let mut archive = zip::ZipArchive::new(cursor)?;
        let file = archive.by_name(name);
        match file {
            Ok(mut f) => {
                let mut buffer = Vec::new();
                f.read_to_end(&mut buffer)?;
                Ok(Some(buffer))
            }
            Err(zip::result::ZipError::FileNotFound) => Ok(None),
            Err(e) => bail!("Failed to extract {}: {}", name, e),
        }
    }

    /// Reads the auxiliary flash data from a Hubris archive
    pub fn read_auxflash_data(&self) -> Result<Option<Vec<u8>>> {
        self.read_file("img/auxi.tlvc")
    }

    /// Determine if two types conclusively differ from one another, performing
    /// a deep comparison.
    pub fn differ(&self, lhs: HubrisGoff, rhs: HubrisGoff) -> Result<bool> {
        let lhs = self.lookup_type(lhs)?;
        let rhs = self.lookup_type(rhs)?;
        match (lhs, rhs) {
            (HubrisType::Base(lt), HubrisType::Base(rt)) => {
                Ok(lt.encoding != rt.encoding || lt.size != rt.size)
            }
            (HubrisType::Struct(lt), HubrisType::Struct(rt)) => {
                lt.differs(self, rt)
            }
            (HubrisType::Enum(lt), HubrisType::Enum(rt)) => {
                lt.differs(self, rt)
            }
            (HubrisType::Array(lt), HubrisType::Array(rt)) => {
                lt.differs(self, rt)
            }
            (HubrisType::Union(lt), HubrisType::Union(rt)) => {
                lt.differs(self, rt)
            }
            (HubrisType::Ptr(l), HubrisType::Ptr(r)) => self.differ(l, r),
            _ => Ok(true),
        }
    }
}

/// Loader for a single ELF file
///
/// This duplicates many member variables from `HubrisArchive`; this `struct` is
/// meant to be used for parallel loading, after which point everything is
/// merged using `HubrisArchive::merge`.
struct HubrisObjectLoader {
    current: u32,

    // image ID
    imageid: Option<(u32, Vec<u8>)>,

    // ELF symbols: name to value/length
    esyms_byname: MultiMap<String, (u32, u32)>,

    // ELF symbols: address to name/length tuple
    esyms: BTreeMap<u32, (String, u32)>,

    // Tasks: name of task to ID
    tasks: HashMap<String, HubrisTask>,

    // Modules: text address to module
    modules: BTreeMap<u32, HubrisModule>,

    // DWARF call frame debugging sections: task to raw bytes
    frames: HashMap<HubrisTask, Vec<u8>>,

    // loaded regions
    loaded: BTreeMap<u32, HubrisRegion>,

    // app table
    apptable: Option<(u32, Vec<u8>)>,

    // Instructions: address to bytes/target tuple. The target will be None if
    // the instruction did not decode as some kind of jump/branch/call.
    instrs: HashMap<u32, (Vec<u8>, Option<HubrisTarget>)>,

    // Manual stack pushes before a syscall
    syscall_pushes: HashMap<u32, Option<Vec<ARMRegister>>>,

    // Unions: goff to union
    unions: HashMap<HubrisGoff, HubrisUnion>,

    // Enums: goff to enum
    enums: HashMap<HubrisGoff, HubrisEnum>,

    // Structures: goff to struct
    structs: HashMap<HubrisGoff, HubrisStruct>,

    // DWARF source code: goff to file/line
    src: HashMap<HubrisGoff, HubrisSrc>,

    // Enums: name to goff
    enums_byname: MultiMap<String, HubrisGoff>,

    // Structures: name to goff
    structs_byname: MultiMap<String, HubrisGoff>,

    // Arrays: goff to array
    arrays: HashMap<HubrisGoff, HubrisArray>,

    // Base types: goff to size
    basetypes: HashMap<HubrisGoff, HubrisBasetype>,

    // Base types: name to goff
    basetypes_byname: HashMap<String, HubrisGoff>,

    // Base types: goff to underlying type
    ptrtypes: HashMap<HubrisGoff, (String, HubrisGoff)>,

    // Space of all namespaces -- but namespacesspace seems needlessly cruel
    namespaces: Namespaces,

    // Inlined: address/nesting tuple to length/goff/origin tuple
    inlined: BTreeMap<(u32, isize), (u32, HubrisGoff, HubrisGoff)>,

    // Subprograms: goff to demangled name
    subprograms: HashMap<HubrisGoff, String>,

    // DWARF symbols: address to HubrisSymbol
    dsyms: BTreeMap<u32, HubrisSymbol>,

    // Variables: name to goff/address/size tuple. Note that variables declared
    // in different modules will appear to have the same name here. Consider
    // using qualified_variables instead.
    variables: MultiMap<String, HubrisVariable>,

    // Qualified Variables: fully qualified Rust demangled name to
    // goff/address/size tuple.
    qualified_variables: MultiMap<String, HubrisVariable>,

    // Definitions: name to goff
    definitions: MultiMap<String, HubrisGoff>,
}

impl HubrisObjectLoader {
    fn new(current: u32) -> Result<Self> {
        Ok(Self {
            current,
            apptable: None,
            imageid: None,
            arrays: HashMap::new(),
            variables: MultiMap::new(),
            basetypes: HashMap::new(),
            basetypes_byname: HashMap::new(),
            esyms: BTreeMap::new(),
            esyms_byname: MultiMap::new(),
            tasks: HashMap::new(),
            dsyms: BTreeMap::new(),
            modules: BTreeMap::new(),
            definitions: MultiMap::new(),
            enums: HashMap::new(),
            enums_byname: MultiMap::new(),
            frames: HashMap::new(),
            inlined: BTreeMap::new(),
            instrs: HashMap::new(),
            loaded: BTreeMap::new(),
            ptrtypes: HashMap::new(),
            unions: HashMap::new(),
            namespaces: Namespaces::new(),
            qualified_variables: MultiMap::new(),
            src: HashMap::new(),
            structs: HashMap::new(),
            structs_byname: MultiMap::new(),
            subprograms: HashMap::new(),
            syscall_pushes: HashMap::new(),
        })
    }

    fn load_object(
        &mut self,
        object: &str,
        task: HubrisTask,
        buffer: &[u8],
    ) -> Result<()> {
        //
        // Initialize Capstone, being sure to specify not only our
        // architecture but also that we are disassembling Thumb-2 --
        // and (importantly) to allow M-profile instructions.
        //
        let mut cs = Capstone::new()
            .arm()
            .mode(arch::arm::ArchMode::Thumb)
            .extra_mode(std::iter::once(arch::arm::ArchExtraMode::MClass))
            .detail(true)
            .build()
            .map_err(|e| anyhow!("failed to initialize disassembler: {e:?}"))?;
        cs.set_skipdata(true).expect("failed to set skipdata");

        use goblin::elf::section_header;

        let mut heapbss = (None, None);
        let mut kstack = (None, None);

        let elf = Elf::parse(buffer).map_err(|e| {
            anyhow!("unrecognized ELF object: {}: {}", object, e)
        })?;

        let arm = elf.header.e_machine == goblin::elf::header::EM_ARM;

        if !arm {
            bail!("{} not an ARM ELF object", object);
        }

        let text = elf.section_headers.iter().find(|sh| {
            if let Some(Ok(name)) = elf.shdr_strtab.get(sh.sh_name) {
                name == ".text"
            } else {
                false
            }
        });

        let textsec = match text {
            None => {
                bail!("couldn't find text in ELF object \"{}\"", object);
            }
            Some(sec) => sec,
        };

        let allocs = elf
            .section_headers
            .iter()
            .enumerate()
            .filter(|(_, sh)| {
                (sh.sh_flags as u32) & section_header::SHF_ALLOC != 0
            })
            .map(|(ndx, _)| ndx)
            .collect::<HashSet<_>>();

        let offset = textsec.sh_offset as u32;
        let textsize = textsec.sh_size as u32;

        log::trace!("loading {} as object {}", object, self.current);

        for sym in elf.syms.iter() {
            if sym.st_name == 0 {
                continue;
            }

            let name = match elf.strtab.get(sym.st_name) {
                Some(n) => n?,
                None => {
                    bail!("bad symbol in {}: {}", object, sym.st_name);
                }
            };

            //
            // We track from the start of our BSS to the end of our heap
            //
            if name == "__sbss" {
                heapbss.0 = Some(sym.st_value as u32);
            }

            if name == "__eheap" {
                heapbss.1 = Some(sym.st_value as u32);
            }

            //
            // If this is the kernel, keep track of _stack_base and _stack_start
            // if/when we encounter them
            //
            if task == HubrisTask::Kernel
                && sym.st_shndx == section_header::SHN_ABS as usize
            {
                if name == "_stack_base" {
                    kstack.0 = Some(sym.st_value as u32);
                }

                if name == "_stack_start" {
                    kstack.1 = Some(sym.st_value as u32);
                }
            }

            //
            // If this is a zero-sized symbol or not against an allocated
            // section (e.g., .idolatry), we don't want to keep track of it.
            //
            if sym.st_size == 0 || !allocs.contains(&sym.st_shndx) {
                continue;
            }

            //
            // On ARM, we must explicitly clear the low bit of the symbol
            // table, which exists only to indicate a function that contains
            // Thumb instructions (which is of course every function on a
            // microprocessor that executes only Thumb instructions).
            //
            assert!(arm);

            let val = if sym.is_function() {
                sym.st_value as u32 & !1
            } else {
                sym.st_value as u32
            };

            let dem = format!("{:#}", demangle(name));

            //
            // The HUBRIS_IMAGE_ID is a special symbol that denotes an ID
            // more or less unique to the image.  If we are the kernel and
            // we encounter this symbol, set our ID.
            //
            if task == HubrisTask::Kernel && name == "HUBRIS_IMAGE_ID" {
                let sec = &elf.section_headers[sym.st_shndx];
                let offset = sec.sh_offset as u32;
                let o = ((val - sec.sh_addr as u32) + offset) as usize;
                let id = buffer.get(o..o + (sym.st_size as usize)).ok_or_else(
                    || anyhow!("bad offset/size for {}: {:?}", name, sym),
                )?;

                self.imageid = Some((val, id.to_vec()));
            }

            self.esyms_byname
                .insert(name.to_string(), (val, sym.st_size as u32));
            self.esyms.insert(val, (dem, sym.st_size as u32));

            if sym.is_function() {
                let o = ((val - textsec.sh_addr as u32) + offset) as usize;
                let t = buffer.get(o..o + (sym.st_size as usize)).ok_or_else(
                    || {
                        anyhow!(
                            "bad offset/size for {}: 0x{:x}, size {}",
                            name,
                            val,
                            sym.st_size
                        )
                    },
                )?;

                self.load_function(object, task, name, val, t, &cs)?;
            }
        }

        use goblin::elf::program_header::{PF_R, PF_W, PF_X};

        elf.program_headers
            .iter()
            .filter(|h| h.p_type == goblin::elf::program_header::PT_LOAD)
            .map(|h| HubrisRegion {
                daddr: None,
                base: h.p_vaddr as u32,
                size: h.p_memsz as u32,
                attr: HubrisRegionAttr {
                    read: h.p_flags & PF_R != 0,
                    write: h.p_flags & PF_W != 0,
                    execute: h.p_flags & PF_X != 0,
                    device: false,
                    dma: false,
                    external: false,
                },
                tasks: vec![task],
            })
            .for_each(|region| {
                self.loaded.insert(region.base, region);
            });

        let memsz = elf.program_headers.iter().fold(0, |ttl, hdr| {
            if hdr.p_type == goblin::elf::program_header::PT_LOAD {
                ttl + hdr.p_memsz
            } else {
                ttl
            }
        });

        if task == HubrisTask::Kernel {
            let apptable = elf.section_headers.iter().find(|sh| {
                if let Some(Ok(name)) = elf.shdr_strtab.get(sh.sh_name) {
                    name == ".hubris_app_table"
                } else {
                    false
                }
            });

            if let Some(sec) = apptable {
                let base = sec.sh_offset as usize;
                let len = sec.sh_size as usize;

                self.apptable = Some((
                    sec.sh_addr as u32,
                    buffer[base..base + len].to_vec(),
                ));
            }

            if let (Some(base), Some(start)) = kstack {
                let region = HubrisRegion {
                    daddr: None,
                    base,
                    size: start - base,
                    attr: HubrisRegionAttr {
                        read: true,
                        write: true,
                        execute: false,
                        device: false,
                        dma: false,
                        external: false,
                    },
                    tasks: vec![task],
                };

                self.loaded.insert(region.base, region);
            }
        }

        self.load_object_dwarf(buffer, &elf)
            .context(format!("{}: failed to load DWARF", object))?;

        self.load_object_frames(task, buffer, &elf)
            .context(format!("{}: failed to load debug frames", object))?;

        let iface = self.load_object_idolatry(object, buffer, &elf)?;

        self.modules.insert(
            textsec.sh_addr as u32,
            HubrisModule {
                name: String::from(object),
                object: self.current,
                textbase: (textsec.sh_addr as u32),
                textsize,
                memsize: memsz as u32,
                heapbss,
                task,
                iface,
            },
        );

        self.tasks.insert(object.to_string(), task);

        Ok(())
    }

    fn load_function(
        &mut self,
        object: &str,
        task: HubrisTask,
        func: &str,
        addr: u32,
        buffer: &[u8],
        cs: &Capstone,
    ) -> Result<()> {
        let instrs = match cs.disasm_all(buffer, addr.into()) {
            Ok(instrs) => instrs,
            Err(err) => {
                bail!(
                    "failed to disassemble {} (addr 0x{:08x}, {}): {}",
                    func,
                    addr,
                    buffer.len(),
                    err
                );
            }
        };

        let mut last: (u32, usize) = (0, 0);

        for (ndx, instr) in instrs.iter().enumerate() {
            let addr: u32 = instr.address() as u32;

            if self.instrs.contains_key(&addr) {
                //
                // This is possible because functions can have multiple symbol
                // table entries; if we have seen this instruction before,
                // we'll split.
                //
                return Ok(());
            }

            let b = instr.bytes();

            last = (addr, b.len());

            let target = self.instr_branch_target(cs, instr);
            self.instrs.insert(addr, (b.to_vec(), target));

            const ARM_INSN_SVC: u32 = arch::arm::ArmInsn::ARM_INS_SVC as u32;

            //
            // If we encounter a syscall instruction, we need to analyze
            // its containing function to determine the hand-written pushes
            // before any system call in order to be able to successfully
            // unwind the stack.
            //
            if let InsnId(ARM_INSN_SVC) = instr.id() {
                if task != HubrisTask::Kernel {
                    self.syscall_pushes.insert(
                        addr + b.len() as u32,
                        Some(presyscall_pushes(cs, &instrs[0..ndx])?),
                    );
                }
            }
        }

        //
        // Regrettably, if Capstone flies off the rails while disassembling,
        // it won't flag an error -- it will simply stop short.  Check to see
        // if we are in this case and explicitly fail.
        //
        if last.0 + last.1 as u32 != addr + buffer.len() as u32 {
            bail!(
                "short disassembly for {}: \
                stopped at 0x{:x}, expected to go to 0x{:x}",
                object,
                last.0,
                addr + buffer.len() as u32
            );
        }

        Ok(())
    }

    fn load_object_dwarf(
        &mut self,
        buffer: &[u8],
        elf: &goblin::elf::Elf,
    ) -> Result<()> {
        // Load all of the sections. This "load" operation just gets the data in
        // RAM -- since we've already loaded the Elf file, this can't fail.
        let dwarf = gimli::Dwarf::<&[u8]>::load(
            // Load the normal DWARF section(s) from our Elf image.
            |id| {
                let sec_result = elf.section_headers.iter().find(|sh| {
                    if let Some(Ok(name)) = elf.shdr_strtab.get(sh.sh_name) {
                        name == id.name()
                    } else {
                        false
                    }
                });
                if let Some(sec) = sec_result {
                    let offset = sec.sh_offset as usize;
                    let size = sec.sh_size as usize;
                    buffer.get(offset..offset + size).ok_or_else(|| {
                        anyhow!("bad offset/size for ELF section {}", id.name())
                    })
                } else {
                    Ok(&[])
                }
            },
            // We don't have a supplemental object file.
            |_| Ok(&[]),
        )?;

        // Borrow all sections wrapped in EndianSlices
        let dwarf = dwarf.borrow(|section| {
            gimli::EndianSlice::new(section, gimli::LittleEndian)
        });

        // Iterate over the compilation units.
        let mut iter = dwarf.units();
        while let Some(header) = iter.next()? {
            let unit = dwarf.unit(header)?;
            let mut entries = unit.entries();
            let mut depth = 0;
            let mut stack: Vec<HubrisGoff> = vec![];

            let mut array = None;
            let mut ns = vec![];

            while let Some((delta, entry)) = entries.next_dfs()? {
                depth += delta;

                //
                // See if our depth has become shallower than our namespace,
                // trimming it until it fits.
                //
                while let Some((_, d)) = ns.last() {
                    if depth > *d {
                        break;
                    }

                    ns.pop();
                }

                let goff = self.dwarf_goff(&unit, entry);
                self.dwarf_fileline(&dwarf, &unit, entry)?;

                if depth as usize >= stack.len() {
                    stack.push(goff);
                } else {
                    stack[depth as usize] = goff;
                }

                match entry.tag() {
                    gimli::constants::DW_TAG_namespace => {
                        self.dwarf_namespace(&dwarf, entry, depth, &mut ns)?;
                    }

                    gimli::constants::DW_TAG_inlined_subroutine => {
                        self.dwarf_inlined(&dwarf, &unit, entry, depth)?;
                    }

                    gimli::constants::DW_TAG_subprogram => {
                        self.dwarf_subprogram(&dwarf, &unit, entry)?;
                    }

                    gimli::constants::DW_TAG_variable => {
                        self.dwarf_variable(&dwarf, &unit, entry)?;
                    }

                    gimli::constants::DW_TAG_structure_type => {
                        let id = ns.last().map(|(id, _)| *id);
                        self.dwarf_struct(&dwarf, &unit, entry, id)?;

                        //
                        // We want to also treat a structure as a namespace
                        // to allow for the disambiguation of embedded
                        // structures.
                        //
                        self.dwarf_namespace(&dwarf, entry, depth, &mut ns)?;
                    }

                    gimli::constants::DW_TAG_base_type => {
                        self.dwarf_basetype(&dwarf, &unit, entry)?;
                    }

                    gimli::constants::DW_TAG_pointer_type => {
                        self.dwarf_ptrtype(&dwarf, &unit, entry)?;
                    }

                    gimli::constants::DW_TAG_array_type => {
                        let mut attrs = entry.attrs();

                        while let Some(attr) = attrs.next()? {
                            if attr.name() != gimli::constants::DW_AT_type {
                                continue;
                            }

                            array = self.dwarf_value_goff(&unit, &attr.value());
                            break;
                        }
                    }

                    gimli::constants::DW_TAG_subrange_type => {
                        if depth == 0 {
                            bail!("no array for subrange {}", goff);
                        }

                        let parent = stack[depth as usize - 1];
                        self.dwarf_array(&unit, entry, parent, array)?;
                        array = None;
                    }

                    gimli::constants::DW_TAG_enumeration_type => {
                        let id = ns.last().map(|(id, _)| *id);
                        self.dwarf_const_enum(&dwarf, &unit, entry, goff, id)?;
                    }

                    gimli::constants::DW_TAG_enumerator => {
                        let parent = stack[depth as usize - 1];

                        self.dwarf_enum_variant(&dwarf, &unit, entry, parent)?;
                    }

                    gimli::constants::DW_TAG_variant_part => {
                        if depth == 0 {
                            bail!("no enum for variant {}", goff);
                        }

                        let parent = stack[depth as usize - 1];
                        self.dwarf_enum(&unit, entry, parent)?;

                        //
                        // The discriminant is a (grand)child member; we need
                        // to duplicate our parent's goff so our child can
                        // find it.
                        //
                        stack[depth as usize] = parent;
                    }

                    gimli::constants::DW_TAG_variant => {
                        if depth == 0 {
                            bail!("no enum for variant {}", goff);
                        }

                        let parent = stack[depth as usize - 1];
                        self.dwarf_variant(&unit, entry, parent)?;

                        //
                        // Our discriminant is still below us as a child
                        // member, so, as in the DW_TAG_variant_part case
                        // (which is our parent), we need to copy our parent
                        // down.
                        //
                        stack[depth as usize] = parent;
                    }

                    gimli::constants::DW_TAG_member => {
                        if depth == 0 {
                            bail!("no parent for member {}", goff);
                        }

                        let parent = stack[depth as usize - 1];
                        self.dwarf_member(&dwarf, &unit, entry, parent)?;
                    }

                    gimli::constants::DW_TAG_union_type => {
                        self.dwarf_union(&dwarf, &unit, entry)?;
                    }

                    _ => {}
                }
            }

            if let Some(array) = array {
                bail!("missing subrange for array {}", array);
            }
        }

        Ok(())
    }

    fn load_object_frames(
        &mut self,
        task: HubrisTask,
        buffer: &[u8],
        elf: &goblin::elf::Elf,
    ) -> Result<()> {
        let id = gimli::SectionId::DebugFrame.name();

        let sh = elf
            .section_headers
            .iter()
            .find(|sh| {
                if let Some(Ok(name)) = elf.shdr_strtab.get(sh.sh_name) {
                    name == id
                } else {
                    false
                }
            })
            .ok_or_else(|| anyhow!("couldn't find {}", id))?;

        let offs = sh.sh_offset as usize;
        let size = sh.sh_size as usize;

        let buf = buffer
            .get(offs..offs + size)
            .ok_or_else(|| anyhow!("bad offset/size for ELF section {}", id))?;
        self.frames.insert(task, buf.to_vec());

        Ok(())
    }

    fn load_object_idolatry(
        &mut self,
        object: &str,
        buffer: &[u8],
        elf: &goblin::elf::Elf,
    ) -> Result<Option<Interface>> {
        //
        // If we have an idolatry definition, load that.
        //
        let idol = elf.section_headers.iter().find(|sh| {
            if let Some(Ok(name)) = elf.shdr_strtab.get(sh.sh_name) {
                name == ".idolatry"
            } else {
                false
            }
        });

        if let Some(idol) = idol {
            let offset = idol.sh_offset as usize;
            let size = idol.sh_size as usize;

            if size == 0 {
                return Ok(None);
            }

            let section = buffer
                .get(offset..offset + size)
                .ok_or_else(|| anyhow!("bad offset/size for .idolatry"))?;

            let s = str::from_utf8(section).context("bad .idolatry string")?;

            match Interface::from_str(s) {
                Ok(interface) => Ok(Some(interface)),
                Err(err) => {
                    warn!("failed to load Idol definition for {object}: {err}");
                    log::debug!("failed Idol for {object} ({err:?}): {s}");
                    Ok(None)
                }
            }
        } else {
            Ok(None)
        }
    }

    fn instr_branch_target(
        &self,
        cs: &Capstone,
        instr: &capstone::Insn,
    ) -> Option<HubrisTarget> {
        let detail = cs.insn_detail(instr).ok()?;

        let mut jump = false;
        let mut call = false;
        let mut brel = None;

        const BREL: u8 = InsnGroupType::CS_GRP_BRANCH_RELATIVE as u8;
        const JUMP: u8 = InsnGroupType::CS_GRP_JUMP as u8;
        const CALL: u8 = InsnGroupType::CS_GRP_CALL as u8;
        const ARM_REG_PC: u16 = arch::arm::ArmReg::ARM_REG_PC as u16;
        const ARM_REG_LR: u16 = arch::arm::ArmReg::ARM_REG_LR as u16;
        const ARM_INSN_POP: u32 = arch::arm::ArmInsn::ARM_INS_POP as u32;

        for g in detail.groups() {
            match g {
                InsnGroupId(BREL) => {
                    let arch = detail.arch_detail();
                    let ops = arch.operands();

                    let op = ops.last().unwrap_or_else(|| {
                        panic!("missing operand!");
                    });

                    if let arch::ArchOperand::ArmOperand(op) = op {
                        if let arch::arm::ArmOperandType::Imm(a) = op.op_type {
                            brel = Some(a as u32);
                        }
                    }
                }

                InsnGroupId(JUMP) => {
                    jump = true;
                }

                InsnGroupId(CALL) => {
                    call = true;
                }
                _ => {}
            }
        }

        if let Some(addr) = brel {
            if call {
                return Some(HubrisTarget::Call(addr));
            } else {
                return Some(HubrisTarget::Direct(addr));
            }
        }

        if call {
            return Some(HubrisTarget::IndirectCall);
        }

        //
        // If this is a JUMP that isn't a CALL, check to see if one of
        // its operands is LR -- in which case it's a return (or could be
        // a return).
        //
        if jump {
            for op in detail.arch_detail().operands() {
                if let arch::ArchOperand::ArmOperand(op) = op {
                    if let arch::arm::ArmOperandType::Reg(RegId(ARM_REG_LR)) =
                        op.op_type
                    {
                        return Some(HubrisTarget::Return);
                    }
                }
            }

            return Some(HubrisTarget::Indirect);
        }

        //
        // Capstone doesn't have a group denoting returns (they are control
        // transfers, but not considered in the JUMP group), so explicitly
        // look for a pop instruction that writes to the PC.
        //
        if let InsnId(ARM_INSN_POP) = instr.id() {
            for op in detail.arch_detail().operands() {
                if let arch::ArchOperand::ArmOperand(op) = op {
                    if let arch::arm::ArmOperandType::Reg(RegId(ARM_REG_PC)) =
                        op.op_type
                    {
                        return Some(HubrisTarget::Return);
                    }
                }
            }
        }

        None
    }

    fn dwarf_goff<R: gimli::Reader<Offset = usize>>(
        &self,
        unit: &gimli::Unit<R>,
        entry: &gimli::DebuggingInformationEntry<
            gimli::EndianSlice<gimli::LittleEndian>,
            usize,
        >,
    ) -> HubrisGoff {
        let goff = match entry.offset().to_unit_section_offset(unit) {
            gimli::UnitSectionOffset::DebugInfoOffset(o) => o.0,
            gimli::UnitSectionOffset::DebugTypesOffset(o) => o.0,
        };

        HubrisGoff { object: self.current, goff }
    }

    fn dwarf_union<R: gimli::Reader<Offset = usize>>(
        &mut self,
        dwarf: &gimli::Dwarf<gimli::EndianSlice<gimli::LittleEndian>>,
        unit: &gimli::Unit<R>,
        entry: &gimli::DebuggingInformationEntry<
            gimli::EndianSlice<gimli::LittleEndian>,
            usize,
        >,
    ) -> Result<()> {
        let mut attrs = entry.attrs();
        let goff = self.dwarf_goff(unit, entry);
        let mut size = None;
        let mut name = None;

        while let Some(attr) = attrs.next()? {
            match attr.name() {
                gimli::constants::DW_AT_name => {
                    name = dwarf_name(dwarf, attr.value());
                }

                gimli::constants::DW_AT_byte_size => {
                    if let gimli::AttributeValue::Udata(value) = attr.value() {
                        size = Some(value as usize)
                    }
                }
                _ => {}
            }
        }

        if let (Some(name), Some(size)) = (name, size) {
            self.unions.insert(
                goff,
                HubrisUnion {
                    name: name.to_string(),
                    goff,
                    size,
                    variants: Vec::new(),
                },
            );
        }

        Ok(())
    }

    fn dwarf_member<R: gimli::Reader<Offset = usize>>(
        &mut self,
        dwarf: &gimli::Dwarf<gimli::EndianSlice<gimli::LittleEndian>>,
        unit: &gimli::Unit<R>,
        entry: &gimli::DebuggingInformationEntry<
            gimli::EndianSlice<gimli::LittleEndian>,
            usize,
        >,
        parent: HubrisGoff,
    ) -> Result<()> {
        let mut attrs = entry.attrs();
        let mut name = None;
        let mut offset = None;
        let mut goff = None;
        let member = self.dwarf_goff(unit, entry);

        while let Some(attr) = attrs.next()? {
            match attr.name() {
                gimli::constants::DW_AT_name => {
                    name = dwarf_name(dwarf, attr.value());
                }

                gimli::constants::DW_AT_data_member_location => {
                    if let gimli::AttributeValue::Udata(value) = attr.value() {
                        offset = Some(value as usize)
                    }
                }

                gimli::constants::DW_AT_type => {
                    goff = self.dwarf_value_goff(unit, &attr.value());
                }

                _ => {}
            }
        }

        if let Some(pstruct) = self.structs.get_mut(&parent) {
            if let (Some(n), Some(offs), Some(g)) = (name, offset, goff) {
                pstruct.members.push(HubrisStructMember {
                    name: n.to_string(),
                    offset: offs,
                    goff: g,
                });
            } else {
                bail!("member {} is incomplete", member);
            }
        } else if let Some(union) = self.enums.get_mut(&parent) {
            if let Some(HubrisDiscriminant::Expected(expect)) =
                union.discriminant
            {
                if member != expect {
                    bail!(
                        "enum {}: expected discriminant {}, found {}",
                        union.goff,
                        expect,
                        member
                    );
                }

                if let (Some(offs), Some(g)) = (offset, goff) {
                    union.discriminant =
                        Some(HubrisDiscriminant::Value(g, offs));
                    return Ok(());
                }

                bail!("enum {}: incomplete discriminant", union.goff);
            }

            //
            // We have an enum variant; add it to our variants.
            //
            if let (Some(n), Some(offs), Some(g)) = (name, offset, goff) {
                union.variants.push(HubrisEnumVariant {
                    name: n.to_string(),
                    offset: offs,
                    goff: Some(g),
                    tag: union.tag,
                });

                union.tag = None;
            } else {
                bail!("enum variant {} is incomplete", member);
            }
        } else if let Some(union) = self.unions.get_mut(&parent) {
            //
            // This is possible because of Rust's (unsafe-only) support for
            // C-style unions.  We track these because some structures are
            // implemented in terms of them (in particular, MaybeUninit).
            //
            if let (Some(n), Some(offs), Some(g)) = (name, offset, goff) {
                union.variants.push(HubrisEnumVariant {
                    name: n.to_string(),
                    offset: offs,
                    goff: Some(g),
                    tag: None,
                });
            } else {
                bail!("union {} is incomplete", parent);
            }
        } else {
            log::trace!("no struct/enum found for {}", parent);
        }

        Ok(())
    }

    fn dwarf_value_goff<R: gimli::Reader<Offset = usize>>(
        &self,
        unit: &gimli::Unit<R>,
        value: &gimli::AttributeValue<
            gimli::EndianSlice<gimli::LittleEndian>,
            usize,
        >,
    ) -> Option<HubrisGoff> {
        let goff = match value {
            gimli::AttributeValue::UnitRef(offs) => {
                match offs.to_unit_section_offset(unit) {
                    gimli::UnitSectionOffset::DebugInfoOffset(o) => o.0,
                    gimli::UnitSectionOffset::DebugTypesOffset(o) => o.0,
                }
            }

            gimli::AttributeValue::DebugInfoRef(offs) => offs.0,

            _ => {
                return None;
            }
        };

        Some(HubrisGoff { object: self.current, goff })
    }

    fn dwarf_variant<R: gimli::Reader<Offset = usize>>(
        &mut self,
        unit: &gimli::Unit<R>,
        entry: &gimli::DebuggingInformationEntry<
            gimli::EndianSlice<gimli::LittleEndian>,
            usize,
        >,
        parent: HubrisGoff,
    ) -> Result<()> {
        let goff = self.dwarf_goff(unit, entry);
        let mut attrs = entry.attrs();
        let mut value = None;

        while let Some(attr) = attrs.next()? {
            if attr.name() == gimli::constants::DW_AT_discr_value {
                value = attr.value().udata_value().map(Tag::Unsigned);

                if value.is_none() {
                    value = attr.value().sdata_value().map(Tag::Signed);
                }

                if value.is_none() {
                    bail!("bad discriminant on union {}", parent);
                }
            }
        }

        if let Some(union) = self.enums.get_mut(&parent) {
            union.tag = value;
            Ok(())
        } else {
            Err(anyhow!("missing enum on variant {}", goff))
        }
    }

    fn dwarf_enum<R: gimli::Reader<Offset = usize>>(
        &mut self,
        unit: &gimli::Unit<R>,
        entry: &gimli::DebuggingInformationEntry<
            gimli::EndianSlice<gimli::LittleEndian>,
            usize,
        >,
        goff: HubrisGoff,
    ) -> Result<()> {
        let mut attrs = entry.attrs();
        let mut discr = None;

        //
        // If we have an enum, we need to first remove it from our
        // structures, putting back any duplicate names that isn't
        // this enum.
        //
        let union = self.structs.remove(&goff).ok_or_else(|| {
            anyhow!("goff {:?} not present in structs map", goff)
        })?;

        self.structs_byname
            .get_vec_mut(&union.name)
            .expect("structs vs structs_byname inconsistency")
            .retain(|&g| g != goff);

        while let Some(attr) = attrs.next()? {
            if attr.name() == gimli::constants::DW_AT_discr {
                discr = self.dwarf_value_goff(unit, &attr.value());
            }
        }

        self.enums.insert(
            goff,
            HubrisEnum {
                name: union.name.clone(),
                goff,
                size: union.size,
                discriminant: discr.map(HubrisDiscriminant::Expected),
                tag: None,
                variants: Vec::new(),
                namespace: union.namespace,
            },
        );

        self.enums_byname.insert(union.name, goff);

        Ok(())
    }

    fn dwarf_enum_variant<R: gimli::Reader<Offset = usize>>(
        &mut self,
        dwarf: &gimli::Dwarf<gimli::EndianSlice<gimli::LittleEndian>>,
        unit: &gimli::Unit<R>,
        entry: &gimli::DebuggingInformationEntry<
            gimli::EndianSlice<gimli::LittleEndian>,
            usize,
        >,
        parent: HubrisGoff,
    ) -> Result<()> {
        let goff = self.dwarf_goff(unit, entry);
        let mut attrs = entry.attrs();
        let mut value = None;
        let mut name = None;

        while let Some(attr) = attrs.next()? {
            match attr.name() {
                gimli::constants::DW_AT_name => {
                    name = dwarf_name(dwarf, attr.value());
                }

                gimli::constants::DW_AT_const_value => {
                    value = attr.value().udata_value().map(Tag::Unsigned);

                    if value.is_none() {
                        value = attr.value().sdata_value().map(Tag::Signed);
                    }

                    if value.is_none() {
                        bail!(
                            "bad discriminant on const enum {}: {:?} ({:?})",
                            parent,
                            attr.value(),
                            name
                        );
                    }
                }

                _ => {}
            }
        }

        if let (Some(name), Some(value)) = (name, value) {
            if let Some(union) = self.enums.get_mut(&parent) {
                union.variants.push(HubrisEnumVariant {
                    name: name.to_string(),
                    offset: 0,
                    goff: None,
                    tag: Some(value),
                });

                Ok(())
            } else {
                Err(anyhow!("missing enum for variant {}", goff))
            }
        } else {
            Err(anyhow!("incomplete const enum variant {}", goff))
        }
    }

    fn dwarf_const_enum<R: gimli::Reader<Offset = usize>>(
        &mut self,
        dwarf: &gimli::Dwarf<gimli::EndianSlice<gimli::LittleEndian>>,
        unit: &gimli::Unit<R>,
        entry: &gimli::DebuggingInformationEntry<
            gimli::EndianSlice<gimli::LittleEndian>,
            usize,
        >,
        goff: HubrisGoff,
        namespace: Option<NamespaceId>,
    ) -> Result<()> {
        let mut attrs = entry.attrs();
        let mut name = None;
        let mut size = None;
        let mut dgoff = None;

        while let Some(attr) = attrs.next()? {
            match attr.name() {
                gimli::constants::DW_AT_name => {
                    name = dwarf_name(dwarf, attr.value());
                }

                gimli::constants::DW_AT_type => {
                    dgoff = self.dwarf_value_goff(unit, &attr.value());
                }

                gimli::constants::DW_AT_byte_size => {
                    if let gimli::AttributeValue::Udata(value) = attr.value() {
                        size = Some(value as usize)
                    }
                }

                _ => {}
            }
        }

        if let (Some(name), Some(dgoff), Some(size)) = (name, dgoff, size) {
            self.enums.insert(
                goff,
                HubrisEnum {
                    name: name.to_string(),
                    goff,
                    size,
                    discriminant: Some(HubrisDiscriminant::Value(dgoff, 0)),
                    tag: None,
                    variants: Vec::new(),
                    namespace,
                },
            );

            self.enums_byname.insert(name.to_string(), goff);
            Ok(())
        } else {
            Err(anyhow!("missing enum on variant {}", goff))
        }
    }

    fn dwarf_array<R: gimli::Reader<Offset = usize>>(
        &mut self,
        unit: &gimli::Unit<R>,
        entry: &gimli::DebuggingInformationEntry<
            gimli::EndianSlice<gimli::LittleEndian>,
            usize,
        >,
        parent: HubrisGoff,
        array: Option<HubrisGoff>,
    ) -> Result<()> {
        let goff = self.dwarf_goff(unit, entry);

        let array = match array {
            Some(array) => array,
            None => bail!("missing array type for subrange type {}", goff),
        };

        let count_attr = entry
            .attrs()
            .find(|attr| Ok(attr.name() == gimli::constants::DW_AT_count))?;
        let count = count_attr.and_then(|a| a.udata_value());

        if let Some(count) = count {
            self.arrays.insert(
                parent,
                HubrisArray { goff: array, count: count as usize },
            );
        }

        Ok(())
    }

    fn dwarf_basetype<R: gimli::Reader<Offset = usize>>(
        &mut self,
        dwarf: &gimli::Dwarf<gimli::EndianSlice<gimli::LittleEndian>>,
        unit: &gimli::Unit<R>,
        entry: &gimli::DebuggingInformationEntry<
            gimli::EndianSlice<gimli::LittleEndian>,
            usize,
        >,
    ) -> Result<()> {
        let mut attrs = entry.attrs();
        let goff = self.dwarf_goff(unit, entry);
        let mut size = None;
        let mut encoding = None;
        let mut name = None;

        while let Some(attr) = attrs.next()? {
            match attr.name() {
                gimli::constants::DW_AT_name => {
                    name = dwarf_name(dwarf, attr.value());
                }

                gimli::constants::DW_AT_byte_size => {
                    if let gimli::AttributeValue::Udata(value) = attr.value() {
                        size = Some(value as usize)
                    }
                }
                gimli::constants::DW_AT_encoding => {
                    if let gimli::AttributeValue::Encoding(en) = attr.value() {
                        encoding = Some(match en {
                            gimli::constants::DW_ATE_signed => {
                                HubrisEncoding::Signed
                            }

                            gimli::constants::DW_ATE_unsigned => {
                                HubrisEncoding::Unsigned
                            }

                            gimli::constants::DW_ATE_float => {
                                HubrisEncoding::Float
                            }

                            gimli::constants::DW_ATE_boolean => {
                                HubrisEncoding::Bool
                            }

                            _ => HubrisEncoding::Unknown,
                        });
                    }
                }
                _ => {}
            }
        }

        if let (Some(size), Some(encoding)) = (size, encoding) {
            self.basetypes.insert(goff, HubrisBasetype { encoding, size });
        }

        if let Some(name) = name {
            self.basetypes_byname.insert(String::from(name), goff);
        }

        Ok(())
    }

    fn dwarf_ptrtype<R: gimli::Reader<Offset = usize>>(
        &mut self,
        dwarf: &gimli::Dwarf<gimli::EndianSlice<gimli::LittleEndian>>,
        unit: &gimli::Unit<R>,
        entry: &gimli::DebuggingInformationEntry<
            gimli::EndianSlice<gimli::LittleEndian>,
            usize,
        >,
    ) -> Result<()> {
        let mut attrs = entry.attrs();
        let goff = self.dwarf_goff(unit, entry);
        let mut underlying = None;
        let mut name = None;

        while let Some(attr) = attrs.next()? {
            match attr.name() {
                gimli::constants::DW_AT_type => {
                    underlying = self.dwarf_value_goff(unit, &attr.value());
                }
                gimli::constants::DW_AT_name => {
                    name = dwarf_name(dwarf, attr.value());
                }
                _ => {}
            }
        }

        let name = name.unwrap_or("<UNNAMED>").to_string();

        if let Some(underlying) = underlying {
            self.ptrtypes.insert(goff, (name, underlying));
        }

        Ok(())
    }

    fn dwarf_namespace(
        &mut self,
        dwarf: &gimli::Dwarf<gimli::EndianSlice<gimli::LittleEndian>>,
        entry: &gimli::DebuggingInformationEntry<
            gimli::EndianSlice<gimli::LittleEndian>,
            usize,
        >,
        depth: isize,
        namespace: &mut Vec<(NamespaceId, isize)>,
    ) -> Result<()> {
        let mut attrs = entry.attrs();

        //
        // For a new namespace, create an entry in our namespaces vector,
        // and push a tuple consisting of the identifer and our depth.
        //
        while let Some(attr) = attrs.next()? {
            if attr.name() == gimli::constants::DW_AT_name {
                if let Some(name) = dwarf_name(dwarf, attr.value()) {
                    namespace.push((
                        self.namespaces.allocate(
                            name,
                            namespace.last().map(|(id, _)| *id),
                        ),
                        depth,
                    ));
                }

                break;
            }
        }

        Ok(())
    }

    fn dwarf_fileline<R: gimli::Reader<Offset = usize>>(
        &mut self,
        dwarf: &gimli::Dwarf<R>,
        unit: &gimli::Unit<R>,
        entry: &gimli::DebuggingInformationEntry<
            gimli::EndianSlice<gimli::LittleEndian>,
            usize,
        >,
    ) -> Result<()> {
        let mut attrs = entry.attrs();
        let mut file = None;
        let mut line = None;

        let goff = self.dwarf_goff(unit, entry);

        while let Some(attr) = attrs.next()? {
            match (attr.name(), attr.value()) {
                (
                    gimli::constants::DW_AT_decl_file,
                    gimli::AttributeValue::FileIndex(value),
                ) => {
                    file = Some(value);
                }
                (
                    gimli::constants::DW_AT_decl_line,
                    gimli::AttributeValue::Udata(value),
                ) => {
                    line = Some(value);
                }
                _ => {}
            }
        }

        if let (Some(file), Some(line)) = (file, line) {
            let header = match &unit.line_program {
                Some(program) => program.header(),
                None => return Ok(()),
            };

            let file = match header.file(file) {
                Some(header) => header,
                None => {
                    bail!("no header at {}", goff);
                }
            };

            let mut comp = None;
            let directory;
            if let Some(dir) = file.directory(header) {
                let dir = dwarf.attr_string(unit, dir)?;
                let dir = dir.to_string_lossy()?;

                if !dir.starts_with('/') {
                    if let Some(comp_dir) = &unit.comp_dir {
                        comp = Some(comp_dir.to_string_lossy()?.into_owned());
                    }
                }

                directory = Some(dir.into_owned())
            } else {
                directory = None
            }

            let s = dwarf.attr_string(unit, file.path_name())?;
            let file = s.to_string_lossy()?.into_owned();

            self.src.insert(
                goff,
                HubrisSrc { file, directory, comp_directory: comp, line },
            );
        }

        Ok(())
    }

    fn dwarf_struct<R: gimli::Reader<Offset = usize>>(
        &mut self,
        dwarf: &gimli::Dwarf<gimli::EndianSlice<gimli::LittleEndian>>,
        unit: &gimli::Unit<R>,
        entry: &gimli::DebuggingInformationEntry<
            gimli::EndianSlice<gimli::LittleEndian>,
            usize,
        >,
        namespace: Option<NamespaceId>,
    ) -> Result<()> {
        let mut attrs = entry.attrs();
        let goff = self.dwarf_goff(unit, entry);
        let mut name = None;
        let mut size = None;

        while let Some(attr) = attrs.next()? {
            match attr.name() {
                gimli::constants::DW_AT_name => {
                    name = dwarf_name(dwarf, attr.value());
                }

                gimli::constants::DW_AT_byte_size => {
                    if let gimli::AttributeValue::Udata(value) = attr.value() {
                        size = Some(value as usize)
                    }
                }

                _ => {}
            }
        }

        if let (Some(name), Some(size)) = (name, size) {
            self.structs.insert(
                goff,
                HubrisStruct {
                    name: name.to_string(),
                    size,
                    goff,
                    members: Vec::new(),
                    namespace,
                },
            );

            self.structs_byname.insert(name.to_string(), goff);
        }

        Ok(())
    }

    fn dwarf_inlined<R: gimli::Reader<Offset = usize>>(
        &mut self,
        dwarf: &gimli::Dwarf<R>,
        unit: &gimli::Unit<R>,
        entry: &gimli::DebuggingInformationEntry<
            gimli::EndianSlice<gimli::LittleEndian>,
            usize,
        >,
        depth: isize,
    ) -> Result<()> {
        //
        // Iterate over our attributes looking for addresses
        //
        let mut attrs = entry.attrs();
        let mut low: Option<u64> = None;
        let mut high: Option<u64> = None;
        let mut origin: Option<HubrisGoff> = None;

        let goff = self.dwarf_goff(unit, entry);

        while let Some(attr) = attrs.next()? {
            match (attr.name(), attr.value()) {
                (
                    gimli::constants::DW_AT_low_pc,
                    gimli::AttributeValue::Addr(addr),
                ) => {
                    low = Some(addr);
                }
                (
                    gimli::constants::DW_AT_high_pc,
                    gimli::AttributeValue::Udata(data),
                ) => {
                    high = Some(data);
                }
                (gimli::constants::DW_AT_abstract_origin, _) => {
                    origin = self.dwarf_value_goff(unit, &attr.value());
                }
                _ => {}
            }
        }

        let origin = match (low, high, origin) {
            (Some(addr), Some(len), Some(origin)) => {
                // If we've got the low and high defined, we'll just go ahead
                // and terminate here.
                self.inlined
                    .insert((addr as u32, depth), (len as u32, goff, origin));

                return Ok(());
            }
            (None, None, Some(o)) => {
                // Otherwise, unwrap origin so it can be used below.
                o
            }
            _ => {
                bail!("missing origin or high or low for {}", goff);
            }
        };

        let mut attrs = entry.attrs();
        while let Some(attr) = attrs.next()? {
            if let (
                gimli::constants::DW_AT_ranges,
                gimli::AttributeValue::RangeListsRef(r),
            ) = (attr.name(), attr.value())
            {
                let raw_ranges = dwarf.ranges.raw_ranges(r, unit.encoding())?;
                let raw_ranges: Vec<_> = raw_ranges.collect()?;

                for r in raw_ranges {
                    if let gimli::RawRngListEntry::AddressOrOffsetPair {
                        begin,
                        end,
                    } = r
                    {
                        let begin = begin as u32;
                        let end = end as u32;

                        self.inlined.insert(
                            (begin, depth),
                            (end - begin, goff, origin),
                        );
                    }
                }

                return Ok(());
            }
        }

        Err(anyhow!("missing address range for {}", goff))
    }

    fn dwarf_subprogram<R: gimli::Reader<Offset = usize>>(
        &mut self,
        dwarf: &gimli::Dwarf<gimli::EndianSlice<gimli::LittleEndian>>,
        unit: &gimli::Unit<R>,
        entry: &gimli::DebuggingInformationEntry<
            gimli::EndianSlice<gimli::LittleEndian>,
            usize,
        >,
    ) -> Result<()> {
        let mut name = None;
        let mut linkage_name = None;
        let mut addr = None;
        let mut len = None;

        let goff = self.dwarf_goff(unit, entry);

        // Iterate over the attributes in the DIE.
        let mut attrs = entry.attrs();
        while let Some(attr) = attrs.next()? {
            match (attr.name(), attr.value()) {
                (
                    gimli::constants::DW_AT_low_pc,
                    gimli::AttributeValue::Addr(value),
                ) => {
                    addr = Some(value);
                }
                (
                    gimli::constants::DW_AT_high_pc,
                    gimli::AttributeValue::Udata(value),
                ) => {
                    len = Some(value);
                }
                (gimli::constants::DW_AT_linkage_name, _) => {
                    linkage_name = dwarf_name(dwarf, attr.value());
                }
                (gimli::constants::DW_AT_name, _) => {
                    name = dwarf_name(dwarf, attr.value());
                }
                _ => {}
            }
        }

        if let Some(name) = name {
            let demangled_name = if let Some(ln) = linkage_name {
                demangle_name(ln)
            } else {
                name.to_string()
            };

            self.subprograms.insert(goff, demangled_name.clone());

            match (addr, len) {
                (Some(addr), Some(len)) if addr != 0 => {
                    self.dsyms.insert(
                        addr as u32,
                        HubrisSymbol {
                            name: name.to_string(),
                            demangled_name,
                            size: len as u32,
                            addr: addr as u32,
                            goff,
                        },
                    );
                }
                _ => {}
            }
        } else {
            log::trace!("no name found for {}", goff);
        }

        Ok(())
    }

    fn dwarf_variable<R: gimli::Reader<Offset = usize>>(
        &mut self,
        dwarf: &gimli::Dwarf<gimli::EndianSlice<gimli::LittleEndian>>,
        unit: &gimli::Unit<R>,
        entry: &gimli::DebuggingInformationEntry<
            gimli::EndianSlice<gimli::LittleEndian>,
            usize,
        >,
    ) -> Result<()> {
        let mut attrs = entry.attrs();
        let goff = self.dwarf_goff(unit, entry);
        let mut name = None;
        let mut linkage_name = None;
        let mut external = None;
        let mut tgoff = None;
        let mut dwarf_location = None;

        'attrloop: while let Some(attr) = attrs.next()? {
            match attr.name() {
                gimli::constants::DW_AT_name => {
                    name = dwarf_name(dwarf, attr.value());
                }

                gimli::constants::DW_AT_external => {
                    if let gimli::AttributeValue::Flag(val) = attr.value() {
                        external = Some(val);
                    }
                }

                gimli::constants::DW_AT_linkage_name => {
                    linkage_name = dwarf_name(dwarf, attr.value());
                }

                gimli::constants::DW_AT_type => {
                    tgoff = self.dwarf_value_goff(unit, &attr.value());
                }
                gimli::constants::DW_AT_location => {
                    if let Some(e) = attr.exprloc_value() {
                        let mut eval = e.evaluation(unit.encoding());
                        let mut result = match eval.evaluate() {
                            Ok(r) => r,
                            Err(e) => {
                                warn!(
                                    "AT_location evaluation failed \
                                    for entry {:?}: {}",
                                    name, e
                                );

                                continue;
                            }
                        };
                        // Loop to nudge forward the evaluation of the expression,
                        // if required.
                        let eval_result = loop {
                            match result {
                                gimli::EvaluationResult::Complete => break Some(eval.result()),
                                gimli::EvaluationResult::RequiresRelocatedAddress(a) => {
                                    result = match eval.resume_with_relocated_address(a) {
                                        Ok(r) => r,
                                        Err(e) => {
                                            warn!(
                                                "AT_location failed \
                                                after resume: {}", e
                                            );
                                            break None;
                                        }
                                    };
                                }
                                x => {
                                    log::debug!(
                                        "unsupported eval result: {:?}", x
                                    );
                                    break None;
                                }
                            }
                        };
                        let eval_result = if let Some(e) = eval_result {
                            e
                        } else {
                            // Could not evaluate, abort this attr.
                            continue;
                        };

                        // Yay! Now we need to see if the pieces that resulted
                        // are useful to us. In this case, "variable" means
                        // static variable, and so we're only interested if it
                        // consists of a contiguous range of pieces at absolute
                        // addresses.
                        if eval_result.iter().any(|piece| {
                            !matches!(
                                piece.location,
                                gimli::read::Location::Address { .. }
                            )
                        }) {
                            // Some portion of this variable is not
                            // in static memory. Ignore it.
                            continue;
                        }
                        if eval_result
                            .iter()
                            .any(|piece| piece.size_in_bits.is_none())
                        {
                            // Some portion of this variable does
                            // not have a fixed size. Ignore it.
                            continue;
                        }
                        // Sort the address/size information from
                        // the pieces so we can easily determine
                        // whether they're contiguous.
                        let mut ranges = eval_result
                            .into_iter()
                            .map(|piece| match piece.location {
                                gimli::read::Location::Address { address } => {
                                    (address, piece.size_in_bits.unwrap())
                                }
                                _ => unreachable!(),
                            })
                            .collect::<Vec<_>>();
                        ranges.sort_by_key(|&(addr, _size)| addr);
                        let base_addr = if let Some((a, _s)) = ranges.first() {
                            *a
                        } else {
                            // ranges list wound up being empty.
                            continue;
                        };
                        let mut next_addr = base_addr;
                        for &(a, s) in &ranges {
                            if a != next_addr {
                                // Discontiguous.
                                continue 'attrloop;
                            }
                            next_addr = a + s / 8;
                        }

                        dwarf_location = Some((
                            base_addr as u32,
                            (next_addr - base_addr) as u32,
                        ));
                    }
                }
                _ => {}
            }
        }

        if external.is_none() && linkage_name.is_none() {
            return Ok(());
        }

        if let (Some(name), Some(tgoff)) = (name, tgoff) {
            let linkage = linkage_name.unwrap_or(name);

            if let Some(syms) = self.esyms_byname.get_vec(linkage) {
                for &(addr, size) in syms {
                    if let btree_map::Entry::Vacant(e) = self.dsyms.entry(addr)
                    {
                        e.insert(HubrisSymbol {
                            name: String::from(name),
                            demangled_name: demangle_name(name),
                            size,
                            addr,
                            goff,
                        });
                        self.variables.insert(
                            String::from(name),
                            HubrisVariable {
                                goff: tgoff,
                                addr,
                                size: size as usize,
                            },
                        );
                        // Note: alternate format (#) removes trailing hash.
                        // This makes duplicates possible, but, we already
                        // handle duplicates in unqualified names, and the
                        // hashes are gross.
                        let qname =
                            format!("{:#}", rustc_demangle::demangle(linkage));
                        self.qualified_variables.insert(
                            qname,
                            HubrisVariable {
                                goff: tgoff,
                                addr,
                                size: size as usize,
                            },
                        );
                    }
                }
            } else if let Some((addr, size)) = dwarf_location {
                log::debug!(
                    "Symbol {} missing from ELF info, using DWARF as fallback.",
                    linkage
                );
                if let btree_map::Entry::Vacant(e) = self.dsyms.entry(addr) {
                    e.insert(HubrisSymbol {
                        name: String::from(name),
                        demangled_name: demangle_name(name),
                        size,
                        addr,
                        goff,
                    });
                    self.variables.insert(
                        String::from(name),
                        HubrisVariable {
                            goff: tgoff,
                            addr,
                            size: size as usize,
                        },
                    );
                    // Note: alternate format (#) removes trailing hash.
                    // This makes duplicates possible, but, we already
                    // handle duplicates in unqualified names, and the
                    // hashes are gross.
                    let qname =
                        format!("{:#}", rustc_demangle::demangle(linkage));
                    self.qualified_variables.insert(
                        qname,
                        HubrisVariable {
                            goff: tgoff,
                            addr,
                            size: size as usize,
                        },
                    );
                }
            } else {
                self.definitions.insert(String::from(name), tgoff);
            }
        }

        Ok(())
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum HubrisTask {
    Kernel,
    Task(u32),
}

impl HubrisTask {
    pub fn id(&self) -> String {
        match self {
            HubrisTask::Kernel => "-".to_string(),
            HubrisTask::Task(id) => format!("{}", id),
        }
    }
    /// Returns the inner `u32` from a `Task` variant.
    ///
    /// # Panics
    /// Panics if this is `HubrisTask::Kernel`
    pub fn task(&self) -> u32 {
        if let HubrisTask::Task(u) = self {
            *u
        } else {
            panic!("Cannot get task id of kernel");
        }
    }
}

impl fmt::Display for HubrisTask {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            HubrisTask::Kernel => write!(f, "kernel"),
            HubrisTask::Task(id) => write!(f, "Task #{}", id),
        }
    }
}

impl From<HubrisGoff> for HubrisTask {
    fn from(goff: HubrisGoff) -> Self {
        if goff.object == 0 {
            HubrisTask::Kernel
        } else {
            HubrisTask::Task(goff.object - 1)
        }
    }
}

///
/// An identifier that corresponds to a global offset within a particular DWARF
/// object.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone)]
pub struct HubrisGoff {
    pub object: u32,
    pub goff: usize,
}

impl fmt::Display for HubrisGoff {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.object > 0 {
            write!(f, "GOFF 0x{:08x} (object {})", self.goff, self.object)
        } else {
            write!(f, "GOFF 0x{:08x}", self.goff)
        }
    }
}

#[derive(Clone, Debug)]
pub struct HubrisSymbol {
    pub addr: u32,
    pub name: String,
    pub demangled_name: String,
    pub size: u32,
    pub goff: HubrisGoff,
}

#[derive(Copy, Clone, Debug)]
pub struct HubrisInlined<'a> {
    pub addr: u32,
    pub name: &'a str,
    pub id: HubrisGoff,
    pub origin: HubrisGoff,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum HubrisEncoding {
    Unknown,
    Signed,
    Unsigned,
    Float,
    Bool,
}

#[derive(Copy, Clone, Debug)]
pub struct HubrisBasetype {
    pub encoding: HubrisEncoding,
    pub size: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct HubrisStructMember {
    pub offset: usize,
    pub name: String,
    pub goff: HubrisGoff,
}

#[derive(Clone, Debug)]
pub struct HubrisStruct {
    pub name: String,
    pub goff: HubrisGoff,
    pub size: usize,
    pub members: Vec<HubrisStructMember>,
    namespace: Option<NamespaceId>,
}

impl HubrisStruct {
    pub fn lookup_member(&self, name: &str) -> Result<&HubrisStructMember> {
        for member in &self.members {
            if member.name == name {
                return Ok(member);
            }
        }

        bail!("missing member: {}.{}", self.name, name)
    }

    /// If this structure is a newtype (that is, a 1-tuple structure), return
    /// the encapsulated type.
    pub fn newtype(&self) -> Option<HubrisGoff> {
        if self.members.len() == 1 && self.members[0].name == "__0" {
            Some(self.members[0].goff)
        } else {
            None
        }
    }

    pub fn probably_a_tuple(&self) -> bool {
        // Scan the shape of the type to tell if it's tupley. Tuples are structs
        // that only have fields of the form __#, where # is a decimal number.
        for m in &self.members {
            if !m.name.starts_with("__") {
                return false;
            }
            if !m.name[2..].chars().all(|c| c.is_numeric()) {
                return false;
            }
        }
        true
    }

    pub fn differs(&self, hubris: &HubrisArchive, rhs: &Self) -> Result<bool> {
        if self.size != rhs.size || self.members.len() != rhs.members.len() {
            Ok(true)
        } else {
            for (lm, rm) in self.members.iter().zip(&rhs.members) {
                if lm.offset != rm.offset || lm.name != rm.name {
                    return Ok(true);
                }

                if hubris.differ(lm.goff, rm.goff)? {
                    return Ok(true);
                }
            }
            Ok(false)
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct HubrisVariable {
    pub goff: HubrisGoff,
    pub addr: u32,
    pub size: usize,
}

#[derive(Copy, Clone, Debug)]
pub struct HubrisArray {
    pub goff: HubrisGoff,
    pub count: usize,
}

impl HubrisArray {
    pub fn differs(&self, hubris: &HubrisArchive, rhs: &Self) -> Result<bool> {
        if self.count != rhs.count {
            Ok(true)
        } else {
            hubris.differ(self.goff, rhs.goff)
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct HubrisRegionAttr {
    pub read: bool,
    pub write: bool,
    pub execute: bool,
    pub device: bool,
    pub dma: bool,
    pub external: bool,
}

#[derive(Clone, Debug)]
pub struct HubrisRegion {
    /// Address of description in kernel RAM
    pub daddr: Option<u32>,

    /// Base address of region
    pub base: u32,

    /// Size of region
    pub size: u32,

    /// Attributes of this region
    pub attr: HubrisRegionAttr,

    /// Tasks using this region (usually just one)
    pub tasks: Vec<HubrisTask>,
}

#[derive(Clone, Debug)]
pub struct HubrisEnumVariant {
    pub name: String,
    pub offset: usize,
    pub goff: Option<HubrisGoff>,
    pub tag: Option<Tag>,
}

/// Type representing an enum variant tag.
///
/// Tags may be signed or unsigned. Every variant of a given enum uses the same
/// signedness. (This should likely be available on the HubrisEnum type but is
/// not.) As a result, signed and unsigned `Tag`s never compare as equal. This
/// can pose a problem for manually rolling enum reconstruction code using
/// `lookup_variant_by_tag`, but `determine_variant` will do the right thing.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Tag {
    Unsigned(u64),
    Signed(i64),
}

/// All u64s can become Tags.
impl From<u64> for Tag {
    fn from(u: u64) -> Self {
        Tag::Unsigned(u)
    }
}

/// All u32s can become Tags.
impl From<u32> for Tag {
    fn from(u: u32) -> Self {
        Tag::Unsigned(u64::from(u))
    }
}

/// All u16s can become Tags.
impl From<u16> for Tag {
    fn from(u: u16) -> Self {
        Tag::Unsigned(u64::from(u))
    }
}

/// All u8s can become Tags.
impl From<u8> for Tag {
    fn from(u: u8) -> Self {
        Tag::Unsigned(u64::from(u))
    }
}

/// All i64s can become Tags.
impl From<i64> for Tag {
    fn from(i: i64) -> Self {
        Tag::Signed(i)
    }
}

/// All i32s can become Tags.
impl From<i32> for Tag {
    fn from(i: i32) -> Self {
        Tag::Signed(i64::from(i))
    }
}

/// All i16s can become Tags.
impl From<i16> for Tag {
    fn from(i: i16) -> Self {
        Tag::Signed(i64::from(i))
    }
}

/// All i8s can become Tags.
impl From<i8> for Tag {
    fn from(i: i8) -> Self {
        Tag::Signed(i64::from(i))
    }
}

/// Some Tags can become u8s.
impl TryFrom<Tag> for u8 {
    type Error = TryFromIntError;

    fn try_from(t: Tag) -> Result<u8, TryFromIntError> {
        match t {
            Tag::Unsigned(u) => u.try_into(),
            Tag::Signed(i) => i.try_into(),
        }
    }
}

/// Some Tags can become u16s.
impl TryFrom<Tag> for u16 {
    type Error = TryFromIntError;

    fn try_from(t: Tag) -> Result<u16, TryFromIntError> {
        match t {
            Tag::Unsigned(u) => u.try_into(),
            Tag::Signed(i) => i.try_into(),
        }
    }
}

/// Some Tags can become u32s.
impl TryFrom<Tag> for u32 {
    type Error = TryFromIntError;

    fn try_from(t: Tag) -> Result<u32, TryFromIntError> {
        match t {
            Tag::Unsigned(u) => u.try_into(),
            Tag::Signed(i) => i.try_into(),
        }
    }
}

/// Some Tags (and all unsigned Tags) can become u64s.
impl TryFrom<Tag> for u64 {
    type Error = TryFromIntError;

    fn try_from(t: Tag) -> Result<u64, TryFromIntError> {
        match t {
            Tag::Unsigned(u) => Ok(u),
            Tag::Signed(i) => i.try_into(),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum HubrisDiscriminant {
    Expected(HubrisGoff),
    Value(HubrisGoff, usize),
}

#[derive(Clone, Debug)]
pub struct HubrisEnum {
    pub name: String,
    pub goff: HubrisGoff,
    pub size: usize,
    /// Info on the discriminant, which can be `None` (missing) if the enum has
    /// only one variant.
    pub discriminant: Option<HubrisDiscriminant>,
    /// temporary to hold tag of next variant
    pub tag: Option<Tag>,
    pub variants: Vec<HubrisEnumVariant>,
    namespace: Option<NamespaceId>,
}

impl HubrisEnum {
    /// Finds a variant of this enum type that has the discriminant `tag`.
    ///
    /// `tag` must have the proper signedness for this enum. Historically a lot
    /// of code assumed that discriminants were always unsigned, and there are
    /// still routines around that make this assumption; they will simply fail
    /// to find variants for enums using signed discriminants.
    ///
    /// If you would like to avoid this pitfall, the `determine_variant`
    /// routine will do the type lookup for you, and should almost always be
    /// preferred where applicable!
    pub fn lookup_variant_by_tag(
        &self,
        tag: Tag,
    ) -> Option<&HubrisEnumVariant> {
        // We prioritize picking a variant with the matching tag
        if let Some(t) = self.variants.iter().find(|v| v.tag == Some(tag)) {
            Some(t)
        } else {
            // Otherwise, we pick a variant with the None tag, for the case of a
            // single-element enum.
            self.variants.iter().find(|v| v.tag.is_none())
        }
    }

    pub fn lookup_variant_by_index(
        &self,
        index: usize,
    ) -> Option<&HubrisEnumVariant> {
        // We assume `index` is based on the DWARF ordering. In practice our
        // caller is probably expecting `index` to be "source code order" (e.g.,
        // for deserializing hubpack-encoded enums), which we assume is the
        // same! This should be true based on section 5.7.10 of [the DWARF
        // spec](https://dwarfstd.org/Dwarf5Std.php).
        self.variants.get(index)
    }

    pub fn lookup_variant_byname(
        &self,
        name: &str,
    ) -> Result<&HubrisEnumVariant> {
        for variant in &self.variants {
            if variant.name == name {
                return Ok(variant);
            }
        }

        bail!("missing variant: {}.{}", self.name, name)
    }

    /// Finds a variant of this enum given a blob of data representing one of
    /// its variants. This handles looking up the offset of the tag in memory,
    /// and handling signed/unsigned tags.
    ///
    /// It's almost always better to use this than to attempt to roll your own
    /// using `lookup_variant_by_tag`.
    pub fn determine_variant(
        &self,
        hubris: &HubrisArchive,
        buf: &[u8],
    ) -> Result<&HubrisEnumVariant> {
        let readtag = |b: &[u8], o, sz, enc| -> Result<Tag> {
            Ok(match (sz, enc) {
                (1, HubrisEncoding::Unsigned) => Tag::from(b[o]),
                (2, HubrisEncoding::Unsigned) => {
                    Tag::from(u16::from_le_bytes(b[o..o + 2].try_into()?))
                }
                (4, HubrisEncoding::Unsigned) => {
                    Tag::from(u32::from_le_bytes(b[o..o + 4].try_into()?))
                }
                (8, HubrisEncoding::Unsigned) => {
                    Tag::from(u64::from_le_bytes(b[o..o + 8].try_into()?))
                }

                (1, HubrisEncoding::Signed) => Tag::from(b[o] as i8),
                (2, HubrisEncoding::Signed) => {
                    Tag::from(i16::from_le_bytes(b[o..o + 2].try_into()?))
                }
                (4, HubrisEncoding::Signed) => {
                    Tag::from(i32::from_le_bytes(b[o..o + 4].try_into()?))
                }
                (8, HubrisEncoding::Signed) => {
                    Tag::from(i64::from_le_bytes(b[o..o + 8].try_into()?))
                }

                _ => {
                    bail!("bad variant size!");
                }
            })
        };

        if let Some(HubrisDiscriminant::Value(goff, offs)) = self.discriminant {
            let (encoding, size) = match hubris.basetypes.get(&goff) {
                Some(v) => (v.encoding, v.size),
                None => {
                    bail!("enum has discriminant of unknown type: {}", goff);
                }
            };

            let val = readtag(buf, offs, size, encoding)?;

            match self.lookup_variant_by_tag(val) {
                None => {
                    bail!("unknown variant: {:#x?}", val);
                }

                Some(variant) => Ok(variant),
            }
        } else {
            if self.variants.is_empty() {
                bail!("enum {} has no variants", self.goff);
            }

            if self.variants.len() > 1 {
                bail!("enum has multiple variants but no discriminant");
            }

            Ok(&self.variants[0])
        }
    }

    fn differs(&self, hubris: &HubrisArchive, rhs: &Self) -> Result<bool> {
        if self.size != rhs.size
            || self.discriminant != rhs.discriminant
            || self.variants.len() != rhs.variants.len()
        {
            Ok(true)
        } else {
            for (lm, rm) in self.variants.iter().zip(&rhs.variants) {
                if lm.offset != rm.offset
                    || lm.name != rm.name
                    || lm.tag != rm.tag
                {
                    return Ok(true);
                }

                match (lm.goff, rm.goff) {
                    (Some(lg), Some(rg)) => {
                        if hubris.differ(lg, rg)? {
                            return Ok(true);
                        }
                    }
                    (None, None) => {}
                    _ => return Ok(true),
                }
            }

            Ok(false)
        }
    }
}

#[derive(Clone, Debug)]
pub struct HubrisUnion {
    pub name: String,
    pub goff: HubrisGoff,
    pub size: usize,
    pub variants: Vec<HubrisEnumVariant>,
}

impl HubrisUnion {
    //
    // This is really horrid, but we are going to heuristically determine if
    // this is a MaybeUninit -- and if so, return the GOFF of the "value"
    // member.  It should go without saying that this is brittle in obvious
    // ways:  if another union has the same two members as MaybeUninit or
    // MaybeUninit starts naming its members differently, this will break --
    // and it will deserve to be broken.
    //
    pub fn maybe_uninit(&self) -> Option<HubrisGoff> {
        if self.variants.len() != 2 {
            return None;
        }

        if self.variants[0].name != "uninit" {
            return None;
        }

        if self.variants[1].name != "value" {
            return None;
        }

        self.variants[1].goff
    }

    fn differs(&self, hubris: &HubrisArchive, rhs: &Self) -> Result<bool> {
        if self.size != rhs.size || self.variants.len() != rhs.variants.len() {
            Ok(true)
        } else {
            for (lm, rm) in self.variants.iter().zip(&rhs.variants) {
                if lm.offset != rm.offset
                    || lm.name != rm.name
                    || lm.tag != rm.tag
                {
                    return Ok(true);
                }

                match (lm.goff, rm.goff) {
                    (Some(lg), Some(rg)) => {
                        if hubris.differ(lg, rg)? {
                            return Ok(true);
                        }
                    }
                    (None, None) => {}
                    _ => return Ok(true),
                }
            }

            Ok(false)
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum HubrisType<'a> {
    Base(&'a HubrisBasetype),
    Struct(&'a HubrisStruct),
    Enum(&'a HubrisEnum),
    Array(&'a HubrisArray),
    Union(&'a HubrisUnion),
    Ptr(HubrisGoff),
}

impl<'a> HubrisType<'a> {
    pub fn name(&self, hubris: &'a HubrisArchive) -> Result<Cow<'a, str>> {
        match self {
            Self::Base(t) => {
                use HubrisEncoding::*;
                let s = match (t.encoding, t.size) {
                    (Signed, 1) => "i8",
                    (Signed, 2) => "i16",
                    (Signed, 4) => "i32",
                    (Signed, 8) => "i64",
                    (Signed, 16) => "i128",

                    (Unsigned, 1) => "u8",
                    (Unsigned, 2) => "u16",
                    (Unsigned, 4) => "u32",
                    (Unsigned, 8) => "u64",
                    (Unsigned, 16) => "u128",

                    (Float, 4) => "f32",
                    (Float, 8) => "f64",

                    (Bool, 1) => "bool",

                    _ => "???",
                };
                Ok(s.into())
            }
            Self::Struct(t) => Ok(Cow::from(&t.name)),
            Self::Enum(t) => Ok(Cow::from(&t.name)),
            Self::Union(t) => Ok(Cow::from(&t.name)),
            Self::Ptr(g) => {
                let name = &hubris
                    .ptrtypes
                    .get(g)
                    .ok_or_else(|| anyhow!("not a pointer: {:?}", g))?
                    .0;
                Ok(name.into())
            }
            Self::Array(t) => {
                let mut name = String::new();
                name += "[";
                name += &hubris.lookup_type(t.goff)?.name(hubris)?;
                write!(name, "; {}]", t.count)?;
                Ok(name.into())
            }
        }
    }

    pub fn size(&self, hubris: &HubrisArchive) -> Result<usize> {
        match self {
            Self::Base(t) => Ok(t.size),
            Self::Struct(t) => Ok(t.size),
            Self::Enum(t) => Ok(t.size),
            Self::Union(t) => Ok(t.size),
            Self::Ptr(_) => Ok(4), // TODO
            Self::Array(t) => {
                let elt_size = hubris.lookup_type(t.goff)?.size(hubris)?;
                Ok(elt_size * t.count)
            }
        }
    }
}

impl<'a> From<&'a HubrisBasetype> for HubrisType<'a> {
    fn from(x: &'a HubrisBasetype) -> Self {
        Self::Base(x)
    }
}

impl<'a> From<&'a HubrisStruct> for HubrisType<'a> {
    fn from(x: &'a HubrisStruct) -> Self {
        Self::Struct(x)
    }
}

impl<'a> From<&'a HubrisEnum> for HubrisType<'a> {
    fn from(x: &'a HubrisEnum) -> Self {
        Self::Enum(x)
    }
}

impl<'a> From<&'a HubrisArray> for HubrisType<'a> {
    fn from(x: &'a HubrisArray) -> Self {
        Self::Array(x)
    }
}

impl<'a> From<&'a HubrisUnion> for HubrisType<'a> {
    fn from(x: &'a HubrisUnion) -> Self {
        Self::Union(x)
    }
}

impl<'a> std::fmt::Display for HubrisType<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            HubrisType::Struct(ty) => write!(f, "struct {}", ty.name),
            HubrisType::Enum(ty) => write!(f, "enum {}", ty.name),
            HubrisType::Base(b) => write!(f, "base type {:?}", b.encoding),
            HubrisType::Array(_) => f.write_str("array"),
            HubrisType::Ptr(_) => f.write_str("pointer"),
            HubrisType::Union(ty) => write!(f, "union {}", ty.name),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum HubrisTarget {
    Direct(u32),
    Indirect,
    Call(u32),
    IndirectCall,
    Return,
}

/// Lightweight stack symbol with either DWARF or ELF symbol info
#[derive(Clone, Debug)]
pub struct HubrisStackSymbol<'a> {
    pub name: &'a str,
    pub addr: u32,
    pub goff: Option<HubrisGoff>,
}

#[derive(Clone, Debug)]
pub struct HubrisStackFrame<'a> {
    pub cfa: u32,
    pub sym: Option<HubrisStackSymbol<'a>>,
    pub registers: BTreeMap<ARMRegister, u32>,
    pub inlined: Option<Vec<HubrisInlined<'a>>>,
}

#[derive(Clone, Debug)]
pub struct HubrisSrc {
    pub file: String,
    pub directory: Option<String>,
    pub comp_directory: Option<String>,
    pub line: u64,
}

impl HubrisSrc {
    pub fn fullpath(&self) -> String {
        format!(
            "{}{}{}",
            if let Some(dir) = &self.comp_directory {
                format!("{}/", dir)
            } else {
                "".to_string()
            },
            if let Some(dir) = &self.directory {
                format!("{}/", dir)
            } else {
                "".to_string()
            },
            self.file
        )
    }
}

#[derive(Clone, Debug)]
pub struct HubrisModule {
    pub name: String,
    pub object: u32,
    pub task: HubrisTask,
    pub textbase: u32,
    pub textsize: u32,
    pub memsize: u32,
    pub heapbss: (Option<u32>, Option<u32>),
    pub iface: Option<Interface>,
}

impl HubrisModule {
    pub fn lookup_struct_byname<'a>(
        &self,
        hubris: &'a HubrisArchive,
        name: &str,
    ) -> Result<Option<&'a HubrisStruct>> {
        match hubris.structs_byname.get_vec(name) {
            Some(v) => {
                let m = hubris
                    .dedup(v.iter().filter(|g| g.object == self.object))?;

                if m.len() > 1 {
                    let all = m
                        .iter()
                        .map(|g| {
                            let ns = hubris.structs.get(g).unwrap().namespace;

                            if let Ok(Some(name)) = hubris
                                .namespaces
                                .to_full_name(ns, &name.to_string())
                            {
                                format!("{name} as {g}")
                            } else {
                                format!("{g}")
                            }
                        })
                        .collect::<Vec<_>>()
                        .join(", ");

                    Err(anyhow!(
                        "{name} matches more than one structure: {all}"
                    ))
                } else if m.is_empty() {
                    Ok(None)
                } else {
                    Ok(Some(hubris.structs.get(&m[0]).unwrap()))
                }
            }
            _ => {
                if let Some(scoped) = try_scoped(name, &hubris.structs_byname) {
                    self.lookup_struct_byname(hubris, scoped)
                } else {
                    Ok(None)
                }
            }
        }
    }

    pub fn lookup_enum_byname<'a>(
        &self,
        hubris: &'a HubrisArchive,
        name: &str,
    ) -> Result<Option<&'a HubrisEnum>> {
        match hubris.enums_byname.get_vec(name) {
            Some(v) => {
                let m = hubris
                    .dedup(v.iter().filter(|g| g.object == self.object))?;

                if m.len() > 1 {
                    let all = m
                        .iter()
                        .map(|g| {
                            let n = hubris.enums.get(g).unwrap().namespace;

                            if let Ok(Some(name)) = hubris
                                .namespaces
                                .to_full_name(n, &name.to_string())
                            {
                                format!("{name} as {g}")
                            } else {
                                format!("{g}")
                            }
                        })
                        .collect::<Vec<_>>()
                        .join(", ");

                    Err(anyhow!("{name} matches more than one enum: {all}"))
                } else if m.is_empty() {
                    Ok(None)
                } else {
                    Ok(Some(hubris.enums.get(&m[0]).unwrap()))
                }
            }
            _ => {
                if let Some(scoped) = try_scoped(name, &hubris.enums_byname) {
                    self.lookup_enum_byname(hubris, scoped)
                } else {
                    Ok(None)
                }
            }
        }
    }
}

#[derive(Copy, Clone, Debug, Default)]
pub struct HubrisPrintFormat {
    pub indent: usize,
    pub newline: bool,
    pub hex: bool,
    pub no_name: bool,
    pub interpret_as_c_string: bool,
}

impl HubrisPrintFormat {
    pub fn delim(&self) -> &'static str {
        if self.newline {
            "\n"
        } else {
            " "
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum HubrisValidate {
    ArchiveMatch,
    Booted,
}

//
// We want to track our external regions because we want to be able to identify
// them, but this requires us knowing what addresses our external regions map
// to.  All of this would be fine, but there exists (or existed, anyway) a
// window of time in which external regions existed, but the archive did not
// contain the necessary metadata to be able to perform this correlation.  To
// reassemble this, we follow a sleazy heuristic, observing that tasks that had
// external regions in this window of time also enabled DMA to those external
// regions -- and only to those regions.  So if we do not have our necessary
// metadata (specifically, the `memory.toml` file), we perform this crude
// reconstruction.
//
#[derive(Debug)]
enum ExternRegions {
    /// We have not yet loaded any external regions
    Unloaded,

    /// We have the metadata to correlate addresses to regions
    ByAddress(HashMap<u32, String>),

    /// We only know which tasks have which regions -- not the addresses of
    /// those regions
    ByTask(HashSet<HubrisTask>),
}

impl ExternRegions {
    fn new() -> Self {
        ExternRegions::Unloaded
    }

    ///
    /// Load external regions from the archive.  Based on the presence of
    /// archive metadata, this will construct the necessary structures to
    /// correlate address to external region.
    ///
    fn load(
        hubris: &HubrisArchive,
        archive: &mut zip::ZipArchive<Cursor<&[u8]>>,
        config: &HubrisConfig,
    ) -> Result<Self> {
        match archive.by_name("memory.toml") {
            Ok(mut file) => {
                let mut memory = String::new();
                file.read_to_string(&mut memory)?;
                let all_memories: IndexMap<String, Vec<HubrisMemoryRegion>> =
                    toml::from_slice(memory.as_bytes())?;

                //
                // We have our memory metadata but it includes all memories
                // (not just extern regions); iterate over our tasks to find
                // the extern regions, and then insert only those into our map.
                //
                let mut map = HashMap::new();
                let mut all_extern_regions = HashSet::new();

                for (_, task) in &config.tasks {
                    if let Some(extern_regions) = &task.extern_regions {
                        for region in extern_regions {
                            all_extern_regions.insert(region.clone());
                        }
                    }
                }

                for (name, memories) in all_memories {
                    if all_extern_regions.contains(&name) {
                        //
                        // Note that we are inserting all memories into our
                        // map (that is, regardless of our image name), with
                        // the (hopefully reasonable?) assumption that a given
                        // address can't map to two different memories across
                        // images.
                        //
                        for memory in memories {
                            map.insert(memory.address, name.clone());
                        }
                    }
                }

                Ok(Self::ByAddress(map))
            }
            _ => {
                let mut set = HashSet::new();

                for (name, task) in &config.tasks {
                    if task.extern_regions.is_some() {
                        set.insert(*hubris.lookup_task(name).unwrap());
                    }
                }

                Ok(Self::ByTask(set))
            }
        }
    }

    ///
    /// Returns `true` iff the specified address is in an external region.
    /// We only require the `task` and the `dma` property in the event that
    /// we don't have the metadata in the archive.
    ///
    fn external(&self, address: u32, task: HubrisTask, dma: bool) -> bool {
        match self {
            ExternRegions::ByAddress(map) => map.contains_key(&address),
            ExternRegions::ByTask(set) => dma && set.contains(&task),
            ExternRegions::Unloaded => panic!("no archive has been loaded"),
        }
    }

    ///
    /// For a given address, return the name of the external region if known.
    /// If the address is not in an external region or the archive lacks the
    /// necessary metadata, this will return `None`.
    ///
    fn lookup_byaddr(&self, address: u32) -> Option<&String> {
        match self {
            ExternRegions::ByAddress(map) => map.get(&address),
            _ => None,
        }
    }
}

//
// When looking up a type by name, it is possible that we are looking for a
// type that is present, but more explicitly scoped than the name we're using
// to look it up.  As with many things, there are several ways to solve this:
// the Right Way, the Wrong Way -- and the Max Power way.  It should be no
// surprise which way we opt for here.
//
fn try_scoped<'a>(
    name: &'a str,
    map: &'a MultiMap<String, HubrisGoff>,
) -> Option<&'a str> {
    let search = name.replace('<', "<.*::");

    if search == name {
        None
    } else {
        use regex::Regex;

        let expr = format!("^{}$", search);
        let re = Regex::new(&expr).unwrap();

        let matched: Vec<_> =
            map.keys().filter(|&n| re.is_match(n)).collect::<_>();

        if matched.len() == 1 {
            Some(matched[0])
        } else {
            None
        }
    }
}

fn dwarf_name<'a>(
    dwarf: &'a gimli::Dwarf<gimli::EndianSlice<gimli::LittleEndian>>,
    value: gimli::AttributeValue<gimli::EndianSlice<gimli::LittleEndian>>,
) -> Option<&'a str> {
    match value {
        gimli::AttributeValue::DebugStrRef(strref) => {
            let dstring = dwarf.debug_str.get_str(strref).ok()?;
            let ddstring = str::from_utf8(dstring.slice()).ok()?;
            Some(ddstring)
        }
        _ => None,
    }
}

/// Demangles `name` as a Rust symbol.
fn demangle_name(name: &str) -> String {
    // Note: "alternate mode" # causes rustc_demangle to leave off the ugly hash
    // values on functions.
    format!("{:#}", rustc_demangle::demangle(name))
}
