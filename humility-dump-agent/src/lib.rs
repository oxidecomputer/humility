// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! Traits and data structures for Hubris dump support
//!
//! This crate defines the `DumpAgent` and `DumpAgentExt` traits, which allows
//! us to execute dump-related actions.  It also includes two implementers of
//! the `DumpAgent` trait:
//!
//! - `UdpDumpAgent`, which implements the trait via the `dump-agent` task on
//!   the target.
//! - `HiffyDumpAgent`, which implements the trait via the `DumpAgent` Idol
//!   interface.  Note that this implementation may _also_ use the network!
//!   However, unlike `UdpDumpAgent`, it would be using the `udprpc` task to
//!   execute HIF calls; our goal is to remove that task from production images.

use anyhow::{anyhow, bail, Context, Result};
use core::mem::size_of;
use humility::{core::Core, hubris::HubrisFlashMap, msg};
use humility_arch_arm::ARMRegister;
use humpty::{
    DumpAreaHeader, DumpRegister, DumpSegment, DumpSegmentData,
    DumpSegmentHeader, DumpTask,
};
use indexmap::IndexMap;
use indicatif::{HumanBytes, HumanDuration, ProgressBar, ProgressStyle};
use num_traits::FromPrimitive;
use std::{
    collections::{BTreeMap, HashMap},
    path::Path,
    time::Instant,
};
use zerocopy::FromBytes;

mod hiffy;
mod udp;

pub use hiffy::HiffyDumpAgent;
pub use udp::UdpDumpAgent;

fn parse_dump_header(buf: &[u8]) -> Result<(DumpAreaHeader, Option<DumpTask>)> {
    let header = DumpAreaHeader::read_from_prefix(buf)
        .ok_or_else(|| anyhow!("failed to parse dump area"))?;

    if header.magic != humpty::DUMP_MAGIC {
        bail!("bad magic at in dump area: {header:x?}");
    }

    //
    // This is a little sleazy (or maybe even a lot sleazy?):  we know that if
    // we have task dump here, the number of segments is sufficiently low to
    // assure that we will have also slurped our task information -- and we
    // know that our task information will immediately follow our segment
    // headers.  (We take some of the sting off of this by at least checking
    // the sizes when we read the dump headers -- but we still rely on the
    // fact that any task information is dumped first.)  Gone fishin'...
    //
    let size = size_of::<DumpAreaHeader>();
    let toffs =
        size + header.nsegments as usize * size_of::<DumpSegmentHeader>();

    let task = if toffs < buf.len() {
        if let Some(DumpSegment::Task(t)) = DumpSegment::from(&buf[toffs..]) {
            Some(t)
        } else {
            None
        }
    } else {
        None
    };

    Ok((header, task))
}

/// Calls parse_dump_header, with nicer error reporting for the specific index
fn parse_dump_header_index(
    index: usize,
    buf: &[u8],
) -> Result<(DumpAreaHeader, Option<DumpTask>)> {
    parse_dump_header(buf).with_context(|| format!("parsing dump area {index}"))
}

pub enum DumpArea {
    ByIndex(usize),
    ByAddress(u32),
}

////////////////////////////////////////////////////////////////////////////////

//
// When using the dump agent, we create our own ersatz Core
//
pub struct DumpAgentCore {
    flash: HubrisFlashMap,
    ram_regions: BTreeMap<u32, Vec<u8>>,
    registers: HashMap<ARMRegister, u32>,
}

impl DumpAgentCore {
    pub fn new(flash: HubrisFlashMap) -> DumpAgentCore {
        Self {
            flash,
            ram_regions: Default::default(),
            registers: Default::default(),
        }
    }

    pub fn add_ram_region(&mut self, addr: u32, contents: Vec<u8>) {
        self.ram_regions.insert(addr, contents);
    }

    pub fn add_register(&mut self, reg: ARMRegister, val: u32) {
        self.registers.insert(reg, val);
    }

    fn read_flash(&self, addr: u32, data: &mut [u8]) -> Result<()> {
        self.flash
            .read(addr, data)
            .ok_or_else(|| anyhow!("address 0x{:08x} not found", addr))
    }

    fn read(&mut self, addr: u32, data: &mut [u8]) -> Result<()> {
        if let Some((&base, contents)) =
            self.ram_regions.range(..=addr).next_back()
        {
            if base > addr || base + (contents.len() as u32) <= addr {
                //
                // We don't have this in RAM -- pull it out of flash.
                //
                return self.read_flash(addr, data);
            }

            let start = (addr - base) as usize;

            if start + data.len() <= contents.len() {
                //
                // This region -- and only this region -- contains our RAM.
                // Copy it and leave.
                //
                data.copy_from_slice(&contents[start..start + data.len()]);
                return Ok(());
            }

            //
            // This region contains our RAM, but there is more.  Copy the bit
            // that we want and recurse.
            //
            let len = contents.len() - start;
            data[..len].copy_from_slice(&contents[start..contents.len()]);
            self.read(addr + len as u32, &mut data[len..])
        } else {
            self.read_flash(addr, data)
        }
    }

    pub fn process_dump(
        &mut self,
        header: &DumpAreaHeader,
        dump: &Vec<u8>,
        task: Option<DumpTask>,
    ) -> Result<()> {
        let nsegments = header.nsegments;
        let mut offset = nsegments as usize * size_of::<DumpSegmentHeader>();

        if offset > dump.len() {
            bail!("in situ dump is short; missing {nsegments} segments");
        }

        if offset == dump.len() {
            bail!("in situ dump is empty");
        }

        while offset < dump.len() {
            let segment = match DumpSegment::from(&dump[offset..]) {
                Some(segment) => segment,
                None => {
                    bail!("short read at offset {offset}");
                }
            };

            match segment {
                DumpSegment::Task(t) => {
                    match task {
                        None => {
                            bail!("found unexpected task {t:?}");
                        }
                        Some(task) if task != t => {
                            bail!(
                                "task mismatch: found {t:?}, expected {task:?}"
                            );
                        }
                        _ => {}
                    }

                    offset += size_of::<DumpTask>();
                    continue;
                }

                DumpSegment::Register(reg) => {
                    //
                    // These are register values; slurp them and continue.
                    //
                    if let Some(register) = ARMRegister::from_u16(reg.register)
                    {
                        self.add_register(register, reg.value);
                    } else {
                        let r = reg.register;
                        bail!(
                            "unrecognized register {r:#x} at offset {offset}"
                        );
                    }

                    offset += size_of::<DumpRegister>();
                    continue;
                }

                DumpSegment::Data(data) => {
                    offset += size_of::<DumpSegmentData>();

                    let len = data.uncompressed_length as usize;

                    let mut contents = vec![0; len];
                    let limit = offset + data.compressed_length as usize;

                    humpty::DumpLzss::decompress(
                        lzss::SliceReader::new(&dump[offset..limit]),
                        lzss::SliceWriter::new(&mut contents),
                    )?;

                    self.add_ram_region(
                        data.address,
                        contents[0..len].to_vec(),
                    );
                    offset = limit;

                    while offset < dump.len()
                        && dump[offset] == humpty::DUMP_SEGMENT_PAD
                    {
                        offset += 1;
                    }
                }

                DumpSegment::Unknown(signature) => {
                    bail!(
                        "unrecognized data with signature \
                    {signature:x?} at offset {offset}"
                    );
                }
            }
        }

        Ok(())
    }
}

impl Core for DumpAgentCore {
    fn info(&self) -> (String, Option<String>) {
        panic!("unexpected call to DumpAgentCore info");
    }

    fn read_8(&mut self, addr: u32, data: &mut [u8]) -> Result<()> {
        self.read(addr, data)
    }

    fn read_reg(&mut self, reg: ARMRegister) -> Result<u32> {
        match self.registers.get(&reg) {
            Some(val) => Ok(*val),
            None => bail!("unexpected read from register {reg:?}"),
        }
    }

    fn write_reg(&mut self, reg: ARMRegister, _value: u32) -> Result<()> {
        bail!("cannot write register {} over dump agent", reg);
    }

    fn write_word_32(&mut self, _addr: u32, _data: u32) -> Result<()> {
        bail!("cannot write a word over dump agent");
    }

    fn write_8(&mut self, _addr: u32, _data: &[u8]) -> Result<()> {
        bail!("cannot write a byte over dump agent");
    }

    fn halt(&mut self) -> Result<()> {
        bail!("unexpected call to halt");
    }

    fn run(&mut self) -> Result<()> {
        bail!("unexpected call to run");
    }

    fn step(&mut self) -> Result<()> {
        bail!("can't step over dump agent");
    }

    fn init_swv(&mut self) -> Result<()> {
        bail!("cannot enable SWV over dump agent");
    }

    fn read_swv(&mut self) -> Result<Vec<u8>> {
        bail!("cannot read SWV over dump agent");
    }

    fn load(&mut self, _path: &Path) -> Result<()> {
        bail!("cannot load flash over dump agent");
    }

    fn reset(&mut self) -> Result<()> {
        bail!("cannot reset over dump agent");
    }

    fn reset_and_halt(&mut self, _dur: std::time::Duration) -> Result<()> {
        bail!("cannot reset over dump agent");
    }

    fn wait_for_halt(&mut self, _dur: std::time::Duration) -> Result<()> {
        bail!("cannot wait for halt over dump agent");
    }
}

////////////////////////////////////////////////////////////////////////////////

/// Trait which abstracts away dump agent interfaces
pub trait DumpAgent {
    fn core(&mut self) -> &mut dyn Core;

    /// Initializes the dump regions
    fn initialize_dump(&mut self) -> Result<()>;

    /// Initializes a set of segments defined as `(address, length)` tuples
    fn initialize_segments(&mut self, segments: &[(u32, u32)]) -> Result<()>;

    /// Reinitializes the dump memory starting at the given area index
    fn reinitialize_dump_from(&mut self, i: u8) -> Result<()>;

    /// Dumps a single task
    ///
    /// This uses using the kernel and supervisor to copy data out of the task,
    /// so an external (or emulated) dumper is not necessary; unfortunately,
    /// this also means that we are unable to dump the supervisor.
    ///
    /// Returns the area index in which the result begins.
    fn dump_task(&mut self, task_index: u32) -> Result<u8>;

    /// Dumps a sub-region of single task
    ///
    /// This uses using the kernel and supervisor to copy data out of the task,
    /// so an external (or emulated) dumper is not necessary; unfortunately,
    /// this also means that we are unable to dump the supervisor.
    ///
    /// Returns the area index in which the result begins.
    fn dump_task_region(
        &mut self,
        task_index: u32,
        start: u32,
        length: u32,
    ) -> Result<u8>;

    /// Kicks off a dump
    fn take_dump(&mut self) -> Result<()>;

    /// General-purpose dump reading abstraction
    ///
    /// This function is given an iterator over areas, which are represented as
    /// `index, offset` tuples.  It reads from those areas, chunking internally
    /// as needed for efficiency.
    ///
    /// In the implementation, reading should terminate under three conditions:
    /// - the iterator runs out
    /// - the `cont` callback returns `false` (in which case the data for which
    ///   it returned `false` is *not* returned)
    /// - the read operation returns an `InvalidArea` error
    ///
    /// The latter is a little subtle, so it merits further explanation.  In
    /// some cases, we may optimistically read dump areas that _may not exist_,
    /// so we rely on this error to tell us when we've gone off the edge of the
    /// map.  As an example, this occurs when using `--dump-agent-status` to
    /// read every single area.
    ///
    /// The `cont` callback can also be used as a progress tracker.  Each time
    /// it is called, it takes an incremental number of bytes that have been
    /// read in the most recent operation.
    #[allow(clippy::type_complexity)]
    fn read_generic(
        &mut self,
        areas: &mut dyn Iterator<Item = (u8, u32)>,
        cont: &mut dyn FnMut(u8, u32, &[u8]) -> Result<bool>,
    ) -> Result<Vec<Vec<u8>>>;
}

/// This is the trait which does the vast majority of dumping
///
/// It's expected to use `read_generic` from a `DumpAgent`
pub trait DumpAgentExt {
    /// Helper function to make calling `read_generic` more pleasant
    fn read_generic<I, F>(&mut self, areas: I, cont: F) -> Result<Vec<Vec<u8>>>
    where
        I: Iterator<Item = (u8, u32)>,
        F: FnMut(u8, u32, &[u8]) -> Result<bool>;

    ////////////////////////////////////////////////////////////////////////
    // Everything beyond this point is implemented in terms of `read_generic`

    /// Reads the root header
    fn read_dump_header(&mut self) -> Result<DumpAreaHeader> {
        self.read_dump_header_at(0)
    }

    /// Reads the header from the given dump area
    fn read_dump_header_at(&mut self, i: u8) -> Result<DumpAreaHeader> {
        let val = self.read_dump_area_start(i)?;
        let (header, _task) = parse_dump_header_index(i as usize, &val)?;
        Ok(header)
    }

    /// Reads 256 bytes from the start of the given dump area
    ///
    /// This is enough data to extract the header
    fn read_dump_area_start(&mut self, i: u8) -> Result<Vec<u8>> {
        // Read the header of this dump area
        let mut val =
            self.read_generic(std::iter::once((i, 0)), |_, _, _| Ok(true))?;
        assert_eq!(val.len(), 1);
        Ok(val.pop().unwrap())
    }

    /// Reads back a single dump area by index
    fn read_dump_area(
        &mut self,
        index: u8,
        progress: &mut dyn FnMut(usize),
    ) -> Result<(DumpAreaHeader, Vec<u8>)> {
        let val = self.read_dump_area_start(index)?;
        let header = DumpAreaHeader::read_from_prefix(val.as_slice())
            .ok_or_else(|| anyhow!("failed to read parse header"))?;

        if header.magic != humpty::DUMP_MAGIC {
            bail!("bad magic at dump offset {:#x}: {:x?}", index, header);
        }

        let size = size_of::<DumpAreaHeader>();

        let mut out = vec![];
        if header.written > size as u32 {
            let max = val.len().min(header.written as usize);
            let data = val[size..max].to_owned();
            progress(data.len());
            out.push(data);
        }

        let chunk_size = val.len();
        assert_eq!(chunk_size, 256);

        let mut chunks = vec![];
        let mut offset = chunk_size.min(header.written as usize);
        while offset < header.written as usize {
            let len = chunk_size.min(header.written as usize - offset);
            chunks.push(offset.try_into().unwrap());
            offset += len;
        }

        // Read all of the region, one 256-byte chunk at a time
        out.extend(self.read_generic(
            chunks.into_iter().map(|offset| (index, offset)),
            |_index, _offset, result| {
                progress(result.len());
                Ok(true)
            },
        )?);

        // Collect chunks into a single vec
        let mut out_data: Vec<u8> = out.into_iter().flatten().collect();

        // Because we read in chunks, we may have extra data at the end here
        out_data.resize(offset, 0u8);

        Ok((header, out_data))
    }

    /// Reads dump headers from the target
    ///
    /// If `raw` is set, includes headers configured as `DUMPER_NONE`;
    /// otherwise, stops once the first `DUMPER_NONE` header is encountered.
    fn read_dump_headers(
        &mut self,
        raw: bool,
    ) -> Result<Vec<(DumpAreaHeader, Option<DumpTask>)>> {
        //
        // Iterate over dump area headers.  Once we've hit an area that hasn't
        // been dumped to, or an invalid area (denoting that all areas are
        // full), we're done...
        //
        let results = self.read_generic(
            (0..).map(|index| (index, 0)),
            |index, _offset, val| {
                let (header, _task) =
                    parse_dump_header_index(index as usize, val)?;
                if !raw && header.dumper == humpty::DUMPER_NONE {
                    Ok(false)
                } else {
                    Ok(true)
                }
            },
        )?;

        results
            .into_iter()
            .enumerate()
            .map(|(i, r)| parse_dump_header_index(i, &r))
            .collect::<Result<Vec<_>>>()
    }

    fn read_dump(
        &mut self,
        area: Option<DumpArea>,
        out: &mut DumpAgentCore,
        verbose: bool,
    ) -> Result<Option<DumpTask>> {
        let (base, headers, task) = {
            // Read dump headers until the first empty header (DUMPER_NONE)
            let all = self.read_dump_headers(false)?;

            if all.len() == 0 {
                bail!("could not read dump; no dump found?");
            }

            let area = match area {
                None => None,
                Some(DumpArea::ByIndex(ndx)) => Some(ndx),
                Some(DumpArea::ByAddress(address)) => all
                    .iter()
                    .enumerate()
                    .filter(|&(_, (header, _))| header.address == address)
                    .map(|(ndx, _)| ndx)
                    .next(),
            };

            match area {
                None | Some(0) if all[0].1.is_none() => (
                    0usize,
                    all.iter()
                        .map(|(h, _t)| *h)
                        .collect::<Vec<DumpAreaHeader>>(),
                    None,
                ),

                Some(ndx) => {
                    let areas = task_areas(&all);
                    match areas.get(&ndx) {
                        None => {
                            bail!("area {ndx} is invalid (--list to list)");
                        }
                        Some((task, headers)) => {
                            (ndx, headers.clone(), Some(*task))
                        }
                    }
                }

                _ => {
                    bail!("area must be explicitly specified (--list to list)");
                }
            }
        };

        let total = headers.iter().fold(0, |sum, header| sum + header.written);

        let started = Instant::now();
        let bar = if verbose {
            let bar = ProgressBar::new(total as u64);
            bar.set_style(ProgressStyle::default_bar().template(
                "humility: pulling [{bar:30}] {bytes}/{total_bytes}",
            ));
            Some(bar)
        } else {
            None
        };

        let mut count = 0;
        let mut contents = vec![];
        for (ndx, _) in headers.iter().enumerate() {
            let index = (ndx + base).try_into().unwrap();
            let (_header, data) = self.read_dump_area(index, &mut |size| {
                count += size;
                if let Some(bar) = &bar {
                    bar.set_position(count as u64);
                }
            })?;
            contents.extend(data.into_iter());
        }

        if let Some(bar) = &bar {
            bar.finish_and_clear();
        }

        if verbose {
            msg!(
                "pulled {} in {}",
                HumanBytes(total as u64),
                HumanDuration(started.elapsed())
            );
        }

        //
        // We have a dump!  On to processing...
        //
        out.process_dump(&headers[0], &contents, task)?;

        Ok(task)
    }
}

impl DumpAgentExt for &mut dyn DumpAgent {
    fn read_generic<I, F>(
        &mut self,
        mut areas: I,
        mut cont: F,
    ) -> Result<Vec<Vec<u8>>>
    where
        I: Iterator<Item = (u8, u32)>,
        F: FnMut(u8, u32, &[u8]) -> Result<bool>,
    {
        DumpAgent::read_generic(*self, &mut areas, &mut cont)
    }
}

impl<'a> DumpAgentExt for Box<dyn DumpAgent + 'a> {
    fn read_generic<I, F>(
        &mut self,
        mut areas: I,
        mut cont: F,
    ) -> Result<Vec<Vec<u8>>>
    where
        I: Iterator<Item = (u8, u32)>,
        F: FnMut(u8, u32, &[u8]) -> Result<bool>,
    {
        DumpAgent::read_generic(self.as_mut(), &mut areas, &mut cont)
    }
}

//
// Because single task dumps can spread over adjacent areas, they can be a
// little tedious to process; iterate over all dump areas and return a map of
// area indices to a task/vector of headers tuple.
//
pub fn task_areas(
    headers: &[(DumpAreaHeader, Option<DumpTask>)],
) -> IndexMap<usize, (DumpTask, Vec<DumpAreaHeader>)> {
    let mut rval = IndexMap::new();

    if headers[0].1.is_none() {
        return rval;
    }

    let mut current = 0;
    let mut areas = vec![headers[0].0];

    for (ndx, (header, task)) in headers.iter().enumerate().skip(1) {
        match task {
            None => {
                //
                // If the header indicates that it's been written, it's
                // a continuation of our previous header -- otherwise, we're
                // done processing entirely.
                //
                if header.dumper != humpty::DUMPER_NONE {
                    areas.push(*header);
                } else {
                    break;
                }
            }
            Some(_task) => {
                //
                // We have hit a new task dump, so record what we have seen
                // for our current task dump, and start a new one.
                //
                rval.insert(current, (headers[current].1.unwrap(), areas));
                current = ndx;
                areas = vec![*header];
            }
        }
    }

    rval.insert(current, (headers[current].1.unwrap(), areas));
    rval
}

////////////////////////////////////////////////////////////////////////////////
