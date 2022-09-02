// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use probe_rs::MemoryInterface;
use probe_rs::{flashing, Probe};

use anyhow::{anyhow, bail, ensure, Result};

use crate::arch::Register;
use crate::arch_rv::RVRegister;
use crate::hubris::*;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::convert::TryInto;
use std::fmt;
use std::fs;
use std::io::Read;
use std::io::Write;
use std::net::TcpStream;
use std::path::Path;
use std::rc::Rc;
use std::str;
use std::time::Duration;
use std::time::Instant;

use goblin::elf::Elf;

pub trait Core {
    fn info(&self) -> (String, Option<String>);
    fn read_word_32(&mut self, addr: u32) -> Result<u32>;
    //TODO should check on first read for unhalted abstract access
    fn read_8(&mut self, addr: u32, data: &mut [u8]) -> Result<()>;
    //TODO should check on first read for unhalted abstract access
    fn read_reg(&mut self, reg: Register) -> Result<u32>;
    fn write_reg(&mut self, reg: Register, value: u32) -> Result<()>;
    fn init_swv(&mut self) -> Result<()>;
    fn read_swv(&mut self) -> Result<Vec<u8>>;
    fn write_word_32(&mut self, addr: u32, data: u32) -> Result<()>;
    fn write_8(&mut self, addr: u32, data: &[u8]) -> Result<()>;

    fn halt(&mut self) -> Result<()>;
    fn run(&mut self) -> Result<()>;
    fn step(&mut self) -> Result<()>;
    fn is_dump(&self) -> bool {
        false
    }

    fn read_word_64(&mut self, addr: u32) -> Result<u64> {
        let mut buf = [0; 8];
        self.read_8(addr, &mut buf)?;
        Ok(u64::from_le_bytes(buf))
    }

    ///
    /// Called to load a flash image.
    ///
    fn load(&mut self, path: &Path) -> Result<()>;

    /// Reset the chip
    fn reset(&mut self) -> Result<()>;

    /// Called before starting a series of operations.  May halt the target if
    /// the target does not allow operations while not halted.  Should not be
    /// intermixed with [`halt`]/[`run`].
    fn op_start(&mut self) -> Result<()> {
        Ok(())
    }

    /// Called after completing a series of operations.  May run the target if
    /// the target does not allow operations while not halted.  Should not be
    /// intermixed with [`halt`]/[`run`].
    fn op_done(&mut self) -> Result<()> {
        Ok(())
    }
}

pub struct UnattachedCore {
    pub probe: probe_rs::Probe,
    pub identifier: String,
    pub vendor_id: u16,
    pub product_id: u16,
    pub serial_number: Option<String>,
}

impl UnattachedCore {
    fn new(
        probe: probe_rs::Probe,
        identifier: String,
        vendor_id: u16,
        product_id: u16,
        serial_number: Option<String>,
    ) -> Self {
        Self { probe, identifier, vendor_id, product_id, serial_number }
    }
}

impl Core for UnattachedCore {
    fn info(&self) -> (String, Option<String>) {
        let ident = format!(
            "{}, VID {:04x}, PID {:04x}",
            self.identifier, self.vendor_id, self.product_id
        );

        (ident, self.serial_number.clone())
    }

    fn read_word_32(&mut self, _addr: u32) -> Result<u32> {
        bail!("Unimplemented when unattached!");
    }

    fn read_8(&mut self, _addr: u32, _data: &mut [u8]) -> Result<()> {
        bail!("Unimplemented when unattached!");
    }

    fn read_reg(&mut self, _reg: Register) -> Result<u32> {
        bail!("Unimplemented when unattached!");
    }

    fn write_reg(&mut self, _reg: Register, _value: u32) -> Result<()> {
        bail!("Unimplemented when unattached!");
    }

    fn write_word_32(&mut self, _addr: u32, _data: u32) -> Result<()> {
        bail!("Unimplemented when unattached!");
    }

    fn write_8(&mut self, _addr: u32, _data: &[u8]) -> Result<()> {
        bail!("Unimplemented when unattached!");
    }

    fn halt(&mut self) -> Result<()> {
        bail!("Unimplemented when unattached!");
    }

    fn run(&mut self) -> Result<()> {
        bail!("Unimplemented when unattached!");
    }

    fn step(&mut self) -> Result<()> {
        bail!("Unimplemented when unattached!");
    }

    fn init_swv(&mut self) -> Result<()> {
        bail!("Unimplemented when unattached!");
    }

    fn read_swv(&mut self) -> Result<Vec<u8>> {
        bail!("Unimplemented when unattached!");
    }

    fn is_dump(&self) -> bool {
        false
    }

    fn load(&mut self, _path: &Path) -> Result<()> {
        bail!("Unimplemented when unattached!");
    }

    fn reset(&mut self) -> Result<()> {
        self.probe.target_reset_assert()?;

        // The closest available documentation on hold time is
        // a comment giving a timeout
        // https://open-cmsis-pack.github.io/Open-CMSIS-Pack-Spec/main/html/debug_description.html#resetHardwareDeassert
        std::thread::sleep(std::time::Duration::from_millis(1000));

        self.probe.target_reset_deassert()?;

        Ok(())
    }
}

pub struct ProbeCore {
    pub session: probe_rs::Session,
    pub identifier: String,
    pub vendor_id: u16,
    pub product_id: u16,
    pub serial_number: Option<String>,
    unhalted_reads: bool,
    halted: u32,
    unhalted_read: BTreeMap<u32, u32>,
    can_flash: bool,
}

impl ProbeCore {
    fn new(
        session: probe_rs::Session,
        identifier: String,
        vendor_id: u16,
        product_id: u16,
        serial_number: Option<String>,
        unhalted_reads: bool,
        can_flash: bool,
    ) -> Self {
        Self {
            session,
            identifier,
            vendor_id,
            product_id,
            serial_number,
            unhalted_reads,
            halted: 0,
            unhalted_read: crate::arch_arm::unhalted_read_regions(),
            can_flash,
        }
    }

    fn halt_and_read(
        &mut self,
        mut func: impl FnMut(&mut probe_rs::Core) -> Result<()>,
    ) -> Result<()> {
        let mut core = self.session.core(0)?;

        if self.unhalted_reads {
            func(&mut core)
        } else {
            let halted = if self.halted == 0 && !core.core_halted()? {
                core.halt(std::time::Duration::from_millis(1000))?;
                true
            } else {
                false
            };

            let rval = func(&mut core);

            if halted {
                core.run()?;
            }

            rval
        }
    }
}

pub const CORE_MAX_READSIZE: usize = 65536; // 64K ought to be enough for anyone

#[rustfmt::skip::macros(anyhow, bail)]
impl Core for ProbeCore {
    fn info(&self) -> (String, Option<String>) {
        let ident = format!(
            "{}, VID {:04x}, PID {:04x}",
            self.identifier, self.vendor_id, self.product_id
        );

        (ident, self.serial_number.clone())
    }

    fn read_word_32(&mut self, addr: u32) -> Result<u32> {
        log::trace!("reading word at {:x}", addr);
        let mut rval = 0;

        if let Some(range) = self.unhalted_read.range(..=addr).next_back() {
            if addr + 4 < range.0 + range.1 {
                let mut core = self.session.core(0)?;
                return Ok(core.read_word_32(addr)?);
            }
        }

        self.halt_and_read(|core| {
            rval = core.read_word_32(addr)?;
            Ok(())
        })?;

        Ok(rval)
    }

    fn read_8(&mut self, addr: u32, data: &mut [u8]) -> Result<()> {
        if data.len() > CORE_MAX_READSIZE {
            bail!("read of {} bytes at 0x{:x} exceeds max of {}",
                data.len(), addr, CORE_MAX_READSIZE);
        }

        if let Some(range) = self.unhalted_read.range(..=addr).next_back() {
            if addr + (data.len() as u32) < range.0 + range.1 {
                let mut core = self.session.core(0)?;
                return Ok(core.read_8(addr, data)?);
            }
        }

        self.halt_and_read(|core| Ok(core.read_8(addr, data)?))
    }

    fn read_reg(&mut self, reg: Register) -> Result<u32> {
        let mut core = self.session.core(0)?;
        let reg_id = Register::to_u16(&reg).unwrap();

        use num_traits::ToPrimitive;

        Ok(core.read_core_reg(Into::<probe_rs::CoreRegisterAddress>::into(
            reg_id,
        ))?)
    }

    //TODO
    fn write_reg(&mut self, reg: Register, value: u32) -> Result<()> {
        let mut core = self.session.core(0)?;
        let mut reg_id = Register::to_u16(&reg).unwrap();

        //See table 3.3 in the riscv debug spec.
        //(https://raw.githubusercontent.com/riscv/riscv-debug-spec/master/riscv-debug-stable.pdf#table.3.3)
        //The pc is read through the DPC register
        if let Register::RiscV(rv_reg) = reg {
            reg_id += 0x1000;
            if rv_reg == RVRegister::PC {
                reg_id = 0x7b1;
            }
        }
        use num_traits::ToPrimitive;

        core.write_core_reg(
            Into::<probe_rs::CoreRegisterAddress>::into(reg_id),
            value,
        )?;

        Ok(())
    }

    fn write_word_32(&mut self, addr: u32, data: u32) -> Result<()> {
        let mut core = self.session.core(0)?;
        core.write_word_32(addr, data)?;
        Ok(())
    }

    fn write_8(&mut self, addr: u32, data: &[u8]) -> Result<()> {
        let mut core = self.session.core(0)?;
        core.write_8(addr, data)?;
        Ok(())
    }

    fn halt(&mut self) -> Result<()> {
        if self.halted == 0 {
            let mut core = self.session.core(0)?;
            core.halt(std::time::Duration::from_millis(1000))?;
        }

        self.halted += 1;
        Ok(())
    }

    fn run(&mut self) -> Result<()> {
        self.halted -= 1;

        if self.halted == 0 {
            let mut core = self.session.core(0)?;
            core.run()?;
        }

        Ok(())
    }

    fn step(&mut self) -> Result<()> {
        let mut core = self.session.core(0)?;
        core.step()?;
        Ok(())
    }
    //TODO
    fn init_swv(&mut self) -> Result<()> {
        //TODO ensure core is arm

        use probe_rs::architecture::arm::swo::SwoConfig;

        let config = SwoConfig::new(0).set_baud(2_000_000);
        self.session.setup_swv(0, &config)?;

        //
        // Because the probe can have sticky errors, we perform one read
        // (and discard the results) to assure that any further errors
        // are legit.
        //
        let _discard = self.session.read_swo();
        Ok(())
    }

    fn read_swv(&mut self) -> Result<Vec<u8>> {
        Ok(self.session.read_swo()?)
    }

    fn load(&mut self, path: &Path) -> Result<()> {
        #[derive(Debug, Default)]
        struct LoadProgress {
            /// total bytes that need to be erased
            total_erase: usize,

            /// bytes that have been erased
            erased: usize,

            /// total bytes that need to be written
            total_write: usize,

            /// number of bytes that have been written
            written: usize,
        }

        use indicatif::{ProgressBar, ProgressStyle};

        if !self.can_flash {
            bail!("cannot flash without explicitly attaching to flash");
        }

        let progress =
            Rc::new(RefCell::new(LoadProgress { ..Default::default() }));

        let bar = ProgressBar::new(0);

        let progress = flashing::FlashProgress::new(move |event| match event {
            flashing::ProgressEvent::Initialized { flash_layout } => {
                progress.borrow_mut().total_erase = flash_layout
                    .sectors()
                    .iter()
                    .map(|s| s.size() as usize)
                    .sum();

                progress.borrow_mut().total_write = flash_layout
                    .pages()
                    .iter()
                    .map(|s| s.size() as usize)
                    .sum();

                bar.set_style(ProgressStyle::default_bar().template(
                    "humility: erasing [{bar:30}] {bytes}/{total_bytes}",
                ));
                bar.set_length(progress.borrow().total_erase as u64);
            }

            flashing::ProgressEvent::SectorErased { size, .. } => {
                progress.borrow_mut().erased += size as usize;
                bar.set_position(progress.borrow().erased as u64);
            }

            flashing::ProgressEvent::PageProgrammed { size, .. } => {
                let mut progress = progress.borrow_mut();

                if progress.written == 0 {
                    progress.erased = progress.total_erase;
                    bar.set_style(ProgressStyle::default_bar().template(
                        "humility: flashing [{bar:30}] {bytes}/{total_bytes}",
                    ));
                    bar.set_length(progress.total_write as u64);
                }

                progress.written += size as usize;
                bar.set_position(progress.written as u64);
            }

            flashing::ProgressEvent::FinishedProgramming => {
                bar.finish_and_clear();
            }

            _ => {}
        });

        let mut options = flashing::DownloadOptions::default();
        options.progress = Some(&progress);

        if let Err(e) = flashing::download_file_with_options(
            &mut self.session,
            path,
            flashing::Format::Hex,
            options,
        ) {
            bail!("Flash loading failed {:?}", e);
        };

        Ok(())
    }

    fn reset(&mut self) -> Result<()> {
        let mut core = self.session.core(0)?;
        core.reset()?;
        Ok(())
    }

    fn op_start(&mut self) -> Result<()> {
        if !self.unhalted_reads {
            self.halt()?;
        }

        Ok(())
    }

    fn op_done(&mut self) -> Result<()> {
        if !self.unhalted_reads {
            self.run()?;
        }

        Ok(())
    }
}

const OPENOCD_COMMAND_DELIMITER: u8 = 0x1a;
const OPENOCD_TRACE_DATA_BEGIN: &str = "type target_trace data ";
const OPENOCD_TRACE_DATA_END: &str = "\r\n";

pub struct OpenOCDCore {
    stream: TcpStream,
    swv: bool,
    last_swv: Option<Instant>,
    halted: bool,
    was_halted: bool,
}

#[rustfmt::skip::macros(anyhow, bail)]
impl OpenOCDCore {
    fn sendcmd(&mut self, cmd: &str) -> Result<String> {
        let mut rbuf = vec![0; 1024];
        let mut result = String::with_capacity(16);

        let mut str = String::from(cmd);
        str.push(OPENOCD_COMMAND_DELIMITER as char);

        self.stream.write_all(str.as_bytes())?;

        loop {
            let rval = self.stream.read(&mut rbuf)?;

            if rbuf[rval - 1] == OPENOCD_COMMAND_DELIMITER {
                result.push_str(str::from_utf8(&rbuf[0..rval - 1])?);
                break;
            }

            result.push_str(str::from_utf8(&rbuf[0..rval])?);
        }

        //
        // Surely not surprisingly, OpenOCD doesn't have a coherent way of
        // indicating that a command has failed.  We fall back to assuming
        // that any return value that contains "Error: " or "invalid command
        // name" is in fact an error.
        //
        if result.contains("Error: ") {
            Err(anyhow!("OpenOCD command \"{}\" failed with \"{}\"", cmd, result))
        } else if result.contains("invalid command name ") {
            Err(anyhow!("OpenOCD command \"{}\" invalid: \"{}\"", cmd, result))
        } else {
            Ok(result)
        }
    }

    fn new() -> Result<OpenOCDCore> {
        let addr = "127.0.0.1:6666".parse()?;
        let timeout = Duration::from_millis(100);
        let stream =
            TcpStream::connect_timeout(&addr, timeout).map_err(|_| {
                anyhow!("can't connect to OpenOCD on port 6666; is it running?")
            })?;
        let mut core = Self {
            stream,
            swv: false,
            last_swv: None,
            halted: false,
            was_halted: false,
        };
        // determine if the core is initially halted
        let _target = core.sendcmd("set targ [target current]")?;
        core.halted = match core.sendcmd("$targ curstate")?.as_str() {
            "halted" => {
                log::trace!("connected to halted core");
                true
            }
            "running" => {
                log::trace!("connected to running core");
                false
            }
            _ => {
                crate::msg!("Target in unknown state, humility will leave the core in a running state");
                false
            }
        };
        // if core was initially halted, we want to leave in a halted state after any operation
        core.was_halted = core.halted;
        Ok(core)
    }
}

#[rustfmt::skip::macros(anyhow, bail)]
impl Core for OpenOCDCore {
    fn info(&self) -> (String, Option<String>) {
        ("OpenOCD".to_string(), None)
    }

    fn read_word_32(&mut self, addr: u32) -> Result<u32> {
        self.op_start()?;
        let result = self.sendcmd(&format!("mrw 0x{:x}", addr))?;
        self.op_done()?;
        Ok(result.parse::<u32>()?)
    }

    fn read_8(&mut self, addr: u32, data: &mut [u8]) -> Result<()> {
        ensure!(
            data.len() <= CORE_MAX_READSIZE,
            "read of {} bytes at 0x{:x} exceeds max of {}",
            data.len(),
            addr,
            CORE_MAX_READSIZE
        );
        self.op_start()?;

        //
        // To read an array, we put it in a TCL variable called "output"
        // and then dump the variable.
        //
        let cmd = format!("mem2array output 8 0x{:x} {}", addr, data.len());

        self.sendcmd("array unset output")?;
        self.sendcmd(&cmd)?;

        let mut index = None;
        let mut seen = vec![false; data.len()];

        let result = self.sendcmd("return $output")?;

        //
        // Entirely on-brand, if the mem2array command has failed wildly,
        // OpenOCD won't actually return an error to us -- it will merely
        // fail to set the variable (and we will therefore fail when
        // we attempt to retrieve the variable).  If we fail to
        // retrieve the variable, we infer it to be a failure to
        // perform the read and bail explicitly.
        //
        if result.contains("no such variable") {
            bail!("read at 0x{:x} for {} bytes failed", addr, data.len());
        }

        //
        // The output here is bonkers: instead of being (merely) the array,
        // it's an (undelimited) set of 2-tuples of (index, value) -- sorted
        // in strict alphabetical order by index (!!).  (That is, index 100
        // comes before, say, index 11.)
        //
        for val in result.split(' ') {
            match index {
                None => {
                    let idx = val.parse::<usize>()?;

                    if idx >= data.len() {
                        bail!("\"{}\": illegal index {}", cmd, idx);
                    }

                    if seen[idx] {
                        bail!("\"{}\": duplicate index {}", cmd, idx);
                    }

                    seen[idx] = true;
                    index = Some(idx);
                }

                Some(idx) => {
                    data[idx] = val.parse::<u8>()?;
                    index = None;
                }
            }
        }

        for v in seen.iter().enumerate() {
            ensure!(v.1, "\"{}\": missing index {}", cmd, v.0);
        }
        self.op_done()?;

        Ok(())
    }

    fn write_reg(&mut self, _reg: Register, _val: u32) -> Result<()> {
        // This does not work right now, TODO?
        // TODO openocd does support reading though
        //
        Err(anyhow!(
            "Writing registers is not currently supported with OpenOCD"
        ))
    }

    //TODO riscv
    fn read_reg(&mut self, reg: Register) -> Result<u32> {
        let mut reg_id = Register::to_u16(&reg).unwrap();
        if let Register::RiscV(rv_reg) = reg {
            log::trace!("converting register id for openocd");
            // The general purpose registers are offset for the debug module, but in openocd they are not
            // See table 3.3 in the riscv debug spec.
            // (https://raw.githubusercontent.com/riscv/riscv-debug-spec/master/riscv-debug-stable.pdf#table.3.3)
            if !reg.is_special() {
                reg_id -= 0x1000;
            } else {
                // OpenOCD offsets all csr by 65
                // see https://github.com/openocd-org/openocd/blob/53556fcded056aa62ffdc6bf0c97bff87d891dab/src/target/riscv/gdb_regs.h#L80
                reg_id += 65
            }
            // In openocd the register for pc is at a special index
            if rv_reg == RVRegister::PC {
                reg_id = 32
            }
        }

        self.op_start()?;
        use num_traits::ToPrimitive;
        //TODO convert to name to avoid register id conflicts?
        let cmd = format!("reg {}", reg_id);
        let rval = self.sendcmd(&cmd)?;

        if let Some(line) = rval.lines().next() {
            if let Some(val) = line.split_whitespace().last() {
                if let Ok(rval) = parse_int::parse::<u32>(val) {
                    return Ok(rval);
                }
            }
        }
        self.op_done()?;

        Err(anyhow!("\"{}\": malformed return value: {:?}", cmd, rval))
    }

    //TODO probably arm specific
    fn init_swv(&mut self) -> Result<()> {
        self.swv = true;
        self.sendcmd("tpiu config disable")?;

        //
        // XXX: This assumes STM32F4's 16Mhz clock
        //
        self.sendcmd("tpiu config internal - uart on 16000000")?;
        self.sendcmd("tcl_trace on")?;

        Ok(())
    }

    //TODO
    fn read_swv(&mut self) -> Result<Vec<u8>> {
        if !self.swv {
            self.init_swv()?
        }

        let mut rbuf = vec![0; 8192];
        let mut swv: Vec<u8> = Vec::with_capacity(8192);

        if let Some(last_swv) = self.last_swv {
            //
            // When we read from SWV from OpenOCD, it will block until data
            // becomes available.  To better approximate the (non-blocking)
            // behavior we see on a directly attached debugger, we return a
            // zero byte read if it has been less than 100 ms since our last
            // read -- relying on OpenOCD to buffer things a bit.
            //
            if last_swv.elapsed().as_secs_f64() < 0.1 {
                return Ok(swv);
            }
        }

        let rval = self.stream.read(&mut rbuf)?;
        self.last_swv = Some(Instant::now());

        if rbuf[rval - 1] != OPENOCD_COMMAND_DELIMITER {
            bail!("missing trace data delimiter: {:?}", rval);
        }

        //
        // OpenOCD can sometimes send multiple command delimters -- or
        // none at all.
        //
        if rval == 1 {
            return Ok(swv);
        }

        let rstr = if rbuf[0] == OPENOCD_COMMAND_DELIMITER {
            str::from_utf8(&rbuf[1..rval - 1])?
        } else {
            str::from_utf8(&rbuf[0..rval - 1])?
        };

        if !rstr.starts_with(OPENOCD_TRACE_DATA_BEGIN) {
            bail!("bogus trace data (bad start): {:?}", rstr);
        }

        if !rstr.ends_with(OPENOCD_TRACE_DATA_END) {
            bail!("bogus trace data (bad end): {:?}", rstr);
        }

        let begin = OPENOCD_TRACE_DATA_BEGIN.len();
        let end = rstr.len() - OPENOCD_TRACE_DATA_END.len();

        for i in (begin..end).step_by(2) {
            if i + 1 >= end {
                bail!("short trace data: {:?}", rval);
            }

            swv.push(u8::from_str_radix(&rstr[i..=i + 1], 16)?);
        }

        Ok(swv)
    }

    fn write_word_32(&mut self, addr: u32, data: u32) -> Result<()> {
        self.op_start()?;
        self.sendcmd(&format!("mww 0x{:x} 0x{:x}", addr, data))?;
        self.op_done()?;
        Ok(())
    }

    fn write_8(&mut self, _addr: u32, _data: &[u8]) -> Result<()> {
        bail!("OpenOCD target does not support modifying state");
    }

    fn halt(&mut self) -> Result<()> {
        log::trace!("halting core");
        //
        // On OpenOCD, we don't halt. If GDB is connected, it gets really,
        // really confused!  This should probably be configurable at
        // some point...
        //
        // Well without unhalted read support cant do anything,
        // so we will pass the onus to the user for now
        self.sendcmd("halt")?;
        self.halted = true;
        Ok(())
    }

    fn run(&mut self) -> Result<()> {
        log::trace!("running core");
        //
        // Well, see above.
        //
        self.sendcmd("resume")?;
        self.halted = false;
        Ok(())
    }

    fn step(&mut self) -> Result<()> {
        todo!();
    }

    fn load(&mut self, path: &Path) -> Result<()> {
        self.sendcmd("reset init")?;
        self.sendcmd(&format!("load_image {} 0x0", path.display()))?;
        self.sendcmd(&format!("verify_image {} 0x0", path.display()))?;
        self.sendcmd("echo \"Doing reset\"")?;
        self.sendcmd("reset run")?;
        Ok(())
    }

    fn reset(&mut self) -> Result<()> {
        self.sendcmd("reset run")?;
        Ok(())
    }

    fn op_start(&mut self) -> Result<()> {
        /*if !self.halted {
            self.halt()?;
            self.halted = true;
        }
        Ok(())*/
        log::trace!("op_start: halting core");
        self.halt()
    }

    fn op_done(&mut self) -> Result<()> {
        log::trace!("was halted: {}", self.was_halted);
        if !self.was_halted {
            log::trace!("op_done: resuming core");
            self.run()?;
        } else {
            log::trace!("op_done: leaving core halted");
        }
        Ok(())
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum GDBServer {
    OpenOCD,
    JLink,
    Qemu,
}

impl fmt::Display for GDBServer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                GDBServer::OpenOCD => "OpenOCD",
                GDBServer::JLink => "JLink",
                GDBServer::Qemu => "QEMU",
            }
        )
    }
}

pub struct GDBCore {
    stream: TcpStream,
    server: GDBServer,
    halted: bool,
    was_halted: bool,
}

const GDB_PACKET_START: char = '$';
const GDB_PACKET_END: char = '#';
const GDB_PACKET_ACK: char = '+';
const GDB_PACKET_HALT: u8 = 3;

#[rustfmt::skip::macros(anyhow, bail)]
impl GDBCore {
    fn prepcmd(&mut self, cmd: &str) -> Vec<u8> {
        let mut payload = vec![GDB_PACKET_START as u8];

        let mut cksum = 0;

        for b in cmd.as_bytes() {
            payload.push(*b);
            cksum += *b as u32;
        }

        //
        // Tack on the goofy checksum beyond the end of the packet.
        //
        let trailer = &format!("{}{:02x}", GDB_PACKET_END, cksum % 256);

        for b in trailer.as_bytes() {
            payload.push(*b);
        }

        log::trace!("sending {}", str::from_utf8(&payload).unwrap());
        payload
    }

    fn firecmd(&mut self, cmd: &str) -> Result<()> {
        log::trace!("sending: {}", cmd);
        let payload = self.prepcmd(cmd);
        self.stream.write_all(&payload)?;
        log::trace!("sent");
        Ok(())
    }

    fn recvack(&mut self) -> Result<()> {
        let mut rbuf = vec![0; 1024];

        let rval = self.stream.read(&mut rbuf)?;
        if rval != 1 {
            /*if &rbuf[..rval] == vec![43, 36, 51, 48, 57, 56, 101, 55, 53, 102, 101, 50, 55, 99, 102, 102, 53, 102, 35, 97, 51] {
                log::trace!("received halt instead of ack, reading again");
                return self.recvack();
            }*/
            //TODO
            log::trace!("expected ack, but got: {:?}", rbuf);
        }
        log::trace!("received ack");
        Ok(())
    }

    fn sendack(&mut self) -> Result<()> {
        self.stream.write_all(&[GDB_PACKET_ACK as u8])?;
        log::trace!("sending ack");
        Ok(())
    }

    fn recvdata(&mut self) -> Result<String> {
        let mut rbuf = vec![0; 1024];
        let mut result = String::new();

        loop {
            log::trace!("reading first chunk");
            let rval = self.stream.read(&mut rbuf)?;
            log::trace!("received {} bytes", rval);
            result.push_str(str::from_utf8(&rbuf[0..rval])?);
            log::trace!("response: {}", result);

            //
            // We are done when we have our closing delimter followed by
            // the two byte checksum.
            //
            let end_yet = result.find(GDB_PACKET_END);
            // let got_ack = result.find(GDB_PACKET_ACK);
            if let None = end_yet {
                log::trace!("reading more data");
                continue;
            }
            if result.find(GDB_PACKET_END) == Some(result.len() - 3) {
                break;
            }
            log::trace!("reading more data");
        }

        //
        // In our result, we should have exactly one opening and exactly
        // one closing delimiter -- and, if expectack is set, at least
        // one ACK as well.
        //
        let start = match result.find(GDB_PACKET_START) {
            Some(ndx) => ndx,
            None => {
                bail!("missing start of packet: \"{}\"", result);
            }
        };

        //
        // By merits of being here, we know we have our end-of-packet...
        //
        let end = result.find(GDB_PACKET_END).unwrap();

        if end < start {
            bail!("start/end inverted: \"{}\"", result);
        }

        Ok(result[start + 1..end].to_string())
    }

    fn sendcmd(&mut self, cmd: &str) -> Result<String> {
        let just_halted = false;
        self.firecmd(cmd)?;
        //self.recvack()?;
        let mut data = self.recvdata()?;
        // if core halted
        if data.contains("T02thread:01;") {
            self.halted = true;
            self.sendack()?;
            log::trace!("halted: trying again");
            self.firecmd(cmd)?;
            data = self.recvdata()?;
        }
        self.sendack()?;
        if just_halted {
            self.firecmd("c")?;
            self.halted = false;
        }
        return Ok(data);
    }

    //TODO Should keep track if the target was initially halted or not, and leave in that state
    fn new(server: GDBServer) -> Result<GDBCore> {
        let port = match server {
            GDBServer::OpenOCD => 3333,
            GDBServer::JLink => 2331,
            GDBServer::Qemu => 3333,
        };

        let host = format!("127.0.0.1:{}", port);
        let addr = host.parse()?;
        let timeout = Duration::from_millis(100);

        let stream =
            TcpStream::connect_timeout(&addr, timeout).map_err(|_| {
                anyhow!(
                "can't connect to {} GDB server on \
                    port {}; is it running?",
                server, port
            )
            })?;

        // set read timout to avoid blocking when waiting for a response that never comes.  This
        // allows an explicit error
        stream.set_read_timeout(Some(Duration::from_millis(1000)))?;
        stream.set_write_timeout(Some(Duration::from_millis(1000)))?;

        //
        // Both the OpenOCD and JLink GDB servers stop the target upon
        // connection.  This is helpful in that we know the state that
        // we're in -- but it's also not the state that we want to be
        // in.  We explicitly run the target before returning.
        //
        let mut core = Self { stream, server, halted: true, was_halted: true };

        // disable ack packets as they are not needed over tcpip
        // core.firecmd("QStartNoAckMode")?;
        let data = core.recvdata();
        match data {
            Err(_err) => {
                log::trace!("connected to halted core");
                core.was_halted = true;
            }
            Ok(data) => {
                if !data.contains("T02thread:01") {
                    bail!("Target did not halt on connect");
                }
                log::trace!("connected to running core");
                core.was_halted = false;
                core.run()?;
            }
        };

        //let supported = core.sendcmd("qSupported")?;
        //log::trace!("{} supported string: {}", server, supported);

        Ok(core)
    }
}

#[rustfmt::skip::macros(anyhow, bail)]
impl Core for GDBCore {
    fn info(&self) -> (String, Option<String>) {
        ("GDB".to_string(), None)
    }

    fn read_word_32(&mut self, addr: u32) -> Result<u32> {
        let mut data = [0; 4];
        self.read_8(addr, &mut data)?;
        Ok(u32::from_le_bytes(data))
    }

    fn read_8(&mut self, addr: u32, data: &mut [u8]) -> Result<()> {
        let cmd = format!("m{:x},{:x}", addr, data.len());

        //self.halt()?;
        let mut rstr = self.sendcmd(&cmd)?;

        if rstr.len() > data.len() * 2 {
            bail!("bad read_8 on cmd {} \
                (expected {}, found {}): {}",
                cmd, data.len() * 2, rstr.len(), rstr);
        }

        for (idx, i) in (0..rstr.len()).step_by(2).enumerate() {
            data[idx] = u8::from_str_radix(&rstr[i..=i + 1], 16)?;
        }

        Ok(())
    }

    fn read_reg(&mut self, reg: Register) -> Result<u32> {
        let reg_id = Register::to_u16(&reg).unwrap();
        use num_traits::ToPrimitive;

        //TODO convert register to name to avoid indexing problems
        let cmd = &format!("p{:02X}", reg_id);

        let rstr = self.sendcmd(cmd)?;

        Ok(u32::from_str_radix(&rstr, 16)?)
    }

    fn write_reg(&mut self, _reg: Register, _value: u32) -> Result<()> {
        Err(anyhow!(
            "{} GDB target does not support modifying state", self.server
        ))
    }

    fn write_word_32(&mut self, _addr: u32, _data: u32) -> Result<()> {
        Err(anyhow!(
            "{} GDB target does not support modifying state", self.server
        ))
    }

    fn write_8(&mut self, _addr: u32, _data: &[u8]) -> Result<()> {
        Err(anyhow!(
            "{} GDB target does not support modifying state", self.server
        ))
    }

    fn halt(&mut self) -> Result<()> {
        /*
        //target is halted whenever a command is sent
        log::trace!("halting");
        self.stream.write_all(&[GDB_PACKET_HALT])?;
        let reply = self.recvdata()?;
        log::trace!("halt reply: {}", reply);
        self.halted = true;
        */
        Ok(())
    }

    fn run(&mut self) -> Result<()> {
        //
        // The OpenOCD target in particular loses its mind if told to
        // continue to when it's already running, insisting on
        // sending a reply with an elaborate message that we don't
        // know to wait on -- so we only continue a target if we know
        // it to be halted.
        //
        if self.halted {
            log::trace!("running core");
            self.firecmd("c")?;
            self.halted = false;
        }

        Ok(())
    }

    fn step(&mut self) -> Result<()> {
        Ok(())
    }

    fn init_swv(&mut self) -> Result<()> {
        Ok(())
    }

    fn read_swv(&mut self) -> Result<Vec<u8>> {
        Err(anyhow!("GDB target does not support SWV"))
    }

    fn load(&mut self, _path: &Path) -> Result<()> {
        bail!("Flash loading is not supported with GDB");
    }

    fn reset(&mut self) -> Result<()> {
        bail!("Reset is not supported with GDB");
    }
}

pub struct DumpCore {
    contents: Vec<u8>,
    regions: BTreeMap<u32, (u32, usize)>,
    registers: HashMap<Register, u32>,
}

impl DumpCore {
    fn new(dump: &str, hubris: &HubrisArchive) -> Result<DumpCore> {
        let mut file = fs::File::open(dump)?;
        let mut regions = BTreeMap::new();

        let mut contents = Vec::new();
        file.read_to_end(&mut contents)?;

        let elf = Elf::parse(&contents).map_err(|e| {
            anyhow!("failed to parse {} as an ELF file: {}", dump, e)
        })?;

        for phdr in elf.program_headers.iter() {
            if phdr.p_type != goblin::elf::program_header::PT_LOAD {
                continue;
            }

            regions.insert(
                phdr.p_vaddr as u32,
                (phdr.p_memsz as u32, phdr.p_offset as usize),
            );
        }

        Ok(Self { contents, regions, registers: hubris.dump_registers() })
    }

    fn check_offset(&self, addr: u32, rsize: usize, offs: usize) -> Result<()> {
        if rsize + offs <= self.contents.len() {
            return Ok(());
        }

        //
        // This really shouldn't happen, as it means that we have a defined
        // region in a program header for memory that wasn't in fact dumped.
        // Still, this might occur if the dump is truncated or otherwise
        // corrupt; offer a message pointing in that direction.
        //
        bail!(
            "0x{:x} is valid, but offset in dump \
            (0x{:x}) + size (0x{:x}) exceeds max (0x{:x}); \
            is the dump truncated or otherwise corrupt?",
            addr,
            offs,
            rsize,
            self.contents.len()
        );
    }
}

#[rustfmt::skip::macros(bail)]
impl Core for DumpCore {
    fn info(&self) -> (String, Option<String>) {
        ("core dump".to_string(), None)
    }

    fn read_word_32(&mut self, addr: u32) -> Result<u32> {
        let rsize: usize = 4;

        if let Some((&base, &(size, offset))) =
            self.regions.range(..=addr).rev().next()
        {
            if base > addr {
                // fall out to the bail below.
            } else if (addr - base) + rsize as u32 > size {
                bail!(
                    "0x{:x} is valid, but relative to base (0x{:x}), \
                    offset (0x{:x}) exceeds max (0x{:x})",
                    addr, base, (addr - base) + rsize as u32, size
                );
            } else {
                let offs = offset + (addr - base) as usize;

                self.check_offset(addr, rsize, offs)?;

                return Ok(u32::from_le_bytes(
                    self.contents[offs..offs + rsize].try_into().unwrap(),
                ));
            }
        }
        bail!("read from invalid address: 0x{:x}", addr);
    }

    fn read_8(&mut self, addr: u32, data: &mut [u8]) -> Result<()> {
        let rsize = data.len();

        if let Some((&base, &(size, offset))) =
            self.regions.range(..=addr).rev().next()
        {
            if base > addr {
                // fall out to the bail below.
            } else if (addr - base) + rsize as u32 > size {
                bail!(
                    "0x{:x} is valid, but relative to base (0x{:x}), \
                    offset (0x{:x}) exceeds max (0x{:x})",
                    addr, base, (addr - base) + rsize as u32, size
                );
            } else {
                let offs = offset + (addr - base) as usize;
                self.check_offset(addr, rsize, offs)?;

                data[..rsize]
                    .copy_from_slice(&self.contents[offs..rsize + offs]);
                return Ok(());
            }
        }

        bail!("read of {} bytes from invalid address: 0x{:x}", rsize, addr);
    }

    fn read_reg(&mut self, reg: Register) -> Result<u32> {
        if let Some(val) = self.registers.get(&reg) {
            Ok(*val)
        } else {
            bail!("register {} not found in dump", reg);
        }
    }

    fn write_reg(&mut self, _reg: Register, _value: u32) -> Result<()> {
        bail!("cannot write register on a dump");
    }

    fn write_word_32(&mut self, _addr: u32, _data: u32) -> Result<()> {
        bail!("cannot write a word on a dump");
    }

    fn write_8(&mut self, _addr: u32, _data: &[u8]) -> Result<()> {
        bail!("cannot write a byte on a dump");
    }

    fn halt(&mut self) -> Result<()> {
        Ok(())
    }

    fn run(&mut self) -> Result<()> {
        Ok(())
    }

    fn step(&mut self) -> Result<()> {
        bail!("can't step a dump");
    }

    fn init_swv(&mut self) -> Result<()> {
        bail!("cannot enable SWV on a dump");
    }

    fn read_swv(&mut self) -> Result<Vec<u8>> {
        bail!("cannot read SWV on a dump");
    }

    fn is_dump(&self) -> bool {
        true
    }

    fn load(&mut self, _path: &Path) -> Result<()> {
        bail!("Flash loading is not supported on a dump");
    }

    fn reset(&mut self) -> Result<()> {
        bail!("Reset is not supported on a dump");
    }
}

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

fn get_usb_probe(index: Option<usize>) -> Result<probe_rs::DebugProbeInfo> {
    let probes = Probe::list_all();

    if probes.is_empty() {
        bail!("no debug probe found; is it plugged in?");
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

#[rustfmt::skip::macros(anyhow, bail)]
pub fn attach_to_probe(probe: &str) -> Result<Box<dyn Core>> {
    let (probe, index) = parse_probe(probe);

    match probe {
        "usb" => {
            let probe_info = get_usb_probe(index)?;

            let res = probe_info.open();

            if let Err(probe_rs::DebugProbeError::Usb(Some(ref err))) = res {
                if let Some(rcode) = err.downcast_ref::<rusb::Error>() {
                    if *rcode == rusb::Error::Busy {
                        bail!(
                            "USB link in use; is OpenOCD or \
                            another debugger running?"
                        );
                    }
                }
            }

            let probe = res?;

            crate::msg!("Opened probe {}", probe_info.identifier);
            Ok(Box::new(UnattachedCore::new(
                probe,
                probe_info.identifier.clone(),
                probe_info.vendor_id,
                probe_info.product_id,
                probe_info.serial_number,
            )))
        }
        "ocd" | "ocdgdb" | "jlink" => {
            bail!("Probe only attachment with {} is not supported", probe)
        }
        "auto" => attach_to_probe("usb"),
        _ => match TryInto::<probe_rs::DebugProbeSelector>::try_into(probe) {
            Ok(selector) => {
                let vidpid = probe;
                let vid = selector.vendor_id;
                let pid = selector.product_id;
                let serial = selector.serial_number.clone();
                let probe = probe_rs::Probe::open(selector)?;
                let name = probe.get_name();

                crate::msg!("Opened {} via {}", vidpid, name);
                Ok(Box::new(UnattachedCore::new(probe, name, vid, pid, serial)))
            }
            Err(_) => Err(anyhow!("unrecognized probe: {}", probe)),
        },
    }
}

#[rustfmt::skip::macros(anyhow, bail)]
pub fn attach_to_chip(
    probe: &str,
    hubris: &HubrisArchive,
    chip: Option<&str>,
) -> Result<Box<dyn Core>> {
    let (probe, index) = parse_probe(probe);

    match probe {
        "usb" => {
            let probe_info = get_usb_probe(index)?;

            let res = probe_info.open();

            if let Err(probe_rs::DebugProbeError::Usb(Some(ref err))) = res {
                if let Some(rcode) = err.downcast_ref::<rusb::Error>() {
                    if *rcode == rusb::Error::Busy {
                        bail!(
                            "USB link in use; is OpenOCD or \
                            another debugger running?"
                        );
                    }
                }
            }

            let probe = res?;

            let name = probe.get_name();

            //
            // probe-rs needs us to specify a chip that it knows about -- but
            // it only really uses this information for flashing the part.  If
            // we are attaching to the part for not pusposes of flashing, we
            // specify a generic ARMv7-M (but then we also indicate that can't
            // flash to assure that we can fail explicitly should flashing be
            // attempted).
            //
            let (session, can_flash) = match chip {
                Some(chip) => (probe.attach(chip)?, true),
                None => match hubris.arch {
                    Some(goblin::elf::header::EM_ARM) => {
                        (probe.attach("armv7m")?, false)
                    }
                    Some(goblin::elf::header::EM_RISCV) => {
                        (probe.attach("riscv")?, false)
                    }
                    None => {
                        bail!("Hubris archive was corrupt, no header found")
                    }
                    _ => bail!("Hubris archive for unsupported architecture"),
                },
            };

            crate::msg!("attached via {}", name);

            Ok(Box::new(ProbeCore::new(
                session,
                probe_info.identifier.clone(),
                probe_info.vendor_id,
                probe_info.product_id,
                probe_info.serial_number,
                hubris.unhalted_reads(),
                can_flash,
            )))
        }

        "ocd" => {
            let mut core = OpenOCDCore::new()?;
            let version = core.sendcmd("version")?;

            if !version.contains("Open On-Chip Debugger") {
                bail!("version string unrecognized: \"{}\"", version);
            }

            crate::msg!("attached via OpenOCD");

            Ok(Box::new(core))
        }

        "auto" => {
            if let Ok(probe) = attach_to_chip("ocd", hubris, chip) {
                return Ok(probe);
            }

            if let Ok(probe) = attach_to_chip("jlink", hubris, chip) {
                return Ok(probe);
            }
            if let Ok(probe) = attach_to_chip("qemu", hubris, chip) {
                return Ok(probe);
            }

            attach_to_chip("usb", hubris, chip)
        }

        "ocdgdb" => {
            let core = GDBCore::new(GDBServer::OpenOCD)?;
            crate::msg!("attached via OpenOCD's GDB server");

            Ok(Box::new(core))
        }

        "jlink" => {
            let core = GDBCore::new(GDBServer::JLink)?;
            crate::msg!("attached via JLink");

            Ok(Box::new(core))
        }

        "qemu" => {
            let core = GDBCore::new(GDBServer::Qemu)?;
            crate::msg!("attached via QEMU GDB server");

            Ok(Box::new(core))
        }

        _ => match TryInto::<probe_rs::DebugProbeSelector>::try_into(probe) {
            Ok(selector) => {
                let vidpid = probe;

                let vid = selector.vendor_id;
                let pid = selector.product_id;
                let serial = selector.serial_number.clone();

                let probe = probe_rs::Probe::open(selector)?;
                let name = probe.get_name();

                //
                // See the block comment in the generic "usb" attach for
                // why we use armv7m here.
                //
                let (session, can_flash) = match chip {
                    Some(chip) => (probe.attach(chip)?, true),
                    None => (probe.attach("armv7m")?, false),
                };

                crate::msg!("attached to {} via {}", vidpid, name);

                Ok(Box::new(ProbeCore::new(
                    session,
                    name,
                    vid,
                    pid,
                    serial,
                    hubris.unhalted_reads(),
                    can_flash,
                )))
            }
            Err(_) => Err(anyhow!("unrecognized probe: {}", probe)),
        },
    }
}

pub fn attach_for_flashing(
    probe: &str,
    hubris: &HubrisArchive,
    chip: &str,
) -> Result<Box<dyn Core>> {
    attach_to_chip(probe, hubris, Some(chip))
}

pub fn attach(probe: &str, hubris: &HubrisArchive) -> Result<Box<dyn Core>> {
    attach_to_chip(probe, hubris, None)
}

pub fn attach_dump(
    dump: &str,
    hubris: &HubrisArchive,
) -> Result<Box<dyn Core>> {
    let core = DumpCore::new(dump, hubris)?;
    crate::msg!("attached to dump");
    Ok(Box::new(core))
}
