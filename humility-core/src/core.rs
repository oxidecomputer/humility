// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use probe_rs::MemoryInterface;
use probe_rs::Probe;

use anyhow::{anyhow, bail, ensure, Result};

use crate::arch::ARMRegister;
use crate::hubris::*;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::convert::TryInto;
use std::fmt;
use std::fs;
use std::io::Read;
use std::io::Write;
use std::net::TcpStream;
use std::str;
use std::time::Duration;
use std::time::Instant;

use goblin::elf::Elf;

pub trait Core {
    fn info(&self) -> (String, Option<String>);
    fn read_word_32(&mut self, addr: u32) -> Result<u32>;
    fn read_8(&mut self, addr: u32, data: &mut [u8]) -> Result<()>;
    fn read_reg(&mut self, reg: ARMRegister) -> Result<u32>;
    fn write_reg(&mut self, reg: ARMRegister, value: u32) -> Result<()>;
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
}

pub struct ProbeCore {
    pub session: probe_rs::Session,
    pub identifier: String,
    pub vendor_id: u16,
    pub product_id: u16,
    pub serial_number: Option<String>,
    halted: bool,
}

impl ProbeCore {
    fn halt_and_exec(
        &mut self,
        mut func: impl FnMut(&mut probe_rs::Core) -> Result<()>,
    ) -> Result<()> {
        let mut core = self.session.core(0)?;

        let halted = if !self.halted && core.core_halted()? {
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
        trace!("reading word at {:x}", addr);
        let mut rval = 0;

        self.halt_and_exec(|core| {
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

        self.halt_and_exec(|core| Ok(core.read_8(addr, data)?))
    }

    fn read_reg(&mut self, reg: ARMRegister) -> Result<u32> {
        let mut core = self.session.core(0)?;
        use num_traits::ToPrimitive;

        Ok(core.read_core_reg(Into::<probe_rs::CoreRegisterAddress>::into(
            ARMRegister::to_u16(&reg).unwrap(),
        ))?)
    }

    fn write_reg(&mut self, reg: ARMRegister, value: u32) -> Result<()> {
        let mut core = self.session.core(0)?;
        use num_traits::ToPrimitive;

        core.write_core_reg(
            Into::<probe_rs::CoreRegisterAddress>::into(
                ARMRegister::to_u16(&reg).unwrap(),
            ),
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
        let mut core = self.session.core(0)?;
        core.halt(std::time::Duration::from_millis(1000))?;
        self.halted = true;
        Ok(())
    }

    fn run(&mut self) -> Result<()> {
        let mut core = self.session.core(0)?;
        core.run()?;
        self.halted = false;
        Ok(())
    }

    fn step(&mut self) -> Result<()> {
        let mut core = self.session.core(0)?;
        core.step()?;
        Ok(())
    }

    fn init_swv(&mut self) -> Result<()> {
        use probe_rs::architecture::arm::swo::SwoConfig;

        let config = SwoConfig::new(0).set_baud(2_000_000);
        self.session.setup_swv(&config)?;

        /*
         * Because the probe can have sticky errors, we perform one read
         * (and discard the results) to assure that any further errors
         * are legit.
         */
        let _discard = self.session.read_swo();
        Ok(())
    }

    fn read_swv(&mut self) -> Result<Vec<u8>> {
        Ok(self.session.read_swo()?)
    }
}

const OPENOCD_COMMAND_DELIMITER: u8 = 0x1a;
const OPENOCD_TRACE_DATA_BEGIN: &str = "type target_trace data ";
const OPENOCD_TRACE_DATA_END: &str = "\r\n";

pub struct OpenOCDCore {
    stream: TcpStream,
    swv: bool,
    last_swv: Option<Instant>,
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

        /*
         * Surely not surprisingly, OpenOCD doesn't have a coherent way of
         * indicating that a command has failed.  We fall back to assuming
         * that any return value that contains "Error: " or "invalid command
         * name" is in fact an error.
         */
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

        Ok(Self { stream, swv: false, last_swv: None })
    }
}

#[rustfmt::skip::macros(anyhow, bail)]
impl Core for OpenOCDCore {
    fn info(&self) -> (String, Option<String>) {
        ("OpenOCD".to_string(), None)
    }

    fn read_word_32(&mut self, addr: u32) -> Result<u32> {
        let result = self.sendcmd(&format!("mrw 0x{:x}", addr))?;
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

        /*
         * To read an array, we put it in a TCL variable called "output"
         * and then dump the variable.
         */
        let cmd = format!("mem2array output 8 0x{:x} {}", addr, data.len());

        self.sendcmd("array unset output")?;
        self.sendcmd(&cmd)?;

        let mut index = None;
        let mut seen = vec![false; data.len()];

        let result = self.sendcmd("return $output")?;

        /*
         * Entirely on-brand, if the mem2array command has failed wildly,
         * OpenOCD won't actually return an error to us -- it will merely
         * fail to set the variable (and we will therefore fail when
         * we attempt to retrieve the variable).  If we fail to
         * retrieve the variable, we infer it to be a failure to
         * perform the read and bail explicitly.
         */
        if result.contains("no such variable") {
            bail!("read at 0x{:x} for {} bytes failed", addr, data.len());
        }

        /*
         * The output here is bonkers: instead of being (merely) the array,
         * it's an (undelimited) set of 2-tuples of (index, value) -- sorted
         * in strict alphabetical order by index (!!).  (That is, index 100
         * comes before, say, index 11.)
         */
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

        Ok(())
    }

    fn write_reg(&mut self, _reg: ARMRegister, _val: u32) -> Result<()> {
        // This does not work right now, TODO?
        //
        Err(anyhow!(
            "Writing registers is not currently supported with OpenOCD"
        ))
    }

    fn read_reg(&mut self, reg: ARMRegister) -> Result<u32> {
        use num_traits::ToPrimitive;

        let cmd = format!("reg {}", ARMRegister::to_u16(&reg).unwrap());
        let rval = self.sendcmd(&cmd)?;

        if let Some(line) = rval.lines().next() {
            if let Some(val) = line.split_whitespace().last() {
                if let Ok(rval) = parse_int::parse::<u32>(val) {
                    return Ok(rval);
                }
            }
        }

        Err(anyhow!("\"{}\": malformed return value: {:?}", cmd, rval))
    }

    fn init_swv(&mut self) -> Result<()> {
        self.swv = true;
        self.sendcmd("tpiu config disable")?;

        /*
         * XXX: This assumes STM32F4's 16Mhz clock
         */
        self.sendcmd("tpiu config internal - uart on 16000000")?;
        self.sendcmd("tcl_trace on")?;

        Ok(())
    }

    fn read_swv(&mut self) -> Result<Vec<u8>> {
        if !self.swv {
            self.init_swv()?
        }

        let mut rbuf = vec![0; 8192];
        let mut swv: Vec<u8> = Vec::with_capacity(8192);

        if let Some(last_swv) = self.last_swv {
            /*
             * When we read from SWV from OpenOCD, it will block until data
             * becomes available.  To better approximate the (non-blocking)
             * behavior we see on a directly attached debugger, we return a
             * zero byte read if it has been less than 100 ms since our last
             * read -- relying on OpenOCD to buffer things a bit.
             */
            if last_swv.elapsed().as_secs_f64() < 0.1 {
                return Ok(swv);
            }
        }

        let rval = self.stream.read(&mut rbuf)?;
        self.last_swv = Some(Instant::now());

        if rbuf[rval - 1] != OPENOCD_COMMAND_DELIMITER {
            bail!("missing trace data delimiter: {:?}", rval);
        }

        /*
         * OpenOCD can sometimes send multiple command delimters -- or
         * none at all.
         */
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
        self.sendcmd(&format!("mww 0x{:x} 0x{:x}", addr, data))?;
        Ok(())
    }

    fn write_8(&mut self, _addr: u32, _data: &[u8]) -> Result<()> {
        bail!("OpenOCD target does not support modifying state");
    }

    fn halt(&mut self) -> Result<()> {
        /*
         * On OpenOCD, we don't halt. If GDB is connected, it gets really,
         * really confused!  This should probably be configurable at
         * some point...
         */
        Ok(())
    }

    fn run(&mut self) -> Result<()> {
        /*
         * Well, see above.
         */
        Ok(())
    }

    fn step(&mut self) -> Result<()> {
        todo!();
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum GDBServer {
    OpenOCD,
    JLink,
}

impl fmt::Display for GDBServer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                GDBServer::OpenOCD => "OpenOCD",
                GDBServer::JLink => "JLink",
            }
        )
    }
}

pub struct GDBCore {
    stream: TcpStream,
    server: GDBServer,
    halted: bool,
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

        /*
         * Tack on the goofy checksum beyond the end of the packet.
         */
        let trailer = &format!("{}{:02x}", GDB_PACKET_END, cksum % 256);

        for b in trailer.as_bytes() {
            payload.push(*b);
        }

        trace!("sending {}", str::from_utf8(&payload).unwrap());
        payload
    }

    fn firecmd(&mut self, cmd: &str) -> Result<()> {
        let mut rbuf = vec![0; 1024];
        let payload = self.prepcmd(cmd);

        self.stream.write_all(&payload)?;

        /*
         * We are expecting no result -- just an ack.
         */
        let rval = self.stream.read(&mut rbuf)?;

        if rval != 1 {
            bail!("cmd {} returned {} bytes: {:?}", cmd, rval,
                str::from_utf8(&rbuf));
        }

        if rbuf[0] != GDB_PACKET_ACK as u8 {
            bail!("cmd {} incorrectly ack'd: {:?}", cmd, rbuf);
        }

        Ok(())
    }

    fn sendack(&mut self) -> Result<()> {
        self.stream.write_all(&[GDB_PACKET_ACK as u8])?;
        Ok(())
    }

    fn recv(&mut self, expectack: bool) -> Result<String> {
        let mut rbuf = vec![0; 1024];
        let mut result = String::new();

        loop {
            let rval = self.stream.read(&mut rbuf)?;

            result.push_str(str::from_utf8(&rbuf[0..rval])?);
            trace!("response: {}", result);

            /*
             * We are done when we have our closing delimter followed by
             * the two byte checksum.
             */
            if result.find(GDB_PACKET_END) == Some(result.len() - 3) {
                break;
            }
        }

        /*
         * We have our response, so ack it back
         */
        self.sendack()?;

        /*
         * In our result, we should have exactly one opening and exactly
         * one closing delimiter -- and, if expectack is set, at least
         * one ACK as well.
         */
        let start = match result.find(GDB_PACKET_START) {
            Some(ndx) => ndx,
            None => {
                bail!("missing start of packet: \"{}\"", result);
            }
        };

        /*
         * By merits of being here, we know we have our end-of-packet...
         */
        let end = result.find(GDB_PACKET_END).unwrap();

        if end < start {
            bail!("start/end inverted: \"{}\"", result);
        }

        match result.find(GDB_PACKET_ACK) {
            Some(ack) => {
                if expectack && ack > start {
                    bail!("found response but no ack: \"{}\"", result);
                }

                if !expectack && ack < start {
                    bail!("found spurious ack: \"{}\"", result);
                }
            }

            None => {
                if expectack {
                    bail!("did not find expected ack: \"{}\"", result);
                }
            }
        }

        Ok(result[start + 1..end].to_string())
    }

    fn sendcmd(&mut self, cmd: &str) -> Result<String> {
        let payload = self.prepcmd(cmd);
        self.stream.write_all(&payload)?;
        self.recv(true)
    }

    fn send_32(&mut self, cmd: &str) -> Result<u32> {
        let rstr = self.sendcmd(cmd)?;
        let mut buf: Vec<u8> = vec![];

        for i in (0..rstr.len()).step_by(2) {
            buf.push(u8::from_str_radix(&rstr[i..=i + 1], 16)?);
        }

        trace!("command {} returned {}", cmd, rstr);

        match rstr.len() {
            2 => Ok(u8::from_le_bytes(buf[..].try_into().unwrap()) as u32),
            4 => Ok(u16::from_le_bytes(buf[..].try_into().unwrap()) as u32),
            8 => Ok(u32::from_le_bytes(buf[..].try_into().unwrap()) as u32),
            16 => {
                /*
                 * Amazingly, for some 32-bit register values under certain
                 * circumstances the JLink seems to return a 64-bit value
                 * (!). We confirm that this value is
                 * representable and return it.
                 */
                let val = u64::from_le_bytes(buf[..].try_into().unwrap());

                if val > std::u32::MAX.into() {
                    Err(anyhow!("bad 64-bit return on cmd {}: {}", cmd, rstr))
                } else {
                    Ok(val as u32)
                }
            }
            _ => Err(anyhow!("bad return on cmd {}: {}", cmd, rstr)),
        }
    }

    fn new(server: GDBServer) -> Result<GDBCore> {
        let port = match server {
            GDBServer::OpenOCD => 3333,
            GDBServer::JLink => 2331,
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

        /*
         * Both the OpenOCD and JLink GDB servers stop the target upon
         * connection.  This is helpful in that we know the state that
         * we're in -- but it's also not the state that we want to be
         * in.  We explicitly run the target before returning.
         */
        let mut core = Self { stream, server, halted: true };

        let supported = core.sendcmd("qSupported")?;
        trace!("{} supported string: {}", server, supported);

        core.run()?;

        Ok(core)
    }
}

#[rustfmt::skip::macros(anyhow, bail)]
impl Core for GDBCore {
    fn info(&self) -> (String, Option<String>) {
        ("GDB".to_string(), None)
    }

    fn read_word_32(&mut self, addr: u32) -> Result<u32> {
        self.send_32(&format!("m{:x},4", addr))
    }

    fn read_8(&mut self, addr: u32, data: &mut [u8]) -> Result<()> {
        let cmd = format!("m{:x},{:x}", addr, data.len());

        let rstr = self.sendcmd(&cmd)?;

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

    fn read_reg(&mut self, reg: ARMRegister) -> Result<u32> {
        use num_traits::ToPrimitive;
        let cmd = &format!("p{:02X}", ARMRegister::to_u16(&reg).unwrap());

        let rval = self.send_32(cmd);

        if self.server == GDBServer::JLink {
            /*
             * Maddeningly, the JLink stops the target whenever a register
             * is read.
             */
            self.firecmd("c")?;
        }

        rval
    }

    fn write_reg(&mut self, _reg: ARMRegister, _value: u32) -> Result<()> {
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
        self.stream.write_all(&[GDB_PACKET_HALT])?;

        let reply = self.recv(false)?;
        trace!("halt reply: {}", reply);
        self.halted = true;

        Ok(())
    }

    fn run(&mut self) -> Result<()> {
        /*
         * The OpenOCD target in particular loses its mind if told to
         * continue to when it's already running, insisting on
         * sending a reply with an elaborate message that we don't
         * know to wait on -- so we only continue a target if we know
         * it to be halted.
         */
        if self.halted {
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
}

pub struct DumpCore {
    contents: Vec<u8>,
    regions: BTreeMap<u32, (u32, usize)>,
    registers: HashMap<ARMRegister, u32>,
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
                    "0x{:x} is valid, but size ({}) exceeds max ({})",
                    addr, rsize, size - (addr - base)
                );
            } else {
                let offs = offset + (addr - base) as usize;

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
                    "0x{:x} is valid, but size ({}) exceeds max ({})",
                    addr, rsize, size - (addr - base)
                );
            } else {
                let offs = offset + (addr - base) as usize;

                data[..rsize]
                    .copy_from_slice(&self.contents[offs..rsize + offs]);

                return Ok(());
            }
        }

        bail!("read of {} bytes from invalid address: 0x{:x}", rsize, addr);
    }

    fn read_reg(&mut self, reg: ARMRegister) -> Result<u32> {
        if let Some(val) = self.registers.get(&reg) {
            Ok(*val)
        } else {
            bail!("register {} not found in dump", reg);
        }
    }

    fn write_reg(&mut self, _reg: ARMRegister, _value: u32) -> Result<()> {
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
}

#[rustfmt::skip::macros(anyhow, bail)]
pub fn attach(mut probe: &str, chip: &str) -> Result<Box<dyn Core>> {
    let mut index: Option<usize> = None;

    if probe.contains('-') {
        let str = probe.to_owned();
        let pieces: Vec<&str> = str.split('-').collect();

        if pieces[0] == "usb" && pieces.len() == 2 {
            if let Ok(val) = pieces[1].parse::<usize>() {
                index = Some(val);
                probe = "usb";
            }
        }
    }

    match probe {
        "usb" => {
            let probes = Probe::list_all();

            if probes.is_empty() {
                bail!("no debug probe found; is it plugged in?");
            }

            let (selected, res) = if let Some(index) = index {
                if index < probes.len() {
                    (index, probes[index].open())
                } else {
                    bail!(
                        "index ({}) exceeds max probe index ({})",
                        index, probes.len() - 1
                    );
                }
            } else if probes.len() == 1 {
                (0, probes[0].open())
            } else {
                bail!("multiple USB probes detected; must \
                       explicitly append index (e.g., \"-p usb-0\")");
            };

            /*
             * By far the most common error is to not be able to attach to a
             * debug probe because something else has already attached to it;
             * we pull this error out to yield a more actionable suggestion!
             */
            if let Err(probe_rs::DebugProbeError::USB(Some(ref err))) = res {
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
            let session = probe.attach(chip)?;

            info!("attached via {}", name);

            Ok(Box::new(ProbeCore {
                session,
                identifier: probes[selected].identifier.clone(),
                vendor_id: probes[selected].vendor_id,
                product_id: probes[selected].product_id,
                serial_number: probes[selected].serial_number.clone(),
                halted: false,
            }))
        }

        "ocd" => {
            let mut core = OpenOCDCore::new()?;
            let version = core.sendcmd("version")?;

            if !version.contains("Open On-Chip Debugger") {
                bail!("version string unrecognized: \"{}\"", version);
            }

            info!("attached via OpenOCD");

            Ok(Box::new(core))
        }

        "auto" => {
            if let Ok(probe) = attach("ocd", chip) {
                return Ok(probe);
            }

            if let Ok(probe) = attach("jlink", chip) {
                return Ok(probe);
            }

            attach("usb", chip)
        }

        "ocdgdb" => {
            let core = GDBCore::new(GDBServer::OpenOCD)?;
            info!("attached via OpenOCD's GDB server");

            Ok(Box::new(core))
        }

        "jlink" => {
            let core = GDBCore::new(GDBServer::JLink)?;
            info!("attached via JLink");

            Ok(Box::new(core))
        }

        _ => match TryInto::<probe_rs::DebugProbeSelector>::try_into(probe) {
            Ok(selector) => {
                let vidpid = probe;

                let vendor_id = selector.vendor_id;
                let product_id = selector.product_id;
                let serial_number = selector.serial_number.clone();

                let probe = probe_rs::Probe::open(selector)?;
                let name = probe.get_name();
                let session = probe.attach(chip)?;

                info!("attached to {} via {}", vidpid, name);

                Ok(Box::new(ProbeCore {
                    session,
                    identifier: name,
                    vendor_id,
                    product_id,
                    serial_number,
                    halted: false,
                }))
            }
            Err(_) => Err(anyhow!("unrecognized probe: {}", probe)),
        },
    }
}

pub fn attach_dump(
    dump: &str,
    hubris: &HubrisArchive,
) -> Result<Box<dyn Core>> {
    let core = DumpCore::new(dump, hubris)?;
    info!("attached to dump");
    Ok(Box::new(core))
}
