/*
 * Copyright 2020 Oxide Computer Company
 */

use probe_rs::Probe;

use anyhow::{anyhow, bail, ensure, Result};

use crate::debug::*;
use std::convert::TryInto;
use std::fmt;
use std::io::Read;
use std::io::Write;
use std::net::TcpStream;
use std::str;

pub trait Core {
    fn read_word_32(&mut self, addr: u32) -> Result<u32>;
    fn read_8(&mut self, addr: u32, data: &mut [u8]) -> Result<()>;
    fn read_reg(&mut self, reg: ARMRegister) -> Result<u32>;
    fn init_swv(&mut self) -> Result<()>;
    fn read_swv(&mut self) -> Result<Vec<u8>>;
    fn write_word_32(&mut self, addr: u32, data: u32) -> Result<()>;
    fn halt(&mut self) -> Result<()>;
    fn run(&mut self) -> Result<()>;
    fn step(&mut self) -> Result<()>;
}

pub struct ProbeCore {
    pub core: probe_rs::Core,
    pub session: probe_rs::Session,
}

const CORE_MAX_READSIZE: usize = 65536; // 64K ought to be enough for anyone

#[rustfmt::skip::macros(anyhow, bail)]
impl Core for ProbeCore {
    fn read_word_32(&mut self, addr: u32) -> Result<u32> {
        Ok(self.core.read_word_32(addr)?)
    }

    fn read_8(&mut self, addr: u32, data: &mut [u8]) -> Result<()> {
        if data.len() > CORE_MAX_READSIZE {
            bail!("read of {} bytes at 0x{:x} exceeds max of {}",
                data.len(), addr, CORE_MAX_READSIZE);
        }

        Ok(self.core.read_8(addr, data)?)
    }

    fn read_reg(&mut self, reg: ARMRegister) -> Result<u32> {
        use num_traits::ToPrimitive;

        Ok(self.core.read_core_reg(
            Into::<probe_rs::CoreRegisterAddress>::into(
                ARMRegister::to_u16(&reg).unwrap(),
            ),
        )?)
    }

    fn write_word_32(&mut self, addr: u32, data: u32) -> Result<()> {
        Ok(self.core.write_word_32(addr, data)?)
    }

    fn halt(&mut self) -> Result<()> {
        self.core.halt()?;
        Ok(())
    }

    fn run(&mut self) -> Result<()> {
        Ok(self.core.run()?)
    }

    fn step(&mut self) -> Result<()> {
        self.core.step()?;
        Ok(())
    }

    fn init_swv(&mut self) -> Result<()> {
        Ok(())
    }

    fn read_swv(&mut self) -> Result<Vec<u8>> {
        Ok(self.session.read_swv()?)
    }
}

const OPENOCD_COMMAND_DELIMITER: u8 = 0x1a;
const OPENOCD_TRACE_DATA_BEGIN: &str = "type target_trace data ";
const OPENOCD_TRACE_DATA_END: &str = "\r\n";

pub struct OpenOCDCore {
    stream: TcpStream,
    swv: bool,
}

#[rustfmt::skip::macros(anyhow, bail)]
impl OpenOCDCore {
    fn sendcmd(&mut self, cmd: &str) -> Result<String> {
        let mut rbuf = vec![0; 1024];
        let mut result = String::with_capacity(16);

        let mut str = String::from(cmd);
        str.push(OPENOCD_COMMAND_DELIMITER as char);

        self.stream.write(str.as_bytes())?;

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
        let stream = TcpStream::connect("localhost:6666").map_err(|_| {
            anyhow!("can't connect to OpenOCD on port 6666; is it running?")
        })?;

        Ok(Self { stream: stream, swv: false })
    }
}

#[rustfmt::skip::macros(anyhow, bail)]
impl Core for OpenOCDCore {
    fn read_word_32(&mut self, addr: u32) -> Result<u32> {
        let result = self.sendcmd(&format!("mrw 0x{:x}", addr))?;
        Ok(result.parse::<u32>()?)
    }

    fn read_8(&mut self, addr: u32, data: &mut [u8]) -> Result<()> {
        ensure!(
            data.len() > CORE_MAX_READSIZE,
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

        /*
         * The output here is bonkers: instead of being (merely) the array,
         * it's an (undelimited) set of 2-tuples of (index, value) -- sorted
         * in strict alphabetical order by index (!!).  (That is, index 100
         * comes before, say, index 11.)
         */
        for val in self.sendcmd("return $output")?.split(" ") {
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
            ensure!(!v.1, "\"{}\": missing index {}", cmd, v.0);
        }

        Ok(())
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

        let rval = self.stream.read(&mut rbuf)?;

        if rbuf[rval - 1] != OPENOCD_COMMAND_DELIMITER {
            bail!("missing trace data delimiter: {:?}", rval);
        }

        let rstr = str::from_utf8(&rbuf[0..rval - 1])?;

        if !rstr.starts_with(OPENOCD_TRACE_DATA_BEGIN) {
            bail!("bogus trace data (bad start): {:?}", rval);
        }

        if !rstr.ends_with(OPENOCD_TRACE_DATA_END) {
            bail!("bogus trace data (bad end): {:?}", rval);
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

    fn halt(&mut self) -> Result<()> {
        self.sendcmd("halt")?;
        Ok(())
    }

    fn run(&mut self) -> Result<()> {
        self.sendcmd("resume")?;
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
        let mut payload = vec![];

        payload.push(GDB_PACKET_START as u8);

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

        self.stream.write(&payload)?;

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
        self.stream.write(&[GDB_PACKET_ACK as u8])?;
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
        self.stream.write(&payload)?;
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
                 * circumstances the JLink seems to return a 64-bit value (!).
                 * We confirm that this value is representable and return it.
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

        let host = format!("localhost:{}", port);

        let stream = TcpStream::connect(host).map_err(|_| {
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
        let mut core = Self { stream: stream, server: server, halted: true };

        let supported = core.sendcmd("qSupported")?;
        trace!("{} supported string: {}", server, supported);

        core.run()?;

        Ok(core)
    }
}

#[rustfmt::skip::macros(anyhow, bail)]
impl Core for GDBCore {
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

        let mut idx = 0;

        for i in (0..rstr.len()).step_by(2) {
            data[idx] = u8::from_str_radix(&rstr[i..=i + 1], 16)?;
            idx += 1;
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

    fn write_word_32(&mut self, _addr: u32, _data: u32) -> Result<()> {
        Err(anyhow!("{} GDB target does not support modifying state", self.server))
    }

    fn halt(&mut self) -> Result<()> {
        self.stream.write(&[GDB_PACKET_HALT])?;

        let reply = self.recv(false)?;
        trace!("halt reply: {}", reply);
        self.halted = true;

        Ok(())
    }

    fn run(&mut self) -> Result<()> {
        /*
         * The OpenOCD target in particular loses its mind if told to continue
         * to when it's already running, insisting on sending a reply with an
         * elaborate message that we don't know to wait on -- so we only
         * continue a target if we know it to be halted.
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

#[rustfmt::skip::macros(anyhow, bail)]
pub fn attach(debugger: &str, chip: &str) -> Result<Box<dyn Core>> {
    match debugger {
        "probe" => {
            let probes = Probe::list_all();

            if probes.len() == 0 {
                bail!("no debug probe found; is it plugged in?");
            }

            let res = probes[0].open();

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
            let core = session.attach_to_core(0)?;

            info!("attached via {}", name);

            Ok(Box::new(ProbeCore { session: session, core: core }))
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

            attach("probe", chip)
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

        _ => Err(anyhow!("unrecognized debugger: {}", debugger)),
    }
}
