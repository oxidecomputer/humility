/*
 * Copyright 2020 Oxide Computer Company
 */

use probe_rs::Probe;

use std::error::Error;
use std::str;
use std::io::Read;
use std::io::Write;
use std::net::TcpStream;
use crate::err;

pub trait Core {
    fn read_word_32(&mut self, addr: u32) -> Result<u32, Box<dyn Error>>;
    fn read_8(&mut self, addr: u32, data: &mut [u8]) ->
        Result<(), Box<dyn Error>>;
    fn init_swv(&mut self) -> Result<(), Box<dyn Error>>;
    fn read_swv(&mut self) -> Result<Vec<u8>, Box<dyn Error>>;
    fn write_word_32(&mut self, addr: u32, data: u32) ->
        Result<(), Box<dyn Error>>;
    fn halt(&mut self) -> Result<(), Box<dyn Error>>;
    fn run(&mut self) -> Result<(), Box<dyn Error>>;
}

pub struct ProbeCore {
    pub core: probe_rs::Core,
    pub session: probe_rs::Session
}

impl Core for ProbeCore {
    fn read_word_32(&mut self, addr: u32) -> Result<u32, Box<dyn Error>> {
        Ok(self.core.read_word_32(addr)?)
    }

    fn read_8(&mut self,
        addr: u32,
        data: &mut [u8]
    ) -> Result<(), Box<dyn Error>> {
        Ok(self.core.read_8(addr, data)?)
    }

    fn write_word_32(&mut self,
        addr: u32,
        data: u32
    ) -> Result<(), Box<dyn Error>> {
        Ok(self.core.write_word_32(addr, data)?)
    }

    fn halt(&mut self) -> Result<(), Box<dyn Error>> {
        self.core.halt()?;
        Ok(())
    }

    fn run(&mut self) -> Result<(), Box<dyn Error>> {
        Ok(self.core.run()?)
    }

    fn init_swv(&mut self) -> Result<(), Box<dyn Error>> {
        Ok(())
    }

    fn read_swv(&mut self) -> Result<Vec<u8>, Box<dyn Error>> {
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

impl OpenOCDCore {
    fn sendcmd(&mut self, cmd: &str) -> Result<String, Box<dyn Error>> {
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
            err!("OpenOCD command \"{}\" failed with \"{}\"", cmd, result)
        } else if result.contains("invalid command name ") {
            err!("OpenOCD command \"{}\" invalid: \"{}\"", cmd, result)
        } else {
            Ok(result)
        }
    }

    fn new() -> Result<OpenOCDCore, Box<dyn Error>> {
        let stream = TcpStream::connect("localhost:6666")
            .map_err(|_| {
                err("can't connect to OpenOCD on port 6666; is it running?")
            })?;

        Ok(Self {
            stream: stream,
            swv: false
        })
    }
}

impl Core for OpenOCDCore {
    fn read_word_32(&mut self, addr: u32) -> Result<u32, Box<dyn Error>> {
        let result = self.sendcmd(&format!("mrw 0x{:x}", addr))?;
        Ok(result.parse::<u32>()?)
    }

    fn read_8(&mut self,
        addr: u32,
        data: &mut [u8]
    ) -> Result<(), Box<dyn Error>> {
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
                        return err!("\"{}\": illegal index {}", cmd, idx);
                    }

                    if seen[idx] {
                        return err!("\"{}\": duplicate index {}", cmd, idx);
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
            if !v.1 {
                return err!("\"{}\": missing index {}", cmd, v.0);
            }
        }

        Ok(())
    }

    fn init_swv(&mut self) -> Result<(), Box<dyn Error>> {
        self.swv = true;
        self.sendcmd("tpiu config disable")?;

        /*
         * XXX: This assumes STM32F4's 16Mhz clock
         */
        self.sendcmd("tpiu config internal - uart on 16000000")?;
        self.sendcmd("tcl_trace on")?;

        Ok(())
    }

    fn read_swv(&mut self) -> Result<Vec<u8>, Box<dyn Error>> {
        if !self.swv {
            self.init_swv()?
        }

        let mut rbuf = vec![0; 8192];
        let mut swv: Vec<u8> = Vec::with_capacity(8192);

        let rval = self.stream.read(&mut rbuf)?;

        if rbuf[rval - 1] != OPENOCD_COMMAND_DELIMITER {
            return err!("missing trace data delimiter: {:?}", rval);
        }

        let rstr = str::from_utf8(&rbuf[0..rval - 1])?;

        if !rstr.starts_with(OPENOCD_TRACE_DATA_BEGIN) {
            return err!("bogus trace data (bad start): {:?}", rval);
        }

        if !rstr.ends_with(OPENOCD_TRACE_DATA_END) {
            return err!("bogus trace data (bad end): {:?}", rval);
        }

        let begin = OPENOCD_TRACE_DATA_BEGIN.len();
        let end = rstr.len() - OPENOCD_TRACE_DATA_END.len();

        for i in (begin..end).step_by(2) {
            if i + 1 >= end {
                return err!("short trace data: {:?}", rval);
            }

            swv.push(u8::from_str_radix(&rstr[i..=i + 1], 16)?);
        }

        Ok(swv)
    }

    fn write_word_32(&mut self,
        addr: u32,
        data: u32
    ) -> Result<(), Box<dyn Error>> {
        self.sendcmd(&format!("mww 0x{:x} 0x{:x}", addr, data))?;
        Ok(())
    }

    fn halt(&mut self) -> Result<(), Box<dyn Error>> {
        self.sendcmd("halt")?;
        Ok(())
    }

    fn run(&mut self) -> Result<(), Box<dyn Error>> {
        self.sendcmd("resume")?;
        Ok(())
    }
}

pub fn attach(
    ocd: &str,
    chip: &str,
) -> Result<Box<dyn Core>, Box<dyn Error>> {
    match ocd {
        "probe" => {
            let probes = Probe::list_all();
            let probe = probes[0].open()?;

            info!("attaching as chip {} ...", chip);
            let session = probe.attach(chip)?;

            let core = session.attach_to_core(0)?;
            info!("attached");

            Ok(Box::new(ProbeCore {
                session: session,
                core: core
            }))
        }

        "ocd" => {
            let mut core = OpenOCDCore::new()?;
            let version = core.sendcmd("version")?;

            if !version.contains("Open On-Chip Debugger") {
                return err!("version string unrecognized: \"{}\"", version);
            }

            info!("attached to OpenOCD");

            Ok(Box::new(core))
        }

        _ => {
            err!("unrecognized on-chip debugger: {}", ocd)
        }
    }
}
