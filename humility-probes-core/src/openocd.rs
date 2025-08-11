// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::probe_rs::CORE_MAX_READSIZE;
use anyhow::{anyhow, bail, ensure, Result};
use humility::core::Core;
use humility_arch_arm::ARMRegister;
use std::io::{Read, Write};
use std::net::TcpStream;
use std::path::Path;
use std::time::Duration;

const OPENOCD_COMMAND_DELIMITER: u8 = 0x1a;

pub struct OpenOCDCore {
    stream: TcpStream,
}

#[rustfmt::skip::macros(anyhow, bail)]
impl OpenOCDCore {
    pub(crate) fn sendcmd(&mut self, cmd: &str) -> Result<String> {
        let mut rbuf = vec![0; 1024];
        let mut result = String::with_capacity(16);

        let mut str = String::from(cmd);
        str.push(OPENOCD_COMMAND_DELIMITER as char);

        self.stream.write_all(str.as_bytes())?;

        loop {
            let rval = self.stream.read(&mut rbuf)?;

            if rbuf[rval - 1] == OPENOCD_COMMAND_DELIMITER {
                result.push_str(std::str::from_utf8(&rbuf[0..rval - 1])?);
                break;
            }

            result.push_str(std::str::from_utf8(&rbuf[0..rval])?);
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

    pub(crate) fn new() -> Result<OpenOCDCore> {
        let addr = "127.0.0.1:6666".parse()?;
        let timeout = Duration::from_millis(100);
        let stream =
            TcpStream::connect_timeout(&addr, timeout).map_err(|_| {
                anyhow!("can't connect to OpenOCD on port 6666; is it running?")
            })?;

        Ok(Self { stream })
    }
}

#[rustfmt::skip::macros(anyhow, bail)]
impl Core for OpenOCDCore {
    fn info(&self) -> (String, Option<String>) {
        ("OpenOCD".to_string(), None)
    }

    fn read_word_32(&mut self, addr: u32) -> Result<u32> {
        let result = self.sendcmd(&format!("mrw 0x{addr:x}"))?;
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

    fn write_word_32(&mut self, addr: u32, data: u32) -> Result<()> {
        self.sendcmd(&format!("mww 0x{addr:x} 0x{data:x}"))?;
        Ok(())
    }

    fn write_8(&mut self, addr: u32, data: &[u8]) -> Result<()> {
        //
        // Perform the writes one byte at a time.  We (obviously?) don't
        // expect these writes to be large (they are likely due to HIF
        // execution), but if they become so, this can be made up to 4X faster
        // (and, it must be said, significantly more complicate) by using mww
        // for the word-aligned writes within the data payload.
        //
        for (i, b) in data.iter().enumerate() {
            self.sendcmd(&format!("mwb 0x{:x} 0x{:x}", addr + i as u32, b))?;
        }

        Ok(())
    }

    fn halt(&mut self) -> Result<()> {
        //
        // On OpenOCD, we don't halt: if GDB is connected, it gets really,
        // really confused!  (There is unfortunately no way to know if GDB is
        // connected or not, but because the OpenOCD target is most often used
        // when GDB is running, we assume that it is indeed running.)
        //
        Ok(())
    }

    fn run(&mut self) -> Result<()> {
        //
        // Well, see above.
        //
        Ok(())
    }

    fn step(&mut self) -> Result<()> {
        todo!();
    }

    fn load(&mut self, _path: &Path) -> Result<()> {
        bail!("Flash loading is not supported with OpenOCD");
    }

    fn reset(&mut self) -> Result<()> {
        bail!("Reset is not supported with OpenOCD");
    }

    fn reset_and_halt(&mut self, _dur: std::time::Duration) -> Result<()> {
        bail!("Reset is not supported with OpenOCD");
    }

    fn wait_for_halt(&mut self, _dur: std::time::Duration) -> Result<()> {
        bail!("Wait for halt is not supported with OpenOCD");
    }
}
