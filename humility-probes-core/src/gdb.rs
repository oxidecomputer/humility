// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::{anyhow, bail, Result};
use humility::core::Core;
use humility_arch_arm::ARMRegister;
use std::fmt;
use std::io::{Read, Write};
use std::net::TcpStream;
use std::path::Path;
use std::time::Duration;

#[derive(Copy, Clone, Debug, PartialEq)]
pub(crate) enum GDBServer {
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

        //
        // Tack on the goofy checksum beyond the end of the packet.
        //
        let trailer = &format!("{}{:02x}", GDB_PACKET_END, cksum % 256);

        for b in trailer.as_bytes() {
            payload.push(*b);
        }

        log::trace!("sending {}", std::str::from_utf8(&payload).unwrap());
        payload
    }

    fn firecmd(&mut self, cmd: &str) -> Result<()> {
        let mut rbuf = vec![0; 1024];
        let payload = self.prepcmd(cmd);

        self.stream.write_all(&payload)?;

        //
        // We are expecting no result -- just an ack.
        //
        let rval = self.stream.read(&mut rbuf)?;

        if rval != 1 {
            bail!("cmd {} returned {} bytes: {:?}", cmd, rval,
                std::str::from_utf8(&rbuf));
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

            result.push_str(std::str::from_utf8(&rbuf[0..rval])?);
            log::trace!("response: {}", result);

            //
            // We are done when we have our closing delimter followed by
            // the two byte checksum.
            //
            if result.find(GDB_PACKET_END) == Some(result.len() - 3) {
                break;
            }
        }

        //
        // We have our response, so ack it back
        //
        self.sendack()?;

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

        log::trace!("command {} returned {}", cmd, rstr);

        match rstr.len() {
            2 => Ok(u32::from(u8::from_le_bytes(buf[..].try_into().unwrap()))),
            4 => Ok(u32::from(u16::from_le_bytes(buf[..].try_into().unwrap()))),
            8 => Ok(u32::from_le_bytes(buf[..].try_into().unwrap())),
            16 => {
                //
                // Amazingly, for some 32-bit register values under certain
                // circumstances the JLink seems to return a 64-bit value
                // (!). We confirm that this value is
                // representable and return it.
                //
                let val = u64::from_le_bytes(buf[..].try_into().unwrap());

                if val > u32::MAX.into() {
                    Err(anyhow!("bad 64-bit return on cmd {}: {}", cmd, rstr))
                } else {
                    Ok(val as u32)
                }
            }
            _ => Err(anyhow!("bad return on cmd {}: {}", cmd, rstr)),
        }
    }

    pub(crate) fn new(server: GDBServer) -> Result<GDBCore> {
        let port = match server {
            GDBServer::OpenOCD => 3333,
            GDBServer::JLink => 2331,
        };

        let host = format!("127.0.0.1:{port}");
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

        //
        // Both the OpenOCD and JLink GDB servers stop the target upon
        // connection.  This is helpful in that we know the state that
        // we're in -- but it's also not the state that we want to be
        // in.  We explicitly run the target before returning.
        //
        let mut core = Self { stream, server, halted: true };

        let supported = core.sendcmd("qSupported")?;
        log::trace!("{} supported string: {}", server, supported);

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
        self.send_32(&format!("m{addr:x},4"))
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
            //
            // Maddeningly, the JLink stops the target whenever a register
            // is read.
            //
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
        log::trace!("halt reply: {}", reply);
        self.halted = true;

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
            self.firecmd("c")?;
            self.halted = false;
        }

        Ok(())
    }

    fn step(&mut self) -> Result<()> {
        Ok(())
    }

    fn load(&mut self, _path: &Path) -> Result<()> {
        bail!("Flash loading is not supported with GDB");
    }

    fn reset(&mut self) -> Result<()> {
        bail!("Reset is not supported with GDB");
    }

    fn reset_and_halt(&mut self, _dur: std::time::Duration) -> Result<()> {
        bail!("Reset is not supported with OpenOCD");
    }

    fn wait_for_halt(&mut self, _dur: std::time::Duration) -> Result<()> {
        bail!("Wait for halt is not supported with GDB");
    }
}
