// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
use crate::DumpAgent;
use anyhow::{anyhow, bail, Context, Result};
use humility::core::{Core, NetAgent};
use rand::Rng;

pub struct UdpDumpAgent<'a> {
    core: &'a mut dyn Core,
}

impl<'a> UdpDumpAgent<'a> {
    pub fn new(core: &'a mut dyn Core) -> Self {
        Self { core }
    }

    /// Sends a remote dump command over the network
    pub fn dump_remote_action(
        &mut self,
        msg: humpty::udp::Request,
    ) -> Result<Result<humpty::udp::Response, humpty::udp::Error>> {
        use humpty::udp::{version, Header, RequestMessage, ResponseMessage};
        let mut rng = rand::thread_rng();
        let header =
            Header { version: version::CURRENT, message_id: rng.gen() };
        let mut buf = vec![
            0u8;
            std::cmp::max(
                std::mem::size_of::<RequestMessage>(),
                std::mem::size_of::<ResponseMessage>()
            )
        ];
        let size = hubpack::serialize(&mut buf, &(header, msg))
            .context("failed to serialize message")?;

        // Send the packet out
        self.core
            .send(&buf[..size], NetAgent::DumpAgent)
            .context("failed to send packet")?;

        // Try to receive a reply
        let size = self
            .core
            .recv(buf.as_mut_slice(), NetAgent::DumpAgent)
            .context("failed to receive packet")?;

        let (reply_header, rest): (Header, _) =
            hubpack::deserialize(&buf[..size])
                .map_err(|_| anyhow!("deserialization of header failed"))?;
        if reply_header.message_id != header.message_id {
            bail!(
                "message ID mismatch: {} != {}",
                reply_header.message_id,
                header.message_id
            );
        } else if reply_header.version < version::MIN {
            bail!(
                "received reply with invalid version: {} < our min ({})",
                reply_header.version,
                version::MIN
            );
        }

        let (reply, _) = hubpack::deserialize(rest).map_err(|_| {
            if reply_header.version > version::CURRENT {
                anyhow!(
                    "deserialization of body failed, \
                        version {} > our version {}",
                    reply_header.version,
                    version::CURRENT
                )
            } else {
                anyhow!(
                    "deserialization of body failed, despite versions matching"
                )
            }
        })?;

        Ok(reply)
    }
}

impl<'a> DumpAgent for UdpDumpAgent<'a> {
    fn read_generic(
        &mut self,
        areas: &mut dyn Iterator<Item = (u8, u32)>,
        cont: &mut dyn FnMut(u8, u32, &[u8]) -> Result<bool>,
    ) -> Result<Vec<Vec<u8>>> {
        let mut out = vec![];
        for (index, offset) in areas {
            let r =
                self.dump_remote_action(humpty::udp::Request::ReadDump {
                    index,
                    offset,
                })?;
            match r {
                Ok(humpty::udp::Response::ReadDump(d)) => {
                    if !cont(index, offset, &d)? {
                        break;
                    } else {
                        out.push(d.to_vec())
                    }
                }
                Err(humpty::udp::Error::InvalidArea) => break,
                _ => bail!("invalid reply: {r:?}"),
            }
        }
        Ok(out)
    }

    fn core(&mut self) -> &mut dyn Core {
        self.core
    }

    fn initialize_dump(&mut self) -> Result<()> {
        let r =
            self.dump_remote_action(humpty::udp::Request::InitializeDump)?;
        match r {
            Ok(humpty::udp::Response::InitializeDump) => Ok(()),
            _ => bail!("invalid response: {r:?}"),
        }
    }

    fn initialize_segments(&mut self, segments: &[(u32, u32)]) -> Result<()> {
        for &(address, length) in segments {
            let r = self.dump_remote_action(
                humpty::udp::Request::AddDumpSegment { address, length },
            )?;
            match r {
                Ok(humpty::udp::Response::AddDumpSegment) => (),
                _ => bail!("invalid response: {r:?}"),
            }
        }
        Ok(())
    }

    fn take_dump(&mut self) -> Result<()> {
        let r = self.dump_remote_action(humpty::udp::Request::TakeDump)?;
        match r {
            Ok(humpty::udp::Response::TakeDump) => Ok(()),
            _ => bail!("invalid response: {r:?}"),
        }
    }

    fn dump_task(&mut self, task_index: u32) -> Result<u8> {
        let r = self.dump_remote_action(humpty::udp::Request::DumpTask {
            task_index,
        })?;
        match r {
            Ok(humpty::udp::Response::DumpTask(out)) => Ok(out),
            _ => bail!("invalid response: {r:?}"),
        }
    }

    fn dump_task_region(
        &mut self,
        task_index: u32,
        start: u32,
        length: u32,
    ) -> Result<u8> {
        let r =
            self.dump_remote_action(humpty::udp::Request::DumpTaskRegion {
                task_index,
                start,
                length,
            })?;
        match r {
            Ok(humpty::udp::Response::DumpTaskRegion(out)) => Ok(out),
            _ => bail!("invalid response: {r:?}"),
        }
    }
}
