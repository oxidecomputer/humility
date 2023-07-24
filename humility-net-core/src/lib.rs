// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! Humility `Core` which communicates over the network
//!
//! A `NetCore` talks to two sockets on the Hubris target:
//! - `udprpc`, which executes arbitrary Idol calls via an ad-hoc protocol
//! - `dump_agent`, which executes dump-related messages via a protocol
//!   specified in `humpty::udp`
//!
//! In addition, it contains the contents of flash from the Hubris image,
//! meaning it can handle flash reads locally.

use anyhow::{anyhow, bail, Context, Result};
use humility::{
    core::{Core, NetAgent},
    hubris::{HubrisArchive, HubrisFlashMap, HubrisRegion, HubrisTask},
    msg,
    net::decode_iface,
};
use humility_arch_arm::ARMRegister;
use humility_dump_agent::{
    DumpAgent, DumpAgentCore, DumpAgentExt, DumpArea, UdpDumpAgent,
};
use std::{
    net::{ToSocketAddrs, UdpSocket},
    path::Path,
    time::Duration,
};

pub struct NetCore {
    /// Socket to communicate with `udprpc`, or `None` if it's not present
    udprpc_socket: Option<UdpSocket>,
    flash: HubrisFlashMap,

    /// Socket to communicate with the dump agent, or `None` if it's not present
    dump_agent_socket: Option<UdpSocket>,

    /// Map of RAM regions, or `None` if the dump agent can't read RAM
    ram: Option<Vec<HubrisRegion>>,

    /// contents of image ID
    imageid: Vec<u8>,
}

impl NetCore {
    fn new(
        ip: &str,
        hubris: &HubrisArchive,
        timeout: Duration,
    ) -> Result<Self> {
        let mut iter = ip.split('%');
        let ip = iter.next().expect("ip address is empty");
        let iface = iter
            .next()
            .ok_or_else(|| anyhow!("Missing scope id in IP (e.g. '%en0')"))?;

        let scopeid = decode_iface(iface)?;

        let udprpc_socket = if hubris.lookup_task("udprpc").is_some() {
            // See oxidecomputer/oana for standard Hubris UDP ports
            let target = format!("[{}%{}]:998", ip, scopeid);

            let dest = target.to_socket_addrs()?.collect::<Vec<_>>();
            let udprpc_socket = UdpSocket::bind("[::]:0")?;
            udprpc_socket.set_read_timeout(Some(timeout))?;
            udprpc_socket.connect(&dest[..])?;
            Some(udprpc_socket)
        } else {
            None
        };

        // We'll check to see if there's a dump agent available over UDP, which
        // means
        // 1) There's a task implementing the `DumpAgent` interface
        // 2) That task has the `net` feature enabled

        // Find the dump agent task name.  This is usually `dump_agent`, but
        // that's not guaranteed; what *is* guaranteed is that it implements the
        // DumpAgent interface.
        let dump_agent_task =
            hubris.lookup_module_by_iface("DumpAgent").map(|t| t.task);
        let has_dump_agent = dump_agent_task
            .map(|t| hubris.does_task_have_feature(t, "net").unwrap())
            .unwrap_or(false);

        //
        // See oxidecomputer/oana for standard Hubris UDP ports
        let dump_agent_socket = if has_dump_agent {
            let target = format!("[{}%{}]:11113", ip, scopeid);
            let dest = target.to_socket_addrs()?.collect::<Vec<_>>();
            let dump_agent_socket = UdpSocket::bind("[::]:0")?;
            dump_agent_socket.set_read_timeout(Some(timeout))?;
            dump_agent_socket.connect(&dest[..])?;
            Some(dump_agent_socket)
        } else {
            None
        };

        let mut out = Self {
            udprpc_socket,
            dump_agent_socket,
            flash: HubrisFlashMap::new(hubris)?,
            ram: None, // filled in below
            imageid: hubris
                .imageid
                .as_ref()
                .ok_or_else(|| anyhow!("missing image ID"))?
                .1
                .clone(),
        };

        // Check for the existence of the DumpAgent.dump_task_region API, which
        // indicates that we are able to read RAM regions over the network.
        let has_dump_task_region = hubris
            .lookup_module_by_iface("DumpAgent")
            .and_then(|m| {
                m.iface.as_ref().map(|i| i.ops.contains_key("dump_task_region"))
            })
            .unwrap_or(false);

        if has_dump_task_region {
            // Surprisingly, this works: `hubris.regions` only reads data from
            // the kernel's task table, which is in flash, so this works despite
            // using a NetCore (which cannot read arbitrary memory).
            let regions = hubris.regions(&mut out)?;
            let mut ram_regions: Vec<_> = regions
                .into_values()
                .filter(|r| {
                    !r.attr.device
                        && !r.attr.external
                        && !r.attr.execute
                        && r.attr.read
                        && r.attr.write
                        && !r.tasks.is_empty()
                        && r.tasks
                            .iter()
                            .any(|t| matches!(t, HubrisTask::Task(..)))
                })
                .collect();

            // Each task is _also_ allowed to read from its own block in the
            // kernel's task table, for remote dumping.  We add those regions to
            // our RAM map here:
            let (base, task_count) = hubris.task_table(&mut out)?;
            let task_t = hubris.lookup_struct_byname("Task")?;
            for t in 0..task_count {
                let base = base + t * task_t.size as u32;
                ram_regions.push(HubrisRegion {
                    daddr: None,
                    base,
                    // Dummy attributes
                    attr: humility::hubris::HubrisRegionAttr {
                        read: true,
                        write: false,
                        execute: false,
                        device: false,
                        dma: false,
                        external: false,
                    },
                    size: task_t.size as u32,
                    tasks: vec![HubrisTask::Task(t)],
                })
            }

            // Sort regions so we can find a target region with binary search
            ram_regions.sort_by_key(|r| r.base);

            // Assert a simple invariant to detect weird images
            for (a, b) in ram_regions.iter().zip(ram_regions.iter().skip(1)) {
                if b.base < a.base + a.size {
                    bail!("overlapping regions: {a:x?}; {b:x?}");
                }
            }

            out.ram = Some(ram_regions);
        }
        Ok(out)
    }

    fn read(&mut self, addr: u32, data: &mut [u8]) -> Result<()> {
        if self.flash.read(addr, data).is_some() {
            Ok(())
        } else {
            self.read_ram(addr, data).context(format!(
                "0x{addr:0x} can't be read via the archive or over the network"
            ))
        }
    }

    /// Reads data from RAM using the (UDP) dump agent as a proxy
    fn read_ram(&mut self, addr: u32, data: &mut [u8]) -> Result<()> {
        if self.dump_agent_socket.is_none() {
            bail!("no dump agent available");
        }
        let Some(ram) = self.ram.clone() else {
            bail!("dump agent cannot read RAM; is your Hubris archive too old?")
        };

        let mut agent_core = DumpAgentCore::new(self.flash.clone());
        let image_id = self.imageid.clone();

        let mut udp_dump = UdpDumpAgent::new(self, &image_id)?;
        let mut aligned_start = addr & !0b11;

        // Bytes remaining is signed, which is non-intuitive; because we can
        // only read aligned chunks of memory, we may read more than our
        // requested size, which would make `remaining` negative.
        let mut remaining = (data.len() as u32 + addr - aligned_start) as isize;
        while remaining > 0 {
            let p = ram.partition_point(|v| v.base <= aligned_start);
            if p == 0 {
                bail!(
                    "address not found in RAM map; \
                     note that non-TCB kernel memory cannot be read remotely"
                );
            }

            // By construction, we know that every region in self.ram has at
            // least one task associated with it.  If this task is the
            // supervisor, then that's bad news.
            let region = ram[p - 1].clone();
            let Some(&task) = region.tasks.iter().find(|t| t.task() != 0)
            else {
                bail!("supervisor memory cannot be read using dump facilities");
            };

            if aligned_start > region.base + region.size {
                bail!(
                    "out of range: {aligned_start:x} >= {:x}",
                    region.base + region.size
                );
            }

            // The read size is clamped by the region and remaining bytes
            let mut read_size = (remaining as u32).min(region.size);

            // In addition, it must be a multiple of 4 bytes for alignment
            while read_size & 0b11 != 0 {
                read_size += 1;
            }

            // Read this chunk of memory remotely
            let dump_index: u8 = match udp_dump.dump_task_region(
                task.task(),
                aligned_start,
                read_size,
            ) {
                Ok(addr) => addr,
                Err(e) if e.to_string().contains("DumpAreaInUse") => {
                    bail!(
                        "out of space for dump_agent; use \
                         `humility dump --initialize-dump-agent` to free space"
                    )
                }
                Err(e) => {
                    bail!("dump agent failed: {e:?}")
                }
            };

            // Collect this region into our ersatz DumpAgentCore
            //
            // Note that we don't bail out early here, because we need to do
            // cleanup before returning.
            let a = (&mut udp_dump as &mut dyn DumpAgent).read_dump(
                Some(DumpArea::ByIndex(dump_index as usize)),
                &mut agent_core,
                false,
            );

            // Pop the most recent dump, since we were just using it to read
            // memory and it doesn't need to take up a dump area forever.  Note
            // that this will also pop any dumps which have occurred
            // (autonomously) in the meantime, but that's preferable to filling
            // up all of dump memory with a single call to `humility ringbuf`.
            let b = udp_dump.reinitialize_dump_from(dump_index);

            // Examine the error codes
            match (a, b) {
                (Err(a), Ok(..)) => return Err(a),
                (Err(a), Err(b)) => {
                    humility::warn!(
                        "error {b} while reinitializing dump \
                         after a previous error"
                    );
                    return Err(a);
                }
                (Ok(..), Err(b)) => return Err(b),
                (Ok(..), Ok(..)) => (),
            }

            aligned_start += read_size;
            remaining -= read_size as isize;
        }

        // By construction, this DumpAgentCore has exactly what it needs!
        agent_core.read_8(addr, data)?;

        Ok(())
    }
}

#[rustfmt::skip::macros(bail)]
impl Core for NetCore {
    fn info(&self) -> (String, Option<String>) {
        ("connected remotely".to_string(), None)
    }

    fn is_net(&self) -> bool {
        true
    }

    fn set_timeout(&mut self, timeout: Duration) -> Result<()> {
        if let Some(d) = self.udprpc_socket.as_ref() {
            d.set_read_timeout(Some(timeout))?;
        }
        if let Some(d) = self.dump_agent_socket.as_ref() {
            d.set_read_timeout(Some(timeout))?;
        }
        Ok(())
    }

    fn send(&self, buf: &[u8], target: NetAgent) -> Result<usize> {
        match target {
            NetAgent::UdpRpc => {
                if let Some(d) = self.udprpc_socket.as_ref() {
                    d.send(buf)
                } else {
                    bail!("no `udprpc` socket");
                }
            }
            NetAgent::DumpAgent => {
                if let Some(d) = self.dump_agent_socket.as_ref() {
                    d.send(buf)
                } else {
                    bail!("no dump agent socket")
                }
            }
        }
        .map_err(anyhow::Error::from)
    }

    fn recv(&self, buf: &mut [u8], target: NetAgent) -> Result<usize> {
        match target {
            NetAgent::UdpRpc => {
                if let Some(d) = self.udprpc_socket.as_ref() {
                    d.recv(buf)
                } else {
                    bail!("no `udprpc` socket");
                }
            }
            NetAgent::DumpAgent => {
                if let Some(d) = self.dump_agent_socket.as_ref() {
                    d.recv(buf)
                } else {
                    bail!("no dump agent socket")
                }
            }
        }
        .map_err(anyhow::Error::from)
    }

    fn read_8(&mut self, addr: u32, data: &mut [u8]) -> Result<()> {
        self.read(addr, data)
    }

    fn read_reg(&mut self, reg: ARMRegister) -> Result<u32> {
        bail!("cannot read register {} over network", reg);
    }

    fn write_reg(&mut self, reg: ARMRegister, _value: u32) -> Result<()> {
        bail!("cannot write register {} over network", reg);
    }

    fn write_word_32(&mut self, _addr: u32, _data: u32) -> Result<()> {
        bail!("cannot write a word over network");
    }

    fn write_8(&mut self, _addr: u32, _data: &[u8]) -> Result<()> {
        bail!("cannot write a byte over network");
    }

    fn halt(&mut self) -> Result<()> {
        Ok(())
    }

    fn run(&mut self) -> Result<()> {
        Ok(())
    }

    fn step(&mut self) -> Result<()> {
        bail!("can't step over network");
    }

    fn init_swv(&mut self) -> Result<()> {
        bail!("cannot enable SWV over network");
    }

    fn read_swv(&mut self) -> Result<Vec<u8>> {
        bail!("cannot read SWV over network");
    }

    fn load(&mut self, _path: &Path) -> Result<()> {
        bail!("cannot load flash over network");
    }

    fn reset(&mut self) -> Result<()> {
        bail!("cannot reset over network");
    }

    fn reset_and_halt(&mut self, _dur: std::time::Duration) -> Result<()> {
        bail!("cannot reset over network");
    }

    fn wait_for_halt(&mut self, _dur: std::time::Duration) -> Result<()> {
        bail!("cannot wait for halt over network");
    }
}

pub fn attach_net(
    ip: &str,
    hubris: &HubrisArchive,
    timeout: Duration,
) -> Result<Box<dyn Core>> {
    let core = NetCore::new(ip, hubris, timeout)?;
    msg!("connecting to {ip}");
    Ok(Box::new(core))
}
