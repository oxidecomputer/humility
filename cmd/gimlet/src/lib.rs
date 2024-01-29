// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility gimlet`
//!
//! `humility gimlet` contacts a (recent firmware version) Gimlet over the
//! management network and extracts certain live diagnostic information.
//!
//! ```console
//! $ humility gimlet --ip fe80::0c1d:9aff:fe64:b8c2%en0 read-seq-regs
//! ```
//!
//! For a complete list of subcommands, use `humility gimlet --help`.

use std::net::{SocketAddrV6, UdpSocket};
use std::time::Duration;

use anyhow::{bail, Context, Result};
use clap::{ArgGroup, IntoApp, Parser};
use humility::net::decode_iface;
use humility_cli::{ExecutionContext, Subcommand};
use humility_cmd::{Archive, Command, CommandKind, Dumper};

/// This is defined in the gimlet TOML.
const HARDCODED_PORT: u16 = 23547;

#[derive(Parser, Debug)]
#[clap(
    name = "gimlet", about = env!("CARGO_PKG_DESCRIPTION"),
    group = ArgGroup::new("target").multiple(false)
)]
struct Args {
    /// How long to wait for a response from the target, in milliseconds.
    #[clap(
        long, short = 'T', default_value_t = 2000, value_name = "ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u64,

    /// UDP port to contact on the target.
    #[clap(long, default_value_t = HARDCODED_PORT)]
    port: u16,

    /// IPv6 address, e.g. `fe80::0c1d:9aff:fe64:b8c2%en0`
    #[clap(
        long,
        env = "HUMILITY_RPC_IP",
        group = "target",
        use_value_delimiter = true
    )]
    ip: String,

    #[clap(subcommand)]
    cmd: SubCmd,
}

#[derive(Parser, Debug)]
enum SubCmd {
    /// Read the current contents of the sequencer FPGA registers.
    ReadSeqRegs,
}

pub struct Client {
    socket: UdpSocket,
    // Allocating this overly-large buffer because we're on the big computer
    // where RAM is cheap, and it's less likely to need adjustment this way.
    buf: [u8; 1024],
}

impl Client {
    pub fn new(ip: &str, port: u16, timeout: Duration) -> Result<Self> {
        let Some((ip, scope)) = ip.rsplit_once('%') else {
            bail!("Missing scope id in IP (e.g. '%en0')")
        };

        let scopeid = decode_iface(scope)?;

        let dest = SocketAddrV6::new(
            ip.parse()?,
            port,
            0, // flow info
            scopeid,
        );

        let socket = UdpSocket::bind("[::]:0")?;
        socket.set_read_timeout(Some(timeout))?;
        socket.connect(dest)?;

        Ok(Self { socket, buf: [0; 1024] })
    }

    pub fn read_sequencer_registers(&mut self) -> Result<Vec<u8>> {
        use gimlet_inspector_protocol::{
            QueryV0, Request, SequencerRegistersResponseV0,
        };

        let len = hubpack::serialize(
            &mut self.buf,
            &Request::V0(QueryV0::SequencerRegisters),
        )?;

        self.socket.send(&self.buf[..len])?;
        let n = self.socket.recv(&mut self.buf).context("receiving packet")?;
        let buf = &self.buf[..n];

        let (response, extra) =
            hubpack::deserialize::<SequencerRegistersResponseV0>(buf)?;
        match response {
            SequencerRegistersResponseV0::Success => Ok(extra.to_vec()),
            e => {
                bail!("failed: {e:?}");
            }
        }
    }
}

fn run(context: &mut ExecutionContext) -> Result<()> {
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let subargs = Args::try_parse_from(subargs)?;

    let mut client = Client::new(
        &subargs.ip,
        subargs.port,
        Duration::from_millis(subargs.timeout),
    )?;

    // We can generalize this when we have more than one command defined.
    match subargs.cmd {
        SubCmd::ReadSeqRegs => {
            let bytes = client.read_sequencer_registers()?;
            println!("Received register contents follow:");
            let mut dumper = Dumper::new();
            dumper.size = 1;
            dumper.dump(&bytes, 0);
        }
    }

    Ok(())
}

pub fn init() -> Command {
    Command {
        app: Args::command(),
        name: "gimlet",
        run,
        kind: CommandKind::Detached { archive: Archive::Ignored },
    }
}
