// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility attest`
//!
//! Communicate with the `attest` task
//!
//! ```console
//! $ humility attest cert-len 0
//! 561
//! $ humility attest cert-chain-len
//! 4
//! $ humility attest cert --file cert0.pem 0
//! $ humility attest cert 0 > cert0.pem
//! $ humility attest cert-chain --file cert-chain.pem
//! $ humility attest cert-chain > cert-chain.pem
//! ```
//!

use anyhow::{bail, Result};
use clap::CommandFactory;
use clap::Parser;
use humility::{core::Core, hubris::HubrisArchive};
use humility_cli::{ExecutionContext, Subcommand};
use humility_cmd::{Archive, Attach, Command, CommandKind, Validate};
use humility_hiffy::{HiffyContext, HiffyLease};
use humility_idol::{HubrisIdol, IdolArgument};
use std::{
    fs::File,
    io::{self, Write},
    path::PathBuf,
};

#[derive(Parser, Debug)]
#[clap(
    name = "attest",
    about = "Send commands to the the root of trust for reporting"
)]
struct AttestArgs {
    /// sets timeout
    #[clap(
        long, short = 'T', default_value_t = 50000, value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    #[clap(subcommand)]
    cmd: AttestCmd,
}

#[derive(Parser, Debug)]
#[clap(name = "subcmd")]
enum AttestCmd {
    /// Get a cert from the RoT-R.
    Cert {
        /// Optional path to file where cert is written. If omitted cert
        /// will be written to stdout.
        #[clap(long, short, parse(from_os_str))]
        file: Option<PathBuf>,

        /// Index of cert in Alias cert chain
        index: u32,
    },
    /// Get each cert in the Alias / attestation signing cert chain.
    CertChain {
        /// Optional path to file where cert is written. If omitted cert
        /// will be written to stdout.
        #[clap(long, short, parse(from_os_str))]
        file: Option<PathBuf>,
    },
    /// Get number of certs in cert chain
    CertChainLen,
    /// Get the length of a cert in cert chain
    CertLen {
        /// Index of cert in cert chain
        index: u32,
    },
}

struct AttestHandler<'a> {
    hubris: &'a HubrisArchive,
    core: &'a mut dyn Core,
    context: HiffyContext<'a>,
}

impl<'a> AttestHandler<'a> {
    pub fn new(
        hubris: &'a HubrisArchive,
        core: &'a mut dyn Core,
        hiffy_timeout: u32,
    ) -> Result<Self> {
        let context = HiffyContext::new(hubris, core, hiffy_timeout)?;
        Ok(Self { hubris, core, context })
    }

    /// Get the length / number of certs in the Alias / attestation signing
    /// cert chain.
    pub fn cert_chain_len(&mut self) -> Result<u32> {
        let cmd = self.hubris.get_idol_command("Attest.cert_chain_len")?;

        let cert_chain_len = match humility_hiffy::hiffy_call(
            self.hubris,
            self.core,
            &mut self.context,
            &cmd,
            &[],
            None,
        )? {
            Ok(v) => v.as_base()?.as_u32().ok_or_else(|| {
                anyhow::anyhow!("Couldn't get a u32 for cert chain length")
            })?,
            Err(e) => bail!("Hiffy error getting cert chain length: {}", e),
        };

        Ok(cert_chain_len)
    }

    /// Get the length of the certificate at the provided index in the Alias
    /// / attestation signing cert chain.
    pub fn cert_len(&mut self, index: u32) -> Result<u32> {
        let cmd = self.hubris.get_idol_command("Attest.cert_len")?;

        let len = match humility_hiffy::hiffy_call(
            self.hubris,
            self.core,
            &mut self.context,
            &cmd,
            &[("index", IdolArgument::Scalar(index as u64))],
            None,
        )? {
            Ok(v) => v.as_base()?.as_u32().ok_or_else(|| {
                anyhow::anyhow!("Couldn't get a u32 for cert chain length")
            })?,
            Err(e) => bail!("Hiffy error getting cert chain length: {}", e),
        };

        Ok(len)
    }

    /// Get certificate at the provide index from Alias / attestation signing
    /// cert chain.
    pub fn cert(&mut self, index: u32) -> Result<String> {
        let cert_len = self.cert_len(index)? as usize;
        let mut cert = vec![0u8; cert_len];

        let cmd = self.hubris.get_idol_command("Attest.cert")?;
        for (i, chunk) in cert.chunks_mut(256).enumerate() {
            let offset = i * 256;
            match humility_hiffy::hiffy_call(
                self.hubris,
                self.core,
                &mut self.context,
                &cmd,
                &[
                    ("index", IdolArgument::Scalar(index as u64)),
                    ("offset", IdolArgument::Scalar(offset as u64)),
                ],
                Some(HiffyLease::Read(chunk)),
            )? {
                Ok(_) => (),
                Err(e) => bail!("Hiffy error sending hash: {}", e),
            };
        }

        // PEM encode certificate w/ unix / linux line endings
        let pem = pem::Pem { tag: String::from("CERTIFICATE"), contents: cert };
        let cert = pem::encode_config(
            &pem,
            pem::EncodeConfig { line_ending: pem::LineEnding::LF },
        );

        Ok(cert)
    }
}

fn attest(context: &mut ExecutionContext) -> Result<()> {
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let subargs = AttestArgs::try_parse_from(subargs)?;

    let hubris = context.archive.as_ref().unwrap();
    let core = &mut **context.core.as_mut().unwrap();

    let mut handler = AttestHandler::new(hubris, core, subargs.timeout)?;

    match subargs.cmd {
        AttestCmd::Cert { file, index } => {
            humility::msg!("getting cert ...");
            let cert = handler.cert(index)?;

            let mut out: Box<dyn Write> = match file {
                Some(file) => Box::new(File::create(file)?),
                None => Box::new(io::stdout()),
            };

            match out.write_all(cert.as_bytes()) {
                Ok(_) => Ok(()),
                Err(e) => bail!("failed to write to file: {}", e),
            }
        }
        AttestCmd::CertChain { file } => {
            humility::msg!("getting cert chain ...");
            let mut out: Box<dyn Write> = match file {
                Some(file) => Box::new(File::create(file)?),
                None => Box::new(io::stdout()),
            };

            // Get each cert in the Alias / attestation signing cert chain
            // starting from the leaf and ending at either the self-signed,
            // PUF derived identity cert, or the last intermediate in the
            // identity manufacturing PKI.
            for index in 0..handler.cert_chain_len()? {
                let cert = handler.cert(index)?;

                match out.write_all(cert.as_bytes()) {
                    Ok(_) => (),
                    Err(e) => bail!("failed to write cert: {}", e),
                }
            }

            Ok(())
        }
        AttestCmd::CertLen { index } => {
            humility::msg!("getting length of cert at index {} ...", index);
            match handler.cert_len(index) {
                Ok(len) => {
                    println!("{}", len);
                    Ok(())
                }
                Err(e) => bail!("{}", e),
            }
        }
        AttestCmd::CertChainLen => {
            humility::msg!("getting length of cert chain ...");
            match handler.cert_chain_len() {
                Ok(len) => {
                    println!("{}", len);
                    Ok(())
                }
                Err(e) => bail!("{}", e),
            }
        }
    }
}

pub fn init() -> Command {
    Command {
        app: AttestArgs::command(),
        name: "attest",
        run: attest,
        kind: CommandKind::Attached {
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
        },
    }
}
