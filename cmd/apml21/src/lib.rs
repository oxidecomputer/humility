// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility apml21`
//!
//! Make RPC calls to AMD's AMPL service via SB-RMI over i2c.
//!
//! Prints out the received values, interpreting them for the
//! given RPC.

use anyhow::Result;
use clap::{ArgGroup, CommandFactory, Parser};
use hif::Op;
use humility_cli::{ExecutionContext, Subcommand};
use humility_cmd::CommandKind;
use humility_cmd::{Archive, Attach, Command, Validate};
use humility_hiffy::HiffyContext;
use humility_idol::{self as idol, HubrisIdol};

#[derive(Parser, Debug)]
#[clap(
    name = "apml21", about = env!("CARGO_PKG_DESCRIPTION"),
    group = ArgGroup::new("command").multiple(false).required(false)
)]
struct ApmlArgs {
    /// sets timeout
    #[clap(
        long, short = 'T', default_value_t = 5000, value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// call a particular function
    #[clap(long, short)]
    call: Option<String>,
}

fn apml(context: &mut ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let subargs = ApmlArgs::try_parse_from(subargs)?;
    let hubris = context.archive.as_ref().unwrap();
    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;
    let cmd = apml_rs::SbRmi21MailboxCmd::PwrCurrentActiveFreqLimSocket;
    let mut buf = vec![];
    buf.resize(32, 0u8);
    hubpack::serialize(&mut buf, &cmd)?;
    let op = hubris.get_idol_command("SbRmi21.mailbox")?;
    let payload = op.payload(&[("cmd", idol::IdolArgument::Array(buf))])?;
    let mut ops = vec![];
    context.idol_call_ops(&op, &payload, &mut ops)?;
    ops.push(Op::Done);
    let result = context.run(core, ops.as_slice(), None)?;
    println!("result = {result:?}");
    Ok(())
}

pub fn init() -> Command {
    Command {
        app: ApmlArgs::command(),
        name: "apml21",
        run: apml,
        kind: CommandKind::Attached {
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
        },
    }
}
