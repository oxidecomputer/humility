// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility host`
//! `humility host` pretty-prints host state, which is sent to the SP over IPCC.
//!
//! It is only functional on a Gimlet SP image.
//!
//! ### `humility host last-panic`
//! Pretty prints the value of `LAST_HOST_PANIC`
//! ```console
//! humility: attached to dump
//! humility: reading LAST_HOST_PANIC
//! humility: fixing up trimmed initial data
//! ipd_cause:   IPCC_PANIC_CALL
//! ipd_error:   0
//! ipd_cpuid:   58
//! ipd_thread:  0xfffff78811079c20
//! ipd_addr:    0x0
//! ipd_pc:      0xfffffffff7ee48b8
//! ipd_fp:      0xfffff78811079a50
//! ipd_rp:      0x0
//! ipd_message: I/O to pool 'oxp_410e4dfb-b4d1-4d73-8c39-077bf436da3a' appears to be hung.
//! ipd_stackid: 16
//! stack trace:
//!   vdev_deadman+0x108          (0xfffffffff7ee48b8)
//!   vdev_deadman+0x43           (0xfffffffff7ee47f3)
//!   spa_deadman+0x84            (0xfffffffff7ed54d4)
//!   cyclic_softint+0xe1         (0xfffffffffc03e061)
//!   cbe_low_level+0x20          (0xfffffffffbc0c3f0)
//!   av_dispatch_softvect+0x72   (0xfffffffffbcc9d12)
//!   apix_dispatch_softint+0x35  (0xfffffffff7c92545)
//!   switch_sp_and_call+0x15     (0xfffffffffbc818c5)
//!   apix_do_softint+0x5a        (0xfffffffff7c925ba)
//!   apix_do_interrupt+0x2bf     (0xfffffffff7c9306f)
//!   _interrupt+0xc3             (0xfffffffffbc00233)
//!   i86_mwait+0x12              (0xfffffffffbc81042)
//!   cpu_idle_mwait+0x14b        (0xfffffffffbc5131b)
//!   cpu_idle_adaptive+0x19      (0xfffffffffbc50f39)
//!   idle+0xa8                   (0xfffffffffbc96c88)
//!   thread_start+0xb            (0xfffffffffbc838bb)
//! ipd_dataidx: 0
//! ipd_data:    [0; 256]
//! ```
//!
//! ### `humility host boot-fail`
//! Pretty-prints the contents of `LAST_HOST_BOOT_FAIL`
//! ```console
//! $ humility host boot-fail
//! humility: attached to dump
//! humility: reading LAST_HOST_BOOT_FAIL
//! [0; 4096]
//! ```

use anyhow::{anyhow, bail, Context, Result};
use clap::{CommandFactory, Parser};
use zerocopy::FromBytes;

use humility::{core::Core, hubris::HubrisArchive, reflect, reflect::Load};
use humility_cli::{ExecutionContext, Subcommand};
use humility_cmd::{Archive, Attach, Command, CommandKind, Validate};
use humility_doppel as doppel;
use humility_log::{msg, warn};

#[derive(Parser, Debug)]
enum HostCommand {
    /// Print the boot failure
    BootFail,
    /// Print the last host panic
    LastPanic,
}

#[derive(Parser, Debug)]
#[clap(name = "host", about = env!("CARGO_PKG_DESCRIPTION"))]
struct HostArgs {
    #[clap(subcommand)]
    cmd: HostCommand,
}

static SEPARATE_HOST_BOOT_FAIL_NAME: &str = "LAST_HOST_BOOT_FAIL";

static SEPARATE_LAST_HOST_PANIC_NAME: &str = "LAST_HOST_PANIC";

static HOST_STATE_BUF_NAME: &str =
    "task_host_sp_comms::ServerImpl::claim_static_resources::BUFS";

/// Mirror type of the internal buf struct in `host_sp_comms`. Must be kept in
/// (partial) sync with that structure (fields that are present need to match,
/// other fields can be ignored).
#[derive(Load)]
struct HostStateBuf {
    last_boot_fail: Vec<u8>,
    last_panic: Vec<u8>,
}

fn read_uqvar(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    name: &str,
) -> Result<Option<Vec<u8>>> {
    let Some(var) = hubris.lookup_variable(name).ok() else {
        return Ok(None);
    };

    let mut buf: Vec<u8> = vec![0u8; var.size];

    core.halt()?;
    core.read_8(var.addr, buf.as_mut_slice())?;
    core.run()?;

    Ok(Some(buf))
}

fn read_qualified_state_buf(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    name: &str,
) -> Result<Option<HostStateBuf>> {
    let Some(var) = hubris.lookup_qualified_variable(name).ok() else {
        return Ok(None);
    };

    let var_ty = hubris.lookup_type(var.goff)?;

    let mut buf: Vec<u8> = vec![0u8; var.size];

    core.halt()?;
    core.read_8(var.addr, &mut buf)?;
    core.run()?;

    let v = reflect::load_value(hubris, &buf, var_ty, 0)?;
    let as_static_cell = doppel::ClaimOnceCell::from_value(&v)?;
    Ok(Some(HostStateBuf::from_value(&as_static_cell.cell.value)?))
}

fn print_escaped_ascii(mut bytes: &[u8]) {
    // Drop trailing NULs to avoid escaping them and cluttering up our output.
    while let Some((&b'\0', prefix)) = bytes.split_last() {
        bytes = prefix;
    }

    if bytes.is_empty() {
        println!("Message contains no non-NUL bytes, not printing.");
        return;
    } else {
        println!("Message is {} bytes long:", bytes.len());
    }

    let mut buf = String::new();
    for &b in bytes {
        match b {
            b'\\' => {
                // Escape any backslashes in the original, so that any escapes
                // _we_ emit are unambiguous.
                buf.push_str("\\\\");
            }
            b'\n' | b'\r' | b'\t' | 0x20..=0x7E => {
                // Pass through basic text characters that we expect we might
                // see in a message.
                buf.push(b as char);
            }
            _ => {
                // Escape any other non-printable characters.
                buf.push_str("\\x");
                buf.push_str(&format!("{b:02X}"));
            }
        }
    }
    println!("{buf}");
}

fn host_boot_fail(hubris: &HubrisArchive, core: &mut dyn Core) -> Result<()> {
    // Try old name:
    let d = read_uqvar(hubris, core, SEPARATE_HOST_BOOT_FAIL_NAME)?;
    if let Some(d) = d {
        print_escaped_ascii(&d);
        return Ok(());
    }
    // Try new name
    let buf = read_qualified_state_buf(hubris, core, HOST_STATE_BUF_NAME)?
        .ok_or_else(|| {
            anyhow!(
                "Could not find host boot variables under any known name; \
            is this a Gimlet image?"
            )
        })?;

    print_escaped_ascii(&buf.last_boot_fail[..]);

    Ok(())
}

// Values and structs are defined in usr/src/uts/oxide/sys/kernel_ipcc.h
// in the `stlouis` branch of `oxidecomputer/illumos-gate`
const IPCC_PANIC_STACKS: usize = 0x10;
const IPCC_PANIC_DATALEN: usize = 0x100;
const IPCC_PANIC_SYMLEN: usize = 0x20;
const IPCC_PANIC_MSGLEN: usize = 0x80;

#[derive(Copy, Clone, Debug, FromBytes)]
#[repr(packed)]
struct IpccPanicData {
    ipd_version: u8,
    ipd_cause: u16,
    ipd_error: u32,

    ipd_cpuid: u32,
    ipd_thread: u64,
    ipd_addr: u64,
    ipd_pc: u64,
    ipd_fp: u64,
    ipd_rp: u64,

    ipd_message: [u8; IPCC_PANIC_MSGLEN],

    ipd_stackidx: u8,
    ipd_stack: [IpccPanicStack; IPCC_PANIC_STACKS],

    ipd_dataidx: u8,
    ipd_data: [u8; IPCC_PANIC_DATALEN],
}

#[derive(Copy, Clone, Debug, FromBytes)]
#[repr(packed)]
struct IpccPanicStack {
    ips_symbol: [u8; IPCC_PANIC_SYMLEN],
    ips_addr: u64,
    ips_offset: u64,
}

fn host_last_panic(hubris: &HubrisArchive, core: &mut dyn Core) -> Result<()> {
    // Try original name:
    let d = read_uqvar(hubris, core, SEPARATE_LAST_HOST_PANIC_NAME)?;
    if let Some(d) = d {
        return print_panic(d);
    }

    // Try new name:
    let buf = read_qualified_state_buf(hubris, core, HOST_STATE_BUF_NAME)?
        .ok_or_else(|| {
            anyhow!(
                "Could not find host boot variables under any known name; \
            is this a Gimlet image?"
            )
        })?;

    print_panic(buf.last_panic)
}

fn print_panic(d: Vec<u8>) -> Result<()> {
    // Fix for https://github.com/oxidecomputer/hubris/issues/1554
    //
    // In some cases, `ipd_cause` is unambiguous based on the first byte;
    // otherwise, we populate a generic value.
    let bonus_byte = match d[0] {
        0x01 => None,
        0xca => Some(0x11),
        0x5e => Some(0x00),
        0xa9 => Some(0x00),
        0xeb => Some(0xff), // can't distinguish between different 0xeb**
        b => {
            warn!("could not decode `ipd_cause`: {b:#04x}");
            Some(0xff)
        }
    };
    let d = if let Some(b) = bonus_byte {
        msg!("fixing up trimmed initial data");
        let mut next = vec![0x01, b];
        next.extend(d);
        next
    } else {
        d
    };

    let p = IpccPanicData::read_from_prefix(d.as_ref())
        .context("failed to deserialize panic data")?;
    if p.ipd_version != 0x01 {
        bail!("unknown `ipd_version` ({})", p.ipd_version);
    }
    println!(
        "ipd_cause:   {}",
        match p.ipd_cause {
            0xca11 => "IPCC_PANIC_CALL".to_owned(),
            0xa900 => "IPCC_PANIC_TRAP".to_owned(),
            0x5e00 => "IPCC_PANIC_USERTRAP".to_owned(),
            0xeb00 => "IPCC_PANIC_EARLYBOOT".to_owned(),
            0xeb97 => "IPCC_PANIC_EARLYBOOT_PROM".to_owned(),
            0xeba9 => "IPCC_PANIC_EARLYBOOT_TRAP".to_owned(),
            0xebff => "IPCC_PANIC_EARLYBOOT_*".to_owned(),
            b => format!("Unknown `ipd_cause` {b:#06x}"),
        }
    );

    let ipd_error = p.ipd_error;
    let ipd_cpuid = p.ipd_cpuid;
    let ipd_thread = p.ipd_thread;
    let ipd_addr = p.ipd_addr;
    let ipd_pc = p.ipd_pc;
    let ipd_fp = p.ipd_fp;
    let ipd_rp = p.ipd_rp;

    println!("ipd_error:   {ipd_error}");
    println!("ipd_cpuid:   {ipd_cpuid}");
    println!("ipd_thread:  {ipd_thread:#x}");
    println!("ipd_addr:    {ipd_addr:#x}");
    println!("ipd_pc:      {ipd_pc:#x}");
    println!("ipd_fp:      {ipd_fp:#x}");
    println!("ipd_rp:      {ipd_rp:#x}");

    match std::str::from_utf8(&p.ipd_message) {
        Ok(s) => println!("ipd_message: {}", s.trim_matches('\0')),
        Err(e) => println!(
            "ipd_message: {:?}\n  (could not decode: {e})",
            p.ipd_message
        ),
    }
    println!("ipd_stackid: {}", p.ipd_stackidx);
    println!("stack trace:");
    let syms: Vec<String> = p
        .ipd_stack
        .iter()
        .map(|s| {
            let sym = match std::str::from_utf8(&s.ips_symbol) {
                Ok(s) => s.to_owned(),
                Err(e) => format!("<could not decode {:?}: {e}>", s.ips_symbol),
            };
            let offset = s.ips_offset;
            format!("{}+{offset:#x}", sym.trim_matches('\0'))
        })
        .collect();
    let width = syms.iter().map(|s| s.len()).max().unwrap_or(0);
    for (sym, s) in syms.iter().zip(p.ipd_stack.iter()) {
        let addr = s.ips_addr;
        println!("  {sym:width$}  ({addr:#016x})");
    }

    println!("ipd_dataidx: {}", p.ipd_dataidx);
    print!("ipd_data:    ");
    if p.ipd_data.iter().all(|&c| c == 0) {
        println!("[0; {}]", p.ipd_data.len());
    } else {
        match std::str::from_utf8(&p.ipd_data) {
            Ok(s) => println!("{}", s.trim_matches('\0')),
            Err(_e) => println!("{:?}", p.ipd_data),
        }
    }

    Ok(())
}

fn host(context: &mut ExecutionContext) -> Result<()> {
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let subargs = HostArgs::try_parse_from(subargs)?;
    let hubris = context.archive.as_ref().unwrap();
    let core = &mut **context.core.as_mut().unwrap();

    match subargs.cmd {
        HostCommand::BootFail => host_boot_fail(hubris, core),
        HostCommand::LastPanic => host_last_panic(hubris, core),
    }
}

pub fn init() -> Command {
    Command {
        app: HostArgs::command(),
        name: "host",
        run: host,
        kind: CommandKind::Attached {
            archive: Archive::Required,
            attach: Attach::Any,
            validate: Validate::Match,
        },
    }
}
