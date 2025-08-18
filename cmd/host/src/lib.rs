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
//! % humility: attached to dump
//! version:   2 (inferred)
//! cause:     IPCC_PANIC_TRAP
//! error:     0
//! cpuid:     0
//! thread:    0xfffff78809c74c20
//! time:      74.639002555 (1970-01-01T00:01:14.639002555+00:00)
//! hrtime:    74639011110
//! addr:      0xfffffcf853a0f940
//! pc:        0xfffffffffbc5fb45
//! fp:        0xfffff78809c748f0
//! rp:        0xfffff78809c749e0
//! registers:
//!            rdi 0xfffffffffc029d70 rsi 0xfffff78809c749d8 rdx 0xfffffffffc029d70
//!            rcx                0x0 r8  0xfffffcf931453280 r9                 0x6
//!            rax                0x0 rbx 0xfffffcf931453280 rbp 0xfffff78809c74b60
//!            r10                0x1 r11 0xfffffffff7c9c510 r12 0xfffffcf931da7210
//!            r13 0xfffffcf931453370 r14                0x0 fsb 0xfffffffffc2604e8
//!            gsb 0xfffff78809c74a90 es                 0x0 fs                 0x0
//!            gs                 0x0 tra                0x6 err                0x0
//!            rip 0xfffffffffc029d70 cs                0x30 rfl            0x10246
//!            rsp 0xfffff78809c74ad8 ss                0x38
//! message:   BAD TRAP: type=6 (#ud Invalid opcode) rp=fffff78809c749e0 addr=fffffcf853a0f940
//! stack:
//!            die+0x105                                (0xfffffffffbc5fb45)
//!            trap+0x855                               (0xfffffffffbc606f5)
//!            cmntrap+0xe9                             (0xfffffffffbc49819)
//!            clock+0x0                                (0xfffffffffc029d70)
//!            cbe_softclock+0x23                       (0xfffffffffbc0c8b3)
//!            av_dispatch_softvect+0x72                (0xfffffffffbccbf02)
//!            apix_dispatch_softint+0x35               (0xfffffffff7c9c545)
//!            switch_sp_and_call+0x15                  (0xfffffffffbc834b5)
//!            apix_do_softint+0x5a                     (0xfffffffff7c9c5ba)
//!            apix_do_interrupt+0x2bf                  (0xfffffffff7c9d06f)
//!            cmnint+0xc3                              (0xfffffffffbc00233)
//!            taskq_bucket_extend+0x15b                (0xfffffffffc17a66b)
//!            taskq_create_common+0x33a                (0xfffffffffc17a19a)
//!            system_taskq_init+0x4e                   (0xfffffffffc1788de)
//!            main+0xbb                                (0xfffffffffc0c50bb)
//!            _locore_start+0x88                       (0xfffffffffbc49708)
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

use anyhow::{Result, anyhow};
use chrono::DateTime;
use clap::{CommandFactory, Parser};

use humility::{core::Core, hubris::HubrisArchive, reflect, reflect::Load};
use humility_cli::{ExecutionContext, Subcommand};
use humility_cmd::{Archive, Attach, Command, CommandKind, Validate};
use humility_doppel as doppel;
use humility_log::msg;

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
    let data = match ipcc_data::PanicData::from_bytes(d)? {
        Some(data) => data,
        None => {
            msg!("panic information is empty");
            return Ok(());
        }
    };

    println!("version:   {}", data.version);
    println!("cause:     {}", data.cause);
    println!("error:     {}", data.error_code);
    println!("cpuid:     {}", data.cpuid);
    println!("thread:    {:#x}", data.thread);

    if let Some(time) = data.time {
        println!(
            "time:      {}.{:09} ({})",
            time.sec,
            time.nsec,
            match DateTime::from_timestamp(time.sec as i64, time.nsec) {
                Some(dt) => dt.to_rfc3339(),
                None => format!("invalid timestamp {time:?}"),
            },
        );
    }

    if let Some(ipcc_data::MonotonicNanoseconds(hrtime)) = data.hrtime {
        let s = hrtime / 1_000_000_000;
        let ns = hrtime % 1_000_000_000;
        println!("hrtime:    {s}.{ns:09}");
    }

    println!("addr:      {:#x}", data.addr);
    println!("pc:        {:#x}", data.pc);
    println!("fp:        {:#x}", data.fp);
    println!("rp:        {:#x}", data.rp);

    if let Some(registers) = data.registers {
        let registers = registers.iter().collect::<Vec<_>>();

        println!("registers:");

        for chunk in registers.chunks(3) {
            print!("          ");

            for (r, v) in chunk {
                let label: String = format!("{r}").chars().take(3).collect();
                print!(" {label:3} {v:#18x}");
            }
            println!();
        }
    }

    println!(
        "message:   {}",
        data.message.unwrap_or_else(|| "<none>".to_string())
    );
    println!("stack:");

    for f in data.stack {
        println!("           {:<48} ({:#016x})", f, f.address);
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
