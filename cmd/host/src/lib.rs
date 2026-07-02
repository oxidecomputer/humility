// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility host`
//! `humility host` pretty-prints host state, which is sent to the SP over IPCC.
//!
//! It is only functional on a Gimlet or Cosmo SP image.
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
//!
//! ### `humility host cosmo last-post-code`
//! Pretty-prints the last POST code seen by the sequencer FPGA (Cosmo only)
//! ```console
//! $ humility host cosmo last-post-code
//! humility: attached to 0483:374f:002A001C4D46500F20373033 via ST-Link V3
//! Bootloader Code: 0xed80000f
//!   Source:  ASP TEE
//!   Status:  BL_ERR_BOUNDARY_CHECK (0x0f)
//!   Detail:  Out of Boundary Condition Reached
//! ```
//!
//! ### `humility host cosmo post-codes`
//! Pretty-prints all last POST codes seen by the sequencer FPGA (Cosmo only)
//! ```console
//! $ humility host cosmo post-codes
//! humility: attached to 0483:374f:002A001C4D46500F20373033 via ST-Link V3
//! Bootloader Code: 0xee1000b3
//!   Source:  ASP BL2
//!   Status:  BL_SUCCESS_BYPASS_IDEVID_CHECK (0xb3)
//!   Detail:  IDEVID validation failed but bypassed (unsecure)
//! Bootloader Code: 0xee1000a0
//!   Source:  ASP BL2
//!   Status:  BL_SUCCESS_C_MAIN (0xa0)
//!   Detail:  Successfully entered C Main
//! Bootloader Code: 0xee1000a3
//!   Source:  ASP BL2
//!   Status:  BL_SUCCESS_DETECT_BOOT_MODE (0xa3)
//!   Detail:  Boot Mode detected and sent to slaves
//! Bootloader Code: 0xee1000a2
//!   Source:  ASP BL2
//!   Status:  BL_SUCCESS_DERIVE_HMAC_KEY (0xa2)
//!   Detail:  HMAC key derived
//! Bootloader Code: 0xee1000a2
//!   Source:  ASP BL2
//!   Status:  BL_SUCCESS_DERIVE_HMAC_KEY (0xa2)
//!   Detail:  HMAC key derived
//! # etc...
//! ```

use anyhow::{Result, bail};
use chrono::DateTime;
use clap::Parser;

use humility::{
    core::Core,
    hubris::HubrisArchive,
    log::{Logger, info, warn},
    reflect::{self, Load, Value},
};
use humility_cli::{ExecutionContext, humility_cmd};
use humility_doppel as doppel;
use humility_hiffy::HiffyContext;
use humility_idol::HubrisIdol;

#[derive(Parser, Debug)]
enum HostCommand {
    /// Print the boot failure
    BootFail,
    /// Print the last host panic
    LastPanic,
    /// Cosmo-specific host commands
    Cosmo {
        #[clap(subcommand)]
        cmd: CosmoHostCommand,
    },
}

#[derive(Parser, Debug)]
enum CosmoHostCommand {
    /// Prints the most recent POST code
    LastPostCode {
        #[clap(long)]
        raw: bool,
    },
    /// Dumps the POST code buffer
    PostCodes {
        #[clap(long)]
        raw: bool,
    },
}

#[derive(Parser, Debug)]
#[clap(name = "host", about = env!("CARGO_PKG_DESCRIPTION"))]
pub struct HostArgs {
    #[clap(subcommand)]
    cmd: HostCommand,
}

static SEPARATE_HOST_BOOT_FAIL_NAME: &str = "LAST_HOST_BOOT_FAIL";

static SEPARATE_LAST_HOST_PANIC_NAME: &str = "LAST_HOST_PANIC";

// Pre-2544
static HOST_STATE_BUF_NAME_1: &str =
    "task_host_sp_comms::ServerImpl::claim_static_resources::BUFS";

// Post-2544
static HOST_STATE_BUF_NAME_2: &str =
    "<task_host_sp_comms::ServerImpl>::claim_static_resources::BUFS";

static PACKRAT_BUF_NAME: &str = "task_packrat::main::BUFS";

// packrat field names
const PACKRAT_LAST_PANIC_PAYLOAD: &str =
    "cell.value.host_info.host_panic_payload";
const PACKRAT_LAST_PANIC_STATE: &str = "cell.value.host_info.host_panic_state";
const PACKRAT_BOOT_FAIL_PAYLOAD: &str =
    "cell.value.host_info.host_panic_payload";
const PACKRAT_BOOT_FAIL_STATE: &str = "cell.value.host_info.host_panic_state";

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
    let buf: doppel::MaybeUninit<Vec<u8>> =
        reflect::read_variable(hubris, core, var)?;
    Ok(Some(buf.value))
}

fn read_qualified_state_buf(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    name: &str,
) -> Result<Option<HostStateBuf>> {
    let Some(var) = hubris.lookup_qualified_variable(name).ok() else {
        return Ok(None);
    };

    let as_static_cell: doppel::ClaimOnceCell<HostStateBuf> =
        reflect::read_variable(hubris, core, var)?;
    Ok(Some(as_static_cell.cell.value))
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

/// Try getting last panic/boot fail from packrat, where it moved to in
/// https://github.com/oxidecomputer/hubris/pull/2518.
fn host_bootinfo_packrat(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    payload: &str,
    state: &str,
) -> Result<Option<Vec<u8>>> {
    // If this variable doesn't exist, we're probably on a REALLY old version
    // of hubris, but don't return the error here, as it means we'll still want
    // to check the host-sp-comms's vars.
    let lookup = hubris.lookup_qualified_variable(PACKRAT_BUF_NAME);
    let Ok(buf_ty) = lookup else {
        return Ok(None);
    };

    // We do ? the error here, because errors while loading indicate some
    // kind of transport error.
    let buf: Value = humility::reflect::read_variable(hubris, core, buf_ty)?;

    // Again, it's possible the image DOES have the packrat buf, but NOT this
    // field (either it is older than #2518, or it is a hostless SP), so treat
    // errors here as "no data".
    let res_payload: Result<Vec<u8>> = buf.field(payload);
    let res_state: Result<Option<Value>> = buf.field(state);

    let (Ok(mut payload), Ok(state)) = (res_payload, res_state) else {
        return Ok(None);
    };

    // Cool, cool, we have the fields! Now check if the state is in a place
    // where there is something reasonable to extract. If the variables DO
    // exist, but DON'T have data, return an empty vec.
    let Some(state) = state else {
        return Ok(Some(vec![]));
    };

    let total: u32 = state.field("total_length")?;
    payload.truncate(total as usize);

    Ok(Some(payload))
}

fn host_boot_fail_spcomms_old(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
) -> Result<Option<Vec<u8>>> {
    read_uqvar(hubris, core, SEPARATE_HOST_BOOT_FAIL_NAME)
}

fn host_boot_fail_spcomms_new(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    base_buf: &str,
) -> Result<Option<Vec<u8>>> {
    let buf = read_qualified_state_buf(hubris, core, base_buf)?;
    let maybe_bf = buf.map(|b| b.last_boot_fail);
    Ok(maybe_bf)
}

/// Try getting boot fail from packrat, where it moved to in
/// https://github.com/oxidecomputer/hubris/pull/2518.
fn host_boot_fail(hubris: &HubrisArchive, core: &mut dyn Core) -> Result<()> {
    // Work through the different places "boot fail" info could be hiding
    let sources: [fn(&HubrisArchive, &mut dyn Core) -> _; _] = [
        |h, c| {
            host_bootinfo_packrat(
                h,
                c,
                PACKRAT_BOOT_FAIL_PAYLOAD,
                PACKRAT_BOOT_FAIL_STATE,
            )
        },
        host_boot_fail_spcomms_old,
        |h, c| host_boot_fail_spcomms_new(h, c, HOST_STATE_BUF_NAME_1),
        |h, c| host_boot_fail_spcomms_new(h, c, HOST_STATE_BUF_NAME_2),
    ];

    for source in sources {
        if let Some(bootfail) = source(hubris, core)? {
            print_escaped_ascii(&bootfail);
            return Ok(());
        }
    }

    bail!(
        "Could not find host boot variables under any known name; is this a \
        Gimlet image?"
    )
}

/// In host-sp-comms, with the legacy-ish name
fn host_last_panic_spcomms_old(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
) -> Result<Option<Vec<u8>>> {
    read_uqvar(hubris, core, SEPARATE_LAST_HOST_PANIC_NAME)
}

/// In host-sp-comms, with the modern-ish name
fn host_last_panic_spcomms_new(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    base_buf: &str,
) -> Result<Option<Vec<u8>>> {
    // Try new name:
    let buf = read_qualified_state_buf(hubris, core, base_buf)?;
    let maybe_panic = buf.map(|b| b.last_panic);
    Ok(maybe_panic)
}

fn host_last_panic(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    log: &Logger,
) -> Result<()> {
    // Work through the different places "last panic" info could be hiding
    let sources: [fn(&HubrisArchive, &mut dyn Core) -> _; _] = [
        |h, c| {
            host_bootinfo_packrat(
                h,
                c,
                PACKRAT_LAST_PANIC_PAYLOAD,
                PACKRAT_LAST_PANIC_STATE,
            )
        },
        host_last_panic_spcomms_old,
        |h, c| host_last_panic_spcomms_new(h, c, HOST_STATE_BUF_NAME_1),
        |h, c| host_last_panic_spcomms_new(h, c, HOST_STATE_BUF_NAME_2),
    ];

    for source in sources {
        if let Some(panic) = source(hubris, core)? {
            return print_panic(panic, log);
        }
    }

    bail!(
        "Could not find host boot variables under any known name; is this a \
        Gimlet image?"
    )
}

fn print_panic(d: Vec<u8>, log: &Logger) -> Result<()> {
    let data = match ipcc_data::PanicData::from_bytes(d)? {
        Some(data) => data,
        None => {
            info!(log, "panic information is empty");
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

/// Print a warning message if the archive is not for a `cosmo` board
fn check_post_code_target(hubris: &HubrisArchive, log: &Logger) {
    if !hubris.manifest.board.contains("cosmo") {
        warn!(
            log,
            "POST code buffer is only present on 'cosmo' hardware \
             but this is a '{}'; hiffy may fail and time out",
            hubris.manifest.board,
        )
    }
}

fn host_post_codes(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    raw: bool,
    log: &Logger,
) -> Result<()> {
    check_post_code_target(hubris, log);
    use hif::*;

    let mut context = HiffyContext::new(
        hubris,
        core,
        std::time::Duration::from_millis(5000),
        log,
    )?;
    let op = hubris.get_idol_command("Sequencer.post_code_buffer_len")?;
    let count = context.call::<u32>(core, &op, &[], None, None)?;

    let op = hubris.get_idol_command("Sequencer.get_post_code")?;
    let handle_value = |v| {
        if raw {
            println!("{v:08x}");
        } else {
            let decoded = turin_post_decoder::decode(v);
            let detail = decoded.lines().join("\n");
            println!("{detail}");
        }
    };

    let send = context.get_function("Send", 4)?;
    let ret_size = hubris.typesize(op.ok)? as u32;
    assert_eq!(ret_size, 4);

    // Each returned value is a FunctionResult::Success(&[..]), which
    // encodes as 6 bytes: 1 byte variant tag, 1 byte slice length, 4 bytes
    // of u32.
    let max_chunk_size = context.rstack_size() / 6;

    // Write a little program to read all of the post codes
    for start in (0..count).step_by(max_chunk_size) {
        let end = (start + max_chunk_size as u32).min(count);
        let label = Target(0);
        let mut ops = vec![
            Op::Push32(op.task.task()), // task id
            Op::Push16(op.code),        // opcode
            Op::Push32(start),          // Starting index
            Op::Push32(0),              // Comparison target (dummy)
            Op::Label(label),           // loop start
        ];
        {
            // block representing the hiffy loop
            ops.push(Op::Drop); // Drop comparison target

            // Expand u32 -> [u8; 4], since that's expected by `send`
            ops.push(Op::Expand32);
            ops.push(Op::Push(4)); // Payload size
            ops.push(Op::Push32(ret_size)); // Return size
            ops.push(Op::Call(send.id));
            ops.push(Op::DropN(2)); // Drop payload and return size
            ops.push(Op::Collect32);
            ops.push(Op::Push(1)); // Increment by four
            ops.push(Op::Add); // index += 1
            ops.push(Op::Push32(end)); // Comparison target
            ops.push(Op::BranchGreaterThan(label)); // Jump to loop start
        }
        ops.push(Op::DropN(4)); // Cleanup
        ops.push(Op::Done); // Finish

        for r in context.run(core, ops.as_slice(), None)? {
            let v = op.decode(&r)?;
            handle_value(v);
        }
    }
    Ok(())
}

fn host_last_post_code(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    raw: bool,
    log: &Logger,
) -> Result<()> {
    check_post_code_target(hubris, log);

    let mut context = HiffyContext::new(
        hubris,
        core,
        std::time::Duration::from_millis(5000),
        log,
    )?;
    let op = hubris.get_idol_command("Sequencer.last_post_code")?;
    let v = context.call::<u32>(core, &op, &[], None, None)?;
    if raw {
        println!("{v:08x}");
    } else {
        let decoded = turin_post_decoder::decode(v);
        let detail = decoded.lines().join("\n");
        println!("{detail}");
    }
    Ok(())
}

fn host(subargs: HostArgs, context: &mut ExecutionContext) -> Result<()> {
    let hubris = &context.cli.archive()?;
    let log = context.log();

    match subargs.cmd {
        // BootFail and LastPanic just need to read memory, so they can attach
        // to either a live system or a dump.
        HostCommand::BootFail => {
            let core = &mut *context.cli.attach_live_or_dump_match(hubris)?;
            host_boot_fail(hubris, core)
        }
        HostCommand::LastPanic => {
            let core = &mut *context.cli.attach_live_or_dump_match(hubris)?;
            host_last_panic(hubris, core, log)
        }
        HostCommand::Cosmo { cmd } => {
            // All cosmo subcommands require making hiffy calls on a live system
            let core = &mut *context.cli.attach_live_booted(hubris)?;
            match cmd {
                CosmoHostCommand::PostCodes { raw } => {
                    host_post_codes(hubris, core, raw, log)
                }
                CosmoHostCommand::LastPostCode { raw } => {
                    host_last_post_code(hubris, core, raw, log)
                }
            }
        }
    }
}

humility_cmd!(HostArgs, host);
