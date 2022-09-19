// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility probe`
//!
//! `humility probe` attempts to infer as much about the hardware state as it
//! can, e.g.:
//!
//! ```console
//! % humility probe
//! humility: attached via ST-Link
//! humility:        probe => STLink V3, VID 0483, PID 374e
//! humility: probe serial => 003700303137511139383538
//! humility:         core => Cortex-M7
//! humility: manufacturer => STMicroelectronics
//! humility:         chip => STM32H7, revision 0x2003
//! humility:       status => executing
//! humility:  debug units => CSTF(x2) CTI(x2) DWT ETM FPB ITM SCS SWO TMC TPIU
//! humility:         CSTF => 0x5c004000, 0x5c013000
//! humility:          CTI => 0x5c011000, 0xe0043000
//! humility:          DWT => 0xe0001000
//! humility:          ETM => 0xe0041000
//! humility:          FPB => 0xe0002000
//! humility:          ITM => 0xe0000000
//! humility:          SCS => 0xe000e000
//! humility:          SWO => 0x5c003000
//! humility:          TMC => 0x5c014000
//! humility:         TPIU => 0x5c015000
//! humility:   ITM status => TRCENA enabled, TCR disabled, TER=0x0
//! humility:           R0 => 0x20006000
//! humility:           R1 => 0x20006000
//! humility:           R2 => 0x0
//! humility:           R3 => 0x0
//! humility:           R4 => 0x0
//! humility:           R5 => 0x0
//! humility:           R6 => 0x0
//! humility:           R7 => 0x0
//! humility:           R8 => 0x0
//! humility:           R9 => 0x0
//! humility:          R10 => 0x0
//! humility:          R11 => 0x0
//! humility:          R12 => 0x0
//! humility:           SP => 0x20006100
//! humility:           LR => 0x802404f
//! humility:           PC => 0x8024052
//! humility:         xPSR => 0x61000000
//! humility:          MSP => 0x20000f48
//! humility:          PSP => 0x20006100
//! humility:          SPR => 0x7000000
//! ```
//!
//! If provided a Hubris archive, `humility probe` will display any register
//! contents symbolically, e.g.:
//!
//! ```console
//! % humility -a ~/hubris/target/demo/dist/build-demo.zip probe
//! humility: attached via ST-Link
//! humility:        probe => STLink V2-1, VID 0483, PID 374b
//! humility: probe serial => 066DFF383032534E43132614
//! humility:         core => Cortex-M4
//! humility: manufacturer => STMicroelectronics
//! humility:         chip => STM32F40x/STM32F41x, revision 0x1007
//! humility:  debug units => DWT ETM FPB ITM SCS TPIU
//! humility:       status => executing
//! humility:          ITM => TRCENA enabled, TCR enabled, TER=0x3
//! humility:           R0 => 0x0
//! humility:           R1 => 0x0
//! humility:           R2 => 0x1
//! humility:           R3 => 0x20001bd4
//! humility:           R4 => 0x20001bd4
//! humility:           R5 => 0x801d988
//! humility:           R6 => 0xb004
//! humility:           R7 => 0x20001bf0
//! humility:           R8 => 0x40004400
//! humility:           R9 => 0x1
//! humility:          R10 => 0x0
//! humility:          R11 => 0xffff
//! humility:          R12 => 0x0
//! humility:           SP => 0x20001ba8
//! humility:           LR => 0x801c12b   <- main+0xef
//! humility:           PC => 0x801d290   <- sys_recv_stub+0x1e
//! humility:         xPSR => 0x61000000
//! humility:          MSP => 0x20000f48
//! humility:          PSP => 0x20001ba8
//! humility:          SPR => 0x7000000
//! ```

use anyhow::Result;
use clap::{CommandFactory, Parser};
use humility::arch::ARMRegister;
use humility::hubris::HubrisValidate;
use humility_cmd::CommandKind;
use humility_cmd::{Archive, Attach, Command, Validate};
use humility_cortex::debug::*;
use humility_cortex::itm::*;
use humility_cortex::scs::*;

#[derive(Parser, Debug)]
#[clap(name = "probe", about = env!("CARGO_PKG_DESCRIPTION"))]
struct ProbeArgs {}

#[rustfmt::skip::macros(format)]
fn probecmd(context: &mut humility::ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let hubris = context.archive.as_ref().unwrap();

    use num_traits::FromPrimitive;
    let mut status = vec![];

    let print = |what, val| {
        humility::msg!("{:>12} => {}", what, val);
    };

    let mut statusif = |val, str: &str| {
        if val {
            status.push(str.to_string());
        }
    };

    let coreinfo = CoreInfo::read(core)?;
    let part = coreinfo.part;

    let dhcsr = DHCSR::read(core)?;
    let dfsr = DFSR::read(core)?;

    let info = core.info();
    print("probe", info.0);
    print(
        "probe serial",
        match info.1 {
            Some(ref serial) => serial.to_string(),
            None => "-".to_string(),
        },
    );

    //
    // Start with information about our core and chip...
    //
    print("core", corename(part));

    let m = &coreinfo.manufacturer;

    print(
        "manufacturer",
        if let Some(manufacturer) = m.get() {
            manufacturer.to_string()
        } else {
            format!("<JEP106 [0x{:x}, 0x{:x}]>", m.cc, m.id)
        },
    );

    print(
        "chip",
        if coreinfo.vendor == Vendor::ARM && part == ARMCore::CortexM0Plus {
            if let Ok(idc) = STM32G0X1_DBGMCU_IDCODE::read(core) {
                format!(
                    "{}, revision 0x{:x}",
                    stm32_chipname(idc.dev_id()),
                    idc.rev_id()
                )
            } else {
                format!("<unknown ARM part 0x{:x}>", coreinfo.manufacturer_part)
            }
        } else if coreinfo.vendor == Vendor::ST && part == ARMCore::CortexM4 {
            if let Ok(idc) = STM32F4_DBGMCU_IDCODE::read(core) {
                format!(
                    "{}, revision 0x{:x}",
                    stm32_chipname(idc.dev_id()),
                    idc.rev_id()
                )
            } else {
                format!("<unknown ST part 0x{:x}>", coreinfo.manufacturer_part)
            }
        } else if coreinfo.vendor == Vendor::ST && part == ARMCore::CortexM7 {
            if let Ok(idc) = STM32H7_DBGMCU_IDC::read(core) {
                format!(
                    "{}, revision 0x{:x}",
                    stm32_chipname(idc.dev_id()),
                    idc.rev_id()
                )
            } else {
                format!("<unknown ST part 0x{:x}>", coreinfo.manufacturer_part)
            }
        } else if coreinfo.vendor == Vendor::NXP && part == ARMCore::CortexM33 {
            let dieid = LPC55_SYSCON_DIEID::read(core);
            let devid = LPC55_SYSCON_DEVID::read(core);
            let patch = LPC55_ROM_PATCH_VERSION::read(core);

            if let (Ok(dieid), Ok(devid), Ok(patch)) = (dieid, devid, patch) {
                format!(
                    "LPC55, ROM revision {}, device revision 0x{:x} ({}), \
                    ROM patch 0x{:x}",
                    devid.rom_revision(), dieid.rev_id(),
                    match dieid.rev_id() {
                        0x0 => { "0A" },
                        0x1 => { "1B" },
                        _ => { "<unknown>" }
                    },
                    patch.patch_version(),
                )
            } else {
                format!("<unknown NXP M33 0x{:x}>", coreinfo.manufacturer_part)
            }
        } else {
            format!("<unknown part 0x{:x}>", coreinfo.manufacturer_part)
        },
    );

    //
    // Now display our chip status
    //
    statusif(dhcsr.restart_status(), "restarting");
    statusif(dhcsr.reset_status(), "resetting");
    statusif(dhcsr.retire_status(), "executing");
    statusif(dhcsr.locked_up(), "locked up");
    statusif(dhcsr.halted(), "halted");
    statusif(dfsr.external(), "external halt");
    statusif(dfsr.vector_catch(), "vector catch");
    statusif(dfsr.watchpoint(), "watchpoint");
    statusif(dfsr.breakpoint(), "breakpoint");
    statusif(dfsr.halted(), "debug halt");

    print(
        "status",
        if status.is_empty() {
            //
            // If the status is unknown, it doesn't mean very much; from the
            // ARMv7-M ARM on the meaning of S_RETIRE_ST:
            //
            //   The architecture does not define precisely when this bit is
            //   set to 1. It requires only that this happen periodically in
            //   Non-debug state to indicate that software execution is
            //   progressing.
            //
            // To see if the core is actually executing instructions, we
            // will attempt to halt it and step it, seeing if the PC moves.
            //
            let rval = core
                .halt()
                .and_then(|_| core.read_reg(ARMRegister::PC))
                .and_then(|val| {
                    core.step()?;
                    Ok(val)
                })
                .and_then(|val| {
                    if core.read_reg(ARMRegister::PC)? == val {
                        Ok("not progressing")
                    } else {
                        Ok("progressing")
                    }
                })
                .map_or_else(|_| "unable to step", |s| s)
                .to_string();
            core.run()?;
            rval
        } else {
            status.join(", ")
        },
    );

    //
    // Now display information about each CoreSight component found
    //
    let mut sorted = coreinfo
        .components
        .keys()
        .filter(|k| k.displayable())
        .map(|k| (format!("{:?}", k), *k))
        .collect::<Vec<(String, CoreSightComponent)>>();

    sorted.sort();

    let comp = |k: &(String, CoreSightComponent)| {
        let len = coreinfo.components.get_vec(&k.1).unwrap().len();

        if len > 1 {
            format!("{}(x{})", k.0, len)
        } else {
            k.0.to_string()
        }
    };

    let units = sorted.iter().map(comp).collect::<Vec<String>>().join(" ");

    print("debug units", units);

    for component in sorted {
        let addrs = coreinfo
            .components
            .get_vec(&component.1)
            .unwrap()
            .iter()
            .map(|addr| format!("0x{:08x}", *addr))
            .collect::<Vec<String>>()
            .join(", ");

        humility::msg!("{:>12} => {}", component.0, addrs);
    }

    print(
        "ITM status",
        match coreinfo.address(CoreSightComponent::ITM) {
            None => "absent".to_string(),
            Some(_) => {
                let mut itm = vec![];

                if DEMCR::read(core)?.trcena() {
                    itm.push("TRCENA enabled");
                } else {
                    itm.push("TRCENA disabled");
                }

                if ITM_TCR::read(core)?.itm_enable() {
                    itm.push("TCR enabled")
                } else {
                    itm.push("TCR disabled")
                }

                let s = format!("TER=0x{:x}", u32::from(ITM_TER::read(core)?));

                itm.push(&s);
                itm.join(", ")
            }
        },
    );

    if !dhcsr.halted() {
        core.halt()?;
    }

    let regions = match hubris.validate(core, HubrisValidate::ArchiveMatch) {
        Ok(_) => {
            //
            // We want to eat any error here: if we fail to load regions on
            // a validated archive, it's because we weren't in fact provided
            // an archive at all. (Unlike for many commands, the archive is
            // optional for "humility probe".)
            //
            hubris.regions(core).unwrap_or_default()
        }
        Err(err) => {
            //
            // If the archive doesn't match, we'll drive on -- but we will
            // indicate that we can't interpret registers correctly.
            //
            humility::warn!(
                "archive mismatch: {}; register contents will \
                not be displayed symbolically",
                err
            );
            Default::default()
        }
    };

    for i in 0..31 {
        let reg = match ARMRegister::from_u16(i) {
            Some(r) => r,
            None => {
                continue;
            }
        };

        let val = core.read_reg(reg)?;

        humility::msg!(
            "{:>12} => 0x{:8} {}",
            format!("{:?}", reg),
            format!("{:x}", val),
            if reg.is_general_purpose() {
                match hubris.explain(&regions, val) {
                    Some(explain) => format!("  <- {}", explain),
                    None => "".to_string(),
                }
            } else {
                "".to_string()
            }
        );
    }
    let cfsr = CFSR::read(core)?;
    if cfsr.has_fault() {
        humility::msg!("Fault detected! Raw CFSR: 0x{:x}", cfsr.0);
        if let Some(bfsr) = cfsr.get_bfsr() {
            humility::msg!(
                "Bus Fault accessing address {} : {:x?}",
                if bfsr.bfarvalid() {
                    format!("{:x}", BFAR::read(core)?.address())
                } else {
                    "unknown".to_string()
                },
                bfsr,
            );
        }
        if let Some(ufsr) = cfsr.get_ufsr() {
            humility::msg!("Usage Fault : {:x?}", ufsr);
        }
        if let Some(mmfsr) = cfsr.get_mmfsr() {
            humility::msg!(
                "MM Fault accessing address {} : {:x?}",
                if mmfsr.mmfarvalid() {
                    format!("{:x}", MMFAR::read(core)?.address())
                } else {
                    "unknown".to_string()
                },
                mmfsr
            );
        }
    }
    if part.has_tz() {
        let sfsr = SFSR::read(core)?;
        if sfsr.has_fault() {
            humility::msg!(
                "Secure Fault accessing address {} : (raw SFSR 0x{:x}) {:x?}",
                if sfsr.sfarvalid() {
                    format!("{:x}", SFAR::read(core)?.address())
                } else {
                    "unknown".to_string()
                },
                sfsr.0,
                sfsr
            )
        }
    }

    if !dhcsr.halted() {
        core.run()?;
    }

    Ok(())
}

pub fn init() -> Command {
    Command {
        app: ProbeArgs::command(),
        name: "probe",
        run: probecmd,
        kind: CommandKind::Attached {
            archive: Archive::Optional,
            attach: Attach::LiveOnly,
            validate: Validate::None,
        },
    }
}
