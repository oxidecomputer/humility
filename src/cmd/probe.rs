/*
 * Copyright 2020 Oxide Computer Company
 */

use crate::cmd::*;
use crate::core::Core;
use crate::debug::*;
use crate::hubris::*;
use crate::itm::*;
use crate::scs::*;
use crate::Args;
use anyhow::Result;
use structopt::clap::App;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(name = "probe", about = "probe for any attached devices")]
struct ProbeArgs {}

#[rustfmt::skip::macros(format)]
fn probecmd(
    hubris: &mut HubrisArchive,
    core: &mut dyn Core,
    _args: &Args,
    _subargs: &Vec<String>,
) -> Result<()> {
    use num_traits::FromPrimitive;
    let mut status = vec![];

    let print = |what, val| {
        info!("{:>12} => {}", what, val);
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

    /*
     * Start with information about our core and chip...
     */
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
        if coreinfo.vendor == Vendor::ST && part == ARMCore::CortexM4 {
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

            if let (Ok(dieid), Ok(devid)) = (dieid, devid) {
                format!(
                    "LPC55, ROM revision {}, device revision 0x{:x} ({})",
                    devid.rom_revision(), dieid.rev_id(),
                    match dieid.rev_id() {
                        0x0 => { "0A" },
                        0x1 => { "1B" },
                        _ => { "<unknown>" }
                    }
                )
            } else {
                format!("<unknown NXP M33 0x{:x}>", coreinfo.manufacturer_part)
            }
        } else {
            format!("<unknown part 0x{:x}>", coreinfo.manufacturer_part)
        },
    );

    /*
     * Now display our chip status
     */
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
        if status.len() == 0 {
            /*
             * If the status is unknown, it doesn't mean very much; from the
             * ARMv7-M ARM on the meaning of S_RETIRE_ST:
             *
             *   The architecture does not define precisely when this bit is
             *   set to 1. It requires only that this happen periodically in
             *   Non-debug state to indicate that software execution is
             *   progressing.
             *
             * To see if the core is actually executing instructions, we
             * will attempt to halt it and step it, seeing if the PC moves.
             */
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

    /*
     * Now display information about each CoreSight component found
     */
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

        info!("{:>12} => {}", component.0, addrs);
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

    for i in 0..31 {
        let reg = match ARMRegister::from_u16(i) {
            Some(r) => r,
            None => {
                continue;
            }
        };

        let val = core.read_reg(reg)?;

        info!(
            "{:>12} => 0x{:8} {}",
            format!("{:?}", reg),
            format!("{:x}", val),
            if i <= 15 {
                if let Some(sval) = hubris.instr_sym(val) {
                    format!(" <- {}{}+0x{:x}",
                        match hubris.instr_mod(val) {
                            Some(module) if module != "kernel" => {
                                format!("{}:", module)
                            }
                            _ => "".to_string()
                        },
                        sval.0, val - sval.1)
                } else {
                    "".to_string()
                }
            } else {
                "".to_string()
            }
        );
    }

    if !dhcsr.halted() {
        core.run()?;
    }

    Ok(())
}

pub fn init<'a, 'b>() -> (Command, App<'a, 'b>) {
    (
        Command::Attached {
            name: "probe",
            archive: Archive::Optional,
            attach: Attach::LiveOnly,
            validate: Validate::Match,
            run: probecmd,
        },
        ProbeArgs::clap(),
    )
}
