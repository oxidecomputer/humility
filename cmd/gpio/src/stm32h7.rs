// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::{Result, anyhow};
use humility_cli::ExecutionContext;
use humility_hiffy::IpcError;
use std::fmt;
use zerocopy::{AsBytes, FromBytes};

use humility_hiffy::HiffyFunction;
use std::collections::BTreeMap;

/// STM32H7 series GPIO Registers
///
/// Pulling these in from any of the available public crates is
/// problematic. The "stm32h7" crate will prevent us from linking
/// on Windows and the "stm32-metapac" crate forces us to calculate
/// offsets for all of the fields in the GPIO Register blocks.
/// In this case, the code is more readable and easier to maintain
/// if we just get our information from the ST Micro datasheet: "RM0433".
#[derive(FromBytes, AsBytes, Copy, Clone)]
#[repr(C)]
struct GpioRegisters {
    /// Mode Register
    moder: u32, // 0x00
    /// Output Type Register
    otyper: u32, // 0x04
    /// Output Speed Register
    ospeedr: u32, // 0x08
    /// Pull-Up/Pull-Down Register
    pupdr: u32, // 0x0C
    /// Input Data Register
    idr: u32, // 0x10
    /// Output Data Register
    odr: u32, // 0x14
    /// Bit Set/Reset Register (write-only)
    bsrr: u32, // 0x18
    /// Configuration Lock Register
    lckr: u32, // 0x1C
    /// Alternate Function Low Register
    afrl: u32, // 0x20
    /// Alternate Function High Register
    afrh: u32, // 0x24
}

struct PinConfig {
    mode: String,
    otype: String,
    speed: String,
    pull: String,
    input: String,
    output: String,
    lock: String,
    alternate: String,
}

impl fmt::Display for PinConfig {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Use the write! macro to format the output to the formatter 'f'
        write!(
            f,
            "{}:{}:{}:{}:{}:{}:{}:{}",
            self.mode,
            self.otype,
            self.speed,
            self.pull,
            self.alternate,
            self.lock,
            self.input,
            self.output
        )
    }
}

/// Store an STM32H7 GPIO group ID ('A', 'B', etc.) and the associated GPIO
/// configuration register block to avoid repeated IO for each Pin.
struct ConfigCache {
    cache: BTreeMap<char, GpioRegisters>,
}

impl ConfigCache {
    fn new() -> ConfigCache {
        let cache: BTreeMap<char, GpioRegisters> = BTreeMap::new();
        Self { cache }
    }

    fn get_pin_config(
        &mut self,
        context: &mut ExecutionContext,
        chip: &str,
        group: char,
        pin: u32,
    ) -> Result<PinConfig> {
        let rb = match self.cache.get(&group) {
            None => {
                let rb = read_gpio_register_block(context, chip, group)?;
                self.cache.insert(group, rb);
                self.cache.get(&group).unwrap()
            }
            Some(rb) => rb,
        };

        let reg = rb.moder;
        let mode = match (reg >> (pin * 2)) & 3 {
            0 => "Input",
            1 => "Output",
            2 => "Alternate",
            3 => "Analog",
            _ => unreachable!(),
        };

        let reg = rb.otyper;
        let otype = match (reg >> pin) & 1 {
            0 => "OutPushPull",
            1 => "OutOpenDrain",
            _ => unreachable!(),
        };

        let reg = rb.ospeedr;
        let speed = match (reg >> (pin * 2)) & 3 {
            0 => "LowSpeed",
            1 => "MediumSpeed",
            2 => "HighSpeed",
            3 => "VeryHighSpeed",
            _ => unreachable!(),
        };

        let reg = rb.pupdr;
        let pull = match (reg >> (pin * 2)) & 3 {
            0 => "NoPull",
            1 => "PullUp",
            2 => "PullDown",
            3 => "ReservedPull",
            _ => unreachable!(),
        };

        let reg = rb.idr;
        let input = match (reg >> pin) & 1 {
            0 => "InZero",
            1 => "InOne",
            _ => unreachable!(),
        };

        let reg = rb.odr;
        let output = match (reg >> pin) & 1 {
            0 => "OutZero",
            1 => "OutOne",
            _ => unreachable!(),
        };

        // Note: Bit 16 indicates that the register is locked or unlocked.
        let reg = rb.lckr;
        let lock = match (reg >> pin) & 1 {
            0 => "Unlocked",
            1 => "Locked",
            _ => unreachable!(),
        };

        let alternate = format!(
            "AF{}",
            if pin < 8 {
                let reg = rb.afrl;
                (reg >> (pin * 4)) & 15
            } else {
                let reg = rb.afrh;
                (reg >> ((pin - 8) * 4)) & 15
            }
        );

        Ok(PinConfig {
            mode: mode.to_string(),
            otype: otype.to_string(),
            speed: speed.to_string(),
            pull: pull.to_string(),
            input: input.to_string(),
            output: output.to_string(),
            lock: lock.to_string(),
            alternate,
        })
    }
}

/// Lookup STM32H7 GPIO configuration register addresses.
fn stm32h753_gpio_registerblock_addr(group: char) -> Option<u32> {
    // Section 11.4: GPIO registers
    match group {
        'A' => Some(0x5802_0000u32),
        'B' => Some(0x5802_0400u32),
        'C' => Some(0x5802_0800u32),
        'D' => Some(0x5802_0C00u32),
        'E' => Some(0x5802_1000u32),
        'F' => Some(0x5802_1400u32),
        'G' => Some(0x5802_1800u32),
        'H' => Some(0x5802_1C00u32),
        'I' => Some(0x5802_2000u32),
        'J' => Some(0x5802_2400u32),
        'K' => Some(0x5802_2800u32),
        _ => None,
    }
}

// Several other STM32 families share the same GPIO register format as the
// STM32H7 series:
//   - STM32F7 series
//   - STM32G4 series
// Support for those chips can be added if needed.
// Note: verify that the non-STM32H7 series are using the same addressing or adapt.
fn chip_has_group(chip: &str, group: char) -> Result<bool> {
    let name = chip.to_ascii_lowercase();

    if name.starts_with("stm32h753") {
        Ok(matches!(group, 'A'..='K'))
    } else if name.starts_with("stm32h743") {
        Ok(matches!(group, 'A'..='I'))
    } else {
        Err(anyhow!("unknown STM32H7 chip '{}'", chip))
    }
}

fn read_gpio_register_block(
    context: &mut ExecutionContext,
    chip: &str,
    group: char,
) -> Result<GpioRegisters> {
    let core = context.core.as_mut().unwrap();
    chip_has_group(chip, group)?;
    let addr = stm32h753_gpio_registerblock_addr(group)
        .ok_or(anyhow!("no address for GPIO group {}", group))?;
    let mut buffer = vec![0u8; std::mem::size_of::<GpioRegisters>()];
    core.read_8(addr, &mut buffer)?;
    let rb = GpioRegisters::read_from_prefix(buffer.as_slice())
        .ok_or(anyhow!("Slice length is incorrect for GpioRegisters"))?;
    Ok(rb)
}

pub fn show_gpio_with_config(
    context: &mut ExecutionContext,
    gpio_input: &HiffyFunction,
    chip: &str,
    args: &[(u16, Option<u8>, String)],
    results: &[Result<Vec<u8>, IpcError>],
) -> Result<()> {
    let mut config_cache = ConfigCache::new();
    for (ndx, arg) in args.iter().enumerate() {
        match arg.1 {
            Some(pin) => {
                let group = arg
                    .2
                    .chars()
                    .next()
                    .ok_or(anyhow!("invalid group '{}'", arg.2))?;
                let config = config_cache
                    .get_pin_config(context, chip, group, pin as u32)?;
                println!(
                    "{}:{:<2} = {} {}",
                    arg.2,
                    pin,
                    match results[ndx] {
                        Err(code) => {
                            gpio_input.strerror(code)
                        }
                        Ok(ref val) => {
                            let arr: &[u8; 2] = val[0..2].try_into()?;
                            let v = u16::from_le_bytes(*arr);
                            format!("{}", (v >> pin) & 1)
                        }
                    },
                    config
                );
            }

            None => match results[ndx] {
                Err(code) => {
                    println!("Port {}: {}", arg.2, gpio_input.strerror(code))
                }
                Ok(ref val) => {
                    let arr: &[u8; 2] = val[0..2].try_into()?;
                    let v = u16::from_le_bytes(*arr);

                    for i in 0..16 {
                        let group = arg
                            .2
                            .chars()
                            .next()
                            .ok_or(anyhow!("invalid group '{}'", arg.2))?;
                        let config = config_cache
                            .get_pin_config(context, chip, group, i as u32)?;
                        println!(
                            "{}:{:<2} = {} {}",
                            arg.2,
                            i,
                            (v >> i) & 1,
                            config
                        );
                    }
                }
            },
        }
    }
    Ok(())
}
