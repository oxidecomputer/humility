// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::{Result, anyhow};
use humility_cli::ExecutionContext;
use humility_hiffy::IpcError;
use std::fmt;
use std::mem::MaybeUninit;

use device::gpioa::RegisterBlock;
use humility_hiffy::HiffyFunction;
use std::collections::BTreeMap;
use stm32h7::stm32h753 as device;

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
    cache: BTreeMap<char, RegisterBlock>,
}

impl ConfigCache {
    fn new() -> ConfigCache {
        let cache: BTreeMap<char, RegisterBlock> = BTreeMap::new();
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

        let reg = rb.moder.read().bits();
        let mode = match (reg >> (pin * 2)) & 3 {
            0 => "Input",
            1 => "Output",
            2 => "Alternate",
            3 => "Analog",
            _ => unreachable!(),
        };

        let reg = rb.otyper.read().bits();
        let otype = match (reg >> pin) & 1 {
            0 => "OutPushPull",
            1 => "OutOpenDrain",
            _ => unreachable!(),
        };

        let reg = rb.ospeedr.read().bits();
        let speed = match (reg >> (pin * 2)) & 3 {
            0 => "LowSpeed",
            1 => "MediumSpeed",
            2 => "HighSpeed",
            3 => "VeryHighSpeed",
            _ => unreachable!(),
        };

        let reg = rb.pupdr.read().bits();
        let pull = match (reg >> (pin * 2)) & 3 {
            0 => "NoPull",
            1 => "PullUp",
            2 => "PullDown",
            3 => "ReservedPull",
            _ => unreachable!(),
        };

        let reg = rb.idr.read().bits();
        let input = match (reg >> pin) & 1 {
            0 => "InZero",
            1 => "InOne",
            _ => unreachable!(),
        };

        let reg = rb.odr.read().bits();
        let output = match (reg >> pin) & 1 {
            0 => "OutZero",
            1 => "OutOne",
            _ => unreachable!(),
        };

        // Note: Bit 16 indicates that the register is locked or unlocked.
        let reg = rb.lckr.read().bits();
        let lock = match (reg >> pin) & 1 {
            0 => "Unlocked",
            1 => "Locked",
            _ => unreachable!(),
        };

        let alternate = format!(
            "AF{}",
            if pin < 8 {
                let reg = rb.afrl.read().bits();
                (reg >> (pin * 4)) & 15
            } else {
                let reg = rb.afrh.read().bits();
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
        'A' => Some(device::GPIOA::ptr() as u32),
        'B' => Some(device::GPIOB::ptr() as u32),
        'C' => Some(device::GPIOC::ptr() as u32),
        'D' => Some(device::GPIOD::ptr() as u32),
        'E' => Some(device::GPIOE::ptr() as u32),
        'F' => Some(device::GPIOF::ptr() as u32),
        'G' => Some(device::GPIOG::ptr() as u32),
        'H' => Some(device::GPIOH::ptr() as u32),
        'I' => Some(device::GPIOI::ptr() as u32),
        'J' => Some(device::GPIOJ::ptr() as u32),
        'K' => Some(device::GPIOK::ptr() as u32),
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
) -> Result<RegisterBlock> {
    let core = &mut **context.core.as_mut().unwrap();
    chip_has_group(chip, group)?;
    let addr = stm32h753_gpio_registerblock_addr(group)
        .ok_or(anyhow!("no address for GPIO group {}", group))?;
    let mut buffer = vec![0u8; std::mem::size_of::<RegisterBlock>()];
    core.read_8(addr, &mut buffer)?;

    let mut rb: MaybeUninit<RegisterBlock> = MaybeUninit::uninit();
    // SAFETY: Copying data from a HW defined struct into that struct.
    unsafe {
        let dest_ptr = rb.as_mut_ptr() as *mut u8;
        std::ptr::copy_nonoverlapping(buffer.as_ptr(), dest_ptr, buffer.len());

        Ok(rb.assume_init())
    }
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
