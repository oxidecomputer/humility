// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::{Result, anyhow};
use humility_cli::ExecutionContext;
use humility_hiffy::HiffyFunction;
use humility_hiffy::IpcError;

use crate::config_cache::ConfigCache;
use crate::stm32h7_parts::{Stm32H7Part, Stm32H7Series};

/// Read GPIO register block from device memory
fn read_gpio_register_block(
    context: &mut ExecutionContext,
    part: &Stm32H7Part,
    group: char,
) -> Result<Vec<u8>> {
    let core = context.core.as_mut().unwrap();
    let addr = part
        .gpio_register_block_address(group)
        .ok_or(anyhow!("no address for GPIO group {}", group))?;
    let mut buffer = vec![0u8; Stm32H7Part::gpio_register_block_size()];
    core.read_8(addr, &mut buffer)?;
    Ok(buffer)
}

pub fn show_gpio_with_config(
    context: &mut ExecutionContext,
    gpio_input: &HiffyFunction,
    chip: &str,
    args: &[(u16, Option<u8>, String)],
    results: &[Result<Vec<u8>, IpcError>],
) -> Result<()> {
    // Look up chip in database
    let part = Stm32H7Series::find_part(chip, true);

    // Reject --with-config for unknown chips (backward compatibility: only basic GPIO ops)
    if !part.is_known() {
        return Err(anyhow!(
            "GPIO `--with-config` is not supported for unknown chip '{}'. \
             Use `--input` without `-w` for basic GPIO operations.",
            chip
        ));
    }

    let mut config_cache = ConfigCache::new();

    for (ndx, arg) in args.iter().enumerate() {
        let group =
            arg.2.chars().next().ok_or(anyhow!("invalid group '{}'", arg.2))?;

        // Check if this group exists on this chip variant
        let group_available = group >= 'A' && group <= part.max_group();

        match arg.1 {
            Some(pin) => {
                // Specific pin requested
                if !group_available {
                    // Group doesn't exist on this chip - show value but no config
                    println!(
                        "{}:{:<2} = {} None",
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
                        }
                    );
                } else if pin >= part.pins_in_group(group) {
                    // Pin doesn't exist in this partially filled group
                    println!(
                        "{}:{:<2} = {} None",
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
                        }
                    );
                } else {
                    // Group exists and pin is valid - show config
                    let register_data = config_cache.get_or_fetch(
                        context,
                        group,
                        |ctx, g| read_gpio_register_block(ctx, part, g),
                    )?;

                    let config =
                        Stm32H7Part::get_pin_config(register_data, pin)?;

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
            }

            None => {
                // All pins in port requested
                if !group_available {
                    // Skip ports that don't exist on this chip variant
                    continue;
                }

                match results[ndx] {
                    Err(code) => {
                        println!(
                            "Port {}: {}",
                            arg.2,
                            gpio_input.strerror(code)
                        )
                    }
                    Ok(ref val) => {
                        let arr: &[u8; 2] = val[0..2].try_into()?;
                        let v = u16::from_le_bytes(*arr);

                        // Get cached register block data
                        let register_data = config_cache.get_or_fetch(
                            context,
                            group,
                            |ctx, g| read_gpio_register_block(ctx, part, g),
                        )?;

                        let max_pin = part.pins_in_group(group);
                        for i in 0..16 {
                            if i >= max_pin {
                                // Pin doesn't exist in this partially filled group - skip it
                                continue;
                            }

                            let config =
                                Stm32H7Part::get_pin_config(register_data, i)?;

                            println!(
                                "{}:{:<2} = {} {}",
                                arg.2,
                                i,
                                (v >> i) & 1,
                                config
                            );
                        }
                    }
                }
            }
        }
    }
    Ok(())
}
