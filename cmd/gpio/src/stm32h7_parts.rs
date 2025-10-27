// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! STM32H7 parts database and GPIO configuration interpretation.
//!
//! This module is based on the STMCUFinder database and provides part lookup
//! and GPIO register interpretation for STM32H7 series microcontrollers.

use anyhow::{Result, anyhow};
use std::fmt;
use zerocopy::{AsBytes, FromBytes};

/// Represents a specific STM32H7 series microcontroller part.
#[derive(Debug, PartialEq, Clone)]
pub struct Stm32H7Part {
    com_part_no: &'static str,
    part_no: &'static str,
    ref_no: &'static str,
    gpio_count: u8,
    groups: u8,
    max_group: char,
}

impl Stm32H7Part {
    // --- Public Getters ---
    #[cfg(test)]
    pub fn com_part_no(&self) -> &'static str {
        self.com_part_no
    }
    #[cfg(test)]
    pub fn part_no(&self) -> &'static str {
        self.part_no
    }
    #[cfg(test)]
    pub fn ref_no(&self) -> &'static str {
        self.ref_no
    }
    #[cfg(test)]
    pub fn gpio_count(&self) -> u8 {
        self.gpio_count
    }

    #[cfg(test)]
    pub fn groups(&self) -> u8 {
        self.groups
    }
    pub fn max_group(&self) -> char {
        self.max_group
    }

    /// Returns true if this is a known chip from the database, false if it's
    /// the fallback for backward compatibility with unknown chips.
    pub fn is_known(&self) -> bool {
        self.part_no != "Unknown"
    }

    /// Get the number of valid pins in a specific GPIO group.
    /// Returns number of pins (0-16) that actually exist in the group.
    pub fn pins_in_group(&self, group: char) -> u8 {
        if group < 'A' || group > self.max_group {
            return 0;
        }

        let group_index = (group as u8) - b'A';
        let full_groups = self.gpio_count / 16;

        if group_index < full_groups {
            // This is a fully populated group
            16
        } else if group_index == full_groups {
            // This is a partially populated group
            let remainder = self.gpio_count % 16;
            if remainder == 0 { 16 } else { remainder }
        } else {
            // Beyond the last group
            0
        }
    }

    /// Lookup STM32H7 GPIO configuration register addresses.
    ///
    /// Calculates the register block address for a given GPIO group.
    /// Each group is offset by 0x400 bytes from the base address.
    /// This method validates against the chip's maximum supported group.
    pub fn gpio_register_block_address(&self, group: char) -> Option<u32> {
        // Section 11.4: GPIO registers
        // Base address for GPIO Port A
        const BASE_ADDR: u32 = 0x5802_0000;
        const GROUP_OFFSET: u32 = 0x400;

        if group < 'A' || group > self.max_group {
            return None;
        }

        let group_index = (group as u8 - b'A') as u32;
        Some(BASE_ADDR + (group_index * GROUP_OFFSET))
    }

    pub fn gpio_register_block_size() -> usize {
        core::mem::size_of::<GpioRegisters>()
    }

    /// Get the detailed configuration of a specific pin.
    pub fn get_pin_config(
        register_block_data: &[u8],
        pin: u8,
    ) -> Result<PinConfig> {
        let rb = GpioRegisters::read_from_prefix(register_block_data).ok_or(
            anyhow!("cannot convert register_block_data to GpioRegisters"),
        )?;
        // Note: Apart from Reserved values, all data can be mapped to valid `GpioRegisters`.

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

        let af_value: &[&'static str] = &[
            "AF0", "AF1", "AF2", "AF3", "AF4", "AF5", "AF6", "AF7", "AF8",
            "AF9", "AF10", "AF11", "AF12", "AF13", "AF14", "AF15",
        ];

        let alternate = af_value[if pin < 8 {
            let reg = rb.afrl;
            (reg >> (pin * 4)) & 15
        } else {
            let reg = rb.afrh;
            (reg >> ((pin - 8) * 4)) & 15
        } as usize];

        Ok(PinConfig {
            mode,
            otype,
            speed,
            pull,
            input,
            output,
            lock,
            alternate,
        })
    }
}

/// Fallback part definition for unknown chips.
/// Provides backward compatibility with old behavior that assumed all chips
/// have GPIO groups A-K (11 groups, 176 pins total).
/// This allows basic GPIO operations without full chip knowledge.
const UNKNOWN_PART: Stm32H7Part = Stm32H7Part {
    com_part_no: "Unknown",
    part_no: "Unknown",
    ref_no: "Unknown",
    gpio_count: 176, // 11 groups * 16 pins
    groups: 11,
    max_group: 'K',
};

/// The main, read-only database of STM32H7 parts for normal (non-test) builds.
const PARTS: &[Stm32H7Part] = &[
    // Commercial Part No,Part No,Reference,I/O,Groups,MaxGroup
    Stm32H7Part {
        com_part_no: "STM32H753ZIT6",
        part_no: "STM32H753ZI",
        ref_no: "STM32H753ZITx",
        gpio_count: 114,
        groups: 8,
        max_group: 'H',
    },
    Stm32H7Part {
        com_part_no: "STM32H753XIH6TR",
        part_no: "STM32H753XI",
        ref_no: "STM32H753XIHx",
        gpio_count: 172,
        groups: 11,
        max_group: 'K',
    },
    Stm32H7Part {
        com_part_no: "STM32H753XIH6",
        part_no: "STM32H753XI",
        ref_no: "STM32H753XIHx",
        gpio_count: 172,
        groups: 11,
        max_group: 'K',
    },
    Stm32H7Part {
        com_part_no: "STM32H753VIT6",
        part_no: "STM32H753VI",
        ref_no: "STM32H753VITx",
        gpio_count: 82,
        groups: 6,
        max_group: 'F',
    },
    Stm32H7Part {
        com_part_no: "STM32H753VIH6",
        part_no: "STM32H753VI",
        ref_no: "STM32H753VIHx",
        gpio_count: 82,
        groups: 6,
        max_group: 'F',
    },
    Stm32H7Part {
        com_part_no: "STM32H753IIT6",
        part_no: "STM32H753II",
        ref_no: "STM32H753IITx",
        gpio_count: 140,
        groups: 9,
        max_group: 'I',
    },
    Stm32H7Part {
        com_part_no: "STM32H753IIK6",
        part_no: "STM32H753II",
        ref_no: "STM32H753IIKx",
        gpio_count: 140,
        groups: 9,
        max_group: 'I',
    },
    Stm32H7Part {
        com_part_no: "STM32H753BIT6",
        part_no: "STM32H753BI",
        ref_no: "STM32H753BITx",
        gpio_count: 168,
        groups: 11,
        max_group: 'K',
    },
    Stm32H7Part {
        com_part_no: "STM32H753AII6",
        part_no: "STM32H753AI",
        ref_no: "STM32H753AIIx",
        gpio_count: 132,
        groups: 9,
        max_group: 'I',
    },
    Stm32H7Part {
        com_part_no: "STM32H743ZIT6TR",
        part_no: "STM32H743ZI",
        ref_no: "STM32H743ZITx",
        gpio_count: 114,
        groups: 8,
        max_group: 'H',
    },
    Stm32H7Part {
        com_part_no: "STM32H743ZIT6",
        part_no: "STM32H743ZI",
        ref_no: "STM32H743ZITx",
        gpio_count: 114,
        groups: 8,
        max_group: 'H',
    },
    Stm32H7Part {
        com_part_no: "STM32H743ZGT6",
        part_no: "STM32H743ZG",
        ref_no: "STM32H743ZGTx",
        gpio_count: 114,
        groups: 8,
        max_group: 'H',
    },
    Stm32H7Part {
        com_part_no: "STM32H743XIH6",
        part_no: "STM32H743XI",
        ref_no: "STM32H743XIHx",
        gpio_count: 172,
        groups: 11,
        max_group: 'K',
    },
    Stm32H7Part {
        com_part_no: "STM32H743XGH6",
        part_no: "STM32H743XG",
        ref_no: "STM32H743XGHx",
        gpio_count: 172,
        groups: 11,
        max_group: 'K',
    },
    Stm32H7Part {
        com_part_no: "STM32H743VIT6TR",
        part_no: "STM32H743VI",
        ref_no: "STM32H743VITx",
        gpio_count: 82,
        groups: 6,
        max_group: 'F',
    },
    Stm32H7Part {
        com_part_no: "STM32H743VIT6",
        part_no: "STM32H743VI",
        ref_no: "STM32H743VITx",
        gpio_count: 82,
        groups: 6,
        max_group: 'F',
    },
    Stm32H7Part {
        com_part_no: "STM32H743VIH6TR",
        part_no: "STM32H743VI",
        ref_no: "STM32H743VIHx",
        gpio_count: 82,
        groups: 6,
        max_group: 'F',
    },
    Stm32H7Part {
        com_part_no: "STM32H743VIH6",
        part_no: "STM32H743VI",
        ref_no: "STM32H743VIHx",
        gpio_count: 82,
        groups: 6,
        max_group: 'F',
    },
    Stm32H7Part {
        com_part_no: "STM32H743VGT6",
        part_no: "STM32H743VG",
        ref_no: "STM32H743VGTx",
        gpio_count: 82,
        groups: 6,
        max_group: 'F',
    },
    Stm32H7Part {
        com_part_no: "STM32H743VGH6",
        part_no: "STM32H743VG",
        ref_no: "STM32H743VGHx",
        gpio_count: 82,
        groups: 6,
        max_group: 'F',
    },
    Stm32H7Part {
        com_part_no: "STM32H743IIT6",
        part_no: "STM32H743II",
        ref_no: "STM32H743IITx",
        gpio_count: 140,
        groups: 9,
        max_group: 'I',
    },
    Stm32H7Part {
        com_part_no: "STM32H743IIK6TR",
        part_no: "STM32H743II",
        ref_no: "STM32H743IIKx",
        gpio_count: 140,
        groups: 9,
        max_group: 'I',
    },
    Stm32H7Part {
        com_part_no: "STM32H743IIK6",
        part_no: "STM32H743II",
        ref_no: "STM32H743IIKx",
        gpio_count: 140,
        groups: 9,
        max_group: 'I',
    },
    Stm32H7Part {
        com_part_no: "STM32H743IGT6",
        part_no: "STM32H743IG",
        ref_no: "STM32H743IGTx",
        gpio_count: 140,
        groups: 9,
        max_group: 'I',
    },
    Stm32H7Part {
        com_part_no: "STM32H743IGK6",
        part_no: "STM32H743IG",
        ref_no: "STM32H743IGKx",
        gpio_count: 140,
        groups: 9,
        max_group: 'I',
    },
    Stm32H7Part {
        com_part_no: "STM32H743BIT6",
        part_no: "STM32H743BI",
        ref_no: "STM32H743BITx",
        gpio_count: 168,
        groups: 11,
        max_group: 'K',
    },
    Stm32H7Part {
        com_part_no: "STM32H743BGT6",
        part_no: "STM32H743BG",
        ref_no: "STM32H743BGTx",
        gpio_count: 168,
        groups: 11,
        max_group: 'K',
    },
    Stm32H7Part {
        com_part_no: "STM32H743AII6TR",
        part_no: "STM32H743AI",
        ref_no: "STM32H743AIIx",
        gpio_count: 132,
        groups: 9,
        max_group: 'I',
    },
    Stm32H7Part {
        com_part_no: "STM32H743AII6",
        part_no: "STM32H743AI",
        ref_no: "STM32H743AIIx",
        gpio_count: 132,
        groups: 9,
        max_group: 'I',
    },
    Stm32H7Part {
        com_part_no: "STM32H743AGI6",
        part_no: "STM32H743AG",
        ref_no: "STM32H743AGIx",
        gpio_count: 132,
        groups: 9,
        max_group: 'I',
    },
];

/// A static, read-only database of parts that only exist for testing purposes.
/// This is conditionally compiled and is only available when running `cargo test`.
#[cfg(test)]
const TEST_ONLY_PARTS: &[Stm32H7Part] = &[
    // Entry to create an ambiguous match on `part_no` for testing.
    Stm32H7Part {
        com_part_no: "STM32H753BI-TEST",
        part_no: "STM32H753BI",
        ref_no: "STM32H753BITx-TEST",
        gpio_count: 50,
        groups: 4,
        max_group: 'D',
    },
];

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
pub struct GpioRegisters {
    /// Mode Register
    pub moder: u32, // 0x00
    /// Output Type Register
    pub otyper: u32, // 0x04
    /// Output Speed Register
    pub ospeedr: u32, // 0x08
    /// Pull-Up/Pull-Down Register
    pub pupdr: u32, // 0x0C
    /// Input Data Register
    pub idr: u32, // 0x10
    /// Output Data Register
    pub odr: u32, // 0x14
    /// Bit Set/Reset Register (write-only)
    pub bsrr: u32, // 0x18
    /// Configuration Lock Register
    pub lckr: u32, // 0x1C
    /// Alternate Function Low Register
    pub afrl: u32, // 0x20
    /// Alternate Function High Register
    pub afrh: u32, // 0x24
}

/// Pin configuration details
#[derive(Debug)]
pub struct PinConfig {
    pub mode: &'static str,
    pub otype: &'static str,
    pub speed: &'static str,
    pub pull: &'static str,
    pub input: &'static str,
    pub output: &'static str,
    pub lock: &'static str,
    pub alternate: &'static str,
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

/// Represents the STM32H7 series of microcontrollers.
///
/// This struct acts as a namespace for functions and data specific to the H7 series.
/// It can be expanded to implement a more generic `ChipSeries` trait in the future.
pub struct Stm32H7Series;

impl Stm32H7Series {
    /// Checks a list of matched parts for uniqueness based on gpio_count.
    fn check_uniqueness(
        matches: Vec<&'static Stm32H7Part>,
        prefer_max_pins: bool,
    ) -> Result<&'static Stm32H7Part, &'static str> {
        if matches.len() == 1 {
            return Ok(matches[0]);
        }

        let first_gpio_count = matches[0].gpio_count;
        if matches.iter().all(|p| p.gpio_count == first_gpio_count) {
            Ok(matches[0])
        } else if prefer_max_pins {
            matches
                .into_iter()
                .max_by_key(|p| p.gpio_count)
                .ok_or("Failed to find part with max pins")
        } else {
            Err(
                "Ambiguous match: Multiple parts found with different GPIO counts",
            )
        }
    }

    /// Finds a part by an identifier, checking fields in a specific order.
    ///
    /// Returns a known part from the database if found, or returns the UNKNOWN_PART
    /// fallback for backward compatibility. The caller can use `is_known()` to
    /// determine if the part is in the database.
    pub fn find_part(
        identifier: &str,
        prefer_max_pins: bool,
    ) -> &'static Stm32H7Part {
        // Collect parts into a Vec to easily create new iterators.
        let all_parts: Vec<&'static Stm32H7Part> = {
            #[cfg(test)]
            {
                PARTS.iter().chain(TEST_ONLY_PARTS.iter()).collect()
            }
            #[cfg(not(test))]
            {
                PARTS.iter().collect()
            }
        };

        // Level 1: Match by `ref_no`
        let matches_ref_no: Vec<_> = all_parts
            .iter()
            .filter(|p| p.ref_no.eq_ignore_ascii_case(identifier))
            .cloned()
            .collect();
        if !matches_ref_no.is_empty()
            && let Ok(part) =
                Self::check_uniqueness(matches_ref_no, prefer_max_pins)
        {
            return part;
        }

        // Level 2: Match by `com_part_no`
        let matches_com_part_no: Vec<_> = all_parts
            .iter()
            .filter(|p| p.com_part_no.eq_ignore_ascii_case(identifier))
            .cloned()
            .collect();
        if !matches_com_part_no.is_empty()
            && let Ok(part) =
                Self::check_uniqueness(matches_com_part_no, prefer_max_pins)
        {
            return part;
        }

        // Level 3: Match by `part_no`
        let matches_part_no: Vec<_> = all_parts
            .iter()
            .filter(|p| p.part_no.eq_ignore_ascii_case(identifier))
            .cloned()
            .collect();
        if !matches_part_no.is_empty()
            && let Ok(part) =
                Self::check_uniqueness(matches_part_no, prefer_max_pins)
        {
            return part;
        }

        // Not found in database - return unknown part fallback for backward compatibility
        &UNKNOWN_PART
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use zerocopy::AsBytes;

    #[test]
    fn test_find_part_success() {
        // Unique match on `ref_no`
        let part = Stm32H7Series::find_part("STM32H753AIIx", false);
        assert!(part.is_known());
        assert_eq!(part.ref_no(), "STM32H753AIIx");

        // Unique match on `com_part_no`
        let part = Stm32H7Series::find_part("STM32H743BGT6", false);
        assert!(part.is_known());
        assert_eq!(part.com_part_no(), "STM32H743BGT6");

        // Multiple matches, but same gpio_count
        let part = Stm32H7Series::find_part("STM32H753XI", false);
        assert!(part.is_known());
        assert_eq!(part.part_no(), "STM32H753XI");
    }

    #[test]
    fn test_find_part_case_insensitive() {
        // Test with lowercase identifier
        let part = Stm32H7Series::find_part("stm32h753aiix", false);
        assert!(part.is_known());
        assert_eq!(part.ref_no(), "STM32H753AIIx");

        // Test with mixed case identifier
        let part = Stm32H7Series::find_part("Stm32H743BGT6", false);
        assert!(part.is_known());
        assert_eq!(part.com_part_no(), "STM32H743BGT6");
    }

    #[test]
    fn test_part_not_found() {
        // Returns unknown fallback for backward compatibility
        let part = Stm32H7Series::find_part("STM32F407VGT6", false);
        assert!(!part.is_known());
        assert_eq!(part.max_group(), 'K');
        assert_eq!(part.gpio_count(), 176);
    }

    #[test]
    fn test_ambiguous_match_with_max_pins_flag() {
        // When ambiguous, prefer_max_pins flag selects highest pin count
        let part = Stm32H7Series::find_part("STM32H753BI", true);
        assert!(part.is_known());
        assert_eq!(part.gpio_count, 168);
    }

    #[test]
    fn test_pins_in_group() {
        // Test STM32H753ZITx: 114 pins, groups A-H (8 groups)
        // 114 / 16 = 7 full groups (A-G = 112 pins), group H has 2 pins
        let part = Stm32H7Series::find_part("STM32H753ZITx", false);
        assert_eq!(part.gpio_count(), 114);
        assert_eq!(part.max_group(), 'H');

        // Groups A-G should have all 16 pins
        for group in 'A'..='G' {
            assert_eq!(
                part.pins_in_group(group),
                16,
                "Group {} should have 16 pins",
                group
            );
        }
        // Group H should have 2 pins (114 - 112 = 2)
        assert_eq!(part.pins_in_group('H'), 2);

        // Groups beyond H should have 0 pins
        assert_eq!(part.pins_in_group('I'), 0);
        assert_eq!(part.pins_in_group('J'), 0);

        // Test STM32H753VIT6: 82 pins, groups A-F (6 groups)
        // 82 / 16 = 5 full groups (A-E = 80 pins), group F has 2 pins
        let part = Stm32H7Series::find_part("STM32H753VIT6", false);
        assert_eq!(part.gpio_count(), 82);
        assert_eq!(part.max_group(), 'F');

        // Groups A-E should have all 16 pins
        for group in 'A'..='E' {
            assert_eq!(
                part.pins_in_group(group),
                16,
                "Group {} should have 16 pins",
                group
            );
        }
        // Group F should have 2 pins (82 - 80 = 2)
        assert_eq!(part.pins_in_group('F'), 2);

        // Groups beyond F should have 0 pins
        assert_eq!(part.pins_in_group('G'), 0);
    }

    #[test]
    fn interpret_config_data() {
        use crate::stm32h7_testdata::GPIO_TEST_DATA;

        let partname = "STM32H753XI";
        let part = Stm32H7Series::find_part(partname, false);
        assert!(part.is_known(), "Lookup failed for {}", partname);
        assert_eq!(part.gpio_count(), 172);
        assert_eq!(part.groups(), 11);
        assert_eq!(part.max_group(), 'K');

        for test_data in GPIO_TEST_DATA.iter() {
            let result = part.gpio_register_block_address(test_data.group);
            assert!(
                result.is_some(),
                "gpio_register_block_address failed for {} group {}",
                partname,
                test_data.group
            );
            let addr = result.unwrap();
            assert_eq!(addr, test_data.addr);

            for pin in 0u8..16u8 {
                let result = Stm32H7Part::get_pin_config(
                    test_data.config_data.as_bytes(),
                    pin,
                );
                assert!(
                    result.is_ok(),
                    "get_pin_config failed for {}:{}:{}",
                    partname,
                    test_data.group,
                    pin
                );
                let pin_config = result.unwrap();
                assert_eq!(
                    pin_config.to_string(),
                    test_data.expected_outputs[pin as usize],
                    "Group:{} Pin:{}",
                    test_data.group,
                    pin
                );
            }
        }
    }
}
