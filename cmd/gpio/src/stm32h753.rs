use humility_cli::ExecutionContext;
use humility_hiffy::IpcError;
use std::mem::MaybeUninit;
use std::fmt;
use anyhow::{anyhow, Result};
 
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

struct ConfigCache {
    cache: BTreeMap<String, RegisterBlock>,
}

impl ConfigCache {
    fn new() -> ConfigCache {
        let cache: BTreeMap<String, RegisterBlock> = BTreeMap::new();
        Self { cache }
    }

    fn get_pin_config(
        &mut self,
        context: &mut ExecutionContext,
        group: &str,
        pin: u32,
    ) -> Result<PinConfig> {
        let rb = match self.cache.get(group) {
            None => {
                let rb = read_gpio_register_block(context, group)?;
                self.cache.insert(group.to_string(), rb);
                self.cache.get(group).unwrap()
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

fn stm32h753_gpio_registerblock_addr(group: &str) -> Option<u32> {
    // Section 11.4: GPIO registers
    match group {
        "A" => Some(device::GPIOA::ptr() as u32),
        "B" => Some(device::GPIOB::ptr() as u32),
        "C" => Some(device::GPIOC::ptr() as u32),
        "D" => Some(device::GPIOD::ptr() as u32),
        "E" => Some(device::GPIOE::ptr() as u32),
        "F" => Some(device::GPIOF::ptr() as u32),
        "G" => Some(device::GPIOG::ptr() as u32),
        "H" => Some(device::GPIOH::ptr() as u32),
        "I" => Some(device::GPIOI::ptr() as u32),
        "J" => Some(device::GPIOJ::ptr() as u32),
        "K" => Some(device::GPIOK::ptr() as u32),
        _ => None,
    }
}

fn read_gpio_register_block(
    context: &mut ExecutionContext,
    group: &str,
) -> Result<RegisterBlock> {
    let core = &mut **context.core.as_mut().unwrap();
    let addr = stm32h753_gpio_registerblock_addr(group).ok_or(anyhow!("no address for GPIO group {}", group))?;
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
    args: &[(u16, Option<u8>, String)],
    results: &[Result<Vec<u8>, IpcError>],
) -> Result<()> {
    let mut config_cache = ConfigCache::new();
    for (ndx, arg) in args.iter().enumerate() {
        match arg.1 {
            Some(pin) => {
                let config =
                    config_cache.get_pin_config(context, &arg.2, pin as u32)?;
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
                        let config = config_cache
                            .get_pin_config(context, &arg.2, i as u32)?;
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
