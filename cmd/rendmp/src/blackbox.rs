// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::fmt::{Display, Formatter};
use zerocopy::FromBytes;

type U32 = zerocopy::byteorder::U32<zerocopy::BE>;
type U16 = zerocopy::byteorder::U16<zerocopy::BE>;
type I16 = zerocopy::byteorder::I16<zerocopy::BE>;

fn format_faults<T: Into<u32> + std::fmt::Binary + Copy>(
    v: T,
    faults: &[(u32, &'static str)],
    f: &mut Formatter,
) -> std::fmt::Result {
    let u: u32 = v.into();
    let faults: Vec<String> = faults
        .iter()
        .filter(|(i, _)| u & (1 << i) != 0)
        .map(|(_, v)| v.to_string())
        .collect();

    let size = std::mem::size_of::<T>() * 8;
    if faults.is_empty() {
        write!(f, "{v:0width$b} ()", width = size)
    } else {
        write!(f, "{v:0width$b} {}", faults.join(" | "), width = size)
    }
}

fn format_faults_desc<T: Into<u32> + std::fmt::Binary + Copy>(
    v: T,
    faults: &[(u32, &'static str, &'static str)],
    f: &mut Formatter,
) -> std::fmt::Result {
    let faults: Vec<_> = faults.iter().map(|(a, b, _)| (*a, *b)).collect();
    format_faults(v, &faults, f)
}

/// Uptime (0.1 sec / LSB)
#[derive(Debug, FromBytes)]
struct Uptime(U32);
impl Display for Uptime {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{} sec", self.0.get() as f32 * 0.1)
    }
}

#[derive(Debug, FromBytes)]
struct ControllerFaultGen2(U32);
impl Display for ControllerFaultGen2 {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        format_faults(
            self.0.get(),
            &[
                (20, "open pin"),
                (19, "MCU fault"),
                (8, "start-up fault"),
                (7, "controller OT fault"),
                (6, "PLL fault"),
                (5, "PLL fault"),
                (4, "oscillator fault"),
                (2, "VCCS UV fault"),
                (1, "VCC UV fault"),
                (0, "VCC UV fault"),
            ],
            f,
        )
    }
}

#[derive(Debug, FromBytes)]
struct RailFault(U32);
impl Display for RailFault {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        format_faults(
            self.0.get(),
            &[
                (21, "Input Supply OC fault"),
                (20, "VCC Under VOUT fault"),
                (19, "SPS fault"),
                (18, "VOUT OV fault"),
                (17, "VOUT UV fault"),
                (15, "Slow Sum OC fault"),
                (14, "Fast Sum OC fault"),
                (10, "Input OC fault"),
                (9, "UT warning"),
                (8, "UT fault"),
                (7, "OT warning"),
                (6, "OT fault"),
                (5, "Input OC warning"),
                (3, "VIN UV warning"),
                (2, "VIN OV warning"),
                (1, "VIN UV fault"),
                (0, "VIN OV fault"),
            ],
            f,
        )
    }
}

#[derive(Debug, FromBytes)]
struct PhaseFaultA(U32);
impl Display for PhaseFaultA {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let v = self.0.get();
        let mut faults = vec![];
        for i in 0..12 {
            if v & (1 << i) != 0 {
                faults.push("phase {i} OC fault");
            }
            if v & (1 << (i + 12)) != 0 {
                faults.push("phase {i} UC fault");
            }
        }
        write!(f, "{}", faults.join(" | "))
    }
}
#[derive(Debug, FromBytes)]
struct PhaseFaultB(U32);
impl Display for PhaseFaultB {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let v = self.0.get();
        let mut faults = vec![];
        for i in 0..12 {
            if v & (1 << (i + 4)) != 0 {
                faults.push("phase {i} ADC UC fault");
            }
            if v & (1 << (i + 16)) != 0 {
                faults.push("phase {i} ADC OC fault");
            }
        }
        write!(f, "{}", faults.join(" | "))
    }
}

#[derive(Debug, FromBytes)]
struct AdcFault(U16);
impl Display for AdcFault {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        format_faults(
            self.0.get(),
            &[
                (7, "telemetry ADC error"),
                (6, "ISEN ADC error"),
                (5, "VSEN ADC error"),
            ],
            f,
        )
    }
}

#[derive(Debug, FromBytes)]
struct StatusWord(U16);
impl Display for StatusWord {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        format_faults(
            self.0.get(),
            &[
                (15, "VOUT"),
                (14, "IOUT"),
                (13, "INPUT"),
                (12, "MFR_SPECIFIC"),
                (11, "POWER_GOOD#"),
                (8, "unknown"),
                (7, "busy"),
                (6, "off"),
                (5, "VOUT_OV_FAULT"),
                (4, "IOUT_OC_FAULT"),
                (3, "VIN_UV_FAULT"),
                (2, "TEMPERATURE"),
                (1, "CML"),
                (0, "none of the above"),
            ],
            f,
        )
    }
}

#[derive(Debug, FromBytes)]
struct StatusCml(u8);
impl Display for StatusCml {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        format_faults_desc(
            self.0,
            &[
                (15, "VOUT", "An output voltage fault or warning has occurred."),
                (14, "IOUT", "An output current fault has occurred."),
                (13, "INPUT", "An input voltage fault or warning has occurred."),
                (12, "MFR_SPECIFIC", "A manufacturer specific fault or warning has occurred."),
                (11, "POWER_GOOD#", "The POWER_GOOD signal is negated."),
                (8, "Unknown", "A fault other than those described in bits [15:9] has occurred."),
                (7, "Busy", "Device busy and unable to respond."),
                (6, "OFF", "This bit is asserted if the unit is not providing power to the output, regardless of the reason, including simply not being enabled"),
                (5, "VOUT_OV_FAULT", "An output overvoltage fault has occurred."),
                (4, "IOUT_OC_FAULT", "An output overcurrent fault has occurred."),
                (3, "VIN_UV_FAULT", "An input undervoltage fault has occurred."),
                (2, "TEMPERATURE", "A temperature fault or warning has occurred."),
                (1, "CML", "A communications, memory, or logic fault has occurred."),
                (0, "None of the Above", "A status change other than those listed above has occurred."),
            ],
            f,
        )
    }
}

#[derive(Debug, FromBytes)]
struct StatusMfr(u8);
impl Display for StatusMfr {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        format_faults_desc(
            self.0,
            &[
                (7, "ADCUNLOCK", "Invalid or unsupported PMBus Command was received."),
                (6, "PSYS, IIN Sense", "A PSYS and/or sensed IIN OC warning has occurred."),
                (5, "CFP Fault", "A CFP fault has occurred."),
                (4, "Internal Temperature Fault", "The controller internal temp has exceeded 130°C."),
                (3, "BBEVENT", "A Black Box event occurred."),
                (2, "LMSEVENT", "A Last Man Standing event has occurred."),
                (1, "SPSFault", "An SPS overcurrent and/or over-temperature event has occurred."),
                (0, "SVIDERROR", "Error on SVID/SVI2 interface"),
            ],
            f,
        )
    }
}

#[derive(Debug, FromBytes)]
struct StatusVout(u8);
impl Display for StatusVout {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        format_faults_desc(
            self.0,
            &[
                (
                    7,
                    "VOUT_OV_FAULT",
                    "Indicates an output overvoltage fault has occurred.",
                ),
                (
                    4,
                    "VOUT_UV_FAULT",
                    "Indicates an output undervoltage fault has occurred.",
                ),
                (
                    3,
                    "VOUT_MAX Warning",
                    "Indicates an output voltage maximum warning has occurred.",
                ),
            ],
            f,
        )
    }
}

#[derive(Debug, FromBytes)]
struct StatusIout(u8);
impl Display for StatusIout {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        format_faults_desc(
            self.0,
            &[
                (
                    7,
                    "IOUT_OC_FAULT",
                    "An output overcurrent fault has occurred.",
                ),
                (
                    5,
                    "IOUT_OC_WARN",
                    "An output overcurrent warning has occurred.",
                ),
                (
                    4,
                    "IOUT_UC_FAULT",
                    "An output undercurrent fault has occurred.",
                ),
                (
                    3,
                    "Current Share Fault",
                    "A current share fault has occurred.",
                ),
            ],
            f,
        )
    }
}

#[derive(Debug, FromBytes)]
struct StatusTemperature(u8);
impl Display for StatusTemperature {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        format_faults_desc(
            self.0,
            &[
                (7, "OT_FAULT", "An over-temperature fault has occurred"),
                (6, "OT_WARN", "An over-temperature warning has occurred"),
                (4, "UT_FAULT", "An under-temperature fault has occurred"),
            ],
            f,
        )
    }
}

#[derive(Debug, FromBytes)]
struct StatusInput(u8);
impl Display for StatusInput {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        format_faults_desc(
            self.0,
            &[
                (7, "VIN_OV_FAULT", "An input overvoltage fault has occurred."),
                (6, "VIN_OV_WARN", "An input overvoltage warning has occurred."),
                (5, "VIN_UV_WARN", "An input undervoltage warning has occurred."),
                (4, "VIN_UV_FAULT", "An input undervoltage fault has occurred."),
                (3, "VIN_ON/OFF", "Disabled due to insufficient input voltage. This could be VIN or VMON."),
                (2, "IIN_OC_FAULT", "An input overcurrent fault has occurred."),
                (1, "IIN_OC_WARN", "An input overcurrent warning has occurred."),
                (0, "Not Supported", "Not supported")],
            f,
        )
    }
}

/// Temperature (direct format 2°C / LSB)
#[derive(Debug, FromBytes)]
struct Temperature(u8);
impl Display for Temperature {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{} °C", self.0 as f32 * 2.0)
    }
}

/// Voltage (direct format 10 mV / LSB)
#[derive(Debug, FromBytes)]
struct VoltageIn(I16);
impl Display for VoltageIn {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{} V", self.0.get() as f32 * 0.01)
    }
}

/// Voltage (direct format 1 mV / LSB)
#[derive(Debug, FromBytes)]
struct VoltageOut(I16);
impl Display for VoltageOut {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{} V", self.0.get() as f32 * 0.001)
    }
}

/// Current (direct format 0.1A / LSB)
#[derive(Debug, FromBytes)]
struct CurrentOut(I16);
impl Display for CurrentOut {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{} A", self.0.get() as f32 * 0.1)
    }
}

/// Current (direct format 10 mA / LSB)
#[derive(Debug, FromBytes)]
struct CurrentIn(I16);
impl Display for CurrentIn {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{} A", self.0.get() as f32 * 0.01)
    }
}

#[derive(Debug, FromBytes)]
struct PhaseFault(U32);
impl Display for PhaseFault {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let v = self.0.get();
        let mut faults = vec![];
        for i in 0..20 {
            if v & (1 << i) != 0 {
                faults.push(i.to_string())
            }
        }
        if faults.is_empty() {
            write!(f, "{v:032b} ()")
        } else {
            write!(f, "{v:032b} {}", faults.join(" | "))
        }
    }
}

#[derive(Debug, FromBytes)]
#[repr(C)]
pub struct BlackboxRamGen2 {
    rail0_uptime: Uptime,
    rail1_uptime: Uptime,
    rail2_uptime: Uptime,
    controller_fault: ControllerFaultGen2,
    rail0_fault: RailFault,
    rail1_fault: RailFault,
    rail2_fault: RailFault,
    phase_fault_a: PhaseFaultA,
    phase_fault_b: PhaseFaultB,
    _reserved: U32,
    adc_fault: AdcFault,
    rail0_status: StatusWord,
    rail1_status: StatusWord,
    rail2_status: StatusWord,
    status_cml: StatusCml,
    status_mfr: StatusMfr,
    rail0_status_vout: StatusVout,
    rail1_status_vout: StatusVout,
    rail2_status_vout: StatusVout,
    rail0_status_iout: StatusIout,
    rail1_status_iout: StatusIout,
    rail2_status_iout: StatusIout,
    rail0_status_temperature: StatusTemperature,
    rail1_status_temperature: StatusTemperature,
    rail2_status_temperature: StatusTemperature,
    rail0_status_input: StatusInput,
    rail1_status_input: StatusInput,
    rail2_status_input: StatusInput,
    rail0_read_vin: VoltageIn,
    rail1_read_vin: VoltageIn,
    rail2_read_vin: VoltageIn,
    rail0_read_vout: VoltageOut,
    rail1_read_vout: VoltageOut,
    rail2_read_vout: VoltageOut,
    rail0_read_iin: CurrentIn,
    rail1_read_iin: CurrentIn,
    rail2_read_iin: CurrentIn,
    rail0_read_iout: CurrentOut,
    rail1_read_iout: CurrentOut,
    rail2_read_iout: CurrentOut,
    rail0_read_temperature: Temperature,
    rail1_read_temperature: Temperature,
    rail2_read_temperature: Temperature,
    controller_read_temperature: Temperature,
    _reserved2: U16,
    vmon: VoltageIn,
    _reserved3: U16,
    vinsen: VoltageIn,
    _reserved4: U16,
    phase0_temperature: Temperature,
    phase1_temperature: Temperature,
    phase2_temperature: Temperature,
    phase3_temperature: Temperature,
    phase4_temperature: Temperature,
    phase5_temperature: Temperature,
    phase6_temperature: Temperature,
    phase7_temperature: Temperature,
    phase8_temperature: Temperature,
    phase9_temperature: Temperature,
    phase10_temperature: Temperature,
    phase11_temperature: Temperature,

    phase0_current: CurrentOut,
    phase1_current: CurrentOut,
    phase2_current: CurrentOut,
    phase3_current: CurrentOut,
    phase4_current: CurrentOut,
    phase5_current: CurrentOut,
    phase6_current: CurrentOut,
    phase7_current: CurrentOut,
    phase8_current: CurrentOut,
    phase9_current: CurrentOut,
    phase10_current: CurrentOut,
    phase11_current: CurrentOut,

    _reserved5: [u32; 4],
}

impl Display for BlackboxRamGen2 {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        writeln!(f, "rail0 uptime: {}", self.rail0_uptime)?;
        writeln!(f, "rail1 uptime: {}", self.rail1_uptime)?;
        writeln!(f, "rail2 uptime: {}", self.rail2_uptime)?;
        writeln!(f, "controller fault: {}", self.controller_fault)?;
        writeln!(f, "rail0 fault: {}", self.rail0_fault)?;
        writeln!(f, "rail1 fault: {}", self.rail1_fault)?;
        writeln!(f, "rail2 fault: {}", self.rail2_fault)?;
        writeln!(f, "phase fault a: {}", self.phase_fault_a)?;
        writeln!(f, "phase fault b: {}", self.phase_fault_b)?;
        writeln!(f, "adc fault: {}", self.adc_fault)?;
        writeln!(f, "rail0 status: {}", self.rail0_status)?;
        writeln!(f, "rail1 status: {}", self.rail1_status)?;
        writeln!(f, "rail2 status: {}", self.rail2_status)?;
        writeln!(f, "status cml: {}", self.status_cml)?;
        writeln!(f, "status mfr: {}", self.status_mfr)?;
        writeln!(f, "rail0 status vout: {}", self.rail0_status_vout)?;
        writeln!(f, "rail1 status vout: {}", self.rail1_status_vout)?;
        writeln!(f, "rail2 status vout: {}", self.rail2_status_vout)?;
        writeln!(f, "rail0 status iout: {}", self.rail0_status_iout)?;
        writeln!(f, "rail1 status iout: {}", self.rail1_status_iout)?;
        writeln!(f, "rail2 status iout: {}", self.rail2_status_iout)?;
        writeln!(
            f,
            "rail0 status temperature: {}",
            self.rail0_status_temperature
        )?;
        writeln!(
            f,
            "rail1 status temperature: {}",
            self.rail1_status_temperature
        )?;
        writeln!(
            f,
            "rail2 status temperature: {}",
            self.rail2_status_temperature
        )?;
        writeln!(f, "rail0 status input: {}", self.rail0_status_input)?;
        writeln!(f, "rail1 status input: {}", self.rail1_status_input)?;
        writeln!(f, "rail2 status input: {}", self.rail2_status_input)?;
        writeln!(f, "rail0 read vin: {}", self.rail0_read_vin)?;
        writeln!(f, "rail1 read vin: {}", self.rail1_read_vin)?;
        writeln!(f, "rail2 read vin: {}", self.rail2_read_vin)?;
        writeln!(f, "rail0 read vout: {}", self.rail0_read_vout)?;
        writeln!(f, "rail1 read vout: {}", self.rail1_read_vout)?;
        writeln!(f, "rail2 read vout: {}", self.rail2_read_vout)?;
        writeln!(f, "rail0 read iin: {}", self.rail0_read_iin)?;
        writeln!(f, "rail1 read iin: {}", self.rail1_read_iin)?;
        writeln!(f, "rail2 read iin: {}", self.rail2_read_iin)?;
        writeln!(f, "rail0 read iout: {}", self.rail0_read_iout)?;
        writeln!(f, "rail1 read iout: {}", self.rail1_read_iout)?;
        writeln!(f, "rail2 read iout: {}", self.rail2_read_iout)?;
        writeln!(f, "rail0 read temperature: {}", self.rail0_read_temperature)?;
        writeln!(f, "rail1 read temperature: {}", self.rail1_read_temperature)?;
        writeln!(f, "rail2 read temperature: {}", self.rail2_read_temperature)?;
        writeln!(
            f,
            "controller read temperature: {}",
            self.controller_read_temperature
        )?;
        writeln!(f, "vmon: {}", self.vmon)?;
        writeln!(f, "vinsen: {}", self.vinsen)?;
        writeln!(f, "phase0 temperature: {}", self.phase0_temperature)?;
        writeln!(f, "phase1 temperature: {}", self.phase1_temperature)?;
        writeln!(f, "phase2 temperature: {}", self.phase2_temperature)?;
        writeln!(f, "phase3 temperature: {}", self.phase3_temperature)?;
        writeln!(f, "phase4 temperature: {}", self.phase4_temperature)?;
        writeln!(f, "phase5 temperature: {}", self.phase5_temperature)?;
        writeln!(f, "phase6 temperature: {}", self.phase6_temperature)?;
        writeln!(f, "phase7 temperature: {}", self.phase7_temperature)?;
        writeln!(f, "phase8 temperature: {}", self.phase8_temperature)?;
        writeln!(f, "phase9 temperature: {}", self.phase9_temperature)?;
        writeln!(f, "phase10 temperature: {}", self.phase10_temperature)?;
        writeln!(f, "phase11 temperature: {}", self.phase11_temperature)?;

        writeln!(f, "phase0 current: {}", self.phase0_current)?;
        writeln!(f, "phase1 current: {}", self.phase1_current)?;
        writeln!(f, "phase2 current: {}", self.phase2_current)?;
        writeln!(f, "phase3 current: {}", self.phase3_current)?;
        writeln!(f, "phase4 current: {}", self.phase4_current)?;
        writeln!(f, "phase5 current: {}", self.phase5_current)?;
        writeln!(f, "phase6 current: {}", self.phase6_current)?;
        writeln!(f, "phase7 current: {}", self.phase7_current)?;
        writeln!(f, "phase8 current: {}", self.phase8_current)?;
        writeln!(f, "phase9 current: {}", self.phase9_current)?;
        writeln!(f, "phase10 current: {}", self.phase10_current)?;
        writeln!(f, "phase11 current: {}", self.phase11_current)?;
        Ok(())
    }
}

#[derive(Debug, FromBytes)]
struct ControllerFaultGen2p5(U32);
impl Display for ControllerFaultGen2p5 {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let v = self.0.get();
        let faults = [
            (23, "open pin"),
            (12, "MCU fault"),
            (9, "start-up fault"),
            (7, "controller OT fault"),
            (6, "PLL fault"),
            (5, "PLL fault"),
            (4, "oscillator fault"),
            (2, "VCCS UV fault"),
            (1, "VCC UV fault"),
            (0, "VCC UV fault"),
        ];
        let faults: Vec<String> = faults
            .iter()
            .filter(|(i, _)| v & (1 << i) != 0)
            .map(|(_, v)| v.to_string())
            .collect();
        if faults.is_empty() {
            write!(f, "0")
        } else {
            write!(f, "{}", faults.join(" | "))
        }
    }
}

#[derive(Debug, FromBytes)]
#[repr(C)]
struct BlackboxRamGen2p5 {
    rail0_uptime: Uptime,
    rail1_uptime: Uptime,
    controller_fault: ControllerFaultGen2p5,
    rail0_fault: RailFault,
    rail1_fault: RailFault,
    phase_fault_uc: PhaseFault,
    phase_fault_oc: PhaseFault,
    adc_fault_uc: PhaseFault,
    adc_fault_oc: PhaseFault,
    _reserved: U32,
    _reserved2: U32,
    _reserved3: U32,
    rail0_status: StatusWord,
    rail1_status: StatusWord,
    _reserved4: U16,
    status_cml: StatusCml,
    status_mfr: StatusMfr,
    rail1_status_vout: StatusVout,
    rail0_status_vout: StatusVout,
    rail1_status_iout: StatusIout,
    rail0_status_iout: StatusIout,
    rail1_status_temperature: StatusTemperature,
    rail0_status_temperature: StatusTemperature,
    rail1_status_input: StatusInput,
    rail0_status_input: StatusInput,
    rail1_read_vin: VoltageIn,
    rail0_read_vin: VoltageIn,
    rail1_read_vout: VoltageOut,
    rail0_read_vout: VoltageOut,
    rail1_read_iin: CurrentIn,
    rail0_read_iin: CurrentIn,
    rail1_read_iout: CurrentOut,
    rail0_read_iout: CurrentOut,
    _reserved5: u8,
    controller_read_temperature: Temperature,
    rail1_read_temperature: Temperature,
    rail0_read_temperature: Temperature,
    _reserved6: [U32; 2],
    phase0_temperature: Temperature,
    phase1_temperature: Temperature,
    phase2_temperature: Temperature,
    phase3_temperature: Temperature,
    phase4_temperature: Temperature,
    phase5_temperature: Temperature,
    phase6_temperature: Temperature,
    phase7_temperature: Temperature,
    phase8_temperature: Temperature,
    phase9_temperature: Temperature,
    phase10_temperature: Temperature,
    phase11_temperature: Temperature,
    phase12_temperature: Temperature,
    phase13_temperature: Temperature,
    phase14_temperature: Temperature,
    phase15_temperature: Temperature,
    phase16_temperature: Temperature,
    phase17_temperature: Temperature,
    phase18_temperature: Temperature,
    phase19_temperature: Temperature,

    phase0_current: CurrentOut,
    phase1_current: CurrentOut,
    phase2_current: CurrentOut,
    phase3_current: CurrentOut,
    phase4_current: CurrentOut,
    phase5_current: CurrentOut,
    phase6_current: CurrentOut,
    phase7_current: CurrentOut,
    phase8_current: CurrentOut,
    phase9_current: CurrentOut,
    phase10_current: CurrentOut,
    phase11_current: CurrentOut,
    phase12_current: CurrentOut,
    phase13_current: CurrentOut,
    phase14_current: CurrentOut,
    phase15_current: CurrentOut,
    phase16_current: CurrentOut,
    phase17_current: CurrentOut,
    phase18_current: CurrentOut,
    phase19_current: CurrentOut,

    _reserved7: [u32; 6],
}
impl Display for BlackboxRamGen2p5 {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        writeln!(f, "rail0 uptime: {}", self.rail0_uptime)?;
        writeln!(f, "rail1 uptime: {}", self.rail1_uptime)?;
        writeln!(f, "controller fault: {}", self.controller_fault)?;
        writeln!(f, "rail0 fault: {}", self.rail0_fault)?;
        writeln!(f, "rail1 fault: {}", self.rail1_fault)?;
        writeln!(f, "phase fault uc: {}", self.phase_fault_uc)?;
        writeln!(f, "phase fault oc: {}", self.phase_fault_oc)?;
        writeln!(f, "adc fault uc: {}", self.adc_fault_uc)?;
        writeln!(f, "adc fault oc: {}", self.adc_fault_oc)?;
        writeln!(f, "rail0 status: {}", self.rail0_status)?;
        writeln!(f, "rail1 status: {}", self.rail1_status)?;
        writeln!(f, "status cml: {}", self.status_cml)?;
        writeln!(f, "status mfr: {}", self.status_mfr)?;
        writeln!(f, "rail1 status vout: {}", self.rail1_status_vout)?;
        writeln!(f, "rail0 status vout: {}", self.rail0_status_vout)?;
        writeln!(f, "rail1 status iout: {}", self.rail1_status_iout)?;
        writeln!(f, "rail0 status iout: {}", self.rail0_status_iout)?;
        writeln!(
            f,
            "rail1 status temperature: {}",
            self.rail1_status_temperature
        )?;
        writeln!(
            f,
            "rail0 status temperature: {}",
            self.rail0_status_temperature
        )?;
        writeln!(f, "rail1 status input: {}", self.rail1_status_input)?;
        writeln!(f, "rail0 status input: {}", self.rail0_status_input)?;
        writeln!(f, "rail1 read vin: {}", self.rail1_read_vin)?;
        writeln!(f, "rail0 read vin: {}", self.rail0_read_vin)?;
        writeln!(f, "rail1 read vout: {}", self.rail1_read_vout)?;
        writeln!(f, "rail0 read vout: {}", self.rail0_read_vout)?;
        writeln!(f, "rail1 read iin: {}", self.rail1_read_iin)?;
        writeln!(f, "rail0 read iin: {}", self.rail0_read_iin)?;
        writeln!(f, "rail1 read iout: {}", self.rail1_read_iout)?;
        writeln!(f, "rail0 read iout: {}", self.rail0_read_iout)?;
        writeln!(
            f,
            "controller read temperature: {}",
            self.controller_read_temperature
        )?;
        writeln!(f, "rail1 read temperature: {}", self.rail1_read_temperature)?;
        writeln!(f, "rail0 read temperature: {}", self.rail0_read_temperature)?;
        writeln!(f, "phase0 temperature: {}", self.phase0_temperature)?;
        writeln!(f, "phase1 temperature: {}", self.phase1_temperature)?;
        writeln!(f, "phase2 temperature: {}", self.phase2_temperature)?;
        writeln!(f, "phase3 temperature: {}", self.phase3_temperature)?;
        writeln!(f, "phase4 temperature: {}", self.phase4_temperature)?;
        writeln!(f, "phase5 temperature: {}", self.phase5_temperature)?;
        writeln!(f, "phase6 temperature: {}", self.phase6_temperature)?;
        writeln!(f, "phase7 temperature: {}", self.phase7_temperature)?;
        writeln!(f, "phase8 temperature: {}", self.phase8_temperature)?;
        writeln!(f, "phase9 temperature: {}", self.phase9_temperature)?;
        writeln!(f, "phase10 temperature: {}", self.phase10_temperature)?;
        writeln!(f, "phase11 temperature: {}", self.phase11_temperature)?;
        writeln!(f, "phase12 temperature: {}", self.phase12_temperature)?;
        writeln!(f, "phase13 temperature: {}", self.phase13_temperature)?;
        writeln!(f, "phase14 temperature: {}", self.phase14_temperature)?;
        writeln!(f, "phase15 temperature: {}", self.phase15_temperature)?;
        writeln!(f, "phase16 temperature: {}", self.phase16_temperature)?;
        writeln!(f, "phase17 temperature: {}", self.phase17_temperature)?;
        writeln!(f, "phase18 temperature: {}", self.phase18_temperature)?;
        writeln!(f, "phase19 temperature: {}", self.phase19_temperature)?;

        writeln!(f, "phase0 current: {}", self.phase0_current)?;
        writeln!(f, "phase1 current: {}", self.phase1_current)?;
        writeln!(f, "phase2 current: {}", self.phase2_current)?;
        writeln!(f, "phase3 current: {}", self.phase3_current)?;
        writeln!(f, "phase4 current: {}", self.phase4_current)?;
        writeln!(f, "phase5 current: {}", self.phase5_current)?;
        writeln!(f, "phase6 current: {}", self.phase6_current)?;
        writeln!(f, "phase7 current: {}", self.phase7_current)?;
        writeln!(f, "phase8 current: {}", self.phase8_current)?;
        writeln!(f, "phase9 current: {}", self.phase9_current)?;
        writeln!(f, "phase10 current: {}", self.phase10_current)?;
        writeln!(f, "phase11 current: {}", self.phase11_current)?;
        writeln!(f, "phase12 current: {}", self.phase12_current)?;
        writeln!(f, "phase13 current: {}", self.phase13_current)?;
        writeln!(f, "phase14 current: {}", self.phase14_current)?;
        writeln!(f, "phase15 current: {}", self.phase15_current)?;
        writeln!(f, "phase16 current: {}", self.phase16_current)?;
        writeln!(f, "phase17 current: {}", self.phase17_current)?;
        writeln!(f, "phase18 current: {}", self.phase18_current)?;
        writeln!(f, "phase19 current: {}", self.phase19_current)?;
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use zerocopy::FromBytes;

    #[test]
    fn test_blackbox_size() {
        assert_eq!(std::mem::size_of::<BlackboxRamGen2>(), 4 * 38);
        assert_eq!(std::mem::size_of::<BlackboxRamGen2p5>(), 4 * 44);
    }

    #[test]
    fn test_blackbox_load() {
        let d: [u8; 176] = [
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x18, 0x41, 0x98, 0x01, 0x00, 0x00, 0x00,
            0x0A, 0x00, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x04, 0xAE,
            0x04, 0xAE, 0x00, 0x00, 0x01, 0x1D, 0x00, 0x00, 0x00, 0x73, 0x00,
            0x00, 0x01, 0x80, 0x00, 0x0C, 0x0C, 0x0C, 0x00, 0x00, 0x00, 0x06,
            0x04, 0xB0, 0x00, 0x00, 0x0C, 0x0C, 0x0C, 0x0C, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x0C, 0x0C, 0x0C, 0x0C, 0x0C, 0x0C,
            0x0C, 0x0C, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x2D, 0x00, 0x32, 0x00, 0x30, 0x00,
            0x32, 0x00, 0x30, 0x00, 0x2C, 0x00, 0x2A, 0x00, 0x30, 0x80, 0x01,
            0x15, 0xDA, 0x00, 0x00, 0x00, 0x88, 0x00, 0x15, 0xDB, 0x08, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xEC, 0x83, 0xED, 0x2E,
        ];
        let bb = BlackboxRamGen2p5::read_from(d.as_slice()).unwrap();
        println!("{bb}");
    }
}
