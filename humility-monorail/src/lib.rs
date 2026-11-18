// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! Library to handle common monorail functions
//!
//! Status of the management network switch and PHYs.
//!
//! "Monorail... monorail... monorail..."
#![warn(missing_docs)]

use humility::core::Core;
use humility::hubris::HubrisArchive;
use humility_hiffy::HiffyContext;
use humility_idol::{HubrisIdol, IdolDecodeError, IdolError};
use std::collections::BTreeMap;

/// Port speed
#[derive(Copy, Clone)]
pub enum Speed {
    /// 100M
    Speed100M,
    /// 1G
    Speed1G,
    /// 10G
    Speed10G,
}

impl std::fmt::Display for Speed {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Speed::Speed100M => write!(f, "100M"),
            Speed::Speed1G => write!(f, "1G"),
            Speed::Speed10G => write!(f, "10G"),
        }
    }
}

impl humility::reflect::Load for Speed {
    fn from_value(v: &humility::reflect::Value) -> anyhow::Result<Self> {
        let s = humility::reflect::Enum::from_value(v)?;
        let e = s.disc();
        match e {
            "Speed100M" => Ok(Self::Speed100M),
            "Speed1G" => Ok(Self::Speed1G),
            "Speed10G" => Ok(Self::Speed10G),
            _ => Err(anyhow::anyhow!("Unknown speed {e}")),
        }
    }
}

/// Port mode
pub enum Mode {
    /// SFI
    Sfi,
    /// 10GBASE-KR
    BaseKr,
    /// SGMII
    Sgmii(Speed),
    /// QSGMII
    Qsgmii(Speed),
}

impl Mode {}

impl std::fmt::Display for Mode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Mode::Sfi => write!(f, "SFI"),
            Mode::BaseKr => write!(f, "BASEKR"),
            Mode::Sgmii(_) => write!(f, "SGMII"),
            Mode::Qsgmii(_) => write!(f, "QSGMII"),
        }
    }
}

impl humility::reflect::Load for Mode {
    fn from_value(v: &humility::reflect::Value) -> anyhow::Result<Self> {
        let s = humility::reflect::Enum::from_value(v)?;
        let e = s.disc();
        match e {
            "Sfi" => Ok(Self::Sfi),
            "BaseKr" => Ok(Self::BaseKr),
            "Sgmii" => {
                let v = s.contents().ok_or(anyhow::anyhow!("bad speed"))?;
                let (speed,) = <(Speed,)>::from_value(v)?;
                Ok(Self::Sgmii(speed))
            }
            "Qsgmii" => {
                let v = s.contents().ok_or(anyhow::anyhow!("bad speed"))?;
                let (speed,) = <(Speed,)>::from_value(v)?;
                Ok(Self::Qsgmii(speed))
            }
            _ => Err(anyhow::anyhow!("unknown mode {e}")),
        }
    }
}

/// Port device types within the VSC7448
pub enum Dev {
    /// 1G device
    Dev1g,
    /// 2.5G device
    Dev2g5,
    /// 10G device
    Dev10g,
}

impl std::fmt::Display for Dev {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Dev::Dev1g => write!(f, "1G"),
            Dev::Dev2g5 => write!(f, "2G5"),
            Dev::Dev10g => write!(f, "10G"),
        }
    }
}

impl humility::reflect::Load for Dev {
    fn from_value(v: &humility::reflect::Value) -> anyhow::Result<Self> {
        let s = humility::reflect::Enum::from_value(v)?;
        match s.disc() {
            "Dev1g" => Ok(Self::Dev1g),
            "Dev2g5" => Ok(Self::Dev2g5),
            "Dev10g" => Ok(Self::Dev10g),
            e => Err(anyhow::anyhow!("unknown dev {e}")),
        }
    }
}

/// SERDES types in the VSC7448
pub enum Serdes {
    /// 1G
    Serdes1g,
    /// 6G
    Serdes6g,
    /// 10G
    Serdes10g,
}

impl std::fmt::Display for Serdes {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Serdes::Serdes1g => write!(f, "1G"),
            Serdes::Serdes6g => write!(f, "6G"),
            Serdes::Serdes10g => write!(f, "10G"),
        }
    }
}

impl humility::reflect::Load for Serdes {
    fn from_value(v: &humility::reflect::Value) -> anyhow::Result<Self> {
        let s = humility::reflect::Enum::from_value(v)?;
        let e = s.disc();
        match e {
            "Serdes1g" => Ok(Self::Serdes1g),
            "Serdes6g" => Ok(Self::Serdes6g),
            "Serdes10g" => Ok(Self::Serdes10g),
            _ => Err(anyhow::anyhow!("unknown Serdes {e}")),
        }
    }
}

/// Link status
pub enum LinkStatus {
    /// Up
    Up,
    /// Down
    Down,
    /// Error
    Error,
}

impl humility::reflect::Load for LinkStatus {
    fn from_value(v: &humility::reflect::Value) -> anyhow::Result<Self> {
        let s = humility::reflect::Enum::from_value(v)?;
        let e = s.disc();
        match e {
            "Up" => Ok(Self::Up),
            "Down" => Ok(Self::Down),
            "Error" => Ok(Self::Error),
            _ => Err(anyhow::anyhow!("unknown link status {e}")),
        }
    }
}

/// PHY models which may be attached to the VSC7448
pub enum PhyType {
    /// VSC8504
    Vsc8504,
    /// VSC8522
    Vsc8522,
    /// VSC8552
    Vsc8552,
    /// VSC8562
    Vsc8562,
}

impl humility::reflect::Load for PhyType {
    fn from_value(v: &humility::reflect::Value) -> anyhow::Result<Self> {
        let s = humility::reflect::Enum::from_value(v)?;
        let e = s.disc();
        match e {
            "Vsc8504" => Ok(Self::Vsc8504),
            "Vsc8522" => Ok(Self::Vsc8522),
            "Vsc8552" => Ok(Self::Vsc8552),
            "Vsc8562" => Ok(Self::Vsc8562),
            _ => Err(anyhow::anyhow!("unknown phytype {e}")),
        }
    }
}

impl std::fmt::Display for PhyType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            PhyType::Vsc8504 => write!(f, "VSC8504"),
            PhyType::Vsc8522 => write!(f, "VSC8522"),
            PhyType::Vsc8552 => write!(f, "VSC8552"),
            PhyType::Vsc8562 => write!(f, "VSC8562"),
        }
    }
}

/// Device within the VSC7448
pub struct SingleDev {
    /// Device type
    pub dev: Dev,
    /// Device index
    ///
    /// This is unique within each device type
    pub num: u8,
}

impl std::fmt::Display for SingleDev {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "DEV{}_{}", self.dev, self.num)
    }
}

/// SERDES within the VSC7448
pub struct SingleSerdes {
    /// SERDES type
    pub serdes: Serdes,
    /// SERDES index
    ///
    /// This is unique within each SERDES type
    pub num: u8,
}

impl std::fmt::Display for SingleSerdes {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "DEV{}_{}", self.serdes, self.num)
    }
}

/// Port config
pub struct PortConfig {
    /// Port mode
    pub mode: Mode,
    /// Physical device
    pub dev: SingleDev,
    /// SERDES used by that device
    pub serdes: SingleSerdes,
}

impl humility::reflect::Load for PortConfig {
    fn from_value(v: &humility::reflect::Value) -> anyhow::Result<Self> {
        let s = humility::reflect::Struct::from_value(v)?;
        let mode = Mode::from_value(s.get("mode")?)?;
        let (dev, num) = <(Dev, u8)>::from_value(s.get("dev")?)?;
        let dev = SingleDev { dev, num };
        let (serdes, num) = <(Serdes, u8)>::from_value(s.get("serdes")?)?;
        let serdes = SingleSerdes { serdes, num };

        Ok(Self { mode, dev, serdes })
    }
}

/// Port status
pub struct PortStatus {
    /// Port configuration
    pub cfg: PortConfig,
    /// Current link status
    pub link_up: LinkStatus,
}

impl humility::reflect::Load for PortStatus {
    fn from_value(v: &humility::reflect::Value) -> anyhow::Result<Self> {
        let s = humility::reflect::Struct::from_value(v)?;
        let cfg = PortConfig::from_value(s.get("cfg")?)?;
        let link_up = LinkStatus::from_value(s.get("link_up")?)?;
        Ok(Self { cfg, link_up })
    }
}

/// PHY status
pub struct PhyStatus {
    /// PHY type
    pub ty: PhyType,
    /// MAC link status
    pub mac_link_up: LinkStatus,
    /// Media link status
    pub media_link_up: LinkStatus,
}

impl humility::reflect::Load for PhyStatus {
    fn from_value(v: &humility::reflect::Value) -> anyhow::Result<Self> {
        let s = humility::reflect::Struct::from_value(v)?;
        let ty = PhyType::from_value(s.get("ty")?)?;
        let mac_link_up = LinkStatus::from_value(s.get("mac_link_up")?)?;
        let media_link_up = LinkStatus::from_value(s.get("media_link_up")?)?;
        Ok(PhyStatus { ty, mac_link_up, media_link_up })
    }
}

/// Represents whether or not a port is configured
pub enum PortConfiguration {
    /// Port is configured
    PortConfigured(PortStatus),
    /// Port is unconfigured in the VSC7448 configuration
    Unconfigured,
    /// Other Idol error
    Other(IdolDecodeError),
}

/// Representes whether or not an external PHY is configured
pub enum PhyConfiguration {
    /// PHY is configured
    PhyConfigured(PhyStatus),
    /// PHY is unconfigured in the VSC7448 configuration (or not present)
    Unconfigured,
    /// Other Idol error
    Other(IdolDecodeError),
}

/// Port + Phy
pub struct PortPhyStatus {
    /// Port status
    pub port: PortConfiguration,
    /// phy status
    pub phy: PhyConfiguration,
}

/// Return value of the overall monorail status where the key is the port number
pub type MonorailStatus = BTreeMap<u8, PortPhyStatus>;

/// Error values
#[derive(Debug, thiserror::Error)]
pub enum MonorailError {
    /// Failure from hiffy
    #[error("hiffy error")]
    Hiffy(#[source] anyhow::Error),
    /// Failure from Idol
    #[error("idol error")]
    Idol(#[source] anyhow::Error),
    /// Failure decoding Idol result
    #[error("idol decode")]
    IdolDecode(#[source] IdolDecodeError),
}

/// Returns the status of all monorail ports
pub fn monorail_status(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
) -> Result<MonorailStatus, MonorailError> {
    let mut map = BTreeMap::new();
    // We're going to get the status of *every* port, instead of doing any smart
    // filtering here; the time it takes to poll a single port is dwarfed by the
    // Hiffy call overhead, so no need to optimize.
    const NUM_PORTS: u8 = 53;
    let results = {
        let op_port = hubris
            .get_idol_command("Monorail.get_port_status")
            .map_err(MonorailError::Idol)?;
        let op_phy = hubris
            .get_idol_command("Monorail.get_phy_status")
            .map_err(MonorailError::Idol)?;
        use hif::*;
        let send =
            context.get_function("Send", 4).map_err(MonorailError::Idol)?;
        let mut ops = vec![];

        let label = Target(0);

        // We run two hiffy loops back to back, which have the same form:
        // - Monorail.get_port_status for every port
        // - Monorail.get_phy_status for every port
        //
        // They're placed back-to-back in Hiffy code, so we'll call the two
        // loops one after the other (i.e. values are not interleaved!)
        for op in [&op_port, &op_phy] {
            assert_eq!(op.args.unwrap().members.len(), 1); // Sanity-check!
            let ret_size = hubris.typesize(op.ok).unwrap() as u32;

            // Here we go!
            ops.push(Op::Push32(op.task.task())); // Task id
            ops.push(Op::Push16(op.code)); // opcode
            ops.push(Op::Push(0)); // port (payload)
            ops.push(Op::Push(NUM_PORTS)); // comparison target (dummy)
            ops.push(Op::Label(label));
            {
                ops.push(Op::Drop); // Drop comparison target
                ops.push(Op::Push(1)); // Payload length
                ops.push(Op::Push32(ret_size)); // Return size
                ops.push(Op::Call(send.id));
                ops.push(Op::DropN(2)); // Drop payload and return size
                ops.push(Op::Push(1)); // Increment by one
                ops.push(Op::Add); // port = port + 1
                ops.push(Op::Push(NUM_PORTS)); // Comparison target
                ops.push(Op::BranchGreaterThan(label)); // Jump to loop start
            }
            ops.push(Op::DropN(4)); // Cleanup
        }

        ops.push(Op::Done); // Finish

        let mut results = context
            .run(core, ops.as_slice(), None)
            .map_err(MonorailError::Hiffy)?;
        let phy_results = results.split_off(NUM_PORTS as usize);
        let port_results = results;
        assert_eq!(port_results.len(), NUM_PORTS as usize);
        assert_eq!(phy_results.len(), NUM_PORTS as usize);

        let port_results = port_results
            .into_iter()
            .map(move |r| op_port.decode::<PortStatus>(&r))
            .collect::<Vec<_>>();
        let phy_results = phy_results
            .into_iter()
            .map(move |r| op_phy.decode::<PhyStatus>(&r))
            .collect::<Vec<_>>();

        // Decode the port and phy status values into reflect::Value
        for (i, (port, phy)) in
            port_results.into_iter().zip(phy_results).enumerate()
        {
            let port = match port {
                Ok(p) => PortConfiguration::PortConfigured(p),
                Err(e) => match e {
                    IdolDecodeError::Idol(IdolError::Named(ref name)) => {
                        match name.as_str() {
                            "UnconfiguredPort" => {
                                PortConfiguration::Unconfigured
                            }
                            _ => PortConfiguration::Other(e),
                        }
                    }
                    _ => PortConfiguration::Other(e),
                },
            };
            let phy = match phy {
                Ok(p) => PhyConfiguration::PhyConfigured(p),
                Err(e) => match e {
                    IdolDecodeError::Idol(IdolError::Named(ref name)) => {
                        match name.as_str() {
                            "UnconfiguredPort" | "NoPhy" => {
                                PhyConfiguration::Unconfigured
                            }
                            _ => PhyConfiguration::Other(e),
                        }
                    }
                    _ => PhyConfiguration::Other(e),
                },
            };
            map.insert(i as u8, PortPhyStatus { port, phy });
        }
        map
    };

    Ok(results)
}
