// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::{Result, anyhow, bail};
use humility::{core::Core, hubris::*};
use std::collections::BTreeMap;

/// Returns a map from voltage `SensorId` to position in `CONTROLLER_CONFIG`
pub fn sensor_id_map(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
) -> Result<BTreeMap<u32, usize>> {
    // Read CONTROLLER_CONFIG from the core, which we'll use to compute
    // indices for rails when using raw PMBus functions.
    let (_name, var) = hubris
        .qualified_variables()
        .find(|&(n, _v)| n == "task_power::bsp::CONTROLLER_CONFIG")
        .ok_or_else(|| anyhow!("could not find CONTROLLER_CONFIG"))?;
    let mut buf: Vec<u8> = vec![0u8; var.size];
    core.halt()?;
    core.read_8(var.addr, buf.as_mut_slice())?;
    core.run()?;
    let controller_config = humility::reflect::load_value(
        hubris,
        &buf,
        hubris.lookup_type(var.goff)?,
        0,
    )?;

    // Destructure CONTROLLER_CONFIG to build a map of voltage sensor IDs
    // (which we can easily compute) to index in the CONTROLLER_CONFIG
    // array.
    use humility::reflect::{Base, Value};
    let Value::Array(array) = controller_config else {
        bail!("invalid shape for CONTROLLER_CONFIG: {controller_config:?}");
    };
    let mut sensor_id_to_index = BTreeMap::new();
    for (i, cfg) in array.iter().enumerate() {
        let Value::Struct(s) = cfg else {
            bail!("invalid shape for CONTROLLER_CONFIG[{i}]: {cfg:?}");
        };
        let v = &s["voltage"];
        let Value::Tuple(t) = v else {
            bail!("could not get 'voltage': {v:?}");
        };
        let Value::Base(b) = &t[0] else {
            bail!("could not get sensor ID from 'voltage': {v:?}");
        };
        let Base::U32(sensor_id) = b else {
            bail!("bad sensor id type: {b:?}");
        };
        sensor_id_to_index.insert(*sensor_id, i);
    }
    Ok(sensor_id_to_index)
}
