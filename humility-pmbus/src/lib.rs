// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::{Result, anyhow};
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
    let array: humility::reflect::Array =
        humility::reflect::load_variable(hubris, core, var)?;

    // Destructure CONTROLLER_CONFIG to build a map of voltage sensor IDs
    // (which we can easily compute) to index in the CONTROLLER_CONFIG
    // array.
    let mut sensor_id_to_index = BTreeMap::new();
    for (i, cfg) in array.iter().enumerate() {
        let sensor_id: u32 = cfg.field("voltage.0")?;
        sensor_id_to_index.insert(sensor_id, i);
    }
    Ok(sensor_id_to_index)
}
