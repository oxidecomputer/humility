// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use humility::hubris::HubrisArchive;

fn main() {
    let hubris = std::env::var("HUMILITY_ARCHIVE").unwrap();
    let probe = std::env::var("HUMILITY_PROBE").unwrap();

    let hubris = HubrisArchive::load_from_path(&hubris).unwrap();

    let core = &mut *humility_probes_core::attach_to_chip(
        &probe,
        Some("STM32H753ZITx"),
        None,
    )
    .unwrap();

    let results = humility_vpd_lib::vpd_list(
        &hubris,
        core,
        std::time::Duration::from_millis(10000),
    )
    .unwrap();

    for r in results {
        println!(
            "index {} lock status {}",
            r.ndx,
            match r.locked {
                Ok(true) => "locked",
                Ok(false) => "unlocked",
                Err(_) => "error",
            }
        );
    }
}
