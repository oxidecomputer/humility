// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! "Hey can you grab a hubris dump"

use humility::hubris::HubrisArchive;
use humility_net::{ScopedV6Addr, attach_net};

fn main() {
    let log = humility_log::init(false);

    let hubris = std::env::var("HUMILITY_ARCHIVE").unwrap();
    let hubris = HubrisArchive::load_from_path(&hubris, &log).unwrap();

    let addr = std::env::var("HUMILITY_IP").unwrap();
    let addr: ScopedV6Addr = addr.parse().unwrap();

    let mut core =
        attach_net(addr, &hubris, std::time::Duration::from_secs(5), &log)
            .unwrap();

    let mut file = std::fs::OpenOptions::new()
        .write(true)
        .create_new(true)
        .open("my_dump.0")
        .unwrap();

    humility_dump::take_system_dump(
        &hubris, &mut core, &mut file, false, // just use UDPRPC
        false, // don't force an overwrite
        &log,
    )
    .unwrap();
}
