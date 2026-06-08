// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//
use humility::hubris::HubrisArchive;
use humility_probes_core::HubrisAttach;

fn main() {
    let hubris = std::env::var("HUMILITY_ARCHIVE").unwrap();
    let probe = std::env::var("HUMILITY_PROBE").unwrap();
    let log = humility::log::init(false);

    let hubris = HubrisArchive::load_from_path(&hubris, &log).unwrap();

    let core = &mut hubris.attach_probe(&probe, &log).unwrap();

    // Read the first VPD
    let target = humility_vpd_lib::VpdTarget::Device(0);

    let vpd = humility_vpd_lib::vpd_read(
        &hubris,
        core,
        target,
        std::time::Duration::from_millis(10000),
        &log,
    )
    .unwrap();

    let reader = match tlvc::TlvcReader::begin(&vpd[..]) {
        Ok(reader) => reader,
        Err(err) => {
            panic!("{:?}", err);
        }
    };

    let p = tlvc_text::dump(reader);
    tlvc_text::save(std::io::stdout(), &p).unwrap();
    println!();
}
