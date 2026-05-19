use humility::hubris::{RawHubrisArchive, HubrisArchive};

fn main() {
    let hubris = std::env::var("HUMILITY_ARCHVE").unwrap();
    let probe = std::env::var("HUMILITY_PROBE").unwrap();

    let hubris = RawHubrisArchive::load(&hubris).unwrap();
    let hubris = HubrisArchive::load(hubris, None).unwrap();

    let core = &mut *humility_probes_core::attach_to_chip(
        &probe,
        Some("STM32H753ZITx"),
        None,
    )
    .unwrap();

    let results = humility_vpd_lib::vpd_list(&hubris, core, 10000).unwrap();

    for r in results {
        println!("index {} lock status {}", r.ndx, match r.locked {
            Ok(true) => "locked",
            Ok(false) => "unlocked",
            Err(_) => "error"
        });
    }
}
