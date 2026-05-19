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

    // Read the first VPD
    let target = humility_vpd_lib::VpdTarget::Device(0);

    let vpd = humility_vpd_lib::vpd_read(&hubris, core, target, 10000).unwrap();

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
