/// Demonstrating using humiltiy as a library with async traits
///
/// This ended up being non-obvious when this was used in another project.
/// This is designed to be a straightfoward example to play around with.
use humility::hubris::HubrisArchive;
use humility_idol::HubrisIdol;
use humility_probes_core::HubrisAttach;
use slog::Logger;
use std::sync::{Arc, Mutex};

#[derive(Debug, thiserror::Error)]
enum WrappedError {
    #[error("HUMILITY_ARCHIVE unset")]
    HumilityArchiveUnset(#[source] std::env::VarError),
    #[error("HUMILTIY_PROBE unset")]
    HumilityProbeUnset(#[source] std::env::VarError),
    #[error("Hubris archive load failed")]
    HubrisArchive(#[source] anyhow::Error),
    #[error("probe attach failed")]
    ProbeAttach(#[source] anyhow::Error),
}

struct MyWrapper {
    // This does not work! `HubrisArchive` cannot be `Sync` because
    // other struct members like addr2line/gimli are not `Sync`.
    // You can experiment to see the full error.
    //hubris: HubrisArchive,
    hubris: Arc<Mutex<HubrisArchive>>,
    log: Logger,
}

impl MyWrapper {
    fn new(log: &Logger) -> Result<Self, WrappedError> {
        let hubris = std::env::var("HUMILITY_ARCHIVE")
            .map_err(WrappedError::HumilityArchiveUnset)?;

        let hubris = HubrisArchive::load_from_path(&hubris, &log)
            .map_err(WrappedError::HubrisArchive)?;

        let log = log.new(slog::o!());

        Ok(Self { hubris: Arc::new(Mutex::new(hubris)), log })
    }
}

#[async_trait::async_trait]
trait MyTrait {
    async fn do_something(&self) -> Result<(), WrappedError>;
}

#[async_trait::async_trait]
impl MyTrait for MyWrapper {
    async fn do_something(&self) -> Result<(), WrappedError> {
        let probe = std::env::var("HUMILITY_PROBE")
            .map_err(WrappedError::HumilityProbeUnset)?;

        let hubris = self.hubris.lock().unwrap();
        let mut core = hubris
            .attach_probe(&probe, 8000, &self.log)
            .map_err(WrappedError::ProbeAttach)?;

        let get_op = hubris.get_idol_command("Sequencer.get_state").unwrap();

        let mut context = humility_hiffy::HiffyContext::new(
            &hubris,
            &mut core,
            std::time::Duration::from_millis(10000),
            &self.log,
        )
        .unwrap();

        let _state =
            context.call::<u8>(&mut core, &get_op, &[], None, None).unwrap();

        Ok(())
    }
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let log = humility::log::init(false);

    let wrapper = MyWrapper::new(&log).unwrap();

    wrapper.do_something().await.unwrap();

    Ok(())
}
