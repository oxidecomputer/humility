// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use humility::core::Core;
use humility::hubris::HubrisArchive;
use humility_idol::HubrisIdol;
use humility_probes_core::HubrisAttach;

#[derive(Debug, PartialEq, Eq)]
struct FixYourParsing;

#[derive(Debug)]
enum PowerState {
    A2,
    A2PlusFans,
    A0,
    A0PlusHP,
    A0Thermtrip,
    A0Reset,
}

impl PowerState {
    fn to_string(&self) -> String {
        match self {
            PowerState::A2 => "A2".to_string(),
            PowerState::A2PlusFans => "A2PlusFans".to_string(),
            PowerState::A0 => "A0".to_string(),
            PowerState::A0PlusHP => "A0PlusHP".to_string(),
            PowerState::A0Thermtrip => "A0Thermtrip".to_string(),
            PowerState::A0Reset => "A0Reset".to_string(),
        }
    }
}

impl std::str::FromStr for PowerState {
    type Err = FixYourParsing;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "A2" => Ok(Self::A2),
            "A2PlusFans" => Ok(Self::A2PlusFans),
            "A0" => Ok(Self::A0),
            "A0PlusHP" => Ok(Self::A0PlusHP),
            "A0Thermtrip" => Ok(Self::A0Thermtrip),
            "A0Reset" => Ok(Self::A0Reset),
            _ => Err(FixYourParsing),
        }
    }
}

fn get_power_state(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut humility_hiffy::HiffyContext,
) -> PowerState {
    let get_op = hubris.get_idol_command("Sequencer.get_state").unwrap();

    let state: Result<humility::reflect::Value, String> =
        humility_hiffy::hiffy_call(
            &hubris,
            core,
            context,
            &get_op,
            &[],
            None,
            None,
        )
        .map_err(|e| match e {
            humility_hiffy::HiffyError::Hiffy(s) => s,
            _ => panic!("hmmm this is ugly"),
        });

    let state = humility_hiffy::hiffy_format_result(&hubris, state);

    state.parse::<PowerState>().unwrap()
}

fn set_power_state(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut humility_hiffy::HiffyContext,
    state: PowerState,
) -> String {
    let set_op = hubris.get_idol_command("Sequencer.set_state").unwrap();

    let state = state.to_string();
    let args = vec![("state", humility_idol::IdolArgument::String(&state))];

    let state: Result<humility::reflect::Value, String> =
        humility_hiffy::hiffy_call(
            &hubris, core, context, &set_op, &args, None, None,
        )
        .map_err(|e| match e {
            humility_hiffy::HiffyError::Hiffy(s) => s,
            _ => panic!("hmmm this is ugly"),
        });

    humility_hiffy::hiffy_format_result(&hubris, state)
}

fn main() {
    let hubris = std::env::var("HUMILITY_ARCHIVE").unwrap();
    let probe = std::env::var("HUMILITY_PROBE").unwrap();

    let hubris = HubrisArchive::load_from_path(&hubris).unwrap();

    let core = &mut *hubris.attach(&probe).unwrap();

    let mut context = humility_hiffy::HiffyContext::new(
        &hubris,
        core,
        std::time::Duration::from_millis(10000),
    )
    .unwrap();

    let state = get_power_state(&hubris, core, &mut context);

    match state {
        PowerState::A0 | PowerState::A0PlusHP => {
            println!("I'm in A0! going to go to A2 now!");
            let result =
                set_power_state(&hubris, core, &mut context, PowerState::A2);
            println!("I was {result}");
        }
        PowerState::A2 | PowerState::A2PlusFans => {
            println!("I'm in A2! going to go to A0 now!");
            let result =
                set_power_state(&hubris, core, &mut context, PowerState::A0);
            println!("I was {result}");
        }
        _ => {
            println!("{state:?} makes my state machine too hard");
        }
    }
}
