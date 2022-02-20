// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::debug::Register;
use crate::register;
use anyhow::Result;
use bitfield::bitfield;
use humility::core::Core;

register!(TPIU_SSPSR, 0xe004_0000,
    #[derive(Copy, Clone)]
    #[allow(non_camel_case_types)]
    pub struct TPIU_SSPSR(u32);
    impl Debug;
    swidth, _: 31, 0;
);

//
// TPIU Asynchronous Clock Prescaler Register
//
register!(TPIU_ACPR, 0xe004_0010,
    #[derive(Copy, Clone)]
    #[allow(non_camel_case_types)]
    pub struct TPIU_ACPR(u32);
    impl Debug;
    pub swoscaler, set_swoscaler: 15, 0;
);

//
// TPIU Selected Pin Protocol Register
//
register!(TPIU_SPPR, 0xe004_00f0,
    #[derive(Copy, Clone)]
    #[allow(non_camel_case_types)]
    pub struct TPIU_SPPR(u32);
    impl Debug;
    _txmode, _set_txmode: 1, 0;
);

pub enum TPIUMode {
    Parallel,
    Manchester,
    NRZ,
}

impl TPIU_SPPR {
    pub fn set_txmode(&mut self, mode: TPIUMode) {
        let val = match mode {
            TPIUMode::Parallel => 0,
            TPIUMode::Manchester => 1,
            TPIUMode::NRZ => 2,
        };

        self._set_txmode(val);
    }
}

//
// TPIU Supported Test Patterns/Modes Register
//
register!(TPIU_STMR, 0xe004_0200,
    #[derive(Copy, Clone)]
    #[allow(non_camel_case_types)]
    pub struct TPIU_STMR(u32);
    impl Debug;
    pub continuous_mode, _: 17;
    pub timed_mode, _: 16;
    pub ff00_pattern, _: 3;
    pub aa55_pattern, _: 2;
    pub walking_0s_pattern, _: 1;
    pub walking_1s_pattern, _: 0;
);

//
// TPIU Flush and Format Status Register
//
register!(TPIU_FFSR, 0xe004_0300,
    #[derive(Copy, Clone)]
    #[allow(non_camel_case_types)]
    pub struct TPIU_FFSR(u32);
    impl Debug;
    pub tracectl_present, _: 2;
    pub formatter_stopped, _: 1;
    pub flush_in_progress, _: 0;
);

//
// TPIU Flush and Format Control Register
//
register!(TPIU_FFCR, 0xe004_0304,
    #[derive(Copy, Clone)]
    #[allow(non_camel_case_types)]
    pub struct TPIU_FFCR(u32);
    impl Debug;
    pub trigin, _: 8;
    pub continuous_formatting, set_continuous_formatting: 1;
);

//
// TPIU Formatter Synchronization Counter Register
//
register!(TPIU_FSCR, 0xe004_0308,
    #[derive(Copy, Clone)]
    #[allow(non_camel_case_types)]
    pub struct TPIU_FSCR(u32);
    impl Debug;
    pub counter, set_counter: 11, 0;
);

bitfield! {
    #[derive(Copy, Clone)]
    pub struct TPIUFrameHalfWord(u16);
    impl Debug;
    data_or_aux, _: 15, 8;
    data_or_id, _: 7, 1;
    f_control, _: 0;
}

impl From<u16> for TPIUFrameHalfWord {
    fn from(value: u16) -> Self {
        Self(value)
    }
}

impl From<(u8, u8)> for TPIUFrameHalfWord {
    fn from(value: (u8, u8)) -> Self {
        Self((value.1 as u16) << 8 | (value.0 as u16))
    }
}

#[derive(Copy, Clone, Debug)]
pub struct TPIUPacket {
    pub id: Option<u8>,
    pub datum: u8,
    pub offset: usize,
    pub time: f64,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum TPIUState {
    Searching,
    SearchingSyncing(usize),
    Framing,
    FramingSyncing(usize),
}

const TPIU_FRAME_SYNC: [u8; 4] = [0xff, 0xff, 0xff, 0x7f];
const TPIU_ID_NULL: u8 = 0;

fn tpiu_next_state(state: TPIUState, byte: u8, offset: usize) -> TPIUState {
    let sync = &TPIU_FRAME_SYNC;

    //
    // Based on our current state and the byte, we're looking at, determine
    // our next framing state.
    //
    let nstate = match state {
        TPIUState::SearchingSyncing(next) if byte != sync[next] => {
            TPIUState::Searching
        }

        TPIUState::FramingSyncing(next) if byte != sync[next] => {
            humility::msg!("TPIU framing derailed at offset {}", offset);
            TPIUState::Searching
        }

        TPIUState::SearchingSyncing(next) if next + 1 < sync.len() => {
            TPIUState::SearchingSyncing(next + 1)
        }

        TPIUState::FramingSyncing(next) if next + 1 < sync.len() => {
            TPIUState::FramingSyncing(next + 1)
        }

        TPIUState::SearchingSyncing(next) => {
            humility::msg!(
                "TPIU sync packet found at offset {}",
                offset - next
            );
            TPIUState::Framing
        }

        TPIUState::FramingSyncing(_) => TPIUState::Framing,

        TPIUState::Searching if byte != sync[0] => TPIUState::Searching,

        TPIUState::Searching => TPIUState::SearchingSyncing(1),

        TPIUState::Framing if byte != sync[0] => TPIUState::Framing,

        TPIUState::Framing => TPIUState::FramingSyncing(1),
    };

    match (state, nstate) {
        (TPIUState::Searching, TPIUState::Searching)
        | (TPIUState::Searching, TPIUState::SearchingSyncing(_))
        | (TPIUState::SearchingSyncing(_), TPIUState::Searching)
        | (TPIUState::SearchingSyncing(_), TPIUState::Framing)
        | (TPIUState::SearchingSyncing(_), TPIUState::SearchingSyncing(_))
        | (TPIUState::Framing, TPIUState::Framing)
        | (TPIUState::Framing, TPIUState::FramingSyncing(_))
        | (TPIUState::FramingSyncing(_), TPIUState::Framing)
        | (TPIUState::FramingSyncing(_), TPIUState::Searching)
        | (TPIUState::FramingSyncing(_), TPIUState::FramingSyncing(_)) => {}
        _ => {
            panic!(
                "illegal state transition at offset {}: {:?} -> {:?}",
                offset, state, nstate
            );
        }
    }

    nstate
}

fn tpiu_check_frame(
    frame: &[(u8, f64, usize)],
    valid: &[bool],
    intermixed: bool,
) -> bool {
    //
    // To check a frame, we go through its half words, checking them for
    // inconsistency.  The false positive rate will very much depend on how
    // crowded the ID space is:  the sparser the valid space, the less likely
    // we are to accept a frame that is in fact invalid.
    //
    let max = frame.len() / 2;

    for i in 0..max {
        let base = i * 2;
        let half = TPIUFrameHalfWord::from((frame[base].0, frame[base + 1].0));

        if half.f_control() {
            //
            // The NULL source identifier denotes data we need to explicitly
            // chuck; check for it.
            //
            if half.data_or_id() == TPIU_ID_NULL as u16 {
                continue;
            }

            //
            // The two conditions under which we can reject a frame:  we
            // either have an ID that isn't expected, or we are not expecting
            // intermixed output and we have an ID on anything but the first
            // half-word of the frame.
            //
            if !valid[half.data_or_id() as usize] || (i > 0 && !intermixed) {
                return false;
            }
        }
    }

    true
}

fn tpiu_check_byte(byte: u8, valid: &[bool]) -> bool {
    let check: TPIUFrameHalfWord = (byte as u16).into();

    check.f_control() && valid[check.data_or_id() as usize]
}

fn tpiu_process_frame(
    frame: &[(u8, f64, usize)],
    id: Option<u8>,
    mut callback: impl FnMut(&TPIUPacket) -> Result<()>,
) -> Result<u8> {
    let high = frame.len() - 1;
    let aux = TPIUFrameHalfWord::from((frame[high - 1].0, frame[high].0));
    let max = frame.len() / 2;
    let mut current = id;

    for i in 0..max {
        let base = i * 2;
        let half = TPIUFrameHalfWord::from((frame[base].0, frame[base + 1].0));
        let auxbit = ((aux.data_or_aux() & (1 << i)) >> i) as u8;
        let last = i == max - 1;

        if half.f_control() {
            //
            // If our bit is set, the sense of the auxiliary bit tells us
            // if the ID takes effect with this halfword (bit is clear), or
            // with the next (bit is set).
            //
            let delay = auxbit != 0;
            let mut packet = TPIUPacket {
                id: Some(half.data_or_id() as u8),
                datum: half.data_or_aux() as u8,
                time: frame[base + 1].1,
                offset: frame[base + 1].2,
            };

            if last {
                //
                // If this is the last half-word, the auxiliary bit "must be
                // ignored" (Sec. D4.2 in the ARM CoreSight Architecture
                // Specification), and applies to the subsequent record.  So
                // in this case, we just return the ID.
                //
                return Ok(packet.id.unwrap());
            }

            match (delay, current) {
                (false, _) => {
                    callback(&packet)?;
                }
                (true, Some(current)) => {
                    let saved = packet.id;
                    packet.id = Some(current);
                    callback(&packet)?;
                    packet.id = saved;
                }
                (true, None) => {
                    //
                    // We have no old ID -- we are going to discard this
                    // byte, but also warn about it.
                    //
                    warn!("orphaned byte at offset {}", packet.offset);
                }
            }

            current = packet.id;
        } else {
            //
            // If our bit is NOT set, the auxiliary bit is the actual bit
            // of data.  If our current is not set, then we are still
            // searching for a first frame; we don't have an ID
            // to associate with it, so we need to chuck the
            // data.
            //
            let id = match current {
                Some(id) => id,
                None => {
                    assert!(!last);
                    continue;
                }
            };

            callback(&TPIUPacket {
                id: Some(id),
                datum: (half.data_or_id() << 1) as u8 | auxbit,
                time: frame[base].1,
                offset: frame[base].2,
            })?;

            if last {
                return Ok(id);
            }

            callback(&TPIUPacket {
                id: Some(id),
                datum: half.data_or_aux() as u8,
                time: frame[base + 1].1,
                offset: frame[base + 1].2,
            })?;
        }
    }

    //
    // We shouldn't be able to get here:  the last half-word handling logic
    // should assure that we return from within the loop.
    //
    unreachable!();
}

pub fn tpiu_ingest_bypass(
    mut readnext: impl FnMut() -> Result<Option<(u8, f64)>>,
    mut callback: impl FnMut(&TPIUPacket) -> Result<()>,
) -> Result<()> {
    let mut datum: u8;
    let mut offs = 0;
    let mut time: f64;

    loop {
        match readnext()? {
            Some(result) => {
                datum = result.0;
                time = result.1;
            }
            None => {
                break;
            }
        }

        offs += 1;

        callback(&TPIUPacket { id: None, datum, time, offset: offs })?
    }

    Ok(())
}

pub fn tpiu_ingest(
    valid: &[bool],
    mut readnext: impl FnMut() -> Result<Option<(u8, f64)>>,
    mut callback: impl FnMut(&TPIUPacket) -> Result<()>,
) -> Result<()> {
    let mut state = TPIUState::Searching;

    let mut ndx = 0;
    let mut frame: Vec<(u8, f64, usize)> = vec![(0u8, 0.0, 0); 16];
    let mut replay: Vec<(u8, f64, usize)> = vec![];

    let mut nvalid = 0;
    let mut id = None;
    let mut offs = 0;
    let mut datum: u8;
    let mut time: f64;

    let mut filter = |packet: &TPIUPacket| {
        if packet.id == Some(TPIU_ID_NULL) {
            Ok(())
        } else {
            callback(packet)
        }
    };

    loop {
        if !replay.is_empty() {
            let popped = replay.pop().unwrap();

            datum = popped.0;
            time = popped.1;
            offs = popped.2;
        } else {
            match readnext()? {
                Some(result) => {
                    datum = result.0;
                    time = result.1;
                }
                None => {
                    break;
                }
            }

            offs += 1;
        }

        match state {
            TPIUState::SearchingSyncing(_) | TPIUState::FramingSyncing(_) => {
                state = tpiu_next_state(state, datum, offs);

                if state == TPIUState::Searching {
                    //
                    // We just got kicked back into searching; we need to
                    // replay this datum to see if it starts a frame.
                    //
                    replay.push((datum, time, offs));
                    continue;
                }
            }

            TPIUState::Searching => {
                if ndx == 0 {
                    state = tpiu_next_state(state, datum, offs);

                    match state {
                        TPIUState::SearchingSyncing(_) => {
                            continue;
                        }
                        TPIUState::Searching => {
                            if !tpiu_check_byte(datum, valid) {
                                continue;
                            }
                        }
                        _ => {
                            unreachable!();
                        }
                    }
                }

                frame[ndx] = (datum, time, offs);
                ndx += 1;

                if ndx < frame.len() {
                    continue;
                }

                //
                // We have a complete frame.  We need to now check the entire
                // frame.
                //
                if tpiu_check_frame(&frame, valid, true) {
                    humility::msg!(
                        "valid TPIU frame starting at offset {}",
                        frame[0].2
                    );
                    id = Some(tpiu_process_frame(&frame, id, &mut filter)?);
                    state = TPIUState::Framing;
                    nvalid = 1;
                    ndx = 0;
                    continue;
                }

                //
                // That wasn't a valid frame; we need to replay.
                //
                while ndx > 1 {
                    replay.push(frame[ndx - 1]);
                    ndx -= 1;
                }

                ndx = 0;
            }

            TPIUState::Framing => {
                if ndx == 0 {
                    state = tpiu_next_state(state, datum, offs);

                    match state {
                        TPIUState::Framing => {}
                        TPIUState::FramingSyncing(_) => {
                            continue;
                        }
                        _ => {
                            unreachable!();
                        }
                    }
                }

                frame[ndx] = (datum, time, offs);
                ndx += 1;

                if ndx < frame.len() {
                    continue;
                }

                //
                // We have a complete frame, but we more or less expect it to
                // be correct.  If this fails, we need to go back in time
                // and resume our search for a frame.
                //
                if !tpiu_check_frame(&frame, valid, true) {
                    warn!(
                        "after {} frame{}, invalid frame at offset {}",
                        nvalid,
                        if nvalid == 1 { "" } else { "s" },
                        frame[0].2
                    );

                    while ndx > 1 {
                        replay.push(frame[ndx - 1]);
                        ndx -= 1;
                    }

                    nvalid = 0;
                    state = TPIUState::Searching;
                } else {
                    nvalid += 1;
                    id = Some(tpiu_process_frame(&frame, id, &mut filter)?);
                }

                ndx = 0;
            }
        }
    }

    humility::msg!("{} valid TPIU frames", nvalid);

    Ok(())
}
