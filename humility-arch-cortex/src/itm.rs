// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::debug::*;
use crate::dwt::*;
use crate::register;
use crate::scs::*;
use crate::swo::*;
use crate::tpiu::*;
use anyhow::Result;
use bitfield::bitfield;
use humility::core::Core;
use humility::hubris::HubrisArchive;

//
// ITM Trace Enable Register
//
register!(ITM_TER, 0xe000_0e00,
    #[derive(Copy, Clone)]
    #[allow(non_camel_case_types)]
    pub struct ITM_TER(u32);
    impl Debug;
    pub enabled, set_enabled: 31, 0;
);

//
// ITM Trace Privilege Register
//
register!(ITM_TPR, 0xe000_0e40,
    #[derive(Copy, Clone)]
    #[allow(non_camel_case_types)]
    pub struct ITM_TPR(u32);
    impl Debug;
    pub privmask, set_privmask: 31, 0;
);

//
// ITM Trace Configuration Register
//
register!(ITM_TCR, 0xe000_0e80,
    #[derive(Copy, Clone)]
    #[allow(non_camel_case_types)]
    pub struct ITM_TCR(u32);
    impl Debug;
    pub itm_busy, _: 23;
    pub traceid, set_traceid: 22, 16;
    pub timestamp_prescaler, _: 9, 8;
    pub swo_enable, _: 4;
    pub dwt_enable, set_dwt_enable: 3;
    pub sync_enable, set_sync_enable: 2;
    pub timestamp_enable, set_timestamp_enable: 1;
    pub itm_enable, set_itm_enable: 0;
);

//
// ITM Lock Access Register
//
register!(ITM_LAR, 0xe000_0fb0,
    #[derive(Copy, Clone)]
    #[allow(non_camel_case_types)]
    pub struct ITM_LAR(u32);
    impl Debug;
    pub key, _: 1;
);

//
// ITM Lock Status Register
//
register!(ITM_LSR, 0xe000_0fb4,
    #[derive(Copy, Clone)]
    #[allow(non_camel_case_types)]
    pub struct ITM_LSR(u32);
    impl Debug;
    pub locked, _: 1;
    pub unlock_required, _: 0;
);

impl ITM_LAR {
    pub fn unlock(core: &mut dyn humility::core::Core) -> Result<()> {
        //
        // To unlock, we write "CoreSight Access" in l33t
        //
        let val: u32 = 0xc5ac_ce55;
        core.write_word_32(ITM_LAR::ADDRESS, val)?;
        Ok(())
    }

    pub fn lock(core: &mut dyn humility::core::Core) -> Result<()> {
        let val: u32 = 0x1de_c0de;
        core.write_word_32(ITM_LAR::ADDRESS, val)?;
        Ok(())
    }
}

#[derive(Debug)]
pub enum ITMPayload {
    None,
    LocalTimestamp {
        timedelta: u32,
        delayed: bool,
        early: bool,
    },
    #[allow(dead_code)]
    Extension {
        payload: u32,
        sh: bool,
    },
    #[allow(dead_code)]
    GlobalTimestamp {
        timestamp: u64,
    },
    Instrumentation {
        port: u32,
        payload: Vec<u8>,
    },
    #[allow(dead_code)]
    Hardware {
        source: u32,
        payload: [u8; 4],
        len: usize,
    },
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ITMHeader {
    Sync,
    Overflow,
    LocalTimestamp1 { tc: u8 },
    LocalTimestamp2 { ts: u8 },
    GlobalTimestamp1,
    GlobalTimestamp2,
    Extension { c: bool, d: u8, s: bool },
    Instrumentation { a: u8, ss: u8 },
    Hardware { a: u8, ss: u8 },
    Malformed(u8),
}

#[derive(Copy, Clone, Debug)]
enum ITMPacketState {
    AwaitingHeader,
    AwaitingPayload,
    Complete,
}

#[derive(Debug)]
pub struct ITMPacket {
    pub header: ITMHeader,
    pub payload: ITMPayload,
    pub offset: usize,
    pub time: f64,
}

fn encode(hdr: ITMHeader) -> u8 {
    match hdr {
        ITMHeader::Sync => 0,
        ITMHeader::Overflow => 0b0111_0000,
        ITMHeader::LocalTimestamp1 { tc } => {
            assert!(tc <= 0b11);
            0b1100_0000 | (tc << 4)
        }

        ITMHeader::LocalTimestamp2 { ts } => {
            assert!(ts != 0);
            assert!(ts < 0b111);
            ts << 4
        }

        ITMHeader::GlobalTimestamp1 => 0b1001_0100,
        ITMHeader::GlobalTimestamp2 => 0b1011_0100,
        ITMHeader::Extension { c, d, s } => {
            let sh = if s { 1 << 2 } else { 0 };
            0b0000_1000 | (d & 0b111) << 4 | if c { 1 << 7 } else { 0 } | sh
        }

        ITMHeader::Instrumentation { a, ss } => {
            assert!(ss != 0);
            assert!((a >> 5) == 0);
            (a << 3) | ss
        }

        ITMHeader::Hardware { a, ss } => {
            assert!(ss != 0);
            assert!((a >> 5) == 0);
            0b0000_0100 | (a << 3) | ss
        }

        ITMHeader::Malformed(_) => {
            panic!("attempt to encode malformed header");
        }
    }
}

fn set(table: &mut [Option<ITMHeader>], hdr: ITMHeader) {
    let val = encode(hdr) as usize;

    match table[val] {
        None => {
            table[val] = Some(hdr);
        }
        Some(h) => {
            panic!(
                "two values for ITM header 0x{:x} (0b{:b}): {:?} and {:?}",
                val, val, h, hdr
            );
        }
    }
}

fn itm_hdrs() -> Vec<Option<ITMHeader>> {
    let mut hdr: Vec<Option<ITMHeader>> = vec![None; 256];
    let bools = [false, true];

    set(&mut hdr, ITMHeader::Sync);
    set(&mut hdr, ITMHeader::Overflow);

    for a in 0..32 {
        for ss in 1..=0b11 {
            set(&mut hdr, ITMHeader::Instrumentation { a, ss });
            set(&mut hdr, ITMHeader::Hardware { a, ss });
        }
    }

    for i in 0..=0b11 {
        set(&mut hdr, ITMHeader::LocalTimestamp1 { tc: i });
    }

    for i in 1..0b111 {
        set(&mut hdr, ITMHeader::LocalTimestamp2 { ts: i });
    }

    for i in 0..=0b111 {
        for s in bools.iter() {
            for c in bools.iter() {
                set(&mut hdr, ITMHeader::Extension { c: *c, d: i, s: *s });
            }
        }
    }

    set(&mut hdr, ITMHeader::GlobalTimestamp1);
    set(&mut hdr, ITMHeader::GlobalTimestamp2);

    hdr
}

fn itm_packet_state(hdr: ITMHeader, payload: &[u8]) -> ITMPacketState {
    let expect = |size: u8| {
        if payload.len() < size as usize {
            ITMPacketState::AwaitingPayload
        } else {
            ITMPacketState::Complete
        }
    };

    let compressed = |max: u8| {
        let mut ndx: u8 = 0;

        while ndx < payload.len() as u8 {
            if ndx == max - 1 || (payload[ndx as usize] & 0b1000_0000) == 0 {
                break;
            }

            ndx += 1;
        }

        ndx + 1
    };

    match hdr {
        ITMHeader::Sync => expect(5),
        ITMHeader::Overflow => ITMPacketState::Complete,
        ITMHeader::LocalTimestamp1 { .. } => expect(compressed(4)),
        ITMHeader::LocalTimestamp2 { .. } => ITMPacketState::Complete,
        ITMHeader::GlobalTimestamp1 => expect(compressed(4)),
        ITMHeader::GlobalTimestamp2 => expect(4),
        ITMHeader::Extension { .. } => expect(compressed(4)),
        ITMHeader::Instrumentation { ss, .. }
        | ITMHeader::Hardware { ss, .. } => expect(match ss {
            0b01 => 1,
            0b10 => 2,
            0b11 => 4,
            _ => panic!("invalid ss"),
        }),
        ITMHeader::Malformed(_) => {
            panic!("cannot determine packet state on malformed header");
        }
    }
}

fn itm_payload_decode(hdr: ITMHeader, payload: &[u8]) -> ITMPayload {
    match hdr {
        ITMHeader::Instrumentation { a, .. } => ITMPayload::Instrumentation {
            port: a as u32,
            payload: payload.to_vec(),
        },

        ITMHeader::LocalTimestamp1 { tc } => {
            let mut delta: u32 = 0;

            for (i, pld) in payload.iter().enumerate() {
                delta |= ((*pld as u32) & 0b0111_1111) << (i * 7);
            }

            ITMPayload::LocalTimestamp {
                delayed: (tc & 0b01) != 0,
                early: (tc & 0b10) != 0,
                timedelta: delta,
            }
        }

        _ => ITMPayload::None,
    }
}

pub fn itm_ingest(
    traceid: Option<u8>,
    mut readnext: impl FnMut() -> Result<Option<(u8, f64)>>,
    mut callback: impl FnMut(&ITMPacket) -> Result<()>,
) -> Result<()> {
    #[derive(Copy, Clone, Debug, Eq, PartialEq)]
    enum IngestState {
        SyncSearching,
        Ingesting,
    }

    let mut state: IngestState = IngestState::SyncSearching;
    let mut pstate: ITMPacketState = ITMPacketState::AwaitingHeader;
    let mut vec = Vec::with_capacity(16);

    let hdrs = &itm_hdrs();
    let mut hdr = ITMHeader::Sync;
    let mut runlen = 0;

    let process = |packet: &TPIUPacket| -> Result<()> {
        let payload = &mut vec;

        if state == IngestState::SyncSearching {
            match packet.datum {
                0 => runlen += 1,
                0x80 => {
                    if runlen >= 5 {
                        humility::msg!(
                            "ITM synchronization packet found at offset {}",
                            packet.offset
                        );
                        state = IngestState::Ingesting;
                    }
                }
                _ => {
                    runlen = 0;
                }
            }

            return Ok(());
        }

        match pstate {
            ITMPacketState::AwaitingHeader => {
                hdr = match hdrs[packet.datum as usize] {
                    Some(hdr) => hdr,
                    None => {
                        callback(&ITMPacket {
                            header: ITMHeader::Malformed(packet.datum),
                            payload: ITMPayload::None,
                            offset: packet.offset,
                            time: packet.time,
                        })?;

                        humility::msg!(
                            "unrecognized ITM header 0x{:x} at offset {}",
                            packet.datum,
                            packet.offset
                        );

                        state = IngestState::SyncSearching;
                        return Ok(());
                    }
                };

                payload.truncate(0);
            }

            ITMPacketState::AwaitingPayload => {
                payload.push(packet.datum);
            }

            ITMPacketState::Complete => {
                panic!("unexpected packet state");
            }
        }

        pstate = itm_packet_state(hdr, payload);

        match pstate {
            ITMPacketState::AwaitingHeader
            | ITMPacketState::AwaitingPayload => {
                return Ok(());
            }
            ITMPacketState::Complete => {}
        }

        if state == IngestState::Ingesting {
            callback(&ITMPacket {
                header: hdr,
                payload: itm_payload_decode(hdr, payload),
                offset: packet.offset,
                time: packet.time,
            })?;
        } else {
            unreachable!();
        }

        pstate = ITMPacketState::AwaitingHeader;

        Ok(())
    };

    match traceid {
        Some(traceid) => {
            let mut valid = vec![false; 256];
            valid[traceid as usize] = true;
            tpiu_ingest(&valid, &mut readnext, process)
        }
        None => tpiu_ingest_bypass(&mut readnext, process),
    }
}

///
/// Enables ITM with an explict clockscaler and traceid.
pub fn itm_enable_explicit(
    core: &mut dyn Core,
    coreinfo: &CoreInfo,
    clockscaler: u16,
    traceid: u8,
    stimuli: u32,
) -> Result<()> {
    //
    // First, enable TRCENA in the DEMCR.
    //
    let mut val = DEMCR::read(core)?;
    val.set_trcena(true);
    val.write(core)?;

    match (coreinfo.vendor, coreinfo.part) {
        (Vendor::ST, ARMCore::CortexM4) => {
            //
            // STM32F4xx-specific: enable TRACE_IOEN in the DBGMCU_CR, and
            // set the trace mode to be asynchronous.
            //
            let mut val = STM32F4_DBGMCU_CR::read(core)?;
            val.set_trace_ioen(true);
            val.set_trace_mode(0);
            val.write(core)?;
        }

        (Vendor::ST, ARMCore::CortexM7) => {
            //
            // STM32H7xx-specific: enable D3 and D1 clock domain + traceclk
            //
            let mut cr = STM32H7_DBGMCU_CR::read(core)?;
            cr.set_srdbgcken(true);
            cr.set_cddbgcken(true);
            cr.set_traceclken(true);
            cr.write(core)?;

            let components = &coreinfo.components;

            if let Some(cstf) = components.get_vec(&CoreSightComponent::CSTF) {
                //
                // If we have two funnels, the first is in D3 -- and it needs
                // to be unlocked and enabled.
                //
                if cstf.len() > 1 {
                    log::trace!("SWTF found at {:x}", cstf[0]);
                    SWO_LAR::unlock(core, cstf[0])?;
                    let mut swtf = SWTF_CTRL::read(core, cstf[0])?;
                    swtf.register.set_es0(true);
                    swtf.write(core)?;
                }
            }
        }

        _ => {}
    }

    let swoscaler = clockscaler as u32;

    if let Some(swo) = coreinfo.address(CoreSightComponent::SWO) {
        //
        // If we have a SWO unit, configure it instead of the TPIU
        //
        log::trace!("SWO found at {:x}", swo);

        SWO_LAR::unlock(core, swo)?;

        let mut codr = SWO_CODR::read(core, swo)?;
        codr.register.set_prescaler(swoscaler);
        codr.write(core)?;

        let mut sppr = SWO_SPPR::read(core, swo)?;
        sppr.register.set_pprot(SWOMode::NRZ.into());
        sppr.write(core)?;
    } else {
        //
        // Otherwise setup the TPIU.
        //
        let mut val = TPIU_SPPR::read(core)?;
        val.set_txmode(TPIUMode::NRZ);
        val.write(core)?;

        let mut val = TPIU_FFCR::read(core)?;
        val.set_continuous_formatting(true);
        val.write(core)?;

        let mut acpr = TPIU_ACPR::read(core)?;
        acpr.set_swoscaler(swoscaler);
        acpr.write(core)?;
        log::trace!("{:#x?}", TPIU_ACPR::read(core)?);
    }

    //
    // Unlock the ITM.
    //
    ITM_LAR::unlock(core)?;

    //
    // Disable the ITM.
    //
    let mut tcr = ITM_TCR::read(core)?;
    tcr.set_itm_enable(false);
    tcr.write(core)?;

    //
    // Spin until the ITM is not busy
    //
    while ITM_TCR::read(core)?.itm_busy() {
        continue;
    }

    //
    // Enable the DWT to generate a synchronization packet every 8M cycles.
    //
    let mut dwt = DWT_CTRL::read(core)?;
    dwt.set_synctap(DWTSyncTapFrequency::CycCnt8M);
    dwt.set_cyccnt_enabled(true);
    dwt.write(core)?;

    //
    // Enable stimuli
    //
    let mut ter = ITM_TER::read(core)?;
    ter.set_enabled(stimuli);
    ter.write(core)?;

    //
    // Allow unprivileged access to all stimulus ports
    //
    let mut tpr = ITM_TPR::read(core)?;
    tpr.set_privmask(0);
    tpr.write(core)?;

    //
    // Set the trace ID
    //
    tcr = ITM_TCR::read(core)?;
    tcr.set_traceid(traceid.into());
    tcr.set_timestamp_enable(stimuli & 0xffff_0000 != 0);

    tcr.set_sync_enable(true);
    tcr.set_itm_enable(true);
    tcr.write(core)?;

    Ok(())
}

///
/// Enables ITM by pulling clock scaler values from the specified Hubris
/// archive.
pub fn itm_enable_ingest(
    core: &mut dyn Core,
    hubris: &HubrisArchive,
    stim: u32,
) -> Result<Option<u8>> {
    let coreinfo = CoreInfo::read(core)?;

    let _info = core.halt();
    core.init_swv()?;

    //
    // Pull our clock scaler from the Hubris archive -- and set our traceid
    // to be a recognizable value.
    //
    let clockscaler = swoscaler(hubris, core)?;
    let traceid = 0x3a;

    itm_enable_explicit(core, &coreinfo, clockscaler, traceid, stim)?;

    core.run()?;

    Ok(if coreinfo.address(CoreSightComponent::SWO).is_some() {
        None
    } else {
        Some(traceid)
    })
}
