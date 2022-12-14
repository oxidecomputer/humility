// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::debug::Register;
use crate::register;
use crate::tpiu::*;
use anyhow::Result;
use bitfield::bitfield;
use humility::core::Core;

macro_rules! etm_register {
    ($reg:ty, $offs:expr, $($arg:tt)*) => (
        register!($reg, 0xe004_1000 + ($offs * 4), $($arg)*);
    )
}

//
// ETM Main Configuration Register
//
etm_register!(ETMCR, 0x000,
    #[derive(Copy, Clone)]
    pub struct ETMCR(u32);
    impl Debug;
    pub vmid_trace_enable, _: 30;
    pub timestamp_enable, set_timestamp_enable: 28;
    pub processor_select, set_processor_select: 27, 25;
    pub instrumentation_access_control, _: 24;
    pub disable_software_writes, _: 23;
    pub disable_debugger_writes, _: 22;
    pub port_size_hibit, _: 21;
    pub data_only_mode, _: 20;
    pub filter_cprt, set_filter_cprt: 19;
    pub suppress_data, set_suppress_data: 18;
    pub port_mode, set_port_mode: 17, 16;
    pub context_id, set_context_id: 15, 14;
    pub half_rate_clocking, set_half_rate_clocking: 13;
    pub cycle_accurate_tracing, set_cycle_accurate_tracing: 12;
    pub port_select, set_port_select: 11;
    pub programming, set_programming: 10;
    pub debug_request_control, _: 9;
    pub branch_output, set_branch_output: 8;
    pub stall_processor, set_stall_processor: 7;
    pub port_size, set_port_size: 6, 4;
    pub data_access, set_data_access: 3, 2;
    pub monitor_cprt, set_monitor_cprt: 1;
    pub power_down, set_power_down: 0;
);

//
// ETM Configuration Code Register
//
etm_register!(ETMCCR, 0x001,
    #[derive(Copy, Clone)]
    pub struct ETMCCR(u32);
    impl Debug;
    pub has_etmidr, _: 31;
    pub protocol, _: 30, 28;
    pub can_memmap, _: 27;
    pub has_startstop, _: 26;
    pub num_context_comparators, _: 25, 24;
    pub has_fifofull, _: 23;
    pub num_external_outputs, _: 22, 20;
    pub num_external_inputs, _: 19, 17;
    pub has_sequencer, _: 16;
    pub num_counters, _: 15, 13;
    pub num_decoder_inputs, _: 12, 8;
    pub num_data_comparators, _: 7, 4;
    pub num_address_comparators, _: 3, 0;
);

//
// ETM Status Register
//
etm_register!(ETMSR, 0x004,
    #[derive(Copy, Clone)]
    pub struct ETMSR(u32);
    impl Debug;
    pub trigger, _: 3;
    pub trace_startstop_status, _: 2;
    pub programming_status, _: 1;
    pub untraced_overflow, _: 0;
);

//
// ETM System Configuration Register
//
etm_register!(ETMSCR, 0x005,
    #[derive(Copy, Clone)]
    pub struct ETMSCR(u32);
    impl Debug;
    pub no_fetch_comparisons, _: 17;
    pub num_supported_processors, _: 14, 12;
    pub has_port_mode, _: 11;
    pub has_port_size, _: 10;
    pub max_port_size_highbit, _: 9;
    pub has_fifofull, _: 8;
    pub has_demux, _: 7;
    pub has_mux, _: 6;
    pub has_normal, _: 5;
    pub has_fullrate_clocking, _: 4;
    pub has_halfrate_clocking, _: 3;
    pub max_port_size, _: 2, 0;
);

//
// ETM TraceEnable Event Register
//
etm_register!(ETMTEEVR, 0x008,
    #[derive(Copy, Clone)]
    pub struct ETMTEEVR(u32);
    impl Debug;
    pub fcn, _: 16, 14;
    pub resource_b, set_resource_b: 13, 7;
    pub resource_a, set_resource_a: 6, 0;
);

//
// ETM TraceEnable Control 1 Register
//
etm_register!(ETMTECR1, 0x009,
    #[derive(Copy, Clone)]
    pub struct ETMTECR1(u32);
    impl Debug;
    pub trace_control_enable, _: 25;
    pub exclude, set_exclude: 24;
    pub map_decode_select, set_map_decode_select: 23, 8;
    pub comparator_select, set_comparator_select: 7, 0;
);

//
// ETM FIFOFULL Region Register
//
etm_register!(ETMFFRR, 0x00a,
    #[derive(Copy, Clone)]
    pub struct ETMFFRR(u32);
    impl Debug;
    pub exclude, set_exclude: 24;
    pub map_decode_select, set_map_decode_select: 23, 8;
    pub comparator_select, set_comparator_select: 7, 0;
);

//
// ETM FIFOFULL Level Register
//
etm_register!(ETMFFLR, 0x00b,
    #[derive(Copy, Clone)]
    pub struct ETMFFLR(u32);
    impl Debug;
    pub fifo_full_level, set_fifo_full_level: 7, 0;
);

//
// ETM Identification Register
//
etm_register!(ETMIDR, 0x079,
    #[derive(Copy, Clone)]
    pub struct ETMIDR(u32);
    impl Debug;
    pub implementer, _: 31, 24;
    pub has_branch_encoding, _: 20;
    pub has_security_extensions, _: 19;
    pub has_thumb32, _: 18;
    pub load_pc_first, _: 16;
    pub family, _: 15, 12;
    pub etm_major, _: 11, 8;
    pub etm_minor, _: 7, 4;
    pub etm_revision, _: 3, 0;
);

//
// ETM Configuration Code Extension Register
//
etm_register!(ETMCCER, 0x07a,
    #[derive(Copy, Clone)]
    pub struct ETMCCER(u32);
    impl Debug;
    pub has_64bit_timestamp, _: 29;
    pub has_binary_timestamp, _: 28;
    pub has_reduced_counter, _: 27;
    pub has_virtual_extensions, _: 26;
    pub has_timestamping, _: 22;
    pub has_etmeibcr, _: 21;
    pub can_use_ice, _: 20;
    pub num_ice_inputs, _: 19, 16;
    pub num_instrumentation_resources, _: 15, 13;
    pub no_data_comparison, _: 12;
    pub all_registers_readable, _: 11;
    pub extended_input_bus_size, _: 10, 3;
    pub num_extended_input_selectors, _: 2, 0;
);

//
// ETM Trace ID Register
//
etm_register!(ETMTRACEIDR, 0x080,
    #[derive(Copy, Clone)]
    pub struct ETMTRACEIDR(u32);
    impl Debug;
    pub traceid, set_traceid: 6, 0;
);

//
// ETM Identification Register 2
//
etm_register!(ETMIDR2, 0x082,
    #[derive(Copy, Clone)]
    pub struct ETMIDR2(u32);
    impl Debug;
    pub swp_store_before_load, _: 1;
    pub rfe_cpsr_before_pc, _: 0;
);

//
// ETM Lock Access Register
//
etm_register!(ETMLAR, 0x3ec,
    #[derive(Copy, Clone)]
    pub struct ETMLAR(u32);
    impl Debug;
    pub key, _: 1;
);

impl ETMLAR {
    pub fn unlock(core: &mut dyn humility::core::Core) -> Result<()> {
        //
        // To unlock, we write "CoreSight Access" in l33t
        //
        let val: u32 = 0xc5ac_ce55;
        core.write_word_32(ETMLAR::ADDRESS, val)?;
        Ok(())
    }

    pub fn lock(core: &mut dyn humility::core::Core) -> Result<()> {
        let val: u32 = 0x1de_c0de;
        core.write_word_32(ETMLAR::ADDRESS, val)?;
        Ok(())
    }
}

//
// ETM Lock Status Register
//
etm_register!(ETMLSR, 0x3ed,
    #[derive(Copy, Clone)]
    pub struct ETMLSR(u32);
    impl Debug;
    pub locked, _: 1;
    pub unlock_required, _: 0;
);

#[derive(Copy, Clone, Debug)]
pub enum ETM3Header {
    BranchAddress { addr: u8, c: bool },
    ASync,
    CycleCount,
    ISync,
    Trigger,
    OutOfOrder { tag: u8, size: u8 },
    StoreFailed,
    ISyncCycleCount,
    OutOfOrderPlaceholder { a: bool, tag: u8 },
    VMID,
    NormalData { a: bool, size: u8 },
    Timestamp { r: bool },
    DataSuppressed,
    Ignore,
    ValueNotTraced { a: bool },
    ContextID,
    ExceptionExit,
    ExceptionEntry,
    PHeaderFormat1 { e: u8, n: u8 },
    PHeaderFormat2 { e0: bool, e1: bool },
}

#[derive(Copy, Clone, Debug)]
enum ETM3PacketState {
    AwaitingHeader,
    AwaitingPayload,
    Complete,
}

#[derive(Copy, Clone, Debug)]
pub enum ETM3SyncReason {
    Periodic,
    TracingEnabled,
    TracingRestarted,
    DebugExit,
}

#[derive(Copy, Clone, Debug)]
pub enum ETM3ProcessorState {
    ARM,
    Thumb,
    ThumbEE,
    Jazelle,
}

#[derive(Copy, Clone, Debug)]
pub enum ETM3Exception {
    HardFault,
    IRQ { irq: u16 },
    UsageFault,
    NMI,
    SVC,
    DebugMonitor,
    MemManage,
    PendSV,
    SysTick,
    ProcessorReset,
    BusFault,
    Reserved { exception: u16 },
}

#[derive(Copy, Clone, Debug)]
pub enum ETM3Payload {
    None,
    BranchAddress {
        addr: u32,
        mask: u32,
        exception: Option<ETM3Exception>,
    },
    ISync {
        context: Option<u32>,
        reason: ETM3SyncReason,
        address: u32,
        processor_state: ETM3ProcessorState,
    },
}

#[derive(Copy, Clone, Debug)]
pub struct ETM3Packet {
    pub header: ETM3Header,
    pub payload: ETM3Payload,
    pub offset: usize,
    pub time: f64,
}

pub struct ETM3Config {
    pub alternative_encoding: bool,
    pub context_id: u8,
    pub data_access: bool,
    pub traceid: u8,
}

fn encode(hdr: ETM3Header) -> u8 {
    match hdr {
        ETM3Header::BranchAddress { addr, c } => {
            0b0000_0001 | if c { 1 << 7 } else { 0 } | (addr & 0b011_1111) << 1
        }
        ETM3Header::ASync => 0b0000_0000,
        ETM3Header::CycleCount => 0b0000_0100,
        ETM3Header::ISync => 0b0000_1000,
        ETM3Header::Trigger => 0b0000_1100,
        ETM3Header::OutOfOrder { tag, size } => {
            (tag & 0b11) << 5 | (size & 0b11) << 2
        }
        ETM3Header::StoreFailed => 0b0101_0000,
        ETM3Header::ISyncCycleCount => 0b0111_0000,
        ETM3Header::OutOfOrderPlaceholder { a, tag } => {
            0b0101_0000 | if a { 1 << 5 } else { 0 } | ((tag & 0b11) << 2)
        }
        ETM3Header::VMID => 0b0011_1100,
        ETM3Header::NormalData { a, size } => {
            0b0000_0010 | if a { 1 << 5 } else { 0 } | ((size & 0b11) << 2)
        }
        ETM3Header::Timestamp { r } => 0b0100_0010 | if r { 1 << 2 } else { 0 },
        ETM3Header::DataSuppressed => 0b0110_0010,
        ETM3Header::Ignore => 0b0110_0110,
        ETM3Header::ValueNotTraced { a } => {
            0b0110_1010 | if a { 1 << 4 } else { 0 }
        }
        ETM3Header::ContextID => 0b0110_1110,
        ETM3Header::ExceptionExit => 0b0111_0110,
        ETM3Header::ExceptionEntry => 0b0111_1110,
        ETM3Header::PHeaderFormat1 { n, e } => {
            0b1000_0000 | ((n & 0b1) << 6) | ((e & 0b1111) << 2)
        }
        ETM3Header::PHeaderFormat2 { e0, e1 } => {
            0b1000_0010
                | if e0 { 1 << 3 } else { 0 }
                | if e1 { 1 << 2 } else { 0 }
        }
    }
}

fn set(table: &mut [Option<ETM3Header>], hdr: ETM3Header) {
    let val = encode(hdr) as usize;

    match table[val] {
        None => {
            table[val] = Some(hdr);
        }
        Some(h) => {
            panic!(
                "two values for 0x{:x} (0b{:b}): {:?} and {:?}",
                val, val, h, hdr
            );
        }
    }
}

fn etm_hdrs() -> Vec<Option<ETM3Header>> {
    let mut hdr: Vec<Option<ETM3Header>> = vec![None; 256];

    for i in 0..=0b11_1111 {
        set(&mut hdr, ETM3Header::BranchAddress { addr: i, c: true });
        set(&mut hdr, ETM3Header::BranchAddress { addr: i, c: false });
    }

    set(&mut hdr, ETM3Header::ASync);
    set(&mut hdr, ETM3Header::CycleCount);
    set(&mut hdr, ETM3Header::ISync);
    set(&mut hdr, ETM3Header::Trigger);

    for tag in 1..=0b11 {
        for size in 0..=0b11 {
            set(&mut hdr, ETM3Header::OutOfOrder { tag, size });
        }
    }

    set(&mut hdr, ETM3Header::StoreFailed);
    set(&mut hdr, ETM3Header::ISyncCycleCount);

    for tag in 1..=0b11 {
        set(&mut hdr, ETM3Header::OutOfOrderPlaceholder { tag, a: true });
        set(&mut hdr, ETM3Header::OutOfOrderPlaceholder { tag, a: false });
    }

    set(&mut hdr, ETM3Header::VMID);

    for size in 0..=0b11 {
        set(&mut hdr, ETM3Header::NormalData { a: true, size });
        set(&mut hdr, ETM3Header::NormalData { a: false, size });
    }

    set(&mut hdr, ETM3Header::Timestamp { r: true });
    set(&mut hdr, ETM3Header::Timestamp { r: false });

    set(&mut hdr, ETM3Header::DataSuppressed);
    set(&mut hdr, ETM3Header::Ignore);

    set(&mut hdr, ETM3Header::ValueNotTraced { a: true });
    set(&mut hdr, ETM3Header::ValueNotTraced { a: false });

    set(&mut hdr, ETM3Header::ContextID);
    set(&mut hdr, ETM3Header::ExceptionExit);
    set(&mut hdr, ETM3Header::ExceptionEntry);

    for e in 0..=0b1111 {
        for n in 0..=0b1 {
            set(&mut hdr, ETM3Header::PHeaderFormat1 { e, n });
        }
    }

    set(&mut hdr, ETM3Header::PHeaderFormat2 { e0: false, e1: false });
    set(&mut hdr, ETM3Header::PHeaderFormat2 { e0: false, e1: true });
    set(&mut hdr, ETM3Header::PHeaderFormat2 { e0: true, e1: false });
    set(&mut hdr, ETM3Header::PHeaderFormat2 { e0: true, e1: true });

    hdr
}

fn etm_packet_state(
    hdr: ETM3Header,
    payload: &[u8],
    config: &ETM3Config,
) -> ETM3PacketState {
    let expect = |size: u8| {
        if payload.len() < size as usize {
            ETM3PacketState::AwaitingPayload
        } else {
            ETM3PacketState::Complete
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

    //
    // This assumes the alternative encoding for branch packets.
    //
    assert!(config.alternative_encoding);

    match hdr {
        ETM3Header::BranchAddress { c, .. } => {
            if payload.is_empty() {
                if c {
                    ETM3PacketState::AwaitingPayload
                } else {
                    ETM3PacketState::Complete
                }
            } else {
                let last = payload[payload.len() - 1];

                //
                // If the high order bit is set, we are always awaiting more
                // payload -- regardless of whether that is in one of the
                // address bytes (up to five) or one of the exception bytes
                // (up to three).
                //
                // If bit 6 is set, we are awaiting an Exception Information
                // Byte.
                //
                if (last & 0b1000_0000) != 0 || (last & 0b0100_0000) != 0 {
                    ETM3PacketState::AwaitingPayload
                } else {
                    ETM3PacketState::Complete
                }
            }
        }

        ETM3Header::CycleCount => expect(compressed(5)),
        ETM3Header::ISync => expect(5 + config.context_id),
        ETM3Header::OutOfOrder { size, .. } => expect(size),

        ETM3Header::ISyncCycleCount => {
            expect(compressed(5) + config.context_id + 5)
        }

        ETM3Header::OutOfOrderPlaceholder { .. } => expect(5),
        ETM3Header::VMID => expect(1),
        ETM3Header::NormalData { a, size } => {
            let dsize = if a && config.data_access { compressed(5) } else { 0 };

            expect(dsize + 1 + size)
        }

        ETM3Header::Timestamp { .. } => expect(compressed(9)),

        ETM3Header::ValueNotTraced { a } => {
            if a {
                expect(compressed(5))
            } else {
                ETM3PacketState::Complete
            }
        }

        ETM3Header::ContextID => expect(config.context_id),
        _ => ETM3PacketState::Complete,
    }
}

fn etm_payload_decode(
    hdr: ETM3Header,
    payload: &[u8],
    config: &ETM3Config,
) -> ETM3Payload {
    let context = |o| match config.context_id {
        0 => None,
        1 => Some(payload[o] as u32),
        2 => Some(u16::from_le_bytes([payload[o], payload[o + 1]]) as u32),
        4 => Some(u32::from_le_bytes([
            payload[o],
            payload[o + 1],
            payload[o + 2],
            payload[o + 3],
        ])),
        _ => {
            panic!("illegal context size");
        }
    };

    let reason = |ibyte| match ((ibyte >> 5) as u8) & 0b11 {
        0b00 => ETM3SyncReason::Periodic,
        0b01 => ETM3SyncReason::TracingEnabled,
        0b10 => ETM3SyncReason::TracingRestarted,
        0b11 => ETM3SyncReason::DebugExit,
        _ => panic!("illegal reason"),
    };

    let processor_state = |ibyte, addr| {
        let j = (ibyte & 0b0001_0000) != 0;
        let t = (addr & 0b0000_0001) != 0;
        let altisa = (ibyte & 0b0000_0100) != 0;

        match (j, t, altisa) {
            (false, false, false) => ETM3ProcessorState::ARM,
            (false, true, false) => ETM3ProcessorState::Thumb,
            (false, true, true) => ETM3ProcessorState::ThumbEE,
            (true, _, false) => ETM3ProcessorState::Jazelle,
            (_, _, _) => panic!("unknown processor state information"),
        }
    };

    let exception = |o| {
        let eib0 = payload[o];

        let mut xcp: u16 = ((eib0 & 0b0001_1110) as u16) >> 1;

        if (eib0 & 0b1000_0000) != 0 {
            let eib1 = payload[o + 1];
            xcp |= ((eib1 & 0b0001_1111) as u16) << 4;
        }

        match xcp {
            0x01..=0x07 => ETM3Exception::IRQ { irq: xcp },
            0x08 => ETM3Exception::IRQ { irq: 0 },
            0x09 => ETM3Exception::UsageFault,
            0x0a => ETM3Exception::NMI,
            0x0b => ETM3Exception::SVC,
            0x0c => ETM3Exception::DebugMonitor,
            0x0d => ETM3Exception::MemManage,
            0x0e => ETM3Exception::PendSV,
            0x0f => ETM3Exception::SysTick,
            0x11 => ETM3Exception::ProcessorReset,
            0x13 => ETM3Exception::HardFault,
            0x15 => ETM3Exception::BusFault,
            0x18..=0x1ff => ETM3Exception::IRQ { irq: xcp - 0x10 },
            _ => ETM3Exception::Reserved { exception: xcp },
        }
    };

    match hdr {
        ETM3Header::ISync => {
            let o = config.context_id as usize;

            let ibyte = payload[o];
            let addr = &payload[o + 1..o + 5];
            let processor_state = processor_state(ibyte, addr[0]);
            let a0 = match processor_state {
                ETM3ProcessorState::Jazelle => addr[0],
                _ => addr[0] & !0b0000_0001,
            };

            ETM3Payload::ISync {
                context: context(0),
                reason: reason(ibyte),
                address: u32::from_le_bytes([a0, addr[1], addr[2], addr[3]]),
                processor_state,
            }
        }
        ETM3Header::BranchAddress { addr, .. } => {
            let mut target: u32 = (addr as u32) << 1;
            let mut nbits = 7;
            let mut xcp = None;

            for (i, pld) in payload.iter().enumerate() {
                //
                // If our continue bit is set, we always have seven new bits
                // of address; or it in, increase the number of bits and
                // continue.
                //
                if (pld & 0b1000_0000) != 0 {
                    target |= ((pld & 0b0111_1111) as u32) << nbits;
                    nbits += 7;
                    continue;
                }

                //
                // The continue bit is not set, so this packet terminates
                // our address.  Bit 6 will indicate whether or not we have
                // exception bytes; if we have exception bytes, we'll pull
                // those now.
                //
                if (pld & 0b0100_0000) != 0 {
                    xcp = Some(exception(i + 1));
                }

                //
                // For our final address packet, it is generally six bits --
                // unless we have a full address change, in which case only
                // four bits remain.
                //
                if i < 4 {
                    target |= ((pld & 0b0011_1111) as u32) << nbits;
                    nbits += 6;
                    break;
                }

                assert_eq!(nbits, 28);
                target |= ((pld & 0b0000_1111) as u32) << nbits;
                nbits += 4;
            }

            ETM3Payload::BranchAddress {
                addr: target,
                mask: if nbits == 32 { 0 } else { !((1 << nbits) - 1) },
                exception: xcp,
            }
        }

        _ => {
            if payload.is_empty() {
                return ETM3Payload::None;
            }

            panic!("unhandled packet {:#x?}!", hdr);
        }
    }
}

pub fn etm_ingest(
    config: &ETM3Config,
    mut readnext: impl FnMut() -> Result<Option<(u8, f64)>>,
    mut callback: impl FnMut(&ETM3Packet) -> Result<()>,
) -> Result<()> {
    #[derive(Copy, Clone, Debug, PartialEq, Eq)]
    enum IngestState {
        ASyncSearching,
        ISyncSearching,
        Ingesting,
    }

    let mut state: IngestState = IngestState::ASyncSearching;
    let mut pstate: ETM3PacketState = ETM3PacketState::AwaitingHeader;
    let mut vec = Vec::with_capacity(16);

    let mut valid = vec![false; 256];
    valid[config.traceid as usize] = true;

    let hdrs = &etm_hdrs();
    let mut hdr = ETM3Header::ASync;
    let mut runlen = 0;

    tpiu_ingest(&valid, &mut readnext, |packet| {
        let payload = &mut vec;

        if state == IngestState::ASyncSearching {
            match packet.datum {
                0 => runlen += 1,
                0x80 => {
                    if runlen >= 5 {
                        humility::msg!(
                            "A-sync alignment synchronization \
                            packet found at offset {}",
                            packet.offset
                        );
                        state = IngestState::ISyncSearching;
                    }
                }
                _ => {
                    runlen = 0;
                }
            }

            return Ok(());
        }

        match pstate {
            ETM3PacketState::AwaitingHeader => {
                hdr = match hdrs[packet.datum as usize] {
                    Some(hdr) => hdr,
                    None => {
                        panic!(
                            "unrecognized ETMv3 header 0x{:x} at line {}",
                            packet.datum, packet.offset
                        );
                    }
                };

                payload.truncate(0);
            }

            ETM3PacketState::AwaitingPayload => {
                payload.push(packet.datum);
            }

            ETM3PacketState::Complete => {
                panic!("unexpected packet state");
            }
        }

        pstate = etm_packet_state(hdr, payload, config);

        match pstate {
            ETM3PacketState::AwaitingHeader
            | ETM3PacketState::AwaitingPayload => {
                return Ok(());
            }
            ETM3PacketState::Complete => {}
        }

        if let (IngestState::ISyncSearching, ETM3Header::ISync) = (state, hdr) {
            //
            // We have our ISync packet -- we can now ingest everything
            // (starting with this packet).
            //
            state = IngestState::Ingesting;
        }

        if state == IngestState::Ingesting {
            callback(&ETM3Packet {
                header: hdr,
                payload: etm_payload_decode(hdr, payload, config),
                offset: packet.offset,
                time: packet.time,
            })?;
        }

        pstate = ETM3PacketState::AwaitingHeader;

        Ok(())
    })
}
