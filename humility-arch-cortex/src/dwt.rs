// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use crate::debug::Register;
use crate::register;
use bitfield::bitfield;
use humility::core::Core;

/*
 * DWT Control Register
 */
register!(DWT_CTRL, 0xe000_1000,
    #[derive(Copy, Clone)]
    #[allow(non_camel_case_types)]
    pub struct DWT_CTRL(u32);
    impl Debug;
    pub num_comparators, _: 31, 28;
    pub no_trace_sampling, _: 27;
    pub no_external_trigger, _: 26;
    pub no_cycle_counter, _: 25;
    pub no_profiling_counter, _: 24;
    pub postcnt_enabled, _: 22;
    pub folded_enabled, _: 21;
    pub lsu_enabled, _: 20;
    pub sleep_enabled, _: 19;
    pub exception_enabled, _: 18;
    pub cpi_enabled, _: 17;
    pub exception_trace_enabled, _: 16;
    pub pc_sampling_enabled, _: 12;
    pub _synctap, _set_synctap: 11, 10;
    pub postcnt_tap, _: 9;
    pub postcnt_init, _: 8, 5;
    pub postcnt_reset, _: 4, 1;
    pub cyccnt_enabled, set_cyccnt_enabled: 0;
);

pub enum DWTSyncTapFrequency {
    Disabled,
    CycCnt8M,   // Every 2^23rd (8M) cycles
    CycCnt32M,  // Every 2^25th (32M) cycles
    CycCnt128M, // Every 2^27th (~128M) cycles
}

impl DWT_CTRL {
    pub fn set_synctap(&mut self, synctap: DWTSyncTapFrequency) {
        let val = match synctap {
            DWTSyncTapFrequency::Disabled => 0b00,
            DWTSyncTapFrequency::CycCnt8M => 0b01,
            DWTSyncTapFrequency::CycCnt32M => 0b10,
            DWTSyncTapFrequency::CycCnt128M => 0b11,
        };

        self._set_synctap(val);
    }
}
