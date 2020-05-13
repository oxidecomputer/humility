/*
 * Copyright 2020 Oxide Computer Company
 */

use crate::debug::Register;
use bitfield::bitfield;
use crate::register;
use crate::tpiu::*;
use std::error::Error;

/*
 * ITM Trace Enable Register
 */
register!(ITM_TER, 0xe000_0e00,
    #[derive(Copy, Clone)]
    #[allow(non_camel_case_types)]
    pub struct ITM_TER(u32);
    impl Debug;
    pub enabled, set_enabled: 31, 0;
);

/*
 * ITM Trace Configuration Register
 */
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

/*
 * ITM Lock Access Register
 */
register!(ITM_LAR, 0xe000_0fb0,
    #[derive(Copy, Clone)]
    #[allow(non_camel_case_types)]
    pub struct ITM_LAR(u32);
    impl Debug;
    pub key, _: 1;
);

/*
 * ITM Lock Status Register
 */
register!(ITM_LSR, 0xe000_0fb4,
    #[derive(Copy, Clone)]
    #[allow(non_camel_case_types)]
    pub struct ITM_LSR(u32);
    impl Debug;
    pub locked, _: 1;
    pub unlock_required, _: 0;
);

impl ITM_LAR {
    pub fn unlock(core: &probe_rs::Core) -> Result<(), probe_rs::Error> {
        /*
         * To unlock, we write "CoreSight Access" in l33t
         */
        let val: u32 = 0xc5_acce55;
        core.write_word_32(ITM_LAR::ADDRESS, val)?;
        Ok(())
    }

    pub fn lock(core: &probe_rs::Core) -> Result<(), probe_rs::Error> {
        let val: u32 = 0x1de_c0de;
        core.write_word_32(ITM_LAR::ADDRESS, val)?;
        Ok(())
    }
}

