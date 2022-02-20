// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::Result;

use crate::register_offs;
use humility::core::Core;
use num_traits::FromPrimitive;
use num_traits::ToPrimitive;

//
// SWO Current Output Divisor Register
//
register_offs!(SWO_CODR, 0x10,
    pub prescaler, set_prescaler: 15, 0;
);

//
// SWO Selected Pin Protocol Register
//
register_offs!(SWO_SPPR, 0xf0,
    pub pprot, set_pprot: 1, 0;
);

#[derive(Copy, Clone, Debug, FromPrimitive, ToPrimitive)]
pub enum SWOMode {
    Parallel = 0,
    Manchester = 1,
    NRZ = 2,
}

impl From<u32> for SWOMode {
    fn from(value: u32) -> Self {
        SWOMode::from_u32(value).unwrap()
    }
}

impl From<SWOMode> for u32 {
    fn from(mode: SWOMode) -> Self {
        SWOMode::to_u32(&mode).unwrap()
    }
}

//
// SWO Formatter and Flush Status Register
//
register_offs!(SWO_FFSR, 0x300,
    pub formatter_nonstop, _: 3;
    pub tracectl_present, _: 2;
    pub formatter_stopped, _: 1;
    pub flush_in_progress, _: 0;
);

//
// SWO Lock Access Register
//
register_offs!(SWO_LAR, 0xfb0,
    pub key, _: 1;
);

impl SWO_LAR {
    pub fn unlock(
        core: &mut dyn humility::core::Core,
        base: u32,
    ) -> Result<()> {
        //
        // To unlock, we write "CoreSight Access" in l33t
        //
        let val: u32 = 0xc5ac_ce55;
        core.write_word_32(SWO_LAR::address(base), val)?;
        Ok(())
    }

    #[allow(dead_code)]
    pub fn lock(core: &mut dyn humility::core::Core, base: u32) -> Result<()> {
        let val: u32 = 0x1de_c0de;
        core.write_word_32(SWO_LAR::address(base), val)?;
        Ok(())
    }
}
