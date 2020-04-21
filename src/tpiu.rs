use bitfield::bitfield;
use crate::debug::Register;
use crate::register;

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
            TPIUMode::NRZ => 2
        };

        self._set_txmode(val);
    }
}
