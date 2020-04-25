use bitfield::bitfield;
use crate::debug::Register;
use crate::register;

register!(TPIU_SSPSR, 0xe004_0000,
    #[derive(Copy, Clone)]
    #[allow(non_camel_case_types)]
    pub struct TPIU_SSPSR(u32);
    impl Debug;
    swidth, _: 31, 0;
);

/*
 * TPIU Asynchronous Clock Prescaler Register
 */
register!(TPIU_ACPR, 0xe004_0010,
    #[derive(Copy, Clone)]
    #[allow(non_camel_case_types)]
    pub struct TPIU_ACPR(u32);
    impl Debug;
    pub swoscaler, set_swoscaler: 15, 0;
);

/*
 * TPIU Selected Pin Protocol Register
 */
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

/*
 * TPIU Supported Test Patterns/Modes Register
 */
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

/*
 * TPIU Flush and Format Status Register
 */
register!(TPIU_FFSR, 0xe004_0300,
    #[derive(Copy, Clone)]
    #[allow(non_camel_case_types)]
    pub struct TPIU_FFSR(u32);
    impl Debug;
    pub tracectl_present, _: 2;
    pub formatter_stopped, _: 1;
    pub flush_in_progress, _: 0;
);

/*
 * TPIU Flush and Format Control Register
 */
register!(TPIU_FFCR, 0xe004_0304,
    #[derive(Copy, Clone)]
    #[allow(non_camel_case_types)]
    pub struct TPIU_FFCR(u32);
    impl Debug;
    pub trigin, _: 8;
    pub continuous_formatting, set_continuous_formatting: 1;
);

/*
 * TPIU Formatter Synchronization Counter Register
 */
register!(TPIU_FSCR, 0xe004_0308,
    #[derive(Copy, Clone)]
    #[allow(non_camel_case_types)]
    pub struct TPIU_FSCR(u32);
    impl Debug;
    pub counter, set_counter: 11, 0;
);

