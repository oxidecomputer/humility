use crate::debug::Register;
use bitfield::bitfield;
use crate::register;

macro_rules! etm_register {
    ($reg:ty, $offs:expr, $($arg:tt)*) => (
        register!($reg, 0xe004_1000 + ($offs * 4), $($arg)*);
    )
}

etm_register!(ETMCCR, 0x001,
    #[derive(Copy, Clone)]
    pub struct ETMCCR(u32);
    impl Debug;
    /// ETMIDR is present; should be set on ETMv2.0 and later
    pub has_etmidr, _: 31;

    /// On ETMv1.3 and earlier, protocol version
    pub protocol, _: 30, 28;

    /// On ETMv2.0 and higher, coprocessor or memory-mapped access avail
    pub can_memmap, _: 27;

    /// On ETMv2.0 and higher, indicates that trace start/stop logic present
    pub has_startstop, _: 26;

    /// Number of context ID comparators
    pub num_context_comparators, _: 25, 24;

    /// FIFOFULL logic is present
    pub has_fifofull, _: 23;
    
    /// Number of external outputs
    pub num_external_outputs, _: 22, 20;

    /// Number of external inputs
    pub num_external_inputs, _: 19, 17;

    pub has_sequencer, _: 16;
    pub num_counters, _: 15, 13;
    pub num_decoder_inputs, _: 12, 8;
    pub num_data_comparators, _: 7, 4;
    pub num_address_comparators, _: 3, 0;
);

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

etm_register!(ETMIDR2, 0x082,
    #[derive(Copy, Clone)]
    pub struct ETMIDR2(u32);
    impl Debug;
    pub swp_store_before_load, _: 1;
    pub rfe_cpsr_before_pc, _: 0;
);

