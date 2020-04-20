use bitfield::bitfield;
use std::mem::size_of;

bitfield! {
    #[derive(Copy, Clone)]
    pub struct Etmccr(u32);
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
}

impl From<u32> for Etmccr {
    fn from(value: u32) -> Self {
        Self(value)
    }
}

impl From<Etmccr> for u32 {
    fn from(reg: Etmccr) -> Self {
        reg.0
    }
}

bitfield! {
    #[derive(Copy, Clone)]
    pub struct Etmidr(u32);
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
}

impl From<u32> for Etmidr {
    fn from(value: u32) -> Self {
        Self(value)
    }
}

impl From<Etmidr> for u32 {
    fn from(reg: Etmidr) -> Self {
        reg.0
    }
}

bitfield! {
    #[derive(Copy, Clone)]
    pub struct Etmidr2(u32);
    impl Debug;
    pub swp_store_before_load, _: 1;
    pub rfe_cpsr_before_pc, _: 0;
}

impl From<u32> for Etmidr2 {
    fn from(value: u32) -> Self {
        Self(value)
    }
}

impl From<Etmidr2> for u32 {
    fn from(reg: Etmidr2) -> Self {
        reg.0
    }
}

pub fn read_etm_reg(core: &probe_rs::Core, etm: u32, regno: u32)
    -> Result<u32, probe_rs::Error>
{
    core.read_word_32(etm + regno * size_of::<u32>() as u32)
}

