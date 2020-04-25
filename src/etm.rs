use crate::debug::Register;
use bitfield::bitfield;
use crate::register;

macro_rules! etm_register {
    ($reg:ty, $offs:expr, $($arg:tt)*) => (
        register!($reg, 0xe004_1000 + ($offs * 4), $($arg)*);
    )
}

/*
 * ETM Main Configuration Register
 */
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

/*
 * ETM Configuration Code Register
 */
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

/*
 * ETM Status Register
 */
etm_register!(ETMSR, 0x004,
    #[derive(Copy, Clone)]
    pub struct ETMSR(u32);
    impl Debug;
    pub trigger, _: 3;
    pub trace_startstop_status, _: 2;
    pub programming_status, _: 1;
    pub untraced_overflow, _: 0;
);

/*
 * ETM System Configuration Register
 */
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

/*
 * ETM TraceEnable Event Register
 */
etm_register!(ETMTEEVR, 0x008,
    #[derive(Copy, Clone)]
    pub struct ETMTEEVR(u32);
    impl Debug;
    pub fcn, _: 16, 14;
    pub resource_b, set_resource_b: 13, 7;
    pub resource_a, set_resource_a: 6, 0;
);

/*
 * ETM TraceEnable Control 1 Register
 */
etm_register!(ETMTECR1, 0x009,
    #[derive(Copy, Clone)]
    pub struct ETMTECR1(u32);
    impl Debug;
    pub trace_control_enable, _: 25;
    pub exclude, set_exclude: 24;
    pub map_decode_select, set_map_decode_select: 23, 8;
    pub comparator_select, set_comparator_select: 7, 0;
);

/*
 * ETM FIFOFULL Region Register
 */
etm_register!(ETMFFRR, 0x00a,
    #[derive(Copy, Clone)]
    pub struct ETMFFRR(u32);
    impl Debug;
    pub exclude, set_exclude: 24;
    pub map_decode_select, set_map_decode_select: 23, 8;
    pub comparator_select, set_comparator_select: 7, 0;
);

/*
 * ETM FIFOFULL Level Register
 */
etm_register!(ETMFFLR, 0x00b,
    #[derive(Copy, Clone)]
    pub struct ETMFFLR(u32);
    impl Debug;
    pub fifo_full_level, set_fifo_full_level: 7, 0;
);

/*
 * ETM Identification Register
 */
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

/*
 * ETM Trace ID Register
 */
etm_register!(ETMTRACEIDR, 0x080,
    #[derive(Copy, Clone)]
    pub struct ETMTRACEIDR(u32);
    impl Debug;
    pub traceid, set_traceid: 6, 0;
);

/*
 * ETM Identification Register 2
 */
etm_register!(ETMIDR2, 0x082,
    #[derive(Copy, Clone)]
    pub struct ETMIDR2(u32);
    impl Debug;
    pub swp_store_before_load, _: 1;
    pub rfe_cpsr_before_pc, _: 0;
);

/*
 * ETM Lock Access Register
 */
etm_register!(ETMLAR, 0x3ec,
    #[derive(Copy, Clone)]
    pub struct ETMLAR(u32);
    impl Debug;
    pub key, _: 1;
);

impl ETMLAR {
    pub fn unlock(core: &probe_rs::Core) -> Result<(), probe_rs::Error> {
        /*
         * To unlock, we write "CoreSight Access" in l33t
         */
        let val: u32 = 0xc5_acce55;
        core.write_word_32(ETMLAR::ADDRESS, val)?;
        Ok(())
    }

    pub fn lock(core: &probe_rs::Core) -> Result<(), probe_rs::Error> {
        let val: u32 = 0x1de_c0de;
        core.write_word_32(ETMLAR::ADDRESS, val)?;
        Ok(())
    }
}

/*
 * ETM Lock Status Register
 */
etm_register!(ETMLSR, 0x3ed,
    #[derive(Copy, Clone)]
    pub struct ETMLSR(u32);
    impl Debug;
    pub locked, _: 1;
    pub unlock_required, _: 0;
);
