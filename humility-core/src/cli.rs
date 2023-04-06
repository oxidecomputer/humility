// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use clap::{AppSettings, ArgGroup, Parser};

#[derive(Parser, Debug, Clone)]
#[clap(
    name = "humility", max_term_width = 80,
    group = ArgGroup::new("hubris").multiple(false)
)]
#[clap(global_setting(AppSettings::NoAutoVersion))]
pub struct Cli {
    /// verbose messages
    #[clap(long, short)]
    pub verbose: bool,

    /// terse output
    #[clap(long, short = 'T', hide = true)]
    pub terse: bool,

    /// sets timeout for Hubris-related operations
    #[clap(
        long, default_value_t = 2000, value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    pub timeout: u32,

    /// print version information
    #[clap(long, short = 'V')]
    pub version: bool,

    /// If a system has multiple debug probes attached, the specific probe
    /// to use.  If specifying a USB probe, its index can be used (e.g.,
    /// "usb-0"); if specifying an exact probe, this is of the form
    /// "vid:pid[:serial]". This may also be set via the HUMILITY_PROBE
    /// environment variable. Run "humility doc" for more information on
    /// probes.
    #[clap(long, short, group = "hubris")]
    pub probe: Option<String>,

    /// File that contains the Hubris archive. This may also be set via
    /// the HUMILITY_ARCHIVE environment variable. Run "humility doc" for
    /// more information on Hubris archives.
    #[clap(long, short, env = "HUMILITY_ARCHIVE", hide_env = true)]
    pub archive: Option<String>,

    /// Hubris dump. This may also be set via the HUMILITY_DUMP environment
    /// variable. Run "humility doc" for more information on debugging
    /// from a hubris dump.
    #[clap(long, short, group = "hubris")]
    pub dump: Option<String>,

    /// IP address of remote Hubris instance. This may also be set via the
    /// HUMILITY_IP environment variable. Run "humility doc" for more
    /// information on running Humility over a network.
    #[clap(long, short, group = "hubris")]
    pub ip: Option<String>,

    /// Hubris environment file. Thie may also be set via the
    /// HUMILITY_ENVIRONMENT environment variable. Run "humility doc" for
    /// more information on Humility environments.
    #[clap(long, short, env = "HUMILITY_ENVIRONMENT", hide_env = true)]
    pub environment: Option<String>,

    /// Target to use from a specified environment. This may also be set via
    /// the HUMILITY_TARGET environment variable. Run "humility doc" for
    /// for information on specifying targets within an environment.
    #[clap(long, short, requires = "environment", group = "hubris")]
    pub target: Option<String>,

    /// If multiple archives are specified in an environment, name of
    /// the archive to use.  Run "humility doc" for more information on
    /// Humility environments and their relationship to archives.
    #[clap(long, requires = "environment")]
    pub archive_name: Option<String>,

    //
    // probe-rs requires the chip to be specified when creating a session,
    // even though it is only used for flashing (which we don't use probe-rs
    // to do).  Historically, we had a `-c` option to specify this, but its
    // presence was causing confusion and it has been deprecated.  However,
    // Hubris uses Humility to flash, and specifies this option.  Because we
    // may want to use this properly in the future (namely, if/when we use
    // probe-rs to flash), we continue to accept this option (and test for its
    // presence), thereby eliminating two potential Hubris flag days.  (Until
    // it once again means something, we hide the option from the help
    // output.)
    //
    #[clap(long, short, env = "HUMILITY_CHIP", hide = true)]
    pub chip: Option<String>,

    /// List targets within an environment. Run "humility doc" for more
    /// information on Humility environments.
    #[clap(
        long = "list-targets",
        requires = "environment",
        conflicts_with = "hubris"
    )]
    pub list_targets: bool,

    #[clap(subcommand)]
    pub cmd: Option<Subcommand>,
}

#[derive(Parser, Debug, Clone)]
pub enum Subcommand {
    #[clap(external_subcommand)]
    Other(Vec<String>),
}
