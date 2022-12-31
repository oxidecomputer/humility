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

    /// probe to use
    #[clap(long, short, env = "HUMILITY_PROBE", group = "hubris")]
    pub probe: Option<String>,

    /// Hubris archive
    #[clap(long, short, env = "HUMILITY_ARCHIVE")]
    pub archive: Option<String>,

    /// Hubris dump
    #[clap(long, short, env = "HUMILITY_DUMP", group = "hubris")]
    pub dump: Option<String>,

    /// IP address of remote Hubris instance
    #[clap(long, short, env = "HUMILITY_IP", group = "hubris")]
    pub ip: Option<String>,

    /// Hubris environment file
    #[clap(long, short, env = "HUMILITY_ENVIRONMENT")]
    pub environment: Option<String>,

    /// target to use from specified environment
    #[clap(
        long,
        short,
        env = "HUMILITY_TARGET",
        requires = "environment",
        group = "hubris"
    )]
    pub target: Option<String>,

    /// If multiple archives are specified in an environment, name of
    /// the archive to use
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

    /// list targets within an environment
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
