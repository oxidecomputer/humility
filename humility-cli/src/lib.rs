// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

pub mod env;

use anyhow::{Result, anyhow, bail};
use clap::Parser;
use env::Environment;
use humility::{
    core::Core,
    hubris::{HubrisArchive, HubrisValidate},
};
use humility_net::{ScopedV6Addr, ScopedV6AddrError};
use std::{str::FromStr, time::Duration};

#[derive(Parser, Debug, Clone)]
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
        value_parser = parse_int::parse::<u32>,
    )]
    pub timeout: u32,

    /// print version information
    #[clap(long, short = 'V')]
    pub version: bool,

    /// If a system has multiple debug probes attached, the specific probe to
    /// use.  If specifying a USB probe, its index can be used (e.g.,
    /// "usb-0"); if specifying an exact probe, this is of the form
    /// "vid:pid[:serial]". If set to "archive", the archive is used rather
    /// than any attached debug probe. This may also be set via the
    /// HUMILITY_PROBE environment variable. Run "humility doc" for more
    /// information on probes.
    #[clap(long, short, group = "hubris")]
    pub probe: Option<String>,

    /// When using a hardware debug probe, set the max speed of the SWD/JTAG
    /// interface on the debug probe in kilohertz.
    #[clap(long, short, value_name = "speed_khz")]
    pub speed: Option<u32>,

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
    pub ip: Option<ScopedV6AddrResult>,

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

    /// Handle to a lazily-initialized Humility logger
    #[clap(skip)]
    log: std::cell::OnceCell<humility::log::Logger>,
}

impl Cli {
    /// Loads an archive based on CLI arguments
    ///
    /// If `--archive` is specified, then that archive is loaded.
    ///
    /// If `--dump` is specified, then the dump is loaded and the archive is
    /// extracted from the dump.
    ///
    /// Otherwise, returns `Ok(None)`
    pub fn try_archive(&self) -> Result<Option<HubrisArchive>> {
        if let Some(archive) = &self.archive {
            let data = std::fs::read(archive)?;
            let raw_archive = hubtools::RawHubrisArchive::from_vec(data)?;
            HubrisArchive::load(raw_archive, None, self.log()).map(Some)
        } else if let Some(dump) = &self.dump {
            let (raw_archive, dump_task) = HubrisArchive::load_dump(dump)?;
            HubrisArchive::load(raw_archive, dump_task, self.log()).map(Some)
        } else {
            Ok(None)
        }
    }

    /// Tries to load a raw archive based on CLI arguments
    ///
    /// If `--archive` is specified, then that archive is loaded.
    ///
    /// If `--dump` is specified, then the dump is loaded and the archive is
    /// extracted from the dump.
    ///
    /// Otherwise, returns `Ok(None)`
    fn try_raw_archive(&self) -> Result<Option<hubtools::RawHubrisArchive>> {
        if let Some(archive) = &self.archive {
            let data = std::fs::read(archive)?;
            let raw_archive = hubtools::RawHubrisArchive::from_vec(data)?;
            Ok(Some(raw_archive))
        } else if let Some(dump) = &self.dump {
            let (raw_archive, _) = HubrisArchive::load_dump(dump)?;
            Ok(Some(raw_archive))
        } else {
            Ok(None)
        }
    }

    /// Attaches to a live core
    ///
    /// Uses the network-based core if `--ip` is provided in the [`Cli`];
    /// otherwises attaches to a probe.
    pub fn attach_live(
        &self,
        hubris: Option<&HubrisArchive>,
        validate: Option<HubrisValidate>,
    ) -> Result<Box<dyn Core>> {
        let mut core = if self.dump.is_some() {
            bail!("must be run against a live system");
        } else if let Some(ip) = &self.ip {
            let Some(hubris) = hubris else {
                bail!("cannot attach over net without Hubris archive");
            };
            let timeout = Duration::from_millis(self.timeout as u64);
            humility_net::attach_net(ip.0.clone()?, hubris, timeout, self.log())
                .map(|b| Box::new(b) as Box<dyn Core>)
        } else {
            #[cfg(feature = "probes")]
            {
                self.attach_probe(hubris).map(|b| Box::new(b) as Box<dyn Core>)
            }
            #[cfg(not(feature = "probes"))]
            {
                Err(anyhow!(
                    "humility was compiled without the \"probes\" feature; \
                     an IP address or dump is required"
                ))
            }
        }?;
        if let Some(validate) = validate {
            let Some(hubris) = hubris else {
                bail!("cannot validate without Hubris archive");
            };
            hubris.validate(&mut *core, validate)?;
        }
        Ok(core)
    }

    /// Attaches to a live core and check that the firmware id matches
    ///
    /// This is a thin wrapper around [`attach_live`](Self::attach_live)
    pub fn attach_live_match(
        &self,
        hubris: &HubrisArchive,
    ) -> Result<Box<dyn Core>> {
        self.attach_live(Some(hubris), Some(HubrisValidate::ArchiveMatch))
    }

    /// Attaches to a live core and check that it has booted
    ///
    /// This is a thin wrapper around [`attach_live`](Self::attach_live)
    pub fn attach_live_booted(
        &self,
        hubris: &HubrisArchive,
    ) -> Result<Box<dyn Core>> {
        self.attach_live(Some(hubris), Some(HubrisValidate::Booted))
    }

    #[cfg(feature = "probes")]
    pub fn attach_probe(
        &self,
        hubris: Option<&HubrisArchive>,
    ) -> Result<humility_probes_core::ProbeCore> {
        let probe = self.probe.as_deref().unwrap_or("auto");
        let chip = hubris.map(|h| h.chip()).transpose()?;
        humility_probes_core::attach_to_chip(
            probe,
            chip.as_deref(),
            self.speed,
            self.log(),
        )
    }

    /// Attaches to a either a dump or a live system, depending on CLI arguments
    ///
    /// The `hubris` archive is mandatory if `validate` is `Some(..)` or if we
    /// are trying to connect over the network.
    pub fn attach_live_or_dump(
        &self,
        hubris: Option<&HubrisArchive>,
        validate: Option<HubrisValidate>,
    ) -> Result<Box<dyn Core>> {
        let mut core = if let Some(dump) = &self.dump {
            humility::core::attach_dump(dump, self.log())
                .map(|b| Box::new(b) as Box<dyn Core>)
        } else if let Some(ip) = &self.ip {
            let Some(hubris) = hubris else {
                bail!("cannot connect over the network without archive");
            };
            let timeout = Duration::from_millis(self.timeout as u64);
            humility_net::attach_net(ip.0.clone()?, hubris, timeout, self.log())
                .map(|b| Box::new(b) as Box<dyn Core>)
        } else {
            #[cfg(feature = "probes")]
            {
                self.attach_probe(hubris).map(|b| Box::new(b) as Box<dyn Core>)
            }
            #[cfg(not(feature = "probes"))]
            {
                Err(anyhow!(
                    "humility was compiled without the \"probes\" feature; \
                     an IP address or dump is required"
                ))
            }
        }?;
        if let Some(validate) = validate {
            let Some(hubris) = hubris else {
                bail!("cannot validate without Hubris archive");
            };
            hubris.validate(&mut *core, validate)?;
        }
        Ok(core)
    }

    /// Attaches to a dump or a live system and checks that it has booted
    ///
    /// This is a thin wrapper around
    /// [`attach_live_or_dump`](Self::attach_live_or_dump).
    pub fn attach_live_or_dump_booted(
        &self,
        hubris: &HubrisArchive,
    ) -> Result<Box<dyn Core>> {
        self.attach_live_or_dump(Some(hubris), Some(HubrisValidate::Booted))
    }

    /// Attaches to a dump or a live system and validates the version
    ///
    /// This is a thin wrapper around
    /// [`attach_live_or_dump`](Self::attach_live_or_dump).
    pub fn attach_live_or_dump_match(
        &self,
        hubris: &HubrisArchive,
    ) -> Result<Box<dyn Core>> {
        self.attach_live_or_dump(
            Some(hubris),
            Some(HubrisValidate::ArchiveMatch),
        )
    }

    /// Attaches to a dump
    ///
    /// Reads from the `--dump` argument to pick a target file
    pub fn attach_dump(&self) -> Result<humility::dump::DumpCore> {
        let core = if let Some(dump) = &self.dump {
            humility::core::attach_dump(dump, self.log())?
        } else {
            bail!("must be run against a dump");
        };
        Ok(core)
    }

    /// Returns a [`HubrisArchive`] built from the context's [`Cli`] data
    ///
    /// If loading the archive fails, then an error is returned
    pub fn archive(&self) -> Result<HubrisArchive> {
        self.try_archive()?.ok_or_else(|| {
            if self.environment.is_some() {
                anyhow!("must provide a Hubris archive, dump, or name")
            } else {
                anyhow!("must provide a Hubris archive or dump")
            }
        })
    }

    /// Returns a raw archive built from the context's [`Cli`] data
    ///
    /// The raw archive is equivalent to the bytes of a ZIP file
    ///
    /// If loading the archive fails, then an error is returned
    pub fn raw_archive(&self) -> Result<hubtools::RawHubrisArchive> {
        self.try_raw_archive()?.ok_or_else(|| {
            if self.environment.is_some() {
                anyhow!("must provide a Hubris archive, dump, or name")
            } else {
                anyhow!("must provide a Hubris archive or dump")
            }
        })
    }

    pub fn log(&self) -> &humility::log::Logger {
        self.log.get_or_init(|| humility::log::init(self.verbose))
    }
}

/// Promotes an argument type and run function into a Humility subcommand
///
/// The `$args_ty` value must implement `clap::Parser`, and `$run_fn` must have
/// the signature `fn run(args: $args_ty, ctx: &mut ExecutionContext) ->
/// anyhow::Result<()>`.
#[macro_export]
macro_rules! humility_cmd {
    ($args_ty:ty, $run_fn:ident) => {
        pub type Args = $args_ty;
        impl Args {
            pub fn run(
                args: Self,
                context: &mut ::humility_cli::ExecutionContext,
            ) -> ::anyhow::Result<()> {
                $run_fn(args, context)
            }
        }
    };
}

pub struct ExecutionContext {
    pub environment: Option<Environment>,
    pub cli: Cli,
}

impl ExecutionContext {
    pub fn log(&self) -> &humility::log::Logger {
        self.cli.log()
    }
}

/// Wrapper which contains a parsed `ScopedV6Addr` or a parse error
///
/// This allows us to defer handling the parse error for an `--ip` argument if
/// it's not actually needed.
#[derive(Clone, Debug)]
pub struct ScopedV6AddrResult(pub Result<ScopedV6Addr, ScopedV6AddrError>);

impl FromStr for ScopedV6AddrResult {
    type Err = std::convert::Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self(s.parse())) // defer to ScopedV6Addr::from_str
    }
}
