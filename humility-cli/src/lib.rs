// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

pub mod env;

use anyhow::{Context, Result, bail};
use clap::{ArgGroup, ArgMatches, Parser, parser::ValueSource};
use env::Environment;
use humility::{
    core::Core,
    hubris::{HubrisArchive, HubrisArchiveDoneness, HubrisValidate},
    msg, net, warn,
};
use std::time::Duration;

#[derive(Parser, Debug, Clone)]
#[clap(
    name = "humility", max_term_width = 80,
    group = ArgGroup::new("hubris").multiple(false),
    disable_version_flag = true,
)]
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
    pub ip: Option<net::ScopedV6Addr>,

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

    /// Subcommand to execute
    #[clap(trailing_var_arg = true)]
    pub cmd: Vec<String>,
}

impl Cli {
    /// Extracts and returns the probe serial name
    ///
    /// We can get the serial name from two different places:
    /// - If the global `--probe` argument is of the form `vid:pid:serial`, then we
    ///   can extract the serial name.  This is also the case if we're using an
    ///   environment file, which populates `args.probe` automatically.
    /// - If the `--serial` argument is given in this command's subarguments,
    ///   then we use it directly.
    ///
    /// This function checks both sources, returns an error if they conflict, and
    /// returns the (optional) serial name otherwise.
    pub fn get_probe_serial(
        &self,
        subargs_serial: Option<&str>,
    ) -> Result<Option<String>> {
        match &self.probe {
            Some(probe) => {
                let re = regex::Regex::new(
                    r"^([[:xdigit:]]+):([[:xdigit:]]+):([[:xdigit:]]+)$",
                )
                .unwrap();
                if let Some(cap) = re.captures(probe) {
                    if subargs_serial.is_some() {
                        if self.target.is_some() {
                            bail!(
                                "Cannot specify probe serial number with both \
                                 environment and `--serial`"
                            );
                        } else {
                            bail!(
                                "Cannot specify probe serial number with both \
                                 `--probe` and `--serial`"
                            );
                        }
                    }
                    Ok(Some(cap.get(3).unwrap().as_str().to_string()))
                } else {
                    bail!(
                        "`--probe` argument must be of the form
                         `vid:pid:serial`, not {probe}",
                    );
                }
            }
            None => Ok(subargs_serial.map(|s| s.to_owned())),
        }
    }

    /// Loads an archive based on CLI arguments
    ///
    /// If `--archive` is specified, then the archive is loaded (and cooked to
    /// the desired doneness).
    ///
    /// If `--dump` is specified, then the dump is loaded and the archive is
    /// extracted from the dump (then cooked to the desired doneness).
    ///
    /// If neither is specified, then a default archive is returned, which
    /// returns `false` from [`HubrisArchive::loaded`].
    // TODO(matt): this is terrible and I want to make it go away, but it won't
    // happen quite yet.
    fn load_archive_with_doneness(
        &self,
        doneness: HubrisArchiveDoneness,
    ) -> Result<HubrisArchive> {
        let mut hubris = HubrisArchive::new();
        if let Some(archive) = &self.archive {
            hubris.load(archive, doneness).with_context(|| {
                format!("failed to load archive \"{}\"", archive)
            })?;
        } else if let Some(dump) = &self.dump {
            hubris
                .load_dump(dump, doneness)
                .with_context(|| format!("failed to load dump \"{}\"", dump))?;
        }
        Ok(hubris)
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
            humility_net_core::attach_net(*ip, hubris, timeout)
        } else {
            self.attach_probe(hubris)
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
    ) -> Result<Box<dyn Core>> {
        let probe = match self.probe.as_deref() {
            Some("archive") => {
                if let Some(hubris) = hubris {
                    return humility::core::attach_archive(hubris);
                } else {
                    bail!("cannot specify `--probe=archive` with no archive");
                }
            }
            Some(p) => p,
            None => "auto",
        };

        let chip = hubris.and_then(|h| h.chip());
        humility_probes_core::attach_to_chip(probe, chip.as_deref(), self.speed)
    }

    #[cfg(not(feature = "probes"))]
    pub fn attach_probe(
        &self,
        _hubris: Option<&HubrisArchive>,
    ) -> Result<Box<dyn Core>> {
        bail!("Did not build with probes!");
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
            humility::core::attach_dump(dump)?
        } else if let Some(ip) = &self.ip {
            let Some(hubris) = hubris else {
                bail!("cannot connect over the network without archive");
            };
            let timeout = Duration::from_millis(self.timeout as u64);
            humility_net_core::attach_net(*ip, hubris, timeout)?
        } else {
            self.attach_probe(hubris)?
        };
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
    pub fn attach_dump(&self) -> Result<Box<dyn Core>> {
        let core = if let Some(dump) = &self.dump {
            humility::core::attach_dump(dump)?
        } else {
            bail!("must be run against a dump");
        };
        Ok(core)
    }

    /// Returns a [`HubrisArchive`] built from the context's [`Cli`] data
    ///
    /// If loading the archive fails, then an error is returned
    pub fn archive(&self) -> Result<HubrisArchive> {
        let a = self.load_archive_with_doneness(HubrisArchiveDoneness::Cook)?;
        if !a.loaded() {
            if self.environment.is_some() {
                bail!("must provide a Hubris archive, dump, or name");
            } else {
                bail!("must provide a Hubris archive or dump");
            }
        }
        Ok(a)
    }

    /// Tries to load an archive
    ///
    /// Returns `Ok(None)` if no archive was specified at the CLI.
    pub fn try_archive(&self) -> Result<Option<HubrisArchive>> {
        let a = self.load_archive_with_doneness(HubrisArchiveDoneness::Cook)?;
        if !a.loaded() { Ok(None) } else { Ok(Some(a)) }
    }

    /// Returns a raw archive built from the context's [`Cli`] data
    ///
    /// The raw archive is equivalent to the bytes of a ZIP file
    ///
    /// If loading the archive fails, then an error is returned
    pub fn raw_archive(&self) -> Result<Vec<u8>> {
        let a = self.load_archive_with_doneness(HubrisArchiveDoneness::Raw)?;
        let out = a.take_raw_archive();
        if out.is_empty() {
            bail!("no archive available");
        } else {
            Ok(out)
        }
    }
}

pub struct ExecutionContext {
    pub environment: Option<Environment>,
    pub cli: Cli,
}

impl ExecutionContext {
    pub fn new(mut cli: Cli, m: &ArgMatches) -> Result<ExecutionContext> {
        //
        // Before we do our processing, we need to check for the presence of
        // environment variables on our mutually exclusive attach options
        // (i.e., dump/probe/ip/target).  Clap has support for options being
        // set as environment variables, so why are we doing it here?  We do
        // this manually because Clap's behavior when an option has a
        // specified environment variable is to treat a variable that is in
        // the environment just as if the option had been set on the command
        // line -- but that isn't actually what we want here: if an option is
        // set on the command line, we want to ignore the fact that a
        // different mutually exclusive option may be in the environment.  So
        // we only look at the environment if none of these is set (Clap
        // assures that at most one is set on the command line), and we
        // examine the environment in an order of precence: probe, ip, target,
        // dump.  If the user has specified no command line options but
        // multiple of these in the environment, they will get the first in
        // this ordering that we find.
        //
        if cli.dump.is_none()
            && cli.probe.is_none()
            && cli.ip.is_none()
            && cli.target.is_none()
        {
            use std::env;

            if let Ok(e) = env::var("HUMILITY_PROBE") {
                cli.probe = Some(e);
            } else if let Ok(e) = env::var("HUMILITY_IP") {
                cli.ip =
                    Some(e.parse().context("could not parse HUMILITY_IP")?);
            } else if let Ok(e) = env::var("HUMILITY_TARGET") {
                cli.target = Some(e);
            } else if let Ok(e) = env::var("HUMILITY_DUMP") {
                cli.dump = Some(e);
            }
        }

        let environment = match (&cli.environment, &cli.target) {
            (Some(env), Some(target)) => {
                let env = match Environment::from_file(env, target) {
                    Ok(e) => e,
                    Err(err) => {
                        msg!("failed to match environment: {err:?}");
                        std::process::exit(1);
                    }
                };

                //
                // Cannot specify a dump/probe/IP address and also an
                // environment and target
                //
                assert!(cli.dump.is_none());
                assert!(cli.probe.is_none());
                assert!(cli.ip.is_none());

                cli.probe = Some(env.probe.clone());

                //
                // If we have an archive on the command-line or in an environment
                // variable, we want to prefer that over whatever is in the
                // environment file -- but we also want to warn the user about
                // what is going on.
                //
                if cli.archive.is_some() {
                    let msg = if m.value_source("archive")
                        == Some(ValueSource::CommandLine)
                    {
                        "archive on command-line"
                    } else {
                        "archive in environment variable"
                    };

                    warn!("{} overriding archive in environment file", msg);
                } else {
                    cli.archive = match env.archive(&cli.archive_name) {
                        Ok(a) => Some(a),
                        Err(e) => {
                            msg!("Failed to get archive: {e}");
                            std::process::exit(1);
                        }
                    }
                }

                Some(env)
            }

            (Some(env), None) => {
                if cli.list_targets {
                    let targets = match Environment::targets(env) {
                        Ok(targets) => targets,
                        Err(err) => {
                            msg!("failed to parse environment: {err:?}");
                            std::process::exit(1);
                        }
                    };

                    if cli.terse {
                        println!(
                            "{}",
                            targets
                                .iter()
                                .map(|(t, _)| &**t)
                                .collect::<Vec<_>>()
                                .join(", ")
                        );
                    } else {
                        println!("{:15} DESCRIPTION", "TARGET");

                        for (target, description) in &targets {
                            println!(
                                "{:15} {}",
                                target,
                                match description {
                                    Some(d) => d,
                                    None => "-",
                                }
                            );
                        }
                    }

                    std::process::exit(0);
                }

                if let Err(err) = Environment::validate(env) {
                    eprintln!("failed to parse environment: {:?}", err);
                    std::process::exit(1);
                }

                None
            }

            _ => None,
        };

        if cli.cmd.is_empty() {
            eprintln!("humility failed: subcommand expected (--help to list)");
            std::process::exit(1);
        }

        //
        // Check to see if we have both a dump and an archive.  Because these
        // conflict with one another but because we allow both of them to be
        // set with an environment variable, we need to manually resolve this:
        // we want to allow an explicitly set value (that is, via the command
        // line) to win the conflict.
        //
        if cli.dump.is_some() && cli.archive.is_some() {
            match (
                m.value_source("dump") == Some(ValueSource::CommandLine),
                m.value_source("archive") == Some(ValueSource::CommandLine),
            ) {
                (true, true) => {
                    msg!("cannot specify both a dump and an archive");
                    std::process::exit(1);
                }

                (false, false) => {
                    msg!(
                        "both dump and archive have been set via environment \
                        variables; unset one of them, or use a command-line \
                        option to override"
                    );
                    std::process::exit(1);
                }

                (true, false) => {
                    warn!(
                        "dump on command-line overriding archive in environment"
                    );
                    cli.archive = None;
                }

                (false, true) => {
                    warn!(
                        "archive on command-line overriding dump in environment"
                    );
                    cli.dump = None;
                }
            }
        }

        Ok(ExecutionContext { environment, cli })
    }
}
