// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility ignition`
//!
//! Tools to interact with the Ignition subsystem

use std::default::Default;

use drv_ignition_api::*;
use humility::cli::Subcommand;
use humility::core::Core;
use humility::hubris::*;
use humility::reflect::*;
use humility_cmd::hiffy::HiffyContext;
use humility_cmd::idol::IdolArgument;
use humility_cmd::idol::IdolOperation;
use humility_cmd::{Archive, Attach, Command, Validate};

use anyhow::{bail, Context, Result};
use clap::{Command as ClapCommand, CommandFactory, Parser};
use colored::Colorize;

#[derive(Parser, Debug)]
#[clap(name = "ignition", about = env!("CARGO_PKG_DESCRIPTION"))]
struct IgnitionArgs {
    /// sets timeout
    #[clap(
        long, short = 'T', default_value = "15000", value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    #[clap(subcommand)]
    cmd: IgnitionCommand,
}

#[derive(Parser, Debug)]
enum IgnitionCommand {
    /// Clear link events for a port
    ClearLinkEvents { port: u8 },
    /// Show the status of all Controller ports
    Status,
    /// Show the state of a port
    Show {
        port: u8,
        /// Show port counters
        #[clap(long, short)]
        counters: bool,
        /// Show link events
        #[clap(long, short = 'e')]
        link_events: bool,
    },
    /// Power off a Target
    PowerOff { port: u8 },
    /// Power on a Target
    PowerOn { port: u8 },
    /// Reset power of a Target
    PowerReset { port: u8 },
}

pub struct IgnitionHandler<'a> {
    hubris: &'a HubrisArchive,
    core: &'a mut dyn Core,
    context: HiffyContext<'a>,
}

impl<'a> IgnitionHandler<'a> {
    pub fn new(
        hubris: &'a HubrisArchive,
        core: &'a mut dyn Core,
        hiffy_timeout: u32,
    ) -> Result<Self> {
        let context = HiffyContext::new(hubris, core, hiffy_timeout)?;
        Ok(Self { hubris, core, context })
    }

    fn get_idol_command(&self, name: &str) -> Result<IdolOperation<'a>> {
        IdolOperation::new(self.hubris, "Ignition", name, None).with_context(
            || {
                format!(
                    "Could not find `Ignition.{}`, \
                     is your Hubris archive new enough?",
                    name
                )
            },
        )
    }

    fn get_state(&mut self, port: u8) -> Result<PortState> {
        let op = self.get_idol_command("state")?;
        match humility_cmd_hiffy::hiffy_call(
            self.hubris,
            self.core,
            &mut self.context,
            &op,
            &[("port", IdolArgument::Scalar(u64::from(port)))],
            None,
        )? {
            Ok(v) => u64::from_value(&(*v.as_tuple()?)[0]).map(PortState),
            Err(e) => bail!("Got Hubris error: {:?}", e),
        }
    }

    fn get_state_dump(&mut self) -> Result<Vec<PortState>> {
        let port_count_op = self.get_idol_command("port_count")?;
        let port_count = match humility_cmd_hiffy::hiffy_call(
            self.hubris,
            self.core,
            &mut self.context,
            &port_count_op,
            &[],
            None,
        )? {
            Ok(v) => u8::from_value(&v).unwrap() as usize,
            Err(e) => bail!("Got Hubris error: {:?}", e),
        };

        let state_dump_op = self.get_idol_command("state_dump")?;
        let state_dump = match humility_cmd_hiffy::hiffy_call(
            self.hubris,
            self.core,
            &mut self.context,
            &state_dump_op,
            &[],
            None,
        )? {
            Ok(v) => {
                let reply = v.as_struct()?;

                if reply.name() != "Ignition_state_dump_REPLY"
                    || reply.len() != 1
                {
                    panic!(
                        "Expected \"Ignition_state_dump_REPLY\", got {}",
                        reply.name()
                    );
                }

                // The reply has only a single member with the array we're
                // after.
                reply
                    .iter()
                    .next()
                    .map(|(_, data)| <[u64; PORT_MAX]>::from_value(data))
                    .unwrap()
                    .unwrap()
            }
            Err(e) => bail!("Got Hubris error: {:?}", e),
        };

        Ok(state_dump[..port_count].iter().map(|s| PortState(*s)).collect())
    }

    fn get_counters(&mut self, port: u8) -> Result<Counters> {
        let op = self.get_idol_command("counters")?;
        match humility_cmd_hiffy::hiffy_call(
            self.hubris,
            self.core,
            &mut self.context,
            &op,
            &[("port", IdolArgument::Scalar(u64::from(port)))],
            None,
        )? {
            Ok(v) => {
                let s = v.as_struct()?;
                let get_count = |name| u8::from_value(&s[name]).unwrap();

                Ok(Counters {
                    status_received: get_count("status_received"),
                    hello_sent: get_count("hello_sent"),
                    request_sent: get_count("request_sent"),
                    message_dropped: get_count("message_dropped"),
                })
            }
            Err(e) => bail!("Got Hubris error: {:?}", e),
        }
    }

    fn get_link_events(
        &mut self,
        port: u8,
        txr: TransceiverSelect,
    ) -> Result<LinkEvents> {
        let op = self.get_idol_command("link_events")?;
        match humility_cmd_hiffy::hiffy_call(
            self.hubris,
            self.core,
            &mut self.context,
            &op,
            &[
                ("port", IdolArgument::Scalar(u64::from(port))),
                ("txr", IdolArgument::String(&txr.to_string())),
            ],
            None,
        )? {
            Ok(v) => u8::from_value(&(*v.as_tuple()?)[0]).map(LinkEvents),
            Err(e) => bail!("Got Hubris error: {:?}", e),
        }
    }

    pub fn port_request(&mut self, port: u8, request: Request) -> Result<()> {
        let request_op = self.get_idol_command("send_request")?;
        let request_str = format!("{:?}", request);

        match humility_cmd_hiffy::hiffy_call(
            self.hubris,
            self.core,
            &mut self.context,
            &request_op,
            &[
                ("port", IdolArgument::Scalar(u64::from(port))),
                ("request", IdolArgument::String(request_str.as_str())),
            ],
            None,
        )? {
            Ok(v) => {
                v.as_base()?;
                Ok(())
            }
            Err(e) => bail!("Got Hubris error: {:?}", e),
        }
    }

    pub fn status(&mut self) -> Result<()> {
        let state_dump = self.get_state_dump()?;

        println!("{}", "PORT | TYPE            POWER_STATE  FAULTS".bold());
        println!("{}", "-----|-------------------------------------".bold());
        for (port, state) in state_dump.iter().enumerate() {
            print!(" {:<3} | ", port);

            if state.target_present() {
                let target = state.target().unwrap();

                let sys_type_str = match SystemType::from(target.id()) {
                    SystemType::Unknown => {
                        format!("Unknown ({:02x}h)", target.id())
                    }
                    sys_type => sys_type.to_string(),
                };
                let power_state_str =
                    if target.system_power_state() == PowerState::On {
                        "On".green()
                    } else if target.system_power_abort() {
                        "Off".red()
                    } else {
                        "Off".yellow()
                    };
                let faults_str = if target.faults().count() == 0 {
                    "0".green()
                } else {
                    format!("{}", target.faults().count()).red()
                };

                print!(
                    "{:<14}  {:<11}  {:<6}",
                    sys_type_str, power_state_str, faults_str,
                );
            } else {
                print!("-");
            }

            println!();
        }

        Ok(())
    }

    fn pretty_print_link(
        status: ReceiverStatus,
        peer_type: &str,
        present: bool,
        link_events: Option<LinkEvents>,
        indent: usize,
    ) {
        println!(
            "{:indent$}Receiver: {}",
            "",
            if status.locked() {
                if status.polarity_inverted() {
                    "locked, polarity inverted"
                } else {
                    "locked"
                }
            } else if status.aligned() {
                "aligned"
            } else {
                "awaiting-peer"
            }
        );
        println!("{:indent$}{} Present: {}", "", peer_type, present);

        if let Some(events) = link_events {
            println!("{:indent$}Link Events: {}", "", events.count());

            {
                let indent = indent + 4;

                if events.encoding_error() {
                    println!("{:indent$}Encoding Error", "");
                }
                if events.decoding_error() {
                    println!("{:indent$}Decoding Error", "");
                }
                if events.ordered_set_invalid() {
                    println!("{:indent$}Ordered Set Invalid", "");
                }
                if events.message_version_invalid() {
                    println!("{:indent$}Message Version Invalid", "");
                }
                if events.message_type_invalid() {
                    println!("{:indent$}Message Type Invalid", "");
                }
                if events.message_checksum_invalid() {
                    println!("{:indent$}Message Checksum Invalid", "");
                }
            }
        }
    }

    pub fn show_port(
        &mut self,
        port: u8,
        show_counters: bool,
        show_link_events: bool,
    ) -> Result<()> {
        let state = self.get_state(port)?;
        let counters = if show_counters {
            self.get_counters(port)?
        } else {
            Default::default()
        };
        let controller_link_events = if show_link_events {
            Some(self.get_link_events(port, TransceiverSelect::Controller)?)
        } else {
            None
        };
        let target_link0_events = if show_link_events {
            Some(self.get_link_events(port, TransceiverSelect::TargetLink0)?)
        } else {
            None
        };
        let target_link1_events = if show_link_events {
            Some(self.get_link_events(port, TransceiverSelect::TargetLink1)?)
        } else {
            None
        };

        let indent = 4usize;

        println!("Port: {}", port);
        Self::pretty_print_link(
            state.receiver_status(),
            "Target",
            state.target_present(),
            controller_link_events,
            0,
        );

        if show_counters {
            println!("Counters:");
            println!(
                "{:indent$}Status Received: {}",
                "", counters.status_received,
            );
            println!("{:indent$}Hello Sent: {}", "", counters.hello_sent,);
            println!("{:indent$}Request Sent: {}", "", counters.request_sent,);
            println!(
                "{:indent$}Message Dropped: {}",
                "", counters.message_dropped,
            );
        }

        println!();

        if let Some(target) = state.target() {
            println!("Sytem Type: {}", SystemType::from(target.id()));
            println!("Sytem Id: 0x{:02x}", target.id());
            println!("System Faults: {}", target.faults().count());
            if target.faults().power_a3() {
                println!("{:indent$}Power A3", "");
            }
            if target.faults().power_a2() {
                println!("{:indent$}Power A2", "");
            }
            if target.faults().rot() {
                println!("{:indent$}RoT", "");
            }
            if target.faults().sp() {
                println!("{:indent$}SP", "");
            }

            println!("System Power:");
            println!("{:indent$}State: {}", "", target.system_power_state());
            println!("{:indent$}Abort: {}", "", target.system_power_abort());
            println!(
                "{:indent$}Request: {}{}",
                "",
                if target.system_power_reset_in_progress() {
                    "power-reset, "
                } else {
                    ""
                },
                if target.system_power_off_in_progress() {
                    "power-off"
                } else if target.system_power_on_in_progress() {
                    "power-on"
                } else {
                    "none"
                }
            );

            println!();
            println!("Target Link 0:");
            Self::pretty_print_link(
                target.link0_receiver_status(),
                "Controller",
                target.controller0_present(),
                target_link0_events,
                indent,
            );

            println!();
            println!("Target Link 1:");
            Self::pretty_print_link(
                target.link1_receiver_status(),
                "Controller",
                target.controller1_present(),
                target_link1_events,
                indent,
            );
        }

        Ok(())
    }

    pub fn clear_link_events(
        &mut self,
        port: u8,
        txr: Option<TransceiverSelect>,
    ) -> Result<()> {
        let txrs = txr
            .map(|l| vec![l])
            .unwrap_or_else(|| TransceiverSelect::ALL.to_vec());

        for txr in txrs {
            let op = self.get_idol_command("clear_link_events")?;
            if let Err(e) = humility_cmd_hiffy::hiffy_call(
                self.hubris,
                self.core,
                &mut self.context,
                &op,
                &[
                    ("port", IdolArgument::Scalar(u64::from(port))),
                    ("txr", IdolArgument::String(&txr.to_string())),
                ],
                None,
            )? {
                bail!("Got Hubris error: {:?}", e);
            }
        }
        Ok(())
    }

    pub fn reset(&mut self) -> Result<()> {
        self.core.reset()
    }
}

fn ignition(context: &mut humility::ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let subargs = IgnitionArgs::try_parse_from(subargs)?;
    let hubris = context.archive.as_ref().unwrap();
    let mut worker = IgnitionHandler::new(hubris, core, subargs.timeout)?;

    match subargs.cmd {
        IgnitionCommand::Status => {
            worker.status()?;
        }
        IgnitionCommand::Show { port, counters, link_events } => {
            worker.show_port(port, counters, link_events)?
        }
        IgnitionCommand::PowerOff { port } => {
            worker.port_request(port, Request::SystemPowerOff)?
        }
        IgnitionCommand::PowerOn { port } => {
            worker.port_request(port, Request::SystemPowerOn)?
        }
        IgnitionCommand::PowerReset { port } => {
            worker.port_request(port, Request::SystemPowerReset)?
        }
        IgnitionCommand::ClearLinkEvents { port } => {
            worker.clear_link_events(port, None)?
        }
    }

    Ok(())
}

pub fn init() -> (Command, ClapCommand<'static>) {
    (
        Command::Attached {
            name: "ignition",
            archive: Archive::Required,
            attach: Attach::Any,
            validate: Validate::Booted,
            run: ignition,
        },
        IgnitionArgs::command(),
    )
}
