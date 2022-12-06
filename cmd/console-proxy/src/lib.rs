// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility console-proxy`
//!
//! Act as a proxy for the host serial console when it is jumpered to the SP.

mod picocom_map;

use std::fs::File;
use std::io::{Read, Write};
use std::os::unix::io::AsRawFd;
use std::path::PathBuf;
use std::time::Duration;
use std::{io, thread};

use anyhow::{anyhow, bail, Context, Result};
use clap::{Command as ClapCommand, CommandFactory, Parser};
use crossbeam_channel::{select, Receiver, Sender};
use picocom_map::RemapRules;
use termios::Termios;

use cmd_hiffy as humility_cmd_hiffy;

use cmd_hiffy::HiffyLease;
use humility::cli::Subcommand;
use humility::core::Core;
use humility::hubris::HubrisArchive;
use humility_cmd::hiffy::HiffyContext;
use humility_cmd::idol::{IdolArgument, IdolOperation};
use humility_cmd::{Archive, Attach, Command, Validate};

const HIFFY_BUF_SIZE: usize = 256;

#[derive(Parser, Debug)]
#[clap(name = "console-proxy", about = env!("CARGO_PKG_DESCRIPTION"))]
struct UartConsoleArgs {
    #[clap(
        long,
        short = 'T',
        default_value_t = 5000,
        value_name = "hiffy_timeout_ms"
    )]
    hiffy_timeout: u32,

    #[clap(
        long,
        short,
        default_value_t = 1000,
        help = "frequency of polling the SP for new data",
        value_name = "poll_interval_ms"
    )]
    poll_interval: u32,

    #[clap(subcommand)]
    cmd: UartConsoleCommand,
}

#[derive(Parser, Debug)]
enum UartConsoleCommand {
    /// Attach Humility as the host console uart client via the
    /// `control_plane_agent` task.
    ///
    /// By default, puts the current terminal into raw mode (use `--no-raw` to
    /// disable this). In raw mode, exit via Ctrl-A Ctrl-X. Ctrl-A Ctrl-A will
    /// send a Ctrl-A; no other prefixed commands are recognized.
    ///
    /// When Humility attaches as the uart client, it disables the SP's default
    /// behavior of constantly reading and discarding data from the console
    /// uart. If the attach process ends cleanly, it will detach and restore the
    /// client setting to MGS (and restore the default behavior). To check
    /// whether this happened, use the `client` subcommand. To forcibly set the
    /// client back to MGS (e.g., if `attach` did not exit cleanly), use the
    /// `detach` subcommand.
    Attach {
        #[clap(
            long = "--no-raw",
            help = "do not put terminal in raw mode",
            action = clap::ArgAction::SetFalse,
        )]
        raw: bool,

        #[clap(
            long,
            help = "Specifies the input character map (i.e., special characters to be replaced when reading from the serial port). See picocom's manpage."
        )]
        imap: Option<String>,

        #[clap(
            long,
            help = "Specifies the output character map (i.e., special characters to be replaced when writing to the serial port). See picocom's manpage."
        )]
        omap: Option<String>,

        #[clap(
            long,
            help = "Record all input read from the serial port to this logfile (before any remapping)."
        )]
        log: Option<PathBuf>,
    },

    /// Detach Humility as the host console uart client via the
    /// `control_plane_agent` task, setting the client back to MGS.
    ///
    /// This has the side effect of putting the SP back into its default mode in
    /// which it constantly reads from the console uart, discarding data as
    /// needed (or forwarding to MGS, if one is attached). While Humility is
    /// attached as the client, that behavior is disabled and hardware flow
    /// control is used to apply backpressure to the host.
    Detach,

    /// Print the current console client (Humility or Mgs).
    Client,
}

pub struct UartConsoleHandler<'a> {
    hubris: &'a HubrisArchive,
    core: &'a mut dyn Core,
    context: HiffyContext<'a>,
    poll_interval: Duration,
}

impl<'a> UartConsoleHandler<'a> {
    pub fn new(
        hubris: &'a HubrisArchive,
        core: &'a mut dyn Core,
        hiffy_timeout: u32,
        poll_interval: u32,
    ) -> Result<Self> {
        let context = HiffyContext::new(hubris, core, hiffy_timeout)?;
        Ok(Self {
            hubris,
            core,
            context,
            poll_interval: Duration::from_millis(u64::from(poll_interval)),
        })
    }

    fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
        let op = IdolOperation::new(
            self.hubris,
            "ControlPlaneAgent",
            "uart_read",
            None,
        )
        .context("Could not find `ControlPlaneAgent.uart_read` operation")?;

        let value = humility_cmd_hiffy::hiffy_call(
            self.hubris,
            self.core,
            &mut self.context,
            &op,
            &[],
            Some(HiffyLease::Read(buf)),
        )?;

        let v = match value {
            Ok(v) => v,
            Err(e) => bail!("Got Hiffy error: {e}"),
        };

        let v =
            v.as_base()?.as_u32().ok_or_else(|| anyhow!("Couldn't get U32"))?;

        Ok(v as usize)
    }

    fn write(&mut self, buf: &[u8]) -> Result<usize> {
        let op = IdolOperation::new(
            self.hubris,
            "ControlPlaneAgent",
            "uart_write",
            None,
        )
        .context("Could not find `ControlPlaneAgent.uart_write` operation")?;

        let buf = &buf[..usize::min(buf.len(), HIFFY_BUF_SIZE)];

        let value = humility_cmd_hiffy::hiffy_call(
            self.hubris,
            self.core,
            &mut self.context,
            &op,
            &[],
            Some(HiffyLease::Write(buf)),
        )?;

        let v = match value {
            Ok(v) => v,
            Err(e) => bail!("Got Hiffy error: {e}"),
        };

        let v =
            v.as_base()?.as_u32().ok_or_else(|| anyhow!("Couldn't get U32"))?;

        Ok(v as usize)
    }

    fn attach(
        &mut self,
        raw: bool,
        imap: RemapRules,
        omap: RemapRules,
        log: Option<File>,
    ) -> Result<()> {
        // Put terminal in raw mode, if requested, with a guard to restore it.
        let _guard = if raw {
            Some(UnrawTermiosGuard::make_stdout_raw()?)
        } else {
            None
        };

        let (stdin_tx, stdin_rx) = crossbeam_channel::unbounded();
        let (stdout_tx, stdout_rx) = crossbeam_channel::unbounded();
        thread::spawn(move || stdin_reader(raw, stdin_tx, omap));
        thread::spawn(move || stdout_writer(stdout_rx, imap, log));

        let mut rx_buf = vec![0; HIFFY_BUF_SIZE];
        let mut tx_buf = Vec::new();
        loop {
            if !tx_buf.is_empty() {
                let nwritten = self.write(&tx_buf)?;
                tx_buf.drain(0..nwritten);
            }

            let nread = self.read(&mut rx_buf)?;
            if nread > 0 {
                stdout_tx.send(rx_buf[..nread].to_owned()).unwrap();
            }

            // If our read returned the max buffer size, don't wait for
            // `poll_interval` to try again, just wait 1ms to give the other
            // channels a chance to sneak something in.
            let timeout = if nread == HIFFY_BUF_SIZE {
                Duration::from_millis(1)
            } else {
                self.poll_interval
            };
            let timeout = crossbeam_channel::after(timeout);

            let mut done = false;
            loop {
                select! {
                    recv(stdin_rx) -> data => {
                        if let Ok(mut data) = data {
                            tx_buf.append(&mut data);
                        } else {
                            done = true;
                            break;
                        }
                    },
                    recv(timeout) -> _ => break,
                }
            }

            if done {
                break;
            }
        }

        self.detach()
    }

    fn detach(&mut self) -> Result<()> {
        let op = IdolOperation::new(
            self.hubris,
            "ControlPlaneAgent",
            "set_humility_uart_client",
            None,
        )
        .context(
            "Could not find `ControlPlaneAgent.set_humility_uart_client` operation",
        )?;

        let value = humility_cmd_hiffy::hiffy_call(
            self.hubris,
            self.core,
            &mut self.context,
            &op,
            &[("attach", IdolArgument::String("false"))],
            None,
        )?;

        match value {
            Ok(_) => Ok(()),
            Err(e) => bail!("Got Hiffy error: {e}"),
        }
    }

    fn current_client(&mut self) -> Result<()> {
        let op = IdolOperation::new(
            self.hubris,
            "ControlPlaneAgent",
            "get_uart_client",
            None,
        )
        .context(
            "Could not find `ControlPlaneAgent.get_uart_client` operation",
        )?;

        let value = humility_cmd_hiffy::hiffy_call(
            self.hubris,
            self.core,
            &mut self.context,
            &op,
            &[],
            None,
        )?;

        let value = match value {
            Ok(v) => v,
            Err(e) => bail!("Got Hiffy error: {e}"),
        };

        let value = value
            .as_enum()
            .context("get_uart_client did not return an enum")?;

        println!("Current console client: {}", value.disc());
        Ok(())
    }
}

fn stdout_writer(
    rx: Receiver<Vec<u8>>,
    remap: RemapRules,
    mut log: Option<File>,
) {
    let mut stdout = io::stdout().lock();
    for data in rx {
        if let Some(log) = log.as_mut() {
            if let Err(err) = log.write_all(&data).and_then(|()| log.flush()) {
                panic!("error writing to logfile: {err}");
            }
        }
        let data = remap.apply(data).collect::<Vec<_>>();
        if let Err(err) = stdout.write_all(&data).and_then(|()| stdout.flush())
        {
            panic!("error writing to stdout: {err}");
        }
    }
}

fn stdin_reader(raw: bool, tx: Sender<Vec<u8>>, remap: RemapRules) {
    const CTRL_A: u8 = b'\x01';
    const CTRL_X: u8 = b'\x18';

    let mut stdin = io::stdin().lock();
    let mut buf = vec![0; 1024];
    let mut next_raw = false;

    loop {
        match stdin.read(&mut buf[..]) {
            Ok(0) => {
                return;
            }
            Ok(n) => {
                let buf = &buf[..n];

                let remapped = if raw {
                    let mut done = false;
                    let remapped = remap
                        .apply(buf.iter().filter_map(|&b| match b {
                            // Ctrl-A means send next one raw
                            CTRL_A => {
                                if next_raw {
                                    // Ctrl-A Ctrl-A should be sent as Ctrl-A
                                    next_raw = false;
                                    Some(b)
                                } else {
                                    next_raw = true;
                                    None
                                }
                            }
                            CTRL_X => {
                                if next_raw {
                                    // Ctrl-A Ctrl-X is our signal to exit;
                                    // we'll finish filtering this iterator but
                                    // then immediately return.
                                    done = true;
                                    None
                                } else {
                                    Some(b)
                                }
                            }
                            _ => {
                                next_raw = false;
                                Some(b)
                            }
                        }))
                        .collect();

                    if done {
                        return;
                    }

                    remapped
                } else {
                    remap.apply(buf.iter().copied()).collect()
                };

                _ = tx.send(remapped);
            }
            Err(err) => panic!("error reading from stdin: {err}"),
        }
    }
}

struct UnrawTermiosGuard {
    stdout: i32,
    ios: Termios,
}

impl Drop for UnrawTermiosGuard {
    fn drop(&mut self) {
        termios::tcsetattr(self.stdout, termios::TCSAFLUSH, &self.ios).unwrap();
    }
}

impl UnrawTermiosGuard {
    fn make_stdout_raw() -> Result<Self> {
        let stdout = io::stdout().as_raw_fd();
        let orig_termios = termios::Termios::from_fd(stdout)?;
        let mut termios = orig_termios;
        termios::cfmakeraw(&mut termios);
        termios::tcsetattr(stdout, termios::TCSANOW, &termios)?;
        termios::tcflush(stdout, termios::TCIOFLUSH)?;
        Ok(Self { stdout, ios: orig_termios })
    }
}

fn console_proxy(context: &mut humility::ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let subargs = UartConsoleArgs::try_parse_from(subargs)?;
    let hubris = context.archive.as_ref().unwrap();
    let mut worker = UartConsoleHandler::new(
        hubris,
        core,
        subargs.hiffy_timeout,
        subargs.poll_interval,
    )?;

    match subargs.cmd {
        UartConsoleCommand::Attach { raw, imap, omap, log } => {
            let imap = match imap {
                Some(s) => s.parse().context("invalid imap rules")?,
                None => RemapRules::default(),
            };
            let omap = match omap {
                Some(s) => s.parse().context("invalid omap rules")?,
                None => RemapRules::default(),
            };
            let log = match log {
                Some(path) => {
                    let f = File::options()
                        .append(true)
                        .create(true)
                        .open(&path)
                        .with_context(|| {
                            format!("failed to open {}", path.display())
                        })?;
                    Some(f)
                }
                None => None,
            };
            worker.attach(raw, imap, omap, log)?;
        }
        UartConsoleCommand::Detach => {
            worker.detach()?;
        }
        UartConsoleCommand::Client => {
            worker.current_client()?;
        }
    }

    Ok(())
}

pub fn init() -> (Command, ClapCommand<'static>) {
    (
        Command::Attached {
            name: "console-proxy",
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
            run: console_proxy,
        },
        UartConsoleArgs::command(),
    )
}
