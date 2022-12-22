// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

mod picocom_map;

use std::fs::File;
use std::io::{Read, Write};
use std::os::unix::io::AsRawFd;
use std::time::Duration;
use std::{io, thread};

use anyhow::{anyhow, bail, Context, Result};
use clap::Parser;
use crossbeam_channel::{select, Sender};
use picocom_map::RemapRules;
use termios::Termios;

use cmd_hiffy as humility_cmd_hiffy;

use cmd_hiffy::HiffyLease;
use humility::cli::Subcommand;
use humility::core::Core;
use humility::hubris::HubrisArchive;
use humility_cmd::hiffy::HiffyContext;
use humility_cmd::idol::{HubrisIdol, IdolArgument};

use super::UartConsoleArgs;
use super::UartConsoleCommand;

const HIFFY_BUF_SIZE: usize = 256;

struct UartConsoleHandler<'a> {
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

    fn uart_read(&mut self, buf: &mut [u8]) -> Result<usize> {
        let op = self.hubris.get_idol_command("ControlPlaneAgent.uart_read")?;

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

    fn uart_write(&mut self, buf: &[u8]) -> Result<usize> {
        let op =
            self.hubris.get_idol_command("ControlPlaneAgent.uart_write")?;

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
        mut log: Option<File>,
    ) -> Result<()> {
        // Put terminal in raw mode, if requested, with a guard to restore it.
        let _guard = if raw {
            Some(UnrawTermiosGuard::make_stdout_raw()?)
        } else {
            None
        };

        let (stdin_tx, stdin_rx) = crossbeam_channel::unbounded();
        thread::spawn(move || stdin_reader(raw, stdin_tx, omap));

        let mut rx_buf = vec![0; HIFFY_BUF_SIZE];
        let mut tx_buf = Vec::new();
        let mut stdout = io::stdout().lock();
        'outer: loop {
            if !tx_buf.is_empty() {
                let nwritten = self.uart_write(&tx_buf)?;
                tx_buf.drain(0..nwritten);
            }

            let nread = self.uart_read(&mut rx_buf)?;
            if nread > 0 {
                let data = &rx_buf[..nread];

                // Record data read to our logfile, if we have one, before
                // performing remapping.
                if let Some(log) = log.as_mut() {
                    log.write_all(data)
                        .and_then(|()| log.flush())
                        .context("error writing to logfile")?;
                }

                // Apply `imap` and write to stdout.
                let data = imap.apply(data.iter().copied()).collect::<Vec<_>>();
                stdout
                    .write_all(&data)
                    .and_then(|()| stdout.flush())
                    .context("error writing to stdout")?;
            }

            // If our read returned the max buffer size or we still have data to
            // send, don't wait for `poll_interval` to try again, just wait 1ms
            // to give the stdin channel a chance to sneak something in.
            let timeout = if nread == HIFFY_BUF_SIZE && !tx_buf.is_empty() {
                Duration::from_millis(1)
            } else {
                self.poll_interval
            };
            let timeout = crossbeam_channel::after(timeout);

            loop {
                select! {
                    recv(stdin_rx) -> data => {
                        if let Ok(mut data) = data {
                            tx_buf.append(&mut data);
                        } else {
                            // `stdin_tx` is closed; we're done.
                            break 'outer;
                        }
                    },
                    recv(timeout) -> _ => break,
                }
            }
        }

        self.detach()
    }

    fn detach(&mut self) -> Result<()> {
        let op = self
            .hubris
            .get_idol_command("ControlPlaneAgent.set_humility_uart_client")?;

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
        let op = self
            .hubris
            .get_idol_command("ControlPlaneAgent.get_uart_client")?;

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

pub(super) fn console_proxy(
    context: &mut humility::ExecutionContext,
) -> Result<()> {
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
            if context.is_interactive {
                bail!("`console-proxy attach` cannot be used from the REPL");
            }

            let imap = imap.parse().context("invalid imap rules")?;
            let omap = omap.parse().context("invalid omap rules")?;

            let log = log
                .map(|path| {
                    File::options()
                        .append(true)
                        .create(true)
                        .open(&path)
                        .with_context(|| {
                            format!("failed to open {}", path.display())
                        })
                })
                .transpose()?;
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
