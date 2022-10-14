// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility repl`
//!
//! read, eval, print, loop

use std::borrow::Cow;

use anyhow::Result;
use clap::Command as ClapCommand;
use humility_cmd::{Archive, Attach, Command, Validate};

use reedline::{
    FileBackedHistory, PromptHistorySearchStatus, Reedline, Signal,
};

use crate::{cmd, version};

struct Prompt;

impl reedline::Prompt for Prompt {
    fn render_prompt_left(&self) -> Cow<str> {
        Cow::from("humility")
    }

    fn render_prompt_right(&self) -> Cow<str> {
        Cow::default()
    }

    fn render_prompt_indicator(
        &self,
        _prompt_mode: reedline::PromptEditMode,
    ) -> Cow<str> {
        // not getting fancy for now
        Cow::from("> ")
    }

    fn render_prompt_multiline_indicator(&self) -> Cow<str> {
        Cow::from("... ")
    }

    fn render_prompt_history_search_indicator(
        &self,
        history_search: reedline::PromptHistorySearch,
    ) -> Cow<str> {
        let prefix = match history_search.status {
            PromptHistorySearchStatus::Passing => "",
            PromptHistorySearchStatus::Failing => "failing ",
        };

        Cow::Owned(format!(
            "({}reverse-search: {}) ",
            prefix, history_search.term
        ))
    }
}

fn repl(context: &mut humility::ExecutionContext) -> Result<()> {
    // hardcoding to 100 entries for now. Even though this is called
    // FileBackedHistory, this constructor only uses one in-memory.
    let history = Box::new(FileBackedHistory::new(100));

    let mut line_editor = Reedline::create()?.with_history(history)?;

    let prompt = Prompt;

    println!("Welcome to the humility REPL! Try out some subcommands, or 'quit' to quit!");

    loop {
        let sig = line_editor.read_line(&prompt)?;

        match sig {
            Signal::Success(buffer) => {
                let buffer = buffer.trim();
                match buffer {
                    "quit" => {
                        println!("Quitting!");
                        std::process::exit(0);
                    }
                    "history" => {
                        line_editor.print_history()?;
                    }
                    user_input => {
                        let result = eval(context, user_input)?;
                        println!("{result}");
                    }
                }
            }
            Signal::CtrlD | Signal::CtrlC => {
                println!("\nAborted!");
                break Ok(());
            }
            Signal::CtrlL => {
                line_editor.clear_screen().unwrap();
            }
        }
    }
}

fn eval(
    context: &mut humility::ExecutionContext,
    user_input: &str,
) -> Result<String> {
    let mut input = vec!["humility"];
    input.extend(user_input.split(' '));

    let (commands, _, cli) = match crate::parse_args(input) {
        Some(s) => s,
        None => return Ok(String::new()),
    };

    if let Some(s) = version(&cli) {
        return Ok(s);
    }

    // merge the new subcommand into our existing CLI, so that we retain options
    // like the archive/dump, so that the user doesn't have to type them in again
    context.cli.cmd = cli.cmd;

    if let Err(e) = cmd::subcommand(context, &commands) {
        Ok(e.to_string())
    } else {
        Ok(String::new())
    }
}

pub fn init() -> (Command, ClapCommand<'static>) {
    (
        Command::Attached {
            name: "repl",
            archive: Archive::Required,
            attach: Attach::Any,
            validate: Validate::Match,
            run: repl,
        },
        ClapCommand::new("repl").about("read, eval, print, loop"),
    )
}
