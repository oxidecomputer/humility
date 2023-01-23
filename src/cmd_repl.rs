// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility repl`
//!
//! read, eval, print, loop

use std::{borrow::Cow, collections::HashMap};

use anyhow::Result;
use clap::Command as ClapCommand;
use clap::CommandFactory;
use humility::cli::Cli;
use humility_cmd::CommandKind;
use humility_cmd::{Archive, Attach, Command, Validate};

use reedline::DefaultPrompt;
use reedline::Span;
use reedline::Suggestion;
use reedline::{
    default_emacs_keybindings, ColumnarMenu, Completer, EditCommand, Emacs,
    FileBackedHistory, KeyCode, KeyModifiers, Keybindings, Reedline,
    ReedlineEvent, ReedlineMenu, Signal,
};

use crate::{cmd, version};

struct Prompt {
    prefix: String,
    default_prompt: DefaultPrompt,
}

impl Prompt {
    fn new() -> Prompt {
        Prompt {
            prefix: String::from("humility"),
            default_prompt: DefaultPrompt,
        }
    }
}

impl reedline::Prompt for Prompt {
    fn render_prompt_left(&self) -> Cow<str> {
        Cow::from(&self.prefix)
    }

    fn render_prompt_right(&self) -> Cow<str> {
        self.default_prompt.render_prompt_right()
    }

    fn render_prompt_indicator(
        &self,
        prompt_mode: reedline::PromptEditMode,
    ) -> Cow<str> {
        self.default_prompt.render_prompt_indicator(prompt_mode)
    }

    fn render_prompt_multiline_indicator(&self) -> Cow<str> {
        self.default_prompt.render_prompt_multiline_indicator()
    }

    fn render_prompt_history_search_indicator(
        &self,
        history_search: reedline::PromptHistorySearch,
    ) -> Cow<str> {
        self.default_prompt
            .render_prompt_history_search_indicator(history_search)
    }
}

fn repl(context: &mut humility::ExecutionContext) -> Result<()> {
    context.is_interactive = true;

    // hardcoding to 100 entries for now. Even though this is called
    // FileBackedHistory, this constructor only uses one in-memory.
    let history = Box::new(FileBackedHistory::new(100));

    let (commands, _) = cmd::init(Cli::command());

    let completer = Box::new(ClapCompleter::new(commands));

    // Use the interactive menu to select options from the completer
    let completion_menu =
        Box::new(ColumnarMenu::default().with_name("completion_menu"));

    let mut keybindings = default_emacs_keybindings();
    add_menu_keybindings(&mut keybindings);
    add_newline_keybinding(&mut keybindings);
    let edit_mode = Box::new(Emacs::new(keybindings));

    let mut line_editor = Reedline::create()
        .with_completer(completer)
        .with_menu(ReedlineMenu::EngineCompleter(completion_menu))
        .with_edit_mode(edit_mode)
        .with_history(history);

    let prompt = Prompt::new();

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

fn add_menu_keybindings(keybindings: &mut Keybindings) {
    keybindings.add_binding(
        KeyModifiers::CONTROL,
        KeyCode::Char('x'),
        ReedlineEvent::UntilFound(vec![
            ReedlineEvent::Menu("history_menu".to_string()),
            ReedlineEvent::MenuPageNext,
        ]),
    );

    keybindings.add_binding(
        KeyModifiers::CONTROL | KeyModifiers::SHIFT,
        KeyCode::Char('x'),
        ReedlineEvent::MenuPagePrevious,
    );

    keybindings.add_binding(
        KeyModifiers::NONE,
        KeyCode::Tab,
        ReedlineEvent::UntilFound(vec![
            ReedlineEvent::Menu("completion_menu".to_string()),
            ReedlineEvent::MenuNext,
        ]),
    );

    keybindings.add_binding(
        KeyModifiers::SHIFT,
        KeyCode::BackTab,
        ReedlineEvent::MenuPrevious,
    );
}

fn add_newline_keybinding(keybindings: &mut Keybindings) {
    // This doesn't work for macOS
    keybindings.add_binding(
        KeyModifiers::ALT,
        KeyCode::Enter,
        ReedlineEvent::Edit(vec![EditCommand::InsertNewline]),
    );
}

pub fn init() -> Command {
    Command {
        app: ClapCommand::new("repl").about("read, eval, print, loop"),
        name: "repl",
        run: repl,
        kind: CommandKind::Attached {
            archive: Archive::Required,
            attach: Attach::Any,
            validate: Validate::Match,
        },
    }
}

struct ClapCompleter {
    commands: HashMap<&'static str, Command>,
}

impl ClapCompleter {
    fn new(commands: HashMap<&'static str, Command>) -> ClapCompleter {
        ClapCompleter { commands }
    }
}

impl Completer for ClapCompleter {
    fn complete(&mut self, line: &str, pos: usize) -> Vec<Suggestion> {
        let mut results = Vec::new();

        let result = {
            if line.contains(' ') {
                let mut words = line.split(' ');
                let command = words.next().unwrap();
                if let Some(command) = self.commands.get(command) {
                    let last_word = words.rev().next().unwrap();
                    let span = Span::new(line.len() - last_word.len(), pos);

                    let mut completions = Vec::new();

                    for arg in command.app.get_arguments() {
                        if let Some(short) = arg.get_short() {
                            let value = format!("-{}", short);
                            if value.starts_with(last_word) {
                                completions.push(Suggestion {
                                    value,
                                    description: arg
                                        .get_help()
                                        .map(|n| n.to_string()),
                                    extra: None,
                                    span,
                                    append_whitespace: true,
                                })
                            }
                        }

                        if let Some(long) = arg.get_long() {
                            let value = format!("--{}", long);
                            // if we only have a '-', that's a short option, not a long one
                            if (last_word != "-")
                                && value.starts_with(last_word)
                            {
                                completions.push(Suggestion {
                                    value,
                                    description: arg
                                        .get_help()
                                        .map(|n| n.to_string()),
                                    extra: None,
                                    span,
                                    append_whitespace: true,
                                })
                            }
                        }
                    }

                    completions
                } else {
                    Vec::new()
                }
            } else {
                self.commands
                    .iter()
                    .filter(|(key, _)| key.starts_with(line))
                    .map(|(key, command)| Suggestion {
                        value: key.to_string(),
                        description: Some(command.short_description()),
                        extra: None,
                        span: Span::new(0, pos),
                        append_whitespace: true,
                    })
                    .collect::<Vec<Suggestion>>()
            }
        };

        results.extend(result);

        results
    }
}
