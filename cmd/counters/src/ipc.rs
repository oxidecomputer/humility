use super::{load_counters, taskname, CountersArgs, Order};
use anyhow::{bail, Result};
use colored::Colorize;
use humility::core::Core;
use humility::hubris::*;
use humility_doppel::{CounterVariant, GenOrRestartCount};
use indexmap::IndexMap;
use std::collections::BTreeMap;
use std::fmt;

pub(super) fn ipc_counter_dump(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    subargs: &CountersArgs,
) -> Result<()> {
    // In order to display task generations accurately, we must find and load
    // the task table:
    let task_table = {
        let (base, task_count) = hubris.task_table(core)?;
        let task_t = hubris.lookup_struct_byname("Task")?.clone();
        humility_doppel::Task::load_tcbs(
            hubris,
            core,
            base,
            task_count as usize,
            &task_t,
        )?
    };

    let mut ipcs = BTreeMap::new();

    for (varname, var) in hubris.qualified_variables() {
        if varname.ends_with("_CLIENT_COUNTERS") {
            let task = HubrisTask::from(var.goff);
            let taskname = taskname(hubris, var)?;
            // If we're only showing IPCs from specific clients, check whether
            // the task name contains that substring.
            if !subargs.client.is_empty()
                && !subargs.client.iter().any(|name| taskname.contains(name))
            {
                continue;
            }

            let gen = task_table[task.task() as usize].generation;
            // Only select counters matching the provided filter, if there is
            // one.
            if let Some(ref name) = subargs.name {
                if !varname.contains(name) {
                    continue;
                }
            }

            let def = hubris.lookup_struct(var.goff)?;
            let ctrs = load_counters(hubris, core, def, var)?;
            let iface = ipcs.entry(varname).or_insert_with(|| {
                let name = def
                    .name
                    .split("::")
                    .last()
                    .map(|s| {
                        s.trim_end_matches("Counts").trim_end_matches("Event")
                    })
                    .unwrap_or("");
                IpcIface { name, methods: Default::default() }
            });
            for (method, count) in ctrs.counts {
                iface
                    .methods
                    .entry(method)
                    .or_default()
                    .0
                    .insert((taskname, gen), count);
            }
        }
    }

    if ipcs.is_empty() {
        if let Some(ref name) = subargs.name {
            bail!(
                "no IPC counters found with names containing \"{}\" (-l to list)",
                name
            );
        } else {
            bail!("no IPC counters found");
        }
    }

    for (varname, mut iface) in ipcs {
        if let Some(order) = subargs.sort {
            iface.sort(order);
        } else if !subargs.full {
            iface.sort(Order::Value)
        }
        if subargs.full {
            println!("{varname}\n{iface:#}");
        } else if iface.total() > 0 {
            println!("{varname}\n{iface}");
        }
    }
    Ok(())
}

struct IpcIface<'a> {
    name: &'a str,
    methods: IndexMap<String, Ipc<'a>>,
}

#[derive(Default)]
struct Ipc<'taskname>(
    IndexMap<(&'taskname str, GenOrRestartCount), CounterVariant>,
);

impl IpcIface<'_> {
    fn total(&self) -> usize {
        self.methods.values().map(Ipc::total).sum()
    }

    fn sort(&mut self, order: Order) {
        for (_, method) in &mut self.methods {
            method.sort(order);
        }
        match order {
            Order::Decl => {}
            Order::Value => {
                self.methods.sort_unstable_by(|_, a, _, b| {
                    a.total().cmp(&b.total()).reverse()
                });
            }
            Order::Alpha => {
                self.methods.sort_unstable_by(|a, _, b, _| a.cmp(b));
            }
        }
    }
}

impl fmt::Display for IpcIface<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let total_len = f.width().unwrap_or(10);
        let dim_space = " ".dimmed();
        const INDENT: &str = "    ";
        for (method_name, ctrs) in &self.methods {
            let total = ctrs.total();
            if total == 0 && !f.alternate() {
                continue;
            }
            let total_str = total.to_string();
            let errors = ctrs.total_errors();
            let ok = total - errors;
            let ok_str = ok.to_string();
            let err_str = errors.to_string();
            writeln!(
                f,
                " fn {}::{}() {dim_space:.>pad$}{} calls\n{INDENT}{}:",
                self.name.bold(),
                method_name.bold(),
                total_str.bold(),
                "clients".bold(),
                pad = 80usize
                    .saturating_sub(" fn ".len())
                    .saturating_sub("() ".len())
                    .saturating_sub("::".len())
                    .saturating_sub(" calls".len())
                    .saturating_sub(method_name.len())
                    .saturating_sub(self.name.len())
                    .saturating_sub(total_str.len())
                    .saturating_sub(1)
            )?;
            let mut formatted_tasks = 0;
            let mut formatted_errors = 0;
            let num_important_tasks = ctrs
                .0
                .iter()
                .filter(|(_, c)| c.total() > 0 || f.alternate())
                .count();
            for ((task, gen), ctrs) in &ctrs.0 {
                let total = ctrs.total();
                if total == 0 && !f.alternate() {
                    continue;
                } else {
                    formatted_tasks += 1;
                }

                let errors = ipc_error_count(ctrs);
                let ok = total - errors;
                let ok_str = format!(
                    "{} {ok}",
                    if num_important_tasks > 1 { "+" } else { "=" }
                );
                let restarts = match gen {
                    GenOrRestartCount::Gen(gen) => {
                        format!(" (gen {gen:?})")
                    }
                    GenOrRestartCount::RestartCount(restarts) => {
                        format!(" ({restarts} restarts)")
                    }
                };
                write!(f, "{INDENT}task {}{restarts} ", task.italic())?;
                if errors == 0 && !f.alternate() {
                    let err_str =
                        if num_important_tasks > 1 { "+ 0" } else { "= 0" };
                    let pad1 = 80usize
                        .saturating_sub(err_str.len())
                        .saturating_sub(task.len())
                        .saturating_sub(restarts.len())
                        .saturating_sub(INDENT.len())
                        .saturating_sub("task  ".len())
                        .saturating_sub(total_len * 2);
                    writeln!(
                        f,
                        "{dim_space:.>pad1$}{} {dim_space:.>pad2$}{} ok",
                        err_str.dimmed(),
                        if ok > 0 { ok_str.green() } else { ok_str.dimmed() },
                        pad2 = 80usize
                            .saturating_sub(pad1)
                            .saturating_sub(ok_str.len())
                            .saturating_sub(" ok".len())
                            .saturating_sub(err_str.len())
                            .saturating_sub(task.len())
                            .saturating_sub(restarts.len())
                            .saturating_sub(INDENT.len())
                            .saturating_sub("task  ".len())
                            .saturating_sub(2)
                    )?;
                } else {
                    writeln!(
                        f,
                        "{dim_space:.>pad$}{} ok",
                        if ok > 0 { ok_str.green() } else { ok_str.dimmed() },
                        pad = 80usize
                            .saturating_sub(ok_str.len())
                            .saturating_sub(task.len())
                            .saturating_sub(restarts.len())
                            .saturating_sub("    task ".len())
                            .saturating_sub(2)
                            .saturating_sub(" ok".len())
                    )?;
                }

                fn fmt_err_variant(
                    ctr: &CounterVariant,
                    mut pfx: String,
                    formatted: &mut usize,
                    num_important: usize,
                    only_important_task: bool,
                    f: &mut fmt::Formatter<'_>,
                ) -> fmt::Result {
                    match ctr {
                        &CounterVariant::Single(value)
                            if value > 0 || f.alternate() =>
                        {
                            *formatted += 1;
                            let total_len = f.width().unwrap_or(10);
                            let value_str = format!("+ {value}",);
                            for _ in 0..pfx.matches('(').count() {
                                pfx.push(')');
                            }
                            let pad = 80usize
                                .saturating_sub(total_len * 2)
                                .saturating_sub(
                                    if num_important > 1 && !only_important_task
                                    {
                                        total_len + 3
                                    } else {
                                        0
                                    },
                                )
                                .saturating_sub(pfx.len())
                                .saturating_sub(value_str.len())
                                .saturating_sub("- Err() ".len())
                                .saturating_sub(INDENT.len());
                            writeln!(
                                f,
                                "{INDENT}- Err({}) {:.>pad$}{}{:.<rem$}",
                                pfx.red(),
                                " ".dimmed(),
                                if value > 0 {
                                    value_str.red()
                                } else {
                                    value_str.dimmed()
                                },
                                " ".dimmed(),
                                rem = 80usize
                                    .saturating_sub(pad)
                                    .saturating_sub(value_str.len())
                                    .saturating_sub(pfx.len())
                                    .saturating_sub("- Err() ".len())
                                    .saturating_sub(INDENT.len())
                                    .saturating_sub(1),
                            )?;
                        }
                        CounterVariant::Nested(map) => {
                            let num_important = map
                                .counts
                                .iter()
                                .filter(|(_, c)| c.total() > 0 || f.alternate())
                                .count();
                            for (name, ctr) in &map.counts {
                                fmt_err_variant(
                                    ctr,
                                    if !pfx.is_empty() {
                                        format!("{pfx}({name}")
                                    } else {
                                        name.clone()
                                    },
                                    formatted,
                                    num_important,
                                    only_important_task,
                                    f,
                                )?;
                            }
                        }
                        _ => {}
                    };

                    Ok(())
                }

                let mut formatted_errs_this_task = 0;
                let errs = if let CounterVariant::Nested(map) = ctrs {
                    map.counts.get("Err")
                } else {
                    None
                };
                if let Some(errs) = errs {
                    let err_total = errs.total();
                    if err_total > 0 || f.alternate() {
                        let total_str = format!("{err_total}");
                        if let CounterVariant::Nested(_) = errs {
                            let only_important_task = num_important_tasks == 1;
                            fmt_err_variant(
                                errs,
                                String::new(),
                                &mut formatted_errs_this_task,
                                0,
                                only_important_task,
                                f,
                            )?;
                            if formatted_errs_this_task > 1
                                && !only_important_task
                            {
                                const TOTAL_INDENT: &str = "      ";
                                let colored_total = if err_total > 0 {
                                    total_str.red()
                                } else {
                                    total_str.dimmed()
                                };
                                let pad = 80usize
                                    .saturating_sub(total_len * 3)
                                    .saturating_sub(total_str.len())
                                    .saturating_sub(TOTAL_INDENT.len())
                                    .saturating_sub(5);
                                write!(
                                    f,
                                    "{TOTAL_INDENT}{:>pad$}{:->underline$}\n\
                                    {TOTAL_INDENT}{:>pad$}= {colored_total}",
                                    "",
                                    "",
                                    "",
                                    underline = total_str.len() + 2,
                                )?;
                                if num_important_tasks > 1 {
                                    writeln!(
                                        f,
                                        " {:->arrow$} {} {colored_total}",
                                        ">".dimmed(),
                                        if err_total > 0 {
                                            "+".red()
                                        } else {
                                            "+".dimmed()
                                        },
                                        arrow = (total_len + 3)
                                            .saturating_sub(4) // spaces and +
                                            .saturating_sub(total_str.len())
                                    )?;
                                } else {
                                    writeln!(f, " err")?;
                                }
                            }
                        }
                    }
                }
                formatted_errors += formatted_errs_this_task;
            }

            if formatted_tasks > 1 || formatted_errors > 1 {
                let err_underline = "-".repeat(err_str.len() + 2);
                let ok_underline = "-".repeat(ok_str.len() + 2);
                writeln!(
                    f,
                    "{INDENT}{:>pad1$}{err_underline}{INDENT}{:>pad2$}{ok_underline}",
                    "",
                    " ",
                    pad1 = 80usize
                        .saturating_sub(total_len * 2)
                        .saturating_sub(err_underline.len())
                        .saturating_sub(INDENT.len()),
                    pad2 = (total_len + 2) - ok_underline.len(),
                )?;
                writeln!(
                    f,
                    "{INDENT}{}:{:>pad1$}= {} err {:>pad2$}= {} ok",
                    "totals".bold(),
                    " ",
                    if errors > 0 { err_str.red() } else { err_str.dimmed() },
                    " ",
                    if ok > 0 { ok_str.green() } else { ok_str.dimmed() },
                    pad1 = 80usize
                        .saturating_sub(total_len * 2)
                        .saturating_sub(err_str.len() + 2)
                        .saturating_sub(INDENT.len())
                        .saturating_sub("totals:".len()),
                    pad2 = total_len - (ok_str.len() + 1),
                )?;
            }
            f.write_str("\n")?;
        }

        Ok(())
    }
}

impl Ipc<'_> {
    fn total(&self) -> usize {
        self.0.values().map(CounterVariant::total).sum()
    }

    fn total_errors(&self) -> usize {
        self.0.values().map(ipc_error_count).sum()
    }

    fn sort(&mut self, order: Order) {
        match order {
            Order::Decl => {}
            Order::Value => {
                self.0.sort_unstable_by(|_, a, _, b| {
                    a.total().cmp(&b.total()).reverse()
                });
            }
            Order::Alpha => {
                self.0.sort_unstable_by(|(a, _), _, (b, _), _| a.cmp(b));
            }
        }
    }
}

fn ipc_error_count(ctr: &CounterVariant) -> usize {
    match ctr {
        CounterVariant::Single(_) => 0,
        CounterVariant::Nested(ref ctrs) => {
            ctrs.counts.get("Err").map(|c| c.total()).unwrap_or(0)
        }
    }
}
