//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility hiffy`
//!
//! `humility hiffy` allows for querying and manipulation of `hiffy`, the
//! HIF agent present in Hubris.  To list all Idol interfaces present in
//! Hubris, use the `-l` (`--list`) option, optionally specifying a filter
//! for tasks or interface names if so desired:
//!
//! ```console
//! $ humility hiffy -l user_leds
//! humility: attached via ST-Link
//! INTERFACE                    TASK
//! UserLeds                     user_leds
//!   |
//!   +--> UserLeds.led_on
//!   |       index                       usize
//!   |       <ok>                        ()
//!   |       <error>                     LedError
//!   |
//!   +--> UserLeds.led_off
//!   |       index                       usize
//!   |       <ok>                        ()
//!   |       <error>                     LedError
//!   |
//!   +--> UserLeds.led_toggle
//!           index                       usize
//!           <ok>                        ()
//!           <error>                     LedError
//! ```
//!
//! To enlist the Hubris agent to call a particular interface and operation,
//! use `-c` (`--call`), using `-a` (`--arguments`) to indicate any arguments,
//! e.g.:
//!
//! ```console
//! $ humility hiffy -c UserLeds.led_toggle -a index=0
//! humility: attached via ST-Link
//! UserLeds.led_toggle() = ()
//! ```
//!
//! To view the raw HIF functions provided to programmatic HIF consumers
//! within Humility, use `-L` (`--list-functions`).
//!

use ::idol::syntax::{Operation, Reply};
use anyhow::{Context, Result, bail};
use clap::{CommandFactory, Parser};
use humility::core::Core;
use humility::hubris::*;
use humility::warn;
use humility_cli::{ExecutionContext, Subcommand};
use humility_cmd::{Archive, Command, CommandKind, Dumper};
use humility_hif_assembler::assembler::TargetConfig;
use humility_hif_assembler::bundle::HifBundle;
use humility_hiffy::*;
use humility_idol as idol;
use std::io::Read;
use std::time::Instant;

#[derive(Parser, Debug)]
#[clap(name = "hiffy", about = env!("CARGO_PKG_DESCRIPTION"))]
struct HiffyArgs {
    /// sets timeout
    #[clap(
        long, short = 'T', default_value_t = 5000, value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// list HIF functions
    #[clap(long = "list-functions", short = 'L')]
    listfuncs: bool,

    /// list interfaces
    #[clap(long, short, conflicts_with = "listfuncs")]
    list: bool,

    /// call a particular function
    #[clap(long, short, conflicts_with_all = &["list", "listfuncs"])]
    call: Option<String>,

    /// input for an operation that takes a lease
    #[clap(long, short, requires = "call")]
    input: Option<String>,

    /// number of bytes to return, when a function has a write-only lease
    #[clap(long, short, requires = "call")]
    num: Option<usize>,

    /// output for an operation that writes to a lease
    #[clap(long, short, requires = "call")]
    output: Option<String>,

    /// print returned data in hex
    #[clap(short = 'x', requires = "num")]
    hex: bool,

    /// arguments
    #[clap(long, short, requires = "call")]
    task: Option<String>,

    /// arguments
    #[clap(long, short, use_value_delimiter = true, requires = "call")]
    arguments: Vec<String>,

    /// execute a .hif or .hifb program file on a target
    #[clap(
        long,
        conflicts_with_all = &["list", "listfuncs", "call", "verify", "assemble"],
        value_name = "FILE"
    )]
    exec: Option<String>,

    /// verify a .hif program (no target needed)
    #[clap(
        long,
        conflicts_with_all = &["list", "listfuncs", "call", "exec", "assemble"],
        value_name = "FILE"
    )]
    verify: Option<String>,

    /// assemble a .hif program to a .hifb bundle (no target needed)
    #[clap(
        long,
        conflicts_with_all = &["list", "listfuncs", "call", "exec", "verify"],
        value_name = "FILE",
        requires = "bundle-output",
    )]
    assemble: Option<String>,

    /// output file for --assemble
    #[clap(long, value_name = "FILE")]
    bundle_output: Option<String>,

    /// write assembled bundle to file (use with --exec on a .hif file)
    #[clap(long, requires = "exec", value_name = "FILE")]
    save_bundle: Option<String>,

    /// output results as JSON
    #[clap(long)]
    json: bool,

    /// filter for list output
    #[clap(use_value_delimiter = true)]
    filter: Vec<String>,
}

pub fn hiffy_list(hubris: &HubrisArchive, filter: Vec<String>) -> Result<()> {
    let print_args = |op: &(&String, &Operation), module, margin| {
        let mut args = op.1.args.iter();

        match args.next() {
            None => {}
            Some(arg) => {
                println!("{}{:<27} {}", margin, arg.0, arg.1.ty.0);

                for arg in args {
                    println!("{}{:<27} {}", margin, arg.0, arg.1.ty.0);
                }
            }
        }

        match idol::lookup_reply(hubris, module, op.0) {
            Ok((_, idol::IdolError::CLike(e))) => match &op.1.reply {
                Reply::Result { ok, .. } => {
                    println!("{}{:<27} {}", margin, "<ok>", ok.ty.0);
                    println!("{}{:<27} {}", margin, "<error>", e.name);
                }
                _ => warn!("mismatch on reply: found {op:?}"),
            },
            Ok((_, idol::IdolError::Complex(t))) => match &op.1.reply {
                Reply::Result { ok, .. } => {
                    println!("{}{:<27} {}", margin, "<ok>", ok.ty.0);
                    println!("{}{:<27} {}", margin, "<error>", t.name);
                }
                _ => warn!("mismatch on reply: found {op:?}"),
            },

            Ok((_, idol::IdolError::None)) => match &op.1.reply {
                Reply::Result { ok, .. } => {
                    //
                    // This is possible if the only error is ServerDeath
                    //
                    println!("{}{:<27} {}", margin, "<ok>", ok.ty.0);
                }
                Reply::Simple(ok) => {
                    println!("{}{:<27} {}", margin, "<ok>", ok.ty.0);
                }
            },
            Err(e) => {
                warn!("{}", e);
            }
        }
    };

    let mut matches = false;

    for i in 0..hubris.ntasks() {
        let module = hubris.lookup_module(HubrisTask::Task(i as u32))?;

        if let Some(iface) = &module.iface {
            if !filter.is_empty()
                && !filter.iter().any(|f| iface.name == *f || module.name == *f)
            {
                continue;
            }

            let mut ops = iface.ops.iter().peekable();

            matches = true;
            println!("{:<28} TASK", "INTERFACE");
            println!("{:<28} {}", iface.name, module.name);
            println!("  |");

            while let Some(op) = ops.next() {
                println!("  +--> {}.{}", iface.name, op.0);

                let last = ops.peek().is_none();
                let c = if last { "" } else { "|" };
                let margin = format!("  {:<8}", c);

                print_args(&op, module, margin);
                println!("  {}", c);
            }
        }
    }

    if !filter.is_empty() && !matches {
        bail!(
            "filter \"{}\" did not match any task or interface; \
            use --list without an argument to list all interfaces",
            filter.join(",")
        );
    }

    Ok(())
}

fn hiffy(context: &mut ExecutionContext) -> Result<()> {
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let hubris = context.archive.as_ref().unwrap();

    let subargs = HiffyArgs::try_parse_from(subargs)?;

    //
    // Offline operations: these only need the archive, not a live
    // target.  Handle them first so we can return before attaching.
    //
    if subargs.list {
        hiffy_list(hubris, subargs.filter)?;
        return Ok(());
    }

    if let Some(ref path) = subargs.verify {
        return hiffy_verify(hubris, path);
    }

    if let Some(ref path) = subargs.assemble {
        let output = subargs.bundle_output.as_ref().unwrap(); // required by clap
        return hiffy_assemble(hubris, path, output);
    }

    if !subargs.filter.is_empty() {
        bail!(
            "extraneous command line argument; missing {}?",
            if subargs.call.is_some() {
                "--arguments"
            } else {
                "--list or --call?"
            }
        );
    }

    //
    // Online operations: these need a live target.  In Unattached
    // mode, the core may be None or archive-only; we need to attach
    // to a live target for --exec, --call, and -L.
    //
    if context.cli.probe.is_none() && context.cli.ip.is_none() {
        bail!(
            "this operation requires a target connection; \
             specify a probe (-p) or network address (--ip)"
        );
    }

    {
        let cli = &context.cli;
        let hubris_ref = context.archive.as_ref().unwrap();
        context.core = Some(humility_cmd::attach_live(cli, hubris_ref)?);
    }

    let core = &mut **context.core.as_mut().unwrap();

    if (subargs.call.is_some() || subargs.exec.is_some()) && core.is_dump() {
        bail!("can't make HIF calls on a dump");
    }

    if let Some(ref exec_path) = subargs.exec {
        return hiffy_exec(hubris, core, &subargs, exec_path);
    }

    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;

    if let Some(call) = subargs.call {
        let func: Vec<&str> = call.split('.').collect();

        if func.len() != 2 {
            bail!("calls must be interface.operation (-l to list)");
        }

        let mut args = vec![];

        for arg in &subargs.arguments {
            let arg: Vec<&str> = arg.split('=').collect();

            if arg.len() != 2 {
                bail!("arguments must be argument=value (-l to list)");
            }

            args.push((arg[0], idol::IdolArgument::String(arg[1])));
        }

        let task = match subargs.task {
            Some(task) => Some(hubris.try_lookup_task(&task)?),
            None => None,
        };

        let op = idol::IdolOperation::new(hubris, func[0], func[1], task)?;

        // Special-case handling: if someone didn't specify `--input`, but
        // is piping data into the `humility` command, then we use `stdin` as
        // the input source.
        let input = if let Some(input) = subargs.input {
            Some(std::fs::read(input)?)
        } else if op.operation.leases.len() == 1
            && op.operation.leases[0].read
            && !op.operation.leases[0].write
            && atty::isnt(atty::Stream::Stdin)
        {
            let mut v = vec![];
            std::io::stdin().read_to_end(&mut v)?;
            Some(v)
        } else {
            None
        };

        let (return_code, output) = {
            let mut output = if let Some(read_size) = subargs.num {
                let read = vec![0u8; read_size];
                Some(read)
            } else {
                None
            };

            (
                hiffy_call(
                    hubris,
                    core,
                    &mut context,
                    &op,
                    &args,
                    input.as_deref(),
                    output.as_deref_mut(),
                )?,
                output,
            )
        };

        hiffy_print_result(hubris, &op, return_code)?;
        if let Some(data) = output {
            if let Some(out) = &subargs.output {
                std::fs::write(out, &data)
                    .context(format!("Could not write to {}", out))?;
                println!("Wrote {} bytes to '{}'", data.len(), out);
            } else if subargs.hex {
                println!("Data: {:x?}", data);
            } else {
                Dumper::new().dump(&data, 0x0);
            }
        }

        return Ok(());
    }

    if !subargs.listfuncs {
        bail!("expected one of -l, -L, -c, or --exec");
    }

    let funcs = context.functions();
    let mut byid: Vec<Option<(&String, &HiffyFunction)>> = vec![];

    byid.resize(funcs.len(), None);

    for (name, func) in &funcs.0 {
        let ndx = func.id.0 as usize;

        if ndx >= byid.len() {
            bail!("ID for function {} ({}) exceeds bounds", name, ndx);
        }

        if let Some((_, _)) = byid[ndx] {
            bail!("function ID {} has conflics", ndx);
        }

        byid[ndx] = Some((name, func));
    }

    println!("{:>3} {:30} #ARGS", "ID", "FUNCTION");

    for (i, id) in byid.iter().enumerate() {
        if let Some((name, func)) = id {
            println!("{:3} {:30} {}", i, name, func.args.len());
        } else {
            bail!("missing function for ID {}", i);
        }
    }

    Ok(())
}

/// Verify a .hif program offline (no target needed).
fn hiffy_verify(hubris: &HubrisArchive, path: &str) -> Result<()> {
    let source = std::fs::read_to_string(path)
        .with_context(|| format!("reading {path}"))?;

    let config = TargetConfig::from_archive(hubris)
        .context("extracting target config from archive")?;
    let asm = humility_hif_assembler::assembler::HifAssembler::new(config);

    let report = asm.verify(&source);
    print!("{report}");

    if !report.ok {
        bail!("verification failed");
    }

    let output =
        asm.assemble(&source).with_context(|| format!("assembling {path}"))?;
    println!("{}", output.stats);

    for w in &output.warnings {
        humility::msg!("warning: {w}");
    }

    // Disassemble: show ops in raw syntax with hex comments
    println!(
        "Ops ({} bytes, {} ops):\n{}",
        output.bundle.text.len(),
        output.ops.len(),
        asm.disassemble(&output.ops),
    );

    Ok(())
}

/// Assemble a .hif program to a .hifb bundle offline (no target needed).
fn hiffy_assemble(
    hubris: &HubrisArchive,
    path: &str,
    output_path: &str,
) -> Result<()> {
    let source = std::fs::read_to_string(path)
        .with_context(|| format!("reading {path}"))?;

    let config = TargetConfig::from_archive(hubris)
        .context("extracting target config from archive")?;
    let asm = humility_hif_assembler::assembler::HifAssembler::new(config);
    let output =
        asm.assemble(&source).with_context(|| format!("assembling {path}"))?;

    for w in &output.warnings {
        humility::msg!("warning: {w}");
    }

    output
        .bundle
        .write_to_file(output_path)
        .with_context(|| format!("writing bundle to {output_path}"))?;

    humility::msg!(
        "assembled {} ({} bytes text, {} estimated results)",
        output_path,
        output.bundle.text.len(),
        output.stats.total_i2c_transactions() + output.stats.idol_calls,
    );
    println!("{}", output.stats);

    Ok(())
}

/// Execute a .hif (text) or .hifb (bundle) program on the target.
fn hiffy_exec(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    subargs: &HiffyArgs,
    path: &str,
) -> Result<()> {
    let is_bundle = path.ends_with(".hifb");

    let (ops, stats, source_text) = if is_bundle {
        // Load pre-assembled bundle
        let bundle = HifBundle::read_from_file(path)
            .with_context(|| format!("reading bundle {path}"))?;

        // Validate image ID
        if let Some(image_id) = hubris.image_id() {
            bundle.validate_image_id(image_id)?;
        }

        let ops: Vec<hif::Op> = postcard::from_bytes(&bundle.text)
            .context("deserializing ops from bundle")?;
        let source = bundle.metadata.source_text.clone();

        // Compute stats from source if available
        let stats = source.as_ref().and_then(|src| {
            humility_hif_assembler::parser::parse(src).ok().map(|prog| {
                humility_hif_assembler::stats::compute_stats(&prog.statements)
            })
        });

        (ops, stats, source)
    } else {
        // Assemble from .hif text
        let source = std::fs::read_to_string(path)
            .with_context(|| format!("reading {path}"))?;

        let config = TargetConfig::from_archive(hubris)
            .context("extracting target config from archive")?;
        let asm = humility_hif_assembler::assembler::HifAssembler::new(config);
        let output = asm
            .assemble(&source)
            .with_context(|| format!("assembling {path}"))?;

        for w in &output.warnings {
            humility::msg!("warning: {w}");
        }

        // Save bundle if requested
        if let Some(ref save_path) = subargs.save_bundle {
            output
                .bundle
                .write_to_file(save_path)
                .with_context(|| format!("writing bundle to {save_path}"))?;
            humility::msg!("saved bundle to {save_path}");
        }

        (output.ops, Some(output.stats), Some(source))
    };

    // Print program info
    if let Some(ref stats) = stats {
        humility::msg!("program: {path}");
        humility::msg!(
            "expected: {} I2C transactions, {} Idol calls",
            stats.total_i2c_transactions(),
            stats.idol_calls,
        );
    }

    // Build result kind list for decoding.  Each result-producing
    // statement maps to a kind (Idol or Raw).
    let result_kinds =
        source_text.as_ref().and_then(|src| build_result_kinds(src).ok());

    // Execute via HiffyContext
    let mut hctx = HiffyContext::new(hubris, core, subargs.timeout)?;
    let start = Instant::now();
    let results = hctx.run(core, &ops, None)?;
    let elapsed = start.elapsed();

    // Count successes and errors
    let n_ok = results.iter().filter(|r| r.is_ok()).count();
    let n_err = results.iter().filter(|r| r.is_err()).count();

    if subargs.json {
        // JSON output with decoded samples
        let mut decoded_samples = vec![];
        let sample_indices = result_sample_indices(&results, 3, 3);
        for i in &sample_indices {
            let decoded =
                decode_result(hubris, &results[*i], result_kinds.as_ref(), *i);
            decoded_samples.push(serde_json::json!({
                "index": i,
                "value": decoded,
            }));
        }

        let json = serde_json::json!({
            "ok": n_err == 0,
            "results": results.len(),
            "successes": n_ok,
            "errors": n_err,
            "elapsed_ms": elapsed.as_millis() as u64,
            "stats": stats.as_ref().map(|s| serde_json::json!({
                "i2c_transactions": s.total_i2c_transactions(),
                "i2c_read_bytes": s.i2c_read_bytes,
                "i2c_write_bytes": s.i2c_write_bytes,
                "idol_calls": s.idol_calls,
                "mux_switches": s.mux_switches,
                "buses": s.buses_touched,
            })),
            "samples": decoded_samples,
            "source": source_text,
        });
        println!("{}", serde_json::to_string_pretty(&json)?);
    } else {
        // Human output
        humility::msg!(
            "executed in {:.1}ms: {} results ({} ok, {} err)",
            elapsed.as_secs_f64() * 1000.0,
            results.len(),
            n_ok,
            n_err,
        );

        if n_err > 0 {
            // Show all errors (up to 10), decoded
            let mut shown = 0;
            for (i, result) in results.iter().enumerate() {
                if result.is_err() {
                    let decoded =
                        decode_result(hubris, result, result_kinds.as_ref(), i);
                    humility::msg!("  [{i}] {decoded}");
                    shown += 1;
                    if shown >= 10 {
                        humility::msg!(
                            "  ... and {} more errors",
                            n_err - shown,
                        );
                        break;
                    }
                }
            }
        }

        // Show first and last success as samples
        if n_ok > 0 && results.len() <= 50 {
            // For small result sets, show all
            for (i, result) in results.iter().enumerate() {
                let decoded =
                    decode_result(hubris, result, result_kinds.as_ref(), i);
                humility::msg!("  [{i}] {decoded}");
            }
        } else if n_ok > 0 {
            // For large result sets, show first and last
            let decoded =
                decode_result(hubris, &results[0], result_kinds.as_ref(), 0);
            humility::msg!("  [0] {decoded}");
            if results.len() > 2 {
                humility::msg!(
                    "  ... ({} more results) ...",
                    results.len() - 2
                );
            }
            let last = results.len() - 1;
            let decoded = decode_result(
                hubris,
                &results[last],
                result_kinds.as_ref(),
                last,
            );
            humility::msg!("  [{last}] {decoded}");
        }
    }

    if n_err > 0 {
        bail!(
            "program completed with {} error{} out of {} results",
            n_err,
            if n_err == 1 { "" } else { "s" },
            results.len(),
        );
    }

    Ok(())
}

/// What produced a given result — used for decoding.
#[derive(Clone)]
enum ResultKind {
    /// An Idol call: (interface, operation).
    Idol(String, String),
    /// Raw bytes (I2C, QSPI, etc.)
    Raw,
}

/// Build a list of result kinds by walking the parsed program and
/// expanding loops.  One entry per expected result, in order.
fn build_result_kinds(source: &str) -> Result<Vec<ResultKind>> {
    let parsed = humility_hif_assembler::parser::parse(source)
        .map_err(|e| anyhow::anyhow!("{e}"))?;
    let mut kinds = vec![];
    walk_for_kinds(&parsed.statements, &mut kinds);
    Ok(kinds)
}

fn walk_for_kinds(
    stmts: &[humility_hif_assembler::parser::Located<
        humility_hif_assembler::parser::Statement,
    >],
    kinds: &mut Vec<ResultKind>,
) {
    use humility_hif_assembler::parser::Statement;

    for stmt in stmts {
        match &stmt.value {
            Statement::IdolCall { interface, operation, .. } => {
                kinds.push(ResultKind::Idol(
                    interface.clone(),
                    operation.clone(),
                ));
            }
            Statement::I2cRead { .. }
            | Statement::I2cWrite { .. }
            | Statement::I2cScan { .. }
            | Statement::I2cRegScan { .. }
            | Statement::Call { .. } => {
                kinds.push(ResultKind::Raw);
            }
            Statement::Repeat { count, body, .. } => {
                let start = kinds.len();
                walk_for_kinds(body, kinds);
                let body_kinds: Vec<_> = kinds[start..].to_vec();
                for _ in 1..*count {
                    kinds.extend(body_kinds.iter().cloned());
                }
            }
            Statement::Sleep { .. } | Statement::Raw { .. } => {}
        }
    }
}

/// Decode a single result for display.
///
/// For Idol calls, this creates an `IdolOperation` via DWARF lookup
/// on each call.  This is fine for sampled results but would benefit
/// from caching if we ever decode all results in a large set.
fn decode_result(
    hubris: &HubrisArchive,
    result: &Result<Vec<u8>, humility_hiffy::IpcError>,
    kinds: Option<&Vec<ResultKind>>,
    index: usize,
) -> String {
    let kind = kinds.and_then(|k| k.get(index));

    match (result, kind) {
        (Ok(bytes), Some(ResultKind::Idol(iface, op_name))) => {
            if let Ok(op) =
                idol::IdolOperation::new(hubris, iface, op_name, None)
            {
                match hiffy_decode(
                    hubris,
                    &op,
                    Ok::<_, IpcError>(bytes.clone()),
                ) {
                    Ok(decoded) => format!(
                        "{iface}.{op_name}() => {}",
                        hiffy_format_result(hubris, decoded)
                    ),
                    Err(_) => {
                        format!("{iface}.{op_name}() => Ok({:02x?})", bytes)
                    }
                }
            } else {
                format!("Ok({:02x?})", bytes)
            }
        }
        (Err(e), Some(ResultKind::Idol(iface, op_name))) => {
            if let Ok(op) =
                idol::IdolOperation::new(hubris, iface, op_name, None)
            {
                match hiffy_decode(hubris, &op, Err::<Vec<u8>, _>(*e)) {
                    Ok(decoded) => format!(
                        "{iface}.{op_name}() => {}",
                        hiffy_format_result(hubris, decoded)
                    ),
                    Err(_) => {
                        format!("{iface}.{op_name}() => Err({e:?})")
                    }
                }
            } else {
                format!("Err({e:?})")
            }
        }
        (Ok(bytes), _) => format!("Ok({:02x?})", bytes),
        (Err(e), _) => format!("Err({e:?})"),
    }
}

/// Pick sample indices: first N errors, first M successes, last success.
fn result_sample_indices(
    results: &[Result<Vec<u8>, humility_hiffy::IpcError>],
    max_errors: usize,
    max_ok: usize,
) -> Vec<usize> {
    let mut indices = vec![];
    let mut err_count = 0;
    let mut ok_count = 0;
    let mut last_ok = None;

    for (i, r) in results.iter().enumerate() {
        if r.is_err() && err_count < max_errors {
            indices.push(i);
            err_count += 1;
        } else if r.is_ok() {
            if ok_count < max_ok {
                indices.push(i);
            }
            ok_count += 1;
            last_ok = Some(i);
        }
    }

    // Always include the last success if not already there
    if let Some(last) = last_ok.filter(|l| !indices.contains(l)) {
        indices.push(last);
    }

    indices.sort();
    indices.dedup();
    indices
}

pub fn init() -> Command {
    Command {
        app: HiffyArgs::command(),
        name: "hiffy",
        run: hiffy,
        kind: CommandKind::Unattached { archive: Archive::Required },
    }
}
