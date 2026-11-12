// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility check`
//!
//! TODO: Add docs

use anyhow::{Context, Error, Result, anyhow, bail};
use clap::Parser;
use humility::hubris::*;
use humility::mem::InMemoryCore;
use humility_cli::{ExecutionContext, humility_cmd};
use humility_doppel::Task;

#[derive(Parser, Debug)]
#[clap(name = "check", about = env!("CARGO_PKG_DESCRIPTION"))]
pub struct CheckArgs {
    /// TODO: Add help text
    #[clap(long, short)]
    verbose: bool,
}

// TODO: Add comment
const MAX_PASSES: usize = 256;

/// TODO: Add docs
struct Outcome {
    kind: &'static str,
    module: String,
    name: String,
    passes: usize,
    error: Option<Error>,
}

fn check(subargs: CheckArgs, context: &mut ExecutionContext) -> Result<()> {
    let hubris = &context.cli.archive()?;

    let mut outcomes = vec![check_regions(hubris)];
    outcomes.extend(check_tasks(hubris));
    for ((name, var), def) in cmd_ringbuf::ringbuf_variables(hubris) {
        outcomes.push(check_variable(
            hubris,
            "ringbuf",
            name,
            var,
            def,
            |buf| {
                let def = def.unwrap();
                cmd_ringbuf::decode_ringbuf(hubris, def, buf).map(|_| ())
            },
        ));
    }
    for ((name, var), def) in cmd_counters::counter_variables(hubris) {
        outcomes.push(check_variable(
            hubris,
            "counters",
            name,
            var,
            def,
            |buf| {
                let def = def.unwrap();
                cmd_counters::decode_counters(hubris, def, buf).map(|_| ())
            },
        ));
    }

    let mut failed = 0;
    for o in &outcomes {
        let status = if o.error.is_some() { "FAIL" } else { "ok" };
        println!(
            "{status:4} {:8} {:20} {} ({} passes)",
            o.kind, o.module, o.name, o.passes
        );
        if let Some(e) = &o.error {
            failed += 1;
            if subargs.verbose {
                println!("     {e:#}");
            } else {
                println!("     {e}");
            }
        }
    }

    println!("{} checks, {failed} failed", outcomes.len());
    if failed > 0 {
        bail!("{failed} checks failed");
    }
    Ok(())
}

/// TODO: Add docs
fn check_regions(hubris: &HubrisArchive) -> Outcome {
    let result = InMemoryCore::from_archive(hubris)
        .and_then(|mut core| hubris.regions(&mut core).map(|_| ()));
    Outcome {
        kind: "kernel",
        module: "kernel".to_owned(),
        name: "regions".to_owned(),
        passes: 1,
        error: result.err(),
    }
}

/// TODO: Add docs
fn check_tasks(hubris: &HubrisArchive) -> Option<Outcome> {
    let var = hubris.lookup_variable("HUBRIS_TASK_TABLE_SPACE").ok()?;
    let (passes, error) = run_passes(hubris, var.goff, var.size, |buf| {
        let mut core = InMemoryCore::from_archive(hubris)?;
        core.add_ram_region(var.addr, buf.to_vec())
            .map_err(|e| anyhow!("could not add RAM region: {e:?}"))?;
        let (base, count) = hubris.task_table(&mut core)?;
        let task_t = hubris.lookup_struct_byname("Task")?;
        Task::load_tcbs(hubris, &mut core, base, count as usize, task_t)?;
        Ok(())
    });
    Some(Outcome {
        kind: "kernel",
        module: "kernel".to_owned(),
        name: "HUBRIS_TASK_TABLE_SPACE".to_owned(),
        passes,
        error,
    })
}

/// TODO: Add docs
fn check_variable(
    hubris: &HubrisArchive,
    kind: &'static str,
    name: &str,
    var: &HubrisVariable,
    def: Option<&HubrisStruct>,
    decode: impl FnMut(&[u8]) -> Result<()>,
) -> Outcome {
    let module = cmd_ringbuf::taskname(hubris, var).unwrap_or("???").to_owned();
    let (passes, error) = if def.is_some() {
        run_passes(hubris, var.goff, var.size, decode)
    } else {
        (0, Some(anyhow!("could not look up type: {:?}", var.goff)))
    };
    Outcome { kind, module, name: name.to_owned(), passes, error }
}

/// TODO: Add docs
fn run_passes(
    hubris: &HubrisArchive,
    goff: HubrisGoff,
    size: usize,
    mut decode: impl FnMut(&[u8]) -> Result<()>,
) -> (usize, Option<Error>) {
    let mut pass = 0;
    let mut needed = 1;
    while pass < needed.min(MAX_PASSES) {
        let mut buf = vec![0u8; size];
        match fill(hubris, goff, pass, &mut buf, 0) {
            Ok(n) => needed = n,
            Err(e) => return (pass, Some(e.context("could not fill"))),
        }
        if let Err(e) = decode(&buf) {
            return (pass + 1, Some(e.context(format!("pass {pass}"))));
        }
        pass += 1;
    }
    (pass, None)
}

/// TODO: Add docs
fn fill(
    hubris: &HubrisArchive,
    goff: HubrisGoff,
    pass: usize,
    buf: &mut [u8],
    offset: usize,
) -> Result<usize> {
    let mut needed = 1;
    match hubris.lookup_type(goff)? {
        HubrisType::Base(_) | HubrisType::Ptr(_) => (),
        HubrisType::Struct(s) => {
            for m in &s.members {
                let n = fill(hubris, m.goff, pass, buf, offset + m.offset)?;
                needed = needed.max(n);
            }
        }
        HubrisType::Array(a) => {
            let size = hubris.lookup_type(a.goff)?.size(hubris)?;
            for i in 0..a.count {
                let n = fill(hubris, a.goff, pass, buf, offset + i * size)?;
                needed = needed.max(n);
            }
        }
        HubrisType::Union(u) => {
            // TODO: Add comment
            let variant = u
                .variants
                .iter()
                .find(|v| v.name == "value")
                .or(u.variants.last());
            if let Some(v) = variant
                && let Some(g) = v.goff
            {
                needed = fill(hubris, g, pass, buf, offset + v.offset)?;
            }
        }
        HubrisType::Enum(e) => {
            if e.variants.is_empty() {
                return Ok(needed);
            }
            needed = e.variants.len();
            let v = &e.variants[pass % e.variants.len()];
            if let Some(g) = v.goff {
                let n = fill(hubris, g, pass, buf, offset + v.offset)?;
                needed = needed.max(n);
            }
            // TODO: Discuss ordering.
            if let (Some(HubrisDiscriminant::Value(dgoff, doff)), Some(tag)) =
                (&e.discriminant, v.tag)
            {
                write_tag(hubris, *dgoff, tag, buf, offset + doff)
                    .with_context(|| format!("{}::{}", e.name, v.name))?;
            }
        }
    }
    Ok(needed)
}

/// TODO: Add docs
fn write_tag(
    hubris: &HubrisArchive,
    goff: HubrisGoff,
    tag: Tag,
    buf: &mut [u8],
    offset: usize,
) -> Result<()> {
    let size = hubris.lookup_basetype(goff)?.size;
    let bytes = match tag {
        Tag::Unsigned(u) => u.to_le_bytes(),
        Tag::Signed(i) => i.to_le_bytes(),
    };
    let Some(dest) = buf.get_mut(offset..offset + size) else {
        bail!("tag at {offset:#x} ({size} bytes) is out of bounds");
    };
    dest.copy_from_slice(&bytes[..size]);
    Ok(())
}

humility_cmd!(CheckArgs, check);
