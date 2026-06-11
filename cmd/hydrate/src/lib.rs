// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility hydrate`
//!
//! `humility hydrate` combines a raw memory dump (obtained from MGS) and a
//! Hubris archive, forming a proper Hubris dump.
//!
//! ```console
//! $ humility -a build-grapefruit-image-default.zip hydrate hubris.dry.0
//! ```
//!
//! The archive is required, because the raw memory dump does not contain debug
//! information.
//!
//! By default, this writes to `hubris.core.TASK_NAME.N` (where `N` is the
//! lowest available value); use `--out` to specify a different path name.

use anyhow::{Context, Result, bail};
use clap::{ArgGroup, Parser};
use humility::{
    hubris::HubrisFlashMap,
    log::{info, warn},
};
use humility_arch_arm::ARMRegister;
use humility_cli::{ExecutionContext, humility_cmd};
use std::{collections::BTreeMap, io::Read, path::PathBuf};

#[derive(Parser, Debug)]
#[clap(
    name = "hydrate", about = env!("CARGO_PKG_DESCRIPTION"),
    group = ArgGroup::new("target").multiple(false)
)]
pub struct HydrateArgs {
    /// Path to write the resulting dump
    #[clap(short, long)]
    out: Option<PathBuf>,

    /// Path to the raw memory dump
    file: PathBuf,
}

struct DryCore {
    flash: HubrisFlashMap,
    mem: BTreeMap<u32, Vec<u8>>,
}

// Helper macro to stub out functions
macro_rules! unsupported{
    ($fn_name:ident($($arg_name:ident: $arg_type:ty),*)) => {
        unsupported!($fn_name($($arg_name: $arg_type),*) -> Result<()>);
    };
    ($fn_name:ident($($arg_name:ident: $arg_type:ty),*) -> $out:ty) => {
        fn $fn_name(&mut self, $($arg_name: $arg_type),*) -> $out {
            bail!(concat!(
                "DryCore does not support ",
                stringify!($fn_name)))
        }
    };
}

impl humility::core::Core for DryCore {
    unsupported!(run());
    unsupported!(halt());
    unsupported!(write_8(_addr: u32, _data: &[u8]));
    unsupported!(op_done());
    unsupported!(op_start());
    unsupported!(read_reg(_reg: ARMRegister) -> Result<u32>);
    unsupported!(write_word_32(_addr: u32, _data: u32));

    fn info(&self) -> (String, Option<String>) {
        ("DryCore".to_owned(), None)
    }
    fn is_archive(&self) -> bool {
        false
    }
    fn is_dump(&self) -> bool {
        true // I guess?
    }
    fn is_net(&self) -> bool {
        false
    }

    fn read_8(&mut self, addr: u32, data: &mut [u8]) -> Result<()> {
        if self.flash.read(addr, data).is_some() {
            return Ok(());
        }

        let Some((base, mem)) = self.mem.range(0..=addr).next_back() else {
            bail!("addr {addr:#08x} is below memory range");
        };
        let offset = (addr - base) as usize;
        let end = offset + data.len();
        if end > mem.len() {
            bail!("region is not large enough; {end:#x} > {:#x}", mem.len());
        }
        data.copy_from_slice(&mem[offset..end]);
        Ok(())
    }
}

fn run(subargs: HydrateArgs, context: &mut ExecutionContext) -> Result<()> {
    let f = std::fs::File::open(&subargs.file)?;
    let mut z = zip::ZipArchive::new(f)?;
    let log = context.log();

    let mut s = String::new();
    z.by_name("dump.json")?
        .read_to_string(&mut s)
        .with_context(|| "could not find `dump.json` in the ZIP archive")?;

    #[derive(serde::Deserialize)]
    struct DumpInfo {
        format: u64,
        task_index: u16,
        crash_time: u64,
        board_name: String,
        git_commit: String,
        archive_id: String,
        fw_version: Option<String>,
    }
    let info: DumpInfo = serde_json::from_str(&s)?;
    if info.format != 1 {
        warn!(
            log,
            "unexpected format in `dump.json`: expected 1, got {}", info.format
        );
    }

    let mut archive_id = [0u8; 8];
    for (i, b) in archive_id.iter_mut().enumerate() {
        *b = u8::from_str_radix(&info.archive_id[i * 2..][..2], 16)?;
    }

    // We have to collect filenames separately, because we can't iterate over
    // `file_names` and read them with `ZipArchive::by_name` simultaneously.
    let mut mem = BTreeMap::new();
    let mem_files = z
        .file_names()
        .filter(|f| f.starts_with("0x"))
        .map(|s| s.to_owned())
        .collect::<Vec<_>>();
    for f in mem_files {
        let num = f.strip_suffix(".bin").unwrap().strip_prefix("0x").unwrap();
        let addr = u32::from_str_radix(num, 16)
            .with_context(|| format!("invalid hex string: {num}"))?;
        let mut data = z.by_name(&f).unwrap();
        let mut v = vec![];
        data.read_to_end(&mut v)?;
        mem.insert(addr, v);
    }

    info!(log, "read dehydrated crash dump");
    info!(log, "  task index: {}", info.task_index);
    info!(log, "  crash time: {}", info.crash_time);
    info!(log, "  archive id: {archive_id:02x?}");
    info!(log, "  board:      {}", info.board_name);
    info!(log, "  git commit: {}", info.git_commit);
    info!(
        log,
        "  version:    {}",
        info.fw_version.as_deref().unwrap_or("[missing]")
    );
    info!(log, "  {} memory regions:", mem.len());
    for (k, v) in &mem {
        info!(log, "    {k:#08x}: {} bytes", v.len());
    }

    // compare archive ID
    let archive = &context.cli.archive()?;
    let expected_id = archive.image_id();
    if archive_id != expected_id {
        bail!(
            "image ID mismatch: archive ID is {expected_id:02x?}, \
             dump ID wants {archive_id:02x?}"
        );
    }
    let mut core = DryCore { mem, flash: HubrisFlashMap::new(archive)? };

    archive.dump(
        &mut core,
        Some(humpty::DumpTask {
            magic: humpty::DUMP_TASK_MAGIC,
            id: info.task_index,
            pad: 0u32,
            time: info.crash_time,
        }),
        subargs.out.as_deref(),
        Some(std::time::Instant::now()),
        log,
    )
}

humility_cmd!(HydrateArgs, run);
