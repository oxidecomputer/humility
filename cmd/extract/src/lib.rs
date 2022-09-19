// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility extract`
//!
//! `humility extract` extracts a file from either a Hubris archive or a
//! dump.  By default, the contents of the file are sent to standard output,
//! e.g.:
//!
//! ```console
//! % humility -a /path/to/my/hubris-archive.zip extract README.TXT
//! humility: extracting README.TXT to stdout
//! This is a build archive containing firmware build artifacts.
//!
//! - app.toml is the config file used to build the firmware.
//! - git-rev is the commit it was built from, with optional dirty flag.
//! - info/ contains human-readable data like logs.
//! - elf/ contains ELF images for all firmware components.
//! - elf/tasks/ contains each task by name.
//! - elf/kernel is the kernel.
//! - img/ contains the final firmware images.
//! - debug/ contains OpenOCD and GDB scripts, if available.
//! ```
//!
//! To list all of the files in an archive, use the `--list` (`-l`) option:
//!
//! ```console
//! % humility -a /path/to/my/hubris-archive.zip extract --list
//!         SIZE NAME
//!          462 README.TXT
//!           46 git-rev
//!         5289 app.toml
//!         1937 chip.toml
//!       197456 elf/task/jefe
//!       208836 elf/task/sys
//!       288440 elf/task/i2c_driver
//!       309100 elf/task/spi_driver
//!      1059092 elf/task/net
//!        65156 elf/task/user_leds
//!        87284 elf/task/ping
//!        31228 elf/task/pong
//!       178600 elf/task/udpecho
//!       370756 elf/task/hiffy
//!       310184 elf/task/hf
//!       286556 elf/task/hash_driver
//!        17612 elf/task/idle
//!       201964 elf/task/rng_driver
//!       555820 elf/kernel
//!         1550 info/allocations.txt
//!         4195 info/map.txt
//!       632426 img/combined.srec
//!       307060 img/combined.elf
//!       862933 img/combined.ihex
//!       307200 img/combined.bin
//!       632426 img/final.srec
//!       307060 img/final.elf
//!       862933 img/final.ihex
//!       307200 img/final.bin
//!         1790 img/flash.ron
//!         1047 debug/script.gdb
//!         1586 debug/openocd.cfg
//!          235 debug/openocd.gdb
//! ```
//!
//! Note that the file specified is treated as a substring; if the substring
//! matches exactly one file, it will be extracted:
//!
//! ```console
//! % humility -d ./hubris.core.79 extract map
//! humility: extracting info/map.txt to stdout
//! ADDRESS  END          SIZE FILE
//! 08000000 08000298      298 target/demo-stm32h753-nucleo/dist/kernel
//! 08000298 08004420     4188 target/demo-stm32h753-nucleo/dist/kernel
//! 08004420 08005604     11e4 target/demo-stm32h753-nucleo/dist/kernel
//! 08005a00 08005a58       58 target/demo-stm32h753-nucleo/dist/idle
//! 08005c00 08005e10      210 target/demo-stm32h753-nucleo/dist/pong
//! 08005e10 08005e88       78 target/demo-stm32h753-nucleo/dist/pong
//! 08006000 080076f4     16f4 target/demo-stm32h753-nucleo/dist/jefe
//! 080076f4 08007c08      514 target/demo-stm32h753-nucleo/dist/jefe
//! 08007c08 08007c08        0 target/demo-stm32h753-nucleo/dist/jefe
//! 08008000 08009798     1798 target/demo-stm32h753-nucleo/dist/udpecho
//! 08009798 08009b90      3f8 target/demo-stm32h753-nucleo/dist/udpecho
//! 08009b90 08009b90        0 target/demo-stm32h753-nucleo/dist/udpecho
//! 08010000 08014020     4020 target/demo-stm32h753-nucleo/dist/hiffy
//! 08014020 08015134     1114 target/demo-stm32h753-nucleo/dist/hiffy
//! 08015134 0801574c      618 target/demo-stm32h753-nucleo/dist/hiffy
//! 08018000 08019970     1970 target/demo-stm32h753-nucleo/dist/i2c_driver
//! 08019970 08019ff4      684 target/demo-stm32h753-nucleo/dist/i2c_driver
//! 08019ff4 0801a300      30c target/demo-stm32h753-nucleo/dist/i2c_driver
//! 0801c000 0801de78     1e78 target/demo-stm32h753-nucleo/dist/spi_driver
//! 0801de78 0801e898      a20 target/demo-stm32h753-nucleo/dist/spi_driver
//! 0801e898 0801eca4      40c target/demo-stm32h753-nucleo/dist/spi_driver
//! 08020000 0802a554     a554 target/demo-stm32h753-nucleo/dist/net
//! 0802a554 0802d74c     31f8 target/demo-stm32h753-nucleo/dist/net
//! 0802d74c 0802d7dc       90 target/demo-stm32h753-nucleo/dist/net
//! 08040000 08041ea0     1ea0 target/demo-stm32h753-nucleo/dist/hf
//! 08041ea0 080426a8      808 target/demo-stm32h753-nucleo/dist/hf
//! 080426a8 080426a8        0 target/demo-stm32h753-nucleo/dist/hf
//! 08044000 08044b8c      b8c target/demo-stm32h753-nucleo/dist/ping
//! 08044b8c 08044d34      1a8 target/demo-stm32h753-nucleo/dist/ping
//! 08044d34 08044d34        0 target/demo-stm32h753-nucleo/dist/ping
//! 08046000 080478b4     18b4 target/demo-stm32h753-nucleo/dist/hash_driver
//! 080478b4 08047f68      6b4 target/demo-stm32h753-nucleo/dist/hash_driver
//! 08047f68 08047f68        0 target/demo-stm32h753-nucleo/dist/hash_driver
//! 08048000 08048fcc      fcc target/demo-stm32h753-nucleo/dist/rng_driver
//! 08048fcc 08049474      4a8 target/demo-stm32h753-nucleo/dist/rng_driver
//! 08049474 08049474        0 target/demo-stm32h753-nucleo/dist/rng_driver
//! 0804a000 0804a634      634 target/demo-stm32h753-nucleo/dist/sys
//! 0804a634 0804a67c       48 target/demo-stm32h753-nucleo/dist/sys
//! 0804a800 0804ac0c      40c target/demo-stm32h753-nucleo/dist/user_leds
//! 0804ac0c 0804ac7c       70 target/demo-stm32h753-nucleo/dist/user_leds
//! 20000400 20000400        0 target/demo-stm32h753-nucleo/dist/kernel
//! 30000000 30000000        0 target/demo-stm32h753-nucleo/dist/net
//! ```
//!
//! If the substring matches more than one file, the command will fail and
//! the matching files will be displayed:
//!
//! ```console
//! % humility -d ./hubris.core.79 extract toml
//! humility extract failed: "toml" matches multiple files: app.toml, stm32h7.toml
//! ```
//!
//! To redirect output to a particular file, use the `--output` (`-o`) option.
//!
//! To dump the entire archive, leave the file unspecified.  (Note that
//! extracting the entire archive requires the specification of an output file
//! to prevent accidental blasts of binary content to the console.)
//!

use anyhow::{bail, Result};
use clap::{CommandFactory, Parser};
use humility::cli::Subcommand;
use humility_cmd::{Command, CommandKind};
use std::fs::File;
use std::io::Cursor;
use std::io::{self, Read, Write};

#[derive(Parser, Debug)]
#[clap(name = "extract", about = env!("CARGO_PKG_DESCRIPTION"))]
struct ExtractArgs {
    /// list contents
    #[clap(long, short)]
    list: bool,

    /// file for output
    #[clap(long, short)]
    output: Option<String>,

    /// Optional file to extract
    file: Option<String>,
}

fn extract(context: &mut humility::ExecutionContext) -> Result<()> {
    let hubris = context.archive.as_ref().unwrap();
    let archive = hubris.archive();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let subargs = ExtractArgs::try_parse_from(subargs)?;

    if subargs.list {
        let cursor = Cursor::new(archive);
        let mut archive = zip::ZipArchive::new(cursor)?;

        println!("{:>12} NAME", "SIZE");

        for i in 0..archive.len() {
            let file = archive.by_index(i)?;
            println!("{:12} {}", file.size(), file.name());
        }

        return Ok(());
    }

    let buffer = if let Some(ref filename) = subargs.file {
        let cursor = Cursor::new(archive);
        let mut archive = zip::ZipArchive::new(cursor)?;
        let mut found = vec![];

        for i in 0..archive.len() {
            let file = archive.by_index(i)?;

            if file.name().contains(filename) {
                found.push((i, file.name().to_string()));
            }
        }

        if found.is_empty() {
            bail!(
                "\"{}\" doesn't match any files (\"--list\" to list)",
                filename
            );
        }

        if found.len() > 1 {
            bail!(
                "\"{}\" matches multiple files: {}",
                filename,
                found
                    .iter()
                    .map(|(_, name)| name.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            );
        }

        humility::msg!("extracting {} to stdout", found[0].1);

        let mut file = archive.by_index(found[0].0)?;

        let mut buffer = Vec::new();
        file.read_to_end(&mut buffer)?;
        buffer
    } else {
        archive.to_vec()
    };

    if let Some(output) = subargs.output {
        let mut ofile = File::create(output)?;
        ofile.write_all(&buffer)?;
    } else {
        //
        // As a precaution against naive use, we force an output file to be
        // specified if the entire archive is to be written.
        //
        if subargs.file.is_none() {
            bail!("must specify output file name to extract entire archive");
        }

        io::stdout().write_all(&buffer)?;
    }

    Ok(())
}

pub fn init() -> Command {
    Command {
        app: ExtractArgs::command(),
        name: "extract",
        run: extract,
        kind: CommandKind::Raw,
    }
}
