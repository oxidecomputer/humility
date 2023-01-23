// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility readmem`
//!
//! `humility readmem` allows one to read a specified range of memory:
//!
//! ```console
//! % humility readmem 0x00011b00
//! humility: attached via DAPLink
//! humility: reading at 0x11b00 for 256 bytes
//!              \/  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f
//! 0x00011b00 | 00 0f 1a bf 81 54 bc f1 01 0f d0 bd 02 44 bc f1 | .....T.......D..
//! 0x00011b10 | 02 0f 51 70 00 d1 d0 bd 91 70 d0 bd 69 00 01 00 | ..Qp.....p..i...
//! 0x00011b20 | 04 00 00 00 04 00 00 00 17 01 01 00 6b 00 01 00 | ............k...
//! 0x00011b30 | f1 00 01 00 65 78 70 6c 69 63 69 74 20 70 61 6e | ....explicit pan
//! 0x00011b40 | 69 63 00 00 3c 1f 01 00 49 00 00 00 0a 00 00 00 | ic..<...I.......
//! 0x00011b50 | 09 00 00 00 76 69 76 61 20 65 6c 20 6a 65 66 65 | ....viva el jefe
//! 0x00011b60 | 0a 54 61 73 6b 20 23 20 50 61 6e 69 63 21 0a 00 | .Task # Panic!..
//! 0x00011b70 | 61 1b 01 00 06 00 00 00 67 1b 01 00 08 00 00 00 | a.......g.......
//! 0x00011b80 | 20 42 61 64 20 53 79 73 63 61 6c 6c 20 55 73 61 |  Bad Syscall Usa
//! 0x00011b90 | 67 65 20 0a 61 1b 01 00 06 00 00 00 80 1b 01 00 | ge .a...........
//! 0x00011ba0 | 13 00 00 00 93 1b 01 00 01 00 00 00 20 53 74 61 | ............ Sta
//! 0x00011bb0 | 63 6b 20 6f 76 65 72 66 6c 6f 77 20 61 74 20 61 | ck overflow at a
//! 0x00011bc0 | 64 64 72 65 73 73 20 30 78 00 00 00 61 1b 01 00 | ddress 0x...a...
//! 0x00011bd0 | 06 00 00 00 ac 1b 01 00 1d 00 00 00 93 1b 01 00 | ................
//! 0x00011be0 | 01 00 00 00 20 4d 65 6d 6f 72 79 20 66 61 75 6c | .... Memory faul
//! 0x00011bf0 | 74 20 61 74 20 61 64 64 72 65 73 73 20 30 78 00 | t at address 0x.
//! ```
//!
//! If an argument is present, it is the number of bytes to read:
//!
//! ```console
//! $ humility readmem 0x00011d00 100
//! humility: attached via DAPLink
//! humility: reading at 0x11d00 for 100 bytes
//!              \/  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f
//! 0x00011d00 | 20 62 6f 75 6e 64 73 3a 20 74 68 65 20 6c 65 6e |  bounds: the len
//! 0x00011d10 | 20 69 73 20 20 62 75 74 20 74 68 65 20 69 6e 64 |  is  but the ind
//! 0x00011d20 | 65 78 20 69 73 20 30 30 30 31 30 32 30 33 30 34 | ex is 0001020304
//! 0x00011d30 | 30 35 30 36 30 37 30 38 30 39 31 30 31 31 31 32 | 0506070809101112
//! 0x00011d40 | 31 33 31 34 31 35 31 36 31 37 31 38 31 39 32 30 | 1314151617181920
//! 0x00011d50 | 32 31 32 32 32 33 32 34 32 35 32 36 32 37 32 38 | 2122232425262728
//! 0x00011d60 | 32 39 33 30                                     | 2930
//! ```
//!
//! Both arguments can be in either hex, decimal, octal or binary (addresses
//! and contents will always be printed in hex):
//!
//! ```console
//! humility readmem 0o216401 0b110
//! humility: attached via DAPLink
//! humility: reading at 0x11d01 for 6 bytes
//!               0 \/  2  3  4  5  6  7  8  9  a  b  c  d  e  f
//! 0x00011d00 |    62 6f 75 6e 64 73                            |  bounds
//! ```
//!
//! The length argument can have an optional size suffix.  Note that "k" is
//! used to to denote the SI kilobytes (that is, 1000 bytes); if one wishes to
//! have a multiples of 1024 bytes (a kibibyte), "KiB" should be used instead.
//!
//! To display as half-words (16-bits) use `-h`; to display as words (32-bits)
//! use `-w`.  (The addresses must be 2-byte and 4-byte aligned, respectively.)
//!
//! ```console
//! $ humility readmem -w 0x20000000 0x40
//! humility: attached via DAPLink
//! humility: reading at 0x20000000 for 64 bytes
//!                    \/        4        8        c
//! 0x20000000 | 00000001 20000180 0000000b 00005020 | ....... .... P..
//! 0x20000010 | 00000002 200001f0 00838042 00000000 | ....... B.......
//! 0x20000020 | 00004db8 00004dc8 00004d28 00004d28 | .M...M..(M..(M..
//! 0x20000030 | 00004d28 00004d28 00004d28 00004d28 | (M..(M..(M..(M..
//! ```
//!
//! A frequent use of `readmem` is to read peripheral memory; as a
//! convenience, a peripheral name can be used in lieu of an address, provided
//! that an archive or dump is also specified:
//!
//! ```console
//! $ humility -a ~/hubris/target/gemini-bu/dist/build-gemini-bu.zip readmem -w i2c4
//! 0x30
//! humility: attached via ST-Link
//!                    \/        4        8        c
//! 0x58001c00 | 000000f7 00020490 00008000 00008000 | ................
//! 0x58001c10 | 10c0ecff 00000000 00000021 00000000 | ........!.......
//! 0x58001c20 | 00000000 00000080 00000000 00000000 | ................
//! ```
//!
//! **Note that reading some peripheral memory may have side effects!**
//!
//! It can also be useful to interpret memory contents symbolically; to do this,
//! provide a dump or achive and specify the `-s` option, e.g.:
//!
//! ```console
//! $ humility  -a ~/hubris/target/gemini-bu-rot/dist/build-gemini-bu-rot.zip readmem -s 0x20004b30 0x40
//! humility: attached via DAPLink
//! 0x20004b30 | 0x20004bdc
//! 0x20004b34 | 0x0000000a <- __EXCEPTIONS+0x2
//! 0x20004b38 | 0x80000000
//! 0x20004b3c | 0x00002e2e <- syscall_entry+0xcf6
//! 0x20004b40 | 0x00000000
//! 0x20004b44 | 0x0003c2e3 <- spi:main+0x5b
//! 0x20004b48 | 0x0003ceda <- spi:sys_send_stub+0xe
//! 0x20004b4c | 0x01000000
//! 0x20004b50 | 0x00000000
//! 0x20004b54 | 0x00000000
//! 0x20004b58 | 0x00000000
//! 0x20004b5c | 0x00000000
//! 0x20004b60 | 0x00000000
//! 0x20004b64 | 0x00000000
//! 0x20004b68 | 0x00000000
//! 0x20004b6c | 0x00000000
//! ```
//!

use anyhow::{bail, Result};
use clap::{CommandFactory, Parser};
use humility::cli::Subcommand;
use humility::hubris::*;
use humility_cmd::{Archive, Attach, Command, CommandKind, Dumper, Validate};
use std::convert::TryInto;

//
// We allow the size to be specified as a number (e.g., with an optional `0x`
// prefix) or with a suffix (e.g. `KiB`) but not both -- with apologies for
// those who yearn to express themselves in terms of octal multiples of
// kibibytes!
//
fn parse_size<T: AsRef<[u8]>>(src: T) -> Result<u64, parse_size::Error> {
    if let Ok(s) = std::str::from_utf8(src.as_ref()) {
        if let Ok(rval) = parse_int::parse::<u64>(s) {
            return Ok(rval);
        }
    }

    let cfg = parse_size::Config::new();
    cfg.parse_size(src)
}

#[derive(Parser, Debug)]
#[clap(name = "readmem", about = env!("CARGO_PKG_DESCRIPTION"))]
struct ReadmemArgs {
    /// print out as halfwords instead of as bytes
    #[clap(long, short = 'H', conflicts_with_all = &["word", "symbol"])]
    halfword: bool,

    /// print out as words instead of as bytes
    #[clap(long, short, conflicts_with_all = &["symbol"])]
    word: bool,

    /// print out as symbols
    #[clap(long, short)]
    symbol: bool,

    /// address to read
    address: String,

    /// length to read
    #[clap(parse(try_from_str = parse_size))]
    length: Option<u64>,
}

fn readmem(context: &mut humility::ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let hubris = context.archive.as_ref().unwrap();

    let subargs = ReadmemArgs::try_parse_from(subargs)?;
    let max = humility::core::CORE_MAX_READSIZE;
    let size = if subargs.word || subargs.symbol {
        4
    } else if subargs.halfword {
        2
    } else {
        1
    };

    let length = subargs.length.unwrap_or(256) as usize;

    if length & (size - 1) != 0 {
        bail!("length must be {}-byte aligned", size);
    }

    if subargs.symbol {
        hubris.validate(core, HubrisValidate::ArchiveMatch)?;
    }

    let addr = match parse_int::parse::<u32>(&subargs.address) {
        Ok(addr) => addr,
        _ => {
            hubris.validate(core, HubrisValidate::ArchiveMatch)?;
            hubris.lookup_peripheral(&subargs.address)?
        }
    };

    if addr & (size - 1) as u32 != 0 {
        bail!("address must be {}-byte aligned", size);
    }

    if length > max {
        bail!("cannot read more than {} bytes", max);
    }

    let mut bytes = vec![0u8; length];

    core.read_8(addr, &mut bytes)?;

    if subargs.symbol {
        for offs in (0..length).step_by(size) {
            let slice = &bytes[offs..offs + size];
            let val = u32::from_le_bytes(slice.try_into().unwrap());
            println!(
                "0x{:08x} | 0x{:08x}{}",
                addr + offs as u32,
                val,
                if let Some(sval) = hubris.instr_sym(val) {
                    format!(
                        " <- {}{}+0x{:x}",
                        match hubris.instr_mod(val) {
                            Some(module) if module != "kernel" => {
                                format!("{}:", module)
                            }
                            _ => "".to_string(),
                        },
                        sval.0,
                        val - sval.1
                    )
                } else {
                    "".to_string()
                }
            );
        }

        return Ok(());
    }

    let mut dumper = Dumper::new();
    dumper.size = size;
    dumper.dump(&bytes, addr);

    Ok(())
}

pub fn init() -> Command {
    Command {
        app: ReadmemArgs::command(),
        name: "readmem",
        run: readmem,
        kind: CommandKind::Attached {
            archive: Archive::Optional,
            attach: Attach::Any,
            validate: Validate::None,
        },
    }
}
