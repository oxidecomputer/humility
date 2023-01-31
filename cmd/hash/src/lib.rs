// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::{anyhow, bail, Result};
use clap::{ArgGroup, CommandFactory, Parser};

use humility::cli::Subcommand;
use humility_cmd::{hiffy::*, CommandKind};
use humility_cmd::{Archive, Attach, Command, Validate};
use sha2::{Digest, Sha256};
use std::fs::File;
use std::io::Read;

use hif::*;

use indicatif::{ProgressBar, ProgressStyle};

extern crate log;

#[derive(Parser, Debug)]
#[clap(
    name = "hash", about = env!("CARGO_PKG_DESCRIPTION"),
    group = ArgGroup::new("command").multiple(false),
    group = ArgGroup::new("data").multiple(false)
)]
struct HashArgs {
    /// Legal sequences:
    ///
    ///   ... hash --digest -s abc # which is equivalent to
    ///
    ///   ... hash --digest -x 61,62,63 # which is equivalent to
    ///
    ///   echo -n abc > abc.txt
    ///   ... hash --digest -f abc.txt
    ///
    ///   ... hash -i --update -s 'a'
    ///   ... hash --update -s 'bc'
    ///   ... hash --finalize
    ///
    ///   ... hash --init
    ///   ... hash --update -f /etc/issue
    ///   ... hash --update -f /etc/issue.net
    ///   ... hash --finalize
    ///

    /// Initialize the hash block and optionally provide a length.
    #[clap(long, short)] // not in group "command" to allow -i update
    init: bool,

    /// In one command, initialize, update, and finalize processing the specified
    /// data. hash.
    /// In the case of Humility/Hiffy as a client, the size of the bytestream
    /// is limited to the size of the scratch buffer that hiffy implements.
    /// For larger bytestreams, use a sequence of init, update, ..., update,
    /// finalize.
    #[clap(long, short, group = "command")]
    digest: bool,

    /// Update the hash with the given data.
    #[clap(long, short, group = "command")]
    update: bool,

    /// Complete the hash computation and return the result.
    #[clap(long, short, group = "command")]
    finalize: bool,

    /// Process built-in test vectors and compare to known results.
    #[clap(long, short, group = "command")]
    test: bool,

    // TODO: if there is reason to use the additional available
    // algorithms/configurations.
    // --algo {SHA-1, SHA224, SHA256, MD5, HMAC}
    // --order {big,little}
    // test --vector {1,2,3...} // run local sw and SP hardware and compare
    // HMAC
    /// Binary is read from a file. TODO: Chunk and loop for digest and update.
    #[clap(long, short = 'F', value_name = "filename", group = "data")]
    file: Option<String>,

    /// Data is provided as comma separated hex digits: e.g. -h 1,1f,c3
    #[clap(long, short = 'x', value_name = "HEX_DATA", group = "data")]
    hex: Option<String>,

    /// Data is provided as a string
    #[clap(long, short, group = "data", value_name = "STRING")]
    string: Option<String>,

    /// sets timeout
    #[clap(
        long, short = 'T', default_value_t = 5000, value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// enable long test
    #[clap(long, short)]
    long: bool,
}

fn hash(context: &mut humility::ExecutionContext) -> Result<()> {
    let core = &mut **context.core.as_mut().unwrap();
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();

    let subargs = HashArgs::try_parse_from(subargs)?;
    let archive = context.archive.as_ref().unwrap();
    let mut context = HiffyContext::new(archive, core, subargs.timeout)?;
    let funcs = context.functions()?;
    let scratch_size = context.scratch_size();
    let mut ops = vec![];

    // Fetch the supplied data if any.
    let mut data = Vec::new();
    let data = if subargs.file.is_some() {
        let filename = subargs.file.unwrap();
        let mut file = File::open(&filename)?;
        if let Err(err) = file.read_to_end(&mut data) {
            bail!("Cannot read file \"{}\": {}", filename, err);
        }
        Some(data.as_slice())
    } else if subargs.hex.is_some() {
        let hex = subargs.hex.unwrap();
        let bytes: Vec<&str> = hex.split(',').collect();
        for byte in &bytes {
            if let Ok(val) = u8::from_str_radix(byte, 16) {
                data.push(val);
            } else {
                bail!("invalid byte {}", byte)
            }
        }
        Some(data.as_slice())
    } else if subargs.string.is_some() {
        data.extend_from_slice(subargs.string.unwrap().as_bytes());
        Some(data.as_slice())
    } else {
        None
    };

    if !subargs.digest && !subargs.update && data.is_some() {
        return Err(anyhow!("data supplied and not used"));
    }
    if (subargs.digest || subargs.update) && data.is_none() {
        return Err(anyhow!("no data provided"));
    }

    // Init by itself and with --update is ok.
    // Other cases are useless or mess up calculations and are disallowed.

    let hash_cmd = if subargs.digest {
        if subargs.init {
            return Err(anyhow!("--init is not used with --digest"));
        }
        Some(funcs.get("HashDigest", 1)?)
    } else if subargs.update {
        if subargs.init {
            ops.push(Op::Call(funcs.get("HashInit", 0)?.id));
        }
        Some(funcs.get("HashUpdate", 1)?)
    } else if subargs.finalize {
        if subargs.init {
            return Err(anyhow!("--init is not used with --finalize"));
        }
        Some(funcs.get("HashFinalize", 0)?)
    } else if subargs.test {
        if subargs.init {
            return Err(anyhow!("--init is not used with --test"));
        }
        None
    } else if subargs.init {
        Some(funcs.get("HashInit", 0)?)
    } else {
        None
    };

    // Process commands other than --test
    if let Some(cmd) = hash_cmd {
        if let Some(data) = data {
            if data.len() > scratch_size {
                if !subargs.long {
                    return Err(anyhow!(
                        "Data size exceeds {}, use --long",
                        scratch_size
                    ));
                }
                // TODO: suppress progress bar with --quiet/--noverbose option
                // Execution can take a long time in some cases (30+ minutes)
                let bar = ProgressBar::new(data.len() as u64);
                bar.set_style(ProgressStyle::default_bar().template(
                    "humility: Hashing [{bar:30}] {bytes}/{total_bytes}",
                ));
                // On first iteration, --digest won't have the Init already pushed.
                if subargs.digest {
                    ops.push(Op::Call(funcs.get("HashInit", 0)?.id));
                }
                for index in (0..data.len()).step_by(scratch_size) {
                    bar.set_position(index as u64);
                    let nbytes = if index + scratch_size > data.len() {
                        data.len() - index
                    } else {
                        scratch_size
                    };
                    let buf = &data[index..index + nbytes];
                    ops.push(Op::Push32(buf.len() as u32));
                    ops.push(Op::Call(funcs.get("HashUpdate", 1)?.id));
                    ops.push(Op::Done);
                    let results =
                        context.run(core, ops.as_slice(), Some(buf))?;
                    if let Err(err) = &results[0] {
                        println!(
                            "Fail at index={}: results={:#?}",
                            index, results
                        );
                        return Err(anyhow!("update fails: {:?}", err));
                    }
                    ops.clear();
                }
                bar.finish_and_clear();
                if subargs.digest {
                    ops.push(Op::Call(funcs.get("HashFinalize", 0)?.id));
                    ops.push(Op::Done);
                    let results = context.run(core, ops.as_slice(), None)?;
                    match &results[0] {
                        Ok(buf) => {
                            if buf.len() != 32 {
                                return Err(anyhow!(
                                    "finalize returns wrong size: {:?}",
                                    buf.len()
                                ));
                            }
                            print_hash(buf.as_slice());
                        }
                        _ => {
                            return Err(anyhow!("finalize fails"));
                        }
                    }
                }
            } else {
                // For update and digest that fit in a single hubris scratch buf,
                // push the length of data to be sent.
                ops.push(Op::Push32(data.len() as u32));
            }
        }

        ops.push(Op::Call(cmd.id));
        ops.push(Op::Done);
        let results = context.run(
            core,
            ops.as_slice(),
            match data {
                Some(data) => Some(data),
                _ => None,
            },
        )?;
        println!("returned results={:?}", &results);
        match &results[0] {
            Ok(buf) => {
                print_hash(buf);
            }
            Err(err) => {
                println!("Error returned: {}", err);
            }
        }
        return Ok(());
    }

    // Any --digest, --init, --update, and --finalize commands are dealt with
    // above.
    // The --test command is the last valid command to consider.

    if !subargs.test {
        bail!("why am I here?")
    }

    let report = |name, v1: &[u8], correct: &[u8]| {
        println!(
            "{}: {}",
            name,
            if v1 == correct {
                "\x1b[32mPass\x1b[m:"
            } else {
                "\x1b[41mFail\x1b[m:"
            }
        );
        for byte in v1.iter().take(256 / 8) {
            print!("{:02x}", byte);
        }
        println!(" result");
        for byte in correct.iter().take(256 / 8) {
            print!("{:02x}", byte);
        }
        println!(" correct");
    };

    let mut datacmd = |_info, id, data: Option<&[u8]>| {
        ops.clear();
        if let Some(payload) = data {
            ops.push(Op::Push32(payload.len() as u32));
        }
        ops.push(Op::Call(id));
        ops.push(Op::Done);
        context.run(
            core,
            ops.as_slice(),
            match data {
                Some(data) => Some(data),
                _ => None,
            },
        )
    };

    let digest_id = funcs.get("HashDigest", 1)?.id;

    // These short tests all fit in a single Hubris scratch buffer.
    struct ShortTest<'a> {
        desc: &'a str,
        vector: &'a [u8],
        expected: [u8; 256 / 8],
    }

    const SHORT_TESTS: &[ShortTest] = &[
        // [NSRL Test Data | NIST](https://www.nist.gov/itl/ssd/software-quality-group/nsrl-test-data)
        // A file containing the ASCII string "abc" results in a 256-bit message
        // digest of
        // BA7816BF 8F01CFEA 414140DE 5DAE2223 B00361A3 96177A9C B410FF61 F20015AD.
        //
        ShortTest {
            desc: "NSRL Test Data 1",
            vector: b"abc".as_slice(),
            expected: [
                0xBA, 0x78, 0x16, 0xBF, 0x8F, 0x01, 0xCF, 0xEA,
                0x41, 0x41, 0x40, 0xDE, 0x5D, 0xAE, 0x22, 0x23,
                0xB0, 0x03, 0x61, 0xA3, 0x96, 0x17, 0x7A, 0x9C,
                0xB4, 0x10, 0xFF, 0x61, 0xF2, 0x00, 0x15, 0xAD
            ]
        },

        // A file containing the ASCII string
        // "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq" results in a
        // 256-bit message digest of
        // 248D6A61 D20638B8 E5C02693 0C3E6039 A33CE459 64FF2167 F6ECEDD4 19DB06C1.
        // 14 x 32-bit words (less than a block)
        ShortTest{
            desc: "NSRL Test Data 2",
            vector: b"abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq".as_slice(),
            expected: [
                0x24, 0x8D, 0x6A, 0x61, 0xD2, 0x06, 0x38, 0xB8,
                0xE5, 0xC0, 0x26, 0x93, 0x0C, 0x3E, 0x60, 0x39,
                0xA3, 0x3C, 0xE4, 0x59, 0x64, 0xFF, 0x21, 0x67,
                0xF6, 0xEC, 0xED, 0xD4, 0x19, 0xDB, 0x06, 0xC1
            ]
        },

        // An even block (16 32-bit words)
        ShortTest{
            desc: "one full block",
            vector: b"1234abcd2345bcde3456cdef4567defg5678efgh6789fghi7890ghij8901hijk".as_slice(),
            expected: [
                0xc6, 0x41, 0xd5, 0x22, 0x3a, 0x4d, 0x0e, 0xd3,
                0xc0, 0x6c, 0x6f, 0xd9, 0xe1, 0x96, 0x17, 0x25,
                0xe9, 0x54, 0xdc, 0x69, 0x99, 0x13, 0xc1, 0xa4,
                0x96, 0xd7, 0x05, 0x98, 0xc6, 0x39, 0x80, 0x7b
            ]
        },

        // One full block plus one byte (16 32-bit words + 1 byte)
        ShortTest{
            desc: "one full block+1 byte",
            vector: b"1234abcd2345bcde3456cdef4567defg5678efgh6789fghi7890ghij8901hijk!".as_slice(),
            expected: [
                0xa3, 0x6c, 0x1c, 0x9f, 0xd8, 0x9b, 0x14, 0xc5,
                0xb5, 0xd7, 0x24, 0x92, 0x8b, 0xed, 0xf0, 0x37,
                0x30, 0x25, 0x00, 0x22, 0xaa, 0x9e, 0xb3, 0x9b,
                0x6f, 0x1e, 0x19, 0x3d, 0x0b, 0x42, 0xef, 0xb9,
            ]
        }
    ];

    for test in SHORT_TESTS {
        println!("\n\nDescription: {}", test.desc);
        let mut hasher = Sha256::new();
        hasher.update(test.vector);
        let lib_sum = hasher.finalize();

        for index in 0..256 / 8 {
            print!("{:02x}", lib_sum[index]);
        }
        println!(" lib_sum");
        assert_eq!(lib_sum[..], test.expected);

        let result = datacmd(test.desc, digest_id, Some(&test.vector[0..]))?;
        match &result[0] {
            Ok(buf) => {
                report(test.desc, buf.as_slice(), &test.expected[0..]);
            }
            _ => bail!("Cannot execute: {}", test.desc),
        }
    }

    if !subargs.long {
        println!("Use --long option to enable 1Mbyte hash test.");
        return Ok(());
    }

    println!("\nLong running tests");

    // Let the message be the binary-coded form of the ASCII string which
    // consists of 1,000,000 repetitions of the character "a" results in a
    // SHA-256 message digest of
    let block = vec![b'a'; scratch_size];
    let block = block.as_slice();
    let expected_sum: [u8; 256 / 8] = [
        0xCD, 0xC7, 0x6E, 0x5C, 0x99, 0x14, 0xFB, 0x92, 0x81, 0xA1, 0xC7, 0xE2,
        0x84, 0xD7, 0x3E, 0x67, 0xF1, 0x80, 0x9A, 0x48, 0xA4, 0x97, 0x20, 0x0E,
        0x04, 0x6D, 0x39, 0xCC, 0xC7, 0x11, 0x2C, 0xD0,
    ];
    let limit = 1_000_000;

    let bar = ProgressBar::new(limit as u64);
    bar.set_style(
        ProgressStyle::default_bar()
            .template("humility: Hashing [{bar:30}] {bytes}/{total_bytes}"),
    );

    let mut hasher = Sha256::new();

    datacmd("test 3 init", funcs.get("HashInit", 0)?.id, None)?;

    let mut count = 0;
    while count < limit {
        let mut nbytes = limit - count;
        if nbytes > block.len() {
            nbytes = block.len();
        }
        hasher.update(&block[0..nbytes]);
        datacmd(
            "test 3 update",
            funcs.get("HashUpdate", 1)?.id,
            Some(&block[0..nbytes]),
        )?;
        count += nbytes;
        bar.set_position(count as u64);
    }
    bar.finish_and_clear();

    let lib_sum = hasher.finalize();
    let result =
        datacmd("test 3 finalize", funcs.get("HashFinalize", 0)?.id, None)?;
    match &result[0] {
        Ok(buf) => {
            report("Test vector 3", buf.as_slice(), &expected_sum[0..]);
            assert_eq!(lib_sum.as_slice(), buf.as_slice());
        }
        Err(err) => {
            bail!("Error executing test 3: {:#?}", err)
        }
    }

    Ok(())
}

fn print_hash(buf: &[u8]) {
    if !buf.is_empty() {
        if buf.len() != 32 {
            println!("Warning: return len != 256 bits");
        }
        for byte in buf {
            print!("{:02x}", byte);
        }
        println!();
    } else {
        println!("returned buffer is empty!");
    }
}

pub fn init() -> Command {
    Command {
        app: HashArgs::command(),
        name: "hash",
        run: hash,
        kind: CommandKind::Attached {
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
        },
    }
}
