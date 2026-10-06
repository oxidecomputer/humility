// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility hash`
//!
//! This is deprecated

use anyhow::{Result, bail};
use clap::{ArgGroup, Parser};

use humility_cli::{ExecutionContext, humility_cmd};

#[derive(Parser, Debug)]
#[clap(
    name = "hash", about = env!("CARGO_PKG_DESCRIPTION"),
    group = ArgGroup::new("command").multiple(false),
    group = ArgGroup::new("data").multiple(false)
)]
pub struct HashArgs {
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
        value_parser = parse_int::parse::<u64>,
    )]
    timeout: u64,

    /// enable long test
    #[clap(long, short)]
    long: bool,
}

fn hash(_subargs: HashArgs, _context: &mut ExecutionContext) -> Result<()> {
    bail!(
        "This function is deprecated due to removal of the self-contained
            hash task"
    )
}

humility_cmd!(HashArgs, hash);
