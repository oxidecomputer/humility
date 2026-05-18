// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

// Our `build.rs` creates `cmds.rs`, which looks at our workspace to assemble
// the commands, and creates an `enum Subcommand` with all subcommands
include!(concat!(env!("OUT_DIR"), "/cmds.rs"));
