// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

pub mod arch;
pub mod core;
pub mod hubris;
pub mod reflect;

#[macro_use]
extern crate num_derive;

/// Give messages to the user.
///
/// These macros are intended to be used whenever producing secondary output to the
/// terminal for users to see. They are their own macros for two reasons:
///
/// 1. They will prepend "humility: " to the output
/// 2. They use stderr rather than stdout
///
/// Additionally, [`warn!`] will generate an eye-grabbing warning.  These
/// macros should be used in lieu of `log::error!`, `log::warn!` or direct
/// `eprintln!` (`log::debug!` and `log::trace!` can be used for debugging
/// output that is to be optionally enabled on the command line).
#[macro_export]
macro_rules! msg {
    ($fmt:expr) => ({
        eprintln!(concat!("humility: ", $fmt));
    });
    ($fmt:expr, $($arg:tt)*) => ({
        eprintln!(concat!("humility: ", $fmt), $($arg)*);
    });
}

#[macro_export]
macro_rules! warn {
    ($fmt:expr) => ({
        use colored::Colorize;
        eprint!("humility: {}: ", "WARNING".red());
        eprintln!($fmt);
    });
    ($fmt:expr, $($arg:tt)*) => ({
        use colored::Colorize;
        eprint!("humility: {}: ", "WARNING".red());
        eprintln!($fmt, $($arg)*);
    });
}
