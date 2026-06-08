// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

// Re-export `slog` macros and types
pub use slog::{Logger, debug, error, info, trace, warn};

/// Drain for [`slog`]
struct HumilityDrain {
    verbose: bool,
}

impl slog::Drain for HumilityDrain {
    type Ok = ();
    type Err = std::io::Error;

    fn log(
        &self,
        record: &slog::Record,
        _values: &slog::OwnedKVList,
    ) -> Result<Self::Ok, Self::Err> {
        use colored::Colorize;
        use std::io::Write;

        // Suppress debug and trace messages unless `verbose` is set.
        if matches!(record.level(), slog::Level::Trace | slog::Level::Debug)
            && !self.verbose
        {
            return Ok(());
        }

        let stderr = std::io::stderr();
        let mut stderr = stderr.lock();

        match record.level() {
            slog::Level::Critical | slog::Level::Error => {
                write!(stderr, "humility: {}: ", "ERROR".red())?;
                writeln!(stderr, "{}", record.msg())?;
            }
            slog::Level::Warning => {
                write!(stderr, "humility: {}: ", "WARNING".red())?;
                writeln!(stderr, "{}", record.msg())?;
            }
            slog::Level::Info | slog::Level::Debug | slog::Level::Trace => {
                writeln!(stderr, "humility: {}", record.msg())?;
            }
        }

        Ok(())
    }
}

/// Initializes a standard Humility logger
pub fn init(verbose: bool) -> slog::Logger {
    use slog::Drain;
    let drain = std::sync::Mutex::new(HumilityDrain { verbose }).fuse();
    slog::Logger::root(drain, slog::o!())
}
