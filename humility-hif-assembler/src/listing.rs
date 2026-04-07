// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! Listing commands: inspect what a Hubris archive offers for HIF
//! program assembly.
//!
//! These functions extract and format information from the archive
//! without needing a target connection.  They are useful for
//! discovering bus names, available functions, and Idol interfaces
//! before writing a program.

use crate::assembler::HifAssembler;
use crate::types::{FunctionInfo, ResolvedBus};
use std::fmt;

/// A formatted listing of I2C buses.
pub struct BusListing<'a> {
    pub buses: Vec<&'a ResolvedBus>,
}

impl fmt::Display for BusListing<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "{:<16} {:>4}  {:<6} {:<6}",
            "BUS", "CTRL", "PORT", "NAME"
        )?;
        for bus in &self.buses {
            writeln!(
                f,
                "{:<16} {:>4}  {:<6} {}",
                bus.name, bus.controller, bus.port_index, bus.port_name,
            )?;
        }
        Ok(())
    }
}

/// A formatted listing of HIF functions.
pub struct FunctionListing<'a> {
    pub functions: Vec<&'a FunctionInfo>,
}

impl fmt::Display for FunctionListing<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{:>3}  {:<24} {:>4}", "ID", "NAME", "ARGS")?;
        for func in &self.functions {
            writeln!(
                f,
                "{:>3}  {:<24} {:>4}",
                func.id, func.name, func.arg_count,
            )?;
        }
        Ok(())
    }
}

impl HifAssembler {
    /// Produce a formatted listing of I2C buses.
    pub fn bus_listing(&self) -> BusListing<'_> {
        BusListing { buses: self.list_buses() }
    }

    /// Produce a formatted listing of HIF functions.
    pub fn function_listing(&self) -> FunctionListing<'_> {
        FunctionListing { functions: self.list_functions() }
    }
}
