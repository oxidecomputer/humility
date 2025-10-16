// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! Generic cache for GPIO configuration register blocks.
//!
//! This module provides a chip-independent way to cache raw GPIO register
//! block data from device memory. The cache stores raw bytes indexed by
//! GPIO group identifiers, avoiding repeated memory reads for the same
//! GPIO port.

use anyhow::Result;
use humility_cli::ExecutionContext;
use std::collections::BTreeMap;

/// A generic cache for GPIO configuration register blocks.
///
/// This cache stores raw byte data for GPIO register blocks, indexed by
/// a group identifier (typically a character like 'A', 'B', etc.). The
/// cache is chip-independent and works with any architecture that organizes
/// GPIO pins into groups/ports with contiguous register blocks.
pub struct ConfigCache {
    cache: BTreeMap<char, Vec<u8>>,
}

impl ConfigCache {
    /// Creates a new empty configuration cache.
    pub fn new() -> ConfigCache {
        ConfigCache { cache: BTreeMap::new() }
    }

    /// Gets or fetches the raw register block data for a GPIO group.
    ///
    /// If the data for the specified group is already cached, returns it
    /// directly. Otherwise, calls the provided `fetch_fn` to read the data
    /// from device memory, caches it, and returns it.
    ///
    /// # Arguments
    ///
    /// * `context` - Execution context with access to the device core
    /// * `group` - GPIO group identifier (e.g., 'A', 'B', 'C')
    /// * `fetch_fn` - Function that fetches the register block from device memory
    ///
    /// # Returns
    ///
    /// A reference to the cached raw register block data
    pub fn get_or_fetch<F>(
        &mut self,
        context: &mut ExecutionContext,
        group: char,
        fetch_fn: F,
    ) -> Result<&[u8]>
    where
        F: FnOnce(&mut ExecutionContext, char) -> Result<Vec<u8>>,
    {
        use std::collections::btree_map::Entry;
        if let Entry::Vacant(e) = self.cache.entry(group) {
            let data = fetch_fn(context, group)?;
            e.insert(data);
        }
        Ok(self.cache.get(&group).unwrap().as_slice())
    }
}
