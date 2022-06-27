// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::{bail, Result};
use serde::Deserialize;
use std::collections::HashMap;
use std::{fs, path::PathBuf};

#[derive(Deserialize, Clone, Debug)]
#[serde(deny_unknown_fields)]
pub struct Environment {
    pub probe: String,
    pub archive: String,
    pub cmds: Option<serde_json::Value>,
}

impl Environment {
    fn read(filename: &str) -> Result<HashMap<String, Environment>> {
        let path = PathBuf::from(filename);
        let input = fs::read_to_string(&path)?;
        Ok(serde_json::from_str(&input)?)
    }

    pub fn validate(filename: &str) -> Result<()> {
        Self::read(filename)?;
        Ok(())
    }

    pub fn from_file(filename: &str, target: &str) -> Result<Self> {
        let env = Self::read(filename)?;

        if let Some(e) = env.get(target) {
            Ok(e.clone())
        } else {
            let keys = env.keys().map(|n| &**n).collect::<Vec<_>>().join(", ");
            bail!("invalid target \"{}\" (expected one of: {})", target, keys);
        }
    }
}
