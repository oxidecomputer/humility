// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::{bail, Result};
use indexmap::IndexMap;
use serde::Deserialize;
use std::{fs, path::PathBuf};

#[derive(Deserialize, Clone, Debug)]
#[serde(deny_unknown_fields)]
pub struct Environment {
    pub probe: String,
    pub archive: String,
    pub description: Option<String>,
    pub cmds: Option<serde_json::Value>,
}

impl Environment {
    fn read(filename: &str) -> Result<IndexMap<String, Environment>> {
        let path = PathBuf::from(filename);
        let input = fs::read_to_string(&path)?;
        Ok(serde_json::from_str(&input)?)
    }

    pub fn validate(filename: &str) -> Result<()> {
        Self::read(filename)?;
        Ok(())
    }

    pub fn targets(filename: &str) -> Result<Vec<(String, Option<String>)>> {
        let env = Self::read(filename)?;
        let mut rval = vec![];

        for (target, e) in &env {
            rval.push((target.clone(), e.description.clone()))
        }

        Ok(rval)
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
