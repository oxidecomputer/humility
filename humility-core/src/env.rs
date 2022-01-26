// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::{anyhow, bail, Result};
use indexmap::IndexMap;
use serde::Deserialize;
use serde_json::Value;
use std::{fs, path::PathBuf};

#[derive(Deserialize, Clone, Debug)]
#[serde(deny_unknown_fields)]
pub struct Environment {
    pub probe: String,
    pub archive: serde_json::Value,
    pub description: Option<String>,
    pub cmds: Option<serde_json::Value>,
}

impl Environment {
    pub fn archive(&self, archive_name: &Option<String>) -> Result<String> {
        match &self.archive {
            Value::String(s) => {
                if archive_name.is_some() {
                    bail!("an archive name was specified but there is only one archive");
                }
                Ok(s.to_string())
            }
            Value::Object(obj) => {
                if let Some(ref n) = archive_name {
                    match obj
                        .get(n)
                        .ok_or_else(|| anyhow!("Couldn't find archive {}", n))?
                    {
                        Value::String(s) => Ok(s.to_string()),
                        _ => bail!("Badly formed archive entry"),
                    }
                } else {
                    bail!("Need an archive name");
                }
            }
            _ => bail!("Badly formed archive entry"),
        }
    }

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

#[test]
fn validate_single_archive() {
    let data = r#"
    {
        "board1": {
            "description": "a board",
            "probe" : "1234:5678:ABCDEFG",
            "archive" : "/some/valid/path"
        }
    }
    "#;

    let v: IndexMap<String, Environment> = serde_json::from_str(&data).unwrap();

    let _b = v.get("board1").unwrap().archive(&None);
}

#[test]
fn validate_multi_archive() {
    let data = r#"
    {
        "board1": {
            "description": "a board",
            "probe" : "1234:5678:ABCDEFG",
            "archive" : {
                "name1" : "/some/valid/path",
                "name2" : "/some/other/path"
            }
        }
    }
    "#;

    let v: IndexMap<String, Environment> = serde_json::from_str(&data).unwrap();

    let _b = v.get("board1").unwrap().archive(&Some("name1".to_string()));
    let _b = v.get("board1").unwrap().archive(&Some("name2".to_string()));
}
