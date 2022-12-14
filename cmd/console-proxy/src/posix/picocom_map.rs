// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! picocom-style character remapping; does not support the "... to hex" rules.

use std::{collections::VecDeque, str::FromStr};

use anyhow::{bail, ensure, Error, Result};

mod raw {
    pub(super) const CR: u8 = b'\r';
    pub(super) const LF: u8 = b'\n';
    pub(super) const BS: u8 = 0x08;
    pub(super) const DEL: u8 = 0x7f;
}

#[derive(Debug, Clone, Copy)]
pub struct RemapRules {
    // For simplicity in our remapping iterator below, we always define mapping
    // rules for CR and LF. If the user didn't request any, we remap them to
    // themselves.
    cr: &'static [u8],
    lf: &'static [u8],
    bsdel: bool,
    delbs: bool,
}

impl Default for RemapRules {
    fn default() -> Self {
        Self { cr: &[raw::CR], lf: &[raw::LF], bsdel: false, delbs: false }
    }
}

impl FromStr for RemapRules {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut cr: Option<&'static [u8]> = None;
        let mut lf: Option<&'static [u8]> = None;
        let mut bsdel = false;
        let mut delbs = false;

        for rule in s.split(',') {
            match rule {
                "crlf" => {
                    ensure!(cr.is_none(), "multiple rules remapping cr");
                    cr = Some(&[raw::LF]);
                }
                "crcrlf" => {
                    ensure!(cr.is_none(), "multiple rules remapping cr");
                    cr = Some(&[raw::CR, raw::LF]);
                }
                "igncr" => {
                    ensure!(cr.is_none(), "multiple rules remapping cr");
                    cr = Some(&[]);
                }
                "lfcr" => {
                    ensure!(lf.is_none(), "multiple rules remapping lf");
                    lf = Some(&[raw::CR]);
                }
                "lfcrlf" => {
                    ensure!(lf.is_none(), "multiple rules remapping lf");
                    lf = Some(&[raw::CR, raw::LF]);
                }
                "ignlf" => {
                    ensure!(lf.is_none(), "multiple rules remapping lf");
                    lf = Some(&[]);
                }
                "bsdel" => {
                    bsdel = true;
                }
                "delbs" => {
                    delbs = true;
                }
                // str::split() always returns at least one element, even if
                // called on the empty string; ignore empty rules instead of
                // bailing.
                "" => (),
                _ => bail!("unknown or unsupported remap rule: {rule:?}"),
            }
        }

        Ok(Self {
            cr: cr.unwrap_or(&[raw::CR]),
            lf: lf.unwrap_or(&[raw::LF]),
            bsdel,
            delbs,
        })
    }
}

impl RemapRules {
    pub fn apply<I>(&self, bytes: I) -> RemapIter<I::IntoIter>
    where
        I: IntoIterator<Item = u8>,
    {
        RemapIter {
            inner: bytes.into_iter(),
            prev: VecDeque::new(),
            rules: *self,
        }
    }
}

pub struct RemapIter<I> {
    inner: I,
    prev: VecDeque<u8>,
    rules: RemapRules,
}

impl<I> Iterator for RemapIter<I>
where
    I: Iterator<Item = u8>,
{
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        while self.prev.is_empty() {
            match self.inner.next()? {
                raw::CR => self.prev.extend(self.rules.cr),
                raw::LF => self.prev.extend(self.rules.lf),
                raw::BS if self.rules.bsdel => self.prev.push_back(raw::DEL),
                raw::DEL if self.rules.delbs => self.prev.push_back(raw::BS),
                b => self.prev.push_back(b),
            }
        }
        self.prev.pop_front()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reject_invalid_rules() {
        // bogus rules
        for s in ["foo", "crlf,foo"] {
            assert!(
                RemapRules::from_str(s)
                    .unwrap_err()
                    .to_string()
                    .contains("unsupported remap rule"),
                "unexpected error message parsing {s:?}"
            );
        }

        // rule strings with duplicates for CR or LF
        for s in [
            "crlf,crlf",
            "crcrlf,crlf",
            "igncr,crlf",
            "lfcr,lfcr",
            "lfcr,lfcrlf",
            "lfcr,ignlf",
        ] {
            assert!(
                RemapRules::from_str(s)
                    .unwrap_err()
                    .to_string()
                    .contains("multiple rules remapping"),
                "unexpected error message parsing {s:?}"
            );
        }
    }

    #[test]
    fn remapping() {
        for (rules, input, expected) in [
            ("crlf", "foo\rbar\n", "foo\nbar\n"),
            ("crcrlf", "foo\rbar\rbaz\n", "foo\r\nbar\r\nbaz\n"),
            ("crlf,lfcr", "foo\rbar\n", "foo\nbar\r"),
            ("igncr,lfcr", "foo\rbar\n", "foobar\r"),
            ("ignlf,crcrlf", "foo\rbar\n", "foo\r\nbar"),
            ("crlf,delbs", "foo\rbar\x7fbaz", "foo\nbar\x08baz"),
            ("igncr,delbs,bsdel", "foo\rbar\x7fbaz\x08", "foobar\x08baz\x7f"),
        ] {
            let rules: RemapRules = rules.parse().unwrap();
            let output = rules
                .apply(input.as_bytes().iter().copied())
                .collect::<Vec<_>>();
            let output = std::str::from_utf8(&output).unwrap();
            assert_eq!(expected, output, "mismatch with rules {rules:?}");
        }
    }
}
