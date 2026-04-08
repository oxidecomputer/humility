// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! Assembled HIF program bundle: metadata + binary bytecode.
//!
//! A bundle is the output of assembly.  It contains:
//!
//! - [`BundleMetadata`] — JSON-serializable header with image ID, board
//!   name, sizes, and provenance.
//! - `text` — postcard-serialized HIF ops, ready for `HIFFY_TEXT`.
//! - `data` — optional payload for `HIFFY_DATA` (used by bulk writes).
//!
//! The bundle can be written to a file (`.hifb` by convention) and
//! loaded later for execution.  The file format is a JSON metadata
//! line followed by a newline, then the raw binary text and data
//! sections.

use anyhow::{bail, Context, Result};
use hif::FunctionResult;
use postcard::take_from_bytes;
use serde::{Deserialize, Serialize};
use std::fmt;
use std::io::Write;
use std::path::Path;

/// Metadata embedded in every assembled bundle.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BundleMetadata {
    /// Bundle format version.
    pub version: u32,
    /// Image ID from the Hubris archive used for assembly.
    /// The runner must check this against the target before uploading.
    pub image_id: Vec<u8>,
    /// Board name from the archive (e.g. "cosmo-b-dev").
    pub board: String,
    /// Assembled program size in bytes (length of `text`).
    pub text_size: usize,
    /// Data section size in bytes (length of `data`).
    pub data_size: usize,
    /// Maximum HIFFY_TEXT on this target.
    pub target_text_size: usize,
    /// Maximum HIFFY_RSTACK on this target.
    pub target_rstack_size: usize,
    /// HIF functions referenced by this program.
    pub functions_used: Vec<String>,
    /// Source file or description, if known.
    pub source: Option<String>,
    /// Estimated number of results the program will produce.
    pub estimated_results: Option<usize>,
    /// Original source text, for traceability.
    pub source_text: Option<String>,
}

/// A assembled HIF program ready for upload and execution.
#[derive(Debug, Clone)]
pub struct HifBundle {
    pub metadata: BundleMetadata,
    /// Postcard-serialized HIF ops for HIFFY_TEXT.
    pub text: Vec<u8>,
    /// Optional data for HIFFY_DATA.
    pub data: Vec<u8>,
}

/// A single result from a HIF function call.
#[derive(Debug, Clone)]
pub enum HifResult {
    /// Function returned successfully with payload bytes.
    Success(Vec<u8>),
    /// Function returned an error code.
    Error(u32),
}

impl HifResult {
    pub fn is_ok(&self) -> bool {
        matches!(self, HifResult::Success(_))
    }

    pub fn is_err(&self) -> bool {
        matches!(self, HifResult::Error(_))
    }

    /// Get the payload bytes if successful.
    pub fn payload(&self) -> Option<&[u8]> {
        match self {
            HifResult::Success(data) => Some(data),
            HifResult::Error(_) => None,
        }
    }

    /// Get the error code if failed.
    pub fn error_code(&self) -> Option<u32> {
        match self {
            HifResult::Success(_) => None,
            HifResult::Error(code) => Some(*code),
        }
    }
}

impl fmt::Display for HifResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            HifResult::Success(data) => {
                write!(f, "Ok({:02x?})", data)
            }
            HifResult::Error(code) => {
                write!(f, "Err({})", code)
            }
        }
    }
}

/// Current bundle file format version.
pub const BUNDLE_VERSION: u32 = 1;

impl HifBundle {
    /// Check whether the assembled program fits in the target's
    /// HIFFY_TEXT buffer.
    pub fn fits_in_target(&self) -> bool {
        self.text.len() <= self.metadata.target_text_size
    }

    /// Validate that this bundle's image ID matches the given target
    /// image ID.
    pub fn validate_image_id(&self, target_id: &[u8]) -> Result<()> {
        if self.metadata.image_id != target_id {
            bail!(
                "image ID mismatch: bundle was assembled for {:02x?}, \
                 target is {:02x?}",
                self.metadata.image_id,
                target_id,
            );
        }
        Ok(())
    }

    /// Decode results from the HIFFY_RSTACK after program execution.
    ///
    /// The `rstack` bytes are the raw contents of the `HIFFY_RSTACK`
    /// buffer read from the target after the program completes.
    /// Results are postcard-encoded `FunctionResult` values terminated
    /// by `FunctionResult::Done`.
    ///
    /// ```rust,ignore
    /// let results = HifBundle::decode_results(&rstack_bytes)?;
    /// for (i, r) in results.iter().enumerate() {
    ///     println!("[{i}] {r}");
    /// }
    /// ```
    pub fn decode_results(rstack: &[u8]) -> Result<Vec<HifResult>> {
        let mut results = vec![];
        let mut remaining = rstack;

        loop {
            if remaining.is_empty() {
                break;
            }

            let (rval, next) = take_from_bytes::<FunctionResult>(remaining)
                .map_err(|e| {
                    anyhow::anyhow!("decoding result {}: {e}", results.len())
                })?;

            match rval {
                FunctionResult::Done => break,
                FunctionResult::Success(payload) => {
                    results.push(HifResult::Success(payload.to_vec()));
                }
                FunctionResult::Failure(code) => {
                    results.push(HifResult::Error(code));
                }
            }

            remaining = next;
        }

        Ok(results)
    }

    /// Write the bundle to a file.
    ///
    /// Format: JSON metadata line, newline, then binary text section,
    /// then binary data section.
    pub fn write_to_file(&self, path: impl AsRef<Path>) -> Result<()> {
        let path = path.as_ref();
        let mut f = std::fs::File::create(path)
            .with_context(|| format!("creating {}", path.display()))?;
        let meta_json = serde_json::to_string(&self.metadata)?;
        f.write_all(meta_json.as_bytes())?;
        f.write_all(b"\n")?;
        f.write_all(&self.text)?;
        f.write_all(&self.data)?;
        Ok(())
    }

    /// Read a bundle from a file.
    pub fn read_from_file(path: impl AsRef<Path>) -> Result<Self> {
        let path = path.as_ref();
        let contents = std::fs::read(path)
            .with_context(|| format!("reading {}", path.display()))?;

        let newline_pos = contents
            .iter()
            .position(|&b| b == b'\n')
            .context("bundle file missing metadata line")?;

        let meta_json = &contents[..newline_pos];
        let metadata: BundleMetadata = serde_json::from_slice(meta_json)
            .context("parsing bundle metadata")?;

        let binary = &contents[newline_pos + 1..];
        if binary.len() < metadata.text_size {
            bail!(
                "bundle file is truncated: expected at least {} bytes \
                 after metadata, got {}",
                metadata.text_size,
                binary.len(),
            );
        }
        let text = binary[..metadata.text_size].to_vec();
        let data = binary[metadata.text_size..].to_vec();

        Ok(HifBundle { metadata, text, data })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use postcard::to_allocvec;

    fn encode_results(results: &[FunctionResult]) -> Vec<u8> {
        let mut buf = vec![];
        for r in results {
            buf.extend_from_slice(
                &to_allocvec(r).expect("encoding FunctionResult"),
            );
        }
        buf
    }

    #[test]
    fn decode_empty_rstack() {
        let rstack = encode_results(&[FunctionResult::Done]);
        let results = HifBundle::decode_results(&rstack).unwrap();
        assert!(results.is_empty());
    }

    #[test]
    fn decode_success_results() {
        let rstack = encode_results(&[
            FunctionResult::Success(&[0x1a, 0x80]),
            FunctionResult::Success(&[0x1b, 0x00]),
            FunctionResult::Done,
        ]);
        let results = HifBundle::decode_results(&rstack).unwrap();
        assert_eq!(results.len(), 2);
        assert!(results[0].is_ok());
        assert_eq!(results[0].payload().unwrap(), &[0x1a, 0x80]);
        assert_eq!(results[1].payload().unwrap(), &[0x1b, 0x00]);
    }

    #[test]
    fn decode_error_results() {
        let rstack = encode_results(&[
            FunctionResult::Failure(3), // NoDevice
            FunctionResult::Done,
        ]);
        let results = HifBundle::decode_results(&rstack).unwrap();
        assert_eq!(results.len(), 1);
        assert!(results[0].is_err());
        assert_eq!(results[0].error_code(), Some(3));
    }

    #[test]
    fn decode_mixed_results() {
        let rstack = encode_results(&[
            FunctionResult::Success(&[0x48]),
            FunctionResult::Failure(3),
            FunctionResult::Success(&[0x49]),
            FunctionResult::Done,
        ]);
        let results = HifBundle::decode_results(&rstack).unwrap();
        assert_eq!(results.len(), 3);
        assert!(results[0].is_ok());
        assert!(results[1].is_err());
        assert!(results[2].is_ok());
    }

    #[test]
    fn decode_display() {
        let ok = HifResult::Success(vec![0x1a, 0x80]);
        assert_eq!(format!("{ok}"), "Ok([1a, 80])");

        let err = HifResult::Error(3);
        assert_eq!(format!("{err}"), "Err(3)");
    }
}
