// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! Error types for the HIF assembler.

use std::fmt;

/// An error discovered during parsing or assembly.
#[derive(Debug, Clone)]
pub struct HifError {
    pub line: usize,
    /// Column (1-based character offset), if known.
    pub col: Option<usize>,
    pub kind: HifErrorKind,
}

#[derive(Debug, Clone)]
pub enum HifErrorKind {
    /// Syntax error in the source text.
    Parse(String),
    /// Reference to an unknown I2C bus name.
    UnknownBus(String),
    /// Reference to an unknown HIF function.
    UnknownFunction(String),
    /// Reference to an unknown Idol interface or operation.
    UnknownIdolOp { interface: String, operation: String },
    /// Argument type or count mismatch for an Idol call.
    IdolArgError(String),
    /// Program text exceeds HIFFY_TEXT buffer.
    TextOverflow { program_bytes: usize, limit: usize },
    /// Estimated results exceed HIFFY_RSTACK buffer.
    RstackOverflow { estimated_bytes: usize, limit: usize },
    /// Too many nested loops (max 4 labels).
    LabelOverflow { used: usize, max: usize },
    /// Unmatched `repeat`/`end` block.
    UnmatchedBlock(String),
    /// Undefined constant reference.
    UndefinedConstant(String),
    /// Invalid numeric literal.
    InvalidNumber(String),
    /// A `.let` name shadows a built-in or prior definition.
    ShadowedName(String),
}

impl fmt::Display for HifError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.line == 0 {
            // Non-source errors (limit checks, serialization failures)
            write!(f, "{}", self.kind)
        } else {
            match self.col {
                Some(col) => {
                    write!(f, "line {}:{}: {}", self.line, col, self.kind)
                }
                None => write!(f, "line {}: {}", self.line, self.kind),
            }
        }
    }
}

impl fmt::Display for HifErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Parse(msg) => write!(f, "parse error: {msg}"),
            Self::UnknownBus(name) => {
                write!(f, "unknown I2C bus '{name}'")
            }
            Self::UnknownFunction(name) => {
                write!(f, "unknown HIF function '{name}'")
            }
            Self::UnknownIdolOp { interface, operation } => {
                write!(f, "unknown Idol operation '{interface}.{operation}'")
            }
            Self::IdolArgError(msg) => {
                write!(f, "Idol argument error: {msg}")
            }
            Self::TextOverflow { program_bytes, limit } => {
                write!(
                    f,
                    "program is {program_bytes} bytes, \
                     exceeds HIFFY_TEXT limit of {limit} bytes"
                )
            }
            Self::RstackOverflow { estimated_bytes, limit } => {
                write!(
                    f,
                    "estimated result size is {estimated_bytes} bytes, \
                     exceeds HIFFY_RSTACK limit of {limit} bytes"
                )
            }
            Self::LabelOverflow { used, max } => {
                write!(
                    f,
                    "program uses {used} labels, exceeds maximum of {max}"
                )
            }
            Self::UnmatchedBlock(tok) => {
                write!(f, "unmatched '{tok}'")
            }
            Self::UndefinedConstant(name) => {
                write!(f, "undefined constant '${name}'")
            }
            Self::InvalidNumber(s) => {
                write!(f, "invalid number '{s}'")
            }
            Self::ShadowedName(name) => {
                write!(f, "'{name}' shadows a built-in or prior definition")
            }
        }
    }
}

impl std::error::Error for HifError {}
