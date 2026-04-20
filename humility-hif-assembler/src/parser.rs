// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! Parser for the HIF text language.
//!
//! The parser operates on the source text before any archive-dependent
//! resolution.  It produces a [`ParsedProgram`] containing
//! [`Statement`] values that the assembler then lowers into HIF ops.
//!
//! ## Syntax Summary
//!
//! ```text
//! # Comment (to end of line)
//!
//! .let NAME value          # define a constant
//!
//! i2c_read  <bus> <addr> [mux=<m>.<s>] [reg=<r>] <nbytes>
//! i2c_write <bus> <addr> [mux=<m>.<s>] [reg=<r>] <byte>[,<byte>...]
//! i2c_scan  <bus> [mux=<m>.<s>]
//! i2c_regscan <bus> <addr> [mux=<m>.<s>]
//!
//! idol <Interface.Operation> [arg=val ...]
//!
//! sleep <N>ms
//!
//! repeat <N> [sleep=<N>ms]
//!     <statements...>
//! end
//!
//! raw {
//!     <raw HIF instructions; $CONSTANTS are expanded>
//! }
//! ```

use crate::error::{HifError, HifErrorKind};
use std::collections::HashMap;

/// A parsed but unresolved HIF program.
///
/// Constants (`.let`) are expanded during parsing; they do not appear
/// in the statement list.  The `constants` map is retained for
/// informational purposes only (e.g., listing what was defined).
#[derive(Debug, Clone)]
pub struct ParsedProgram {
    pub statements: Vec<Located<Statement>>,
    /// Constants that were defined.  Values have already been
    /// substituted into statements during parsing.
    pub constants: HashMap<String, String>,
}

/// A statement with its source line number.
#[derive(Debug, Clone)]
pub struct Located<T> {
    pub line: usize,
    pub value: T,
}

/// Mux specifier: address and segment.
#[derive(Debug, Clone)]
pub struct MuxSpec {
    pub address: u8,
    pub segment: u8,
}

/// A parsed I2C bus reference — either a named bus or explicit
/// controller.port.
#[derive(Debug, Clone)]
pub enum BusRef {
    /// A named bus from app.toml (e.g. "mid", "front").
    Named(String),
    /// Explicit controller and port name (e.g. "3.H").
    Explicit { controller: u8, port: String },
}

/// An I2C device reference — either a numeric address or a symbolic
/// device name that the assembler resolves from the TargetConfig.
#[derive(Debug, Clone, PartialEq)]
pub enum DeviceRef {
    /// Numeric I2C address (e.g. 0x48).
    Address(u8),
    /// Device name to resolve (e.g. "tmp117", "v3p3_sys").
    Named(String),
}

/// A single statement in the HIF source.
#[derive(Debug, Clone)]
pub enum Statement {
    I2cRead {
        bus: BusRef,
        address: DeviceRef,
        mux: Option<MuxSpec>,
        register: Option<u8>,
        nbytes: u8,
    },
    I2cWrite {
        bus: BusRef,
        address: DeviceRef,
        mux: Option<MuxSpec>,
        register: Option<u8>,
        data: Vec<u8>,
    },
    I2cScan {
        bus: BusRef,
        mux: Option<MuxSpec>,
    },
    I2cRegScan {
        bus: BusRef,
        address: DeviceRef,
        mux: Option<MuxSpec>,
    },
    IdolCall {
        interface: String,
        operation: String,
        args: Vec<(String, String)>,
    },
    Sleep {
        ms: u32,
    },
    Repeat {
        count: u32,
        sleep_ms: Option<u32>,
        body: Vec<Located<Statement>>,
    },
    /// Call any HIF function by name with optional numeric arguments.
    Call {
        function: String,
        args: Vec<u32>,
    },
    Raw {
        lines: Vec<String>,
    },
}

/// Parse a HIF source string into a [`ParsedProgram`].
///
/// Constants (`.let`) are expanded eagerly during parsing: any `$NAME`
/// token in subsequent lines is replaced with the constant's value
/// before the line is parsed.
pub fn parse(source: &str) -> Result<ParsedProgram, HifError> {
    let mut constants = HashMap::new();
    let lines: Vec<(usize, &str)> =
        source.lines().enumerate().map(|(i, l)| (i + 1, l)).collect();

    let statements = parse_block(&lines, &mut constants, &mut 0)?;

    Ok(ParsedProgram { statements, constants })
}

fn parse_block(
    lines: &[(usize, &str)],
    constants: &mut HashMap<String, String>,
    pos: &mut usize,
) -> Result<Vec<Located<Statement>>, HifError> {
    let mut stmts = vec![];

    while *pos < lines.len() {
        let (line_num, raw_line) = lines[*pos];
        let line = strip_comment(raw_line).trim();

        if line.is_empty() {
            *pos += 1;
            continue;
        }

        // Expand constants
        let line = expand_constants(line, constants, line_num)?;
        let line = line.trim();

        if line == "end" {
            // Caller handles this
            return Ok(stmts);
        }

        // Check for raw block: first token is "raw", line ends with "{"
        // Raw blocks skip syntactic sugar but still expand $CONSTANTS.
        let is_raw = {
            let tokens: Vec<&str> = line.split_whitespace().collect();
            tokens.first() == Some(&"raw") && line.trim_end().ends_with('{')
        };
        if is_raw {
            *pos += 1;
            let mut raw_lines = vec![];
            while *pos < lines.len() {
                let (raw_line_num, rl) = lines[*pos];
                let rl = strip_comment(rl).trim();
                if rl == "}" {
                    *pos += 1;
                    break;
                }
                // Expand constants inside raw blocks
                let rl = expand_constants(rl, constants, raw_line_num)?;
                raw_lines.push(rl.trim().to_string());
                *pos += 1;
            }
            stmts.push(Located {
                line: line_num,
                value: Statement::Raw { lines: raw_lines },
            });
            continue;
        }

        let stmt = parse_statement(line, line_num, lines, constants, pos)?;
        if let Some(s) = stmt {
            stmts.push(Located { line: line_num, value: s });
        }
        // parse_statement advances pos for repeat blocks; otherwise we
        // advance here.
    }

    Ok(stmts)
}

fn parse_statement(
    line: &str,
    line_num: usize,
    lines: &[(usize, &str)],
    constants: &mut HashMap<String, String>,
    pos: &mut usize,
) -> Result<Option<Statement>, HifError> {
    let tokens: Vec<&str> = line.split_whitespace().collect();
    if tokens.is_empty() {
        *pos += 1;
        return Ok(None);
    }

    let result = match tokens[0] {
        ".let" => {
            if tokens.len() != 3 {
                return Err(mkerr(
                    line_num,
                    HifErrorKind::Parse(".let requires NAME VALUE".into()),
                ));
            }
            constants.insert(tokens[1].to_string(), tokens[2].to_string());
            *pos += 1;
            return Ok(None);
        }
        "i2c_read" => {
            *pos += 1;
            parse_i2c_read(&tokens[1..], line_num)?
        }
        "i2c_write" => {
            *pos += 1;
            parse_i2c_write(&tokens[1..], line_num)?
        }
        "i2c_scan" => {
            *pos += 1;
            parse_i2c_scan(&tokens[1..], line_num)?
        }
        "i2c_regscan" => {
            *pos += 1;
            parse_i2c_regscan(&tokens[1..], line_num)?
        }
        "idol" => {
            *pos += 1;
            parse_idol(&tokens[1..], line_num)?
        }
        "sleep" => {
            *pos += 1;
            parse_sleep(&tokens[1..], line_num)?
        }
        "repeat" => {
            *pos += 1;
            parse_repeat(&tokens[1..], line_num, lines, constants, pos)?
        }
        "call" => {
            *pos += 1;
            parse_call(&tokens[1..], line_num)?
        }
        other => {
            return Err(mkerr(
                line_num,
                HifErrorKind::Parse(format!("unknown statement '{other}'")),
            ));
        }
    };

    Ok(Some(result))
}

fn parse_bus_ref(s: &str) -> BusRef {
    if let Some((ctrl, port)) = s.split_once('.') {
        if let Ok(c) = parse_num::<u8>(ctrl) {
            return BusRef::Explicit { controller: c, port: port.to_string() };
        }
    }
    BusRef::Named(s.to_string())
}

/// Parse a device reference: either a numeric address (0x48, 72)
/// or a symbolic name (tmp117, v3p3_sys).
fn parse_device_ref(s: &str) -> DeviceRef {
    match parse_num::<u8>(s) {
        Ok(addr) => DeviceRef::Address(addr),
        Err(_) => DeviceRef::Named(s.to_string()),
    }
}

fn parse_mux(s: &str) -> Result<MuxSpec, String> {
    let val = s
        .strip_prefix("mux=")
        .ok_or_else(|| format!("expected mux=<addr>.<seg>, got '{s}'"))?;
    let (addr, seg) = val
        .split_once('.')
        .ok_or_else(|| format!("mux requires addr.segment, got '{val}'"))?;
    Ok(MuxSpec {
        address: parse_num::<u8>(addr)
            .map_err(|_| format!("bad mux address '{addr}'"))?,
        segment: parse_num::<u8>(seg)
            .map_err(|_| format!("bad mux segment '{seg}'"))?,
    })
}

fn parse_reg(s: &str) -> Result<u8, String> {
    let val = s
        .strip_prefix("reg=")
        .ok_or_else(|| format!("expected reg=<val>, got '{s}'"))?;
    parse_num::<u8>(val).map_err(|_| format!("bad register '{val}'"))
}

/// Parse `i2c_read <bus> <addr> [mux=...] [reg=...] <nbytes>`
fn parse_i2c_read(tokens: &[&str], line: usize) -> Result<Statement, HifError> {
    if tokens.len() < 3 {
        return Err(mkerr(
            line,
            HifErrorKind::Parse(
                "i2c_read requires: <bus> <addr> [mux=...] [reg=...] <nbytes>"
                    .into(),
            ),
        ));
    }

    let bus = parse_bus_ref(tokens[0]);
    let address = parse_device_ref(tokens[1]);
    let mut mux = None;
    let mut register = None;
    let mut nbytes_tok = None;

    for &tok in &tokens[2..] {
        if tok.starts_with("mux=") {
            mux = Some(
                parse_mux(tok)
                    .map_err(|e| mkerr(line, HifErrorKind::Parse(e)))?,
            );
        } else if tok.starts_with("reg=") {
            register = Some(
                parse_reg(tok)
                    .map_err(|e| mkerr(line, HifErrorKind::Parse(e)))?,
            );
        } else {
            nbytes_tok = Some(tok);
        }
    }

    let nbytes = match nbytes_tok {
        Some(t) => parse_num_err::<u8>(t, line)?,
        None => {
            return Err(mkerr(
                line,
                HifErrorKind::Parse("i2c_read missing <nbytes>".into()),
            ));
        }
    };

    Ok(Statement::I2cRead { bus, address, mux, register, nbytes })
}

/// Parse `i2c_write <bus> <addr> [mux=...] [reg=...] <byte>[,<byte>...]`
fn parse_i2c_write(
    tokens: &[&str],
    line: usize,
) -> Result<Statement, HifError> {
    if tokens.len() < 3 {
        return Err(mkerr(
            line,
            HifErrorKind::Parse(
                "i2c_write requires: <bus> <addr> [mux=...] [reg=...] <bytes>"
                    .into(),
            ),
        ));
    }

    let bus = parse_bus_ref(tokens[0]);
    let address = parse_device_ref(tokens[1]);
    let mut mux = None;
    let mut register = None;
    let mut data_tok = None;

    for &tok in &tokens[2..] {
        if tok.starts_with("mux=") {
            mux = Some(
                parse_mux(tok)
                    .map_err(|e| mkerr(line, HifErrorKind::Parse(e)))?,
            );
        } else if tok.starts_with("reg=") {
            register = Some(
                parse_reg(tok)
                    .map_err(|e| mkerr(line, HifErrorKind::Parse(e)))?,
            );
        } else {
            data_tok = Some(tok);
        }
    }

    let data = match data_tok {
        Some(t) => t
            .split(',')
            .map(|b| {
                parse_num::<u8>(b).map_err(|_| {
                    mkerr(line, HifErrorKind::InvalidNumber(b.to_string()))
                })
            })
            .collect::<Result<Vec<_>, _>>()?,
        None => {
            return Err(mkerr(
                line,
                HifErrorKind::Parse("i2c_write missing data bytes".into()),
            ));
        }
    };

    Ok(Statement::I2cWrite { bus, address, mux, register, data })
}

/// Parse `i2c_scan <bus> [mux=...]`
fn parse_i2c_scan(tokens: &[&str], line: usize) -> Result<Statement, HifError> {
    if tokens.is_empty() {
        return Err(mkerr(
            line,
            HifErrorKind::Parse("i2c_scan requires <bus>".into()),
        ));
    }
    let bus = parse_bus_ref(tokens[0]);
    let mut mux = None;
    for &tok in &tokens[1..] {
        if tok.starts_with("mux=") {
            mux = Some(
                parse_mux(tok)
                    .map_err(|e| mkerr(line, HifErrorKind::Parse(e)))?,
            );
        }
    }
    Ok(Statement::I2cScan { bus, mux })
}

/// Parse `i2c_regscan <bus> <addr> [mux=...]`
fn parse_i2c_regscan(
    tokens: &[&str],
    line: usize,
) -> Result<Statement, HifError> {
    if tokens.len() < 2 {
        return Err(mkerr(
            line,
            HifErrorKind::Parse("i2c_regscan requires <bus> <addr>".into()),
        ));
    }
    let bus = parse_bus_ref(tokens[0]);
    let address = parse_device_ref(tokens[1]);
    let mut mux = None;
    for &tok in &tokens[2..] {
        if tok.starts_with("mux=") {
            mux = Some(
                parse_mux(tok)
                    .map_err(|e| mkerr(line, HifErrorKind::Parse(e)))?,
            );
        }
    }
    Ok(Statement::I2cRegScan { bus, address, mux })
}

/// Parse `idol <Interface.Operation> [arg=val ...]`
fn parse_idol(tokens: &[&str], line: usize) -> Result<Statement, HifError> {
    if tokens.is_empty() {
        return Err(mkerr(
            line,
            HifErrorKind::Parse("idol requires <Interface.Operation>".into()),
        ));
    }
    let parts: Vec<&str> = tokens[0].split('.').collect();
    if parts.len() != 2 {
        return Err(mkerr(
            line,
            HifErrorKind::Parse(format!(
                "idol call must be Interface.Operation, got '{}'",
                tokens[0]
            )),
        ));
    }
    let mut args = vec![];
    for &tok in &tokens[1..] {
        if let Some((k, v)) = tok.split_once('=') {
            args.push((k.to_string(), v.to_string()));
        } else {
            return Err(mkerr(
                line,
                HifErrorKind::Parse(format!(
                    "idol arguments must be key=value, got '{tok}'"
                )),
            ));
        }
    }
    Ok(Statement::IdolCall {
        interface: parts[0].to_string(),
        operation: parts[1].to_string(),
        args,
    })
}

/// Parse `sleep <N>ms`
fn parse_sleep(tokens: &[&str], line: usize) -> Result<Statement, HifError> {
    if tokens.is_empty() {
        return Err(mkerr(
            line,
            HifErrorKind::Parse("sleep requires <N>ms".into()),
        ));
    }
    let s = tokens[0];
    let ms_str = s.strip_suffix("ms").unwrap_or(s);
    let ms = parse_num::<u32>(ms_str)
        .map_err(|_| mkerr(line, HifErrorKind::InvalidNumber(s.to_string())))?;
    Ok(Statement::Sleep { ms })
}

/// Parse `call <function> [arg ...]`
fn parse_call(tokens: &[&str], line: usize) -> Result<Statement, HifError> {
    if tokens.is_empty() {
        return Err(mkerr(
            line,
            HifErrorKind::Parse("call requires <function> [args...]".into()),
        ));
    }
    let function = tokens[0].to_string();
    let mut args = vec![];
    for &tok in &tokens[1..] {
        let v = parse_num::<u32>(tok).map_err(|_| {
            mkerr(line, HifErrorKind::InvalidNumber(tok.to_string()))
        })?;
        args.push(v);
    }
    Ok(Statement::Call { function, args })
}

/// Parse `repeat <N> [sleep=<N>ms]` and its body up to `end`.
fn parse_repeat(
    tokens: &[&str],
    line: usize,
    lines: &[(usize, &str)],
    constants: &mut HashMap<String, String>,
    pos: &mut usize,
) -> Result<Statement, HifError> {
    if tokens.is_empty() {
        return Err(mkerr(
            line,
            HifErrorKind::Parse("repeat requires <count>".into()),
        ));
    }
    let count = parse_num::<u32>(tokens[0]).map_err(|_| {
        mkerr(line, HifErrorKind::InvalidNumber(tokens[0].to_string()))
    })?;
    let mut sleep_ms = None;
    for &tok in &tokens[1..] {
        if let Some(val) = tok.strip_prefix("sleep=") {
            let ms_str = val.strip_suffix("ms").unwrap_or(val);
            sleep_ms = Some(parse_num::<u32>(ms_str).map_err(|_| {
                mkerr(line, HifErrorKind::InvalidNumber(val.to_string()))
            })?);
        }
    }

    let body = parse_block(lines, constants, pos)?;

    // parse_block returns when it hits "end" or EOF.
    // Consume the "end" line, or error if we hit EOF.
    if *pos < lines.len() {
        let (_, end_line) = lines[*pos];
        if strip_comment(end_line).trim() == "end" {
            *pos += 1;
        } else {
            return Err(mkerr(
                line,
                HifErrorKind::UnmatchedBlock("repeat".into()),
            ));
        }
    } else {
        return Err(mkerr(
            line,
            HifErrorKind::UnmatchedBlock("repeat (missing 'end')".into()),
        ));
    }

    Ok(Statement::Repeat { count, sleep_ms, body })
}

// -- Utilities --

fn strip_comment(line: &str) -> &str {
    match line.find('#') {
        Some(i) => &line[..i],
        None => line,
    }
}

fn expand_constants(
    line: &str,
    constants: &HashMap<String, String>,
    line_num: usize,
) -> Result<String, HifError> {
    if !line.contains('$') {
        return Ok(line.to_string());
    }
    let mut result = line.to_string();
    // Iterate over all $NAME references.  We do a simple scan for
    // '$' followed by word characters.
    while let Some(start) = result.find('$') {
        let rest = &result[start + 1..];
        let end = rest
            .find(|c: char| !c.is_alphanumeric() && c != '_')
            .unwrap_or(rest.len());
        let name = &rest[..end];
        if name.is_empty() {
            break;
        }
        match constants.get(name) {
            Some(val) => {
                result = format!(
                    "{}{}{}",
                    &result[..start],
                    val,
                    &result[start + 1 + end..]
                );
            }
            None => {
                // Column is 1-based; start is 0-based offset in the line
                return Err(mkerr_col(
                    line_num,
                    start + 1,
                    HifErrorKind::UndefinedConstant(name.to_string()),
                ));
            }
        }
    }
    Ok(result)
}

fn mkerr(line: usize, kind: HifErrorKind) -> HifError {
    HifError { line, col: None, kind }
}

fn mkerr_col(line: usize, col: usize, kind: HifErrorKind) -> HifError {
    HifError { line, col: Some(col), kind }
}

/// Parse a numeric literal, accepting `0x` hex, `0b` binary, `0o` octal,
/// and plain decimal.
///
/// Returns `Err(())` on failure; callers wrap this with context via
/// `parse_num_err`.
#[allow(clippy::result_unit_err)]
pub fn parse_num<T: TryFrom<u64>>(s: &str) -> Result<T, ()> {
    let val = if let Some(hex) =
        s.strip_prefix("0x").or_else(|| s.strip_prefix("0X"))
    {
        u64::from_str_radix(hex, 16).map_err(|_| ())?
    } else if let Some(bin) =
        s.strip_prefix("0b").or_else(|| s.strip_prefix("0B"))
    {
        u64::from_str_radix(bin, 2).map_err(|_| ())?
    } else if let Some(oct) =
        s.strip_prefix("0o").or_else(|| s.strip_prefix("0O"))
    {
        u64::from_str_radix(oct, 8).map_err(|_| ())?
    } else {
        s.parse::<u64>().map_err(|_| ())?
    };
    T::try_from(val).map_err(|_| ())
}

fn parse_num_err<T: TryFrom<u64>>(s: &str, line: usize) -> Result<T, HifError> {
    parse_num::<T>(s)
        .map_err(|_| mkerr(line, HifErrorKind::InvalidNumber(s.to_string())))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_simple_i2c_read() {
        let prog = parse("i2c_read mid 0x48 reg=0x00 2").unwrap();
        assert_eq!(prog.statements.len(), 1);
        match &prog.statements[0].value {
            Statement::I2cRead { bus, address, register, nbytes, .. } => {
                assert!(matches!(bus, BusRef::Named(n) if n == "mid"));
                assert_eq!(*address, DeviceRef::Address(0x48));
                assert_eq!(*register, Some(0x00));
                assert_eq!(*nbytes, 2);
            }
            other => panic!("expected I2cRead, got {other:?}"),
        }
    }

    #[test]
    fn parse_i2c_read_with_mux() {
        let prog = parse("i2c_read front 0x50 mux=0x70.1 reg=0x00 16").unwrap();
        match &prog.statements[0].value {
            Statement::I2cRead { mux, .. } => {
                let mux = mux.as_ref().unwrap();
                assert_eq!(mux.address, 0x70);
                assert_eq!(mux.segment, 1);
            }
            other => panic!("expected I2cRead, got {other:?}"),
        }
    }

    #[test]
    fn parse_explicit_bus() {
        let prog = parse("i2c_read 3.H 0x48 2").unwrap();
        match &prog.statements[0].value {
            Statement::I2cRead { bus, .. } => {
                assert!(matches!(
                    bus,
                    BusRef::Explicit { controller: 3, port }
                    if port == "H"
                ));
            }
            other => panic!("expected I2cRead, got {other:?}"),
        }
    }

    #[test]
    fn parse_repeat_block() {
        let src = "repeat 10\n  i2c_read mid 0x48 2\nend\n";
        let prog = parse(src).unwrap();
        assert_eq!(prog.statements.len(), 1);
        match &prog.statements[0].value {
            Statement::Repeat { count, body, .. } => {
                assert_eq!(*count, 10);
                assert_eq!(body.len(), 1);
            }
            other => panic!("expected Repeat, got {other:?}"),
        }
    }

    #[test]
    fn parse_constants() {
        let src = ".let ADDR 0x48\ni2c_read mid $ADDR 2\n";
        let prog = parse(src).unwrap();
        assert_eq!(prog.statements.len(), 1);
        match &prog.statements[0].value {
            Statement::I2cRead { address, .. } => {
                assert_eq!(*address, DeviceRef::Address(0x48));
            }
            other => panic!("expected I2cRead, got {other:?}"),
        }
    }

    #[test]
    fn parse_idol_call() {
        let prog = parse("idol Sensor.get id=3").unwrap();
        match &prog.statements[0].value {
            Statement::IdolCall { interface, operation, args } => {
                assert_eq!(interface, "Sensor");
                assert_eq!(operation, "get");
                assert_eq!(args.len(), 1);
                assert_eq!(args[0], ("id".to_string(), "3".to_string()));
            }
            other => panic!("expected IdolCall, got {other:?}"),
        }
    }

    #[test]
    fn parse_sleep() {
        let prog = parse("sleep 50ms").unwrap();
        match &prog.statements[0].value {
            Statement::Sleep { ms } => assert_eq!(*ms, 50),
            other => panic!("expected Sleep, got {other:?}"),
        }
    }

    #[test]
    fn parse_comment_and_blank() {
        let src = "# this is a comment\n\ni2c_read mid 0x48 2\n";
        let prog = parse(src).unwrap();
        assert_eq!(prog.statements.len(), 1);
    }

    #[test]
    fn undefined_constant_is_error() {
        let result = parse("i2c_read mid $MISSING 2");
        assert!(result.is_err());
    }

    #[test]
    fn parse_nested_repeat() {
        let src = "\
            repeat 10\n\
              repeat 5\n\
                i2c_read mid 0x48 2\n\
              end\n\
            end\n";
        let prog = parse(src).unwrap();
        assert_eq!(prog.statements.len(), 1);
        match &prog.statements[0].value {
            Statement::Repeat { body, .. } => {
                assert_eq!(body.len(), 1);
                assert!(matches!(&body[0].value, Statement::Repeat { .. }));
            }
            other => panic!("expected Repeat, got {other:?}"),
        }
    }

    #[test]
    fn parse_raw_block() {
        let src = "raw {\n  push 0x48\n  push_none\n  done\n}\n";
        let prog = parse(src).unwrap();
        assert_eq!(prog.statements.len(), 1);
        match &prog.statements[0].value {
            Statement::Raw { lines } => {
                assert_eq!(lines.len(), 3);
            }
            other => panic!("expected Raw, got {other:?}"),
        }
    }

    #[test]
    fn parse_raw_block_expands_constants() {
        let src = ".let ADDR 0x48\nraw {\n  push $ADDR\n  done\n}\n";
        let prog = parse(src).unwrap();
        match &prog.statements[0].value {
            Statement::Raw { lines } => {
                assert_eq!(lines[0], "push 0x48");
            }
            other => panic!("expected Raw, got {other:?}"),
        }
    }

    #[test]
    fn parse_raw_block_undefined_constant_is_error() {
        let src = "raw {\n  push $MISSING\n}\n";
        let result = parse(src);
        assert!(result.is_err());
    }

    #[test]
    fn parse_raw_block_extra_spaces() {
        // "raw  {" with extra spaces should also work
        let src = "raw  {\n  push 1\n}\n";
        let prog = parse(src).unwrap();
        assert_eq!(prog.statements.len(), 1);
    }

    #[test]
    fn parse_i2c_write() {
        let prog = parse("i2c_write mid 0x48 reg=0x01 0x00,0x80").unwrap();
        match &prog.statements[0].value {
            Statement::I2cWrite { data, register, .. } => {
                assert_eq!(*register, Some(0x01));
                assert_eq!(data, &[0x00, 0x80]);
            }
            other => panic!("expected I2cWrite, got {other:?}"),
        }
    }

    #[test]
    fn parse_missing_end_is_error() {
        let src = "repeat 10\n  i2c_read mid 0x48 2\n";
        let result = parse(src);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(
            matches!(&err.kind, HifErrorKind::UnmatchedBlock(_)),
            "expected UnmatchedBlock, got {err:?}"
        );
    }

    #[test]
    fn parse_sleep_without_suffix() {
        // sleep without "ms" should also parse (bare number = ms)
        let prog = parse("sleep 50").unwrap();
        match &prog.statements[0].value {
            Statement::Sleep { ms } => assert_eq!(*ms, 50),
            other => panic!("expected Sleep, got {other:?}"),
        }
    }

    #[test]
    fn parse_repeat_with_sleep() {
        let src = "repeat 5 sleep=10ms\n  i2c_read mid 0x48 2\nend\n";
        let prog = parse(src).unwrap();
        match &prog.statements[0].value {
            Statement::Repeat { sleep_ms, .. } => {
                assert_eq!(*sleep_ms, Some(10));
            }
            other => panic!("expected Repeat, got {other:?}"),
        }
    }

    #[test]
    fn parse_symbolic_device_name() {
        let prog = parse("i2c_read mid tmp117 reg=0x00 2").unwrap();
        match &prog.statements[0].value {
            Statement::I2cRead { address, .. } => {
                assert_eq!(*address, DeviceRef::Named("tmp117".into()));
            }
            other => panic!("expected I2cRead, got {other:?}"),
        }
    }

    #[test]
    fn parse_numeric_address_still_works() {
        let prog = parse("i2c_read mid 0x48 reg=0x00 2").unwrap();
        match &prog.statements[0].value {
            Statement::I2cRead { address, .. } => {
                assert_eq!(*address, DeviceRef::Address(0x48));
            }
            other => panic!("expected I2cRead, got {other:?}"),
        }
    }

    #[test]
    fn parse_call_no_args() {
        let prog = parse("call QspiReadId").unwrap();
        match &prog.statements[0].value {
            Statement::Call { function, args } => {
                assert_eq!(function, "QspiReadId");
                assert!(args.is_empty());
            }
            other => panic!("expected Call, got {other:?}"),
        }
    }

    #[test]
    fn parse_call_with_args() {
        let prog = parse("call SpiRead 0x10 0x20 256").unwrap();
        match &prog.statements[0].value {
            Statement::Call { function, args } => {
                assert_eq!(function, "SpiRead");
                assert_eq!(args, &[0x10, 0x20, 256]);
            }
            other => panic!("expected Call, got {other:?}"),
        }
    }

    #[test]
    fn parse_call_missing_function() {
        let result = parse("call");
        assert!(result.is_err());
    }
}
