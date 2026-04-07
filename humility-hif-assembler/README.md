# humility-hif-assembler

Assembler for HIF (Hubris Interchange Format) text programs.

## Overview

This crate translates text-based HIF programs into bytecode that the
`hiffy` task on a Hubris SP or RoT can execute.  It resolves symbolic
names (bus names, function names, device addresses) against a Hubris
archive, validates programs against target buffer sizes, and produces
bundles with embedded image IDs for safe upload.

The text language provides syntactic sugar for common operations while
raw HIF instructions remain available for anything the sugar doesn't
cover.

## Quick Start (planned CLI integration)

The following commands are not yet implemented in `humility hiffy`.
They show the intended workflow once CLI integration lands.

```bash
# Assemble and verify a program against a Hubris archive
humility -a gimlet-c-dev.zip hiffy --verify stress.hif

# Assemble to a bundle file
humility -a gimlet-c-dev.zip hiffy --assemble stress.hif -o stress.hifb

# Run on a target
humility -a gimlet-c-dev.zip -t c71 hiffy --run stress.hifb
```

## Text Format

Programs are line-oriented with `#` comments and `.let` constants.

### I2C Operations

```
# Read 2 bytes from register 0x00 of device 0x48 on the "mid" bus
i2c_read mid 0x48 reg=0x00 2

# With mux routing
i2c_read front 0x50 mux=0x70.1 reg=0x00 16

# Write bytes
i2c_write mid 0x48 reg=0x01 0x00,0x80

# Scan all addresses on a bus
i2c_scan mid

# Scan all registers of a device
i2c_regscan mid 0x48
```

Bus names (`mid`, `front`, `rear`, `m2`) come from the archive's
`app.toml`.  Explicit `<controller>.<port>` syntax (e.g. `3.H`) is
also accepted.

### Idol RPC Calls

```
idol Sensor.get id=3
idol Thermal.get_mode
```

(Idol lowering is not yet implemented.)

### Loops

```
repeat 500
    i2c_read mid 0x48 reg=0x00 2
end

# With sleep between iterations
repeat 100 sleep=10ms
    i2c_read mid 0x48 reg=0x00 2
end
```

### Constants

```
.let TEMP_REG 0x00
.let ITERATIONS 1000

repeat $ITERATIONS
    i2c_read mid 0x48 reg=$TEMP_REG 2
end
```

### Sleep

```
sleep 50ms
```

Values over 100ms are automatically split into multiple Sleep calls.

### Raw Instructions

For anything the sugar doesn't cover.  Constants are expanded inside
raw blocks.

```
.let ADDR 0x48
raw {
    push $ADDR
    push_none
    push 2
    call I2cRead
    drop_n 7
    done
}
```

Available raw instructions: `push`, `push16`, `push32`, `push_none`,
`drop`, `drop_n`, `swap`, `add`, `label`, `branch_gt`, `branch_gte`,
`branch_lt`, `call`, `done`.

## TargetConfig

`TargetConfig` captures everything the assembler needs from a Hubris
archive in a single serializable struct:

- Image ID
- I2C bus topology (buses, devices, muxes, sensors)
- HIF function table (names, IDs, argument types, error codes)
- Buffer sizes (HIFFY_TEXT, HIFFY_DATA, HIFFY_RSTACK)

It can be extracted from an archive or loaded from a JSON fixture:

```rust
// From an archive
let config = TargetConfig::from_archive_file("gimlet-c-dev.zip")?;

// From a checked-in fixture
let config: TargetConfig =
    serde_json::from_str(&std::fs::read_to_string("fixtures/gimlet-c.json")?)?;

let asm = HifAssembler::new(config);
```

Pre-generated fixtures in `fixtures/` allow tests to run without
access to a Hubris archive or build environment.

## Testing

```bash
# Unit tests (no archive needed)
cargo test -p humility-hif-assembler --lib

# Fixture-based tests (no archive needed, uses checked-in JSON)
cargo test -p humility-hif-assembler --test fixture_tests

# Integration tests (requires a built archive)
HUBRIS_ARCHIVE=$(cd ~/Oxide/src/hubris/master && \
    cargo -q xtask print --archive app/gimlet/rev-c-dev.toml) \
    cargo test -p humility-hif-assembler --test archive_integration

# Regenerate a fixture from an archive
HUBRIS_ARCHIVE=path/to/archive.zip GENERATE_FIXTURE=1 \
    cargo test -p humility-hif-assembler --test archive_integration \
    generate_fixture -- --nocapture
```

## Relationship to humility-hiffy and RFD 659

This crate overlaps with code in `humility-hiffy`.  The overlap is
intentional and designed for eventual convergence.

### What overlaps

| Capability | humility-hiffy | hif-assembler |
|---|---|---|
| DWARF function table discovery | `HiffyContext::new()` | `archive.rs extract_hiffy_functions()` |
| Function name/arg resolution | `HiffyFunction` + `get()` | `FunctionInfo` + alias table |
| Result decoding | `HiffyContext::results()` | `HifBundle::decode_results()` |
| I2C parameter resolution | `humility-i2c` `I2cArgs` | `ResolvedBus` + bus name map |
| Idol call construction | `idol_call_ops()` family | Not yet implemented |
| Program construction | Per-command Op building | Text parser + assembler |

### Why the duplication exists

`humility-hiffy` is tightly coupled to a live target connection
(`Core` trait) and the humility CLI dispatch model.  It cannot be
used as a library for offline program construction, fixture
generation, or scripted test drivers.  This crate provides those
capabilities without modifying `humility-hiffy`.

### Convergence plan (RFD 659)

RFD 659 proposes turning humility into a library.  When that happens:

- **`TargetConfig`** replaces the ad-hoc archive introspection
  scattered through `HiffyContext::new()`.  It becomes the
  serializable contract between archive loading and program
  construction.

- **`HifAssembler`** replaces the per-command Op construction in
  `cmd/i2c`, `cmd/pmbus`, `cmd/gpio`, etc.  Each command becomes a
  thin wrapper that parses CLI args into a HIF text program (or uses
  `ProgramBuilder`) and hands it to the assembler.

- **`decode_results()`** becomes the shared result parser, replacing
  the inline `take_from_bytes` loop in `HiffyContext::results()`.

- **`HiffyContext`** narrows to execution only: uploading bytecode
  to a target (via probe or NetHiffy), kicking the hiffy task, and
  reading back results.  It no longer needs to know how programs are
  constructed.

### Design discipline

To keep convergence clean:

- This crate uses only public `HubrisArchive` APIs, never
  `HiffyContext` internals.
- Types are `Serialize + Deserialize` so they work as file formats
  and API contracts.
- The text language is a superset of what humility commands generate
  today — any program humility builds internally can be expressed in
  the text format.
