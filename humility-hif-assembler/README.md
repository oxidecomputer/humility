# humility-hif-assembler

Assembler for HIF (Hubris Interchange Format) text programs.

## Overview

This crate translates text-based HIF programs into bytecode that the
`hiffy` task on a Hubris SP or RoT can execute.  It resolves symbolic
names (bus names, function names, device addresses) against a Hubris
archive, validates programs against target buffer sizes, and produces
bundles with embedded image IDs for safe upload.

## Quick Start

```bash
SP_IP=fe80::aa40:25ff:fe05:0500%3
ARCHIVE=build-sidecar-b-lab-image-default.zip

# Verify a program offline (no target needed) — shows stats and ops
humility -a $ARCHIVE hiffy --verify stress.hif

# Assemble to a bundle file (no target needed)
humility -a $ARCHIVE hiffy --assemble stress.hif --bundle-output stress.hifb

# Unlock the sidecar's network interface
faux-mgs --interface axf2 \
    --discovery-addr "[${SP_IP%\%*}]:11111" \
    monorail unlock -t 3600sec

# Execute a program on a target over the network
humility -a $ARCHIVE --ip $SP_IP hiffy --exec stress.hif

# JSON output for scripting
humility -a $ARCHIVE --ip $SP_IP hiffy --exec stress.hif --json
```

The assembler checks the archive's `app.toml` to verify that the
target image can accept HIF programs over the network.  Specifically,
it checks that the `hiffy` task has the `net` feature enabled.  If
it's missing, the assembler warns at assembly time:

```
warning: hiffy task does not have 'net' feature; network execution
requires adding features = ["net", "vlan"] and a hiffy socket to
the app.toml (probe execution still works)
```

This check runs during `--verify`, `--assemble`, and `--exec`, so
you find out before attempting a network connection that the image
won't support it.

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

Bus names (`mid`, `front`, `rear`, `northeast0`, etc.) come from the
archive's `app.toml`.  Explicit `<controller>.<port>` syntax (e.g.
`3.H`) is also accepted.

### Generic Function Calls

Any HIF function can be called by name with optional numeric
arguments.  This covers QSPI, GPIO, Hash, SPI, and any future
functions without needing per-function sugar:

```
# No args
call QspiReadId

# With args
call GpioInput 5
```

Function names come from the `HIFFY_FUNCTIONS` table in the archive
(extracted from DWARF).  Both CamelCase (`QspiReadId`) and
snake_case (`qspi_read_id`) are accepted.

### Idol RPC Calls

```
idol Sensor.get id=3
idol SpRot.status
idol Thermal.get_mode
idol Power.read_mode dev=0 rail=0 index=0
```

The assembler resolves interface and operation names from the
`.idolatry` sections in the archive, encodes arguments, and emits
the appropriate `Send` call.  Reply sizes are computed from DWARF
so results are properly captured.

### Loops

```
repeat 200
    i2c_read mid 0x48 reg=0x00 2
end

# With sleep between iterations
repeat 100 sleep=10ms
    i2c_read mid 0x48 reg=0x00 2
end
```

Loops consume one of the four available HIF labels per nesting
level.  Iteration count is limited by RSTACK capacity (~250-680
results depending on result size).

### Constants

```
.let TEMP_REG 0x00
.let SENSOR 0x48
.let ITERATIONS 200

repeat $ITERATIONS
    i2c_read rear $SENSOR reg=$TEMP_REG 2
end
```

### Sleep

```
sleep 50ms
```

Values over 100ms are automatically split into multiple Sleep calls.

### Raw Instructions

For anything else.  Constants are expanded inside raw blocks:

```
.let ADDR 0x48
raw {
    push $ADDR
    push_none
    push 2
    call I2cRead
    drop_n 7
}
```

Available: `push`, `push16`, `push32`, `push_none`, `drop`,
`drop_n`, `swap`, `add`, `label`, `branch_gt`, `branch_gte`,
`branch_lt`, `call`, `done`.

## Output

### Verify output (`--verify`)

Shows program stats, expected resource usage, and a disassembly
of the generated HIF ops.  The disassembly uses the assembler's
raw syntax (left column) with postcard byte encoding (right
column), so it can be pasted into a `raw {}` block and re-assembled
to produce the same bytecode.  Function IDs are resolved back to
names from the archive.

```
OK

  text:       31 / 4096 bytes
  rstack:     40 / 2048 bytes (est. 5 results)
  labels:      1 / 4
  functions: i2c_read
  buses: northeast1
I2C transactions: 5
  reads:  5 (10 bytes)
Buses: northeast1

Ops (31 bytes, 18 ops):
raw {
  push 0                     # 00: 04 00
  push_none                  # 02: 07
  label 0                    # 03: 00 00
  drop                       # 05: 02
  push 1                     # 06: 04 01
  push 1                     # 08: 04 01
  push_none                  # 0a: 07
  push_none                  # 0b: 07
  push 0x48                  # 0c: 04 48
  push 0                     # 0e: 04 00
  push 2                     # 10: 04 02
  call I2cRead               # 12: 01 05
  drop_n 7                   # 14: 03 07
  push 1                     # 16: 04 01
  add                        # 18: 0a
  push 5                     # 19: 04 05
  branch_gt 0                # 1b: 10 00
  done                       # 1d: 14
}
```

### Execution output (`--exec`, default)

```
humility: program: stress.hif
humility: expected: 50 I2C transactions, 0 Idol calls
humility: executed in 658.2ms: 50 results (50 ok, 0 err)
humility:   [0] Ok([0c, cf])
humility:   ... (48 more results) ...
humility:   [49] Ok([0c, cf])
```

Idol results are fully decoded using DWARF type info:

```
humility:   [0] SpRot.status() => SprotStatus { rot: RotStatus { ... }, sp: SpStatus { ... } }
```

### JSON output (`--json`)

```json
{
  "ok": true,
  "results": 50,
  "successes": 50,
  "errors": 0,
  "elapsed_ms": 658,
  "stats": {
    "i2c_transactions": 50,
    "i2c_read_bytes": 100,
    "buses": ["northeast1"]
  },
  "samples": [
    {"index": 0, "value": "Ok([0c, cf])"},
    {"index": 49, "value": "Ok([0c, cf])"}
  ]
}
```

### Exit codes

- 0: program ran, no errors
- 1: program ran, errors detected (use `--json` for details)
- Non-zero: assembly or execution failed

## Scripting

```bash
#!/bin/bash
result=$(humility -a $ARCHIVE --ip $SP hiffy --exec stress.hif --json)
errors=$(echo "$result" | jq .errors)

if [ "$errors" -gt 0 ]; then
    echo "Failure after $(echo "$result" | jq .successes) successes"
    humility -a $ARCHIVE --ip $SP ringbuf i2c_driver
    humility -a $ARCHIVE --ip $SP tasks
fi
```

## TargetConfig

`TargetConfig` captures everything the assembler needs from a Hubris
archive in a single serializable struct:

- Image ID
- I2C bus topology (buses, devices, muxes, sensors)
- HIF function table (names, IDs, argument types, error codes)
- Idol interfaces (operations, argument/reply types, sizes, encoding)
- Buffer sizes (HIFFY_TEXT, HIFFY_DATA, HIFFY_RSTACK)

It can be extracted from an archive or loaded from a JSON fixture:

```rust
// From an archive
let config = TargetConfig::from_archive_file("sidecar-b-lab.zip")?;

// From a checked-in fixture (test data only)
let config: TargetConfig =
    serde_json::from_str(&std::fs::read_to_string("fixtures/sidecar-b.json")?)?;

let asm = HifAssembler::new(config);
```

## ProgramBuilder

For generating programs from Rust (e.g., PRNG-driven fuzz testing):

```rust
let mut prog = ProgramBuilder::new();
prog.comment("temperature stress test");
prog.repeat(200, |body| {
    body.i2c_read("rear", 0x48, Some(0x00), 2);
});
let source = prog.finish();
let output = asm.assemble(&source)?;
```

## Testing

```bash
# Unit tests (no archive needed)
cargo test -p humility-hif-assembler --lib

# Fixture-based tests (no archive needed, uses checked-in JSON)
cargo test -p humility-hif-assembler --test fixture_tests

# Integration tests (requires a built archive)
HUBRIS_ARCHIVE=path/to/archive.zip \
    cargo test -p humility-hif-assembler --test archive_integration

# Regenerate a fixture from an archive
HUBRIS_ARCHIVE=path/to/archive.zip GENERATE_FIXTURE=1 \
    cargo test -p humility-hif-assembler --test archive_integration \
    generate_fixture -- --nocapture
```

## Hubris Image Requirements

For network execution (`--ip`), the Hubris image must have:

1. The `hiffy` task with `features = ["net", "vlan"]`
2. A `net` task-slot on the hiffy task
3. A `socket` notification on the hiffy task
4. A UDP socket configured for hiffy:

```toml
# In the dev.toml or lab.toml overlay:
[tasks.hiffy]
features = ["net", "vlan"]
task-slots = ["net"]
notifications = ["socket"]

[config.net.sockets.hiffy]
kind = "udp"
owner = {name = "hiffy", notification = "socket"}
port = 11115
tx = { packets = 3, bytes = 32 }
rx = { packets = 1, bytes = 4096 }
```

The hiffy task's priority must be lower than the net task's priority
(higher number = lower priority) to avoid priority inversion.

Probe execution (`-p`) works with any image that has a hiffy task.

## Relationship to humility-hiffy and RFD 659

This crate overlaps with code in `humility-hiffy`.  The overlap is
intentional and designed for eventual convergence.

`humility-hiffy` is tightly coupled to a live target connection
(`Core` trait) and the humility CLI dispatch model.  It cannot be
used as a library for offline program construction, fixture
generation, or scripted test drivers.  This crate provides those
capabilities without modifying `humility-hiffy`.

RFD 659 proposes turning humility into a library.  When that happens,
`TargetConfig` could become the serializable contract between archive
loading and program construction, and `HifAssembler` replaces the
per-command Op construction scattered across humility's subcommands.
See `lib.rs` module docs for details.
