# Testing Humility

Many Humility commands require being attached to a live system to be able to
operate (and quite a few also require specific target devices); absent an
apparatus to have test machines connected to attached requisite hardware, these
commands must (more or less) be tested manually. 

For commands that can operate on a dump, however, testing can be 
more readily automated:  for these commands, we use <a
href="https://github.com/assert-rs/trycmd">trycmd</a> to test Humility
commands on a wide variety of input in the form of Hubris core files (see
<tt><a href="https://github.com/oxidecomputer/humility#humility-dump">humility
dump</a></tt> for details on Hubris dumps).

## Adding dumps 

Cores that act as input are in the `cmd/cores` subdirectory; to add a new core
file, deposit it there with a unique name that begins with `hubris.core.`

A core is opaque once checked in:  nothing in the file says which board it came
from or which toolchain built the image.  When a core is added to cover
something specific, record it under "Dump provenance" below, so that a later
reader can tell what it is for and whether it is still needed.

## Dump provenance

`hubris.core.sidecar-rust-1.95.0` was taken from a sidecar-b-lab running
`all-sp-v1.76.0` (GITC `1a60776c91377792aeed1d4ce4849bcb98339818`), the first
Hubris release series built with stable Rust 1.95.0.  That toolchain changed
the in-DWARF representation of `MaybeUninit`, which broke every command that
decodes one until humility learned to handle it (oxidecomputer/hubris#2615).
It is here so the suite keeps exercising that layout.  `humility sensors`
against a dump uses the `readmem` backend, which is the path that failed.

Note that this is a sidecar image, so the commands that want Gimlet state
(`spd`, `host`, and counters naming `gimlet_seq`) are marked as expected
failures, per "Indicating expected failure" below.

## Keeping up with the toolchain

These dumps are frozen:  each one decodes the same way forever, so the suite
catches humility regressions against the toolchains already represented here,
but it cannot catch the *next* change to how rustc represents a type.  Nothing
in the corpus was built by a toolchain that does not yet exist, so a change of
that kind shows up as a person in the lab hitting it, which is how #2615 was
found.

The way to keep that window short is to add a dump whenever Hubris changes
`rust-toolchain.toml`, and to record it under "Dump provenance" above.  That is
what `hubris.core.sidecar-rust-1.95.0` is, and what `hubris.core.new-compiler`
and `hubris.core.nightly-2022-11-01` appear to have been for earlier eras.

## Adding archives

Some tests are able to operate on archives alone.  Archives that act as input
for these tests are in the `cmd/archives` subdirectoty; to add a new archive,
deposit it there with a unique name that begins with `build-`.

## Running tests

Running `cargo test` will automatically generate the `toml` files that
correspond to a test case for each postmortem command across each dump, and
then run those tests.

To run a more limited subset, the (Humility-specific) `TRYCMD_TEST`
environment variable can be set to a particular test or to a wildcard, e.g.
to run only the `humility manifest` command against all dumps:

```console
$ TRYCMD_TEST="tests/cmd/manifest/*.toml" cargo test
```

Note that this can also be used to run all tests against a specific dump:

```console
$ TRYCMD_TEST="tests/cmd/*/*.kiowa.18.toml" cargo test
```

Or against a particular archive:

$ TRYCMD_TEST="tests/cmd/*/*.gimlet-rot-c-image-b.zip.toml" cargo test

## Changes in output

`trycmd` is particularly valuable when the output of many commands change.
Running `cargo test` will yield the differences between the expected standard
output/standard error and the results of running the command; if the results
look correct, re-running the tests with `TRYCMD` set to `overwrite` will
rewrite the content of the test output to contain the (newly correct) output:

```console
$ TRYCMD_TEST="tests/cmd/tasks/*.toml" TRYCMD=overwrite cargo test
```

Note that the run that contains `TRYCMD=overwrite` will fail -- but because
the output will have been updated, a subsequent run without setting
`TRYCMD` should succeed.

## Indicating expected failure

Some commands fail on some dumps because they are seeking state that is
not always present.  To denote that these are expected to fail, the status
should be indicated in the corresponding test TOML file:

```toml
status.code = 1
```

(It can be helpful to also add a comment to indicate why the command fails
on the dump.)

