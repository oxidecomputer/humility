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

