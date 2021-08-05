# Repro for LLVM issue

This repo is not, in fact, a functional Humility, but rather an attempt to
reproduce a an LLVM optimization issue that results in a non-terminating loop.

To build this, you should install the `nightly-2021-03-04` Rust compiler,
you must do this:

```console
cargo +nightly-2021-03-04-x86_64-unknown-linux-gnu build --release 2> /dev/null
```

If this has correctly reproduced the issue, running the Humility `pmbus`
command against a Hubris archive should yield an infinite loop:

```console
$ ./target/release/humility -a /path/to/a/valid/hubris/archive pmbus -c 20 -d 0
i is 0
i is 1
i is 2
i is 3
i is 4
i is 5
i is 6
i is 7
i is 8
i is 9
i is 10
...
i is 248
i is 249
i is 250
i is 251
i is 252
i is 253
i is 254
i is 255
i is 0
i is 1
i is 2
i is 3
...
```

If you have not reproduced it, you will simply see 256 lines of output.
Note that compiling with `debug`, running with `opt-level=1` or running a
compiler more recent than `nightly-2021-03-04` will all result in a correctly
functioning program.

The pass that seems to be inducing this is pass 659367:

```
BISECT: NOT running pass (659367) Expand memcmp() to load/stores on function (_ZN8humility3cmd5pmbus5pmbus17hc4299134eab278b6E)
```

That is, this results in a correctly functioning binary:

```
RUSTFLAGS="-C codegen-units=1 -C opt-level=2 -C llvm-args=-opt-bisect-limit=659366" cargo +nightly-2021-03-04-x86_64-unknown-linux-gnu build --release 2> /dev/null
```

This results in an incorrect binary:

```
RUSTFLAGS="-C codegen-units=1 -C opt-level=2 -C llvm-args=-opt-bisect-limit=659367" cargo +nightly-2021-03-04-x86_64-unknown-linux-gnu build --release 2> /dev/null
```

