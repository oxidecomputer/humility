#!/bin/bash
#:
#: name = "build-and-test"
#: variety = "basic"
#: target = "helios"
#: rust_toolchain = "stable"
#: output_rules = []
#:

set -o errexit
set -o pipefail
set -o xtrace

#
# Disable incremental builds, as per ".github/workflows/ci.yaml":
#
export CARGO_INCREMENTAL=0

banner deps
need=(
	'/ooce/library/libusb-1'
	'/library/libftdi1'
)
PKG_SUCCESS_ON_NOP=1 pfexec pkg install -v "${need[@]}"
pkg list -v "${need[@]}"

cargo --version
rustc --version

banner test
ptime -m cargo test --profile=ci --verbose
