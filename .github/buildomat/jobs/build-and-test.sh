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
	'/library/libusb'
	'/library/libftdi1'
)
missing=()
for (( i = 0; i < ${#need[@]}; i++ )); do
	p=${need[$i]}
	if ! pkg info -q "$p"; then
		missing+=( "$p" )
	fi
done
if (( ${#missing[@]} > 0 )); then
	pfexec pkg install -v "${missing[@]}"
fi
pkg list -v "${need[@]}"

cargo --version
rustc --version

banner test
ptime -m cargo test --profile=ci --verbose
