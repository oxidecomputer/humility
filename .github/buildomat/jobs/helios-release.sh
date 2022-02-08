#!/bin/bash
#:
#: name = "helios-release"
#: variety = "basic"
#: target = "helios"
#: rust_toolchain = "stable"
#: output_rules = [
#:	"/work/out/*",
#: ]
#:

set -o errexit
set -o pipefail
set -o xtrace

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

banner build
ptime -m cargo build --verbose --release

banner compress
mkdir -p /work/out
cp target/release/humility /work/out/humility
cd /work/out
digest -a sha256 humility > humility.sha256.txt
ptime -m gzip -9 humility
digest -a sha256 humility.gz > humility.gz.sha256.txt
