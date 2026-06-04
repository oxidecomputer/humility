#!/bin/ksh
#:
#: name = "humility-p5p"
#: variety = "basic"
#: target = "helios-3.0"
#: rust_toolchain = "stable"
#: output_rules = [
#:   "=/out/humility.p5p",
#:   "=/out/humility.p5p.sha256",
#: ]
#:
#: [[publish]]
#: series = "repo"
#: name = "humility.p5p"
#: from_output = "/out/humility.p5p"
#:
#: [[publish]]
#: series = "repo"
#: name = "humility.p5p.sha256"
#: from_output = "/out/humility.p5p.sha256"
#:

set -ex

#
# Disable incremental builds, as per ".github/workflows/ci.yaml":
#
export CARGO_INCREMENTAL=0

#
# Install the libraries that humility links against. These are required to
# build the binary, and must also be present so that `pkgdepend resolve` can
# map the binary's NEEDED libraries to the packages that provide them.
#
banner deps
need=(
	'/ooce/library/libusb-1'
	'/library/libftdi1'
)
PKG_SUCCESS_ON_NOP=1 pfexec pkg install -v "${need[@]}"
pkg list -v "${need[@]}"

cargo --version
rustc --version

#
# `cargo xtask package` builds the humility binary and produces the p5p.
#
banner package
ptime -m cargo xtask package

banner copy
pfexec mkdir -p /out
pfexec chown "$UID" /out
PKG_NAME="/out/humility.p5p"
mv pkg/packages/repo/*.p5p "$PKG_NAME"
sha256sum "$PKG_NAME" > "$PKG_NAME.sha256"
