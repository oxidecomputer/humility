#!/bin/ksh
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.
#
# Build a Helios (IPS) package for humility. This is normally invoked via
# `cargo xtask package`, which builds the binary first; it can also be run
# directly from the pkg/ directory once target/release/humility exists.
#

typeset -r PUBLISHER=helios
typeset -r HELIOS_RELEASE=$(awk -F= '/^VERSION=/{print $2}' /etc/os-release)
typeset -r COMMIT_COUNT=$(git rev-list --count HEAD)
typeset -r REPO=packages/repo
typeset -r BINARY=../target/release/humility
typeset -ri TAG=0
set -e

rm -rf proto packages
rm -f humility.{base,generate,final}.p5m

function fatal {
	print -u2 "$@"
	exit 1
}

# create the proto area
mkdir -p proto/usr/bin
[ -x "$BINARY" ] || fatal "$BINARY not found"
cp "$BINARY" proto/usr/bin/humility

VERSION=$(awk -F\" '/^version = /{print $2; exit}' ../humility-bin/Cargo.toml)
PKG_VERSION="$VERSION.$COMMIT_COUNT-$HELIOS_RELEASE.$TAG"

# create the package
sed -e "s/%PUBLISHER%/$PUBLISHER/g" \
    -e "s/%PKG_VERSION%/$PKG_VERSION/g" \
    humility.template.p5m | pkgmogrify -v -O humility.base.p5m

pkgdepend generate -d proto humility.base.p5m > humility.generate.p5m

mkdir -p packages
pkgdepend resolve -d packages -s resolve.p5m humility.generate.p5m

cat humility.base.p5m packages/humility.generate.p5m.resolve.p5m \
	> humility.final.p5m

pkgrepo create $REPO
pkgrepo add-publisher -s $REPO $PUBLISHER

pkgsend publish -d proto -s $REPO humility.final.p5m
pkgrecv -a -d packages/repo/humility-$PKG_VERSION.p5p -s $REPO \
	-v -m latest '*'

