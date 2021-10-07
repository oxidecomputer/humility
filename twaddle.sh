#!/bin/sh

set -euxo pipefail

CMD="cargo run --release -- -a ../hubris3/target/gimletlet/dist/build-gimletlet.zip"

# B10 = reset
# E15 = done
# E11 = CS
$CMD gpio -c Input:OpenDrain:High:None:AF0 -p E:15
$CMD gpio -c Output:OpenDrain:High:None:AF0 -p B:10
$CMD gpio -c Output:PushPull:High:None:AF0 -p E:11

# enter reset
$CMD gpio -r -p B:10
# assert CS
$CMD gpio -r -p E:11
# leave reset
$CMD gpio -s -p B:10

# stuff data into brain
$CMD spiload $1

# leave in a state where I can inspect it.
