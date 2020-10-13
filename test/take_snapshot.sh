#!/bin/bash

set -euxo pipefail

dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
prog="$dir/../_build/default/bin/btrshotter.exe"

fs="$1"

src="$fs/src"
dst="$fs/dst"

btrfs subvolume create "$src"
btrfs subvolume create "$dst"

touch "$src/target"

$prog take -src "$src" -dst "$dst"

ls "$dst"/*/target
