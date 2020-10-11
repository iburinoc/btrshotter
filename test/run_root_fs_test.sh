#!/bin/bash

set -euxo pipefail

root="$(mktemp -d)"

idx="$(($RANDOM+1000))"
dev="/dev/loop${idx}"

image="$root/image"
dd if=/dev/zero of="$image" count=30 bs=10M

losetup "$dev" "$image"

mkfs.btrfs "$dev"

fs="$root/fs"
mkdir "$fs"
mount "$dev" "$fs"

"$@" "$fs"

umount "$fs"
losetup -d "$dev"
rm "$image"
