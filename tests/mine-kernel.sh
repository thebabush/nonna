#!/usr/bin/env bash
# Build the Linux-kernel C benchmark: two version snapshots of a sparse
# subsystem slice (evolved pairs come from cross-version function drift),
# then mine ground truth. Mirrors mine-registry.sh (Rust) / mine-pypi.sh
# (Python).
#
#   bash tests/mine-kernel.sh [corpus-dir]   # default /tmp/kernel-corpus
#
# Writes bench-out/kernel-pairs.tsv + bench-out/kernel-paths.txt, then:
#   nonna rank bench-out/kernel-pairs.tsv $(cat bench-out/kernel-paths.txt)
set -eu
BIN=_build/default/nonna/cli/main.exe
DEST=${1:-/tmp/kernel-corpus}
# Directory names must look like "name-X.Y.Z" for the miner's version
# stripping; same-major tags so drift lands in `evolved`, not evolved_major.
VERSIONS="6.16 6.10"
SLICE="kernel lib mm fs/ext4 net/ipv4 drivers/gpu/drm/i915"

mkdir -p "$DEST" bench-out
CLONE="$DEST/.linux-git"
if [ ! -d "$CLONE" ]; then
  git clone --depth 1 --filter=blob:none --sparse \
    https://github.com/torvalds/linux "$CLONE"
fi
for v in $VERSIONS; do
  WT="$DEST/linux-$v.0"
  if [ ! -d "$WT" ]; then
    git -C "$CLONE" fetch --depth 1 origin tag "v$v"
    git -C "$CLONE" worktree add --no-checkout --detach "$WT" "v$v"
    git -C "$WT" sparse-checkout set $SLICE
    git -C "$WT" checkout
  fi
done

: > bench-out/kernel-paths.txt
for v in $VERSIONS; do echo "$DEST/linux-$v.0" >> bench-out/kernel-paths.txt; done

"$BIN" mine $(cat bench-out/kernel-paths.txt) -o bench-out/kernel-pairs.tsv
echo "next: $BIN rank bench-out/kernel-pairs.tsv \$(cat bench-out/kernel-paths.txt)"
