#!/usr/bin/env bash
# Mine ground-truth similarity pairs from multi-version crates in the local
# cargo registry cache, then evaluate the engine against them:
#   bash tests/mine-registry.sh [outdir]
# Results depend on what ~/.cargo/registry happens to contain — the numbers
# are comparable across engine changes on the SAME machine/cache.
set -eu
BIN=_build/default/nonna/cli/main.exe
OUT=${1:-bench-out}
mkdir -p "$OUT"
REG=$(ls -d ~/.cargo/registry/src/*/ | head -1)

# crates present in >= 2 versions (densest source of "evolved" positives);
# windows*/libc excluded (huge generated declaration files, ~no fn bodies)
ls "$REG" | sed -E 's/-[0-9][0-9a-zA-Z.+-]*$//' | sort | uniq -d \
  | while read -r b; do
      ls -d "$REG$b"-[0-9]* 2>/dev/null | while read -r d; do
        bb=$(basename "$d" | sed -E 's/-[0-9][0-9a-zA-Z.+-]*$//')
        [ "$bb" = "$b" ] && echo "$d"
      done
    done | grep -vE 'windows|libc' > "$OUT/paths.txt"

echo "mining $(wc -l < "$OUT/paths.txt") crate versions..."
$BIN mine $(tr '\n' ' ' < "$OUT/paths.txt") -o "$OUT/pairs.tsv"
$BIN eval "$OUT/pairs.tsv" | tee "$OUT/report.txt"
