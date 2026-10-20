#!/usr/bin/env bash
# Regression checks for version-aware ranking. Run after dune build nonna.
set -euo pipefail
BIN="$PWD/_build/default/nonna/cli/main.exe"
TMP=$(mktemp -d)
TMP=$(cd "$TMP" && pwd -P)
trap 'rm -rf "$TMP"' EXIT
CORPUS="$TMP/corpus"
mkdir -p "$CORPUS/sample-1.0.0/src" "$CORPUS/sample-2.0.0/src"
A="$CORPUS/sample-1.0.0/src/lib.rs"
B="$CORPUS/sample-2.0.0/src/lib.rs"
cat > "$A" <<'EOF'
pub fn original(values: &[i64]) -> i64 {
    let mut total = 0;
    for v in values {
        total += v;
    }
    total
}
EOF
cat > "$B" <<'EOF'
pub fn renamed(text: &str) -> usize {
    let mut n = 0;
    for c in text.chars() {
        if c == '\n' {
            n += 1;
        }
    }
    n
}
EOF
printf 'pos\trenamed\t%s\t1\t7\toriginal\t%s\t1\t10\trenamed\n' "$A" "$B" > "$TMP/pairs.tsv"

run() {
    "$BIN" rank "$TMP/pairs.tsv" "$CORPUS" -o "$TMP/ranks.tsv" > "$TMP/summary"
}
accepts_copy() {
    run
    awk -F '\t' -v copy="$COPY" '$8 == 1 && $15 == copy {ok=1} END {exit !ok}' "$TMP/ranks.tsv"
    echo "ok   rank: $1"
}
skips_copy() {
    run
    awk -F '\t' -v copy="$COPY" -v b="$B" '$8 == 1 && $15 == b && $9 != copy {ok=1} END {exit !ok}' "$TMP/ranks.tsv"
    echo "ok   rank: $1"
}
rejects_copy() {
    run
    awk -F '\t' -v copy="$COPY" '$8 != 1 && $9 == copy && $15 != copy {ok=1} END {exit !ok}' "$TMP/ranks.tsv"
    echo "ok   rank: $1"
}

mkdir -p "$CORPUS/sample-3.0.0/src"
COPY="$CORPUS/sample-3.0.0/src/lib.rs"
cp "$A" "$COPY"
skips_copy "another version of A is skipped for a renamed pair"

{ printf 'struct Other;\nimpl Other {\n'; sed 's/fn original/fn renamed/' "$A"; printf '}\n'; } > "$COPY"
accepts_copy "repeated method names still use crate/path/name identity"

sed 's/fn original/fn renamed/' "$A" > "$COPY"
accepts_copy "another version of B counts"

sed 's/fn original/fn sibling/' "$A" > "$COPY"
rejects_copy "different function name stays a competitor"

cp "$A" "$COPY"
mv "$COPY" "$CORPUS/sample-3.0.0/src/other.rs"
COPY="$CORPUS/sample-3.0.0/src/other.rs"
rejects_copy "different relative path stays a competitor"

mkdir -p "$CORPUS/unrelated-3.0.0/src"
mv "$COPY" "$CORPUS/unrelated-3.0.0/src/lib.rs"
COPY="$CORPUS/unrelated-3.0.0/src/lib.rs"
rejects_copy "different crate stays a competitor"

rm "$COPY"
# B is the only remaining candidate: this also checks that A isn't its own hit.
run
awk -F '\t' -v b="$B" '$8 == 1 && $15 == b {ok=1} END {exit !ok}' "$TMP/ranks.tsv"
echo "ok   rank: exact B remains accepted and A is excluded"

cp "$A" "$B"
run
awk -F '\t' -v b="$B" '$8 == 1 && $15 == b {ok=1} END {exit !ok}' "$TMP/ranks.tsv"
echo "ok   rank: exact-location matching survives stale pair names"

cp "$A" "$COPY"
run
awk -F '\t' -v b="$B" '$8 == 1 && $15 == b {ok=1} END {exit !ok}' "$TMP/ranks.tsv"
echo "ok   rank: an unrelated tied hit does not penalize B"

ln -s "$CORPUS" "$TMP/alias"
printf 'pos\texact\t%s\t1\t7\toriginal\t%s\t1\t7\toriginal\n' \
    "$TMP/alias/sample-1.0.0//src/lib.rs" \
    "$TMP/alias/sample-2.0.0//src/lib.rs" > "$TMP/pairs.tsv"
run
awk -F '\t' -v b="$B" '$8 == 1 && $15 == b {ok=1} END {exit !ok}' "$TMP/ranks.tsv"
echo "ok   rank: symlinks and double slashes resolve to the indexed endpoints"

# evolved pair (A and B are the same function): A's other versions count
printf 'pub fn original(values: &[i64]) -> i64 {\n    let mut total = 1;\n    for v in values {\n        total *= v;\n    }\n    total\n}\n' > "$B"
cp "$A" "$CORPUS/sample-3.0.0/src/lib.rs"
COPY=$(cd "$CORPUS/sample-3.0.0/src" && pwd -P)/lib.rs
printf 'pos\tevolved\t%s\t1\t7\toriginal\t%s\t1\t7\toriginal\n' "$A" "$B" > "$TMP/pairs.tsv"
accepts_copy "another version of A counts for an evolved pair"
