#!/usr/bin/env bash
# Phase 1 regression checks. Run from the repo root:
#   bash tests/run.sh
# Requires a built tree (see DESIGN.md "Build notes" for env setup).
set -u
BIN=_build/default/nonna/cli/main.exe
fail=0

check() { # name, condition
  if eval "$2"; then echo "ok   $1"; else echo "FAIL $1"; fail=1; fi
}

OUT=$("$BIN" query tests/fixtures/corpus -- tests/fixtures/draft.rs -t 0.15 -k 5)

# Rename-invariance: avg (renamed/reordered mean) must match mean with j >= 0.95.
AVG_LINE=$(echo "$OUT" | grep -A2 '── avg' | grep 'mean (')
check "rename-invariance: avg matches mean" \
  "[ -n \"\$AVG_LINE\" ] && awk -v l=\"\$AVG_LINE\" 'BEGIN{split(l,a,\" \"); exit !(a[2] >= 0.95)}'"

# Containment: floor_all (subset of clamp_all) containment >= 0.85, > its jaccard.
FLOOR_LINE=$(echo "$OUT" | grep -A2 '── floor_all' | grep 'clamp_all (')
check "containment: floor_all in clamp_all" \
  "[ -n \"\$FLOOR_LINE\" ] && awk -v l=\"\$FLOOR_LINE\" 'BEGIN{split(l,a,\" \"); exit !(a[4] >= 0.85 && a[4] > a[2])}'"

# Precision: unrelated join_with must have no hit above a meaningful gate.
# (Exact candidate generation surfaces weak hits at low thresholds by design;
# the claim is that none of them score >= 0.4.)
JOIN_MAX=$(echo "$OUT" | awk '/── join_with/{f=1;next} /^── /{f=0} f && /jaccard/{print $2}' | sort -rn | head -1)
check "precision: join_with nothing >= 0.4" \
  "[ -z \"\$JOIN_MAX\" ] || awk -v x=\"\$JOIN_MAX\" 'BEGIN{exit !(x < 0.4)}'"

# Dupes: across fixtures + spike, the only >=0.9 pair is mean <-> avg.
DUPES=$("$BIN" dupes tests/fixtures spike -t 0.9 | grep -c '<->')
check "dupes: exactly one high-sim pair in fixtures" "[ \"\$DUPES\" = 1 ]"

exit $fail
