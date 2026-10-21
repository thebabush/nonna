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
# (the fixtures live under tests/, which the test-code path rule flags: --tests)
DUPES=$("$BIN" dupes tests/fixtures spike -t 0.9 --tests | grep -c '<->')
check "dupes: exactly one high-sim pair in fixtures" "[ \"\$DUPES\" = 1 ]"
NOTESTS=$("$BIN" dupes tests/fixtures spike -t 0.9 | grep -c '<->')
check "dupes: test-path units skipped by default" "[ \"\$NOTESTS\" = 0 ]"

# Clone classes (D18): the same pair is one class of two, and the summary's
# percentage is its duplicated SLOC over the code lines scanned.
CLS=$("$BIN" dupes tests/fixtures spike -t 0.9 --tests --classes)
check "dupes --classes: one class, mean + avg" \
  "echo \"\$CLS\" | grep -q '^nonna dupes: 1 clone classes, 2 functions' &&
   [ \"\$(echo \"\$CLS\" | grep -c '^#')\" = 1 ] &&
   echo \"\$CLS\" | grep -q '^  mean ' && echo \"\$CLS\" | grep -q '^  avg '"
SUM=$(echo "$CLS" | head -1)
check "dupes --classes: summary percentage is D / T" \
  "echo \"\$SUM\" | awk '{d=\$8; p=\$11; t=\$13; sub(/[(]/,\"\",p); sub(/%/,\"\",p);
     exit !(d > 0 && t > 0 && sprintf(\"%.1f\", 100*d/t) == p)}'"
check "dupes --classes: class SLOC is the summary's" \
  "[ \"\$(echo \"\$CLS\" | grep '^#1 ' | sed 's/.*(\\([0-9]*\\) duplicated).*/\\1/')\" = \"\$(echo \"\$SUM\" | awk '{print \$8}')\" ]"
CLSB=$("$BIN" dupes tests/fixtures spike -t 0.9 --tests --classes --blocks | head -1)
check "dupes --classes --blocks: blocks inside the class add nothing" \
  "echo \"\$CLSB\" | grep -q '0 of them outside the classes: '\"\$(echo \"\$SUM\" | awk '{print \$8}')\"' duplicated SLOC in all'"
# A closure in a class member folds into it: outer (14 code lines) holds f
# (11), plain is 11; the class deletes plain only and the total is 14 + 11.
NEST=$("$BIN" dupes tests/classes -t 0.3 --tests --classes | head -1)
check "dupes --classes: nested member folded (11 of 25)" \
  "echo \"\$NEST\" | grep -q '3 functions, 11 duplicated SLOC (44.0% of 25 code lines'"

# Rust macro arguments are reparsed: assert-only tests that call different
# functions no longer share a signature; renamed copies still match.
MAC=$("$BIN" dupes tests/macros -t 0.0 --tests --min-features 1)
MAC_SAME=$(echo "$MAC" | grep 'token_cases ' | grep 'token_cases_again ' | awk '{print $2}')
check "macros: renamed assert-only copy still matches (j >= 0.95)" \
  "[ -n \"\$MAC_SAME\" ] && awk -v x=\"\$MAC_SAME\" 'BEGIN{exit !(x >= 0.95)}'"
MAC_DIFF=$(echo "$MAC" | grep 'path_cases ' | grep 'token_cases ' | awk '{print $2}' | sort -rn | head -1)
check "macros: different callees do not collapse (j < 0.6)" \
  "[ -z \"\$MAC_DIFF\" ] || awk -v x=\"\$MAC_DIFF\" 'BEGIN{exit !(x < 0.6)}'"
check "macros: unparseable token trees are indexed" \
  "echo \"\$MAC\" | grep -q 'indexed 4 units'"

# Call-graph context (D17, family.context, default off): wrappers with one
# body calling dissimilar callees drift apart, wrappers calling similar
# callees do not; one closure in two dissimilar parents stops being identical.
pair_j() { # dupes output, name a, name b -> jaccard of that pair
  echo "$1" | grep -F " $2 (" | grep -F " $3 (" | awk '{print $2}' | head -1
}
CTX_OFF=$("$BIN" dupes tests/context -t 0.0 --tests)
CTX_ON=$("$BIN" --weights family.context=0.5 dupes tests/context -t 0.0 --tests)
CTX_NOPARENT=$("$BIN" --weights family.context=0.5,context.parent=0 dupes tests/context -t 0.0 --tests)
D_OFF=$(pair_j "$CTX_OFF" wrap_checksum wrap_label)
D_ON=$(pair_j "$CTX_ON" wrap_checksum wrap_label)
check "context: wrappers over dissimilar callees score lower ($D_OFF -> $D_ON)" \
  "[ -n \"\$D_OFF\" ] && [ -n \"\$D_ON\" ] && awk -v a=\"\$D_OFF\" -v b=\"\$D_ON\" 'BEGIN{exit !(b < a - 0.1)}'"
S_OFF=$(pair_j "$CTX_OFF" wrap_weight wrap_cost)
S_ON=$(pair_j "$CTX_ON" wrap_weight wrap_cost)
check "context: wrappers over similar callees stay high ($S_OFF -> $S_ON)" \
  "[ -n \"\$S_OFF\" ] && [ -n \"\$S_ON\" ] && awk -v a=\"\$S_OFF\" -v b=\"\$S_ON\" 'BEGIN{exit !(b >= a)}'"
L_OFF=$(pair_j "$CTX_OFF" 'ids_csv::<lambda>' 'scaled_labels::<lambda>')
L_ON=$(pair_j "$CTX_ON" 'ids_csv::<lambda>' 'scaled_labels::<lambda>')
L_NOPARENT=$(pair_j "$CTX_NOPARENT" 'ids_csv::<lambda>' 'scaled_labels::<lambda>')
check "context: one closure in dissimilar parents drifts apart ($L_OFF -> $L_ON)" \
  "[ \"\$L_OFF\" = 1.000 ] && [ -n \"\$L_ON\" ] && awk -v x=\"\$L_ON\" 'BEGIN{exit !(x < 0.9)}'"
check "context: context.parent=0 switches the parent off" "[ \"\$L_NOPARENT\" = 1.000 ]"

# check: a throwaway repo with util.rs committed; draft.rs's avg copies mean.
ABS_BIN=$PWD/$BIN
REPO=$(mktemp -d)
mkdir "$REPO/src"
cp tests/fixtures/corpus/util.rs "$REPO/src/util.rs"
(cd "$REPO" && git init -q && git add -A &&
  git -c user.name=t -c user.email=t@t commit -qm init)
cp tests/fixtures/draft.rs "$REPO/src/draft.rs"
CHECK=$(cd "$REPO" && "$ABS_BIN" check)
check "check: untracked copy of mean reported" \
  "echo \"\$CHECK\" | grep -A1 '^  avg ' | grep -q '~ mean '"
CHECK=$(cd "$REPO" && "$ABS_BIN" check --staged)
check "check --staged: unstaged files ignored" \
  "echo \"\$CHECK\" | grep -q '^nonna: no changed function'"
(cd "$REPO" && git add -A)
(cd "$REPO" && "$ABS_BIN" check --staged --strict >/dev/null)
check "check --staged --strict: exit 1 on a finding" "[ $? = 1 ]"
(cd "$REPO" && git -c user.name=t -c user.email=t@t commit -qm draft &&
  sed -i.bak 's/acc += item;/acc += item; \/\/ sum/' src/draft.rs && rm src/draft.rs.bak)
CHECK=$(cd "$REPO" && "$ABS_BIN" check)
check "check: a dupe from before the change stays quiet" \
  "echo \"\$CHECK\" | grep -q '^nonna: no changed function resembles existing code (1 checked)'"
rm -rf "$REPO"

exit $fail
