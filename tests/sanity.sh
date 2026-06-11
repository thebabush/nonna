#!/usr/bin/env bash
# Sanity dataset checks. Run from the repo root:  bash tests/sanity.sh
#
# Dataset: tests/sanity/{rust,python,javascript,go}/{a,b}.*
#   a.* = originals (mean, longest, count_char, clamp_all) + distractors
#         (join_with, fib)
#   b.* = renamed/reordered clones, a containment subset (floor_all),
#         and an algorithmic twin (tally = count_char as while+index)
#
# HARD assertions (the design contract, per language):
#   1. renamed mean    -> rank-1 match of mean,    jaccard >= 0.9
#   2. renamed longest -> rank-1 match of longest, jaccard >= 0.9
#   3. floor_all       -> containment in clamp_all >= 0.85
#   4. no wrong-group pair scores jaccard >= 0.6
# INFORMATIONAL (measured, not asserted): the while+index twin, and
# cross-language matching of the same algorithms.
set -u
BIN=_build/default/nonna/cli/main.exe
fail=0
ok()  { echo "ok   $1"; }
bad() { echo "FAIL $1"; fail=1; }

section() { # $1=query-output $2=draft-fn-name -> that draft's hit lines
  printf '%s\n' "$1" | awk -v n="$2" \
    '$0 ~ ("^── " n " ") {f=1; next} /^── / {f=0} f && /jaccard/'
}

ge() { awk -v x="${1:-0}" -v y="$2" 'BEGIN{exit !(x+0>=y)}'; }

check_lang() { # lang mean ravg longest pwide clamp floor count tally
  local L=$1 MEAN=$2 RAVG=$3 LONG=$4 PWIDE=$5 CLAMP=$6 FLOOR=$7 COUNT=$8 TALLY=$9
  local A B OUT line name j c
  A=$(ls tests/sanity/"$L"/a.*) || return
  B=$(ls tests/sanity/"$L"/b.*) || return
  OUT=$("$BIN" query "$A" -- "$B" -t 0.1 -k 5 2>/dev/null)

  line=$(section "$OUT" "$RAVG" | head -1)
  name=$(echo "$line" | awk '{print $5}'); j=$(echo "$line" | awk '{print $2}')
  if [ "$name" = "$MEAN" ] && ge "$j" 0.9; then
    ok "$L: $RAVG -> $MEAN (jaccard $j)"
  else
    bad "$L: $RAVG expected rank-1 $MEAN with j>=0.9, got '${name:-none}' j=${j:-0}"
  fi

  line=$(section "$OUT" "$PWIDE" | head -1)
  name=$(echo "$line" | awk '{print $5}'); j=$(echo "$line" | awk '{print $2}')
  if [ "$name" = "$LONG" ] && ge "$j" 0.9; then
    ok "$L: $PWIDE -> $LONG (jaccard $j)"
  else
    bad "$L: $PWIDE expected rank-1 $LONG with j>=0.9, got '${name:-none}' j=${j:-0}"
  fi

  c=$(section "$OUT" "$FLOOR" | awk -v t="$CLAMP" '$5==t {print $4; exit}')
  if ge "${c:-0}" 0.85; then
    ok "$L: $FLOOR in $CLAMP (containment ${c})"
  else
    bad "$L: $FLOOR expected containment >=0.85 in $CLAMP, got '${c:-none}'"
  fi

  local badpairs
  badpairs=$(printf '%s\n' "$OUT" | awk \
    -v ok1="$RAVG:$MEAN" -v ok2="$PWIDE:$LONG" -v ok3="$FLOOR:$CLAMP" \
    -v ok4="$TALLY:$COUNT" '
      /^── / {d=$2}
      /jaccard/ {
        pair = d ":" $5
        if ($2+0 >= 0.6 && pair!=ok1 && pair!=ok2 && pair!=ok3 && pair!=ok4)
          print "    " pair " jaccard " $2
      }')
  if [ -z "$badpairs" ]; then
    ok "$L: no wrong-group pair >= 0.6"
  else
    bad "$L: wrong-group pairs above 0.6:"
    echo "$badpairs"
  fi

  line=$(section "$OUT" "$TALLY" | head -1)
  echo "info $L: while+index twin $TALLY -> ${line:-  (no match at 0.1)}"
}

echo "── within-language invariants"
#           mean  ravg            longest  pwide       clamp     floor     count      tally
check_lang rust       mean running_average longest pick_widest clamp_all floor_all count_char tally
check_lang python     mean running_average longest pick_widest clamp_all floor_all count_char tally
check_lang javascript mean runningAverage  longest pickWidest  clampAll  floorAll  countChar  tally
check_lang go         mean runningAverage  longest pickWidest  clampAll  floorAll  countChar  tally

echo
echo "── cross-language (informational: same algorithm, rust corpus)"
for L in python javascript go; do
  B=$(ls tests/sanity/"$L"/a.*)
  "$BIN" query tests/sanity/rust/a.rs -- "$B" -t 0.02 -k 1 2>/dev/null \
    | awk -v l="$L" '
        /^── / {d=$2}
        /jaccard/ {printf "  %-10s %-12s -> %s (j %s, c %s)\n", l, d, $5, $2, $4; found[d]=1}
        /no similar/ {printf "  %-10s %-12s -> (none)\n", l, d}'
done

echo
exit $fail
