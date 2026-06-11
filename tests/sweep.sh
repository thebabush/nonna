#!/usr/bin/env bash
# Hyperparameter sweep on SEARCH metrics (MRR / recall@k through the real
# pipeline — `nonna rank`):
#   bash tests/sweep.sh <corpus-paths-file> [pairs.tsv]
# corpus-paths-file: one crate dir per line (tests/mine-registry.sh writes one
# to bench-out/paths.txt). Sweeps propagation depth, then single-channel base
# ablations. ~2.5 min per config. Pairwise FPR checks live in `nonna eval`.
set -u
BIN=_build/default/nonna/cli/main.exe
PATHS_FILE=${1:?usage: sweep.sh <corpus-paths-file> [pairs.tsv]}
PAIRS=${2:-bench-out/pairs.tsv}
CORPUS=$(tr '\n' ' ' < "$PATHS_FILE")

row() { # name, extra-args...
  local name=$1; shift
  "$BIN" rank "$PAIRS" $CORPUS "$@" 2>/dev/null | awk -v n="$name" '
    /^evolved  /        { ev_m=$3; ev_5=$5; ev_x=$7 }
    /^evolved_major /   { em_m=$3; em_5=$5; em_x=$7 }
    /^renamed /         { rn_m=$3 }
    /^exact /           { ex_m=$3 }
    /^ALL /             { all=$3 }
    END { printf "%-22s ALL=%-6s exact=%-6s renamed=%-6s | evolved mrr=%-6s r@5=%-6s miss=%-6s | evo_major mrr=%-6s r@5=%-6s miss=%s\n",
                 n, all, ex_m, rn_m, ev_m, ev_5, ev_x, em_m, em_5, em_x }'
}

echo "== propagation depth"
for it in 0 1 2 3; do
  row "iters=$it" --iters "$it"
done

echo
echo "== single-channel base ablations (default depth)"
row "base (none)"
for ch in call_names field_names string_values int_values float_values ty_descrs param_pos macro_tokens; do
  row "+$ch" --with "$ch"
done
