#!/usr/bin/env bash
# MCP protocol smoke test (newline-delimited JSON-RPC over stdio).
# Run from the repo root: bash tests/mcp-smoke.sh
set -u
BIN=_build/default/nonna/cli/main.exe
ROOT=$(pwd)/tests/fixtures

# nonna only indexes a workspace opted in with a `.nonna` marker (opt-in gate).
touch "$ROOT/.nonna"
trap 'rm -f "$ROOT/.nonna"' EXIT

OUT=$( {
  echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"smoke"}}}'
  echo '{"jsonrpc":"2.0","method":"notifications/initialized"}'
  sleep 2 # indexing is async
  echo '{"jsonrpc":"2.0","id":2,"method":"tools/list"}'
  echo '{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"status","arguments":{}}}'
  # reuse-before-write: a drafted mean-like fn (renamed vars, single line).
  # The fixture corpus lives under tests/ and so counts as test code, which
  # single-query tools skip by default: opt in, and check the opt-out hint.
  echo '{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"find_similar","arguments":{"code":"fn my_avg(qs: &[f64]) -> f64 { let mut k = 0usize; let mut s = 0.0; for q in qs { s += q; k += 1; } if k == 0 { 0.0 } else { s / k as f64 } }","language":"rust","include_tests":true}}}'
  echo '{"jsonrpc":"2.0","id":10,"method":"tools/call","params":{"name":"find_similar","arguments":{"code":"fn my_avg(qs: &[f64]) -> f64 { let mut k = 0usize; let mut s = 0.0; for q in qs { s += q; k += 1; } if k == 0 { 0.0 } else { s / k as f64 } }","language":"rust"}}}'
  # query by location
  echo "{\"jsonrpc\":\"2.0\",\"id\":5,\"method\":\"tools/call\",\"params\":{\"name\":\"query_similar\",\"arguments\":{\"file\":\"$ROOT/draft.rs\",\"name\":\"avg\",\"include_tests\":true}}}"
  # algebra: floor_all (subset) vs clamp_all -> B-A should be the hi-branch
  echo "{\"jsonrpc\":\"2.0\",\"id\":6,\"method\":\"tools/call\",\"params\":{\"name\":\"diff_functions\",\"arguments\":{\"a_file\":\"$ROOT/draft.rs\",\"a_name\":\"floor_all\",\"b_file\":\"$ROOT/corpus/util.rs\",\"b_name\":\"clamp_all\"}}}"
  # whole-corpus dupe finding (no query fn), filtered by name. The fixtures
  # live under tests/, so they count as test code: excluded unless asked for.
  echo '{"jsonrpc":"2.0","id":7,"method":"tools/call","params":{"name":"find_duplicates","arguments":{"threshold":0.5,"name":"clamp","include_tests":true}}}'
  echo '{"jsonrpc":"2.0","id":9,"method":"tools/call","params":{"name":"find_duplicates","arguments":{"threshold":0.5,"name":"clamp"}}}'
  # find_similar filters (same dup knobs): min_lines gates the mean match away
  echo '{"jsonrpc":"2.0","id":8,"method":"tools/call","params":{"name":"find_similar","arguments":{"code":"fn my_avg(qs: &[f64]) -> f64 { let mut k = 0usize; let mut s = 0.0; for q in qs { s += q; k += 1; } if k == 0 { 0.0 } else { s / k as f64 } }","language":"rust","min_lines":9999}}}'
} | "$BIN" mcp "$ROOT" 2>/dev/null )

fail=0
check() { if echo "$OUT" | grep -q "$2"; then echo "ok   $1"; else echo "FAIL $1"; fail=1; fi }

check "initialize"                '"serverInfo"'
check "tools listed"              '"find_similar"'
check "status reports index"      'indexed functions: '
check "drafted fn finds mean"     'similar to drafted `my_avg`'
check "find_similar hit is mean"  '## `mean`'
check "find_similar hints at excluded tests" 'test-code functions excluded'
check "query_similar finds mean"  'jaccard 1.000'
check "diff: intersection scores" 'A ∩ B: jaccard'
check "diff: B-A has the fix"     'B − A'
check "diff: hi-branch unique"    'hi'
check "find_duplicates lists pairs" 'duplicate pair'
check "find_duplicates excludes test code by default" 'test code and were excluded'
check "find_similar min_lines gates" 'No similar function found for drafted `my_avg`'
check "status reports literals off" 'literals: off'

# ── .nonna as config: literals = true flips the hashing profile ──────────────
printf '# nonna workspace config\nliterals = true\n' > "$ROOT/.nonna"
LOUT=$( {
  echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"smoke"}}}'
  sleep 2
  echo '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"status","arguments":{}}}'
} | "$BIN" mcp "$ROOT" 2>/dev/null )
echo "$LOUT" | grep -q 'literals: on' && echo "ok   status reports literals on (.nonna config)" || { echo "FAIL status reports literals on (.nonna config)"; fail=1; }
echo "$LOUT" | grep -q 'literals = true' && echo "ok   instructions mention literals config" || { echo "FAIL instructions mention literals config"; fail=1; }
: > "$ROOT/.nonna"

# ── HTTP transport (nonna serve) ─────────────────────────────────────────────
PORT=18976
"$BIN" serve "$ROOT" -p $PORT 2>/dev/null &
SRV=$!
trap 'kill $SRV 2>/dev/null; rm -f "$ROOT/.nonna"' EXIT
sleep 3
HOUT=$(curl -s -X POST "http://127.0.0.1:$PORT/mcp" -H 'Content-Type: application/json' \
  -d '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-03-26","capabilities":{}}}')
echo "$HOUT" | grep -q 'rename-invariant' && echo "ok   http: initialize + instructions" || { echo "FAIL http: initialize"; fail=1; }
HOUT=$(curl -s -X POST "http://127.0.0.1:$PORT/mcp" -H 'Content-Type: application/json' \
  -d '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"status","arguments":{}}}')
echo "$HOUT" | grep -q 'indexed functions' && echo "ok   http: tools/call status" || { echo "FAIL http: tools/call"; fail=1; }
curl -s "http://127.0.0.1:$PORT/" | grep -q 'duplication explorer' && echo "ok   http: explorer UI" || { echo "FAIL http: explorer UI"; fail=1; }
curl -s "http://127.0.0.1:$PORT/api/pairs" | grep -q '"pairs"' && echo "ok   http: explorer pairs API" || { echo "FAIL http: pairs API"; fail=1; }
exit $fail
