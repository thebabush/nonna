#!/usr/bin/env bash
# MCP protocol smoke test (newline-delimited JSON-RPC over stdio).
# Run from the repo root: bash tests/mcp-smoke.sh
set -u
BIN=_build/default/nonna/cli/main.exe
ROOT=$(pwd)/tests/fixtures

OUT=$( {
  echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"smoke"}}}'
  echo '{"jsonrpc":"2.0","method":"notifications/initialized"}'
  sleep 2 # indexing is async
  echo '{"jsonrpc":"2.0","id":2,"method":"tools/list"}'
  echo '{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"status","arguments":{}}}'
  # reuse-before-write: a drafted mean-like fn (renamed vars, single line)
  echo '{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"find_similar","arguments":{"code":"fn my_avg(qs: &[f64]) -> f64 { let mut k = 0usize; let mut s = 0.0; for q in qs { s += q; k += 1; } if k == 0 { 0.0 } else { s / k as f64 } }","language":"rust"}}}'
  # query by location
  echo "{\"jsonrpc\":\"2.0\",\"id\":5,\"method\":\"tools/call\",\"params\":{\"name\":\"query_similar\",\"arguments\":{\"file\":\"$ROOT/draft.rs\",\"name\":\"avg\"}}}"
  # algebra: floor_all (subset) vs clamp_all -> B-A should be the hi-branch
  echo "{\"jsonrpc\":\"2.0\",\"id\":6,\"method\":\"tools/call\",\"params\":{\"name\":\"diff_functions\",\"arguments\":{\"a_file\":\"$ROOT/draft.rs\",\"a_name\":\"floor_all\",\"b_file\":\"$ROOT/corpus/util.rs\",\"b_name\":\"clamp_all\"}}}"
} | "$BIN" mcp "$ROOT" 2>/dev/null )

fail=0
check() { if echo "$OUT" | grep -q "$2"; then echo "ok   $1"; else echo "FAIL $1"; fail=1; fi }

check "initialize"                '"serverInfo"'
check "tools listed"              '"find_similar"'
check "status reports index"      'indexed functions: '
check "drafted fn finds mean"     'similar to drafted `my_avg`'
check "find_similar hit is mean"  '## `mean`'
check "query_similar finds mean"  'jaccard 1.000'
check "diff: intersection scores" 'A ∩ B: jaccard'
check "diff: B-A has the fix"     'B − A'
check "diff: hi-branch unique"    'hi'

# ── HTTP transport (nonna serve) ─────────────────────────────────────────────
PORT=18976
"$BIN" serve "$ROOT" -p $PORT 2>/dev/null &
SRV=$!
trap 'kill $SRV 2>/dev/null' EXIT
sleep 3
HOUT=$(curl -s -X POST "http://127.0.0.1:$PORT/mcp" -H 'Content-Type: application/json' \
  -d '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-03-26","capabilities":{}}}')
echo "$HOUT" | grep -q 'rename-invariant' && echo "ok   http: initialize + instructions" || { echo "FAIL http: initialize"; fail=1; }
HOUT=$(curl -s -X POST "http://127.0.0.1:$PORT/mcp" -H 'Content-Type: application/json' \
  -d '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"status","arguments":{}}}')
echo "$HOUT" | grep -q 'indexed functions' && echo "ok   http: tools/call status" || { echo "FAIL http: tools/call"; fail=1; }
exit $fail
