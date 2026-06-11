#!/usr/bin/env bash
# LSP protocol smoke test: initialize against tests/fixtures, didOpen draft.rs,
# expect a publishDiagnostics mentioning `mean`. Run from the repo root.
set -u
BIN=_build/default/nonna/cli/main.exe
ROOT=$(pwd)

msg() { printf 'Content-Length: %d\r\n\r\n%s' "${#1}" "$1"; }

OUT=$( {
  msg "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"rootUri\":\"file://$ROOT/tests/fixtures\",\"capabilities\":{}}}"
  msg '{"jsonrpc":"2.0","method":"initialized","params":{}}'
  sleep 2 # indexing is async; give the fixtures corpus time to land
  msg "{\"jsonrpc\":\"2.0\",\"method\":\"textDocument/didOpen\",\"params\":{\"textDocument\":{\"uri\":\"file://$ROOT/tests/fixtures/draft.rs\",\"languageId\":\"rust\",\"version\":1,\"text\":\"\"}}}"
  msg "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"nonna/findSimilar\",\"params\":{\"textDocument\":{\"uri\":\"file://$ROOT/tests/fixtures/draft.rs\"},\"position\":{\"line\":5,\"character\":0}}}"
  msg "{\"jsonrpc\":\"2.0\",\"id\":4,\"method\":\"nonna/functionText\",\"params\":{\"textDocument\":{\"uri\":\"file://$ROOT/tests/fixtures/draft.rs\"},\"position\":{\"line\":5,\"character\":0}}}"
  msg '{"jsonrpc":"2.0","id":2,"method":"shutdown","params":null}'
  msg '{"jsonrpc":"2.0","method":"exit","params":null}'
} | "$BIN" lsp 2>/dev/null )

fail=0
check() { if echo "$OUT" | grep -q "$2"; then echo "ok   $1"; else echo "FAIL $1"; fail=1; fi }

check "initialize reply"            '"serverInfo"'
check "workspace indexed"           'indexed [0-9]* units'
check "diagnostics published"       'publishDiagnostics'
check "avg flagged as dupe of mean" 'similar to `mean`'
check "related locations attached"  'relatedInformation'
check "findSimilar resolves cursor fn"  '"query":"avg"'
check "findSimilar returns mean hit"    '"name":"mean"'
check "functionText returns fn body"    'acc += item'
exit $fail
