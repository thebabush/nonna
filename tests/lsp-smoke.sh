#!/usr/bin/env bash
# LSP protocol smoke test: initialize against a copy of tests/fixtures, didOpen
# draft.rs, expect a publishDiagnostics mentioning `mean`. Run from the repo
# root. The copy matters: diagnostics skip test code, and anything under a
# tests/ directory counts as test code — so the corpus is staged outside it.
set -u
BIN=_build/default/nonna/cli/main.exe
STAGE=$(mktemp -d)
trap 'rm -rf "$STAGE"' EXIT
cp -R tests/fixtures "$STAGE/fixtures"
FIX="$STAGE/fixtures"
OPEN=$(node -e 'const fs = require("fs"); const {pathToFileURL} = require("url"); const p = process.argv[1]; console.log(JSON.stringify({jsonrpc:"2.0",method:"textDocument/didOpen",params:{textDocument:{uri:pathToFileURL(p).href,languageId:"rust",version:1,text:fs.readFileSync(p,"utf8")}}}));' "$FIX/draft.rs")

msg() { printf 'Content-Length: %d\r\n\r\n%s' "${#1}" "$1"; }

OUT=$( {
  msg "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"rootUri\":\"file://$FIX\",\"capabilities\":{}}}"
  msg '{"jsonrpc":"2.0","method":"initialized","params":{}}'
  sleep 2 # indexing is async; give the fixtures corpus time to land
  msg "$OPEN"
  msg "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"nonna/findSimilar\",\"params\":{\"textDocument\":{\"uri\":\"file://$FIX/draft.rs\"},\"position\":{\"line\":5,\"character\":0}}}"
  msg "{\"jsonrpc\":\"2.0\",\"id\":4,\"method\":\"nonna/functionText\",\"params\":{\"textDocument\":{\"uri\":\"file://$FIX/draft.rs\"},\"position\":{\"line\":5,\"character\":0}}}"
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
