<div align="center">

# 👵🏻

</div>

> Stop your agent from reinventing the standard library.

**nonna** is a structural code-similarity engine for coding agents, built on
[Opengrep](https://github.com/opengrep/opengrep)'s IL. It hashes each function's
resolved control/data-flow graph into a feature set and scores pairs with
weighted Jaccard and asymmetric containment. Matching is rename-invariant:
variable names, field names, literals and concrete types are ignored by
default — two functions match if they compute the same way.

The primary loop is **reuse-before-write**: an agent drafts a function, asks
`find_similar`, and learns "this already exists in dep X — call it instead",
across the workspace, all transitive cargo deps, and std.

Design, decisions and evaluation: [DESIGN.md](DESIGN.md).
Looking for v1 (the Python slop detector with the WASM demo)?
→ [nonna-v1](https://github.com/thebabush/nonna-v1).

## 🏗️ Build

[![ci](https://github.com/thebabush/nonna/actions/workflows/ci.yml/badge.svg)](https://github.com/thebabush/nonna/actions/workflows/ci.yml)

Linux and macOS (on Windows, use WSL — the engine's build is Unix-only).
Requires an OCaml 5.3.0 opam switch and bash ≥ 5. From the repo root:

```sh
git submodule update --init --recursive   # vendor/opengrep + its grammars
export PATH=/opt/homebrew/bin:$PATH
eval $(opam env --switch=opengrep-5.3.0 --set-switch)
. vendor/opengrep/libs/ocaml-tree-sitter-core/tree-sitter-config.sh
dune build nonna
```

Binary: `_build/default/nonna/cli/main.exe` (aliased as `nonna` below).
First build compiles the vendored engine (one-time). Gotcha roundup: the
tree-sitter env script is mandatory (C stubs fail without it), and any binary
embedding the parser must call `Parsing_init.init ()` (ours do).

## 🔪 CLI

```sh
nonna query <corpus...> -- <draft.rs>      # find fns similar to each fn in draft
nonna dupes <dir> [-t 0.5]                 # intra-corpus clone pairs
#   filters: --metric max|jaccard, --name SUB, --include/--exclude SUB,..,
#            --min-lines/--min-features N, -n LIMIT
nonna features <file>                      # debug: per-fn feature dump
nonna graph <file> --fn NAME [-o DIR]      # DOT per propagation round (+ source)
nonna dump-il <file> [--fn NAME]           # compact IL CFG
nonna corpus <root>                        # cargo deps + std discovery / cache

# benchmarking
nonna mine <paths...> [-o pairs.tsv]       # mine ground-truth pairs (token-level)
nonna eval <pairs.tsv> [-o scores.tsv]     # pairwise recall/FPR
nonna rank <pairs.tsv> <corpus...>         # MRR / recall@k through the pipeline

# servers
nonna lsp                                  # stdio LSP (diagnostics, find-similar)
nonna mcp [root]                           # stdio MCP (per-session)
nonna serve [root] [-p 8976]               # HTTP MCP + duplication explorer UI

# global flags: --profile structural|full, --literals, --iters N, --with ch1,ch2
#   --literals: also hash literal values (like BSim's constant option), so
#   match tables / wrappers that differ only in constants stop scoring 1.0
# dupes: test code (#[cfg(test)], #[test], test files/dirs) is skipped
#   unless --tests; --min-lines/--min-features drop one-line closures
```

## 🤖 Agent integration (MCP)

```sh
# per-session (cold index each start):
claude mcp add nonna -- /path/to/main.exe mcp /path/to/workspace
# or: one warm daemon shared by all sessions
/path/to/main.exe serve /path/to/workspace &
claude mcp add --transport http nonna http://127.0.0.1:8976/mcp
```

Tools: `find_similar` (drafted code → ranked existing fns with source),
`query_similar` (file + line|name), `diff_functions` (A∩B scores + per-side
unique regions by source line — for a bug/fix pair, A−B ≈ bug, B−A ≈ fix),
`find_duplicates` (workspace clone pairs, no query fn — the dedup/refactor
view, with threshold/metric/name/include-exclude-path/size filters and a
`time_budget_s` that returns partial results instead of hanging), `status`.
Every tool skips test code unless `include_tests` is set. Long calls honour
MCP `notifications/cancelled`. The server's `instructions` explain score
semantics to the agent (jaccard ≈ 1: same up to renaming; containment ≈ 1:
"it does everything yours does, plus more").

Create a `.nonna` file at the workspace root to enable MCP indexing. It may
also hold per-workspace config, one `key = value` per line:

```
literals = true           # hash literal values too (default false; see --literals)
profile = full            # structural (default) | full
exclude = vendor/, bench/ # path substrings to leave out of the workspace index
include = crates/, src/   # or: only index files whose path contains one of these
```

The
corpus root is indexed at startup (workspace plus cargo deps + std
from per-`crate@version` caches under `~/.cache/nonna/sigdb` — first dep index
~1 min, warm loads < 0.5 s). Check `status` before trusting empty results.
Subsequent tool and explorer requests check workspace and local dependency
contents and refresh the index when files change. Registry/Git dependency
caches are separated by resolved source and source directory.

`nonna serve` also hosts the **duplication explorer** at `http://127.0.0.1:8976/`:
all similar pairs in the corpus with a threshold slider, name/file/min-size
filters, and a keyboard-navigable side-by-side function diff — both a dedup
navigator and a way to eyeball how the engine scores real code.

## 🧑‍💻 Editor integration (VSCode)

`editor/vscode-nonna/` — diagnostics on open/edit/save ("`avg` is similar to `mean`
(util.rs:1) — jaccard 1.00"), expandable related locations in the Problems
panel, lightbulb actions (function-body diff / open-to-the-side), and a
"nonna: Find Similar Functions" palette command. Setup: see its README;
short version: `npm install` there, symlink the folder into
`~/.vscode/extensions/nonna-dev.nonna-0.0.1`, set `nonna.serverPath`.
Diagnostics and related locations use current unsaved buffers and refresh
across open documents. Reindexing preserves edits received during the scan.

## 🧪 Tests & benchmarks

```sh
bash tests/run.sh            # core regressions (fixtures)
bash tests/sanity.sh         # 16 invariant assertions across Rust/Python/JS/Go
bash tests/lsp-smoke.sh      # LSP protocol session
bash tests/mcp-smoke.sh      # MCP stdio + HTTP session (incl. diff algebra)
node tests/regressions.js   # live indexes, buffer positions, dependency caches (Node 22+)
bash tests/mine-registry.sh  # mine + evaluate against your local cargo registry
bash tests/sweep.sh bench-out/paths.txt   # hyperparameter sweep (MRR/recall@k)
uv run tests/llm_judge.py --pairs ... --scores ...   # LLM-judged pair quality
```

Current headline numbers (rank MRR through the full pipeline; defaults are
tuned per language and need no flags):

| benchmark                          | ALL   | exact | renamed | evolved MRR / r@5 |
|------------------------------------|-------|-------|---------|-------------------|
| Rust (crates, 56k units)           | 0.963 | 1.000 | 0.987   | 0.766 / 0.859     |
| Python (PyPI, 17k units)           | 0.970 | 1.000 | 0.986   | 0.950 / 0.986     |
| C (Linux v6.10↔v6.16, 103k units)  | 0.918 | 1.000 | 1.000   | 0.888 / 0.944     |

"evolved" = the same function edited between two real releases — the hard
kind. Full tables, per-language configs, and caveats in DESIGN.md.

## 📜 License

nonna's code is [MIT](LICENSE). The vendored engine (`vendor/opengrep`, a git
submodule — not distributed by this repo) is LGPL-2.1; binaries built from this
repo statically link it, and LGPL §6 is satisfied by this repository's public
source and build instructions (you can relink against a modified engine).
Note: some engine source headers mention a linking exception that the engine's
LICENSE file does not actually contain — we conservatively assume plain
LGPL-2.1.

## 🍝 Disclaimer

Vibecoded with love.
