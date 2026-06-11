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
nonna serve [root] [-p 8976]               # HTTP MCP (shared warm index)

# global flags: --profile structural|full, --iters N, --with ch1,ch2
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
`status`. The server's `instructions` explain score semantics to the agent
(jaccard ≈ 1: same up to renaming; containment ≈ 1: "it does everything yours
does, plus more").

The corpus root is indexed at startup (workspace first, then cargo deps + std
from per-`crate@version` caches under `~/.cache/nonna/sigdb` — first dep index
~1 min, warm loads < 0.5 s). Check `status` before trusting empty results.

## 🧑‍💻 Editor integration (VSCode)

`editor/vscode-nonna/` — diagnostics on open/save ("`avg` is similar to `mean`
(util.rs:1) — jaccard 1.00"), expandable related locations in the Problems
panel, lightbulb actions (function-body diff / open-to-the-side), and a
"nonna: Find Similar Functions" palette command. Setup: see its README;
short version: `npm install` there, symlink the folder into
`~/.vscode/extensions/nonna-dev.nonna-0.0.1`, set `nonna.serverPath`.

## 🧪 Tests & benchmarks

```sh
bash tests/run.sh            # core regressions (fixtures)
bash tests/sanity.sh         # 16 invariant assertions across Rust/Python/JS/Go
bash tests/lsp-smoke.sh      # LSP protocol session
bash tests/mcp-smoke.sh      # MCP stdio + HTTP session (incl. diff algebra)
bash tests/mine-registry.sh  # mine + evaluate against your local cargo registry
bash tests/sweep.sh bench-out/paths.txt   # hyperparameter sweep (MRR/recall@k)
uv run tests/llm_judge.py --pairs ... --scores ...   # LLM-judged pair quality
```

Current headline numbers (Rust benchmark, 56k-unit corpus): ALL MRR 0.963;
renamed clones rank-1 98.7%; version-evolved functions r@10 0.92; candidate
miss ≤ 1.6%. Full tables and caveats in DESIGN.md.

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
