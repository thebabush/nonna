# nonna v2 — Design

*Living document. Started 2026-06-09; substrate pivoted to Opengrep and engine
built through MCP v0 on 2026-06-10/11. Build/usage instructions: see README.md.*

## What this is

v1 (`../nonna`) is a multi-language, tree-sitter, batch "code slop detector" with a
human-facing WASM demo (k-hop dataflow feature hashing, MinHash+LSH,
weighted Jaccard — Python adapter only, ~10K LOC Rust). v2 is a **different product
on a borrowed substrate**:

- **Agentic-first.** The user is an LLM coding agent. Success = agents write less
  duplicate code, refactor more correctly, and reuse existing implementations
  (workspace + deps + std) instead of reinventing them.
- **Built on Opengrep's engine** (not a from-scratch IR). We borrow the hard part —
  parsing → generic AST → **IL** (a CFG-based, name-resolved, normalized intermediate
  language) — and build similarity/index/agent layers on top, in OCaml, in-tree.
- **Rust-focused, not Rust-locked.** Opengrep parses ~30 languages into one IL;
  the within-language invariants hold in every language we test (Rust, Python,
  JavaScript, Go), but tuning is Rust-first (N8).
- **Incremental.** Dep/std signatures are cached per `crate@version` (immutable);
  only workspace code is re-extracted.
- **MCP first, LSP second** — both thin frontends over one engine.

### Core agent loops

1. **Reuse-before-write** *(primary, shipped)*: agent drafts a fn → `find_similar`
   over workspace+deps+std → "this exists in dep X, call it instead."
2. **Dedup / refactor** *(CLI `dupes` shipped; `find_cluster` later)*.
3. **Diff algebra** *(shipped as `diff_functions`)*: A∩B scores + per-side unique
   regions localized to source lines (bug/fix localization: A−B ≈ bug, B−A ≈ fix).
4. **Quality gate** *(LSP diagnostics shipped)*: flag newly-written near-duplicates.
5. *(stretch)* vuln/taint via Opengrep's open interproc taint. De-prioritized.

## Why Opengrep (the substrate pivot)

We evaluated `syn` (Rust-native, syntactic-only) and chose against it. Opengrep's
**IL** is almost exactly the IR we would have spent months building:

- **3-address normalized op stream**: side-effect-free `exp` / `instr` split; calls
  and assigns lifted out of expressions (`src/il/IL.ml`).
- **Real CFG**: `Enter/Exit/TrueNode/FalseNode/Join/NInstr/NCond/...`; `for`/`while`/
  `foreach` unified into one `Loop`; `match` → `If` chains; `?` → `Elvis`.
- **Name resolution** via `sid` (gensym-unique; shadowing/scoping pre-resolved) —
  the rename-invariance substrate, which `syn` could never give us.
- **Const propagation** and **source maps** (`eorig`/`iorig`) built in.
- **Mature Rust frontend** (~3,700 lines, zero `OtherExpr` catch-alls).
- **License**: engine is LGPL 2.1; Opengrep over Semgrep CE for consortium
  governance (no rug-pull) and interproc taint staying open.

Accepted costs: no SSA (we add reaching-defs + φ ourselves — see D12), no
*inferred* types (declared types are in the IL), internal/unstable APIs (vendored
at a pinned fork point, as a git submodule).

Known substrate caveats (measured):
- printf-style macros (`format!`) lower to `FixmeExp` — name + token bag survive,
  internal dataflow doesn't. Low rate in practice; graceful degradation.
- `else if`-as-expression also produces a `FixmeExp` for the result temp (control
  structure itself lowers fine).
- ~18 std files fail to parse (rustc 1.93 syntax newer than the vendored grammar).

## The pipeline (current)

```
            ┌──────────── Opengrep engine (vendored submodule, LGPL) ──────────┐
 sources ──▶│ tree-sitter → generic AST → Naming_AST → Implicit_return →       │
            │ AST_to_IL → IL.fun_cfg (CFG + sid + source maps)                  │
            └───────────────────────────┬──────────────────────────────────────┘
                                        │ per fn-like unit (incl. lambdas, D6)
                          ┌─────────────▼─────────────┐
                          │ DFG (nonna_features.Dfg)  │  reaching-defs + φ merges,
                          │                           │  closure grafting (N7),
                          │ seed hash per node        │  params by pattern-bound sids
                          │  └ recursive exp-tree fold│
                          │ 1 propagation round       │  (depth swept: 1 optimal)
                          │ emit per round, tagged    │
                          └─────────────┬─────────────┘
                                        │ + CFG-shape features (Structural)
                                        │ + call-surface / str/int/float consts /
                                        │   param-return pattern (Semantic)
                          ┌─────────────▼─────────────┐
                          │ Signature: weighted set   │  channels per D14:
                          │ (dedup max-weight, sorted)│  base = structural
                          └─────────────┬─────────────┘         + call_names
                                        │                        + int_values
                          ┌─────────────▼─────────────┐
                          │ Engine: inverted index    │  exact candidates:
                          │ feature → postings        │  df ≤ max(100, N/50)
                          │ + per-crate sigdb caches  │  ∪ 8 rarest features
                          └─────────────┬─────────────┘
                                        │ weighted Jaccard + containment,
                                        │ ranked by max(j, c)
                ┌──────────────┬────────┴──────┬───────────────┐
              CLI            LSP (stdio)     MCP (stdio)     MCP (HTTP,
        (debug/bench)     + VSCode ext     per-session     shared warm index)
```

Scoring: weighted Jaccard (symmetric) + asymmetric containment ("the target does
everything the query does, plus more" — the D5 'call this instead' signal; measured
to carry most evolved-pair recall). Hashes are FNV-1a-64/splitmix64 (no xxhash
binding in-switch; only collision rate matters), 62-bit non-negative ints.

## Decisions log

- **D1 — Substrate: Opengrep IL** (was `syn`). Borrow parsing→generic AST→IL; build our
  layers on top. OCaml engine, vendored at a pinned fork point (git submodule).
- **D2 — Primary loop: reuse-before-write.** Precision-first: a wrong "this exists" is
  worse than a miss. dedup / `verify_refactor` after; quality-gate / vuln later.
- **D3 — Corpus: workspace + transitive deps + std.** Everything cargo resolves + std
  (needs `rust-src`). Deps immutable → cached globally by `crate@version`. *(shipped)*
- **D4 — Frontend: daemon, MCP-first.** MCP tool surface first; LSP sync/diagnostics
  second. Both thin frontends over the core. *(both shipped; LSP v0 pulled forward
  for visibility — `nonna serve` HTTP daemon is the warm shared-index path)*
- **D5 — Match scoring: Jaccard + containment.** Weighted Jaccard + **asymmetric
  containment**. "Call this" band gates on high sim/containment and (still open, N1)
  `is_public_api`. Subgraph alignment deferred. *(containment measured to carry
  evolved-pair recall: 0.92+ max-gate vs ~0.76 jaccard-only)*
- **D6 — Granularity: uniform flat units.** Index every fn-like item (free fn, method,
  trait default, impl method, closure) as a flat unit; lambdas named
  `parent::<lambda>`. Generics ≈ concrete cousins via normalized type params.
  Trait↔impl modeling deferred (N5). *(+ N7: closures additionally grafted into
  their parent's DFG — grafting adds parent-side signal, replaces nothing)*
- **D7 — cfg/features: index all branches.** No compile; parse + index all `cfg` arms.
- **D8 — IDF: stable background corpus.** *Amended by benchmark:* score-time IDF
  re-weighting is a **wash** (ALL-MRR ±0.001, both profiles); **IDF code dropped**.
  The df table earns its keep as stop-feature candidate generation in the inverted
  index. Re-add only if a benchmark demands it (one `fw` hook in Signature scoring;
  see git history at the "inverted index" commit).
- **D9 — Process model: persisted-index first.** Deps/std from shared global sigdb
  cache (`~/.cache/nonna/sigdb`, keyed `name-version-profile-formatversion`);
  workspace re-extracted per process. Warm file-watching daemon is a later upgrade
  (`nonna serve` is the seed). *(shipped for deps/std)*
- **D10 — Base: Opengrep, not Semgrep CE.** Same engine code today; consortium
  governance (rug-pull immunity) + interproc taint staying open under LGPL.
- **D11 — Implementation: pure OCaml, in-tree** (N6 resolved). *Forced, not chosen:*
  the **IL has no serializer** (only the generic AST is ATD-serializable); consuming
  IL requires in-process OCaml. Consequence: v1's algorithms reimplemented in OCaml
  (v1 is the reference port).
- **D12 — DFG adaptation to the IL.** v1's IR was fully flat; the IL keeps pure
  operator trees nested and puts conditions in CFG nodes. Deviations from a literal
  v1 port: (a) seed hashes **recursively fold the whole side-effect-free exp tree**
  (commutative ops combine child hashes order-insensitively); (b) `NCond`/`NReturn`/
  `NThrow` are **emitting, non-defining** DFG nodes; (c) direct-call callee names are
  not dataflow predecessors unless locally bound (closures); method receivers are;
  (d) field/index **stores** emit but define nothing (v1 parity); (e) def-use binding
  is **flow-sensitive reaching definitions** with **on-demand φ merge nodes** (v1's
  `Op::Phi`: commutative, non-emitting) memoized per def-set — no dominance frontiers.
  `Implicit_return.mark_implicit_return` must run on the AST before `AST_to_IL`
  (else expression-bodied fns — the Rust default — have zero return dataflow).
- **D13 — Scope: single-function matching only.** No call-graph/interprocedural
  matching. Cross-function refactors (helper extraction, test-loop parameterization)
  are **accepted misses**: a fn whose body moved into a callee is a different
  function by the engine's own definition. (Closure grafting is NOT interprocedural —
  one source-level function either way.)
- **D14 — Structural hashing is the baseline; names/values/types are channels.**
  Seed hashes contain no identifier strings, literal values, type descriptors or
  param positions by default — only operator kinds, call kinds, arities,
  offset/composite kinds, control shape. Each signal is a flag (`Dfg.cfg`,
  `--with`), richer profiles via `Signature.profile` (`--profile structural|full`;
  the full profile adds the delta-semantic DFG channel + named call-surface +
  literal-value sets). *Default revised by the rank sweep (user-approved):*
  base = structural + **call_names** + **int_values** — call targets are API
  identity, not naming noise; the no-names verdict stands for variables, fields,
  strings, floats, types, positions and macro text. Propagation depth = **1**
  (swept 0–5: zero loses signal, deeper adds brittle features — our emission is
  per-round cumulative, so shallow depths already carry the context signal).

## Evaluation

Three instruments, in order of authority:

1. **Ground-truth benchmark** (`nonna mine` / `nonna eval` / `nonna rank`;
   `tests/mine-registry.sh` reproduces): pairs mined from multi-version crates in
   the local cargo registry (126 crate-versions → ~18.8k units ≥30 tokens →
   ~15.5k labeled pairs). Labels are TOKEN-level (independent of the feature
   pipeline — non-circular): *exact* (same body tokens), *renamed* (same shape
   after renaming variable-position identifiers; call targets/paths/fields kept —
   ground truth mirrors the claimed invariance), *evolved*/*evolved_major* (same
   crate+file+per-file-unique fn name across versions, body changed; split by
   semver major), *random* negatives (token-jaccard < 0.3, size-matched),
   *samefile* hard negatives. **`rank` is the headline metric** (MRR / recall@k /
   candidate-miss through the real pipeline over the full ~56k-unit corpus);
   `eval` gives pairwise threshold-recall + FPR (AUC was dropped as meaningless
   for the product). `tests/sweep.sh` sweeps knobs (`--iters`, `--with`) on rank.
2. **LLM judge** (`tests/llm_judge.py`, headless `claude -p`): scores sampled
   pairs on purpose/algorithm/structure/dataflow/api/naming/overall; used to
   diagnose *what kind* of gap the engine has and to arbitrate label noise.
   n=298: jaccard tracks structure +0.85 ≈ overall ≈ algorithm ≈ dataflow +0.83 >
   naming +0.77 > purpose +0.69; exactly one false positive in the judged set.
3. **Sanity suite** (`tests/sanity.sh`): same 4 function groups + distractors in
   Rust/Python/JS/Go; 16 hard assertions (renamed clones rank-1 ≥0.9, containment
   ≥0.85, no wrong-group pair ≥0.6). The regression floor for any default change.
   Plus `tests/run.sh` (fixtures), `tests/lsp-smoke.sh`, `tests/mcp-smoke.sh`.

**Current results** (default config: depth 1, base = structural+call_names+int_values):

| rank (56k corpus) | n | MRR | r@1 | r@5 | r@10 | miss |
|---|---|---|---|---|---|---|
| exact | 5690 | 1.000 | 1.000 | 1.000 | 1.000 | 0.000 |
| renamed | 156 | 0.987 | 0.981 | 0.994 | 0.994 | 0.000 |
| evolved | 192 | 0.723 | 0.630 | 0.812 | 0.922 | 0.010 |
| evolved_major | 942 | 0.786 | 0.710 | 0.881 | 0.918 | 0.016 |
| **ALL** | 6980 | **0.963** | | | | |

| pairwise | j≥0.5 | j≥0.7 | max(j,c)≥0.5 |
|---|---|---|---|
| pos exact | 1.000 | 1.000 | 1.000 |
| pos renamed | 0.987 | 0.974 | 1.000 |
| pos evolved | 0.797 | 0.536 | 0.974 |
| pos evolved_major | 0.760 | 0.549 | 0.923 |
| neg random (FPR) | 0.000 | 0.000 | 0.025 |
| neg samefile (FPR) | 0.003 | 0.001 | 0.046 |

Caveats attached to these numbers: the mining corpus is network/parsing-flavored
(float-poor — the "float values useless" ablation is corpus-relative); the tuning
oracle is Rust-only (N8); evolved_major's low tail is genuine rewrites.

### Findings log (chronological, condensed — details in git history)

- Propagation viz (`nonna graph`, DOT per round) caught two real bugs on day one:
  **ParamPattern sids** (all Rust params were dangling — pattern-bound sids now
  map to param nodes) and motivated **reaching-defs + φ** over last-write-wins.
- **Implicit returns** must be marked explicitly; without them, expression-bodied
  fns had zero return dataflow.
- **LSH dropped** for an exact inverted index: banding lost 15–22% of evolved
  targets before scoring; sub-linear approximation buys nothing below ~10⁶ units.
  The df-cutoff needed an "always probe the 8 rarest features" guarantee (10% of
  exact dupes were otherwise unfindable — common-shaped fns have all-hot features).
- **IDF: benchmarked wash, code deleted** (D8).
- **Delta-channel architecture beats base-swapping**: emit rich-config hashes only
  where they differ from the structural base (full-set emission doubles the
  mismatch mass of structural edits).
- **Closure grafting (N7)**: combinator-style fns were thin shells + detached
  lambdas; cross-style drafts went from no-match to top-hit (0.40→0.56 j at
  depth 3; 0.56+ at depth 1).
- **Depth sweep**: 1 round optimal (0 loses signal: evolved MRR −4.4pp; 2+ adds
  brittleness). Metric choice matters: on MRR, `+call_names` is the strongest
  single channel (+6.6pp evolved MRR) though pairwise threshold-recall said the
  opposite — names lower absolute scores but raise relative rank.
- **Record construction is a mono-hash** (one added struct field → j 0.39 on
  otherwise-identical fns): per-field sub-features are a known, unimplemented lever.
- Cross-language matching is weak (idiom lowering differs); loop-style changes
  (foreach ↔ index-while) are an honest cliff. Both measured, neither a goal.

## Module layout (actual)

```
nonna/
  features/  nonna_features   # fhash, il_util, dfg (graph + grafting + φ),
                              # structural, semantic, signature (profiles)
  index/     nonna_index      # engine: inverted index + jaccard/containment rank
  cli/       (executable)     # main (commands/flags), units (enumeration),
                              # bench (mine/eval/rank), viz (DOT), corpus (cargo+std),
                              # sigdb (caches), lsp_server, mcp_server
editor/vscode-nonna/          # minimal LSP client + find-similar + fn-diff
tests/                        # run.sh, sanity{.sh,/}, lsp-smoke.sh, mcp-smoke.sh,
                              # mine-registry.sh, sweep.sh, llm_judge.py, fixtures/
vendor/opengrep               # pinned engine (git submodule @ 8ee180dc)
```

## Phasing

- **Phase 0 — IL spine.** ✅ Opengrep builds from source; IL validated on real Rust.
- **Phase 1 — intra-workspace reuse.** ✅ features → signature → index → `cli query`.
- **Phase 2 — full corpus.** ✅ core: cargo deps + std via cached sigdbs (D3/D9).
  Open: rank by callability/visibility (`is_public_api`) — N1.
- **Phase 3 — MCP.** ✅ v0: `find_similar` / `query_similar` / `diff_functions` /
  `status`, stdio + HTTP transports, LLM-facing `instructions`.
- **Phase 4 — warm + grow.** LSP v0 shipped early (diagnostics + find-similar +
  fn-diff; index is a startup snapshot). Remaining: incremental re-index on save,
  warm shared daemon as the default path, `find_cluster`, `verify_refactor`;
  later vuln via Opengrep's open taint.

## Open questions

- **N1 — Reuse precision.** The "call this" gate: thresholds, containment weighting,
  `is_public_api`/visibility filtering, callability ranking.
- **N5 — Rust-specific granularity.** Trait/impl/generic modeling beyond flat units.
- **N8 — Per-language tuning.** Defaults are tuned on a Rust-only benchmark (D1
  prioritizes Rust anyway). To tune another language: extend the miner's keyword
  list, mine multi-version packages from npm/pypi caches, re-run `tests/sweep.sh`.
  The 4-language sanity suite is the regression floor meanwhile.
- *(resolved: N2 → D7, N6 → D11, N7 → closure grafting)*

## Known levers, deliberately not pulled yet

- Per-field record-construction features (the j=0.39 one-field cliff).
- Dual-profile indexing (structural wins renamed, full wins evolved — complementary).
- Float-value channel on a float-heavy corpus; per-language sweeps (N8).
- Loop-idiom normalization (index-while ↔ foreach) if cross-style recall matters.
- Incremental index updates (Phase 4).
