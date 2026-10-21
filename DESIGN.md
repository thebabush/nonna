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
- Rust macro arguments that are not single tokens or dotted paths
  (`assert_eq!(f(x), "a")`) lower to `FixmeExp` — name + token bag survive,
  internal dataflow doesn't. *Not* low-rate in test code: assert-only tests
  collapsed to one signature (455 of 641 distinct test pairs at j = 1.00 in a
  62k-SLOC workspace). `Units.reparse_rust_macro_args` now reparses `()`/`[]`
  token trees as expressions before naming (see the findings log); `{}` macros
  and token trees that don't parse keep the bag.
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
  one source-level function either way.) Duplicated blocks inside otherwise
  different functions are listed (`dupes --blocks`, D16) but scored only
  within each unit's own Tree family: no cross-function scoring. *Amended
  by D17:* call-graph context is a feature *family*, not interprocedural
  matching — a function is still matched as one unit, against other units;
  its callees' and enclosing parent's features only add evidence to its
  own signature. A body moved into a helper is still a different
  function.
- **D14 — Structural hashing is the baseline; names/values/types are channels.**
  Seed hashes contain no identifier strings, literal values, type descriptors or
  param positions by default — only operator kinds, call kinds, arities,
  offset/composite kinds, control shape. Each signal is a flag (`Dfg.cfg`,
  `--with`), richer profiles via `Signature.profile` (`--profile structural|full`;
  the full profile adds the delta-semantic DFG channel + named call-surface +
  literal-value sets). *Default revised by the rank sweep (user-approved):*
  base = structural + **call_names** + **int_values** — call targets are API
  identity, not naming noise; the no-names verdict stands for variables,
  floats, types, positions and macro text. Rust now also enables
  **string_chunks** (version-aware rerun below); fields and strings otherwise
  follow per-language defaults. The initial propagation depth was **1**
  (swept 0–5: zero loses signal, deeper adds brittle features — our emission is
  per-round cumulative, so shallow depths already carry the context signal).
- **D15 — Feature families, fused scoring.** A signature holds one weighted
  feature array per *family* (`Signature.family`); each family is scored as its
  own weighted Jaccard / containment and the scores are fused as
  Σ w_f·J_f / Σ w_f over the families non-empty on either side (containment:
  non-empty in the query). A family empty on one side scores 0; empty on both,
  it is skipped; weight 0 drops it. Weights default in `signature.ml`, override
  as `--weights family.<name>=w`. `raw` (postings, candidate generation) stays
  one sorted array, the union of all families; non-Base families salt their
  hashes with a per-family seed so they can't coincide with Base hashes. Motive:
  ida-similarity scores a function's own features and its context as two
  Jaccards (0.5/0.5), which beats one Jaccard over the union. *(shipped as
  plumbing first: `Base` held every feature, so scores were bit-identical;
  the syntax-subtree family followed as D16, call-graph context as D17)*
- **D16 — Syntax-subtree family + block clones** *(default off pending
  sweep)*. `Syntax_tree` hashes every statement subtree of a function body,
  at every depth, from the generic AST after naming and implicit-return
  marking (the unit's `ubody`): a statement's hash mixes its kind with its
  normalized token stream, each direct child statement standing as one
  token, its own hash — so hashes compose bottom-up. Normalization: local
  variables and parameters (resolved sids, binding sites included) are one
  constant, not first-occurrence numbers, which would be relative to the
  subtree hashed and break bottom-up reuse; call targets keep their name
  (D14), a qualified callee its last segment, a locally bound callee is a
  variable; field/method names, keywords, operators, types and non-local
  names keep their text (as the DFG keeps fields for Python/C); literals
  are their kind; comments carry no tokens. In macro token bags the reparse
  left opaque (`{}` macros, qualified ones like `tracing::info!`), tokens
  spelled like a body variable count as it and quoted/numeric tokens as
  literals. **`Tree` family**: one feature per subtree except the body
  itself (whole functions are Base's job, and the body hash is the one any
  reordering breaks), weight 1 + ln(statements in the subtree): `return x;`
  1, a 15-statement block 3.7. Extracted only when `family.tree` > 0, so
  default signatures and outputs are unchanged and `Sigdb.format_version`
  stays 7 (old caches hold one family array; `family_features` reads a
  missing family as empty; a non-default weight changes the profile tag).
  Sanity with `family.tree=0.5`: renamed pairs stay j = 1.000 in all four
  languages (Tree alone: 1.000), but the containment subset (`floor_all` ⊂
  `clamp_all`) drops to 0.69–0.72 (Tree containment 0.275 alone: dropping an
  `else if` changes every enclosing subtree's hash) and fails ≥ 0.85; all 16
  pass up to `family.tree=0.07`. **`dupes --blocks`**: exact block clones on
  the same hashes, scb-check style with call names kept: compound statements
  (if / loop / switch-match / try / with, bare nested blocks) with ≥ 2
  statements in their largest body (scb's count) and ≥ 3 code lines
  (`--min-stmts`, `--min-lines`); instances deduplicated by span (closures
  nest in their parent), overlapping ones dropped, groups whose instances are
  all whole function bodies dropped (those are function pairs); test code
  and path filters as in `dupes`; sorted by duplicated SLOC (sum minus the
  largest instance). Marghera (62k SLOC, tests included): 39 groups, 429
  duplicated SLOC, incl. the 18-line `if storage.is_durable()` block in
  `src/main.rs`. Of scb's 62 block groups we find 21 exactly and 10 in part;
  the 31 missed are 12 whole-body groups (dropped by rule) and 19 whose
  copies differ in a callee (6), a field name (6) or an enum variant / type
  path (7) — the merges D14 says not to make. A `match` ends a line early
  in the listing: the generic AST keeps no closing brace for it.
  **Weight sweep (2026-10-21), kept off.** Rank = the 300 crate-version,
  2,196-pair corpus; marghera = `dupes -t 0.5 --tests`; scb = the 187
  scb-check function-clone pairs found at max(j,c) ≥ 0.5 / j ≥ 0.9.

  | family.tree | evolved MRR | evolved_major | renamed | ALL | marghera pairs | named j=1 | scb | sanity |
  |---|---|---|---|---|---|---|---|---|
  | 0 (default) | 0.921 | 0.814 | 0.978 | 0.868 | 23,100 | 1,018 | 174 / 50 | 16/16 |
  | 0.07 | 0.925 | 0.814 | 0.973 | 0.868 | 22,288 | 420 | 175 / 44 | 16/16 |
  | 0.2 | 0.931 | 0.814 | 0.948 | 0.867 | 21,260 | 420 | 169 / 38 | 12/16 |
  | 0.35 | 0.924 | 0.810 | 0.931 | 0.860 | 20,390 | 420 | 158 / 34 | 12/16 |
  | 0.5 | 0.920 | 0.806 | 0.926 | 0.856 | 19,924 | 420 | 157 / 34 | 12/16 |

  No weight beats ALL; evolved gains (+0.010 at 0.2) are paid by renamed,
  likely because Tree keeps type and path names the miner's renamed pairs
  change (Base ignores types). The value of D16 is `--blocks`; a Tree
  variant hashing type/path names to their kind is the next thing to try.

- **D17 — Call-graph context family** *(default off pending sweep)*. After
  ida-similarity's one-hop context: thin wrappers and one-line predicates
  look alike structurally, and what tells them apart is what they call and
  where they live. **`Context` family**: the max-weight union of a unit's
  own Base features, its callees' and (for a nested unit) its enclosing
  parent's, salted as a family (D15) so it never coincides with Base;
  scored as its own Jaccard and fused with Base (ida-similarity: two
  Jaccards beat one over the union). Neighbours come from a **second pass
  over the unit set being indexed** (`Units.extract_all` / `signatures`,
  after every unit's own signature exists), so the resolution scope is
  that set: the workspace within the workspace (`dupes`, MCP/LSP workspace
  index, `check`'s disk index), each dep's sigdb within that dep, `rank`
  within each corpus path (one crate-version dir; never across versions).
  *Callees*: each call-target name (`Semantic.call_targets`: named and
  method calls, last path segment, as the call-names channel) resolved to
  the one unit of the set with that name; ambiguous names (`new`, `fmt`,
  `from`, any two methods sharing a name) and self-calls resolve to
  nothing; no second hop (callees2 measured worse there), no callers (they
  help there only behind a fan-in gate). *Parent*: the narrowest named,
  non-lambda unit of the same file whose lines contain the unit — the
  parent a closure's `parent::<lambda>` name (D6) gives; nested named
  closures and nested fns get one too; equal line ranges only make a
  lambda the child. Nested lambdas take the named fn, not the outer
  lambda. Knobs, as `--weights` overrides (no rebuild; part of the sigdb
  weight digest): `family.context` (0 = off, extracted only when > 0),
  `context.callees` / `context.parent` (on at > 0, default on),
  `context.max_callee_features` (callees with a larger own signature are
  skipped; 0 = no gate, the default). `Signature.size` leaves Context out:
  min-feature gates and reported counts are the unit's own. **Single-query
  paths** (`query`'s draft, MCP `find_similar`/`query_similar`/
  `diff_functions`, LSP diagnostics/find-similar and on-change index
  entries, `check`'s changed units) resolve within the query file or
  snippet only, not the index: a workspace unit queried by file carries
  less context than its indexed copy. Default outputs are byte-identical
  (marghera `dupes -t 0.5 --tests` and `--blocks`), `Sigdb.format_version`
  stays 7 (a default signature gains one empty array; old caches read the
  missing family as empty). Fixture (`tests/context`, base 1.0 + context
  0.5): one-body wrappers over dissimilar callees 0.672 → 0.498, over
  renamed-copy callees 0.672 → 0.736; one closure in two dissimilar
  parents 1.000 → 0.772. Marghera (`dupes -t 0.5 --tests -n 1000000`,
  `family.context=0.5`): 23,100 → 12,445 pairs, j ≥ 0.999 9,153 → 1,328,
  same 4,651 units; sanity unchanged (its functions call nothing in-set).
  Cost: the second pass is ~0.3 s of 5.6 s indexing; the full `dupes` run
  goes 17.4 → 36.2 s, almost all in the pair scan (a bigger `raw`, so
  more candidates, and two families scored per pair). Note
  `family.context=0.5` fuses 2:1 with Base; ida-similarity's 0.5/0.5 is
  `family.context=1`.
  **Sweep (2026-10-21)**, same columns as D16's:

  | context | evolved MRR | evolved_major | renamed | ALL | marghera pairs | named j=1 | scb | sanity |
  |---|---|---|---|---|---|---|---|---|
  | off (default) | 0.921 | 0.814 | 0.978 | 0.868 | 23,100 | 1,018 | 174 / 50 | 16/16 |
  | 0.5, parent only | 0.918 | 0.813 | 0.974 | 0.866 | 12,415 | 994 | 174 / 47 | 16/16 |
  | 0.5, callees only | 0.921 | 0.795 | 0.970 | 0.856 | 19,312 | 1,018 | 167 / 56 | 16/16 |
  | 0.25, both | 0.920 | 0.803 | 0.969 | 0.860 | 13,285 | 994 | 175 / 55 | 16/16 |
  | 0.5, both | 0.919 | 0.794 | 0.966 | 0.854 | 12,445 | 994 | 167 / 53 | 16/16 |
  | 1.0, both | 0.915 | 0.774 | 0.964 | 0.841 | 12,462 | 994 | 162 / 66 | 16/16 |

  Callees cost ranking (evolved_major −0.019 at 0.5): across versions a
  function's callees evolve with it. The parent is nearly free on rank (ALL
  −0.002) and removes 11,288 marghera pairs, 99.6% of them with a closure
  side: the same small closure in unrelated parents, the one-line-closure
  noise the workspace was first reported for.

- **D18 — Clone classes** (`dupes --classes`, MCP `find_duplicates`
  `group: true`). Pair lists grow O(n²) in families of near-identical
  functions (accessors, `as_str` tables); scb-check reports groups and a
  duplicated share instead. A class is a connected component (union-find)
  of the pairs that survive the `dupes` filters and score ≥
  `--class-threshold` on the gated metric (default `-t`, so the pair list
  never changes). Single linkage chains (A~B, B~C, A≁C), so each class
  shows its jaccard range over the pairs present and how many of its
  N(N-1)/2 pairs are present; a tighter class threshold splits the chain.
  The representative is the member with the highest mean jaccard to the
  others (absent pairs 0), ties by location. **Duplicated SLOC** is
  `Units.dup_sloc` (sum minus the largest; blocks share it) over the
  members not nested in another member: a closure inside a member folds
  into it (marked `(nested)`). The summary total counts each line once:
  the members a class would delete (all unfolded ones but the largest),
  across classes, with those nested in another deleted member dropped, so
  it can sit below the sum of the per-class figures (marghera -t 0.7: 2,170
  vs 2,191). The denominator is the code lines of the scanned units, nested
  ones folded the same way: the indexed units past the side gates (tests,
  paths), or with `--blocks` every gated unit, indexed or not, since block
  groups come from all of them. With `--blocks` the summary adds the block
  groups' duplicated lines (every instance but the largest, by line) that
  are not inside a function the classes delete, and the union. Marghera
  (`-t 0.7 --min-lines 3`, no tests): 151 classes, 484 functions, 2,170
  duplicated SLOC (8.1% of 26,787), largest class the 25 one-line
  accessors at j 1.000; `--class-threshold 0.9`: 90 classes, 294
  functions, 1,111 SLOC (the 10-member class splits into three of 3 and
  drops one member, the 11-member one keeps 9; the accessors stay
  together, they are identical); `--blocks` adds 132
  of the 137 block-clone lines, 8.6% in all. The MCP summary's total is
  the workspace's; with `include_deps` a class can hold dep functions,
  whose lines then count as duplicated too.

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
   Ranking accepts B's exact location or another version of B, identified by
   crate + relative file path + function name; A itself is excluded. When A
   and B are different functions (renamed/exact pairs), A's own other
   versions are dropped from the ranking: neither hits nor competitors. This identity can conflate same-named methods in different impls;
   it does not establish semantic equivalence. The best accepted hit supplies
   the rank, with ties counted in its favor. Earlier exact-B-only results penalized valid
   cross-version matches and need rerunning before drawing tuning conclusions.
   `rank -o` writes TSV columns: kind, A file/line, B file/line, accepted-hit
   jaccard/containment, rank (0 = no accepted candidate), top-hit file/line/name/
   jaccard/containment, competitor marker, accepted-hit file/line/name.
2. **LLM judge** (`tests/llm_judge.py`, headless `claude -p`): scores sampled
   pairs on purpose/algorithm/structure/dataflow/api/naming/overall; used to
   diagnose *what kind* of gap the engine has and to arbitrate label noise.
   n=298: jaccard tracks structure +0.85 ≈ overall ≈ algorithm ≈ dataflow +0.83 >
   naming +0.77 > purpose +0.69; exactly one false positive in the judged set.
3. **Sanity suite** (`tests/sanity.sh`): same 4 function groups + distractors in
   Rust/Python/JS/Go; 16 hard assertions (renamed clones rank-1 ≥0.9, containment
   ≥0.85, no wrong-group pair ≥0.6). The regression floor for any default change.
   Plus `tests/run.sh` (fixtures), `tests/lsp-smoke.sh`, `tests/mcp-smoke.sh`.

**Corrected Rust string comparison (2026-10-20).** The original analysis
corpus has 300 crate-version paths, 130,746 indexed units, and 2,196 positive
evolved/renamed pairs; all three runs skipped zero pairs. Depth 2 and
`exp_nodes` are held fixed. Whole strings adds `string_values`; chunks adds
`string_chunks` to the old baseline, without enabling `string_values`.

| kind | n | base MRR | whole strings MRR | chunks MRR | base r@1 | whole strings r@1 | chunks r@1 |
|---|---|---|---|---|---|---|---|
| evolved | 667 | 0.924 | 0.930 | 0.925 | 0.889 | 0.900 | 0.892 |
| evolved_major | 1246 | 0.787 | 0.821 | 0.811 | 0.731 | 0.770 | 0.757 |
| renamed | 283 | 0.986 | 0.983 | 0.986 | 0.979 | 0.972 | 0.979 |
| **ALL** | **2196** | **0.854** | **0.875** | **0.869** | | | |

**String chunks are the Rust default (user decision).** Both string options
beat the baseline (the renamed loss is negligible next to the major-version
gain). Whole strings measured slightly higher (ALL 0.875 vs 0.869; on
major-version pairs 20 exclusive top-1 wins versus 3 for chunks), but chunks
were chosen anyway: they keep partial credit when a message or format string
is edited, where a whole-string seed is simply lost. `Sigdb.format_version = 5`
invalidates old dependency signatures.
These numbers rank the best acceptable hit, including
ties, so they improve on the earlier post-hoc check of only the top hit.
The renamed column predates dropping A's other versions: back then they
counted as hits, which credited 13–14 of the 283 renamed pairs per config
(similar in all three, so the comparison stands).
Inputs, per-pair TSVs, summaries, a pre-default-change binary and `rerun.sh`
are saved locally under `bench-out/rust-strings-fixed/` (gitignored).
Two vendored zstd C files failed parsing in all three runs, with no labeled pairs
skipped.

**Historical results** (exact-B-only metric; depth 1,
base = structural+call_names+int_values; not comparable to the corrected table):

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
- **Expression-node graphs (`exp_nodes`) win Rust's evolved kind** (2026-06-11):
  collapsed seeds give infinite hops inside an expression but k hops across
  instructions — one edit kills the whole tree's feature. With sub-expressions
  as DFG nodes (operands as edges, var reads resolving to reaching defs at the
  leaf, per-field record nodes), depth means one thing everywhere and edits
  poison only their k-hop neighborhood. Rust and Python defaults are now
  exp_nodes @ depth 2 — Rust evolved MRR 0.723→0.766 (r@5 0.812→0.859,
  evolved_major 0.778), Python evolved 0.929→0.950 (r@5 0.964→0.986, ALL
  0.969→0.970), exact/renamed flat on both. C measured a tie vs collapsed
  depth-0 (0.917 vs 0.918, better tail) and keeps collapsed — the only
  collapsed language left.
- **Closure grafting (N7)**: combinator-style fns were thin shells + detached
  lambdas; cross-style drafts went from no-match to top-hit (0.40→0.56 j at
  depth 3; 0.56+ at depth 1).
- **Depth sweep**: 1 round optimal (0 loses signal: evolved MRR −4.4pp; 2+ adds
  brittleness). Metric choice matters: on MRR, `+call_names` is the strongest
  single channel (+6.6pp evolved MRR) though pairwise threshold-recall said the
  opposite — names lower absolute scores but raise relative rank.
- **Record construction is a mono-hash** (one added struct field → j 0.39 on
  otherwise-identical fns): per-field sub-features are a known, unimplemented lever.
- **Rust macro arguments reparsed** (2026-10-21; found by comparing `dupes`
  with scb-check's exact AST-hash clones). Opengrep keeps a macro's token tree
  as an opaque ArgMacro bag unless every argument is a single token or path,
  so `assert_eq!(linux_file_token(..), "..")` and `assert_eq!(codeview_path(..),
  "")` hashed identically. A pre-naming AST pass copies each such `()`/`[]`
  token tree, at its own byte offsets, into a blank copy of the file wrapped
  as statements of one synthetic fn, parses that with the Rust parser and
  splices the expressions back in as arguments (positions carry over; parse
  errors drop the region they point at, then bisect). Marghera (`dupes -t 0.5
  --tests`, named fns): j = 1.00 pairs 2,569 → 1,018; nonna-only j ≥ 0.9 pairs
  vs scb 773 → 39. Rank (300 crate-versions, 2,196 pairs): ALL MRR 0.868 →
  0.868; evolved 0.925 → 0.921, evolved_major 0.811 → 0.814 (r@5 0.883 →
  0.898, r@10 0.905 → 0.920), renamed 0.978 flat. Cost: +24% on marghera,
  none measured on a 20-crate registry sample. `Sigdb.format_version = 6`.
- Cross-language matching is weak (idiom lowering differs); loop-style changes
  (foreach ↔ index-while) are an honest cliff. Both measured, neither a goal.

## Module layout (actual)

```
nonna/
  features/  nonna_features   # fhash, il_util, dfg (graph + grafting + φ),
                              # structural, semantic, signature (profiles,
                              # families + fused scoring, D15; context
                              # family, D17), syntax_tree (subtree hashes,
                              # D16)
  index/     nonna_index      # engine: inverted index + jaccard/containment rank
  cli/       (executable)     # main (commands/flags), units (enumeration,
                              # context second pass, D17),
                              # blocks (dupes --blocks, D16),
                              # clones (dupes --classes, D18),
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
- **N8 — Per-language tuning.** Partially answered (2026-06-11): the Python
  benchmark exists (`tests/mine-pypi.sh`, 28 package-versions, 11.4k pairs; the
  miner's token normalizer is language-aware via per-extension keyword tables).
  Baseline with Rust-tuned defaults is already strong (evolved MRR 0.86 vs
  Rust's 0.72 — fewer types means version edits perturb less); judge
  correlations mirror Rust (n=148: structure +0.78, purpose +0.64). **Sweep
  verdict: optimal channels are per-language.** Rust now uses
  `call_names+int_values+string_chunks`. The old “strings cost renamed −5pp”
  verdict used the exact-B-only metric and is superseded by the corrected
  comparison above. Python: those PLUS `field_names`
  — ALL MRR 0.955→0.969, evolved MRR 0.862→0.929, samefile FPR(max-gate)
  0.068→0.036; `ty_descrs` actively hurts (annotation drift breaks even exact
  pairs). Depth 1 optimal/flat in both. Per-language base configs are wired
  into extraction (`Dfg.base_cfg_for : Lang.t -> cfg`).
  C (2026-06-11, Linux kernel v6.10↔v6.16, sparse slice kernel/lib/mm/ext4/
  ipv4/i915, 70k units, 8.1k evolved pairs): parser verdict GO — 1.4% of
  files fail outright, then only 0.2% FixmeExp / 1.5% NTodo; macro calls
  keep their names (only macro ARGS go fixme), so `nonna parse-stats` is the
  standard pre-tuning probe for any new language. Sweep: same winning
  channels as Python (`+field_names+string_values`: ALL 0.905→0.918, evolved
  0.870→0.888), but **depth 0** — propagation is flat on MRR for C and
  doubles candidate misses, so depth is per-language too
  (`Dfg.iters_for`; `--iters` still overrides globally). macro_tokens /
  ty_descrs / param_pos are flat and dent renamed (identifier leakage).
  Judge (n=249, sonnet): structure +0.844 > dataflow +0.810 > algorithm
  +0.804 ≫ purpose +0.650 — same ordering as Rust/Python. Engine misses
  concentrate in 6.10→6.16 idiom migrations (`CLASS(fd)`, `guard(mutex)`,
  accessor introductions): same algorithm, mechanically different IL.
  Remaining (issue #1): JS/Go sweeps. Python-specific: unittest↔pytest
  assert migrations dominate the evolved tail — normalization pass endorsed.
- *(resolved: N2 → D7, N6 → D11, N7 → closure grafting)*

## Known levers, deliberately not pulled yet

- **String chunks: Rust and OCaml, not tuned.** Default for Rust (see the
  corrected comparison above) and OCaml, opt-in elsewhere (`--with
  string_chunks`). OCaml (opengrep `src/`+`libs/`, v1.20.0 vs v1.31.0, 18.8k
  units, 275 evolved pairs, version-aware rank): evolved MRR 0.910 base /
  0.915 whole strings / 0.911 chunks — flat; same-file pairs at max(j,c) ≥ 0.5
  10.1% / 6.7% / 7.7% of 594. Chosen for the false-positive cut at no ranking
  cost, and for consistency with Rust.
  Qualitative analysis of Rust major-version pairs found 20 wins unique to
  whole strings versus 3 unique to chunks. Source inspection shows both SQL
  sibling confusion and lost contextual discrimination: nom's `term` uses
  `"*"`/`"/"`, while `expr` uses `"+"`/`"-"` (these are not shared literals).
  This comparison changes both representation and weight: `string_values`
  enters DFG seeds, while `string_chunks` adds separate whole-string/chunk
  sets. Long strings produce more chunk features, but ranking differences
  alone do not isolate that effect. Of the three major-version chunks wins,
  `execute_migration` recovers an SQL table-name edit; `ascii_class` and
  `emit_end_of_early_data_tls13` already rank first in the baseline.
  Capping weight per string, down-weighting common chunks, and separating
  contextual hashing from chunk overlap are the next experiments: they target
  the sibling confusion that costs chunks against whole strings.
- Per-field record-construction features — delivered on Rust/Python by
  `exp_nodes` (each record field is its own node); C still collapsed
  (measured tie, kept the simpler winner).
- `thru_copies` (measured 2026-06-12, kept OFF): copy-like defs (copy /
  cast / `x = s.field`) made transparent to dataflow so consumers bind
  through to the underlying def. Surgically fixes the kernel
  type-laundering pairs (chacha_permute j@1 0.30→0.99, bpf_dynptr_slice
  0.56→1.00) but aggregate is a wash everywhere: C still prefers depth 0
  (where the poison chain doesn't exist — depth 0 IS copy-transparency,
  applied indiscriminately), Rust/Python exp@2 flat (0.766→0.763 /
  0.950→0.952). The rescued pair class is too thin and the erased
  copy-hop signal pays back the gains.
- Dual-profile indexing (structural wins renamed, full wins evolved — complementary).
- Float-value channel on a float-heavy corpus; per-language sweeps (N8).
- Loop-idiom normalization (index-while ↔ foreach) if cross-style recall matters.
- Incremental index updates (Phase 4).
- Embedding-proposed benchmark pairs: the token miner only proves easy
  relationships (exact/renamed), so positives are biased toward what the
  engine is already good at. Pipeline: code-embedding model proposes
  high-cosine cross-package pairs not already token-labeled → LLM judge
  verifies → new `semantic` benchmark kind (reported separately; "two
  models agreed" epistemics, never mixed with provable kinds). Also audits
  negatives. Embeddings stay out of the ENGINE (candidate miss is already
  ≤1.6%); this is purely a labeling instrument.
