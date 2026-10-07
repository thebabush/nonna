(* In-memory search engine: inverted index (feature hash -> posting list)
   for EXACT candidate generation, then exact weighted Jaccard + containment
   re-ranking. (LSH was measured and removed: 15-22% candidate-miss at our
   scale, and scoring a candidate costs ~1us — sub-linear approximation buys
   nothing below ~10^6 units.)

   Ubiquitous features (document frequency above a cutoff) are skipped during
   candidate GENERATION only — they still contribute to scoring.

   Two-phase by type: [builder] is the mutable, add-only side owned by the
   indexing thread; [t] is an immutable snapshot ([freeze] copies). Queries
   on a partially-built index are unrepresentable, which is the point — the
   explorer once crashed exactly that way (Dynarray iteration raced add). *)

module Signature = Nonna_features.Signature

type meta = {
  name : string;
  file : string;
  line_start : int;
  line_end : int;
  code_lines : int;
  is_test : bool; (* under #[cfg(test)] / #[test], or a test-shaped path *)
}

type hit = {
  meta : meta;
  jaccard : float;
  containment : float;
}

(* Lexical nesting (either direction; true for self). Nested defs are
   indexed as their own units AND grafted into the parent's CFG, so
   parent vs child is a tautological near-dupe — never report it. *)
let nests (a : meta) (b : meta) : bool =
  a.file = b.file
  && ((a.line_start <= b.line_start && b.line_end <= a.line_end)
     || (b.line_start <= a.line_start && a.line_end <= b.line_end))

(* ── builder ─────────────────────────────────────────────────────────────── *)

type builder = {
  b_postings : (int, int Dynarray.t) Hashtbl.t;
  mutable b_sigs : (Signature.t * meta) array;
  mutable b_n : int;
}

let create () = { b_postings = Hashtbl.create 4096; b_sigs = [||]; b_n = 0 }

(* Fids are assigned in insertion order, so every posting list is sorted
   ascending — [candidates] relies on that to bound a scan to an index prefix
   without touching the rest of the list. *)
let add (b : builder) (meta : meta) (sg : Signature.t) : int =
  let fid = b.b_n in
  if fid >= Array.length b.b_sigs then (
    let cap = max 64 (2 * Array.length b.b_sigs) in
    let bigger = Array.make cap (sg, meta) in
    Array.blit b.b_sigs 0 bigger 0 b.b_n;
    b.b_sigs <- bigger);
  b.b_sigs.(fid) <- (sg, meta);
  b.b_n <- b.b_n + 1;
  Array.iter
    (fun h ->
      match Hashtbl.find_opt b.b_postings h with
      | Some d -> Dynarray.add_last d fid
      | None ->
          let d = Dynarray.create () in
          Dynarray.add_last d fid;
          Hashtbl.add b.b_postings h d)
    sg.Signature.raw;
  fid

let built (b : builder) = b.b_n

(* ── frozen snapshots ────────────────────────────────────────────────────── *)

type t = {
  postings : (int, int array) Hashtbl.t;
  sigs : (Signature.t * meta) array;
}

let freeze (b : builder) : t =
  let postings = Hashtbl.create (Hashtbl.length b.b_postings) in
  Hashtbl.iter
    (fun h d -> Hashtbl.add postings h (Dynarray.to_array d))
    b.b_postings;
  { postings; sigs = Array.sub b.b_sigs 0 b.b_n }

let empty : t = { postings = Hashtbl.create 1; sigs = [||] }

(* Rebuild a snapshot with every unit from [file] dropped and the file's
   current units ([fresh]) appended. The LSP calls this on save so a saved
   function stops matching its own now-stale copy — line drift breaks the
   positional self-check in [nests] — and diagnostics reflect the code on disk.
   Rebuilds the whole inverted index (O(indexed units)); fine for interactive,
   one-file-at-a-time saves. *)
let refresh_file (t : t) ~(file : string) (fresh : (meta * Signature.t) list) : t
    =
  let b = create () in
  Array.iter (fun (sg, m) -> if m.file <> file then ignore (add b m sg)) t.sigs;
  List.iter (fun (m, sg) -> ignore (add b m sg)) fresh;
  freeze b

let size (t : t) = Array.length t.sigs

let get_meta (t : t) (fid : int) : meta =
  let _, m = t.sigs.(fid) in
  m

(* Features with df above this don't generate candidates (they still score).
   Scales with the size of the corpus being SEARCHED; never bites on small
   corpora. *)
let df_cutoff_for (n : int) : int = max 100 (n / 50)

(* Always probe at least this many of the query's RAREST features, even when
   they exceed the cutoff: a common-shaped function can have every feature
   above the cutoff (measured: 10% of exact dupes went unfindable), and its
   rarest features are the cheapest postings to scan anyway. *)
let probe_rarest = 8

(* Number of fids below [max_fid] in a sorted posting list: the feature's
   document frequency within the index prefix [0, max_fid). *)
let prefix_len (d : int array) (max_fid : int) : int =
  let lo = ref 0 and hi = ref (Array.length d) in
  while !lo < !hi do
    let mid = (!lo + !hi) / 2 in
    if d.(mid) < max_fid then lo := mid + 1 else hi := mid
  done;
  !lo

(* [max_fid] restricts generation to the index prefix [0, max_fid) — the
   workspace, when deps/std are indexed after it. The restriction has to
   happen HERE, not by filtering the result: the server indexes hundreds of
   thousands of dep functions behind a few thousand workspace ones, and an
   unscoped probe walks every dep posting (and sizes the df cutoff off the
   whole corpus) only to throw the dep fids away. Measured: a workspace-only
   dupe scan over 5.3k fns with 337k dep fns behind them took 120s unscoped.
   Scoped, df and the cutoff are computed over the prefix alone, so the
   result is exactly what a workspace-only index would produce. *)
let candidates ?max_fid (t : t) (sg : Signature.t) : int list =
  let n = size t in
  let bound =
    match max_fid with Some m when m > 0 && m < n -> m | _ -> n
  in
  let cutoff = df_cutoff_for bound in
  let seen = Hashtbl.create 256 in
  let addp (len, d) =
    for i = 0 to len - 1 do
      Hashtbl.replace seen d.(i) ()
    done
  in
  let by_df =
    sg.Signature.raw |> Array.to_list
    |> List.filter_map (fun h ->
           match Hashtbl.find_opt t.postings h with
           | Some d ->
               let len =
                 if bound = n then Array.length d else prefix_len d bound
               in
               if len = 0 then None else Some (len, d)
           | None -> None)
    |> List.sort (fun (a, _) (b, _) -> compare a b)
  in
  List.iteri
    (fun i (df, d) -> if i < probe_rarest || df <= cutoff then addp (df, d))
    by_df;
  Hashtbl.fold (fun fid () acc -> fid :: acc) seen []

(* Case-insensitive substring test (empty needle matches everything). *)
let contains_ci ~(sub : string) (s : string) : bool =
  sub = ""
  ||
  let s = String.lowercase_ascii s and sub = String.lowercase_ascii sub in
  let ls = String.length s and lsub = String.length sub in
  let rec at i = i + lsub <= ls && (String.sub s i lsub = sub || at (i + 1)) in
  at 0

(* A file passes when it matches some include (or there are none) and no
   exclude — substrings, case-insensitive. Shared by the dedup pair filter
   and the single-query match filter. *)
let path_match ~(include_paths : string list) ~(exclude_paths : string list)
    (file : string) : bool =
  (include_paths = []
  || List.exists (fun sub -> contains_ci ~sub file) include_paths)
  && not (List.exists (fun sub -> contains_ci ~sub file) exclude_paths)

(* Post-scan gates for the single-query ranking (find_similar / query_similar),
   mirroring [dup_filter] for the whole-corpus path. All cheap, all AND-ed and
   applied to each candidate MATCH: [q_by_max] picks the gated score (max(j,c)
   vs jaccard); [q_name_sub] matches the match's name (""=off); the paths gate
   its file; [q_min_lines]/[q_min_features] gate the match's own size; [q_scope]
   <=0 ranks the whole corpus, >0 restricts matches to the index prefix
   [0,q_scope) (workspace fns index first, so the workspace count = "my code");
   [q_include_tests] false drops matches flagged as test code. *)
type query_filter = {
  q_by_max : bool;
  q_name_sub : string;
  q_include_paths : string list;
  q_exclude_paths : string list;
  q_min_lines : int;
  q_min_features : int;
  q_scope : int;
  q_include_tests : bool;
}

let query_pass_all =
  {
    q_by_max = true;
    q_name_sub = "";
    q_include_paths = [];
    q_exclude_paths = [];
    q_min_lines = 0;
    q_min_features = 0;
    q_scope = 0;
    q_include_tests = true;
  }

(* Query with an externally-extracted signature. `exclude` skips a fid
   (used to avoid matching a function against itself); `filter` gates the
   matches (defaults to pass-all, preserving the bare-ranking behaviour). *)
let query ?(exclude = -1) ?(accept = fun (_ : meta) -> true)
    ?(filter = query_pass_all) (t : t) (qsig : Signature.t) ~(threshold : float)
    ~(max_results : int) : hit list =
  let smax =
    if filter.q_scope > 0 then min filter.q_scope (size t) else size t
  in
  candidates ~max_fid:smax t qsig
  |> List.filter_map (fun fid ->
         if fid = exclude || fid >= smax then None
         else
           let sg, meta = t.sigs.(fid) in
           if (not (accept meta)) || (meta.is_test && not filter.q_include_tests)
           then None
           else
             let j = Signature.jaccard qsig sg in
             let c = Signature.containment ~query:qsig sg in
             let score = if filter.q_by_max then Float.max j c else j in
             if
               score >= threshold
               && meta.code_lines >= filter.q_min_lines
               && Signature.size sg >= filter.q_min_features
               && contains_ci ~sub:filter.q_name_sub meta.name
               && path_match ~include_paths:filter.q_include_paths
                    ~exclude_paths:filter.q_exclude_paths meta.file
             then Some { meta; jaccard = j; containment = c }
             else None)
  |> List.sort (fun a b ->
         compare
           (Float.max b.jaccard b.containment, b.jaccard)
           (Float.max a.jaccard a.containment, a.jaccard))
  |> List.filteri (fun i _ -> i < max_results)

(* All intra-index pairs with jaccard AND containment (max of both
   directions) — the duplication-explorer feed. Gated on max(j, c). *)
type pair = {
  a : meta;
  b : meta;
  a_features : int;
  b_features : int;
  j : float;
  c : float;
}

(* Post-scan filters for whole-corpus dupe finding ("nonna dupes" / the MCP
   find_duplicates tool / the explorer feed). All cheap, all AND-ed:
   [name_sub] matches EITHER side (""=off); [include_paths]/[exclude_paths] gate
   each file (see [path_ok]); [min_lines]/[min_features] gate the SMALLER side;
   [limit]<=0 = unbounded. [by_max] picks the score the threshold gates on —
   max(j,c) (the "call it instead" signal) or jaccard. [include_tests] false
   drops every pair with a test-code side (the default: measured 91% of all
   pairs on a mid-size Rust workspace sat inside #[cfg(test)] modules). *)
type dup_filter = {
  threshold : float;
  by_max : bool;
  name_sub : string;
  include_paths : string list;
  exclude_paths : string list;
  min_lines : int;
  min_features : int;
  limit : int;
  scope_a : int;
  scope_b : int;
  include_tests : bool;
}

let default_filter =
  {
    threshold = 0.5;
    by_max = true;
    name_sub = "";
    include_paths = [];
    exclude_paths = [];
    min_lines = 0;
    min_features = 0;
    limit = 0;
    scope_a = 0;
    scope_b = 0;
    include_tests = false;
  }

(* A file passes the path scope if it matches some include (or there are none)
   and matches no exclude — substrings, case-insensitive. A pair is kept only
   when BOTH its files pass, so "include crates/foo/" means both sides live
   there and "exclude /tests/" drops any pair touching a test file. *)
let path_ok (flt : dup_filter) (file : string) : bool =
  path_match ~include_paths:flt.include_paths ~exclude_paths:flt.exclude_paths
    file

(* Outcome of a whole-corpus scan. [scanned] is how far the outer (a-side)
   loop got out of [total]; [complete] is false when [should_stop] cut the
   scan short — the pairs found so far are valid, just not exhaustive. *)
type scan = { pairs : pair list; scanned : int; total : int; complete : bool }

let duplicates_scan ?(should_stop = fun () -> false) (t : t)
    (flt : dup_filter) : scan =
  let seen = Hashtbl.create 256 in
  let out = ref [] in
  (* Bound each side of a pair to an index prefix (<=0 = whole corpus). The
     index lays out workspace functions first, then deps/std, so a [scope_a]
     of the workspace count restricts the (expensive) outer loop to "my code"
     — the difference between O(workspace) and O(workspace+deps+std). *)
  let bound n = if n > 0 then min n (size t) else size t in
  let amax = bound flt.scope_a and bmax = bound flt.scope_b in
  (* per-side gates that sink every pair the side is in — checked before any
     candidate generation or scoring *)
  let side_ok (m : meta) =
    (flt.include_tests || not m.is_test) && path_ok flt m.file
  in
  let fid = ref 0 in
  let stopped = ref false in
  while !fid < amax && not !stopped do
    if should_stop () then stopped := true
    else (
      let a = !fid in
      let sg, meta = t.sigs.(a) in
      (if side_ok meta then
         candidates ~max_fid:bmax t sg
         |> List.iter (fun cand ->
                if cand <> a && cand < bmax then (
                  let key = (min a cand, max a cand) in
                  if not (Hashtbl.mem seen key) then (
                    Hashtbl.replace seen key ();
                    let csg, cmeta = t.sigs.(cand) in
                    if (not (nests meta cmeta)) && side_ok cmeta then
                      let af = Signature.size sg and bf = Signature.size csg in
                      if
                        min meta.code_lines cmeta.code_lines >= flt.min_lines
                        && min af bf >= flt.min_features
                        && (contains_ci ~sub:flt.name_sub meta.name
                           || contains_ci ~sub:flt.name_sub cmeta.name)
                      then
                        let j = Signature.jaccard sg csg in
                        let c =
                          Float.max
                            (Signature.containment ~query:sg csg)
                            (Signature.containment ~query:csg sg)
                        in
                        let score = if flt.by_max then Float.max j c else j in
                        if score >= flt.threshold then
                          out :=
                            {
                              a = meta;
                              b = cmeta;
                              a_features = af;
                              b_features = bf;
                              j;
                              c;
                            }
                            :: !out))));
      incr fid)
  done;
  let sorted = List.sort (fun (p : pair) (q : pair) -> compare q.j p.j) !out in
  let pairs =
    if flt.limit > 0 then List.filteri (fun i _ -> i < flt.limit) sorted
    else sorted
  in
  { pairs; scanned = !fid; total = amax; complete = not !stopped }

let duplicates_filtered (t : t) (flt : dup_filter) : pair list =
  (duplicates_scan t flt).pairs

(* The explorer feed: max(j,c)-gated, unfiltered, all pairs. *)
let duplicates_full (t : t) ~(threshold : float) : pair list =
  duplicates_filtered t { default_filter with threshold }
