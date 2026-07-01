(* Search engine over function signatures.

   Two-phase by TYPE (illegal states unrepresentable): a [builder] only
   accepts insertions; a [t] is an immutable snapshot that only answers
   queries. The explorer/MCP "query during indexing" races are compile
   errors under this split — publish snapshots with [freeze], keep the
   builder private to the indexing thread. *)

type meta = {
  name : string;
  file : string;
  line_start : int;
  line_end : int;
  code_lines : int; (* lines carrying code tokens (no comments/docstrings) *)
}

type hit = {
  meta : meta;
  jaccard : float;
  containment : float; (* of the query in the target *)
}

val nests : meta -> meta -> bool
(** Same file and one line range contains the other (either direction).
    A nested def vs its enclosing function is a tautological match — the
    closure graft makes the parent's body mostly the child — so such
    pairs are never reported. Also true for self (a range nests itself). *)

(* ── building (single-threaded, indexing side) ─────────────────────────── *)

type builder

val create : unit -> builder
val add : builder -> meta -> Nonna_features.Signature.t -> int
val built : builder -> int
(** units added so far (progress reporting) *)

(* ── querying (immutable snapshots) ────────────────────────────────────── *)

type t

val empty : t
val freeze : builder -> t
(** Snapshot copy: the builder may keep growing afterwards; the snapshot
    never changes. Safe to share across threads. *)

val refresh_file :
  t -> file:string -> (meta * Nonna_features.Signature.t) list -> t
(** A new snapshot with every unit from [file] dropped and the given current
    units appended. Rebuilds the inverted index (O(indexed units)); used by the
    LSP on save so a saved function stops matching its own drifted copy. *)

val size : t -> int
val get_meta : t -> int -> meta

(* Post-scan gates for the single-query ranking (find_similar / query_similar),
   mirroring [dup_filter]. Applied to each candidate match: [q_by_max] gates the
   threshold on max(j,c) (default) vs jaccard; [q_name_sub] / the path lists gate
   the match's name and file; [q_min_lines]/[q_min_features] gate the match's own
   size; [q_scope]>0 restricts matches to the index prefix [0,q_scope) (workspace
   fns index first → "my code only"), <=0 = whole corpus. *)
type query_filter = {
  q_by_max : bool;
  q_name_sub : string;
  q_include_paths : string list;
  q_exclude_paths : string list;
  q_min_lines : int;
  q_min_features : int;
  q_scope : int;
}

val query :
  ?exclude:int ->
  ?filter:query_filter ->
  t ->
  Nonna_features.Signature.t ->
  threshold:float ->
  max_results:int ->
  hit list

type pair = {
  a : meta;
  b : meta;
  a_features : int; (* signature sizes — "how much evidence" per side *)
  b_features : int;
  j : float; (* weighted jaccard *)
  c : float; (* containment, max of both directions *)
}

(* Whole-corpus dupe finding with filters (CLI [dupes], MCP find_duplicates). *)
type dup_filter = {
  threshold : float; (* min score on the gated metric *)
  by_max : bool; (* gate on max(j,c) (default) vs jaccard only *)
  name_sub : string; (* substring on either side's name ("" = off) *)
  include_paths : string list; (* both files must match one of these (case-insens substr; []=all) *)
  exclude_paths : string list; (* drop pairs where either file matches one of these *)
  min_lines : int; (* min code_lines on the smaller side *)
  min_features : int; (* min signature size on the smaller side *)
  limit : int; (* cap on results sorted by jaccard desc (<=0 = all) *)
  scope_a : int; (* restrict pair's 1st side to index prefix [0,scope_a) (<=0 = all) *)
  scope_b : int; (* restrict pair's 2nd side likewise — workspace fns index first *)
}

val default_filter : dup_filter
val duplicates_filtered : t -> dup_filter -> pair list

val duplicates_full : t -> threshold:float -> pair list
(** Unfiltered max(j,c)-gated feed for the explorer. *)
