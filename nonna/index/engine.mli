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

val size : t -> int
val get_meta : t -> int -> meta

val query :
  ?exclude:int ->
  t ->
  Nonna_features.Signature.t ->
  threshold:float ->
  max_results:int ->
  hit list

val duplicates : t -> threshold:float -> (meta * meta * float) list

type pair = {
  a : meta;
  b : meta;
  a_features : int; (* signature sizes — "how much evidence" per side *)
  b_features : int;
  j : float; (* weighted jaccard *)
  c : float; (* containment, max of both directions *)
}

val duplicates_full : t -> threshold:float -> pair list
