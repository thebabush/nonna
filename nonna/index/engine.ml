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

let size (t : t) = Array.length t.sigs

let get_meta (t : t) (fid : int) : meta =
  let _, m = t.sigs.(fid) in
  m

(* Features with df above this don't generate candidates (they still score).
   Scales with corpus size; never bites on small corpora. *)
let df_cutoff (t : t) : int = max 100 (size t / 50)

(* Always probe at least this many of the query's RAREST features, even when
   they exceed the cutoff: a common-shaped function can have every feature
   above the cutoff (measured: 10% of exact dupes went unfindable), and its
   rarest features are the cheapest postings to scan anyway. *)
let probe_rarest = 8

let candidates (t : t) (sg : Signature.t) : int list =
  let cutoff = df_cutoff t in
  let seen = Hashtbl.create 256 in
  let addp d = Array.iter (fun fid -> Hashtbl.replace seen fid ()) d in
  let by_df =
    sg.Signature.raw |> Array.to_list
    |> List.filter_map (fun h ->
           match Hashtbl.find_opt t.postings h with
           | Some d -> Some (Array.length d, d)
           | None -> None)
    |> List.sort (fun (a, _) (b, _) -> compare a b)
  in
  List.iteri
    (fun i (df, d) -> if i < probe_rarest || df <= cutoff then addp d)
    by_df;
  Hashtbl.fold (fun fid () acc -> fid :: acc) seen []

(* Query with an externally-extracted signature. `exclude` skips a fid
   (used to avoid matching a function against itself). *)
let query ?(exclude = -1) (t : t) (qsig : Signature.t) ~(threshold : float)
    ~(max_results : int) : hit list =
  candidates t qsig
  |> List.filter_map (fun fid ->
         if fid = exclude then None
         else
           let sg, meta = t.sigs.(fid) in
           let j = Signature.jaccard qsig sg in
           let c = Signature.containment ~query:qsig sg in
           if Float.max j c >= threshold then
             Some { meta; jaccard = j; containment = c }
           else None)
  |> List.sort (fun a b ->
         compare
           (Float.max b.jaccard b.containment, b.jaccard)
           (Float.max a.jaccard a.containment, a.jaccard))
  |> List.filteri (fun i _ -> i < max_results)

(* All intra-index pairs with jaccard AND containment (max of both
   directions) — the duplication-explorer feed. Gated on max(j, c). *)
let duplicates_full (t : t) ~(threshold : float) :
    (meta * meta * float * float) list =
  let seen = Hashtbl.create 256 in
  let out = ref [] in
  for fid = 0 to size t - 1 do
    let sg, meta = t.sigs.(fid) in
    candidates t sg
    |> List.iter (fun cand ->
           if cand <> fid then (
             let key = (min fid cand, max fid cand) in
             if not (Hashtbl.mem seen key) then (
               Hashtbl.replace seen key ();
               let csg, cmeta = t.sigs.(cand) in
               if not (nests meta cmeta) then
                 let j = Signature.jaccard sg csg in
                 let c =
                   Float.max
                     (Signature.containment ~query:sg csg)
                     (Signature.containment ~query:csg sg)
                 in
                 if Float.max j c >= threshold then
                   out := (meta, cmeta, j, c) :: !out)))
  done;
  List.sort (fun (_, _, a, _) (_, _, b, _) -> compare b a) !out

(* All intra-index pairs above the threshold ("nonna dupes"). *)
let duplicates (t : t) ~(threshold : float) : (meta * meta * float) list =
  let seen = Hashtbl.create 256 in
  let out = ref [] in
  for fid = 0 to size t - 1 do
    let sg, meta = t.sigs.(fid) in
    candidates t sg
    |> List.iter (fun cand ->
           if cand <> fid then (
             let key = (min fid cand, max fid cand) in
             if not (Hashtbl.mem seen key) then (
               Hashtbl.replace seen key ();
               let csg, cmeta = t.sigs.(cand) in
               if not (nests meta cmeta) then
                 let j = Signature.jaccard sg csg in
                 if j >= threshold then out := (meta, cmeta, j) :: !out)))
  done;
  List.sort (fun (_, _, a) (_, _, b) -> compare b a) !out
