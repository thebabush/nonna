(* In-memory search engine: inverted index (feature hash -> posting list)
   for EXACT candidate generation, then exact weighted Jaccard + containment
   re-ranking. Replaced banded-MinHash LSH (measured: 15-22% candidate-miss
   on evolved pairs at our corpus scale; an inverted index finds every
   target sharing >= 1 generative feature, and scoring one candidate is ~1us
   so sub-linear approximation buys nothing below ~10^6 units).

   Ubiquitous features (document frequency above a cutoff) are skipped during
   candidate GENERATION only — they still contribute to scoring. (Score-time
   IDF was benchmarked: ALL-MRR wash, both profiles; the code was dropped.) *)

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
  containment : float; (* of the query in the target *)
}

type t = {
  postings : (int, int Dynarray.t) Hashtbl.t; (* feature -> fids *)
  mutable sigs : (Signature.t * meta) array;
  mutable n : int;
}

let create () = { postings = Hashtbl.create 4096; sigs = [||]; n = 0 }

let add (t : t) (meta : meta) (sg : Signature.t) : int =
  let fid = t.n in
  if fid >= Array.length t.sigs then (
    let cap = max 64 (2 * Array.length t.sigs) in
    let bigger = Array.make cap (sg, meta) in
    Array.blit t.sigs 0 bigger 0 t.n;
    t.sigs <- bigger);
  t.sigs.(fid) <- (sg, meta);
  t.n <- t.n + 1;
  Array.iter
    (fun h ->
      match Hashtbl.find_opt t.postings h with
      | Some d -> Dynarray.add_last d fid
      | None ->
          let d = Dynarray.create () in
          Dynarray.add_last d fid;
          Hashtbl.add t.postings h d)
    sg.Signature.raw;
  fid

let size (t : t) = t.n

let get_meta (t : t) (fid : int) : meta =
  let _, m = t.sigs.(fid) in
  m

(* Features with df above this don't generate candidates (they still score).
   Scales with corpus size; never bites on small corpora. *)
let df_cutoff (t : t) : int = max 100 (t.n / 50)

(* Always probe at least this many of the query's RAREST features, even when
   they exceed the cutoff: a common-shaped function can have every feature
   above the cutoff (measured: 10% of exact dupes went unfindable), and its
   rarest features are the cheapest postings to scan anyway. *)
let probe_rarest = 8

let candidates (t : t) (sg : Signature.t) : int list =
  let cutoff = df_cutoff t in
  let seen = Hashtbl.create 256 in
  let add d = Dynarray.iter (fun fid -> Hashtbl.replace seen fid ()) d in
  let by_df =
    sg.Signature.raw |> Array.to_list
    |> List.filter_map (fun h ->
           match Hashtbl.find_opt t.postings h with
           | Some d -> Some (Dynarray.length d, d)
           | None -> None)
    |> List.sort (fun (a, _) (b, _) -> compare a b)
  in
  List.iteri
    (fun i (df, d) -> if i < probe_rarest || df <= cutoff then add d)
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

(* All intra-index pairs above the threshold ("nonna dupes"). *)
let duplicates (t : t) ~(threshold : float) : (meta * meta * float) list =
  let seen = Hashtbl.create 256 in
  let out = ref [] in
  for fid = 0 to t.n - 1 do
    let sg, meta = t.sigs.(fid) in
    candidates t sg
    |> List.iter (fun cand ->
           if cand <> fid then (
             let key = (min fid cand, max fid cand) in
             if not (Hashtbl.mem seen key) then (
               Hashtbl.replace seen key ();
               let csg, cmeta = t.sigs.(cand) in
               let j = Signature.jaccard sg csg in
               if j >= threshold then out := (meta, cmeta, j) :: !out)))
  done;
  List.sort (fun (_, _, a) (_, _, b) -> compare b a) !out
