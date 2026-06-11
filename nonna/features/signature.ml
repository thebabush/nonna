(* Function signature: the weighted feature set used for similarity.
   Port of v1's signature.rs, plus asymmetric containment (D5). *)

type weighted = { hash : Fhash.t; weight : float }

type t = {
  features : weighted array; (* sorted by hash, deduplicated (max weight) *)
  raw : Fhash.t array; (* sorted hashes, for MinHash *)
}

(* Per-source weights (v1 defaults: dfg 1.0, call surface 0.8, consts 0.5,
   structural 0.3). DFG features are weighted per tag. *)
let dfg_weight (tag : Dfg.tag) : float =
  match tag with
  | Dfg.BinOp | Dfg.UnOp | Dfg.Call | Dfg.Field | Dfg.Index | Dfg.Construct
  | Dfg.Control ->
      1.0
  | Dfg.ConstString | Dfg.ConstOther -> 0.8
  | Dfg.MacroBag -> 0.8

let misc_weight = 0.8
let structural_weight = 0.3

(* D14 — the BASELINE is name-free, value-free structural hashing. Names,
   literal values and types are signal CHANNELS, each with a weight that can
   be zeroed: delta-semantic DFG features (rich hashes where they differ from
   the structural base), the named call-surface set, and the literal-value
   set. `structural` is the default profile; `full` ≈ the pre-D14 engine. *)
type profile = {
  w_delta_sem : float; (* delta semantic-DFG channel (names+values+types) *)
  w_calls : float; (* named call-surface set features *)
  w_strs : float; (* string-literal set features *)
  w_ints : float; (* integer-literal set features *)
  w_floats : float; (* float-literal set features *)
}

let structural_profile =
  { w_delta_sem = 0.; w_calls = 0.; w_strs = 0.; w_ints = 0.; w_floats = 0. }

let full_profile =
  { w_delta_sem = 0.5; w_calls = 0.8; w_strs = 0.5; w_ints = 0.5; w_floats = 0.5 }

(* Set once at CLI startup (--profile). *)
let default_profile : profile ref = ref structural_profile

let extract ?(profile : profile option) (fcfg : IL.fun_cfg) : t =
  let p = match profile with Some p -> p | None -> !default_profile in
  let sem = Semantic.extract_parts fcfg in
  let all : (Fhash.t * float) list =
    (Dfg.extract fcfg
    |> List.map (fun (f : Dfg.feature) -> (f.Dfg.hash, dfg_weight f.Dfg.tag)))
    @ (if p.w_delta_sem > 0. then
         Dfg.extract_delta ~rich:Dfg.semantic_cfg fcfg
         |> List.map (fun (f : Dfg.feature) ->
                (f.Dfg.hash, p.w_delta_sem *. dfg_weight f.Dfg.tag))
       else [])
    @ (if p.w_calls > 0. then
         sem.Semantic.calls |> List.map (fun h -> (h, p.w_calls))
       else [])
    @ (if p.w_strs > 0. then
         sem.Semantic.str_consts |> List.map (fun h -> (h, p.w_strs))
       else [])
    @ (if p.w_ints > 0. then
         sem.Semantic.int_consts |> List.map (fun h -> (h, p.w_ints))
       else [])
    @ (if p.w_floats > 0. then
         sem.Semantic.float_consts |> List.map (fun h -> (h, p.w_floats))
       else [])
    @ (sem.Semantic.misc |> List.map (fun h -> (h, misc_weight)))
    @ (Structural.extract fcfg |> List.map (fun h -> (h, structural_weight)))
  in
  (* dedup: same hash appearing multiple times keeps the max weight *)
  let tbl : (Fhash.t, float) Hashtbl.t = Hashtbl.create 64 in
  List.iter
    (fun (h, w) ->
      match Hashtbl.find_opt tbl h with
      | Some w0 when w0 >= w -> ()
      | _ -> Hashtbl.replace tbl h w)
    all;
  let features =
    Hashtbl.fold (fun h w acc -> { hash = h; weight = w } :: acc) tbl []
    |> List.sort (fun a b -> compare a.hash b.hash)
    |> Array.of_list
  in
  { features; raw = Array.map (fun f -> f.hash) features }

let size (s : t) : int = Array.length s.raw

(* Merge-walk two hash-sorted weighted arrays, folding over the union.
   `f acc weight_in_a weight_in_b` with 0. when absent. *)
let fold_union (f : 'a -> float -> float -> 'a) (init : 'a) (a : t) (b : t) :
    'a =
  let na = Array.length a.features and nb = Array.length b.features in
  let rec go acc i j =
    if i >= na && j >= nb then acc
    else if j >= nb || (i < na && a.features.(i).hash < b.features.(j).hash)
    then go (f acc a.features.(i).weight 0.) (i + 1) j
    else if i >= na || b.features.(j).hash < a.features.(i).hash then
      go (f acc 0. b.features.(j).weight) i (j + 1)
    else go (f acc a.features.(i).weight b.features.(j).weight) (i + 1) (j + 1)
  in
  go init 0 0

(* Weighted Jaccard: sum(min) / sum(max). *)
let jaccard (a : t) (b : t) : float =
  let inter, union =
    fold_union
      (fun (i, u) wa wb -> (i +. Float.min wa wb, u +. Float.max wa wb))
      (0., 0.) a b
  in
  if union = 0. then 0. else inter /. union

(* Asymmetric containment (D5): how much of `query` is covered by `target`?
   1.0 means the target does everything the query does (and possibly more). *)
let containment ~(query : t) (target : t) : float =
  let inter, qsum =
    fold_union
      (fun (i, q) wq wt -> (i +. Float.min wq wt, q +. wq))
      (0., 0.) query target
  in
  if qsum = 0. then 0. else inter /. qsum
