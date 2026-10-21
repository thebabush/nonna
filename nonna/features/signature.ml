(* Function signature: the weighted feature set used for similarity.
   Port of v1's signature.rs, plus asymmetric containment (D5). *)

type weighted = { hash : Fhash.t; weight : float }

(* D15 — feature families: groups of features scored as separate weighted
   Jaccards and fused by family weight (ida-similarity style). [Base] holds
   the IL features (DFG, structural, semantic); [Tree] the syntax-subtree
   hashes (D16, Syntax_tree); [Context] the call-graph context (D17): a
   unit's own Base features with its callees' and enclosing parent's,
   added in a second pass over the unit set ([with_context]). *)
type family = Base | Tree | Context

let all_families : family list = [ Base; Tree; Context ]

(* position in [all_families] *)
let family_index (f : family) : int =
  match f with Base -> 0 | Tree -> 1 | Context -> 2

let family_name (f : family) : string =
  match f with Base -> "base" | Tree -> "tree" | Context -> "context"

type t = {
  families : weighted array array;
      (* indexed by [family_index]; each sorted by hash, deduplicated (max
         weight) *)
  raw : Fhash.t array;
      (* sorted union of every family's hashes: the engine's postings and
         candidate generation (D8) *)
}

(* A family past the end of [families] is empty: signatures cached before
   [Tree] or [Context] existed hold fewer arrays, and still read back as
   valid. *)
let family_features (s : t) (f : family) : weighted array =
  let i = family_index f in
  if i < Array.length s.families then s.families.(i) else [||]

(* Per-source weights (v1 defaults: dfg 1.0, call surface 0.8, consts 0.5,
   structural 0.3). DFG features are weighted per tag. *)
let default_dfg_weight (tag : Dfg.tag) : float =
  match tag with
  | Dfg.BinOp | Dfg.UnOp | Dfg.Call | Dfg.Field | Dfg.Index | Dfg.Construct
  | Dfg.Control ->
      1.0
  | Dfg.ConstString | Dfg.ConstOther -> 0.8
  | Dfg.MacroBag -> 0.8

(* Runtime per-tag weight overrides (--weights, for tuning sweeps), keyed by
   Dfg.tag_name. Empty = the defaults above. *)
let weight_overrides : (string, float) Hashtbl.t = Hashtbl.create 8
let set_weight (name : string) (w : float) : unit =
  Hashtbl.replace weight_overrides name w

let dfg_weight (tag : Dfg.tag) : float =
  match Hashtbl.find_opt weight_overrides (Dfg.tag_name tag) with
  | Some w -> w
  | None -> default_dfg_weight tag

let misc_weight = 0.8

(* string_chunks channel: whole strings and their chunks (overridable as
   "string_set" / "string_chunks" via --weights, like the DFG tags) *)
let override (name : string) (default : float) : float =
  Option.value (Hashtbl.find_opt weight_overrides name) ~default

(* Fusion weights per family (D15), overridable as "family.<name>" via
   --weights. A family weighted 0 is left out of the fused score.
   [Tree] and [Context] are off (0.0) pending the rank sweep (D16, D17). *)
let default_family_weight (f : family) : float =
  match f with Base -> 1.0 | Tree -> 0.0 | Context -> 0.0

let family_weight (f : family) : float =
  override ("family." ^ family_name f) (default_family_weight f)

(* D17 gates, overridable through --weights like the weights (ida-similarity's
   onehop_* settings): callees and the enclosing parent each switch on at a
   value > 0; callees whose own signature holds more than
   [context.max_callee_features] features are skipped (0 = no gate). *)
let context_callees () = override "context.callees" 1.0 > 0.
let context_parent () = override "context.parent" 1.0 > 0.

let context_max_callee_features () =
  int_of_float (override "context.max_callee_features" 0.)

let string_set_weight () = override "string_set" 0.5
let string_chunk_weight () = override "string_chunks" 0.2
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

(* Non-Base families remix their hashes with a per-family seed so they cannot
   coincide with Base hashes in [raw] (beyond the usual 62-bit collision
   odds); Base hashes stay as extracted, so postings and caches don't move. *)
let family_seed (f : family) : Fhash.t =
  Fhash.hash_str ("nonna.family." ^ family_name f)

let salt (f : family) (h : Fhash.t) : Fhash.t =
  if f = Base then h else Fhash.mix (family_seed f) h

(* One family's array from its (hash, weight) lists: the same hash
   appearing multiple times keeps the max weight; hashes are salted (non-Base
   families) and sorted. *)
let family_array (f : family) (lists : (Fhash.t * float) list list) :
    weighted array =
  let tbl : (Fhash.t, float) Hashtbl.t = Hashtbl.create 64 in
  List.iter
    (List.iter (fun (h, w) ->
         match Hashtbl.find_opt tbl h with
         | Some w0 when w0 >= w -> ()
         | _ -> Hashtbl.replace tbl h w))
    lists;
  Hashtbl.fold (fun h w acc -> { hash = salt f h; weight = w } :: acc) tbl []
  |> List.sort (fun a b -> compare a.hash b.hash)
  |> Array.of_list

let raw_of (families : weighted array array) : Fhash.t array =
  Array.concat (Array.to_list families)
  |> Array.map (fun f -> f.hash)
  |> Array.to_list |> List.sort_uniq compare |> Array.of_list

(* Build a signature from per-family (hash, weight) lists (a family may
   appear more than once). Within a family the same hash appearing multiple
   times keeps the max weight. *)
let of_families (parts : (family * (Fhash.t * float) list) list) : t =
  let families =
    all_families
    |> List.map (fun f ->
           family_array f
             (List.filter_map
                (fun (f', all) -> if f' = f then Some all else None)
                parts))
    |> Array.of_list
  in
  { families; raw = raw_of families }

(* D17: [s] with its [Context] family set to the union of its own Base
   features and its [neighbours]' (resolved callees, enclosing parent),
   deduplicated with max weight and salted like any non-Base family. Base
   hashes are stored unsalted (see [salt]), so they are read back as
   extracted. Which neighbours count is the caller's job
   (Units.extract_all): only a second pass over the unit set knows them. *)
let with_context (s : t) (neighbours : t list) : t =
  let base (sg : t) =
    Array.to_list (family_features sg Base)
    |> List.map (fun w -> (w.hash, w.weight))
  in
  let ctx = family_array Context (List.map base (s :: neighbours)) in
  let families =
    Array.of_list
      (List.map
         (fun f -> if f = Context then ctx else family_features s f)
         all_families)
  in
  { families; raw = raw_of families }

(* [lang] selects the per-language base channels (Dfg.base_cfg_for). [body]
   is the function's AST body, which the [Tree] family hashes; it is only
   read when that family carries weight, so default runs never pay for it. *)
let extract ?(profile : profile option) ?(lang : Lang.t option)
    ?(body : AST_generic.stmt option) (fcfg : IL.fun_cfg) : t =
  let p = match profile with Some p -> p | None -> !default_profile in
  let fc =
    match lang with Some l -> Dfg.base_cfg_for l | None -> !Dfg.base_cfg
  in
  let iters = Dfg.iters_for lang in
  let sem = Semantic.extract_parts fcfg in
  let all : (Fhash.t * float) list =
    (Dfg.extract ~fc ~iters fcfg
    |> List.map (fun (f : Dfg.feature) -> (f.Dfg.hash, dfg_weight f.Dfg.tag)))
    @ (if p.w_delta_sem > 0. then
         Dfg.extract_delta ~base:fc ~iters ~rich:Dfg.semantic_cfg fcfg
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
    @ (if fc.Dfg.string_chunks then
         (sem.Semantic.str_consts |> List.map (fun h -> (h, string_set_weight ())))
         @ (sem.Semantic.str_chunks
           |> List.map (fun h -> (h, string_chunk_weight ())))
       else [])
    @ (sem.Semantic.misc |> List.map (fun h -> (h, misc_weight)))
    @ (Structural.extract fcfg |> List.map (fun h -> (h, structural_weight)))
  in
  let tree =
    match body with
    | Some b when family_weight Tree > 0. -> Syntax_tree.features b
    | _ -> []
  in
  of_families [ (Base, all); (Tree, tree) ]

(* Total feature count across families (the min-feature gates). [Context]
   is left out: it repeats the unit's own Base features plus its
   neighbours', which say nothing about how much the unit itself holds, so
   the gates (and the counts reported) don't move when it is switched on. *)
let size (s : t) : int =
  List.fold_left
    (fun n f -> if f = Context then n else n + Array.length (family_features s f))
    0 all_families

(* Merge-walk two hash-sorted weighted arrays, folding over the union.
   `f acc weight_in_a weight_in_b` with 0. when absent. *)
let fold_union (f : 'a -> float -> float -> 'a) (init : 'a)
    (a : weighted array) (b : weighted array) : 'a =
  let na = Array.length a and nb = Array.length b in
  let rec go acc i j =
    if i >= na && j >= nb then acc
    else if j >= nb || (i < na && a.(i).hash < b.(j).hash) then
      go (f acc a.(i).weight 0.) (i + 1) j
    else if i >= na || b.(j).hash < a.(i).hash then
      go (f acc 0. b.(j).weight) i (j + 1)
    else go (f acc a.(i).weight b.(j).weight) (i + 1) (j + 1)
  in
  go init 0 0

(* Weighted Jaccard of one family: sum(min) / sum(max). *)
let family_jaccard (a : weighted array) (b : weighted array) : float =
  let inter, union =
    fold_union
      (fun (i, u) wa wb -> (i +. Float.min wa wb, u +. Float.max wa wb))
      (0., 0.) a b
  in
  if union = 0. then 0. else inter /. union

(* Containment of one family: sum(min) / sum(query). *)
let family_containment ~(query : weighted array) (target : weighted array) :
    float =
  let inter, qsum =
    fold_union
      (fun (i, q) wq wt -> (i +. Float.min wq wt, q +. wq))
      (0., 0.) query target
  in
  if qsum = 0. then 0. else inter /. qsum

(* D15 fusion: sum_f w_f * score_f / sum_f w_f over the families [present]
   selects (and that carry weight). A lone family returns its own score
   untouched, so a Base-only signature scores exactly as before families. *)
let fuse ~(present : family -> bool) (score : family -> float) : float =
  let parts =
    List.filter_map
      (fun f ->
        let w = family_weight f in
        if w > 0. && present f then Some (w, f) else None)
      all_families
  in
  match parts with
  | [] -> 0.
  | [ (_, f) ] -> score f
  | _ ->
      let num, den =
        List.fold_left
          (fun (n, d) (w, f) -> (n +. (w *. score f), d +. w))
          (0., 0.) parts
      in
      num /. den

(* Weighted Jaccard, fused over the families non-empty on either side; a
   family empty on one side only scores 0 there. *)
let jaccard (a : t) (b : t) : float =
  fuse
    ~present:(fun f ->
      Array.length (family_features a f) > 0
      || Array.length (family_features b f) > 0)
    (fun f -> family_jaccard (family_features a f) (family_features b f))

(* Asymmetric containment (D5): how much of `query` is covered by `target`?
   1.0 means the target does everything the query does (and possibly more).
   Fused over the families non-empty in the query. *)
let containment ~(query : t) (target : t) : float =
  fuse
    ~present:(fun f -> Array.length (family_features query f) > 0)
    (fun f ->
      family_containment ~query:(family_features query f)
        (family_features target f))

(* Per-family scores, unfused (experiments): every family non-empty on
   either side, containment of [a] in [b]. *)
type family_score = { family : family; jaccard : float; containment : float }

let family_scores (a : t) (b : t) : family_score list =
  List.filter_map
    (fun f ->
      let fa = family_features a f and fb = family_features b f in
      if Array.length fa = 0 && Array.length fb = 0 then None
      else
        Some
          {
            family = f;
            jaccard = family_jaccard fa fb;
            containment = family_containment ~query:fa fb;
          })
    all_families
