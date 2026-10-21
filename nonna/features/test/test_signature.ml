(* D15: with a single family, fused scoring must be exactly the plain
   weighted Jaccard / containment over that family's features. *)

module Signature = Nonna_features.Signature

(* Reference: weighted Jaccard / containment over (hash, weight) lists,
   deduplicated with max weight, written independently of Signature. *)
let dedup (l : (int * float) list) : (int * float) list =
  let tbl = Hashtbl.create 16 in
  List.iter
    (fun (h, w) ->
      match Hashtbl.find_opt tbl h with
      | Some w0 when w0 >= w -> ()
      | _ -> Hashtbl.replace tbl h w)
    l;
  Hashtbl.fold (fun h w acc -> (h, w) :: acc) tbl [] |> List.sort compare

let weight_of l h = Option.value (List.assoc_opt h l) ~default:0.

let union_keys a b = List.sort_uniq compare (List.map fst a @ List.map fst b)

let ref_jaccard a b =
  let a = dedup a and b = dedup b in
  let i, u =
    List.fold_left
      (fun (i, u) h ->
        let wa = weight_of a h and wb = weight_of b h in
        (i +. Float.min wa wb, u +. Float.max wa wb))
      (0., 0.) (union_keys a b)
  in
  if u = 0. then 0. else i /. u

let ref_containment q t =
  let q = dedup q and t = dedup t in
  let i, s =
    List.fold_left
      (fun (i, s) h ->
        let wq = weight_of q h and wt = weight_of t h in
        (i +. Float.min wq wt, s +. wq))
      (0., 0.) (union_keys q t)
  in
  if s = 0. then 0. else i /. s

let fails = ref 0

let check name ok =
  if not ok then (
    Printf.printf "FAIL %s\n" name;
    incr fails)

let cases =
  [
    ( "overlap",
      [ (1, 1.0); (2, 0.8); (3, 0.3); (5, 0.5) ],
      [ (2, 1.0); (3, 0.3); (4, 0.8); (5, 0.2) ] );
    ("dup max weight", [ (7, 0.3); (7, 0.8); (9, 1.0) ], [ (7, 0.5); (9, 1.0) ]);
    ("disjoint", [ (1, 1.0) ], [ (2, 1.0) ]);
    ("one side empty", [ (1, 1.0); (2, 0.3) ], []);
    ("both empty", [], []);
  ]

let () =
  let run () =
    List.iter
      (fun (name, la, lb) ->
        let a = Signature.of_families [ (Signature.Base, la) ]
        and b = Signature.of_families [ (Signature.Base, lb) ] in
        (* exact float equality: fusion must not round a lone family *)
        check (name ^ ": jaccard") (Signature.jaccard a b = ref_jaccard la lb);
        check (name ^ ": containment a in b")
          (Signature.containment ~query:a b = ref_containment la lb);
        check (name ^ ": containment b in a")
          (Signature.containment ~query:b a = ref_containment lb la);
        check (name ^ ": size") (Signature.size a = List.length (dedup la));
        check (name ^ ": raw = base hashes")
          (Array.to_list a.Signature.raw = List.map fst (dedup la));
        match Signature.family_scores a b with
        | [] -> check (name ^ ": family_scores empty") (la = [] && lb = [])
        | [ s ] ->
            check (name ^ ": family_scores")
              (s.Signature.family = Signature.Base
              && s.Signature.jaccard = ref_jaccard la lb
              && s.Signature.containment = ref_containment la lb)
        | _ -> check (name ^ ": family_scores arity") false)
      cases
  in
  run ();
  (* a non-default family weight must not perturb a lone family's score *)
  Signature.set_weight "family.base" 0.7;
  run ();
  Hashtbl.reset Signature.weight_overrides;
  if !fails > 0 then exit 1
