(* Structural features: CFG shape, loops, node-kind histogram.
   Port of v1's structural.rs, adapted from basic blocks to the IL's
   node-per-instruction CFG. Coarse bucketing allows fuzzy matching. *)

let bucket (v : int) (thresholds : int list) : int =
  let rec go i = function
    | [] -> i
    | t :: rest -> if v <= t then i else go (i + 1) rest
  in
  go 0 thresholds

let hash_of (tagv : int) (ints : int list) : Fhash.t =
  let f = Fhash.Feed.create () in
  Fhash.Feed.tag f tagv;
  List.iter (Fhash.Feed.int f) ints;
  Fhash.Feed.finish f

(* Count back edges via iterative DFS (gray-node detection). *)
let back_edge_count (cfg : IL.cfg) : int =
  let color : (int, [ `Gray | `Black ]) Hashtbl.t = Hashtbl.create 64 in
  let count = ref 0 in
  let rec visit ni =
    Hashtbl.replace color ni `Gray;
    List.iter
      (fun s ->
        match Hashtbl.find_opt color s with
        | Some `Gray -> incr count
        | Some `Black -> ()
        | None -> visit s)
      (Il_util.succs cfg ni);
    Hashtbl.replace color ni `Black
  in
  visit cfg.CFG.entry;
  !count

let extract (fcfg : IL.fun_cfg) : Fhash.t list =
  let cfg = fcfg.IL.cfg in
  let nodes = Il_util.nodes_in_order cfg in
  let n_nodes = List.length nodes in
  let n_edges =
    List.fold_left
      (fun acc (ni, _) -> acc + List.length (Il_util.succs cfg ni))
      0 nodes
  in
  let count pred = List.length (List.filter (fun (_, n) -> pred n.IL.n) nodes) in
  let n_cond = count (function IL.NCond _ -> true | _ -> false) in
  let n_join = count (function IL.Join -> true | _ -> false) in
  let n_ret = count (function IL.NReturn _ -> true | _ -> false) in
  let n_throw = count (function IL.NThrow _ -> true | _ -> false) in
  let n_instr = count (function IL.NInstr _ -> true | _ -> false) in
  let n_back = back_edge_count cfg in
  let cyclomatic = max 1 (n_edges - n_nodes + 2) in
  [
    (* overall shape *)
    hash_of 0xA1
      [
        bucket n_nodes [ 4; 8; 16; 32; 64; 128 ];
        bucket n_edges [ 4; 10; 22; 46; 94; 190 ];
        bucket cyclomatic [ 1; 3; 7; 15; 31 ];
      ];
    (* node-kind histogram (v1's edge-kind histogram analog) *)
    hash_of 0xA2
      [
        bucket n_cond [ 0; 1; 3; 7; 15 ];
        bucket n_join [ 0; 1; 3; 7; 15 ];
        bucket n_ret [ 0; 1; 3; 7 ];
        bucket n_throw [ 0; 1; 3 ];
        bucket n_instr [ 2; 5; 11; 23; 47; 95 ];
      ];
    (* loopiness *)
    hash_of 0xA0 [ bucket n_back [ 0; 1; 2; 4; 8 ] ];
  ]
