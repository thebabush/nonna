(* Parser/IL quality probe — the go/no-go check before tuning a new
   language. Counts how much of the IL is real instructions versus macro
   wreckage (FixmeInstr / FixmeExp) and untranslated statements
   (NTodo / NOther). If wreckage dominates, signatures are structure-blind
   and channel tuning is pointless; fix the frontend first. *)

module Signature = Nonna_features.Signature
module Il_util = Nonna_features.Il_util

type ustats = {
  instrs : int;
  fixme_instrs : int;
  conds : int;
  returns : int;
  todos : int; (* NTodo + NOther: statements the IL gave up on *)
  exps : int;
  fixme_exps : int;
  feats : int;
}

let stats_of_unit (u : Units.unit_info) : ustats =
  let instrs = ref 0
  and fixme_instrs = ref 0
  and conds = ref 0
  and returns = ref 0
  and todos = ref 0
  and exps = ref 0
  and fixme_exps = ref 0 in
  Il_util.nodes_in_order u.Units.ucfg.IL.cfg
  |> List.iter (fun ((_, node) : int * IL.node) ->
         (match node.IL.n with
         | IL.NInstr i -> (
             incr instrs;
             match i.IL.i with
             | IL.FixmeInstr _ -> incr fixme_instrs
             | _ -> ())
         | IL.NCond _ -> incr conds
         | IL.NReturn _ -> incr returns
         | IL.NTodo _ | IL.NOther _ -> incr todos
         | _ -> ());
         Il_util.exps_of_node node.IL.n
         |> List.iter
              (Il_util.iter_exp (fun e ->
                   incr exps;
                   match e.IL.e with
                   | IL.FixmeExp _ -> incr fixme_exps
                   | _ -> ())));
  let feats =
    Signature.size (Signature.extract ~lang:u.Units.ulang u.Units.ucfg)
  in
  {
    instrs = !instrs;
    fixme_instrs = !fixme_instrs;
    conds = !conds;
    returns = !returns;
    todos = !todos;
    exps = !exps;
    fixme_exps = !fixme_exps;
    feats;
  }

(* share of a unit's instr+exp volume that is macro wreckage *)
let fixme_share (s : ustats) : float =
  let bad = s.fixme_instrs + s.fixme_exps + s.todos in
  let all = s.instrs + s.exps + s.todos in
  if all = 0 then 0. else float_of_int bad /. float_of_int all

let pct a b = if b = 0 then 0. else 100. *. float_of_int a /. float_of_int b

(* Evenly-spaced deterministic sample (no RNG: reruns are comparable). *)
let sample (n : int) (xs : 'a list) : 'a list =
  let len = List.length xs in
  if n <= 0 || len <= n then xs
  else
    let arr = Array.of_list xs in
    List.init n (fun i -> arr.(i * len / n))

let run (paths : string list) (n_sample : int) (ext_filter : string option) :
    unit =
  let files =
    Units.source_files_of_paths paths
    |> List.filter (fun f ->
           match ext_filter with
           | Some e -> Filename.extension f = e
           | None -> true)
    |> sample n_sample
  in
  Printf.printf "probing %d files...\n%!" (List.length files);
  let failed = ref 0 in
  let per_file : (string * ustats list) list =
    files
    |> List.filter_map (fun f ->
           match Units.units_of_file f with
           | us -> Some (f, List.map stats_of_unit us)
           | exception _ ->
               incr failed;
               None)
  in
  let all = List.concat_map snd per_file in
  let nu = List.length all in
  let sum g = List.fold_left (fun acc s -> acc + g s) 0 all in
  let instrs = sum (fun s -> s.instrs)
  and fixme_instrs = sum (fun s -> s.fixme_instrs)
  and exps = sum (fun s -> s.exps)
  and fixme_exps = sum (fun s -> s.fixme_exps)
  and todos = sum (fun s -> s.todos)
  and conds = sum (fun s -> s.conds)
  and returns = sum (fun s -> s.returns) in
  let matchable = List.length (List.filter (fun s -> s.feats >= Units.min_features) all) in
  Printf.printf "files : %d parsed, %d failed (%.1f%%)\n"
    (List.length per_file) !failed
    (pct !failed (List.length files));
  Printf.printf "units : %d total; %d (%.1f%%) at/above min features (%d)\n"
    nu matchable (pct matchable nu) Units.min_features;
  if nu > 0 then (
    Printf.printf
      "IL    : per unit avg %.1f instr / %.1f cond / %.1f return\n"
      (float_of_int instrs /. float_of_int nu)
      (float_of_int conds /. float_of_int nu)
      (float_of_int returns /. float_of_int nu);
    Printf.printf
      "fixme : %.1f%% of instrs (FixmeInstr), %.1f%% of exps (FixmeExp), \
       %d NTodo/NOther (%.1f%% of stmts)\n"
      (pct fixme_instrs instrs) (pct fixme_exps exps) todos
      (pct todos (instrs + conds + returns + todos));
    let clean, light, heavy =
      List.fold_left
        (fun (c, l, h) s ->
          let sh = fixme_share s in
          if sh = 0. then (c + 1, l, h)
          else if sh < 0.25 then (c, l + 1, h)
          else (c, l, h + 1))
        (0, 0, 0) all
    in
    Printf.printf
      "units : %.1f%% fixme-free, %.1f%% light (<25%% wreckage), %.1f%% \
       heavy (>=25%%)\n"
      (pct clean nu) (pct light nu) (pct heavy nu);
    (* worst offenders: where to look when the numbers are bad *)
    let by_file =
      per_file
      |> List.filter_map (fun (f, us) ->
             let bad = List.filter (fun s -> fixme_share s >= 0.25) us in
             if bad = [] then None else Some (f, List.length bad, List.length us))
      |> List.sort (fun (_, a, _) (_, b, _) -> compare b a)
    in
    match by_file with
    | [] -> ()
    | _ ->
        Printf.printf "heaviest files (units >=25%% wreckage / total):\n";
        by_file
        |> List.filteri (fun i _ -> i < 10)
        |> List.iter (fun (f, bad, tot) ->
               Printf.printf "  %3d/%-3d %s\n" bad tot f))
