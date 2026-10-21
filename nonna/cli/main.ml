(* nonna CLI — Phase 1: intra-workspace similarity.
 *
 *   nonna features <file>                      debug: per-fn feature dump
 *   nonna dupes <dir|files...> [-t 0.5]        intra-corpus clone pairs
 *       filters: --metric max|jaccard, --name SUB, --include/--exclude SUB,SUB,
 *                --min-lines/--min-features N, -n LIMIT, --tests (include test code)
 *       --blocks: exact duplicated blocks instead of function pairs (blocks.ml)
 *       --classes [--class-threshold T] [--expand]: clone classes over the
 *                pairs plus a duplicated-SLOC summary (clones.ml); with
 *                --blocks, block clones join the summary
 *   nonna query <corpus...> -- <draft.rs> [-t 0.25] [-k 5]
 *       reuse-before-write: for each fn in draft, top matches in corpus
 *   nonna check [--staged | --base REF] [paths...] [--deps] [--strict]
 *       changed fns (git) that resemble existing code — see check.ml
 *   nonna graph <file> [--fn NAME] [-o DIR]    DOT per propagation round
 *   nonna dump-il <file> [--fn NAME]           compact IL CFG
 *   nonna mine <paths...> [-o pairs.tsv]       mine ground-truth pairs
 *   nonna eval <pairs.tsv>                     score pairs, report metrics
 *)

module Signature = Nonna_features.Signature
module Dfg = Nonna_features.Dfg
module Engine = Nonna_index.Engine

(* ── Commands ────────────────────────────────────────────────────────────── *)

let cmd_features (file : string) =
  Units.extract_all (Units.units_of_file file)
  |> List.iter (fun ((u : Units.unit_info), sg) ->
         let feats = Dfg.extract u.Units.ucfg in
         Printf.printf "=== %s (%s)%s — %d dfg features, %d total (%s)\n"
           u.Units.uname (Units.loc_str u)
           (if u.Units.utest then " [test]" else "")
           (List.length feats) (Signature.size sg)
           (Signature.all_families
           |> List.filter (fun f -> Signature.family_weight f > 0.)
           |> List.map (fun f ->
                  Printf.sprintf "%s %d" (Signature.family_name f)
                    (Array.length (Signature.family_features sg f)))
           |> String.concat ", ");
         feats
         |> List.iter (fun (f : Dfg.feature) ->
                Printf.printf "  %016x %s\n" f.Dfg.hash
                  (Dfg.tag_name f.Dfg.tag)))

(* Clone-class mode of [dupes] (D18): summary line first, then the classes.
   [blocks] adds block clones (D16) to the summary and lists their groups
   after the classes. *)
type block_opts = { min_stmts : int; min_lines : int }

type classes_opts = {
  class_threshold : float;
  expand : bool;
  blocks : block_opts option;
}

let cmd_dupes ?classes (paths : string list) (flt : Engine.dup_filter) =
  let units = Units.units_of_paths paths in
  let eng, kept = Units.index_units units in
  let tests =
    List.length (List.filter (fun ((u : Units.unit_info), _) -> u.Units.utest) kept)
  in
  let indexed =
    Printf.sprintf "indexed %d units (of %d; min %d features) from %d file(s)%s"
      (List.length kept) (List.length units) Units.min_features
      (List.length (Units.source_files_of_paths paths))
      (if flt.Engine.include_tests then ""
       else Printf.sprintf "; skipping %d test units (--tests to include)" tests)
  in
  if classes = None then print_endline indexed;
  (* in class mode every pair feeds the classes; -n caps the classes listed
     instead (as MCP find_duplicates group:true) *)
  let pairs =
    Engine.duplicates_filtered eng
      (if classes = None then flt else { flt with Engine.limit = 0 })
  in
  match classes with
  | Some o ->
      let cs =
        Clones.classes ~threshold:o.class_threshold ~by_max:flt.Engine.by_max
          pairs
      in
      let gated (u : Units.unit_info) = Engine.side_ok flt (Units.meta_of u) in
      (* the units the scan covered, for the line total: the pair scan's
         (indexed, past the side gates), or with blocks every gated unit,
         indexed or not, since blocks are found in all of them *)
      let scanned =
        match o.blocks with
        | None -> List.filter_map (fun (u, _) -> if gated u then Some u else None) kept
        | Some _ -> List.filter gated units
      in
      let summary = Clones.summary cs ~scanned:(List.map Units.meta_of scanned) in
      let gs =
        match o.blocks with
        | None -> []
        | Some { min_stmts; min_lines } -> Blocks.groups ~min_stmts ~min_lines scanned
      in
      (match o.blocks with
      | None -> Printf.printf "nonna dupes: %s\n" summary
      | Some _ ->
          let total =
            Clones.code_lines (Clones.roots (List.map Units.meta_of scanned))
          in
          let extra = Clones.block_extra_sloc cs gs in
          let dup = Clones.total_dup_sloc cs + extra in
          Printf.printf
            "nonna dupes: %s; %d block clone group(s), %d duplicated SLOC, %d \
             of them outside the classes: %d duplicated SLOC in all (%.1f%%)\n"
            summary (List.length gs)
            (List.fold_left (fun n (g : Blocks.group) -> n + g.Blocks.dup_sloc) 0 gs)
            extra dup (Clones.pct dup total));
      print_endline indexed;
      let limit = flt.Engine.limit in
      List.iteri
        (fun k c ->
          if limit <= 0 || k < limit then begin
            print_newline ();
            List.iter print_endline (Clones.render ~expand:o.expand (k + 1) c)
          end)
        cs;
      Option.iter
        (fun { min_stmts; min_lines } ->
          if gs <> [] then (
            Printf.printf
              "\nblock clones (blocks >= %d statements, >= %d code lines):\n"
              min_stmts min_lines;
            Blocks.print_groups gs))
        o.blocks
  | None ->
      if pairs = [] then print_endline "no duplicate candidates above threshold."
      else
        pairs
        |> List.iter (fun (p : Engine.pair) ->
               Printf.printf "j %.3f c %.3f  %s (%s:%d)  <->  %s (%s:%d)\n"
                 p.Engine.j p.Engine.c p.Engine.a.Engine.name
                 p.Engine.a.Engine.file p.Engine.a.Engine.line_start
                 p.Engine.b.Engine.name p.Engine.b.Engine.file
                 p.Engine.b.Engine.line_start)

let cmd_query (corpus : string list) (draft : string) (threshold : float)
    (top_k : int) =
  let eng, kept = Units.index_units (Units.units_of_paths corpus) in
  Printf.printf "corpus: %d units indexed\n" (List.length kept);
  (* the draft's call-graph context (D17) resolves within the draft file *)
  Units.extract_all (Units.units_of_file draft)
  |> List.iter (fun ((u : Units.unit_info), sg) ->
         Printf.printf "\n── %s (%s) — %d features\n" u.Units.uname
           (Units.loc_str u) (Signature.size sg);
         if Signature.size sg < Units.min_features then
           print_endline "  (too small to match)"
         else
           let hits =
             let self_m = Units.meta_of u in
             Engine.query eng sg ~threshold ~max_results:top_k ~accept:(fun m ->
                 not (Engine.nests self_m m))
           in
           if hits = [] then print_endline "  no similar function found."
           else
             hits
             |> List.iter (fun (h : Engine.hit) ->
                    Printf.printf
                      "  jaccard %.3f  containment %.3f  %s (%s:%d-%d)\n"
                      h.Engine.jaccard h.Engine.containment
                      h.Engine.meta.Engine.name h.Engine.meta.Engine.file
                      h.Engine.meta.Engine.line_start
                      h.Engine.meta.Engine.line_end))

let cmd_graph (file : string) (fn_filter : string option) (outdir : string) =
  let units = Units.units_of_file file in
  let selected =
    match fn_filter with
    | None -> units
    | Some f -> List.filter (fun (u : Units.unit_info) -> u.Units.uname = f) units
  in
  if selected = [] then (
    Printf.eprintf "no matching function%s in %s\n"
      (match fn_filter with Some f -> " '" ^ f ^ "'" | None -> "s")
      file;
    exit 1);
  let file_lines =
    try
      let ic = open_in file in
      let rec go acc =
        match input_line ic with
        | l -> go (l :: acc)
        | exception End_of_file ->
            close_in ic;
            List.rev acc
      in
      go []
    with _ -> []
  in
  selected
  |> List.iter (fun (u : Units.unit_info) ->
         let g =
           Dfg.graph_of
             ~fc:(Dfg.base_cfg_for u.Units.ulang)
             ~iters:(Dfg.iters_for (Some u.Units.ulang))
             u.Units.ucfg
         in
         let source =
           if u.Units.uline_start > 0 && file_lines <> [] then
             Some
               ( u.Units.uline_start,
                 file_lines
                 |> List.filteri (fun i _ ->
                        i + 1 >= u.Units.uline_start
                        && i + 1 <= u.Units.uline_end) )
           else None
         in
         let paths =
           Viz.write_rounds ?source ~outdir ~fn_name:u.Units.uname g
         in
         Printf.printf "%s (%s): %d nodes, %d rounds\n" u.Units.uname
           (Units.loc_str u)
           (Array.length g.Dfg.dnodes)
           (Array.length g.Dfg.rounds);
         List.iter (fun p -> Printf.printf "  %s\n" p) paths)

let node_str (n : IL.node_kind) : string =
  match n with
  | IL.Enter -> "enter"
  | IL.Exit -> "exit"
  | IL.Join -> "join"
  | IL.TrueNode e -> Printf.sprintf "true(%s)" (IL_pp.pp_exp e)
  | IL.FalseNode e -> Printf.sprintf "false(%s)" (IL_pp.pp_exp e)
  | IL.NInstr i -> IL_pp.pp_instr_kind i.IL.i
  | IL.NCond (_, e) -> Printf.sprintf "if %s" (IL_pp.pp_exp e)
  | IL.NReturn (_, e) -> Printf.sprintf "return %s" (IL_pp.pp_exp e)
  | IL.NThrow (_, e) -> Printf.sprintf "throw %s" (IL_pp.pp_exp e)
  | IL.NGoto (_, l) -> Printf.sprintf "goto %s" (IL.str_of_label l)
  | IL.NOther _ -> "<other>"
  | IL.NTodo _ -> "<todo>"

let cmd_dump_il (file : string) (fn_filter : string option) =
  Units.units_of_file file
  |> List.iter (fun (u : Units.unit_info) ->
         if fn_filter = None || fn_filter = Some u.Units.uname then (
           let cfg = u.Units.ucfg.IL.cfg in
           let nodes = Nonna_features.Il_util.nodes_in_order cfg in
           Printf.printf "=== %s (%s): %d reachable CFG nodes\n" u.Units.uname
             (Units.loc_str u) (List.length nodes);
           nodes
           |> List.iter (fun (ni, (n : IL.node)) ->
                  let succs =
                    Nonna_features.Il_util.succs cfg ni
                    |> List.map string_of_int |> String.concat ","
                  in
                  Printf.printf "  [%2d] %-50s -> %s\n" ni (node_str n.IL.n)
                    succs)))

(* ── Arg parsing (minimal) ───────────────────────────────────────────────── *)

let rec split_ddash acc = function
  | [] -> (List.rev acc, [])
  | "--" :: rest -> (List.rev acc, rest)
  | x :: rest -> split_ddash (x :: acc) rest

let parse_flags (args : string list) : string list * (string * string) list =
  let rec go pos flags = function
    | [] -> (List.rev pos, flags)
    | ("-t" | "--threshold") :: v :: rest -> go pos (("t", v) :: flags) rest
    | ("-k" | "--top") :: v :: rest -> go pos (("k", v) :: flags) rest
    | ("-n" | "--limit") :: v :: rest -> go pos (("n", v) :: flags) rest
    | ("-o" | "--out") :: v :: rest -> go pos (("o", v) :: flags) rest
    | ("-p" | "--port") :: v :: rest -> go pos (("p", v) :: flags) rest
    | "--name" :: v :: rest -> go pos (("name", v) :: flags) rest
    | "--include" :: v :: rest -> go pos (("include", v) :: flags) rest
    | "--exclude" :: v :: rest -> go pos (("exclude", v) :: flags) rest
    | "--min-lines" :: v :: rest -> go pos (("min-lines", v) :: flags) rest
    | "--min-features" :: v :: rest -> go pos (("min-features", v) :: flags) rest
    | "--metric" :: v :: rest -> go pos (("metric", v) :: flags) rest
    | "--fn" :: v :: rest -> go pos (("fn", v) :: flags) rest
    | "--tests" :: rest -> go pos (("tests", "1") :: flags) rest
    | "--blocks" :: rest -> go pos (("blocks", "1") :: flags) rest
    | "--classes" :: rest -> go pos (("classes", "1") :: flags) rest
    | "--expand" :: rest -> go pos (("expand", "1") :: flags) rest
    | "--class-threshold" :: v :: rest ->
        go pos (("class-threshold", v) :: flags) rest
    | "--min-stmts" :: v :: rest -> go pos (("min-stmts", v) :: flags) rest
    | "--staged" :: rest -> go pos (("staged", "1") :: flags) rest
    | "--base" :: v :: rest -> go pos (("base", v) :: flags) rest
    | "--deps" :: rest -> go pos (("deps", "1") :: flags) rest
    | "--strict" :: rest -> go pos (("strict", "1") :: flags) rest
    | "--sample" :: v :: rest -> go pos (("sample", v) :: flags) rest
    | "--ext" :: v :: rest -> go pos (("ext", v) :: flags) rest
    | x :: rest -> go (x :: pos) flags rest
  in
  go [] [] args

let flag flags k default conv =
  match List.assoc_opt k flags with Some v -> conv v | None -> default

(* comma-separated list value, empty entries dropped (for --include/--exclude) *)
let split_csv (s : string) : string list =
  String.split_on_char ',' s |> List.filter (fun x -> x <> "")

let usage () =
  prerr_endline
    "usage:\n\
    \  nonna features <file>\n\
    \  nonna dupes <dir|files...> [-t 0.5] [--metric max|jaccard] [-n LIMIT]\n\
    \                            [--name SUB] [--include SUB,..] [--exclude SUB,..]\n\
    \                            [--min-lines N] [--min-features N] [--tests]\n\
    \  nonna dupes <dir|files...> --blocks [--min-stmts 2] [--min-lines 3]\n\
    \                            [--include SUB,..] [--exclude SUB,..] [--tests]\n\
    \                            (exact duplicated blocks inside functions)\n\
    \  nonna dupes <dir|files...> --classes [--class-threshold T] [--expand]\n\
    \                            [pair filters] [--blocks]\n\
    \                            (clone classes + duplicated-SLOC summary)\n\
    \  nonna query <corpus...> -- <draft.rs> [-t 0.25] [-k 5]\n\
    \  nonna check [--staged | --base REF] [paths...] [-t 0.7] [--deps]\n\
    \                            [--strict]\n\
    \  nonna graph <file> [--fn NAME] [-o DIR]   (DOT per propagation round)\n\
    \  nonna dump-il <file> [--fn NAME]          (compact IL CFG)\n\
    \  nonna parse-stats <paths...> [--sample N] [--ext .c]  (IL quality probe)\n\
    \  nonna mine <paths...> [-o pairs.tsv]      (ground-truth pair mining)\n\
    \  nonna eval <pairs.tsv> [-o scores.tsv]\n\
    \  nonna rank <pairs.tsv> <corpus paths...> [-o ranks.tsv]  (MRR / recall@k)\n\
    \  nonna lsp                                 (stdio LSP server)\n\
    \  nonna mcp [root]                          (stdio MCP server; index root)\n\
    \  nonna serve [root] [-p 8976]              (HTTP MCP server, shared warm index)\n\
    \  nonna corpus <root>                       (debug: cargo deps + std discovery)\n\
    \  (global: --profile structural|full, --literals, --iters N, --with ch1,ch2)\n\
    \  --literals: hash literal values too (string/float; ints always), so\n\
    \             functions that differ only in constants stop matching";
  exit 1

(* Global --profile flag (D14): structural (default, name-free hashing) or
   full (adds the name/value/type channels). Stripped before dispatch. *)
let rec strip_profile acc = function
  | [] -> List.rev acc
  | "--literals" :: rest ->
      Dfg.set_literals true;
      strip_profile acc rest
  | "--profile" :: v :: rest ->
      (match v with
      | "structural" -> Signature.default_profile := Signature.structural_profile
      | "full" -> Signature.default_profile := Signature.full_profile
      | _ ->
          prerr_endline "unknown --profile (use: structural | full)";
          exit 1);
      strip_profile acc rest
  | "--iters" :: v :: rest ->
      Dfg.iterations_override := Some (int_of_string v);
      strip_profile acc rest
  | "--weights" :: v :: rest ->
      (* per-DFG-tag weight overrides for tuning sweeps, e.g.
         --weights construct=1.3,control=1.2 (tag names per Dfg.tag_name;
         family fusion weights as family.<name>, D15; the context family's
         gates as context.callees / context.parent / context.max_callee_features,
         D17) *)
      String.split_on_char ',' v
      |> List.iter (fun kv ->
             match String.split_on_char '=' kv with
             | [ name; w ] -> Signature.set_weight name (float_of_string w)
             | _ ->
                 prerr_endline ("bad --weights entry: " ^ kv);
                 exit 1);
      strip_profile acc rest
  | "--with" :: v :: rest ->
      (* fold channels into the BASE hashes (ablation studies) *)
      String.split_on_char ',' v
      |> List.iter (fun f ->
             let c = !Dfg.base_cfg in
             Dfg.base_cfg :=
               (match f with
               | "call_names" -> { c with Dfg.call_names = true }
               | "field_names" -> { c with Dfg.field_names = true }
               | "int_values" -> { c with Dfg.int_values = true }
               | "float_values" -> { c with Dfg.float_values = true }
               | "string_values" -> { c with Dfg.string_values = true }
               | "string_chunks" -> { c with Dfg.string_chunks = true }
               | "ty_descrs" -> { c with Dfg.ty_descrs = true }
               | "param_pos" -> { c with Dfg.param_pos = true }
               | "macro_tokens" -> { c with Dfg.macro_tokens = true }
               | "exp_nodes" -> { c with Dfg.exp_nodes = true }
               | "thru_copies" -> { c with Dfg.thru_copies = true }
               | _ ->
                   prerr_endline ("unknown --with flag: " ^ f);
                   exit 1));
      strip_profile acc rest
  | x :: rest -> strip_profile (x :: acc) rest

let () =
  Parsing_init.init ();
  match strip_profile [] (Sys.argv |> Array.to_list |> List.tl) with
  | "features" :: [ file ] -> cmd_features file
  | "dump-il" :: rest -> (
      let pos, flags = parse_flags rest in
      match pos with
      | [ file ] -> cmd_dump_il file (List.assoc_opt "fn" flags)
      | _ -> usage ())
  | "graph" :: rest -> (
      let pos, flags = parse_flags rest in
      match pos with
      | [ file ] ->
          cmd_graph file
            (List.assoc_opt "fn" flags)
            (flag flags "o" "viz-out" (fun s -> s))
      | _ -> usage ())
  | "dupes" :: rest when List.mem "--blocks" rest && not (List.mem "--classes" rest) ->
      (* block clones only (D16); the function-pair listing is not run *)
      let pos, flags = parse_flags rest in
      if pos = [] then usage ();
      Blocks.run
        ~include_tests:(List.mem_assoc "tests" flags)
        ~include_paths:(flag flags "include" [] split_csv)
        ~exclude_paths:(flag flags "exclude" [] split_csv)
        ~min_stmts:(flag flags "min-stmts" Blocks.min_stmts int_of_string)
        ~min_lines:(flag flags "min-lines" Blocks.min_lines int_of_string)
        pos
  | "dupes" :: rest ->
      let pos, flags = parse_flags rest in
      if pos = [] then usage ();
      let by_max =
        match flag flags "metric" "jaccard" (fun s -> s) with
        | "max" -> true
        | "jaccard" -> false
        | _ ->
            prerr_endline "unknown --metric (use: max | jaccard)";
            exit 1
      in
      let threshold = flag flags "t" 0.5 float_of_string in
      let classes =
        if List.mem_assoc "classes" flags then
          Some
            {
              class_threshold =
                flag flags "class-threshold" threshold float_of_string;
              expand = List.mem_assoc "expand" flags;
              blocks =
                (if List.mem_assoc "blocks" flags then
                   Some
                     {
                       min_stmts =
                         flag flags "min-stmts" Blocks.min_stmts int_of_string;
                       min_lines =
                         flag flags "min-lines" Blocks.min_lines int_of_string;
                     }
                 else None);
            }
        else None
      in
      cmd_dupes ?classes pos
        {
          Engine.threshold;
          by_max;
          name_sub = flag flags "name" "" (fun s -> s);
          include_paths = flag flags "include" [] split_csv;
          exclude_paths = flag flags "exclude" [] split_csv;
          min_lines = flag flags "min-lines" 0 int_of_string;
          min_features = flag flags "min-features" 0 int_of_string;
          limit = flag flags "n" 0 int_of_string;
          scope_a = 0;
          scope_b = 0;
          include_tests = List.mem_assoc "tests" flags;
        }
  | "parse-stats" :: rest ->
      let pos, flags = parse_flags rest in
      if pos = [] then usage ();
      Stats.run pos
        (flag flags "sample" 0 int_of_string)
        (List.assoc_opt "ext" flags)
  | "query" :: rest -> (
      let pos, flags = parse_flags rest in
      match split_ddash [] pos with
      | corpus, [ draft ] when corpus <> [] ->
          cmd_query corpus draft
            (flag flags "t" 0.25 float_of_string)
            (flag flags "k" 5 int_of_string)
      | _ -> usage ())
  | "check" :: rest ->
      let pos, flags = parse_flags rest in
      Check.run
        ~staged:(List.mem_assoc "staged" flags)
        ~base:(List.assoc_opt "base" flags)
        ~deps:(List.mem_assoc "deps" flags)
        ~strict:(List.mem_assoc "strict" flags)
        ~threshold:(flag flags "t" Check.threshold float_of_string)
        pos
  | "mine" :: rest ->
      let pos, flags = parse_flags rest in
      if pos = [] then usage ();
      Bench.mine pos (flag flags "o" "pairs.tsv" (fun s -> s))
  | "eval" :: rest -> (
      let pos, flags = parse_flags rest in
      match pos with
      | [ pairs ] -> Bench.eval ?scores_out:(List.assoc_opt "o" flags) pairs
      | _ -> usage ())
  | "rank" :: rest -> (
      let pos, flags = parse_flags rest in
      match pos with
      | pairs :: corpus when corpus <> [] ->
          Bench.rank ?ranks_out:(List.assoc_opt "o" flags) pairs corpus
      | _ -> usage ())
  | [ "lsp" ] -> Lsp_server.run ()
  | "mcp" :: rest -> (
      let pos, _ = parse_flags rest in
      match pos with
      | [] -> Mcp_server.run (Sys.getcwd ())
      | [ root ] -> Mcp_server.run root
      | _ -> usage ())
  | "corpus" :: rest -> (
      let pos, _ = parse_flags rest in
      match pos with
      | [ root ] ->
          let deps = Corpus.cargo_deps root @ Corpus.std_deps () in
          Printf.printf "corpus for %s: %d deps\n" root (List.length deps);
          let eng = Nonna_index.Engine.create () in
          let _, total = Corpus.add_deps eng ~log:print_endline root in
          Printf.printf "total dep/std functions: %d (cache: %s)\n" total
            (Corpus.cache_dir ())
      | _ -> usage ())
  | "serve" :: rest -> (
      let pos, flags = parse_flags rest in
      let port = flag flags "p" 8976 int_of_string in
      match pos with
      | [] -> Mcp_server.serve (Sys.getcwd ()) port
      | [ root ] -> Mcp_server.serve root port
      | _ -> usage ())
  | _ -> usage ()
