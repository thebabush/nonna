(* nonna CLI — Phase 1: intra-workspace similarity.
 *
 *   nonna features <file>                      debug: per-fn feature dump
 *   nonna dupes <dir|files...> [-t 0.5]        intra-corpus clone pairs
 *   nonna query <corpus...> -- <draft.rs> [-t 0.25] [-k 5]
 *       reuse-before-write: for each fn in draft, top matches in corpus
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
  Units.units_of_file file
  |> List.iter (fun (u : Units.unit_info) ->
         let feats = Dfg.extract u.Units.ucfg in
         let sg = Signature.extract ~lang:u.Units.ulang u.Units.ucfg in
         Printf.printf "=== %s (%s) — %d dfg features, %d total\n"
           u.Units.uname (Units.loc_str u) (List.length feats)
           (Signature.size sg);
         feats
         |> List.iter (fun (f : Dfg.feature) ->
                Printf.printf "  %016x %s\n" f.Dfg.hash
                  (Dfg.tag_name f.Dfg.tag)))

let cmd_dupes (paths : string list) (threshold : float) =
  let units = Units.units_of_paths paths in
  let eng, kept = Units.index_units units in
  Printf.printf "indexed %d units (of %d; min %d features) from %d file(s)\n"
    (List.length kept) (List.length units) Units.min_features
    (List.length (Units.source_files_of_paths paths));
  let pairs = Engine.duplicates eng ~threshold in
  if pairs = [] then print_endline "no duplicate candidates above threshold."
  else
    pairs
    |> List.iter (fun ((a : Engine.meta), (b : Engine.meta), j) ->
           Printf.printf "%.3f  %s (%s:%d)  <->  %s (%s:%d)\n" j a.Engine.name
             a.Engine.file a.Engine.line_start b.Engine.name b.Engine.file
             b.Engine.line_start)

let cmd_query (corpus : string list) (draft : string) (threshold : float)
    (top_k : int) =
  let eng, kept = Units.index_units (Units.units_of_paths corpus) in
  Printf.printf "corpus: %d units indexed\n" (List.length kept);
  Units.units_of_file draft
  |> List.iter (fun (u : Units.unit_info) ->
         let sg = Signature.extract ~lang:u.Units.ulang u.Units.ucfg in
         Printf.printf "\n── %s (%s) — %d features\n" u.Units.uname
           (Units.loc_str u) (Signature.size sg);
         if Signature.size sg < Units.min_features then
           print_endline "  (too small to match)"
         else
           let hits =
             let self_m = Units.meta_of u in
             Engine.query eng sg ~threshold ~max_results:top_k
             (* the draft may itself be inside the corpus (drop self) or be
                a nested def queried against its container (tautological) *)
             |> List.filter (fun (h : Engine.hit) ->
                    not (Engine.nests self_m h.Engine.meta))
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
           Dfg.graph_of ~fc:(Dfg.base_cfg_for u.Units.ulang)
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
    | ("-o" | "--out") :: v :: rest -> go pos (("o", v) :: flags) rest
    | ("-p" | "--port") :: v :: rest -> go pos (("p", v) :: flags) rest
    | "--fn" :: v :: rest -> go pos (("fn", v) :: flags) rest
    | "--sample" :: v :: rest -> go pos (("sample", v) :: flags) rest
    | "--ext" :: v :: rest -> go pos (("ext", v) :: flags) rest
    | x :: rest -> go (x :: pos) flags rest
  in
  go [] [] args

let flag flags k default conv =
  match List.assoc_opt k flags with Some v -> conv v | None -> default

let usage () =
  prerr_endline
    "usage:\n\
    \  nonna features <file>\n\
    \  nonna dupes <dir|files...> [-t 0.5]\n\
    \  nonna query <corpus...> -- <draft.rs> [-t 0.25] [-k 5]\n\
    \  nonna graph <file> [--fn NAME] [-o DIR]   (DOT per propagation round)\n\
    \  nonna dump-il <file> [--fn NAME]          (compact IL CFG)\n\
    \  nonna parse-stats <paths...> [--sample N] [--ext .c]  (IL quality probe)\n\
    \  nonna mine <paths...> [-o pairs.tsv]      (ground-truth pair mining)\n\
    \  nonna eval <pairs.tsv> [-o scores.tsv]\n\
    \  nonna rank <pairs.tsv> <corpus paths...>  (MRR / recall@k, end to end)\n\
    \  nonna lsp                                 (stdio LSP server)\n\
    \  nonna mcp [root]                          (stdio MCP server; index root)\n\
    \  nonna serve [root] [-p 8976]              (HTTP MCP server, shared warm index)\n\
    \  nonna corpus <root>                       (debug: cargo deps + std discovery)\n\
    \  (global: --profile structural|full, --iters N, --with ch1,ch2)";
  exit 1

(* Global --profile flag (D14): structural (default, name-free hashing) or
   full (adds the name/value/type channels). Stripped before dispatch. *)
let rec strip_profile acc = function
  | [] -> List.rev acc
  | "--profile" :: v :: rest ->
      (match v with
      | "structural" -> Signature.default_profile := Signature.structural_profile
      | "full" -> Signature.default_profile := Signature.full_profile
      | _ ->
          prerr_endline "unknown --profile (use: structural | full)";
          exit 1);
      strip_profile acc rest
  | "--iters" :: v :: rest ->
      Dfg.iterations := int_of_string v;
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
               | "ty_descrs" -> { c with Dfg.ty_descrs = true }
               | "param_pos" -> { c with Dfg.param_pos = true }
               | "macro_tokens" -> { c with Dfg.macro_tokens = true }
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
  | "dupes" :: rest ->
      let pos, flags = parse_flags rest in
      if pos = [] then usage ();
      cmd_dupes pos (flag flags "t" 0.5 float_of_string)
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
      let pos, _ = parse_flags rest in
      match pos with
      | pairs :: corpus when corpus <> [] -> Bench.rank pairs corpus
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
