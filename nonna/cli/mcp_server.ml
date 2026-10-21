(* MCP server (stdio, newline-delimited JSON-RPC — the MCP stdio transport).
 *
 * Phase 3 / D4: the agent-facing product surface. The corpus root is indexed
 * once at startup (background thread). Tools:
 *
 *   find_similar    drafted code (string) -> ranked existing functions.
 *                   THE reuse-before-write loop: call before committing a
 *                   freshly written function.
 *   query_similar   function already on disk (file + line|name) -> matches.
 *   diff_functions  signature algebra over two functions A and B:
 *                   scores (A ∩ B) plus per-side unique regions — DFG nodes
 *                   whose hashes never appear on the other side at ANY
 *                   propagation depth, grouped by source line. For a
 *                   bug/fix pair: A−B ≈ the bug, B−A ≈ the fix.
 *   find_duplicates whole-corpus clone pairs (no query fn) + filters —
 *                   the dedup/refactor view; complements find_similar.
 *   status          index size / readiness.
 *)

module J = Yojson.Safe
module JU = Yojson.Safe.Util
module Engine = Nonna_index.Engine
module Signature = Nonna_features.Signature
module Dfg = Nonna_features.Dfg

(* ── Wire ────────────────────────────────────────────────────────────────── *)

let send_mutex = Mutex.create ()

let send (j : J.t) : unit =
  Mutex.lock send_mutex;
  print_string (J.to_string j ^ "\n");
  flush stdout;
  Mutex.unlock send_mutex

(* tools/call result helpers *)
let tool_text ?(is_error = false) (text : string) : J.t =
  `Assoc
    [
      ( "content",
        `List [ `Assoc [ ("type", `String "text"); ("text", `String text) ] ]
      );
      ("isError", `Bool is_error);
    ]

(* ── Index ───────────────────────────────────────────────────────────────── *)

let engine : Engine.t ref = ref Engine.empty
let index_root = ref ""
let indexing_done = ref false
let workspace_fns = ref 0
let workspace_test_fns = ref 0 (* of [workspace_fns], flagged as test code *)
let dep_count = ref 0
let dep_fns = ref 0

(* the root has a .nonna marker (opt-in gate, see Workspace.marker_name) *)
let enabled = ref false

let literals_desc () : string =
  if Dfg.literals_on () then
    "on (string/float/int literal values are hashed: functions differing \
     only in constants do not match)"
  else
    "language defaults (integer values are hashed; Python, C and C++ also \
     hash strings, Rust and OCaml hash string chunks. Set `literals = true` \
     in .nonna or start with --literals to include string and float values \
     in every language)"

(* Tool dispatch, refreshes and explorer reads share one state boundary. *)
let dispatch_mutex = Mutex.create ()

let with_dispatch f =
  Mutex.lock dispatch_mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock dispatch_mutex) f

(* ── Cancellation ────────────────────────────────────────────────────────
   Every tools/call runs on its own thread so the stdio loop keeps reading
   while a scan runs. A client that gives up sends MCP
   `notifications/cancelled` (the SDKs do on timeout or abort); that flips
   the request's flag and the running scan stops at its next poll instead of
   grinding on for an answer nobody will read — before this, each aborted
   sweep cost the full scan at 100% CPU, and they queued. Calls still execute
   one at a time under [dispatch_mutex]; one cancelled while queued returns
   as soon as its turn comes. *)
let cancel_mutex = Mutex.create ()
let in_flight : (string, bool ref) Hashtbl.t = Hashtbl.create 8

let with_cancel_flag (id : J.t) (f : bool ref -> 'a) : 'a =
  let key = J.to_string id in
  let flag = ref false in
  Mutex.lock cancel_mutex;
  Hashtbl.replace in_flight key flag;
  Mutex.unlock cancel_mutex;
  Fun.protect
    ~finally:(fun () ->
      Mutex.lock cancel_mutex;
      Hashtbl.remove in_flight key;
      Mutex.unlock cancel_mutex)
    (fun () -> f flag)

let cancel_request (id : J.t) : unit =
  Mutex.lock cancel_mutex;
  (match Hashtbl.find_opt in_flight (J.to_string id) with
  | Some flag -> flag := true
  | None -> ());
  Mutex.unlock cancel_mutex

(* Stop predicate of the tool call currently executing; long scans poll it. *)
let stop_requested : (unit -> bool) ref = ref (fun () -> false)

exception Cancelled

let dependencies : Corpus.dep list ref = ref []
let indexed_stamp : (string * string option) list ref = ref []

(* Content digests also catch same-size edits with unchanged mtimes. Include
   manifests so dependency additions/removals trigger rediscovery. *)
let workspace_stamp root deps =
  let roots =
    root
    :: List.filter_map
         (fun (d : Corpus.dep) ->
           if d.source = None then Some d.src_dir else None)
         deps
  in
  let files =
    Workspace.files roots
    @ List.concat_map
        (fun dir ->
          List.map (Filename.concat dir) [ "Cargo.toml"; "Cargo.lock" ])
        roots
  in
  List.sort_uniq String.compare files
  |> List.map (fun file ->
         ( file,
           try Some (Digest.to_hex (Digest.file file))
           with Sys_error _ -> None ))

let build_snapshot root deps =
  let stamp = workspace_stamp root deps in
  let workspace = Corpus.index_files (Workspace.files [ root ]) in
  let files = Hashtbl.create 256 in
  List.iter
    (fun (e : Sigdb.entry) -> Hashtbl.replace files e.meta.file ())
    workspace;
  let b = Engine.create () in
  let add (e : Sigdb.entry) = ignore (Engine.add b e.meta e.sg) in
  List.iter add workspace;
  List.iter
    (fun dep ->
      let entries =
        Corpus.entries_of_dep dep
        |> List.filter (fun (e : Sigdb.entry) ->
               not (Hashtbl.mem files e.meta.file))
      in
      List.iter add entries;
      List.iter
        (fun (e : Sigdb.entry) -> Hashtbl.replace files e.meta.file ())
        entries)
    deps;
  (Engine.freeze b, List.length workspace, stamp)

let install_snapshot deps (snapshot, count, stamp) =
  engine := snapshot;
  dependencies := deps;
  workspace_fns := count;
  workspace_test_fns := 0;
  for fid = 0 to count - 1 do
    if (Engine.get_meta snapshot fid).Engine.is_test then incr workspace_test_fns
  done;
  dep_count := List.length deps;
  dep_fns := Engine.size snapshot - count;
  indexed_stamp := stamp;
  Explorer.cached_pairs := None

let refresh_workspace () =
  if
    !enabled && !indexing_done
    && workspace_stamp !index_root !dependencies <> !indexed_stamp
  then
    let deps = Corpus.dependencies !index_root in
    install_snapshot deps (build_snapshot !index_root deps)

let index_async root =
  let root = try Unix.realpath root with _ -> root in
  index_root := root;
  let marker = Filename.concat root Workspace.marker_name in
  enabled := Sys.file_exists marker;
  if !enabled then Workspace.apply_marker_config marker;
  if not !enabled then indexing_done := true
  else
    ignore
      (Thread.create
         (fun () ->
           (try
              let deps = Corpus.dependencies root in
              let snapshot = build_snapshot root deps in
              with_dispatch (fun () -> install_snapshot deps snapshot)
            with e ->
              prerr_endline
                ("nonna mcp: indexing failed: " ^ Printexc.to_string e));
           with_dispatch (fun () -> indexing_done := true))
         ())

(* ── Argument plumbing ───────────────────────────────────────────────────── *)

let str_arg args k = try Some (JU.member k args |> JU.to_string) with _ -> None
let int_arg args k = try Some (JU.member k args |> JU.to_int) with _ -> None

(* A JSON array of strings (missing / wrong-typed -> []). Tolerates a bare
   string too, so an agent passing "src/" instead of ["src/"] still works. *)
let str_list_arg args k : string list =
  match (try Some (JU.member k args) with _ -> None) with
  | Some (`List l) ->
      List.filter_map (fun j -> try Some (JU.to_string j) with _ -> None) l
  | Some (`String s) when s <> "" -> [ s ]
  | _ -> []

let float_arg args k =
  try Some (JU.member k args |> JU.to_number) with _ -> None

let bool_arg args k =
  try Some (JU.member k args |> JU.to_bool) with _ -> None

let ext_of_language = function
  | "python" -> ".py"
  | "javascript" -> ".js"
  | "typescript" -> ".ts"
  | "go" -> ".go"
  | "java" -> ".java"
  | "c" -> ".c"
  | "cpp" | "c++" | "cxx" | "cc" -> ".cc"
  | "ocaml" | "ml" -> ".ml"
  | _ -> ".rs"

(* Drafted code is parsed via a temp file (deleted afterwards); its text is
   cached so reports can still quote source lines for drafted sides. *)
let code_cache : (string, string array) Hashtbl.t = Hashtbl.create 8

let slice (path : string) (first : int) (last : int) : string list =
  match Hashtbl.find_opt code_cache path with
  | Some lines ->
      let n = Array.length lines in
      List.init
        (max 0 (min last n - first + 1))
        (fun i -> lines.(first - 1 + i))
  | None -> Units.file_slice path first last

let units_of_code (code : string) (language : string) :
    string * Units.unit_info list =
  let path = Filename.temp_file "nonna_mcp" (ext_of_language language) in
  let oc = open_out path in
  output_string oc code;
  close_out oc;
  let units = try Units.units_of_file path with _ -> [] in
  (try Sys.remove path with _ -> ());
  Hashtbl.replace code_cache path
    (Array.of_list (String.split_on_char '\n' code));
  (path, units)

(* The unit of [file] at [line], else named [name], else its first one;
   with all of the file's units (the call-graph context scope, D17). *)
let unit_in_file (file : string) ~(line : int option) ~(name : string option)
    : Units.unit_info option * Units.unit_info list =
  let units = Units.units_of_file file in
  ( (match (line, name) with
    | Some line, _ -> Units.unit_at_in units line
    | None, Some name ->
        List.find_opt (fun (u : Units.unit_info) -> u.Units.uname = name) units
    | None, None -> ( match units with u :: _ -> Some u | [] -> None)),
    units )

(* Resolve one side of a tool call: <p>_code [+ <p>_language] or
   <p>_file + (<p>_line | <p>_name). Returns (label, unit, the units of its
   file or snippet — its call-graph context scope, D17) or an error. *)
let resolve_side (args : J.t) (p : string) :
    (string * Units.unit_info * Units.unit_info list, string) result =
  match str_arg args (p ^ "_code") with
  | Some code -> (
      let lang =
        Option.value (str_arg args (p ^ "_language")) ~default:"rust"
      in
      match units_of_code code lang with
      | _, (u :: _ as units) -> Ok (u.Units.uname ^ " (drafted)", u, units)
      | _, [] -> Error (p ^ "_code: no parseable function found"))
  | None -> (
      match str_arg args (p ^ "_file") with
      | None -> Error (p ^ ": provide " ^ p ^ "_code or " ^ p ^ "_file")
      | Some file -> (
          match
            unit_in_file file
              ~line:(int_arg args (p ^ "_line"))
              ~name:(str_arg args (p ^ "_name"))
          with
          | Some u, units ->
              Ok
                ( Printf.sprintf "%s (%s:%d)" u.Units.uname file
                    u.Units.uline_start,
                  u,
                  units )
          | None, _ -> Error (p ^ ": no matching function in " ^ file)))

(* ── Hits rendering ──────────────────────────────────────────────────────── *)

let snippet_cap = 40

let hit_block (h : Engine.hit) : string =
  let m = h.Engine.meta in
  let body = slice m.Engine.file m.Engine.line_start m.Engine.line_end in
  let body =
    if List.length body > snippet_cap then
      List.filteri (fun i _ -> i < snippet_cap) body @ [ "  /* ... */" ]
    else body
  in
  Printf.sprintf "## `%s` — %s:%d-%d\njaccard %.3f, containment %.3f\n```\n%s\n```"
    m.Engine.name m.Engine.file m.Engine.line_start m.Engine.line_end
    h.Engine.jaccard h.Engine.containment
    (String.concat "\n" body)

(* [among]: the units of [u]'s file or snippet, which its call-graph
   context (D17) resolves within — not the index. *)
let query_unit ~(among : Units.unit_info list) (u : Units.unit_info)
    ~(threshold : float) ~(top_k : int) ~(filter : Engine.query_filter) :
    Engine.hit list =
  let sg = Units.extract_in among u in
  if Signature.size sg < Units.min_features then []
  else
    let self_m = Units.meta_of u in
    Engine.query !engine sg ~threshold ~max_results:top_k ~filter
      ~accept:(fun m -> not (Engine.nests self_m m))

let hits_text (label : string) ~(filter : Engine.query_filter)
    (hits : Engine.hit list) : string =
  if hits = [] then
    Printf.sprintf
      "No similar function found for %s.\n(The index holds %d functions%s%s.)"
      label (Engine.size !engine)
      (if !indexing_done then "" else "; indexing is still running")
      (if (not filter.Engine.q_include_tests) && !workspace_test_fns > 0 then
         Printf.sprintf
           "; %d workspace test-code functions excluded — pass \
            include_tests:true to rank against them"
           !workspace_test_fns
       else "")
  else
    Printf.sprintf "Functions similar to %s:\n\n%s" label
      (String.concat "\n\n" (List.map hit_block hits))

(* ── diff_functions: signature algebra ───────────────────────────────────── *)

(* Nodes of [ga] none of whose round-hashes appear anywhere in [gb]:
   the parts of A with no structural counterpart in B, at any context
   depth. Grouped by source line. *)
let unique_nodes (ga : Dfg.graph) (gb : Dfg.graph) : (int * string) list =
  let bset = Hashtbl.create 256 in
  Array.iteri
    (fun i (d : Dfg.dnode) ->
      if d.Dfg.emit then
        Array.iter (fun round -> Hashtbl.replace bset round.(i) ()) gb.Dfg.rounds)
    gb.Dfg.dnodes;
  let out = ref [] in
  Array.iteri
    (fun i (d : Dfg.dnode) ->
      if d.Dfg.emit then
        let matched =
          Array.exists (fun round -> Hashtbl.mem bset round.(i)) ga.Dfg.rounds
        in
        if not matched then out := (d.Dfg.dline, d.Dfg.dlabel) :: !out)
    ga.Dfg.dnodes;
  List.sort compare !out

let side_report (title : string) (uniq : (int * string) list)
    (u : Units.unit_info) : string =
  if uniq = [] then Printf.sprintf "%s: nothing — fully covered." title
  else
    let by_line = Hashtbl.create 16 in
    List.iter
      (fun (l, lbl) ->
        Hashtbl.replace by_line l
          (lbl :: Option.value (Hashtbl.find_opt by_line l) ~default:[]))
      uniq;
    let lines =
      Hashtbl.fold (fun l lbls acc -> (l, lbls) :: acc) by_line []
      |> List.sort compare
      |> List.map (fun (l, lbls) ->
             let src =
               if l > 0 then
                 match slice u.Units.ufile l l with
                 | [ s ] -> String.trim s
                 | _ -> ""
               else ""
             in
             Printf.sprintf "  line %d: %s%s" l
               (String.concat ", " (List.sort_uniq compare lbls))
               (if src = "" then "" else Printf.sprintf "\n    > %s" src))
    in
    Printf.sprintf "%s:\n%s" title (String.concat "\n" lines)

let diff_functions (args : J.t) : J.t =
  match (resolve_side args "a", resolve_side args "b") with
  | Error e, _ | _, Error e -> tool_text ~is_error:true e
  | Ok (la, ua, units_a), Ok (lb, ub, units_b) ->
      let sa = Units.extract_in units_a ua in
      let sb = Units.extract_in units_b ub in
      let ga =
        Dfg.graph_of
          ~fc:(Dfg.base_cfg_for ua.Units.ulang)
          ~iters:(Dfg.iters_for (Some ua.Units.ulang))
          ua.Units.ucfg
      in
      let gb =
        Dfg.graph_of
          ~fc:(Dfg.base_cfg_for ub.Units.ulang)
          ~iters:(Dfg.iters_for (Some ub.Units.ulang))
          ub.Units.ucfg
      in
      let text =
        String.concat "\n\n"
          [
            Printf.sprintf
              "A = %s\nB = %s\n\nA ∩ B: jaccard %.3f | A⊂B containment %.3f \
               | B⊂A containment %.3f"
              la lb (Signature.jaccard sa sb)
              (Signature.containment ~query:sa sb)
              (Signature.containment ~query:sb sa);
            side_report "A − B (only in A — for a bug/fix pair: ≈ the bug)"
              (unique_nodes ga gb) ua;
            side_report "B − A (only in B — for a bug/fix pair: ≈ the fix)"
              (unique_nodes gb ga) ub;
          ]
      in
      tool_text text

(* Build the single-query match filter from tool args, mirroring the dedup
   knobs. [with_name] is false for query_similar, where `name` already selects
   the function to query and so can't double as a result filter. Unlike
   find_duplicates, the score gate defaults to max(j,c) and matches against
   deps/std by default (a single query is cheap; the workspace-only scope is
   opt-in via include_deps:false). Test code is excluded unless include_tests
   is set: a drafted function wants a production twin, not a test case. *)
let query_filter_of_args ?(with_name = true) (args : J.t) : Engine.query_filter =
  let include_deps = Option.value (bool_arg args "include_deps") ~default:true in
  {
    Engine.q_by_max =
      (match str_arg args "metric" with Some "jaccard" -> false | _ -> true);
    q_name_sub =
      (if with_name then Option.value (str_arg args "name") ~default:"" else "");
    q_include_paths = str_list_arg args "include";
    q_exclude_paths = str_list_arg args "exclude";
    q_min_lines = Option.value (int_arg args "min_lines") ~default:0;
    q_min_features = Option.value (int_arg args "min_features") ~default:0;
    q_scope = (if include_deps then 0 else !workspace_fns);
    q_include_tests =
      Option.value (bool_arg args "include_tests") ~default:false;
  }

(* ── find_duplicates: whole-corpus clone pairs (no query function) ───────── *)

let dup_line (p : Engine.pair) : string =
  Printf.sprintf "j %.3f c %.3f  %s (%s:%d-%d)  <->  %s (%s:%d-%d)" p.Engine.j
    p.Engine.c p.Engine.a.Engine.name p.Engine.a.Engine.file
    p.Engine.a.Engine.line_start p.Engine.a.Engine.line_end p.Engine.b.Engine.name
    p.Engine.b.Engine.file p.Engine.b.Engine.line_start p.Engine.b.Engine.line_end

let find_duplicates (args : J.t) : J.t =
  if not !indexing_done then
    tool_text ~is_error:true
      (Printf.sprintf
         "indexing still in progress (%d functions so far) — `find_duplicates` \
          scans the index, so wait until `status` reports done."
         (Engine.size !engine))
  else if !workspace_fns = 0 then
    tool_text
      (Printf.sprintf
         "No workspace functions indexed under %s, so there is nothing to dedup."
         !index_root)
  else
    (* Default to WORKSPACE-internal clone pairs. The index holds the workspace
       first, then deps/std (often hundreds of thousands of fns) — an all-pairs
       scan over that is the dedup view nobody wants and takes minutes. The
       outer (a) side is always the workspace prefix; include_deps only widens
       the (b) side, so cost stays O(workspace · candidates). *)
    let include_deps = Option.value (bool_arg args "include_deps") ~default:false in
    let include_tests =
      Option.value (bool_arg args "include_tests") ~default:false
    in
    (* group:true lists clone classes (D18): every pair feeds them, and
       limit caps the classes listed instead *)
    let group = Option.value (bool_arg args "group") ~default:false in
    let limit = Option.value (int_arg args "limit") ~default:50 in
    let flt =
      {
        Engine.threshold =
          Option.value (float_arg args "threshold") ~default:0.7;
        by_max =
          (match str_arg args "metric" with Some "max" -> true | _ -> false);
        name_sub = Option.value (str_arg args "name") ~default:"";
        include_paths = str_list_arg args "include";
        exclude_paths = str_list_arg args "exclude";
        min_lines = Option.value (int_arg args "min_lines") ~default:0;
        min_features = Option.value (int_arg args "min_features") ~default:0;
        limit = (if group then 0 else limit);
        scope_a = !workspace_fns;
        scope_b = (if include_deps then 0 else !workspace_fns);
        include_tests;
      }
    in
    (* A wall-clock budget backs up client cancellation: a scan that outlives
       it returns what it found so far, flagged partial, rather than holding
       the (serialized) dispatcher for everyone else. *)
    let budget = Option.value (float_arg args "time_budget_s") ~default:120. in
    let deadline = Unix.gettimeofday () +. budget in
    let stop = !stop_requested in
    let scan =
      Engine.duplicates_scan !engine flt ~should_stop:(fun () ->
          stop () || (budget > 0. && Unix.gettimeofday () > deadline))
    in
    if stop () then raise Cancelled;
    let pairs = scan.Engine.pairs in
    let scope_desc =
      (if include_deps then "workspace × (workspace + deps/std)"
       else
         Printf.sprintf "workspace-internal (%d fns; pass include_deps to widen)"
           !workspace_fns)
      ^
      if include_tests then ""
      else
        Printf.sprintf
          "; %d workspace fns are test code and were excluded (include_tests \
           to keep them)"
          !workspace_test_fns
    in
    let partial =
      if scan.Engine.complete then ""
      else
        Printf.sprintf
          "\n\nPARTIAL: the %.0fs time budget stopped the scan after %d of %d \
           functions. Narrow it (include/name/min_lines) or raise time_budget_s."
          budget scan.Engine.scanned scan.Engine.total
    in
    if group then
      let cs =
        Clones.classes ~threshold:flt.Engine.threshold ~by_max:flt.Engine.by_max
          pairs
      in
      (* the line total covers the workspace fns the scan's side gates pass *)
      let scanned =
        List.init !workspace_fns (Engine.get_meta !engine)
        |> List.filter (Engine.side_ok flt)
      in
      let shown = if limit > 0 then List.filteri (fun i _ -> i < limit) cs else cs in
      tool_text
        (Printf.sprintf "%s, %s.%s%s%s" (Clones.summary cs ~scanned) scope_desc
           (if List.length shown < List.length cs then
              Printf.sprintf " Showing the first %d classes (limit)."
                (List.length shown)
            else "")
           (String.concat ""
              (List.mapi
                 (fun k c ->
                   "\n\n" ^ String.concat "\n" (Clones.render ~expand:false (k + 1) c))
                 shown))
           partial)
    else if pairs = [] then
      tool_text
        (Printf.sprintf "No duplicate pairs above the given filters, %s.%s"
           scope_desc partial)
    else
      tool_text
        (Printf.sprintf "%d duplicate pair(s), %s:\n\n%s%s" (List.length pairs)
           scope_desc
           (String.concat "\n" (List.map dup_line pairs))
           partial)

(* ── Tools ───────────────────────────────────────────────────────────────── *)

let schema (props : (string * string * string) list) (required : string list) :
    J.t =
  `Assoc
    [
      ("type", `String "object");
      ( "properties",
        `Assoc
          (List.map
             (fun (name, ty, descr) ->
               let spec =
                 if ty = "string[]" then
                   [
                     ("type", `String "array");
                     ("items", `Assoc [ ("type", `String "string") ]);
                     ("description", `String descr);
                   ]
                 else [ ("type", `String ty); ("description", `String descr) ]
               in
               (name, `Assoc spec))
             props) );
      ("required", `List (List.map (fun r -> `String r) required));
    ]

let side_props p what =
  [
    (p ^ "_code", "string", what ^ " as a code string (instead of a file)");
    (p ^ "_language", "string",
     "language of " ^ p ^ "_code: rust|python|javascript|typescript|go|java|c|cpp|ocaml (default rust)");
    (p ^ "_file", "string", "absolute path containing " ^ what);
    (p ^ "_line", "integer", "a line inside the function (1-based)");
    (p ^ "_name", "string", "the function's name (alternative to line)");
  ]

(* Hidden search keywords: Claude Code's ToolSearch scores a word-boundary
   match in `_meta["anthropic/searchHint"]` (+4, between a name part and a
   description word) but never shows it to the model. Each word can also
   pull the tool into an unrelated search, so a hint carries only the
   vocabulary of the job its own tool does. *)
let search_hint (hint : string) : string * J.t =
  ("_meta", `Assoc [ ("anthropic/searchHint", `String hint) ])

(* Descriptions lead with purpose and trigger: Cursor keeps 200 chars, some
   Codex runs 140; Claude Code cuts at 2048. *)
let tool_defs : J.t =
  `List
    [
      `Assoc
        [
          ("name", `String "find_similar");
          ("title", `String "Check for an existing equivalent function");
          ( "description",
            `String
              "Check whether a function you just wrote already exists. Returns \
               existing functions (workspace, dependencies, std) that are \
               structurally similar to it: duplicates, clones, copy-paste \
               variants, renamed reimplementations, ranked with \
               jaccard/containment scores and source. Use it after writing a \
               new function or helper, before moving on. Give the function as \
               `code` (+ `language`). A match near 1.0: call the existing \
               function instead of keeping the duplicate. No match is not \
               proof that none exists." );
          search_hint
            "reuse existing function functions implementation helper already \
             exists duplicate clone similar similarity";
          ( "inputSchema",
            schema
              [
                ("code", "string", "the drafted function source code");
                ("language", "string",
                 "rust|python|javascript|typescript|go|java|c|cpp|ocaml (default rust)");
                ("top_k", "integer", "max results (default 5)");
                ("threshold", "number",
                 "min similarity on the gated metric (default 0.25)");
                ("metric", "string",
                 "gate on max(jaccard,containment) (default) or jaccard — pass \
                  \"jaccard\" to require the same shape, not just a subset");
                ("name", "string",
                 "only matches whose function name contains this substring");
                ("include", "string[]",
                 "restrict matches to files whose path contains one of these \
                  substrings (case-insensitive), e.g. [\"crates/foo/\"]");
                ("exclude", "string[]",
                 "drop matches whose file path contains one of these substrings, \
                  e.g. [\"/tests/\", \"vendor\"]");
                ("min_lines", "integer", "drop matches with fewer code lines");
                ("min_features", "integer", "drop matches with fewer features");
                ("include_deps", "boolean",
                 "rank against deps/std too (default true; false = workspace only)");
                ("include_tests", "boolean",
                 "also rank against test code (#[cfg(test)]/#[test] fns, test \
                  files); default false");
              ]
              [ "code" ] );
        ];
      `Assoc
        [
          ("name", `String "query_similar");
          ("title", `String "Find functions similar to one on disk");
          ( "description",
            `String
              "Find existing functions similar to one already in a file: \
               duplicates, clones and copy-paste variants of the function at \
               `file` + `line` or `name`. Use it on a function already on \
               disk, or to locate copies and refactor candidates in the \
               repository; for code not yet saved use find_similar. Returns \
               ranked matches with jaccard/containment scores and source. No \
               match is not proof that none exists." );
          search_hint "similar existing function clone duplicate file line";
          ( "inputSchema",
            schema
              ([
                 ("file", "string", "absolute path of the source file");
                 ("line", "integer", "a line inside the function (1-based)");
                 ("name", "string", "the function's name (alternative to line)");
                 ("top_k", "integer", "max results (default 5)");
                 ("threshold", "number",
                  "min similarity on the gated metric (default 0.25)");
                 ("metric", "string",
                  "gate on max(jaccard,containment) (default) or jaccard — pass \
                   \"jaccard\" to require the same shape, not just a subset");
                 ("include", "string[]",
                  "restrict matches to files whose path contains one of these \
                   substrings (case-insensitive), e.g. [\"crates/foo/\"]");
                 ("exclude", "string[]",
                  "drop matches whose file path contains one of these substrings, \
                   e.g. [\"/tests/\", \"vendor\"]");
                 ("min_lines", "integer", "drop matches with fewer code lines");
                 ("min_features", "integer", "drop matches with fewer features");
                 ("include_deps", "boolean",
                  "rank against deps/std too (default true; false = workspace \
                   only)");
                 ("include_tests", "boolean",
                  "also rank against test code (#[cfg(test)]/#[test] fns, test \
                   files); default false");
               ])
              [ "file" ] );
        ];
      `Assoc
        [
          ("name", `String "diff_functions");
          ("title", `String "Compare two functions structurally");
          ( "description",
            `String
              "Compare two functions and show where they differ: similarity \
               scores plus the dataflow regions unique to each side, by \
               source line. Use it to check a near-duplicate is really \
               equivalent before replacing it, or to localize a bug against \
               its fixed version (A−B ≈ bug, B−A ≈ fix). Each side is given \
               as code (<side>_code) or as <side>_file plus <side>_line or \
               <side>_name." );
          search_hint "compare diff two functions difference equivalent bug fix";
          ( "inputSchema",
            schema (side_props "a" "function A" @ side_props "b" "function B") []
          );
        ];
      `Assoc
        [
          ("name", `String "find_duplicates");
          ("title", `String "List duplicate function pairs in the workspace");
          ( "description",
            `String
              "Find duplicate functions across the workspace: clone pairs, \
               copy-pasted and near-identical functions, no query function \
               needed. Use when refactoring, deduplicating or reviewing for \
               duplicated code. Each pair reports jaccard and containment \
               with both locations. Scans \
               workspace functions by default; include_deps also matches them \
               against dependencies and std. Test code is skipped unless \
               include_tests is set, since test cases duplicate each other by \
               design. Known false positives of rename invariance: \
               enum-to-string match tables, one-query DB wrappers and \
               cfg(unix)/cfg(not(unix)) twins score 1.0 against their siblings \
               (the literals setting in .nonna separates most of them); \
               min_lines and min_features drop the one-line closures. With \
               group:true it returns clone classes instead (connected groups \
               of the pairs, each with its score range, members and \
               duplicated SLOC) under a summary of the workspace's \
               duplicated share." );
          search_hint
            "dedup deduplicate duplicates duplicated clone clones copy-paste \
             refactor cleanup classes";
          ( "inputSchema",
            schema
              [
                ("threshold", "number",
                 "min similarity on the gated metric (default 0.7)");
                ("metric", "string",
                 "gate the threshold on jaccard (default) or max(jaccard,containment) \
                  — pass \"max\" to also surface subset/'call it instead' dupes");
                ("name", "string",
                 "only pairs where either function name contains this substring");
                ("include", "string[]",
                 "restrict to files whose path contains one of these substrings \
                  (case-insensitive) — both sides of a pair must match, e.g. \
                  [\"crates/foo/\"] to dedup within one crate");
                ("exclude", "string[]",
                 "drop any pair where either file's path contains one of these \
                  substrings, e.g. [\"/tests/\", \"generated\"]");
                ("min_lines", "integer",
                 "drop pairs whose smaller side has fewer code lines");
                ("min_features", "integer",
                 "drop pairs whose smaller side has fewer features");
                ("limit", "integer",
                 "max pairs returned, jaccard desc (default 50); with group, \
                  max classes");
                ("group", "boolean",
                 "return clone classes (groups of mutually duplicate \
                  functions, largest duplicated SLOC first) plus a \
                  duplicated-SLOC summary instead of pairs (default false)");
                ("include_deps", "boolean",
                 "also match workspace fns against deps/std (default false: \
                  workspace-internal only)");
                ("include_tests", "boolean",
                 "keep pairs with a test-code side (default false)");
                ("time_budget_s", "number",
                 "wall-clock budget; a scan that outlives it returns partial \
                  results marked PARTIAL (default 120, 0 = unlimited)");
              ]
              [] );
        ];
      `Assoc
        [
          ("name", `String "status");
          ("title", `String "Show nonna index status");
          ( "description",
            `String
              "Show nonna's index state: workspace root, how many functions \
               are indexed (workspace, test code, dependencies), whether \
               background indexing has finished, and the literals setting." );
          ("inputSchema", schema [] []);
        ];
    ]

let call_tool (name : string) (args : J.t) : J.t =
  match name with
  | "status" ->
      if not !enabled then
        tool_text
          (Printf.sprintf
             "root: %s\nnonna is disabled here: no `%s` marker file at the \
              workspace root, so nothing is indexed. Create one (e.g. `touch \
              %s`) and restart the session to enable structural search."
             !index_root Workspace.marker_name Workspace.marker_name)
      else
        tool_text
          (Printf.sprintf
             "root: %s\nindexed functions: %d (%d workspace, of which %d test \
              code; %d from %d deps/std)\nindexing: %s\nliterals: %s"
             !index_root (Engine.size !engine) !workspace_fns
             !workspace_test_fns !dep_fns !dep_count
             (if !indexing_done then "done" else "in progress")
             (literals_desc ()))
  | ("find_similar" | "query_similar" | "diff_functions" | "find_duplicates")
    when not !enabled ->
      tool_text ~is_error:true
        (Printf.sprintf
           "nonna is disabled for %s: no `%s` marker file at the workspace \
            root. Create one (`touch %s`) and restart the session to enable it."
           !index_root Workspace.marker_name Workspace.marker_name)
  | "find_similar" -> (
      match str_arg args "code" with
      | None -> tool_text ~is_error:true "missing required argument: code"
      | Some code -> (
          let lang = Option.value (str_arg args "language") ~default:"rust" in
          let top_k = Option.value (int_arg args "top_k") ~default:5 in
          let threshold =
            Option.value (float_arg args "threshold") ~default:0.25
          in
          let filter = query_filter_of_args args in
          match units_of_code code lang with
          | _, [] ->
              tool_text ~is_error:true
                ("no parseable " ^ lang ^ " function in `code`")
          | _, units ->
              units
              |> List.filteri (fun i _ -> i < 5)
              |> List.map (fun (u : Units.unit_info) ->
                     hits_text
                       ("drafted `" ^ u.Units.uname ^ "`")
                       ~filter
                       (query_unit ~among:units u ~threshold ~top_k ~filter))
              |> String.concat "\n\n---\n\n" |> tool_text))
  | "query_similar" -> (
      match str_arg args "file" with
      | None -> tool_text ~is_error:true "missing required argument: file"
      | Some file -> (
          match
            unit_in_file file ~line:(int_arg args "line")
              ~name:(str_arg args "name")
          with
          | None, _ -> tool_text ~is_error:true ("no matching function in " ^ file)
          | Some u, units ->
              let top_k = Option.value (int_arg args "top_k") ~default:5 in
              let threshold =
                Option.value (float_arg args "threshold") ~default:0.25
              in
              let filter = query_filter_of_args ~with_name:false args in
              tool_text
                (hits_text
                   (Printf.sprintf "`%s` (%s:%d)" u.Units.uname file
                      u.Units.uline_start)
                   ~filter
                   (query_unit ~among:units u ~threshold ~top_k ~filter)
                ^
                if u.Units.utest && not filter.Engine.q_include_tests then
                  Printf.sprintf
                    "\n\n(`%s` is test code; other test functions are excluded \
                     from matches by default — pass include_tests:true to rank \
                     against them.)"
                    u.Units.uname
                else "")))
  | "diff_functions" -> diff_functions args
  | "find_duplicates" -> find_duplicates args
  | _ -> tool_text ~is_error:true ("unknown tool: " ^ name)

(* ── Dispatch ────────────────────────────────────────────────────────────── *)

(* Server-level context for the LLM (MCP `instructions`): when to reach for
   nonna, by bare tool name, and how to read the numbers. Claude Code shows
   them before any tool search but does not score them; Codex folds them
   into every tool's search text, so they stay on domain words (no generic
   search/file/lookup/docs) and tool-specific caveats live in each tool's
   description (see ~/work/hr/mcp-tool-selection/docs/tool-naming.md).
   Computed per initialize: the literals line depends on the workspace
   config. *)
let instructions () =
  "nonna answers \"does code like this already exist?\" for this workspace, \
   its dependencies and std, by structure, not by name. Use it (1) right \
   after writing a new function or helper, before moving on: `find_similar` \
   with its code (or `query_similar` with file+line|name). A match means \
   call the existing function instead of keeping a duplicate. (2) When \
   refactoring or deduplicating: `find_duplicates` lists clone pairs, no \
   query needed (`group: true` for clone classes and the duplicated share). \
   (3) To see where two similar functions differ, e.g. before replacing one \
   with the other: `diff_functions`. Queries are code, not natural \
   language. Matching ignores names, identifiers and concrete types; literal \
   values "
  ^ (if Dfg.literals_on () then
       "are hashed in this workspace (literals = true), so functions that \
        differ only in constants do not match."
     else
       "use language defaults: integer values are hashed, Python, C and C++ \
        also hash strings, Rust and OCaml hash string chunks (an edited \
        string still partly matches); float values are ignored.")
  ^ " Scores: jaccard ≈ 1.0 = same function up to renaming; containment ≈ \
     1.0 = the match does everything yours does (maybe more), a strong \
     \"call it instead\" signal even at moderate jaccard. The index builds \
     in the background; results say when it is still incomplete. Test code \
     is excluded unless include_tests:true."

(* [Reply]: a response to send now. [Silent]: a notification, nothing to
   send. [Deferred]: a tool call — the transport decides whether to run it
   inline (HTTP: the connection's own thread) or on a fresh thread (stdio:
   so the read loop stays free to see a cancellation for it). *)
type reply = Reply of J.t | Silent | Deferred of (unit -> J.t)

let result_msg (id : J.t) (result : J.t) : J.t =
  `Assoc [ ("jsonrpc", `String "2.0"); ("id", id); ("result", result) ]

let error_msg (id : J.t) (code : int) (msg : string) : J.t =
  `Assoc
    [
      ("jsonrpc", `String "2.0");
      ("id", id);
      ("error", `Assoc [ ("code", `Int code); ("message", `String msg) ]);
    ]

let handle_message (msg : J.t) : reply =
  let meth = try JU.member "method" msg |> JU.to_string with _ -> "" in
  let id = JU.member "id" msg in
  let params = JU.member "params" msg in
  match meth with
  | "initialize" ->
      let proto =
        try JU.member "protocolVersion" params |> JU.to_string
        with _ -> "2024-11-05"
      in
      Reply
        (result_msg id
           (`Assoc
              [
                ("protocolVersion", `String proto);
                ("capabilities", `Assoc [ ("tools", `Assoc []) ]);
                ( "serverInfo",
                  `Assoc
                    [ ("name", `String "nonna"); ("version", `String "0.1") ] );
                ("instructions", `String (instructions ()));
              ]))
  | "notifications/initialized" -> Silent
  | "notifications/cancelled" ->
      cancel_request (JU.member "requestId" params);
      Silent
  | "ping" -> Reply (result_msg id (`Assoc []))
  | "tools/list" -> Reply (result_msg id (`Assoc [ ("tools", tool_defs) ]))
  | "tools/call" ->
      let name = try JU.member "name" params |> JU.to_string with _ -> "" in
      let args = JU.member "arguments" params in
      Deferred
        (fun () ->
          with_cancel_flag id (fun flag ->
              with_dispatch (fun () ->
                  if !flag then error_msg id (-32800) "request cancelled"
                  else (
                    stop_requested := (fun () -> !flag);
                    Fun.protect
                      ~finally:(fun () ->
                        stop_requested := (fun () -> false);
                        Hashtbl.clear code_cache)
                      (fun () ->
                        let r =
                          try
                            refresh_workspace ();
                            call_tool name args
                          with
                          | Cancelled ->
                              tool_text ~is_error:true "request cancelled"
                          | e -> tool_text ~is_error:true (Printexc.to_string e)
                        in
                        result_msg id r)))))
  | _ ->
      if id = `Null then Silent
      else Reply (error_msg id (-32601) ("unhandled: " ^ meth))

(* ── stdio transport (newline-delimited JSON) ────────────────────────────── *)

let run (root : string) : unit =
  index_async root;
  (* Deferred calls answer from their own threads; on EOF, drain them before
     exiting so a client that pipes requests and closes stdin (the smoke
     test; any batch driver) still gets every reply. *)
  let pending = ref 0 in
  let pending_mutex = Mutex.create () in
  let pending_done = Condition.create () in
  let spawn run =
    Mutex.lock pending_mutex;
    incr pending;
    Mutex.unlock pending_mutex;
    ignore
      (Thread.create
         (fun () ->
           (try send (run ())
            with e -> prerr_endline ("nonna mcp: " ^ Printexc.to_string e));
           Mutex.lock pending_mutex;
           decr pending;
           Condition.broadcast pending_done;
           Mutex.unlock pending_mutex)
         ())
  in
  let rec loop () =
    match input_line stdin with
    | line ->
        (if String.trim line <> "" then
           match J.from_string line with
           | msg -> (
               match handle_message msg with
               | Reply resp -> send resp
               | Silent -> ()
               | Deferred run -> spawn run
               | exception e ->
                   prerr_endline ("nonna mcp: " ^ Printexc.to_string e))
           | exception _ -> ());
        loop ()
    | exception End_of_file ->
        Mutex.lock pending_mutex;
        while !pending > 0 do
          Condition.wait pending_done pending_mutex
        done;
        Mutex.unlock pending_mutex
  in
  loop ()

(* ── HTTP transport (MCP streamable HTTP, the request/response subset) ───── *)
(* One warm index shared by every agent session — stdio spawns a cold server
   per session. POST /mcp with a JSON-RPC message; responses are plain JSON
   (no SSE: tools are simple request/response). *)

let http_response (oc : out_channel) ?(status = "200 OK")
    ?(content_type = "application/json") (body : string) : unit =
  Printf.fprintf oc
    "HTTP/1.1 %s\r\nContent-Type: %s\r\nContent-Length: %d\r\nConnection: \
     close\r\n\r\n%s"
    status content_type (String.length body) body;
  flush oc

(* "/api/fn?file=..&start=1" -> ("/api/fn", [("file", ".."); ("start", "1")]) *)
let split_path_query (target : string) : string * (string * string) list =
  match String.index_opt target '?' with
  | None -> (target, [])
  | Some i ->
      let path = String.sub target 0 i in
      let q = String.sub target (i + 1) (String.length target - i - 1) in
      let params =
        String.split_on_char '&' q
        |> List.filter_map (fun kv ->
               match String.index_opt kv '=' with
               | None -> None
               | Some e ->
                   Some
                     ( String.sub kv 0 e,
                       Wire.percent_decode ~plus_as_space:true
                         (String.sub kv (e + 1) (String.length kv - e - 1)) ))
      in
      (path, params)

(* GET routes: the duplication explorer UI + its JSON API. *)
let handle_get (oc : out_channel) (target : string) : unit =
  let path, params = split_path_query target in
  match path with
  | "/" | "/index.html" ->
      http_response oc ~content_type:"text/html; charset=utf-8" Explorer.html
  | "/api/pairs" ->
      http_response oc
        (J.to_string (Explorer.pairs_json !engine ~ready:!indexing_done))
  | "/api/fn" -> (
      match
        ( List.assoc_opt "file" params,
          Option.bind (List.assoc_opt "start" params) int_of_string_opt,
          Option.bind (List.assoc_opt "end" params) int_of_string_opt )
      with
      | Some file, Some start, Some stop ->
          http_response oc
            (J.to_string (Explorer.fn_json !engine ~file ~start ~stop))
      | _ ->
          http_response oc ~status:"400 Bad Request"
            {|{"error":"file, start, end required"}|})
  | _ ->
      http_response oc ~content_type:"text/plain"
        (Printf.sprintf "nonna mcp: %d functions indexed (%s)\n"
           (Engine.size !engine)
           (if !indexing_done then "ready" else "indexing"))

let handle_http_conn (fd : Unix.file_descr) : unit =
  let ic = Unix.in_channel_of_descr fd in
  let oc = Unix.out_channel_of_descr fd in
  (try
     let request_line = input_line ic in
     let clen = Wire.read_content_length ic in
     if
       String.length request_line >= 4
       && String.uppercase_ascii (String.sub request_line 0 4) = "POST"
     then
       let body = really_input_string ic clen in
       match handle_message (J.from_string body) with
       | Reply resp -> http_response oc (J.to_string resp)
       | Deferred run -> http_response oc (J.to_string (run ()))
       | Silent -> http_response oc ~status:"202 Accepted" ""
       | exception e ->
           http_response oc ~status:"500 Internal Server Error"
             (J.to_string (error_msg `Null (-32700) (Printexc.to_string e)))
     else
       (* GET <target> HTTP/1.1 -> explorer UI / JSON API *)
       let target =
         match String.split_on_char ' ' request_line with
         | _ :: t :: _ -> t
         | _ -> "/"
       in
       with_dispatch (fun () ->
           refresh_workspace ();
           handle_get oc target)
   with _ -> ());
  try Unix.close fd with _ -> ()

let serve (root : string) (port : int) : unit =
  (* a client hanging up mid-response must be an EPIPE in that connection's
     thread (already caught there), not a SIGPIPE killing the process *)
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  index_async root;
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
  Unix.listen sock 16;
  Printf.eprintf
    "nonna: explorer http://127.0.0.1:%d/ | mcp http://127.0.0.1:%d/mcp \
(root: %s)\n%!"
    port port root;
  while true do
    let fd, _ = Unix.accept sock in
    ignore (Thread.create handle_http_conn fd)
  done
