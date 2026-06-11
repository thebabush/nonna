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

let reply (id : J.t) (result : J.t) : unit =
  send (`Assoc [ ("jsonrpc", `String "2.0"); ("id", id); ("result", result) ])

let reply_error (id : J.t) (code : int) (msg : string) : unit =
  send
    (`Assoc
      [
        ("jsonrpc", `String "2.0");
        ("id", id);
        ( "error",
          `Assoc [ ("code", `Int code); ("message", `String msg) ] );
      ])

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
let dep_count = ref 0
let dep_fns = ref 0

let index_async (root : string) : unit =
  index_root := root;
  ignore
    (Thread.create
       (fun () ->
         (try
            (* the BUILDER never escapes this thread; query paths only ever
               see frozen snapshots (the type system enforces it) *)
            let b = Engine.create () in
            Units.units_of_paths [ root ]
            |> List.iter (fun (u : Units.unit_info) ->
                   let sg =
                     Signature.extract
                       ~ext:(Filename.extension u.Units.ufile)
                       u.Units.ucfg
                   in
                   if Signature.size sg >= Units.min_features then
                     ignore
                       (Engine.add b
                          {
                            Engine.name = u.Units.uname;
                            file = u.Units.ufile;
                            line_start = u.Units.uline_start;
                            line_end = u.Units.uline_end;
                          }
                          sg));
            workspace_fns := Engine.built b;
            (* expose the workspace early; deps stream in behind it *)
            engine := Engine.freeze b;
            (* D3: transitive cargo deps + std, from cached sigdbs *)
            let nd, nf = Corpus.add_deps b root in
            dep_count := nd;
            dep_fns := nf;
            engine := Engine.freeze b
          with e ->
            prerr_endline ("nonna mcp: indexing failed: " ^ Printexc.to_string e));
         indexing_done := true)
       ())

(* ── Argument plumbing ───────────────────────────────────────────────────── *)

let str_arg args k = try Some (JU.member k args |> JU.to_string) with _ -> None
let int_arg args k = try Some (JU.member k args |> JU.to_int) with _ -> None

let float_arg args k =
  try Some (JU.member k args |> JU.to_number) with _ -> None

let ext_of_language = function
  | "python" -> ".py"
  | "javascript" -> ".js"
  | "typescript" -> ".ts"
  | "go" -> ".go"
  | "java" -> ".java"
  | "c" -> ".c"
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

(* Resolve one side of a tool call: <p>_code [+ <p>_language] or
   <p>_file + (<p>_line | <p>_name). Returns (label, unit) or an error. *)
let resolve_side (args : J.t) (p : string) :
    (string * Units.unit_info, string) result =
  match str_arg args (p ^ "_code") with
  | Some code -> (
      let lang =
        Option.value (str_arg args (p ^ "_language")) ~default:"rust"
      in
      match units_of_code code lang with
      | _, u :: _ -> Ok (u.Units.uname ^ " (drafted)", u)
      | _, [] -> Error (p ^ "_code: no parseable function found"))
  | None -> (
      match str_arg args (p ^ "_file") with
      | None -> Error (p ^ ": provide " ^ p ^ "_code or " ^ p ^ "_file")
      | Some file -> (
          let unit =
            match (int_arg args (p ^ "_line"), str_arg args (p ^ "_name")) with
            | Some line, _ -> Units.unit_at file line
            | None, Some name ->
                Units.units_of_file file
                |> List.find_opt (fun (u : Units.unit_info) ->
                       u.Units.uname = name)
            | None, None -> (
                match Units.units_of_file file with u :: _ -> Some u | [] -> None)
          in
          match unit with
          | Some u ->
              Ok (Printf.sprintf "%s (%s:%d)" u.Units.uname file
                    u.Units.uline_start, u)
          | None -> Error (p ^ ": no matching function in " ^ file)))

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

let query_unit (u : Units.unit_info) ~(threshold : float) ~(top_k : int) :
    Engine.hit list =
  let sg = Signature.extract ~ext:(Filename.extension u.Units.ufile) u.Units.ucfg in
  if Signature.size sg < Units.min_features then []
  else
    let self_m =
      {
        Engine.name = u.Units.uname;
        file = u.Units.ufile;
        line_start = u.Units.uline_start;
        line_end = u.Units.uline_end;
      }
    in
    Engine.query !engine sg ~threshold ~max_results:(top_k + 1)
    |> List.filter (fun (h : Engine.hit) ->
           not (Engine.nests self_m h.Engine.meta))
    |> List.filteri (fun i _ -> i < top_k)

let hits_text (label : string) (hits : Engine.hit list) : string =
  if hits = [] then
    Printf.sprintf
      "No similar function found for %s.\n(The index holds %d functions%s.)"
      label (Engine.size !engine)
      (if !indexing_done then "" else "; indexing is still running")
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
  | Ok (la, ua), Ok (lb, ub) ->
      let sa = Signature.extract ~ext:(Filename.extension ua.Units.ufile) ua.Units.ucfg in
      let sb = Signature.extract ~ext:(Filename.extension ub.Units.ufile) ub.Units.ucfg in
      let ga =
        Dfg.graph_of
          ~fc:(Dfg.base_cfg_for (Filename.extension ua.Units.ufile))
          ua.Units.ucfg
      in
      let gb =
        Dfg.graph_of
          ~fc:(Dfg.base_cfg_for (Filename.extension ub.Units.ufile))
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
               ( name,
                 `Assoc
                   [
                     ("type", `String ty); ("description", `String descr);
                   ] ))
             props) );
      ("required", `List (List.map (fun r -> `String r) required));
    ]

let side_props p what =
  [
    (p ^ "_code", "string", what ^ " as a code string (instead of a file)");
    (p ^ "_language", "string",
     "language of " ^ p ^ "_code: rust|python|javascript|typescript|go (default rust)");
    (p ^ "_file", "string", "absolute path containing " ^ what);
    (p ^ "_line", "integer", "a line inside the function (1-based)");
    (p ^ "_name", "string", "the function's name (alternative to line)");
  ]

let tool_defs : J.t =
  `List
    [
      `Assoc
        [
          ("name", `String "find_similar");
          ( "description",
            `String
              "Find existing functions structurally similar to a drafted \
               one. Call this BEFORE committing a freshly written function: \
               if a strong match exists (jaccard or containment near 1), \
               prefer calling the existing function instead of adding a \
               duplicate." );
          ( "inputSchema",
            schema
              [
                ("code", "string", "the drafted function source code");
                ("language", "string",
                 "rust|python|javascript|typescript|go|java|c (default rust)");
                ("top_k", "integer", "max results (default 5)");
                ("threshold", "number", "min max(jaccard,containment) (default 0.25)");
              ]
              [ "code" ] );
        ];
      `Assoc
        [
          ("name", `String "query_similar");
          ( "description",
            `String
              "Find functions similar to one already on disk, identified by \
               file plus a line inside it or its name." );
          ( "inputSchema",
            schema
              ([
                 ("file", "string", "absolute path of the source file");
                 ("line", "integer", "a line inside the function (1-based)");
                 ("name", "string", "the function's name (alternative to line)");
                 ("top_k", "integer", "max results (default 5)");
                 ("threshold", "number",
                  "min max(jaccard,containment) (default 0.25)");
               ])
              [ "file" ] );
        ];
      `Assoc
        [
          ("name", `String "diff_functions");
          ( "description",
            `String
              "Structural set-algebra over two functions A and B: similarity \
               scores (A ∩ B) plus the regions unique to each side, grouped \
               by source line. For a buggy function A and its fixed version \
               B: A−B localizes ≈ the bug, B−A ≈ the fix. Each side is given \
               as code (<side>_code) or as <side>_file + <side>_line/_name." );
          ( "inputSchema",
            schema (side_props "a" "function A" @ side_props "b" "function B") []
          );
        ];
      `Assoc
        [
          ("name", `String "status");
          ( "description",
            `String "Index status: corpus root, size, readiness." );
          ("inputSchema", schema [] []);
        ];
    ]

let call_tool (name : string) (args : J.t) : J.t =
  match name with
  | "status" ->
      tool_text
        (Printf.sprintf
           "root: %s\nindexed functions: %d (%d workspace + %d from %d \
            deps/std)\nindexing: %s"
           !index_root (Engine.size !engine) !workspace_fns !dep_fns
           !dep_count
           (if !indexing_done then "done" else "in progress"))
  | "find_similar" -> (
      match str_arg args "code" with
      | None -> tool_text ~is_error:true "missing required argument: code"
      | Some code -> (
          let lang = Option.value (str_arg args "language") ~default:"rust" in
          let top_k = Option.value (int_arg args "top_k") ~default:5 in
          let threshold =
            Option.value (float_arg args "threshold") ~default:0.25
          in
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
                       (query_unit u ~threshold ~top_k))
              |> String.concat "\n\n---\n\n" |> tool_text))
  | "query_similar" -> (
      match str_arg args "file" with
      | None -> tool_text ~is_error:true "missing required argument: file"
      | Some file -> (
          let unit =
            match (int_arg args "line", str_arg args "name") with
            | Some line, _ -> Units.unit_at file line
            | None, Some name ->
                Units.units_of_file file
                |> List.find_opt (fun (u : Units.unit_info) ->
                       u.Units.uname = name)
            | None, None -> (
                match Units.units_of_file file with u :: _ -> Some u | [] -> None)
          in
          match unit with
          | None -> tool_text ~is_error:true ("no matching function in " ^ file)
          | Some u ->
              let top_k = Option.value (int_arg args "top_k") ~default:5 in
              let threshold =
                Option.value (float_arg args "threshold") ~default:0.25
              in
              tool_text
                (hits_text
                   (Printf.sprintf "`%s` (%s:%d)" u.Units.uname file
                      u.Units.uline_start)
                   (query_unit u ~threshold ~top_k))))
  | "diff_functions" -> diff_functions args
  | _ -> tool_text ~is_error:true ("unknown tool: " ^ name)

(* ── Dispatch ────────────────────────────────────────────────────────────── *)

(* Server-level context for the LLM (MCP `instructions`): what kind of
   similarity this is, and how to read the numbers. *)
let instructions =
  "nonna finds structurally similar functions: each function's resolved \
   control/data-flow graph (semgrep/opengrep IL) is hashed into a feature \
   set via iterative dataflow hashing; similarity = weighted jaccard + \
   asymmetric containment over those sets. Queries are code (snippets or \
   file locations); natural-language queries will not work. Matching is \
   rename-invariant: variable names, identifiers, literal values and \
   concrete types are ignored by default — two functions match if they \
   compute the same way. Reading scores: jaccard ≈ 1.0 means same function \
   up to renaming/formatting; containment ≈ 1.0 means the other function \
   does everything this one does (and possibly more) — a strong 'call it \
   instead' signal even when jaccard is moderate. The workspace is indexed \
   at startup in the background (seconds for normal repos, ~a minute for \
   huge ones): call `status` first; an empty result while indexing is still \
   in progress is inconclusive, not a no-match."

(* Returns Some response for requests, None for notifications. *)
let result_msg (id : J.t) (result : J.t) : J.t =
  `Assoc [ ("jsonrpc", `String "2.0"); ("id", id); ("result", result) ]

let error_msg (id : J.t) (code : int) (msg : string) : J.t =
  `Assoc
    [
      ("jsonrpc", `String "2.0");
      ("id", id);
      ("error", `Assoc [ ("code", `Int code); ("message", `String msg) ]);
    ]

(* Tool calls may parse files; serialize dispatch across transports. *)
let dispatch_mutex = Mutex.create ()

let handle_message (msg : J.t) : J.t option =
  let meth = try JU.member "method" msg |> JU.to_string with _ -> "" in
  let id = JU.member "id" msg in
  let params = JU.member "params" msg in
  match meth with
  | "initialize" ->
      let proto =
        try JU.member "protocolVersion" params |> JU.to_string
        with _ -> "2024-11-05"
      in
      Some
        (result_msg id
           (`Assoc
             [
               ("protocolVersion", `String proto);
               ("capabilities", `Assoc [ ("tools", `Assoc []) ]);
               ( "serverInfo",
                 `Assoc
                   [ ("name", `String "nonna"); ("version", `String "0.1") ]
               );
               ("instructions", `String instructions);
             ]))
  | "notifications/initialized" -> None
  | "ping" -> Some (result_msg id (`Assoc []))
  | "tools/list" -> Some (result_msg id (`Assoc [ ("tools", tool_defs) ]))
  | "tools/call" ->
      let name = try JU.member "name" params |> JU.to_string with _ -> "" in
      let args = JU.member "arguments" params in
      Mutex.lock dispatch_mutex;
      let r =
        try call_tool name args
        with e -> tool_text ~is_error:true (Printexc.to_string e)
      in
      Mutex.unlock dispatch_mutex;
      Some (result_msg id r)
  | _ ->
      if id = `Null then None
      else Some (error_msg id (-32601) ("unhandled: " ^ meth))

(* ── stdio transport (newline-delimited JSON) ────────────────────────────── *)

let run (root : string) : unit =
  index_async root;
  let rec loop () =
    match input_line stdin with
    | line ->
        (if String.trim line <> "" then
           match J.from_string line with
           | msg -> (
               match handle_message msg with
               | Some resp -> send resp
               | None -> ()
               | exception e ->
                   prerr_endline ("nonna mcp: " ^ Printexc.to_string e))
           | exception _ -> ());
        loop ()
    | exception End_of_file -> ()
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

let percent_decode (s : string) : string =
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let rec go i =
    if i < n then
      if s.[i] = '%' && i + 2 < n then (
        (match int_of_string_opt ("0x" ^ String.sub s (i + 1) 2) with
        | Some c -> Buffer.add_char b (Char.chr c)
        | None -> Buffer.add_char b s.[i]);
        go (i + 3))
      else (
        Buffer.add_char b (if s.[i] = '+' then ' ' else s.[i]);
        go (i + 1))
  in
  go 0;
  Buffer.contents b

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
                       percent_decode
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
     let rec read_headers len =
       match String.trim (input_line ic) with
       | "" -> len
       | h -> (
           match String.index_opt h ':' with
           | Some i
             when String.lowercase_ascii (String.sub h 0 i) = "content-length"
             ->
               read_headers
                 (int_of_string
                    (String.trim (String.sub h (i + 1) (String.length h - i - 1))))
           | _ -> read_headers len)
     in
     let clen = read_headers 0 in
     if
       String.length request_line >= 4
       && String.uppercase_ascii (String.sub request_line 0 4) = "POST"
     then (
       let body = really_input_string ic clen in
       match handle_message (J.from_string body) with
       | Some resp -> http_response oc (J.to_string resp)
       | None -> http_response oc ~status:"202 Accepted" ""
       | exception e ->
           http_response oc ~status:"500 Internal Server Error"
             (J.to_string
                (error_msg `Null (-32700) (Printexc.to_string e))))
     else
       (* GET <target> HTTP/1.1 -> explorer UI / JSON API *)
       let target =
         match String.split_on_char ' ' request_line with
         | _ :: t :: _ -> t
         | _ -> "/"
       in
       handle_get oc target
   with _ -> ());
  (try Unix.close fd with _ -> ())

let serve (root : string) (port : int) : unit =
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
