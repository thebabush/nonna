(* Minimal LSP server (stdio, hand-rolled JSON-RPC — no lsp lib dep).
 *
 * v0 scope: the workspace is indexed once at `initialize`; on every
 * didOpen/didSave the file's fn-units are queried against that index and
 * strong matches are published as Information diagnostics on the unit's
 * first line ("similar to `mean` (util.rs:1) — jaccard 0.95 ...").
 * Open buffers are indexed on change; diagnostics carry document versions.
 *)

module J = Yojson.Safe
module JU = Yojson.Safe.Util
module Engine = Nonna_index.Engine
module Signature = Nonna_features.Signature

(* report matches with max(jaccard, containment) above this *)
let report_threshold = 0.7

(* Diagnostics are advisory editor squiggles, so only flag structurally rich
   functions. Trivial getters carry ~9 features and near-dupe each other at
   1.00 (pure noise); real functions start ~15. This floor is deliberately
   higher than the index-wide Units.min_features (5), which still governs the
   CLI/MCP power-user paths — it gates BOTH the queried unit and its matches. *)
let diag_min_features = 15

(* [diag_min_features] counts every channel, and a one-line closure
   (`|&t| key(&xs[t]) == k`) clears 15 on the non-dfg channels alone — then
   near-dupes every other one-liner at 1.00. Real code has a few lines of it,
   so gate BOTH sides on code lines as well. *)
let diag_min_lines = 3

(* ── Wire protocol ───────────────────────────────────────────────────────── *)

let read_message () : J.t option =
  match Wire.read_content_length stdin with
  | 0 -> None
  | n -> Some (J.from_string (really_input_string stdin n))
  | exception End_of_file -> None

(* protocol writes happen from the main loop AND the indexing thread *)
let send_mutex = Mutex.create ()

let send (j : J.t) : unit =
  let s = J.to_string j in
  Mutex.lock send_mutex;
  Printf.printf "Content-Length: %d\r\n\r\n%s" (String.length s) s;
  flush stdout;
  Mutex.unlock send_mutex

let reply (id : J.t) (result : J.t) : unit =
  send (`Assoc [ ("jsonrpc", `String "2.0"); ("id", id); ("result", result) ])

let notify (meth : string) (params : J.t) : unit =
  send
    (`Assoc
       [
         ("jsonrpc", `String "2.0"); ("method", `String meth); ("params", params);
       ])

let log_to_client (msg : string) : unit =
  notify "window/logMessage"
    (`Assoc [ ("type", `Int 3); ("message", `String ("nonna: " ^ msg)) ])

(* ── URIs ────────────────────────────────────────────────────────────────── *)

let path_of_uri (uri : string) : string =
  let p =
    if String.length uri >= 7 && String.sub uri 0 7 = "file://" then
      String.sub uri 7 (String.length uri - 7)
    else uri
  in
  let p = Wire.percent_decode p in
  (* The index stores realpath-canonical paths (symlinks resolved — see
     Units.source_files_of_paths), so canonicalize queries the same way.
     Otherwise a workspace opened through a symlink makes every function match
     its own un-resolved copy (self-flag). realpath needs the file to exist;
     fall back to the raw path if it doesn't. *)
  try Unix.realpath p with _ -> p

(* ── Analysis ────────────────────────────────────────────────────────────── *)

let engine : Engine.t ref = ref Engine.empty

(* Serialize state transitions and publication, including background commits.
   A completed scan replays changes received since it began. *)
let state_mutex = Mutex.create ()

let with_state f =
  Mutex.lock state_mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock state_mutex) f

let workspace_root : string option ref = ref None
let generation = ref 0

type document = { version : int; text : string; units : Units.unit_info list }

let open_docs : (string, document) Hashtbl.t = Hashtbl.create 16

let changes : (string, (Engine.meta * Signature.t) list) Hashtbl.t =
  Hashtbl.create 16

let republish_open : (unit -> unit) ref = ref (fun () -> ())

let entries units =
  List.filter_map
    (fun (u : Units.unit_info) ->
      let sg = Signature.extract ~lang:u.ulang u.ucfg in
      if Signature.size sg < Units.min_features then None
      else Some (Units.meta_of u, sg))
    units

let disk_units path = try Units.units_of_file path with _ -> []

let document_units uri =
  match Hashtbl.find_opt open_docs uri with
  | Some doc -> doc.units
  | None -> disk_units (path_of_uri uri)

let replace_file path units =
  let fresh = entries units in
  Hashtbl.replace changes path fresh;
  engine := Engine.refresh_file !engine ~file:path fresh

let index_workspace_async root =
  incr generation;
  let ticket = !generation in
  Hashtbl.clear changes;
  ignore
    (Thread.create
       (fun () ->
         try
           let b = Engine.create () in
           Units.units_of_paths [ root ]
           |> entries
           |> List.iter (fun (m, sg) -> ignore (Engine.add b m sg));
           let snapshot = Engine.freeze b in
           with_state (fun () ->
               if ticket = !generation then (
                 let snapshot =
                   Hashtbl.fold
                     (fun file fresh eng -> Engine.refresh_file eng ~file fresh)
                     changes snapshot
                 in
                 let snapshot =
                   Hashtbl.fold
                     (fun uri doc eng ->
                       Engine.refresh_file eng ~file:(path_of_uri uri)
                         (entries doc.units))
                     open_docs snapshot
                 in
                 engine := snapshot;
                 Hashtbl.clear changes;
                 log_to_client
                   (Printf.sprintf "indexed %d units under %s"
                      (Engine.size snapshot) root);
                 !republish_open ()))
         with e -> log_to_client ("indexing failed: " ^ Printexc.to_string e))
       ())

let line_range (line0 : int) : J.t =
  `Assoc
    [
      ("start", `Assoc [ ("line", `Int line0); ("character", `Int 0) ]);
      ("end", `Assoc [ ("line", `Int line0); ("character", `Int 999) ]);
    ]

let diagnostics_for (uri : string) : J.t list =
  document_units uri
  |> List.filter_map (fun (u : Units.unit_info) ->
         let sg = Signature.extract ~lang:u.Units.ulang u.Units.ucfg in
         if
           Signature.size sg < diag_min_features
           || u.Units.ucode_lines < diag_min_lines
         then None
         else
           let self_m = Units.meta_of u in
           Engine.query !engine sg ~threshold:report_threshold ~max_results:5
             ~accept:(fun m -> not (Engine.nests self_m m))
             ~filter:
               {
                 Engine.q_by_max = true;
                 q_name_sub = "";
                 q_include_paths = [];
                 q_exclude_paths = [];
                 q_min_lines = diag_min_lines;
                 q_min_features = diag_min_features;
                 q_scope = 0;
               }
           |> function
           | [] -> None
           | best :: _ as hits ->
               let m = best.Engine.meta in
               let line = max 0 (u.Units.uline_start - 1) in
               let more =
                 match List.length hits - 1 with
                 | 0 -> ""
                 | n -> Printf.sprintf " (+%d more)" n
               in
               let message =
                 Printf.sprintf
                   "%s is similar to `%s` (%s:%d) — jaccard %.2f, containment \
                    %.2f%s"
                   u.Units.uname m.Engine.name
                   (Filename.basename m.Engine.file)
                   m.Engine.line_start best.Engine.jaccard
                   best.Engine.containment more
               in
               (* every hit as a clickable child in the Problems panel; the
                  message prefix "name — " is parsed by the code action *)
               let related =
                 hits
                 |> List.map (fun (h : Engine.hit) ->
                        let hm = h.Engine.meta in
                        `Assoc
                          [
                            ( "location",
                              `Assoc
                                [
                                  ("uri", `String ("file://" ^ hm.Engine.file));
                                  ( "range",
                                    line_range
                                      (max 0 (hm.Engine.line_start - 1)) );
                                ] );
                            ( "message",
                              `String
                                (Printf.sprintf
                                   "%s — jaccard %.2f, containment %.2f"
                                   hm.Engine.name h.Engine.jaccard
                                   h.Engine.containment) );
                          ])
               in
               Some
                 (`Assoc
                    [
                      ("range", line_range line);
                      ("severity", `Int 3) (* Information *);
                      ("source", `String "nonna");
                      ("message", `String message);
                      ("relatedInformation", `List related);
                    ]))

let publish_diags ?version uri diags =
  let fields = [ ("uri", `String uri); ("diagnostics", `List diags) ] in
  let fields =
    match version with
    | None -> fields
    | Some v -> ("version", `Int v) :: fields
  in
  notify "textDocument/publishDiagnostics" (`Assoc fields)

let publish uri =
  match Hashtbl.find_opt open_docs uri with
  | None -> ()
  | Some doc ->
      publish_diags ~version:doc.version uri
        (try diagnostics_for uri with _ -> [])

let () =
  republish_open := fun () -> Hashtbl.iter (fun uri _ -> publish uri) open_docs

let update_document uri version text =
  match Hashtbl.find_opt open_docs uri with
  | Some doc when version <= doc.version -> ()
  | _ ->
      let path = path_of_uri uri in
      let units = try Units.units_of_text ~path text with _ -> [] in
      Hashtbl.replace open_docs uri { version; text; units };
      replace_file path units;
      !republish_open ()

(* Source text of the fn-unit at a position (for the virtual diff docs). *)
let function_text (uri : string) (line0 : int) : J.t =
  let path = path_of_uri uri in
  match Units.unit_at_in (document_units uri) (line0 + 1) with
  | None -> `Assoc [ ("name", `Null); ("text", `String "") ]
  | Some u ->
      let lines =
        match Hashtbl.find_opt open_docs uri with
        | None -> Units.file_slice path u.Units.uline_start u.Units.uline_end
        | Some doc ->
            String.split_on_char '\n' doc.text
            |> List.filteri (fun i _ ->
                   i + 1 >= u.uline_start && i + 1 <= u.uline_end)
      in
      `Assoc
        [
          ("name", `String u.Units.uname);
          ("text", `String (String.concat "\n" lines));
        ]

(* "Find similar" (palette command): given a cursor position, take the
   narrowest named fn-unit containing it and return ranked matches. Lower
   threshold than diagnostics — this is an explicit request, show more. *)
let find_similar (uri : string) (line0 : int) : J.t =
  match Units.unit_at_in (document_units uri) (line0 + 1) with
  | None -> `Assoc [ ("query", `Null); ("hits", `List []) ]
  | Some u ->
      let sg = Signature.extract ~lang:u.Units.ulang u.Units.ucfg in
      let hits =
        let self_m = Units.meta_of u in
        Engine.query !engine sg ~threshold:0.2 ~max_results:15 ~accept:(fun m ->
            not (Engine.nests self_m m))
        |> List.map (fun (h : Engine.hit) ->
               let m = h.Engine.meta in
               `Assoc
                 [
                   ("name", `String m.Engine.name);
                   ("file", `String m.Engine.file);
                   ("line_start", `Int m.Engine.line_start);
                   ("line_end", `Int m.Engine.line_end);
                   ("jaccard", `Float h.Engine.jaccard);
                   ("containment", `Float h.Engine.containment);
                 ])
      in
      `Assoc [ ("query", `String u.Units.uname); ("hits", `List hits) ]

(* ── Dispatch ────────────────────────────────────────────────────────────── *)

let server_capabilities : J.t =
  `Assoc
    [
      ( "capabilities",
        `Assoc
          [
            ( "textDocumentSync",
              `Assoc
                [
                  ("openClose", `Bool true);
                  ("change", `Int 1);
                  ("save", `Bool true);
                ] );
          ] );
      ( "serverInfo",
        `Assoc [ ("name", `String "nonna"); ("version", `String "0.1") ] );
    ]

let handle (msg : J.t) : unit =
  let meth = try JU.member "method" msg |> JU.to_string with _ -> "" in
  let id = JU.member "id" msg in
  let params = JU.member "params" msg in
  match meth with
  | "initialize" -> (
      reply id server_capabilities;
      let root =
        match JU.member "rootUri" params with
        | `String uri -> Some (path_of_uri uri)
        | _ -> (
            try
              Some
                (path_of_uri
                   (JU.member "workspaceFolders" params
                   |> JU.index 0 |> JU.member "uri" |> JU.to_string))
            with _ -> None)
      in
      match root with
      | Some r ->
          workspace_root := Some r;
          log_to_client ("indexing " ^ r ^ " (background)...");
          index_workspace_async r
      | None -> log_to_client "no workspace root; index empty")
  | "initialized" -> ()
  | "textDocument/didOpen" ->
      let td = JU.member "textDocument" params in
      update_document
        (JU.member "uri" td |> JU.to_string)
        (JU.member "version" td |> JU.to_int)
        (JU.member "text" td |> JU.to_string)
  | "textDocument/didChange" -> (
      let td = JU.member "textDocument" params in
      let uri = JU.member "uri" td |> JU.to_string in
      let version = JU.member "version" td |> JU.to_int in
      (* Full synchronization: each change is a complete replacement. *)
      match JU.member "contentChanges" params |> JU.to_list |> List.rev with
      | change :: _ when Hashtbl.mem open_docs uri ->
          update_document uri version (JU.member "text" change |> JU.to_string)
      | _ -> ())
  | "textDocument/didClose" ->
      let uri =
        JU.member "textDocument" params |> JU.member "uri" |> JU.to_string
      in
      Hashtbl.remove open_docs uri;
      let path = path_of_uri uri in
      replace_file path (disk_units path);
      publish_diags uri [];
      !republish_open ()
  | "textDocument/didSave" ->
      let uri =
        JU.member "textDocument" params |> JU.member "uri" |> JU.to_string
      in
      replace_file (path_of_uri uri) (document_units uri);
      !republish_open ()
  | "nonna/reindex" -> (
      match !workspace_root with
      | Some r ->
          log_to_client ("reindexing " ^ r ^ " (background)...");
          index_workspace_async r;
          reply id (`Assoc [ ("status", `String "reindexing") ])
      | None -> reply id (`Assoc [ ("status", `String "no workspace root") ]))
  | "nonna/findSimilar" | "nonna/functionText" -> (
      try
        let uri =
          JU.member "textDocument" params |> JU.member "uri" |> JU.to_string
        in
        let line =
          JU.member "position" params |> JU.member "line" |> JU.to_int
        in
        reply id
          (if meth = "nonna/functionText" then function_text uri line
           else find_similar uri line)
      with e ->
        send
          (`Assoc
             [
               ("jsonrpc", `String "2.0");
               ("id", id);
               ( "error",
                 `Assoc
                   [
                     ("code", `Int (-32603));
                     ("message", `String (Printexc.to_string e));
                   ] );
             ]))
  | "shutdown" -> reply id `Null
  | "exit" -> exit 0
  | _ ->
      (* politely refuse unknown REQUESTS; ignore unknown notifications *)
      if id <> `Null then
        send
          (`Assoc
             [
               ("jsonrpc", `String "2.0");
               ("id", id);
               ( "error",
                 `Assoc
                   [
                     ("code", `Int (-32601));
                     ("message", `String ("unhandled: " ^ meth));
                   ] );
             ])

let run () : unit =
  let rec loop () =
    match read_message () with
    | None -> ()
    | Some msg ->
        (try with_state (fun () -> handle msg)
         with e -> prerr_endline ("nonna lsp: " ^ Printexc.to_string e));
        loop ()
  in
  loop ()
