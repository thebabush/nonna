(* Minimal LSP server (stdio, hand-rolled JSON-RPC — no lsp lib dep).
 *
 * v0 scope: the workspace is indexed once at `initialize`; on every
 * didOpen/didSave the file's fn-units are queried against that index and
 * strong matches are published as Information diagnostics on the unit's
 * first line ("similar to `mean` (util.rs:1) — jaccard 0.95 ...").
 * The index is NOT updated on edits (stale-but-useful); restart to refresh.
 *)

module J = Yojson.Safe
module JU = Yojson.Safe.Util
module Engine = Nonna_index.Engine
module Signature = Nonna_features.Signature

(* report matches with max(jaccard, containment) above this *)
let report_threshold = 0.7

(* ── Wire protocol ───────────────────────────────────────────────────────── *)

let read_message () : J.t option =
  let rec read_headers (len : int) =
    match input_line stdin with
    | "" | "\r" -> len
    | line -> (
        match String.index_opt line ':' with
        | Some i
          when String.lowercase_ascii (String.sub line 0 i) = "content-length"
          ->
            read_headers
              (int_of_string
                 (String.trim
                    (String.sub line (i + 1) (String.length line - i - 1))))
        | _ -> read_headers len)
  in
  match read_headers 0 with
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
      [ ("jsonrpc", `String "2.0"); ("method", `String meth);
        ("params", params) ])

let log_to_client (msg : string) : unit =
  notify "window/logMessage"
    (`Assoc [ ("type", `Int 3); ("message", `String ("nonna: " ^ msg)) ])

(* ── URIs ────────────────────────────────────────────────────────────────── *)

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
        Buffer.add_char b s.[i];
        go (i + 1))
  in
  go 0;
  Buffer.contents b

let path_of_uri (uri : string) : string =
  let p =
    if String.length uri >= 7 && String.sub uri 0 7 = "file://" then
      String.sub uri 7 (String.length uri - 7)
    else uri
  in
  percent_decode p

(* ── Analysis ────────────────────────────────────────────────────────────── *)

let engine : Engine.t ref = ref Engine.empty

(* Indexing runs on a background thread: the protocol loop must stay
   responsive (shutdown/didOpen) — a synchronous index of a big workspace
   made client stops time out. The engine ref is swapped atomically at the
   end; queries before that see an empty index (no diagnostics yet). *)
let index_workspace_async (root : string) : unit =
  ignore
    (Thread.create
       (fun () ->
         try
           let b = Engine.create () in
           Units.units_of_paths [ root ]
           |> List.iter (fun (u : Units.unit_info) ->
                  let sg = Signature.extract ~ext:(Filename.extension u.Units.ufile) u.Units.ucfg in
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
           let eng = Engine.freeze b in
           engine := eng;
           log_to_client
             (Printf.sprintf "indexed %d units under %s" (Engine.size eng)
                root)
         with e -> log_to_client ("indexing failed: " ^ Printexc.to_string e))
       ())

let line_range (line0 : int) : J.t =
  `Assoc
    [
      ("start", `Assoc [ ("line", `Int line0); ("character", `Int 0) ]);
      ("end", `Assoc [ ("line", `Int line0); ("character", `Int 999) ]);
    ]

let diagnostics_for (path : string) : J.t list =
  Units.units_of_file path
  |> List.filter_map (fun (u : Units.unit_info) ->
         let sg = Signature.extract ~ext:(Filename.extension u.Units.ufile) u.Units.ucfg in
         if Signature.size sg < Units.min_features then None
         else
           let self_m =
             {
               Engine.name = u.Units.uname;
               file = path;
               line_start = u.Units.uline_start;
               line_end = u.Units.uline_end;
             }
           in
           Engine.query !engine sg ~threshold:report_threshold ~max_results:5
           |> List.filter (fun (h : Engine.hit) ->
                  not (Engine.nests self_m h.Engine.meta))
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
                   "%s is similar to `%s` (%s:%d) — jaccard %.2f, \
                    containment %.2f%s"
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
                                    line_range (max 0 (hm.Engine.line_start - 1))
                                  );
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

let publish (uri : string) : unit =
  let diags = try diagnostics_for (path_of_uri uri) with _ -> [] in
  notify "textDocument/publishDiagnostics"
    (`Assoc [ ("uri", `String uri); ("diagnostics", `List diags) ])

(* Source text of the fn-unit at a position (for the virtual diff docs). *)
let function_text (uri : string) (line0 : int) : J.t =
  let path = path_of_uri uri in
  match Units.unit_at path (line0 + 1) with
  | None -> `Assoc [ ("name", `Null); ("text", `String "") ]
  | Some u ->
      let lines = Units.file_slice path u.Units.uline_start u.Units.uline_end in
      `Assoc
        [
          ("name", `String u.Units.uname);
          ("text", `String (String.concat "\n" lines));
        ]

(* "Find similar" (palette command): given a cursor position, take the
   narrowest named fn-unit containing it and return ranked matches. Lower
   threshold than diagnostics — this is an explicit request, show more. *)
let find_similar (uri : string) (line0 : int) : J.t =
  let path = path_of_uri uri in
  match Units.unit_at path (line0 + 1) with
  | None -> `Assoc [ ("query", `Null); ("hits", `List []) ]
  | Some u ->
      let sg = Signature.extract ~ext:(Filename.extension u.Units.ufile) u.Units.ucfg in
      let hits =
        let self_m =
          {
            Engine.name = u.Units.uname;
            file = path;
            line_start = u.Units.uline_start;
            line_end = u.Units.uline_end;
          }
        in
        Engine.query !engine sg ~threshold:0.2 ~max_results:15
        |> List.filter (fun (h : Engine.hit) ->
               not (Engine.nests self_m h.Engine.meta))
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
                  ("change", `Int 0);
                  ("save", `Bool true);
                ] );
          ] );
      ( "serverInfo",
        `Assoc [ ("name", `String "nonna"); ("version", `String "0.1") ] );
    ]

let handle (msg : J.t) : unit =
  let meth =
    try JU.member "method" msg |> JU.to_string with _ -> ""
  in
  let id = JU.member "id" msg in
  let params = JU.member "params" msg in
  match meth with
  | "initialize" ->
      reply id server_capabilities;
      let root =
        match JU.member "rootUri" params with
        | `String uri -> Some (path_of_uri uri)
        | _ -> (
            try
              Some
                (path_of_uri
                   (JU.member "workspaceFolders" params |> JU.index 0
                  |> JU.member "uri" |> JU.to_string))
            with _ -> None)
      in
      (match root with
      | Some r ->
          log_to_client ("indexing " ^ r ^ " (background)...");
          index_workspace_async r
      | None -> log_to_client "no workspace root; index empty")
  | "initialized" -> ()
  | "textDocument/didOpen" | "textDocument/didSave" -> (
      try
        publish
          (JU.member "textDocument" params |> JU.member "uri" |> JU.to_string)
      with e -> log_to_client (Printexc.to_string e))
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
        (try handle msg
         with e ->
           prerr_endline ("nonna lsp: " ^ Printexc.to_string e));
        loop ()
  in
  loop ()
