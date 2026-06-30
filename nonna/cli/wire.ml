(* Shared transport helpers for the JSON-RPC servers: the stdio LSP server
   (Lsp_server) and the HTTP MCP server (Mcp_server) both frame messages with
   Content-Length headers and decode percent-escaped paths/queries. *)

(* Read RFC822-style headers from [ic] up to the blank separator line, returning
   the Content-Length value (0 if absent). Lines may be CRLF- or LF-terminated;
   a trailing CR is trimmed before matching. *)
let read_content_length (ic : in_channel) : int =
  let rec loop len =
    match String.trim (input_line ic) with
    | "" -> len
    | line -> (
        match String.index_opt line ':' with
        | Some i
          when String.lowercase_ascii (String.sub line 0 i) = "content-length"
          ->
            loop
              (int_of_string
                 (String.trim
                    (String.sub line (i + 1) (String.length line - i - 1))))
        | _ -> loop len)
  in
  loop 0

(* Percent-decode [s]. With [plus_as_space] (HTTP query strings), '+' decodes to
   a space; off by default (file:// URIs, where '+' is literal). Malformed
   %-escapes are passed through unchanged. *)
let percent_decode ?(plus_as_space = false) (s : string) : string =
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
        Buffer.add_char b (if plus_as_space && s.[i] = '+' then ' ' else s.[i]);
        go (i + 1))
  in
  go 0;
  Buffer.contents b
