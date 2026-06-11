(* Graphviz rendering of the DFG and its hash propagation.
   One DOT file per propagation round: node labels carry the current hash
   (low 32 bits, 8 hex); nodes whose hash changed in this round are
   highlighted. Round 0 = local seed hashes. *)

module Dfg = Nonna_features.Dfg

let esc (s : string) : string =
  String.concat ""
    (List.map
       (fun c ->
         match c with
         | '"' -> "\\\""
         | '\\' -> "\\\\"
         | '\n' -> "\\n"
         | c -> String.make 1 c)
       (List.init (String.length s) (String.get s)))

let tag_color (t : Dfg.tag) =
  match t with
  | Dfg.Call -> "#cce5ff"
  | Dfg.BinOp | Dfg.UnOp -> "#e2d5f8"
  | Dfg.ConstString | Dfg.ConstOther -> "#d5f8d5"
  | Dfg.Field | Dfg.Index -> "#fff3cd"
  | Dfg.Construct -> "#f8d5e2"
  | Dfg.MacroBag -> "#ffd9b3"
  | Dfg.Control -> "#e8e8e8"

(* DOT for one round. [round] indexes g.rounds; changed-vs-previous-round
   nodes get a bold red border (round 0: everything is "new", no highlight).
   [source] = (first_line, lines) rendered as a panel so graph nodes (tagged
   :NN with their source line) can be read against the code. *)
let dot_of_round ?(source : (int * string list) option)
    (g : Dfg.graph) ~(fn_name : string) ~(round : int) : string =
  let b = Buffer.create 4096 in
  let n = Array.length g.dnodes in
  let cur = g.rounds.(round) in
  let prev = if round = 0 then cur else g.rounds.(round - 1) in
  Buffer.add_string b "digraph dfg {\n";
  Buffer.add_string b "  rankdir=TB;\n";
  Buffer.add_string b
    (Printf.sprintf
       "  label=\"%s — round %d/%d%s\"; labelloc=t; fontsize=20; \
        fontname=\"Helvetica\";\n"
       (esc fn_name) round
       (Array.length g.rounds - 1)
       (if round = 0 then " (local seeds)" else ""));
  Buffer.add_string b
    "  node [shape=box, style=\"filled,rounded\", fontname=\"Menlo\", \
     fontsize=11];\n";
  Buffer.add_string b "  edge [fontname=\"Menlo\", fontsize=9, color=\"#666666\"];\n";
  (match source with
  | Some (first_line, lines) ->
      let body =
        lines
        |> List.mapi (fun i l ->
               Printf.sprintf "%2d  %s\\l" (first_line + i) (esc l))
        |> String.concat ""
      in
      Buffer.add_string b
        (Printf.sprintf
           "  src [shape=box, style=\"filled\", fillcolor=\"#fcfcf4\", \
            color=\"#bbbbaa\", fontname=\"Menlo\", fontsize=10, \
            label=\"%s\"];\n"
           body)
  | None -> ());
  let referenced = Array.make n false in
  Array.iter
    (fun (d : Dfg.dnode) ->
      Array.iter (fun (pi, _) -> referenced.(pi) <- true) d.Dfg.preds)
    g.dnodes;
  let hidden i (d : Dfg.dnode) =
    (* unused sentinel/params are noise *)
    (match d.Dfg.dkind with
    | Dfg.KSentinel | Dfg.KParam -> true
    | Dfg.KOp | Dfg.KPhi -> false)
    && not referenced.(i)
  in
  for i = 0 to n - 1 do
    let d = g.dnodes.(i) in
    if not (hidden i d) then begin
    let changed = round > 0 && cur.(i) <> prev.(i) in
    let shape, fill =
      match d.Dfg.dkind with
      | Dfg.KParam -> ("ellipse", "#d0e8f2")
      | Dfg.KSentinel -> ("ellipse", "#dddddd")
      | Dfg.KPhi -> ("diamond", "#f5f5dc")
      | Dfg.KOp -> ("box", tag_color d.Dfg.dtag)
    in
    let extra =
      (if changed then ", penwidth=3, color=\"#cc2200\"" else ", color=\"#888888\"")
      ^ (if d.Dfg.emit then "" else ", style=\"filled,rounded,dashed\"")
    in
    let comm = if d.Dfg.commutative then " ⊕" else "" in
    let line = if d.Dfg.dline > 0 then Printf.sprintf " :%d" d.Dfg.dline else "" in
    Buffer.add_string b
      (Printf.sprintf
         "  n%d [shape=%s, fillcolor=\"%s\", label=\"%s%s%s\\n%08Lx\"%s];\n"
         i shape fill (esc d.Dfg.dlabel) comm line
         (Int64.logand (Int64.of_int cur.(i)) 0xFFFFFFFFL)
         extra)
    end
  done;
  for i = 0 to n - 1 do
    let d = g.dnodes.(i) in
    let np = Array.length d.Dfg.preds in
    Array.iter
      (fun (pi, pos) ->
        let lbl =
          if np > 1 && not d.Dfg.commutative then
            Printf.sprintf " [label=\"%d\"]" pos
          else ""
        in
        Buffer.add_string b (Printf.sprintf "  n%d -> n%d%s;\n" pi i lbl))
      d.Dfg.preds
  done;
  Buffer.add_string b "}\n";
  Buffer.contents b

let safe_name (s : string) : string =
  String.map
    (fun c ->
      match c with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' -> c
      | _ -> '_')
    s

(* Write one DOT per round for a function; returns the file paths. *)
let write_rounds ?source ~(outdir : string) ~(fn_name : string)
    (g : Dfg.graph) : string list =
  if not (Sys.file_exists outdir) then Sys.mkdir outdir 0o755;
  List.init (Array.length g.rounds) (fun r ->
      let path =
        Filename.concat outdir
          (Printf.sprintf "%s_round%d.dot" (safe_name fn_name) r)
      in
      let oc = open_out path in
      output_string oc (dot_of_round ?source g ~fn_name ~round:r);
      close_out oc;
      path)
