(* `nonna dupes --blocks` (D16): exact block clones, scb-check style but with
 * call names kept. A block is a compound statement (if / loop / switch or
 * match / try / with, or a bare block nested in a block) whose largest body
 * holds at least [min_stmts] statements and which spans at least
 * [min_lines] code lines (counted like Units.code_lines_of_body). Blocks
 * with equal Syntax_tree hashes form a group.
 *
 * Function-level matching cannot see a block duplicated inside two
 * otherwise different functions (D13); this listing does, and scores
 * nothing: it is a separate report, not a similarity channel.
 *)

module G = AST_generic
module Engine = Nonna_index.Engine
module Fhash = Nonna_features.Fhash
module Syntax_tree = Nonna_features.Syntax_tree

let min_stmts = 2
let min_lines = 3

type instance = {
  file : string;
  first : Tok.location;
  last : Tok.location;
  sloc : int;
  lines : int list; (* its code lines, ascending ([sloc] of them) *)
  whole : bool; (* the entire body of some unit: already a function pair *)
}

type group = { instances : instance list; dup_sloc : int }

let is_compound (st : G.stmt) : bool =
  match st.G.s with
  | G.If _ | G.While _ | G.DoWhile _ | G.For _ | G.Switch _ | G.Try _
  | G.WithUsingResource _ ->
      true
  | _ -> false

(* The statements a body consists of when it holds just one: a body that is
   one `if` IS that `if`, through block and statement-expression wrappers. *)
let rec sole (st : G.stmt) : G.stmt list =
  match st.G.s with
  | G.Block (_, [ s ], _) | G.ExprStmt ({ G.e = G.StmtExpr s; _ }, _) ->
      s :: sole s
  | _ -> []

(* Qualifying blocks of one unit, as (hash, instance). Closures are units
   nested in their parent, so the caller sees their blocks twice. *)
let blocks_of_unit ~min_stmts ~min_lines (u : Units.unit_info) :
    (Fhash.t * instance) list =
  let nodes = Syntax_tree.nodes u.Units.ubody in
  let bare =
    List.concat_map
      (fun (n : Syntax_tree.node) ->
        match n.Syntax_tree.stmt.G.s with
        | G.Block (_, l, _) ->
            List.filter (fun s -> match s.G.s with G.Block _ -> true | _ -> false) l
        | _ -> [])
      nodes
  in
  let whole = sole u.Units.ubody in
  nodes
  |> List.filter_map (fun (n : Syntax_tree.node) ->
         let st = n.Syntax_tree.stmt in
         if
           (is_compound st || List.memq st bare)
           && n.Syntax_tree.body_stmts >= min_stmts
         then
           let lines = Units.code_line_set_of_body st in
           let sloc = List.length lines in
           if sloc < min_lines then None
           else
             Some
               ( n.Syntax_tree.hash,
                 {
                   file = u.Units.ufile;
                   first = n.Syntax_tree.first;
                   last = n.Syntax_tree.last;
                   sloc;
                   lines;
                   whole = List.memq st whole;
                 } )
         else None)

let span (i : instance) = (i.first.Tok.pos.Pos.bytepos, i.last.Tok.pos.Pos.bytepos)

let overlaps (a : instance) (b : instance) : bool =
  a.file = b.file
  &&
  let a1, a2 = span a and b1, b2 = span b in
  a1 <= b2 && b1 <= a2

let groups ?(min_stmts = min_stmts) ?(min_lines = min_lines)
    (units : Units.unit_info list) : group list =
  (* one instance per (file, span): a closure's blocks come through both the
     closure and its parent; whole-body if it is that in any of them *)
  let seen : (string * (int * int), Fhash.t * instance) Hashtbl.t =
    Hashtbl.create 256
  in
  List.iter
    (fun u ->
      List.iter
        (fun (h, i) ->
          let key = (i.file, span i) in
          match Hashtbl.find_opt seen key with
          | Some (h0, i0) ->
              Hashtbl.replace seen key (h0, { i0 with whole = i0.whole || i.whole })
          | None -> Hashtbl.replace seen key (h, i))
        (blocks_of_unit ~min_stmts ~min_lines u))
    units;
  let by_hash : (Fhash.t, instance list) Hashtbl.t = Hashtbl.create 256 in
  Hashtbl.iter
    (fun _ (h, i) ->
      Hashtbl.replace by_hash h
        (i :: Option.value (Hashtbl.find_opt by_hash h) ~default:[]))
    seen;
  Hashtbl.fold
    (fun _ is acc ->
      let is =
        List.sort (fun a b -> compare (a.file, span a) (b.file, span b)) is
        |> List.fold_left
             (fun kept i ->
               if List.exists (overlaps i) kept then kept else i :: kept)
             []
        |> List.rev
      in
      if List.length is < 2 || List.for_all (fun i -> i.whole) is then acc
      else
        { instances = is; dup_sloc = Units.dup_sloc (List.map (fun i -> i.sloc) is) }
        :: acc)
    by_hash []
  |> List.sort (fun a b ->
         (* most duplicated SLOC first; then by first location, for a stable
            order *)
         let key g =
           ( -g.dup_sloc,
             -List.length g.instances,
             match g.instances with i :: _ -> (i.file, span i) | [] -> ("", (0, 0)) )
         in
         compare (key a) (key b))

(* The units a scan covers: test code only with --tests, path scope as in
   [dupes]. *)
let scanned ~(include_tests : bool) ~(include_paths : string list)
    ~(exclude_paths : string list) (units : Units.unit_info list) :
    Units.unit_info list =
  List.filter
    (fun u ->
      (include_tests || not u.Units.utest)
      && Engine.path_match ~include_paths ~exclude_paths u.Units.ufile)
    units

let print_groups (gs : group list) : unit =
  gs
  |> List.iter (fun g ->
         let sloc = List.fold_left (fun n i -> max n i.sloc) 0 g.instances in
         Printf.printf "\n%d instances, %d SLOC (%d duplicated)\n"
           (List.length g.instances) sloc g.dup_sloc;
         List.iter
           (fun i ->
             Printf.printf "  %s:%d-%d\n" i.file i.first.Tok.pos.Pos.line
               i.last.Tok.pos.Pos.line)
           g.instances)

let run ~(include_tests : bool) ~(include_paths : string list)
    ~(exclude_paths : string list) ~min_stmts ~min_lines (paths : string list)
    : unit =
  let units = Units.units_of_paths paths in
  let tests = List.length (List.filter (fun u -> u.Units.utest) units) in
  let scanned = scanned ~include_tests ~include_paths ~exclude_paths units in
  let gs = groups ~min_stmts ~min_lines scanned in
  Printf.printf
    "block clones: %d group(s), %d duplicated SLOC, from %d units in %d \
     file(s) (blocks >= %d statements, >= %d code lines)%s\n"
    (List.length gs)
    (List.fold_left (fun n g -> n + g.dup_sloc) 0 gs)
    (List.length scanned)
    (List.length (Units.source_files_of_paths paths))
    min_stmts min_lines
    (if include_tests then ""
     else Printf.sprintf "; skipping %d test units (--tests to include)" tests);
  print_groups gs
