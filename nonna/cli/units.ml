(* Function-unit enumeration shared by the CLI commands. *)

module G = AST_generic
module Signature = Nonna_features.Signature
module Engine = Nonna_index.Engine

type unit_info = {
  uname : string;
  ufile : string;
  ulang : Lang.t;
  uline_start : int;
  uline_end : int;
  ucode_lines : int; (* lines carrying code tokens; comments/docstrings out *)
  ucfg : IL.fun_cfg;
  utokens : string list; (* body token contents (comment/ws-free) *)
  utest : bool; (* test code — see [is_test_path] / [test_ranges] *)
}

let entity_name (ent_opt : G.entity option) : string =
  match ent_opt with
  | Some { G.name = G.EN (G.Id ((s, _), _)); _ } -> s
  | Some { G.name = G.EN (G.IdQualified { G.name_last = (s, _), _; _ }); _ } ->
      s
  (* A def whose name is a binding PATTERN rather than an identifier:
     `let f = |x| ...` in Rust, `const f = x => ...` in JS. Very common, and
     the closure is the function-unit — so take the bound name. *)
  | Some { G.name = G.EPattern (G.PatId ((s, _), _)); _ } -> s
  | Some _ -> "<entity>"
  | None -> "<lambda>"

(* Units the AST gave no usable name for. They must not act as the qualifying
   parent of a nested lambda ("<entity>::<lambda>" tells you nothing about
   where you are) — walk out to a real name instead. *)
let is_anonymous (name : string) : bool = name = "<lambda>" || name = "<entity>"

(* Lines of actual code in a body: distinct lines carrying at least one AST
   token. Comments and blank lines carry no tokens, so they never count;
   docstring-shaped statements (a bare string-literal expression statement)
   are excluded explicitly — a function that is one docstring and one return
   is 2 code lines no matter how long the docstring. *)
let code_lines_of_body (body : G.stmt) : int =
  let pos_of tok =
    match Tok.loc_of_tok tok with
    | Ok l -> Some (l.Tok.pos.Pos.line, l.Tok.pos.Pos.bytepos)
    | Error _ -> None
  in
  let doc_pos = Hashtbl.create 4 in
  let v =
    object
      inherit [_] G.iter_no_id_info as super

      method! visit_stmt env st =
        (match st.G.s with
        | G.ExprStmt ({ G.e = G.L (G.String _); _ }, _) ->
            AST_generic_helpers.ii_of_any (G.S st)
            |> List.iter (fun tok ->
                   Option.iter
                     (fun p -> Hashtbl.replace doc_pos p ())
                     (pos_of tok))
        | _ -> ());
        super#visit_stmt env st
    end
  in
  v#visit_stmt () body;
  let lines = Hashtbl.create 16 in
  AST_generic_helpers.ii_of_any (G.S body)
  |> List.iter (fun tok ->
         match pos_of tok with
         | Some ((line, _) as p) ->
             if not (Hashtbl.mem doc_pos p) then Hashtbl.replace lines line ()
         | None -> ());
  Hashtbl.length lines

(* ── OCaml IL normalization (D-ocaml) ─────────────────────────────────────
   opengrep parses OCaml but its AST_to_IL leaves function bodies un-lowered:
   expression-bodied functions and bare expression-statements (sequence
   elements, if/match branches) are wrapped in an OS_ExprStmt2 "other stmt"
   that AST_to_IL has no case for, so the whole body collapses to a single
   NTodo and the function carries ZERO dataflow features. We fix it here, on
   the generic AST nonna already owns, rather than patching the vendored
   submodule:

   - rewrite [OtherStmt (OS_ExprStmt2, [E e])] -> [ExprStmt e]; AST_to_IL
     lowers ExprStmt, and funcbody_to_stmt turns an FBStmt-wrapped ExprStmt
     into exactly what an FBExpr body would produce, so this one rewrite
     covers both the body wrapper and the nested statement cases.
   - mark each body's tail expression as an implicit return, so the returned
     value becomes a Control node (what opengrep's Implicit_return pass does
     for the languages on its allowlist — OCaml is not on it). Measured to
     matter: without it, hard-negative (same-file) FPR ~doubles. *)
let rec mark_tail_return (st : G.stmt) : unit =
  match st.G.s with
  | G.ExprStmt (e, _) -> e.G.is_implicit_return <- true
  | G.Block (_, stmts, _) -> (
      match List.rev stmts with last :: _ -> mark_tail_return last | [] -> ())
  | G.If (_, _, th, el) ->
      mark_tail_return th;
      Option.iter mark_tail_return el
  | G.Switch (_, _, cases) ->
      List.iter
        (function
          | G.CasesAndBody (_, s) -> mark_tail_return s | G.CaseEllipsis _ -> ())
        cases
  | G.Try (_, body, catches, else_opt, _finally) ->
      (* the value is the body's tail (no exception) or a handler's tail (caught);
         an else block, when present, is the no-exception value. finally runs but
         its value is discarded. *)
      mark_tail_return body;
      List.iter (fun (_, _, h) -> mark_tail_return h) catches;
      Option.iter (fun (_, s) -> mark_tail_return s) else_opt
  (* While/For/etc. are unit-valued in OCaml — no tail value to mark. *)
  | _ -> ()

let normalize_ocaml (ast : G.program) : G.program =
  let v =
    object
      inherit [_] G.map_legacy as super

      method! visit_stmt env st =
        match st.G.s with
        | G.OtherStmt (G.OS_ExprStmt2, [ G.E e ]) ->
            G.exprstmt (super#visit_expr env e)
        | _ -> super#visit_stmt env st

      method! visit_function_definition env fdef =
        let fdef = super#visit_function_definition env fdef in
        mark_tail_return (AST_generic_helpers.funcbody_to_stmt fdef.G.fbody);
        fdef
    end
  in
  List.map (fun st -> v#visit_stmt () st) ast

(* ── Test code ─────────────────────────────────────────────────────────────
   Test code duplicates itself by nature: one assertion-shaped body per case,
   differing only in literals, plus repeated fixtures and setup. Measured on a
   mid-size Rust workspace, 91% of all dupe pairs (22,339 of 24,543) sat
   inside #[cfg(test)] modules. Units are flagged here, once, so every
   consumer (dupe scans, ranking, LSP diagnostics) can drop them by default.
   Two signals, OR-ed:
   - attributes (Rust): the unit sits under a definition carrying
     #[cfg(test)] (the `mod tests` idiom — nesting, not just the fn's own
     attrs), or carries #[test] / #[tokio::test] / #[cfg(test)] itself.
     cfg(not(test)) is not test code.
   - path: a test/tests/__tests__ directory component, or a test-shaped
     basename: test_*.py, *_test.go, *.test.ts, *.spec.js, tests.rs, ... *)

let is_test_path (file : string) : bool =
  let stem = Filename.remove_extension (Filename.basename file) in
  List.exists
    (fun d -> d = "test" || d = "tests" || d = "__tests__")
    (String.split_on_char '/' (Filename.dirname file))
  || stem = "test" || stem = "tests"
  || String.starts_with ~prefix:"test_" stem
  || List.exists
       (fun suffix -> String.ends_with ~suffix stem)
       [ "_test"; "_tests"; ".test"; ".spec" ]

let name_last (n : G.name) : string =
  match n with
  | G.Id ((s, _), _) -> s
  | G.IdQualified { G.name_last = (s, _), _; _ } -> s

let is_test_attr (a : G.attribute) : bool =
  match a with
  | G.NamedAttr (_, name, (_, args, _)) -> (
      match name_last name with
      | "test" -> true (* #[test], #[tokio::test], #[rstest::test], ... *)
      | "cfg" ->
          (* cfg(test), cfg(all(test, ..)), cfg(any(.., test)); never not(test).
             The parser hands cfg's token tree over as expressions when it can
             and as a raw token bag otherwise — accept both. *)
          let rec mentions_test (e : G.expr) =
            match e.G.e with
            | G.N (G.Id (("test", _), _)) -> true
            | G.Call
                ( { G.e = G.N (G.Id ((("all" | "any"), _), _)); _ },
                  (_, inner, _) ) ->
                args_mention inner
            | _ -> false
          and args_mention args =
            List.exists
              (function
                | G.Arg e -> mentions_test e
                | G.OtherArg (_, anys) ->
                    let toks =
                      List.concat_map
                        (fun any ->
                          AST_generic_helpers.ii_of_any any
                          |> List.map Tok.content_of_tok)
                        anys
                    in
                    List.mem "test" toks && not (List.mem "not" toks)
                | _ -> false)
              args
          in
          args_mention args
      | _ -> false)
  | _ -> false

(* Line ranges of every definition carrying a test attribute (a `mod tests`
   spans all its functions; a `#[test] fn` spans itself). *)
let test_ranges (ast : G.program) : (int * int) list =
  let ranges = ref [] in
  let v =
    object
      inherit [_] G.iter_no_id_info as super

      method! visit_definition env ((ent, _) as def) =
        (if List.exists is_test_attr ent.G.attrs then
           match AST_generic_helpers.range_of_any_opt (G.Def def) with
           | Some (l1, l2) ->
               ranges := (l1.Tok.pos.Pos.line, l2.Tok.pos.Pos.line) :: !ranges
           | None -> ());
        super#visit_definition env def
    end
  in
  v#visit_program () ast;
  !ranges

let units_of_file (file : string) : unit_info list =
  let file = try Unix.realpath file with _ -> file in
  let path = Fpath.v file in
  let ast = Parse_target.parse_program path in
  let lang = Lang.lang_of_filename_exn path in
  let ast = if lang = Lang.Ocaml then normalize_ocaml ast else ast in
  Naming_AST.resolve lang ast;
  let path_is_test = is_test_path file in
  let test_ranges = test_ranges ast in
  (* Mark trailing expressions as returning, so expression-bodied fns (the
     Rust default) get NReturn nodes in the IL. AST_to_IL only consumes the
     flag; without this pass there are no return nodes at all. *)
  Implicit_return.mark_implicit_return lang ast;
  let units = ref [] in
  Visit_function_defs.visit
    (fun ent_opt fdef ->
      let name = entity_name ent_opt in
      let body_stmt = AST_generic_helpers.funcbody_to_stmt fdef.G.fbody in
      let body_any = G.S body_stmt in
      let line_start, line_end =
        match AST_generic_helpers.range_of_any_opt body_any with
        | Some (l1, l2) -> (l1.Tok.pos.Pos.line, l2.Tok.pos.Pos.line)
        | None -> (0, 0)
      in
      match CFG_build.cfg_of_gfdef lang fdef with
      | fcfg ->
          units :=
            {
              uname = name;
              ufile = file;
              ulang = lang;
              uline_start = line_start;
              uline_end = line_end;
              ucode_lines = code_lines_of_body body_stmt;
              ucfg = fcfg;
              utokens = Nonna_features.Il_util.token_strings_of_any body_any;
              utest =
                path_is_test
                || List.exists
                     (fun (a, b) -> a <= line_start && line_start <= b)
                     test_ranges;
            }
            :: !units
      | exception e ->
          Printf.eprintf "warn: IL translation failed for %s in %s: %s\n" name
            file (Printexc.to_string e))
    ast;
  let units = List.rev !units in
  (* qualify anonymous units by their narrowest enclosing NAMED unit:
     "<lambda>" -> "find_in::<lambda>" *)
  units
  |> List.map (fun u ->
         if not (is_anonymous u.uname) then u
         else
           let parent =
             units
             |> List.filter (fun p ->
                    (not (is_anonymous p.uname))
                    && p.uline_start <= u.uline_start
                    && u.uline_end <= p.uline_end)
             |> List.sort (fun a b ->
                    compare
                      (a.uline_end - a.uline_start)
                      (b.uline_end - b.uline_start))
           in
           match parent with
           | p :: _ -> { u with uname = p.uname ^ "::" ^ u.uname }
           | [] -> u)

(* Languages we index. Rust-focused (D1), but multi-language comes via the
   shared IL — these are the ones exercised by the sanity dataset. *)
let indexable_exts =
  [
    ".rs";
    ".py";
    ".js";
    ".ts";
    ".go";
    ".java";
    ".c";
    ".h";
    ".cc";
    ".cpp";
    ".cxx";
    ".cppm";
    ".hpp";
    ".hh";
    ".hxx";
    ".ml";
    ".mli";
  ]

(* build outputs / dependency caches, never source corpus *)
let skip_dirs = [ "target"; "_build"; "node_modules"; "dist"; "__pycache__" ]

(* generated bindings etc.; nothing hand-written is this big *)
let max_file_bytes = 600 * 1024

let source_files_of_paths (paths : string list) : string list =
  let out = ref [] in
  (* lstat + skip symlinks: bazel-* convenience links dangle or point into
     build-output trees, and following links can loop. Any per-entry error
     (permissions, races) skips that entry, never the whole walk. *)
  let rec walk p =
    match (Unix.lstat p).Unix.st_kind with
    | Unix.S_LNK -> ()
    | Unix.S_DIR ->
        Sys.readdir p |> Array.to_list |> List.sort compare
        |> List.iter (fun entry ->
               if
                 (not (List.mem entry skip_dirs))
                 && not (String.length entry > 0 && entry.[0] = '.')
               then walk (Filename.concat p entry))
    | Unix.S_REG ->
        if
          List.mem (Filename.extension p) indexable_exts
          && (Unix.lstat p).Unix.st_size <= max_file_bytes
        then out := p :: !out
    | _ -> ()
    | exception _ -> ()
  in
  (* roots may themselves be symlinks (workspace setups): resolve them *)
  List.iter (fun p -> walk (try Unix.realpath p with _ -> p)) paths;
  List.rev !out

let units_of_paths (paths : string list) : unit_info list =
  source_files_of_paths paths
  |> List.concat_map (fun f ->
         try units_of_file f
         with e ->
           Printf.eprintf "warn: failed to parse %s: %s\n" f
             (Printexc.to_string e);
           [])

let loc_str (u : unit_info) =
  Printf.sprintf "%s:%d-%d" u.ufile u.uline_start u.uline_end

(* Units below this many features are too small to match meaningfully. *)
let min_features = 5

(* The one place an Engine.meta is built from a unit. *)
let meta_of (u : unit_info) : Engine.meta =
  {
    Engine.name = u.uname;
    file = u.ufile;
    line_start = u.uline_start;
    line_end = u.uline_end;
    code_lines = u.ucode_lines;
    is_test = u.utest;
  }

let is_lambda (u : unit_info) : bool =
  String.length u.uname >= 8
  && String.sub u.uname (String.length u.uname - 8) 8 = "<lambda>"

(* The narrowest NAMED fn-unit containing a (1-based) line; falls back to
   the narrowest lambda. *)
let unit_at_in (units : unit_info list) (line : int) : unit_info option =
  let containing =
    units
    |> List.filter (fun u -> line >= u.uline_start && line <= u.uline_end)
    |> List.sort (fun a b ->
           compare (a.uline_end - a.uline_start) (b.uline_end - b.uline_start))
  in
  match List.find_opt (fun u -> not (is_lambda u)) containing with
  | Some u -> Some u
  | None -> ( match containing with u :: _ -> Some u | [] -> None)

let unit_at path line = unit_at_in (units_of_file path) line

(* Parse editor buffers without changing the user's file. Keep the real
   document path in metadata so exclusions and related locations agree. *)
let units_of_text ~(path : string) (text : string) : unit_info list =
  let tmp = Filename.temp_file "nonna-buffer-" (Filename.extension path) in
  Fun.protect
    ~finally:(fun () -> try Sys.remove tmp with Sys_error _ -> ())
    (fun () ->
      let oc = open_out_bin tmp in
      Fun.protect
        ~finally:(fun () -> close_out_noerr oc)
        (fun () -> output_string oc text);
      units_of_file tmp
      |> List.map (fun u ->
             { u with ufile = path; utest = u.utest || is_test_path path }))

(* 1-based inclusive line slice of a file. *)
let file_slice (path : string) (first : int) (last : int) : string list =
  try
    let ic = open_in path in
    let rec go i acc =
      match input_line ic with
      | l -> go (i + 1) (if i >= first && i <= last then l :: acc else acc)
      | exception End_of_file ->
          close_in ic;
          List.rev acc
    in
    go 1 []
  with _ -> []

let index_units (units : unit_info list) :
    Engine.t * (unit_info * Signature.t) list =
  let b = Engine.create () in
  let kept =
    units
    |> List.filter_map (fun u ->
           let sg = Signature.extract ~lang:u.ulang u.ucfg in
           if Signature.size sg < min_features then None
           else (
             ignore (Engine.add b (meta_of u) sg);
             Some (u, sg)))
  in
  (Engine.freeze b, kept)
