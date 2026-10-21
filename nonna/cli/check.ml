(* `nonna check`: which functions a git change adds or modifies, and which of
 * them now look like existing code — for a pre-commit hook or a manual run.
 *
 *   nonna check [paths...]           working tree (+ untracked) vs HEAD
 *   nonna check --staged [paths...]  the index vs HEAD (pre-commit)
 *   nonna check --base REF           working tree vs merge-base(REF, HEAD)
 *   (-t gates max(jaccard, containment), default 0.7; --deps adds cargo
 *   deps + std to the index, off by default: big trees take long to index)
 *
 * Only NEW resemblances are reported: a modified function's matches are
 * dropped when its previous version already had them, so code that was
 * duplicated before the change stays quiet. The report is written for the
 * agent (or person) making the change: where to look and how to read the
 * scores. Advisory: exits 0 unless --strict.
 *)

module Engine = Nonna_index.Engine
module Signature = Nonna_features.Signature

(* ── Advisory gate (shared with the LSP diagnostics) ─────────────────────── *)

(* report matches with max(jaccard, containment) above this *)
let threshold = 0.7

(* Diagnostics are advisory editor squiggles, so only flag structurally rich
   functions. Trivial getters carry ~9 features and near-dupe each other at
   1.00 (pure noise); real functions start ~15. This floor is deliberately
   higher than the index-wide Units.min_features (5), which still governs the
   CLI/MCP power-user paths — it gates BOTH the queried unit and its matches. *)
let min_features = 15

(* [min_features] counts every channel, and a one-line closure
   (`|&t| key(&xs[t]) == k`) clears 15 on the non-dfg channels alone — then
   near-dupes every other one-liner at 1.00. Real code has a few lines of it,
   so gate BOTH sides on code lines as well. *)
let min_lines = 3

(* Strong matches of a unit, never itself or a def nesting it. Test code is
   skipped on both sides: test cases are near-dupes of each other by design. *)
let matches ?(threshold = threshold) (eng : Engine.t) (u : Units.unit_info)
    (sg : Signature.t) : Engine.hit list =
  if
    Signature.size sg < min_features
    || u.Units.ucode_lines < min_lines
    || u.Units.utest
  then []
  else
    let self_m = Units.meta_of u in
    Engine.query eng sg ~threshold ~max_results:5
      ~accept:(fun m -> not (Engine.nests self_m m))
      ~filter:
        {
          Engine.q_by_max = true;
          q_name_sub = "";
          q_include_paths = [];
          q_exclude_paths = [];
          q_min_lines = min_lines;
          q_min_features = min_features;
          q_scope = 0;
          q_include_tests = false;
        }

(* ── git ─────────────────────────────────────────────────────────────────── *)

(* stdout of a git command, None when it fails (stderr is dropped) *)
let git (args : string list) : string option =
  try
    let argv = Array.of_list ("git" :: "-c" :: "core.quotePath=false" :: args) in
    let out, inp, err = Unix.open_process_args_full "git" argv [||] in
    close_out inp;
    let text = In_channel.input_all out in
    ignore (In_channel.input_all err);
    match Unix.close_process_full (out, inp, err) with
    | Unix.WEXITED 0 -> Some text
    | _ -> None
  with _ -> None

let empty_tree = "4b825dc642cb6eb9a060e54bf8d69288fbee4904"

let lines (s : string) : string list =
  String.split_on_char '\n' s |> List.filter (fun l -> l <> "")

(* A changed file: its path now and before (None = added), and the new-side
   line ranges the change touches. Paths are relative to the repo root. *)
type change = {
  path : string;
  old_path : string option;
  ranges : (int * int) list;
}

let strip_prefix (p : string) (s : string) : string option =
  let n = String.length p in
  if String.length s >= n && String.sub s 0 n = p then
    Some (String.sub s n (String.length s - n))
  else None

(* git appends a tab to ---/+++ paths that contain spaces *)
let diff_path (s : string) : string =
  match String.index_opt s '\t' with Some i -> String.sub s 0 i | None -> s

(* "@@ -a,b +c,d @@": new-side lines c..c+d-1; a pure deletion (d = 0) sits
   after line c, so it touches c and c+1 *)
let hunk_range (line : string) : (int * int) option =
  match String.split_on_char ' ' line with
  | "@@" :: _ :: plus :: _ when String.length plus > 1 && plus.[0] = '+' -> (
      let spec = String.sub plus 1 (String.length plus - 1) in
      match List.map int_of_string_opt (String.split_on_char ',' spec) with
      | [ Some c ] -> Some (c, c)
      | [ Some c; Some 0 ] -> Some (c, c + 1)
      | [ Some c; Some d ] -> Some (c, c + d - 1)
      | _ -> None)
  | _ -> None

(* Parse `git diff -U0` output. Deleted files are dropped. *)
let parse_diff (text : string) : change list =
  let out = ref [] in
  let cur = ref None in
  let old = ref None in
  let flush () =
    (match !cur with
    | Some c -> out := { c with ranges = List.rev c.ranges } :: !out
    | None -> ());
    cur := None
  in
  lines text
  |> List.iter (fun l ->
         match strip_prefix "--- " l with
         | Some p ->
             flush ();
             old := strip_prefix "a/" (diff_path p)
         | None -> (
             match strip_prefix "+++ " l with
             | Some p -> (
                 match strip_prefix "b/" (diff_path p) with
                 | Some path -> cur := Some { path; old_path = !old; ranges = [] }
                 | None -> (* /dev/null: deleted *) ())
             | None -> (
                 match (!cur, hunk_range l) with
                 | Some c, Some r -> cur := Some { c with ranges = r :: c.ranges }
                 | _ -> ())));
  flush ();
  List.rev !out

(* ── The check ───────────────────────────────────────────────────────────── *)

type status = Added | Modified

type finding = {
  unit_ : Units.unit_info;
  status : status;
  hits : Engine.hit list;
}

let touches (ranges : (int * int) list) (u : Units.unit_info) : bool =
  List.exists (fun (a, b) -> a <= u.Units.uline_end && u.Units.uline_start <= b)
    ranges

let units_of_text ~path text =
  try Units.units_of_text ~path text
  with e ->
    Printf.eprintf "warn: failed to parse %s: %s\n" path (Printexc.to_string e);
    []

(* Drop hits nested in a better-ranked one (a fn and its own lambda): both
   point the reader at the same code. *)
let distinct_hits (hits : Engine.hit list) : Engine.hit list =
  List.fold_left
    (fun kept (h : Engine.hit) ->
      if
        List.exists
          (fun (k : Engine.hit) -> Engine.nests k.Engine.meta h.Engine.meta)
          kept
      then kept
      else h :: kept)
    [] hits
  |> List.rev

(* Drop findings nested in another finding (a changed lambda whose enclosing
   fn is reported too). *)
let outermost (findings : finding list) : finding list =
  let meta f = Units.meta_of f.unit_ in
  List.filter
    (fun f ->
      not
        (List.exists
           (fun g ->
             g != f
             && Engine.nests (meta g) (meta f)
             && g.unit_.Units.uline_end - g.unit_.Units.uline_start
                >= f.unit_.Units.uline_end - f.unit_.Units.uline_start)
           findings))
    findings

let rel (root : string) (file : string) : string =
  Option.value (strip_prefix (root ^ "/") file) ~default:file

let print_report root (checked : int) (findings : finding list) =
  match findings with
  | [] ->
      Printf.printf "nonna: no changed function resembles existing code (%d checked)\n"
        checked
  | _ ->
      let n = List.length findings in
      Printf.printf "nonna: %d changed function%s resemble%s existing code\n" n
        (if n = 1 then "" else "s")
        (if n = 1 then "s" else "");
      findings
      |> List.iter (fun f ->
             let u = f.unit_ in
             Printf.printf "\n  %s  %s:%d-%d  (%s)\n" u.Units.uname
               (rel root u.Units.ufile) u.Units.uline_start u.Units.uline_end
               (match f.status with Added -> "added" | Modified -> "modified");
             f.hits
             |> List.iter (fun (h : Engine.hit) ->
                    let m = h.Engine.meta in
                    Printf.printf "    ~ %s  %s:%d-%d  jaccard %.2f  containment %.2f\n"
                      m.Engine.name (rel root m.Engine.file) m.Engine.line_start
                      m.Engine.line_end h.Engine.jaccard h.Engine.containment));
      print_string
        "\n\
         Read the functions above before keeping this change. jaccard ~1: the \
         same function up to renaming. containment ~1: the existing function \
         already does everything the changed one does, and maybe more. Prefer \
         calling, extending or generalizing it over keeping a near-copy; if \
         the resemblance is intended, carry on.\n"

let run ~(staged : bool) ~(base : string option) ~(deps : bool)
    ~(strict : bool) ~(threshold : float) (paths : string list) : unit =
  let fail msg =
    prerr_endline ("nonna check: " ^ msg);
    exit 2
  in
  let root =
    match git [ "rev-parse"; "--show-toplevel" ] with
    | Some s -> (
        let r = String.trim s in
        try Unix.realpath r with _ -> r)
    | None -> fail "not inside a git repository"
  in
  let marker = Filename.concat root Workspace.marker_name in
  if Sys.file_exists marker then Workspace.apply_marker_config marker;
  let base =
    match base with
    | Some r -> (
        match git [ "merge-base"; r; "HEAD" ] with
        | Some s -> String.trim s
        | None -> fail ("no merge base with " ^ r))
    | None -> (
        match git [ "rev-parse"; "--verify"; "-q"; "HEAD" ] with
        | Some s -> String.trim s
        | None -> empty_tree)
  in
  let pathspec = "--" :: paths in
  let diff =
    match
      git
        ([ "diff"; "-U0"; "--no-color"; "--no-ext-diff"; "-M" ]
        @ (if staged then [ "--cached" ] else [])
        @ (base :: pathspec))
    with
    | Some d -> d
    | None -> fail "git diff failed"
  in
  let untracked =
    if staged then []
    else
      match
        git ([ "ls-files"; "--others"; "--exclude-standard"; "--full-name" ] @ pathspec)
      with
      | Some s ->
          lines s
          |> List.map (fun path ->
                 { path; old_path = None; ranges = [ (1, max_int) ] })
      | None -> []
  in
  let abs p = Filename.concat root p in
  let changes =
    parse_diff diff @ untracked
    |> List.filter (fun c ->
           List.mem (Filename.extension c.path) Units.indexable_exts
           && Workspace.file_ok (abs c.path))
  in
  let new_text c =
    if staged then git [ "show"; ":" ^ c.path ]
    else try Some (In_channel.with_open_bin (abs c.path) In_channel.input_all)
      with Sys_error _ -> None
  in
  let changed =
    changes
    |> List.filter_map (fun c ->
           Option.map
             (fun text -> (c, units_of_text ~path:(abs c.path) text))
             (new_text c))
  in
  (* the workspace as the change leaves it: disk, or the index when staged *)
  let staged_files = List.map (fun (c, _) -> abs c.path) changed in
  let disk =
    Corpus.index_files
      (Workspace.files [ root ]
      |> List.filter (fun f -> not (staged && List.mem f staged_files)))
  in
  let b = Engine.create () in
  let add (e : Sigdb.entry) = ignore (Engine.add b e.meta e.sg) in
  List.iter add disk;
  if staged then List.iter (fun (_, us) -> List.iter add (Corpus.entries_of_units us)) changed;
  if deps then ignore (Corpus.add_deps b root);
  let eng = Engine.freeze b in
  let sig_of (u : Units.unit_info) = Units.extract u in
  let checked = ref 0 in
  let findings =
    changed
    |> List.concat_map (fun (c, units) ->
           let old_units =
             match c.old_path with
             | None -> []
             | Some op -> (
                 match git [ "show"; base ^ ":" ^ op ] with
                 | Some text -> units_of_text ~path:(abs c.path) text
                 | None -> [])
           in
           units
           |> List.filter (touches c.ranges)
           |> List.filter_map (fun (u : Units.unit_info) ->
                  incr checked;
                  let previous =
                    List.filter
                      (fun (o : Units.unit_info) -> o.Units.uname = u.Units.uname)
                      old_units
                  in
                  (* what the previous version already resembled *)
                  let known =
                    previous
                    |> List.concat_map (fun o -> matches ~threshold eng o (sig_of o))
                    |> List.map (fun (h : Engine.hit) ->
                           (h.Engine.meta.Engine.file, h.Engine.meta.Engine.name))
                  in
                  let hits =
                    matches ~threshold eng u (sig_of u)
                    |> List.filter (fun (h : Engine.hit) ->
                           not
                             (List.mem
                                (h.Engine.meta.Engine.file, h.Engine.meta.Engine.name)
                                known))
                  in
                  let hits = distinct_hits hits in
                  if hits = [] then None
                  else
                    Some
                      {
                        unit_ = u;
                        status = (if previous = [] then Added else Modified);
                        hits;
                      }))
  in
  let findings = outermost findings in
  print_report root !checked findings;
  if strict && findings <> [] then exit 1
