(* The `.nonna` workspace marker and its config, shared by the MCP server
   and `nonna check`. *)

module Engine = Nonna_index.Engine
module Signature = Nonna_features.Signature
module Dfg = Nonna_features.Dfg

(* Opt-in gate: nonna only indexes a workspace that explicitly enables it with a
   `.nonna` marker file at the root. Registered at user scope, the server is
   spawned in every repo; without this gate it would index them all. *)
let marker_name = ".nonna"

(* Workspace path scope from .nonna: a file is indexed when it matches some
   `include` substring (or there are none) and no `exclude` substring — the
   same case-insensitive substring semantics as the tool filters. Without it
   a repo that vendors a parser or keeps benchmark corpora indexes all of it
   as "my code": nonna-v2 itself came out at 96k functions, 114s to index and
   126s per dupe scan, every top pair two versions of the same vendored
   package; scoped to its own sources it is a few thousand. *)
let include_paths : string list ref = ref []
let exclude_paths : string list ref = ref []

let file_ok (file : string) : bool =
  Engine.path_match ~include_paths:!include_paths ~exclude_paths:!exclude_paths
    file

let files (roots : string list) : string list =
  Units.source_files_of_paths roots |> List.filter file_ok

(* The marker doubles as per-workspace config: `key = value` lines, `#`
   comments. Keys: literals = true|false (see Dfg.set_literals), profile =
   structural|full, include / exclude = comma-separated path substrings (see
   [file_ok]). Anything else is ignored. Applied before indexing,
   so the settings shape the index itself (and the literals one is part of
   the sigdb profile tag). *)
let apply_marker_config (path : string) : unit =
  let csv v =
    String.split_on_char ',' v |> List.map String.trim
    |> List.filter (fun s -> s <> "")
  in
  Units.file_slice path 1 max_int
  |> List.iter (fun line ->
         match String.index_opt line '#' with
         | Some 0 -> ()
         | _ -> (
             match String.split_on_char '=' line with
             | [ k; v ] -> (
                 match (String.trim k, String.trim v) with
                 | "literals", ("true" | "on" | "1") -> Dfg.set_literals true
                 | "literals", ("false" | "off" | "0") -> Dfg.set_literals false
                 | "profile", "full" ->
                     Signature.default_profile := Signature.full_profile
                 | "profile", "structural" ->
                     Signature.default_profile := Signature.structural_profile
                 | "include", v -> include_paths := csv v
                 | "exclude", v -> exclude_paths := csv v
                 | _ -> ())
             | _ -> ()))
