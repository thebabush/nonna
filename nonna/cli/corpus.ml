(* Corpus discovery (D3): workspace + transitive cargo deps + std.
 *
 * Deps come from `cargo metadata` (cargo has already downloaded sources);
 * std from `rustc --print sysroot` + the rust-src component. Each dep is
 * indexed once into a global sigdb cache (~/.cache/nonna) keyed by
 * resolved source, directory and extraction config; local code is never cached. *)

module J = Yojson.Safe
module JU = Yojson.Safe.Util
module Engine = Nonna_index.Engine
module Signature = Nonna_features.Signature

type dep = {
  name : string;
  version : string;
  src_dir : string;
  source : string option; (* None = mutable local/path dependency *)
}

let run_capture (cmd : string) : string option =
  try
    let ic = Unix.open_process_in cmd in
    let buf = Buffer.create 65536 in
    (try
       while true do
         Buffer.add_channel buf ic 65536
       done
     with End_of_file -> ());
    match Unix.close_process_in ic with
    | Unix.WEXITED 0 -> Some (Buffer.contents buf)
    | _ -> None
  with _ -> None

(* Transitive deps outside Cargo's explicit workspace member set, including
   mutable path dependencies. Offline fallback keeps this working without
   network when Cargo.lock + caches exist. *)
let cargo_deps (root : string) : dep list =
  if not (Sys.file_exists (Filename.concat root "Cargo.toml")) then []
  else
    let run extra =
      run_capture
        (Printf.sprintf
           "cd %s && cargo metadata --format-version 1 %s 2>/dev/null"
           (Filename.quote root) extra)
    in
    let out = match run "" with Some o -> Some o | None -> run "--offline" in
    match out with
    | None -> []
    | Some out -> (
        try
          let metadata = J.from_string out in
          let members = JU.member "workspace_members" metadata |> JU.to_list in
          let packages = JU.member "packages" metadata in
          JU.to_list packages
          |> List.filter_map (fun p ->
                 if List.mem (JU.member "id" p) members then None
                 else
                   let name = JU.member "name" p |> JU.to_string in
                   let version = JU.member "version" p |> JU.to_string in
                   let manifest = JU.member "manifest_path" p |> JU.to_string in
                   let src_dir = Unix.realpath (Filename.dirname manifest) in
                   let source =
                     match JU.member "source" p with
                     | `String s -> Some s
                     | _ -> None
                   in
                   Some { name; version; src_dir; source })
        with _ -> [])

(* std/core/alloc sources via the rust-src component (if installed). *)
let std_deps () : dep list =
  match run_capture "rustc --print sysroot 2>/dev/null" with
  | None -> []
  | Some sysroot -> (
      let sysroot = String.trim sysroot in
      let lib = Filename.concat sysroot "lib/rustlib/src/rust/library" in
      let version =
        match run_capture "rustc --version 2>/dev/null" with
        | Some v -> (
            match String.split_on_char ' ' (String.trim v) with
            | _ :: ver :: _ -> ver
            | _ -> "unknown")
        | None -> "unknown"
      in
      match Sys.is_directory lib with
      | true ->
          [ "core"; "alloc"; "std" ]
          |> List.filter_map (fun n ->
                 let d = Filename.concat lib n in
                 if Sys.file_exists d then
                   Some
                     {
                       name = "rust-" ^ n;
                       version;
                       src_dir = Unix.realpath d;
                       source = Some ("sysroot:" ^ sysroot);
                     }
                 else None)
      | false | (exception _) -> [])

let cache_dir () : string =
  let base =
    match Sys.getenv_opt "XDG_CACHE_HOME" with
    | Some d -> d
    | None -> Filename.concat (Sys.getenv "HOME") ".cache"
  in
  let dir = Filename.concat base "nonna/sigdb" in
  let rec mkdirs d =
    if not (Sys.file_exists d) then (
      mkdirs (Filename.dirname d);
      try Unix.mkdir d 0o755 with _ -> ())
  in
  mkdirs dir;
  dir

let index_dir (dir : string) : Sigdb.entry list =
  Units.units_of_paths [ dir ]
  |> List.filter_map (fun (u : Units.unit_info) ->
         let sg = Signature.extract ~lang:u.Units.ulang u.Units.ucfg in
         if Signature.size sg < Units.min_features then None
         else Some { Sigdb.meta = Units.meta_of u; sg })

(* Load a dep's sigdb from cache, indexing + caching on miss. *)
let entries_of_dep (d : dep) : Sigdb.entry list =
  match d.source with
  | None -> index_dir d.src_dir
  | Some source -> (
      let identity =
        Digest.to_hex (Digest.string (source ^ "\000" ^ d.src_dir))
      in
      let cache =
        Filename.concat (cache_dir ())
          (Printf.sprintf "%s-%s-%s-%s-v%d.bin" d.name d.version identity
             (Sigdb.profile_tag ()) Sigdb.format_version)
      in
      match Sigdb.load cache with
      | Some entries
        when List.for_all
               (fun (e : Sigdb.entry) -> Sys.file_exists e.meta.file)
               entries ->
          entries
      | Some _ | None ->
          let entries = index_dir d.src_dir in
          (try Sigdb.save cache entries with _ -> ());
          entries)

let dependencies root =
  cargo_deps root
  @
  if Sys.file_exists (Filename.concat root "Cargo.toml") then std_deps ()
  else []

(* Extend an engine with all deps + std of a cargo workspace.
   Returns (dep count, function count added); logs per-dep via [log]. *)
let add_deps (b : Engine.builder) ?(log = fun (_ : string) -> ())
    (root : string) : int * int =
  (* std belongs in the corpus only for cargo projects — a Python workspace
     gains nothing from 19k Rust std functions *)
  let deps = dependencies root in
  let added = ref 0 in
  deps
  |> List.iter (fun d ->
         let entries = entries_of_dep d in
         List.iter
           (fun (e : Sigdb.entry) ->
             ignore (Engine.add b e.Sigdb.meta e.Sigdb.sg))
           entries;
         added := !added + List.length entries;
         log
           (Printf.sprintf "  %s-%s: %d fns" d.name d.version
              (List.length entries)));
  (List.length deps, !added)
