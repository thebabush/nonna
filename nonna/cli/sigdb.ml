(* Persistent signature databases (D9): one file per crate@version, cached
   globally — dep sources are immutable, so their signatures are too.
   Format: Marshal of (version, profile_tag, entries). The version constant
   MUST be bumped whenever feature extraction changes; stale caches are
   silently re-indexed. *)

module Engine = Nonna_index.Engine
module Signature = Nonna_features.Signature

(* bump on any change to hashing/features/weights, or to the [meta] record
   (Marshal is untyped: a reshaped record reads back as garbage, not an error) *)
let format_version = 4

type entry = { meta : Engine.meta; sg : Signature.t }

let profile_tag () : string =
  let module Dfg = Nonna_features.Dfg in
  let weights =
    Hashtbl.fold
      (fun name w acc -> (name, w) :: acc)
      Signature.weight_overrides []
    |> List.sort compare
  in
  let weight_tag =
    Digest.to_hex (Digest.string (Marshal.to_string weights []))
  in
  Printf.sprintf "%s-i%s-b%x-p%x-c%x-w%s"
    (if !Signature.default_profile = Signature.full_profile then "full"
     else "structural")
    (match !Dfg.iterations_override with
    | Some n -> string_of_int n
    | None ->
        Printf.sprintf "r%dp%dc%d"
          (Dfg.iters_for (Some Lang.Rust))
          (Dfg.iters_for (Some Lang.Python))
          (Dfg.iters_for (Some Lang.C)))
    (Dfg.cfg_bits (Dfg.base_cfg_for Lang.Rust))
    (Dfg.cfg_bits (Dfg.base_cfg_for Lang.Python))
    (Dfg.cfg_bits (Dfg.base_cfg_for Lang.C))
    weight_tag

let save (path : string) (entries : entry list) : unit =
  let tmp =
    Filename.temp_file ~temp_dir:(Filename.dirname path) "sigdb-" ".tmp"
  in
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists tmp then Sys.remove tmp)
    (fun () ->
      let oc = open_out_bin tmp in
      Fun.protect
        ~finally:(fun () -> close_out_noerr oc)
        (fun () ->
          Marshal.to_channel oc (format_version, profile_tag (), entries) [];
          flush oc);
      Sys.rename tmp path)

let load (path : string) : entry list option =
  if not (Sys.file_exists path) then None
  else
    try
      let ic = open_in_bin path in
      let v, tag, entries =
        Fun.protect
          ~finally:(fun () -> close_in_noerr ic)
          (fun () -> (Marshal.from_channel ic : int * string * entry list))
      in
      if v = format_version && tag = profile_tag () then Some entries else None
    with _ -> None
