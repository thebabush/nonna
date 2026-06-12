(* Persistent signature databases (D9): one file per crate@version, cached
   globally — dep sources are immutable, so their signatures are too.
   Format: Marshal of (version, profile_tag, entries). The version constant
   MUST be bumped whenever feature extraction changes; stale caches are
   silently re-indexed. *)

module Engine = Nonna_index.Engine
module Signature = Nonna_features.Signature

(* bump on any change to hashing/features/weights *)
let format_version = 3

type entry = { meta : Engine.meta; sg : Signature.t }

let profile_tag () : string =
  let module Dfg = Nonna_features.Dfg in
  Printf.sprintf "%s-i%s-b%x-p%x-c%x"
    (if !Signature.default_profile = Signature.full_profile then "full"
     else "structural")
    (match !Dfg.iterations_override with
    | Some n -> string_of_int n
    | None -> "L" (* per-language defaults *))
    (Dfg.cfg_bits (Dfg.base_cfg_for Lang.Rust))
    (Dfg.cfg_bits (Dfg.base_cfg_for Lang.Python))
    (Dfg.cfg_bits (Dfg.base_cfg_for Lang.C))

let save (path : string) (entries : entry list) : unit =
  let tmp = path ^ ".tmp" in
  let oc = open_out_bin tmp in
  Marshal.to_channel oc (format_version, profile_tag (), entries) [];
  close_out oc;
  Sys.rename tmp path

let load (path : string) : entry list option =
  if not (Sys.file_exists path) then None
  else
    try
      let ic = open_in_bin path in
      let v, tag, entries =
        (Marshal.from_channel ic : int * string * entry list)
      in
      close_in ic;
      if v = format_version && tag = profile_tag () then Some entries
      else None
    with _ -> None
