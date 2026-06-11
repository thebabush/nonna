(* Ground-truth pair mining + engine evaluation.
 *
 * Labels come from TOKEN-level analysis of real code (independent of the
 * IL/feature pipeline, so the benchmark can't be circular):
 *
 *   pos/exact    same body token sequence, different location
 *   pos/renamed  same shape after renaming variable-position identifiers
 *                (call targets kept — mirrors the invariance we claim)
 *   pos/evolved  same crate (version stripped) + same file + same fn name,
 *                body tokens changed — real-world near-miss positives
 *   neg/random   different shape, token-jaccard < 0.3, size-matched
 *   neg/samefile different fns from the same file (same style: hard negative)
 *
 * `mine` writes a TSV; `eval` scores each pair with the engine and reports
 * per-kind score distributions, recall/FPR at thresholds, AUC, and the
 * worst failures for manual inspection.
 *)

module Fhash = Nonna_features.Fhash
module Signature = Nonna_features.Signature

(* ── Token normalization ─────────────────────────────────────────────────── *)

let rust_keywords =
  [
    "as"; "async"; "await"; "break"; "const"; "continue"; "crate"; "dyn";
    "else"; "enum"; "extern"; "false"; "fn"; "for"; "if"; "impl"; "in";
    "let"; "loop"; "match"; "mod"; "move"; "mut"; "pub"; "ref"; "return";
    "self"; "Self"; "static"; "struct"; "super"; "trait"; "true"; "type";
    "unsafe"; "use"; "where"; "while"; "union";
  ]

let python_keywords =
  [
    "False"; "None"; "True"; "and"; "as"; "assert"; "async"; "await";
    "break"; "class"; "continue"; "def"; "del"; "elif"; "else"; "except";
    "finally"; "for"; "from"; "global"; "if"; "import"; "in"; "is";
    "lambda"; "nonlocal"; "not"; "or"; "pass"; "raise"; "return"; "try";
    "while"; "with"; "yield"; "self"; "cls";
  ]

let kw_tbl_of (kws : string list) =
  let t = Hashtbl.create 64 in
  List.iter (fun k -> Hashtbl.replace t k ()) kws;
  t

let rust_kw_tbl = kw_tbl_of rust_keywords
let python_kw_tbl = kw_tbl_of python_keywords

(* ground-truth normalization must speak the file's language *)
let kw_tbl_for (file : string) =
  match Filename.extension file with
  | ".py" -> python_kw_tbl
  | _ -> rust_kw_tbl

let is_ident (s : string) =
  s <> ""
  && (match s.[0] with 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false)
  && String.for_all
       (function 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true | _ -> false)
       s

(* Rename variable-position identifiers to "$"; keep identifiers wherever the
   ENGINE keeps them — call/path position, macro names, and field position
   (after "."). Ground-truth invariance must mirror the claimed invariance. *)
let normalize ~kw_tbl (toks : string array) : string list =
  let n = Array.length toks in
  List.init n (fun i ->
      let t = toks.(i) in
      if not (is_ident t) || Hashtbl.mem kw_tbl t then t
      else
        let next = if i + 1 < n then toks.(i + 1) else "" in
        let prev = if i > 0 then toks.(i - 1) else "" in
        if next = "(" || next = "::" || prev = "::" || next = "!" || prev = "."
        then t
        else "$")

let hash_toks (l : string list) : int =
  let f = Fhash.Feed.create () in
  List.iter (Fhash.Feed.str f) l;
  Fhash.Feed.finish f

let uniq_sorted (l : string list) : string array =
  Array.of_list (List.sort_uniq compare l)

let set_jaccard (a : string array) (b : string array) : float =
  let na = Array.length a and nb = Array.length b in
  let rec go i j inter =
    if i >= na || j >= nb then inter
    else if a.(i) = b.(j) then go (i + 1) (j + 1) (inter + 1)
    else if a.(i) < b.(j) then go (i + 1) j inter
    else go i (j + 1) inter
  in
  let inter = go 0 0 0 in
  let union = na + nb - inter in
  if union = 0 then 0. else float_of_int inter /. float_of_int union

(* ── Mining ──────────────────────────────────────────────────────────────── *)

type uent = {
  file : string;
  line : int;
  line_end : int;
  name : string;
  ekey : int; (* exact token-sequence hash *)
  rkey : int; (* rename-normalized hash *)
  tokset : string array; (* normalized token set, for jaccard *)
  ntoks : int;
}

let min_tokens = 30

(* "<registry>/<name-version>/src/foo.rs" -> ("name", "src/foo.rs") *)
let crate_of (file : string) : string * string =
  let comps = String.split_on_char '/' file in
  (* "name-1.2.3" (semver suffix needs a dot: the registry index dir is
     "index.crates.io-<hex>" and must not match) *)
  let version_dash c =
    let n = String.length c in
    let rec find i =
      if i >= n - 1 then None
      else if
        c.[i] = '-'
        && (match c.[i + 1] with '0' .. '9' -> true | _ -> false)
        && String.contains_from c (i + 1) '.'
      then Some i
      else find (i + 1)
    in
    find 0
  in
  let rec go = function
    | [] -> ("", file)
    | [ _file ] -> ("", file)
    | c :: rest -> (
        match version_dash c with
        | Some i -> (String.sub c 0 i, String.concat "/" rest)
        | None -> go rest)
  in
  go comps

let collect (paths : string list) : uent list =
  Units.units_of_paths paths
  |> List.filter_map (fun (u : Units.unit_info) ->
         if Units.is_lambda u then None
         else
           let toks = Array.of_list u.Units.utokens in
           if Array.length toks < min_tokens then None
           else
             let norm = normalize ~kw_tbl:(kw_tbl_for u.Units.ufile) toks in
             Some
               {
                 file = u.Units.ufile;
                 line = u.Units.uline_start;
                 line_end = u.Units.uline_end;
                 name = u.Units.uname;
                 ekey = hash_toks (Array.to_list toks);
                 rkey = hash_toks norm;
                 tokset = uniq_sorted norm;
                 ntoks = Array.length toks;
               })

let pair_row label kind (a : uent) (b : uent) =
  Printf.sprintf "%s\t%s\t%s\t%d\t%d\t%s\t%s\t%d\t%d\t%s" label kind a.file
    a.line a.line_end a.name b.file b.line b.line_end b.name

let mine (paths : string list) (out : string) =
  let ents = collect paths in
  Printf.eprintf "mine: %d units (>= %d tokens)\n%!" (List.length ents)
    min_tokens;
  let rows = ref [] in
  let n_pos = ref 0 in

  (* positives: shared rename-key, distinct location; one pair per group *)
  let by_rkey : (int, uent list) Hashtbl.t = Hashtbl.create 1024 in
  List.iter
    (fun e ->
      Hashtbl.replace by_rkey e.rkey
        (e :: Option.value (Hashtbl.find_opt by_rkey e.rkey) ~default:[]))
    ents;
  Hashtbl.iter
    (fun _ group ->
      match List.sort (fun a b -> compare (a.file, a.line) (b.file, b.line)) group with
      | a :: rest -> (
          (* prefer a cross-file partner *)
          match List.find_opt (fun b -> b.file <> a.file) rest with
          | Some b ->
              let kind = if a.ekey = b.ekey then "exact" else "renamed" in
              rows := pair_row "pos" kind a b :: !rows;
              incr n_pos
          | None -> (
              match rest with
              | b :: _ when (b.file, b.line) <> (a.file, a.line) ->
                  let kind = if a.ekey = b.ekey then "exact" else "renamed" in
                  rows := pair_row "pos" kind a b :: !rows;
                  incr n_pos
              | _ -> ()))
      | [] -> ())
    by_rkey;

  (* evolved: same crate base + same in-crate path + same fn name,
     different shape (rkey) — the same function at two versions, edited.
     Names like `new`/`from` recur many times per file (different impl
     blocks); only names UNIQUE within their file are unambiguous. *)
  let name_count : (string * string, int) Hashtbl.t = Hashtbl.create 1024 in
  List.iter
    (fun e ->
      let k = (e.file, e.name) in
      Hashtbl.replace name_count k
        (1 + Option.value (Hashtbl.find_opt name_count k) ~default:0))
    ents;
  let unique e = Hashtbl.find_opt name_count (e.file, e.name) = Some 1 in
  let by_ident : (string, uent list) Hashtbl.t = Hashtbl.create 1024 in
  List.iter
    (fun e ->
      let base, rel = crate_of e.file in
      if base <> "" && unique e then
        let k = base ^ "|" ^ rel ^ "|" ^ e.name in
        Hashtbl.replace by_ident k
          (e :: Option.value (Hashtbl.find_opt by_ident k) ~default:[]))
    ents;
  (* semver "major" (0.x counts x as major, per convention) *)
  let major_of (file : string) : string =
    let base, _ = crate_of file in
    let comps = String.split_on_char '/' file in
    let vdir =
      List.find_opt
        (fun c ->
          String.length c > String.length base
          && String.sub c 0 (String.length base) = base
          && c.[String.length base] = '-')
        comps
    in
    match vdir with
    | None -> ""
    | Some c -> (
        let v =
          String.sub c (String.length base + 1)
            (String.length c - String.length base - 1)
        in
        match String.split_on_char '.' v with
        | "0" :: m :: _ -> "0." ^ m
        | m :: _ -> m
        | [] -> "")
  in
  let n_evolved = ref 0 in
  Hashtbl.iter
    (fun _ group ->
      match List.sort (fun a b -> compare (a.file, a.line) (b.file, b.line)) group with
      | a :: rest -> (
          match List.find_opt (fun b -> b.rkey <> a.rkey && b.file <> a.file) rest with
          | Some b ->
              let kind =
                if major_of a.file = major_of b.file then "evolved"
                else "evolved_major"
              in
              rows := pair_row "pos" kind a b :: !rows;
              incr n_evolved
          | None -> ())
      | [] -> ())
    by_ident;

  (* negatives *)
  let arr = Array.of_list ents in
  let n = Array.length arr in
  Random.init 42;
  let n_rand = ref 0 in
  let target_rand = max 100 (!n_pos + !n_evolved) in
  let attempts = ref 0 in
  while !n_rand < target_rand && !attempts < 50 * target_rand && n > 1 do
    incr attempts;
    let a = arr.(Random.int n) and b = arr.(Random.int n) in
    let ratio =
      float_of_int (min a.ntoks b.ntoks) /. float_of_int (max a.ntoks b.ntoks)
    in
    if
      a.rkey <> b.rkey
      && (a.file, a.line) < (b.file, b.line)
      && ratio >= 0.5
      && set_jaccard a.tokset b.tokset < 0.3
    then (
      rows := pair_row "neg" "random" a b :: !rows;
      incr n_rand)
  done;
  (* hard negatives: same file, different shape *)
  let by_file : (string, uent list) Hashtbl.t = Hashtbl.create 256 in
  List.iter
    (fun e ->
      Hashtbl.replace by_file e.file
        (e :: Option.value (Hashtbl.find_opt by_file e.file) ~default:[]))
    ents;
  let n_hard = ref 0 in
  Hashtbl.iter
    (fun _ group ->
      if !n_hard < target_rand / 2 then
        match List.sort (fun a b -> compare a.line b.line) group with
        | a :: rest -> (
            match
              List.find_opt
                (fun b ->
                  b.rkey <> a.rkey && set_jaccard a.tokset b.tokset < 0.35)
                rest
            with
            | Some b ->
                rows := pair_row "neg" "samefile" a b :: !rows;
                incr n_hard
            | None -> ())
        | [] -> ())
    by_file;

  let oc = open_out out in
  List.iter (fun r -> output_string oc (r ^ "\n")) (List.rev !rows);
  close_out oc;
  Printf.printf
    "mined %d pairs -> %s\n  pos: %d shape (exact+renamed), %d evolved\n\
    \  neg: %d random, %d samefile\n"
    (List.length !rows) out !n_pos !n_evolved !n_rand !n_hard

(* ── Evaluation ──────────────────────────────────────────────────────────── *)

type prow = {
  plabel : string;
  pkind : string;
  fa : string;
  la : int;
  na : string;
  fb : string;
  lb : int;
  nb : string;
}

let read_pairs (path : string) : prow list =
  let ic = open_in path in
  let rows = ref [] in
  (try
     while true do
       let line = input_line ic in
       match String.split_on_char '\t' line with
       | [ l; k; fa; la; _lae; na; fb; lb; _lbe; nb ] ->
           rows :=
             {
               plabel = l;
               pkind = k;
               fa;
               la = int_of_string la;
               na;
               fb;
               lb = int_of_string lb;
               nb;
             }
             :: !rows
       | _ -> ()
     done
   with End_of_file -> close_in ic);
  List.rev !rows

let pct (sorted : float array) (p : float) : float =
  let n = Array.length sorted in
  if n = 0 then nan
  else sorted.(min (n - 1) (int_of_float (p *. float_of_int (n - 1) +. 0.5)))

(* ── Ranking evaluation (search-engine metrics) ──────────────────────────── *)

(* For every positive pair (a, b): index the WHOLE corpus, query with a
   through the real pipeline (inverted-index candidates -> max(jaccard,containment)
   ranking), and find b's rank. Unlike pairwise eval this includes LSH
   candidate-selection losses. Ties (e.g. exact dupes across versions) don't
   count against b: rank = 1 + #strictly-better. *)
let rank (pairs_file : string) (corpus_paths : string list) =
  let module Engine = Nonna_index.Engine in
  let pairs = read_pairs pairs_file in
  (* pairs.tsv paths may differ cosmetically from walked paths (double
     slashes, symlinks) — join on the canonical form *)
  let canon (p : string) : string = try Unix.realpath p with _ -> p in
  let bld = Engine.create () in
  let fid_of : (string * int, int) Hashtbl.t = Hashtbl.create 8192 in
  let sig_of : (int, Signature.t) Hashtbl.t = Hashtbl.create 8192 in
  Units.units_of_paths corpus_paths
  |> List.iter (fun (u : Units.unit_info) ->
         let sg = Signature.extract ~lang:u.Units.ulang u.Units.ucfg in
         if Signature.size sg >= Units.min_features then (
           let fid = Engine.add bld (Units.meta_of u) sg in
           Hashtbl.replace fid_of (canon u.Units.ufile, u.Units.uline_start) fid;
           Hashtbl.replace sig_of fid sg))
  |> ignore;
  let eng = Engine.freeze bld in
  Printf.eprintf "rank: corpus indexed (%d units)\n%!" (Engine.size eng);

  (* kind -> ranks (0 = not found among LSH candidates) *)
  let ranks : (string, int list ref) Hashtbl.t = Hashtbl.create 8 in
  let skipped = ref 0 in
  let record kind r =
    match Hashtbl.find_opt ranks kind with
    | Some l -> l := r :: !l
    | None -> Hashtbl.add ranks kind (ref [ r ])
  in
  let key (h : Engine.hit) =
    (Float.max h.Engine.jaccard h.Engine.containment, h.Engine.jaccard)
  in
  pairs
  |> List.iter (fun p ->
         if p.plabel = "pos" then
           match
             ( Hashtbl.find_opt fid_of (canon p.fa, p.la),
               Hashtbl.find_opt fid_of (canon p.fb, p.lb) )
           with
           | Some fa, Some _ ->
               let qsig = Hashtbl.find sig_of fa in
               let hits =
                 Engine.query ~exclude:fa eng qsig ~threshold:0.0
                   ~max_results:max_int
               in
               let fb = canon p.fb in
               let target (h : Engine.hit) =
                 h.Engine.meta.Engine.file = fb
                 && h.Engine.meta.Engine.line_start = p.lb
               in
               (match List.find_opt target hits with
               | None -> record p.pkind 0 (* not a candidate *)
               | Some hb ->
                   let kb = key hb in
                   let better =
                     List.length (List.filter (fun h -> key h > kb) hits)
                   in
                   record p.pkind (better + 1))
           | _ -> incr skipped);

  Printf.printf "ranked positives over a %d-unit corpus (%d skipped: \
                 below min features / unparsed)\n\n"
    (Engine.size eng) !skipped;
  Printf.printf "%-14s %6s %7s %7s %7s %7s %9s\n" "kind" "n" "MRR" "r@1"
    "r@5" "r@10" "miss";
  let all_ranks = ref [] in
  Hashtbl.fold (fun k v acc -> (k, !v) :: acc) ranks []
  |> List.sort compare
  |> List.iter (fun (kind, rs) ->
         all_ranks := rs @ !all_ranks;
         let n = List.length rs in
         let nf = float_of_int (max 1 n) in
         let mrr =
           List.fold_left
             (fun a r -> a +. if r > 0 then 1. /. float_of_int r else 0.)
             0. rs
           /. nf
         in
         let at k =
           float_of_int (List.length (List.filter (fun r -> r > 0 && r <= k) rs))
           /. nf
         in
         let miss =
           float_of_int (List.length (List.filter (( = ) 0) rs)) /. nf
         in
         Printf.printf "%-14s %6d %7.3f %7.3f %7.3f %7.3f %9.3f\n" kind n mrr
           (at 1) (at 5) (at 10) miss);
  let rs = !all_ranks in
  let n = List.length rs in
  let nf = float_of_int (max 1 n) in
  let mrr =
    List.fold_left
      (fun a r -> a +. if r > 0 then 1. /. float_of_int r else 0.)
      0. rs
    /. nf
  in
  Printf.printf "%-14s %6d %7.3f\n" "ALL" n mrr

let eval ?(scores_out : string option) (pairs_file : string) =
  let pairs = read_pairs pairs_file in
  (* signature cache per file: line_start -> signature *)
  let cache : (string, (int, Signature.t) Hashtbl.t) Hashtbl.t =
    Hashtbl.create 256
  in
  let sig_at file line =
    let tbl =
      match Hashtbl.find_opt cache file with
      | Some t -> t
      | None ->
          let t = Hashtbl.create 16 in
          (try
             Units.units_of_file file
             |> List.iter (fun (u : Units.unit_info) ->
                    Hashtbl.replace t u.Units.uline_start
                      (Signature.extract ~lang:u.Units.ulang u.Units.ucfg))
           with _ -> ());
          Hashtbl.replace cache file t;
          t
    in
    Hashtbl.find_opt tbl line
  in
  let scored =
    pairs
    |> List.filter_map (fun p ->
           match (sig_at p.fa p.la, sig_at p.fb p.lb) with
           | Some sa, Some sb ->
               let j = Signature.jaccard sa sb in
               let c =
                 Float.max
                   (Signature.containment ~query:sa sb)
                   (Signature.containment ~query:sb sa)
               in
               Some (p, j, c)
           | _ -> None)
  in
  Printf.printf "evaluated %d/%d pairs (rest failed to re-locate)\n\n"
    (List.length scored) (List.length pairs);

  (match scores_out with
  | Some path ->
      let oc = open_out path in
      scored
      |> List.iter (fun (p, j, c) ->
             Printf.fprintf oc "%s\t%d\t%s\t%d\t%.4f\t%.4f\n" p.fa p.la p.fb
               p.lb j c);
      close_out oc;
      Printf.printf "per-pair scores -> %s\n\n" path
  | None -> ());

  (* per-kind stats *)
  let kinds =
    List.sort_uniq compare (List.map (fun (p, _, _) -> (p.plabel, p.pkind)) scored)
  in
  Printf.printf "%-14s %5s %7s %7s %7s %7s   %s\n" "kind" "n" "p10" "median"
    "p90" "mean"
    "j>=0.5 / j>=0.7 / max(j,c)>=0.5   (recall for pos, FPR for neg)";
  kinds
  |> List.iter (fun (lbl, kind) ->
         let sel =
           scored
           |> List.filter (fun (p, _, _) -> (p.plabel, p.pkind) = (lbl, kind))
         in
         let js = Array.of_list (List.map (fun (_, j, _) -> j) sel) in
         Array.sort compare js;
         let n = Array.length js in
         let nf = float_of_int (max 1 n) in
         let mean = Array.fold_left ( +. ) 0. js /. nf in
         let frac_ge t =
           float_of_int (List.length (List.filter (fun (_, j, _) -> j >= t) sel))
           /. nf
         in
         (* the product gate (D5): max of jaccard and containment *)
         let frac_max_ge t =
           float_of_int
             (List.length
                (List.filter (fun (_, j, c) -> Float.max j c >= t) sel))
           /. nf
         in
         Printf.printf
           "%-4s%-10s %5d %7.3f %7.3f %7.3f %7.3f   %.3f / %.3f / %.3f\n" lbl
           kind n (pct js 0.10) (pct js 0.50) (pct js 0.90) mean (frac_ge 0.5)
           (frac_ge 0.7) (frac_max_ge 0.5));

  (* worst failures *)
  let show (p, j, c) =
    Printf.printf "  j=%.3f c=%.3f  %s (%s:%d)  <->  %s (%s:%d)  [%s/%s]\n"
      j c p.na p.fa p.la p.nb p.fb p.lb p.plabel p.pkind
  in
  Printf.printf "\nworst positives (lowest jaccard):\n";
  scored
  |> List.filter (fun (p, _, _) -> p.plabel = "pos")
  |> List.sort (fun (_, a, _) (_, b, _) -> compare a b)
  |> List.filteri (fun i _ -> i < 10)
  |> List.iter show;
  Printf.printf "\nworst negatives (highest jaccard):\n";
  scored
  |> List.filter (fun (p, _, _) -> p.plabel = "neg")
  |> List.sort (fun (_, a, _) (_, b, _) -> compare b a)
  |> List.filteri (fun i _ -> i < 10)
  |> List.iter show
