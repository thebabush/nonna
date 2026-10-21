(* Clone classes over reported pairs (D18): `nonna dupes --classes`, MCP
 * find_duplicates group:true. A class is a connected component of the pairs
 * that survive the dupes filters and score >= the class threshold (default
 * the pair threshold). Single linkage chains (A~B, B~C, A and C unrelated),
 * so every class reports its lowest pair score and how many of its possible
 * pairs are present; a tighter --class-threshold splits a chain without
 * changing the pair list.
 *
 * Duplicated SLOC counts each line once: members nested in another member
 * (closures in their parent) fold into it, and the summary total does the
 * same across classes.
 *)

module Engine = Nonna_index.Engine

type clone_class = {
  members : Engine.meta list; (* representative first, then most central *)
  folded : Engine.meta list; (* members nested in another member *)
  kept : Engine.meta; (* the largest unfolded member: the copy you keep *)
  pairs : int; (* pairs present, of N(N-1)/2 *)
  j_min : float;
  j_max : float;
  sloc : int; (* median code lines of the unfolded members *)
  dup_sloc : int; (* Units.dup_sloc over the unfolded members *)
}

(* (file, line, end, name): identity and the location tie-break *)
let loc_key (m : Engine.meta) = (m.Engine.file, m.Engine.line_start, m.Engine.line_end, m.Engine.name)

(* The units no other unit of the list contains (Engine.nests); of equal
   ranges, the first by name. Lexical nesting never partially overlaps, so
   one sweep per file in (start asc, end desc) order sees each parent before
   its children. *)
let roots (ms : Engine.meta list) : Engine.meta list =
  let sorted =
    List.sort_uniq
      (fun (a : Engine.meta) (b : Engine.meta) ->
        compare
          (a.Engine.file, a.Engine.line_start, -a.Engine.line_end, a.Engine.name)
          (b.Engine.file, b.Engine.line_start, -b.Engine.line_end, b.Engine.name))
      ms
  in
  let _, acc =
    List.fold_left
      (fun ((last : Engine.meta option), acc) (m : Engine.meta) ->
        match last with
        | Some l when l.Engine.file = m.Engine.file && m.Engine.line_end <= l.Engine.line_end
          ->
            (last, acc)
        | _ -> (Some m, m :: acc))
      (None, []) sorted
  in
  List.rev acc

let code_lines (ms : Engine.meta list) : int =
  List.fold_left (fun n (m : Engine.meta) -> n + m.Engine.code_lines) 0 ms

let median (l : int list) : int =
  match List.sort compare l with
  | [] -> 0
  | s -> List.nth s (List.length s / 2)

(* Classes from [pairs], those scoring >= [threshold] on the dupes metric
   ([by_max]: max(j,c), else j) linked. Sorted by duplicated SLOC, then size,
   then the representative's location. *)
let classes ~(threshold : float) ~(by_max : bool) (pairs : Engine.pair list) :
    clone_class list =
  let pairs =
    List.filter
      (fun (p : Engine.pair) ->
        (if by_max then Float.max p.Engine.j p.Engine.c else p.Engine.j)
        >= threshold)
      pairs
  in
  (* union-find over members, keyed by location *)
  let parent : (string * int * int * string, Engine.meta) Hashtbl.t =
    Hashtbl.create 256
  in
  let rec find (m : Engine.meta) : Engine.meta =
    match Hashtbl.find_opt parent (loc_key m) with
    | None ->
        Hashtbl.replace parent (loc_key m) m;
        m
    | Some p when loc_key p = loc_key m -> m
    | Some p ->
        let r = find p in
        Hashtbl.replace parent (loc_key m) r;
        r
  in
  List.iter
    (fun (p : Engine.pair) ->
      let ra = find p.Engine.a and rb = find p.Engine.b in
      if loc_key ra <> loc_key rb then
        (* the smaller key roots the merge, so roots are deterministic *)
        if compare (loc_key ra) (loc_key rb) < 0 then
          Hashtbl.replace parent (loc_key rb) ra
        else Hashtbl.replace parent (loc_key ra) rb)
    pairs;
  let by_root : (string * int * int * string, Engine.pair list) Hashtbl.t =
    Hashtbl.create 64
  in
  List.iter
    (fun (p : Engine.pair) ->
      let k = loc_key (find p.Engine.a) in
      Hashtbl.replace by_root k
        (p :: Option.value (Hashtbl.find_opt by_root k) ~default:[]))
    pairs;
  Hashtbl.fold
    (fun _ (ps : Engine.pair list) acc ->
      let score : (string * int * int * string, float) Hashtbl.t =
        Hashtbl.create 16
      in
      let metas = Hashtbl.create 16 in
      let add (m : Engine.meta) j =
        Hashtbl.replace metas (loc_key m) m;
        Hashtbl.replace score (loc_key m)
          (j +. Option.value (Hashtbl.find_opt score (loc_key m)) ~default:0.)
      in
      List.iter
        (fun (p : Engine.pair) ->
          add p.Engine.a p.Engine.j;
          add p.Engine.b p.Engine.j)
        ps;
      let n = Hashtbl.length metas in
      (* centrality: mean jaccard to the other members, absent pairs 0 *)
      let mean m = Hashtbl.find score (loc_key m) /. float_of_int (n - 1) in
      let members =
        Hashtbl.fold (fun _ m l -> m :: l) metas []
        |> List.sort (fun a b ->
               compare (-.mean a, loc_key a) (-.mean b, loc_key b))
      in
      let top = roots members in
      let folded =
        List.filter
          (fun m -> not (List.exists (fun r -> loc_key r = loc_key m) top))
          members
        |> List.sort (fun a b -> compare (loc_key a) (loc_key b))
      in
      let kept =
        List.fold_left
          (fun (k : Engine.meta) (m : Engine.meta) ->
            if m.Engine.code_lines > k.Engine.code_lines then m else k)
          (List.hd top) top
      in
      let js = List.map (fun (p : Engine.pair) -> p.Engine.j) ps in
      {
        members;
        folded;
        kept;
        pairs = List.length ps;
        j_min = List.fold_left Float.min 1. js;
        j_max = List.fold_left Float.max 0. js;
        sloc = median (List.map (fun (m : Engine.meta) -> m.Engine.code_lines) top);
        dup_sloc =
          Units.dup_sloc (List.map (fun (m : Engine.meta) -> m.Engine.code_lines) top);
      }
      :: acc)
    by_root []
  |> List.sort (fun a b ->
         compare
           (-a.dup_sloc, -List.length a.members, loc_key (List.hd a.members))
           (-b.dup_sloc, -List.length b.members, loc_key (List.hd b.members)))

(* The unfolded members other than the kept copy: the lines you could
   delete. *)
let deleted (c : clone_class) : Engine.meta list =
  List.filter
    (fun m ->
      loc_key m <> loc_key c.kept
      && not (List.exists (fun f -> loc_key f = loc_key m) c.folded))
    c.members

(* Duplicated SLOC over all classes, each line once: a member deleted in one
   class and nested in a member deleted in another counts once. *)
let total_dup_sloc (cs : clone_class list) : int =
  code_lines (roots (List.concat_map deleted cs))

(* Code lines the block groups (D16) duplicate beyond [cs]: per group, every
   instance but the largest, minus lines inside a function the classes
   already delete; each (file, line) once. *)
let block_extra_sloc (cs : clone_class list) (gs : Blocks.group list) : int =
  let dels = roots (List.concat_map deleted cs) in
  let inside file line =
    List.exists
      (fun (m : Engine.meta) ->
        m.Engine.file = file && m.Engine.line_start <= line
        && line <= m.Engine.line_end)
      dels
  in
  let seen = Hashtbl.create 256 in
  List.iter
    (fun (g : Blocks.group) ->
      let biggest =
        List.fold_left
          (fun (k : Blocks.instance) (i : Blocks.instance) ->
            if i.Blocks.sloc > k.Blocks.sloc then i else k)
          (List.hd g.Blocks.instances) g.Blocks.instances
      in
      List.iter
        (fun (i : Blocks.instance) ->
          if i != biggest then
            List.iter
              (fun l ->
                if not (inside i.Blocks.file l) then
                  Hashtbl.replace seen (i.Blocks.file, l) ())
              i.Blocks.lines)
        g.Blocks.instances)
    gs;
  Hashtbl.length seen

let pct (n : int) (total : int) : float =
  if total = 0 then 0. else 100. *. float_of_int n /. float_of_int total

(* "C clone classes, F functions, D duplicated SLOC (P% of T code lines
   scanned)" — [scanned] is every unit scanned, nested ones included; its
   line count folds them too. *)
let summary (cs : clone_class list) ~(scanned : Engine.meta list) : string =
  let total = code_lines (roots scanned) in
  let dup = total_dup_sloc cs in
  Printf.sprintf
    "%d clone classes, %d functions, %d duplicated SLOC (%.1f%% of %d code \
     lines scanned)"
    (List.length cs)
    (List.fold_left (fun n c -> n + List.length c.members) 0 cs)
    dup (pct dup total) total

(* One class: header, then the representative and up to 4 more members (all
   with [expand]). *)
let render ~(expand : bool) (k : int) (c : clone_class) : string list =
  let n = List.length c.members in
  let header =
    Printf.sprintf "#%d  %d fns  j %.3f–%.3f  ~%d SLOC each (%d duplicated)  %d of %d pairs"
      k n c.j_min c.j_max c.sloc c.dup_sloc c.pairs (n * (n - 1) / 2)
  in
  let shown = if expand then c.members else List.filteri (fun i _ -> i < 5) c.members in
  let line i (m : Engine.meta) =
    Printf.sprintf "  %s  %s:%d%s%s" m.Engine.name m.Engine.file m.Engine.line_start
      (if i = 0 then "  (representative)" else "")
      (if List.exists (fun f -> loc_key f = loc_key m) c.folded then "  (nested)"
       else "")
  in
  (header :: List.mapi line shown)
  @
  if List.length shown < n then
    [ Printf.sprintf "  … %d more" (n - List.length shown) ]
  else []
