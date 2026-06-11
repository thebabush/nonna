(* Iterative DFG feature extraction over the Opengrep IL.
 *
 * Port of v1's iterative_dfg.rs (iterative propagation): every def-site
 * gets a local "seed" hash, then N propagation rounds mix in predecessor
 * hashes (position-tagged, commutativity-aware). Features are emitted at
 * every round, so the signature contains both local (most rename-invariant)
 * and progressively more contextual hashes.
 *
 * Adaptations forced by the IL (D12, documented in DESIGN.md):
 * - The IL lifts calls/assigns out of expressions but keeps pure operator
 *   trees nested, so the seed hash folds the whole side-effect-free exp tree
 *   recursively. Commutative operators combine child hashes order-insensitively.
 * - Comparisons/conditions live in NCond CFG nodes (not assignments), and
 *   returns/throws carry exps; these become emitting, non-defining DFG nodes.
 * - Variables are IL names keyed by their gensym-unique `sid` (Naming_AST has
 *   already resolved scoping/shadowing); v1's VarId is `sid` here.
 * - Macro remnants (FixmeExp/FixmeInstr) hash as name + token bag (MacroBag).
 *
 * `graph_of` exposes the assembled DFG plus a hash snapshot per propagation
 * round (round 0 = local seeds) for debugging/visualization; `extract`
 * derives the tagged feature list from those snapshots.
 *)

module G = AST_generic

type tag =
  | BinOp
  | UnOp
  | Call
  | ConstString
  | ConstOther
  | Field
  | Index
  | Construct
  | MacroBag
  | Control (* conditions, returns, throws *)

let tag_name = function
  | BinOp -> "binop"
  | UnOp -> "unop"
  | Call -> "call"
  | ConstString -> "str"
  | ConstOther -> "const"
  | Field -> "field"
  | Index -> "index"
  | Construct -> "construct"
  | MacroBag -> "macro"
  | Control -> "control"

type feature = { hash : Fhash.t; tag : tag }

(* Propagation depth. Runtime-tunable (--iters). Final-round-only emission
   schemes want ~3 rounds; ours emits EVERY round cumulatively,
   so deeper rounds only add increasingly brittle features. Swept 1-5 against
   the mined benchmark (2026-06-11): depth 1 dominates — evolved j-recall@0.5
   0.729 -> 0.823, AUC 0.9970 -> 0.9975, rank renamed-MRR 0.994 -> 1.000,
   evolved_major candidate-miss halved, cross-style N7 case 0.40 -> 0.56 —
   with FPR flat and ~3x cheaper extraction. *)
let iterations = ref 1

(* ── Local (seed) hashing of expressions ─────────────────────────────────── *)

let op_commutative (op : G.operator) =
  match op with
  | G.Plus | G.Mult | G.BitOr | G.BitXor | G.BitAnd | G.And | G.Or | G.Xor
  | G.Eq | G.NotEq | G.PhysEq | G.NotPhysEq ->
      true
  | _ -> false

let composite_descr (ck : IL.composite_kind) =
  match ck with
  | IL.CTuple -> "tuple"
  | IL.CArray -> "array"
  | IL.CList -> "list"
  | IL.CSet -> "set"
  | IL.Constructor n -> "ctor:" ^ fst n.IL.ident
  | IL.Regexp -> "regexp"

let leaf (tagv : int) (strs : string list) : Fhash.t =
  let f = Fhash.Feed.create () in
  Fhash.Feed.tag f tagv;
  List.iter (Fhash.Feed.str f) strs;
  Fhash.Feed.finish f

let var_leaf = leaf 0x14 [] (* bare variable read: anonymous (rename-invariant) *)

(* What goes into the seed hashes (D14). The BASELINE is name-free, value-free
   structural hashing: operator kinds, call kinds (named/method/dynamic/
   builtin), arities, offsets, composite kinds, control shape. Every flag
   below admits one extra signal channel; `semantic_cfg` (all on) is the rich
   end, emitted as a salted delta channel on top of the structural base. *)
type cfg = {
  call_names : bool; (* call / constructor target name strings *)
  field_names : bool; (* field names: Dot offsets, record fields, stores *)
  int_values : bool; (* integer/bool literal values (kinds always kept) *)
  float_values : bool; (* float literal values *)
  string_values : bool; (* string/char/regexp/atom literal values *)
  ty_descrs : bool; (* declared-type descriptors (params / casts / new) *)
  param_pos : bool; (* parameter position in param seeds *)
  macro_tokens : bool; (* macro token bags (FixmeExp / FixmeInstr) *)
}

let structural_cfg =
  {
    call_names = false;
    field_names = false;
    int_values = false;
    float_values = false;
    string_values = false;
    ty_descrs = false;
    param_pos = false;
    macro_tokens = false;
  }

let semantic_cfg =
  {
    call_names = true;
    field_names = true;
    int_values = true;
    float_values = true;
    string_values = true;
    ty_descrs = true;
    param_pos = true;
    macro_tokens = true;
  }

(* does this literal kind's VALUE go into the seed under [fc]? *)
let keep_value (fc : cfg) (kind : string) : bool =
  match kind with
  | "int" | "bool" -> fc.int_values
  | "float" | "imag" | "ratio" -> fc.float_values
  | "string" | "char" | "regexp" | "atom" -> fc.string_values
  | _ -> false

(* The cfg used for the BASE hashes (and as the delta channel's base).
   D14 default, revised by the 2026-06-11 rank sweep: structural plus
   call_names + int_values — the best measured combo (ALL MRR 0.963,
   evolved MRR +7.2pp vs all-off; call targets are API identity, not
   naming noise). Everything else stays off; --with folds channels in
   at runtime for ablations. *)
let base_cfg : cfg ref =
  ref { structural_cfg with call_names = true; int_values = true }

(* compact tag for cache keys / reports *)
let cfg_bits (c : cfg) : int =
  (if c.call_names then 1 else 0)
  lor (if c.field_names then 2 else 0)
  lor (if c.int_values then 4 else 0)
  lor (if c.float_values then 8 else 0)
  lor (if c.string_values then 16 else 0)
  lor (if c.ty_descrs then 32 else 0)
  lor (if c.param_pos then 64 else 0)
  lor (if c.macro_tokens then 128 else 0)

(* Recursively hash a side-effect-free exp; also collect the names it reads,
   in traversal order (these become DFG predecessor edges). *)
let rec hash_exp ~(fc : cfg) (e : IL.exp) : Fhash.t * IL.name list =
  let hash_exp = hash_exp ~fc in
  let hash_lval = hash_lval ~fc in
  match e.IL.e with
  | IL.Fetch lval -> hash_lval lval
  | IL.Literal lit ->
      let kind, value = Il_util.const_descr lit in
      let parts =
        if keep_value fc kind then kind :: Option.to_list value else [ kind ]
      in
      (leaf 0x13 parts, [])
  | IL.Cast (ty, e1) ->
      let h1, ns = hash_exp e1 in
      let parts = if fc.ty_descrs then [ Il_util.ty_descr ty ] else [] in
      (Fhash.mix (leaf 0x1A parts) h1, ns)
  | IL.Operator ((op, _), args) ->
      let parts = List.map (fun a -> hash_exp (IL_helpers.exp_of_arg a)) args in
      let hs = List.map fst parts in
      let ns = List.concat_map snd parts in
      let seed =
        leaf 0x10 [ G.show_operator op; string_of_int (List.length args) ]
      in
      let hs = if op_commutative op then List.sort compare hs else hs in
      (List.fold_left Fhash.mix seed hs, ns)
  | IL.Composite (ck, (_, xs, _)) ->
      let parts = List.map hash_exp xs in
      let ck_descr =
        match ck with
        | IL.Constructor _ when not fc.call_names -> "ctor"
        | _ -> composite_descr ck
      in
      let seed = leaf 0x17 [ ck_descr; string_of_int (List.length xs) ] in
      ( List.fold_left Fhash.mix seed (List.map fst parts),
        List.concat_map snd parts )
  | IL.RecordOrDict fes ->
      let parts =
        List.map
          (function
            | IL.Field (n, e1) ->
                let h, ns = hash_exp e1 in
                let fparts =
                  if fc.field_names then [ fst n.IL.ident ] else []
                in
                (Fhash.mix (leaf 0x18 fparts) h, ns)
            | IL.Entry (k, v) ->
                let hk, nk = hash_exp k in
                let hv, nv = hash_exp v in
                (Fhash.mix hk hv, nk @ nv)
            | IL.Spread e1 ->
                let h, ns = hash_exp e1 in
                (Fhash.mix (leaf 0x19 []) h, ns))
          fes
      in
      let seed = leaf 0x1C [ string_of_int (List.length fes) ] in
      ( List.fold_left Fhash.mix seed (List.map fst parts),
        List.concat_map snd parts )
  | IL.FixmeExp (_, any, eopt) ->
      let toks =
        if fc.macro_tokens then Il_util.token_strings_of_any any else []
      in
      let h = leaf 0x1F toks in
      (match eopt with
      | Some e1 ->
          let h1, ns = hash_exp e1 in
          (Fhash.mix h h1, ns)
      | None -> (h, []))

(* Hash an lval's offset chain in application order; collect names read. *)
and hash_lval ~(fc : cfg) (lval : IL.lval) : Fhash.t * IL.name list =
  let hash_exp = hash_exp ~fc in
  let base_h, base_ns =
    match lval.IL.base with
    | IL.Var n -> (var_leaf, [ n ])
    | IL.VarSpecial (sp, _) -> (leaf 0x21 [ IL.show_var_special sp ], [])
    | IL.Mem e ->
        let h, ns = hash_exp e in
        (Fhash.mix (leaf 0x22 []) h, ns)
  in
  List.fold_left
    (fun (h, ns) (o : IL.offset) ->
      match o.IL.o with
      | IL.Dot n ->
          let fparts = if fc.field_names then [ fst n.IL.ident ] else [] in
          (Fhash.mix h (leaf 0x15 fparts), ns)
      | IL.Index e ->
          let he, ns2 = hash_exp e in
          (Fhash.mix (Fhash.mix h (leaf 0x16 [])) he, ns @ ns2)
      | IL.Slice i -> (Fhash.mix h (leaf 0x23 [ string_of_int i ]), ns))
    (base_h, base_ns)
    (List.rev lval.IL.rev_offset)

(* ── DFG node construction ───────────────────────────────────────────────── *)

(* A predecessor name plus how to treat it when it doesn't resolve locally:
   callee names of direct calls are OptEdge (the call target is already in the
   seed hash; a global fn name is not dataflow), everything else falls back
   to the shared dangling sentinel (v1 parity). *)
type pred_name = Edge of IL.name | OptEdge of IL.name

type local = {
  seed : Fhash.t;
  pnames : pred_name list;
  commutative : bool;
  emit : bool;
  ltag : tag;
  descr : string; (* human-readable, for graph dumps *)
}

let mk ?(comm = false) ?(emit = true) ~tag ~descr seed pnames =
  { seed; pnames; commutative = comm; emit; ltag = tag; descr }

let edges ns = List.map (fun n -> Edge n) ns

let truncate (n : int) (s : string) =
  if String.length s <= n then s else String.sub s 0 (n - 1) ^ "…"

let const_label lit =
  let kind, value = Il_util.const_descr lit in
  match value with
  | Some v ->
      if kind = "string" then "\"" ^ truncate 10 v ^ "\"" else truncate 12 v
  | None -> kind

(* Classify + locally hash one IL instruction. *)
let local_of_instr ~(fc : cfg) (i : IL.instr) : local =
  let hash_exp = hash_exp ~fc in
  let hash_lval = hash_lval ~fc in
  match i.IL.i with
  | IL.Assign (lval, exp) -> (
      let hexp, rhs_names = hash_exp exp in
      match (lval.IL.base, lval.IL.rev_offset) with
      | IL.Var _, [] -> (
          let seed = Fhash.mix (leaf 0x01 []) hexp in
          let preds = edges rhs_names in
          match exp.IL.e with
          | IL.Fetch { IL.base = IL.Var _; rev_offset = [] } ->
              (* bare copy: propagate through, don't emit *)
              mk ~emit:false ~tag:BinOp ~descr:"copy" seed preds
          | IL.Fetch { IL.rev_offset = { o = IL.Dot f; _ } :: _; _ } ->
              mk ~tag:Field ~descr:("." ^ fst f.IL.ident) seed preds
          | IL.Fetch _ -> mk ~tag:Index ~descr:"[..]" seed preds
          | IL.Literal (G.String _ as lit) ->
              mk ~tag:ConstString ~descr:(const_label lit) seed preds
          | IL.Literal lit ->
              mk ~tag:ConstOther ~descr:(const_label lit) seed preds
          | IL.Composite (ck, (_, [], _)) ->
              (* empty container literals: ubiquitous boilerplate, no signal *)
              mk ~emit:false ~tag:Construct
                ~descr:(composite_descr ck ^ "[]")
                seed preds
          | IL.Composite (ck, _) ->
              mk ~tag:Construct ~descr:(composite_descr ck) seed preds
          | IL.RecordOrDict _ -> mk ~tag:Construct ~descr:"record" seed preds
          | IL.Operator ((op, _), args) ->
              let t = if List.length args >= 2 then BinOp else UnOp in
              mk
                ~comm:(op_commutative op)
                ~tag:t ~descr:(G.show_operator op) seed preds
          | IL.FixmeExp _ -> mk ~tag:MacroBag ~descr:"macro" seed preds
          | IL.Cast (ty, _) ->
              mk ~emit:false ~tag:BinOp
                ~descr:("as " ^ Il_util.ty_descr ty)
                seed preds)
      | _ ->
          (* store: x.f = v / x[i] = v — emits, defines nothing (v1 parity) *)
          let hl, lnames = hash_lval lval in
          let t, d =
            match lval.IL.rev_offset with
            | { o = IL.Dot f; _ } :: _ -> (Field, "store ." ^ fst f.IL.ident)
            | _ -> (Index, "store [..]")
          in
          let seed = Fhash.mix (leaf 0x02 []) (Fhash.mix hl hexp) in
          mk ~tag:t ~descr:d seed (edges (lnames @ rhs_names)))
  | IL.AssignAnon (_, _) ->
      (* lambda/anon class: separate unit per D6; pass-through here *)
      mk ~emit:false ~tag:Construct ~descr:"lambda" (leaf 0x03 []) []
  | IL.Call (_, fexp, args) ->
      (* the call KIND (named/method/dynamic) is structural and always kept;
         the target NAME is a flagged channel *)
      let ckind, cname, callee_preds =
        match fexp.IL.e with
        | IL.Fetch { IL.base = IL.Var fn; rev_offset = [] } ->
            ("named", fst fn.IL.ident, [ OptEdge fn ])
        | IL.Fetch ({ IL.rev_offset = { o = IL.Dot m; _ } :: _; _ } as lv) ->
            let _, ns = hash_lval lv in
            ("method", fst m.IL.ident, edges ns)
        | _ ->
            let _, ns = hash_exp fexp in
            ("dynamic", "", edges ns)
      in
      let target_parts =
        ckind :: (if fc.call_names && cname <> "" then [ cname ] else [])
      in
      let parts = List.map (fun a -> hash_exp (IL_helpers.exp_of_arg a)) args in
      let seed =
        List.fold_left Fhash.mix
          (leaf 0x12 (target_parts @ [ string_of_int (List.length args) ]))
          (List.map fst parts)
      in
      let short = if cname = "" then ckind else cname in
      mk ~tag:Call
        ~descr:(Printf.sprintf "%s(%d)" (truncate 14 short) (List.length args))
        seed
        (callee_preds @ edges (List.concat_map snd parts))
  | IL.CallSpecial (_, (sp, _), args) ->
      let parts = List.map (fun a -> hash_exp (IL_helpers.exp_of_arg a)) args in
      let seed =
        List.fold_left Fhash.mix
          (leaf 0x12
             [
               "special:" ^ IL.show_call_special sp;
               string_of_int (List.length args);
             ])
          (List.map fst parts)
      in
      mk ~tag:Call
        ~descr:(truncate 16 (IL.show_call_special sp))
        seed
        (edges (List.concat_map snd parts))
  | IL.New (_, ty, ctor, args) ->
      let parts =
        (match ctor with Some e -> [ hash_exp e ] | None -> [])
        @ List.map (fun a -> hash_exp (IL_helpers.exp_of_arg a)) args
      in
      let ty_part = if fc.ty_descrs then [ Il_util.ty_descr ty ] else [] in
      let seed =
        List.fold_left Fhash.mix
          (leaf 0x1B (ty_part @ [ string_of_int (List.length args) ]))
          (List.map fst parts)
      in
      mk ~tag:Construct
        ~descr:("new " ^ truncate 12 (Il_util.ty_descr ty))
        seed
        (edges (List.concat_map snd parts))
  | IL.FixmeInstr (_, any) ->
      let toks =
        if fc.macro_tokens then Il_util.token_strings_of_any any else []
      in
      mk ~tag:MacroBag ~descr:"macro!" (leaf 0x1F toks) []

(* Control nodes (cond/return/throw): emitting, non-defining. *)
let local_of_control ~(fc : cfg) (tagv : int) ~(descr : string)
    ~(emit_trivial : bool) (e : IL.exp) : local =
  let h, ns = hash_exp ~fc e in
  let comm, op_str =
    match e.IL.e with
    | IL.Operator ((op, _), _) -> (op_commutative op, " " ^ G.show_operator op)
    | _ -> (false, "")
  in
  let emit = emit_trivial || not (Il_util.exp_is_trivial e) in
  mk ~comm ~emit ~tag:Control ~descr:(descr ^ op_str)
    (Fhash.mix (leaf tagv []) h)
    (edges ns)

(* ── Graph assembly + iterative propagation ──────────────────────────────── *)

type nkind = KOp | KParam | KSentinel | KPhi

type dnode = {
  mutable hash : int;
  mutable prev : int;
  mutable preds : (int * int) array; (* (node index, operand position) *)
  mutable commutative : bool;
  mutable emit : bool;
  mutable dtag : tag;
  mutable dlabel : string;
  mutable dkind : nkind;
  mutable dline : int; (* source line (via orig), 0 = unknown *)
}

let fresh_dnode () =
  {
    hash = 0;
    prev = 0;
    preds = [||];
    commutative = false;
    emit = false;
    dtag = BinOp;
    dlabel = "";
    dkind = KOp;
    dline = 0;
  }

let mask62 = 0x3FFFFFFFFFFFFFFF

(* The assembled DFG plus one hash snapshot per propagation round.
   rounds.(0) = local seeds; rounds.(r) = state after round r. *)
type graph = { dnodes : dnode array; rounds : int array array }

module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)

let graph_of ?(fc : cfg = !base_cfg) (fcfg : IL.fun_cfg) : graph =
  let nodes : dnode Dynarray.t = Dynarray.create () in
  let push (d : dnode) : int =
    Dynarray.add_last nodes d;
    Dynarray.length nodes - 1
  in

  (* Dangling-use sentinel (v1's 0xEE external-input marker), shared by the
     root unit and all grafted closures. *)
  let sentinel_idx =
    let d = fresh_dnode () in
    let h = leaf 0xEE [] in
    d.hash <- h;
    d.prev <- h;
    d.dlabel <- "extern";
    d.dkind <- KSentinel;
    push d
  in

  (* Build one fn-unit's subgraph into the shared node array. [captures] are
     the definitions visible at a closure's creation site (empty for the root
     unit) — exactly what the closure captures. Returns the DFG indices of
     the unit's return nodes.

     N7 (closure grafting): an `AssignAnon (x, Lambda)` whose lambda CFG is in
     fcfg.lambdas recursively builds the closure body HERE, with the defs
     reaching the creation site as its captures; the AssignAnon node becomes a
     pass-through fed by the closure's returns. Combinator-style code
     (`opt.map(|i| …)`) thus carries its closure dataflow in the parent's
     signature, matching the same algorithm written inline. The lambda is
     still also indexed as its own flat unit (D6). *)
  let rec build (fcfg : IL.fun_cfg) (captures : IntSet.t IntMap.t) : int list =
  (* Parameter nodes: seeded by position + declared type, non-emitting.
     For ParamPattern (Rust!), the IL pname is a synthetic implicit param
     whose sid differs from the pattern-bound names used by the body —
     register every bound sid against the same param node. Param defs are
     the definitions reaching Enter. *)
  let param_defs : IntSet.t IntMap.t ref = ref captures in
  fcfg.IL.params
  |> List.iteri (fun i p ->
         let pname_binding =
           match IL_helpers.pname_of_param p with
           | Some pname ->
               [
                 ( G.SId.to_int pname.IL.sid,
                   !(pname.IL.id_info.G.id_type) );
               ]
           | None -> []
         in
         let pat_bindings =
           match p with
           | IL.ParamPattern (_, pat) -> Il_util.pat_bound_sids pat
           | IL.Param _ | IL.ParamRest _ | IL.ParamFixme -> []
         in
         let bindings = pname_binding @ pat_bindings in
         if bindings <> [] then (
           let ty =
             match
               (fc.ty_descrs, List.find_map (fun (_, t) -> t) bindings)
             with
             | true, Some t -> Il_util.ty_descr t
             | _ -> ""
           in
           let pos = if fc.param_pos then string_of_int i else "" in
           let d = fresh_dnode () in
           let h = leaf 0xE0 [ pos; ty ] in
           d.hash <- h;
           d.prev <- h;
           d.dlabel <-
             Printf.sprintf "p%d%s" i (if ty = "" then "" else ": " ^ ty);
           d.dkind <- KParam;
           let idx = push d in
           List.iter
             (fun (sid, _) ->
               param_defs :=
                 IntMap.add sid (IntSet.singleton idx) !param_defs)
             bindings));

  (* Pass 1: reserve a DFG node per relevant CFG node; record which CFG node
     generates which definition (sid -> dfg node). *)
  let cfg = fcfg.IL.cfg in
  let cfg_nodes = Il_util.nodes_in_order cfg in
  let gen : (int, int * int) Hashtbl.t = Hashtbl.create 32 in
  let pending : (int * int * IL.node_kind) list =
    List.filter_map
      (fun ((ni : int), (node : IL.node)) ->
        match node.IL.n with
        | IL.NInstr i ->
            let idx = push (fresh_dnode ()) in
            (match IL_helpers.lval_of_instr_opt i with
            | Some { IL.base = IL.Var n; rev_offset = [] } ->
                Hashtbl.replace gen ni (G.SId.to_int n.IL.sid, idx)
            | Some _ | None -> ());
            Some (idx, ni, node.IL.n)
        | IL.NCond _ | IL.NReturn _ | IL.NThrow _ ->
            Some (push (fresh_dnode ()), ni, node.IL.n)
        | IL.Enter | IL.Exit | IL.TrueNode _ | IL.FalseNode _ | IL.Join
        | IL.NGoto _ | IL.NOther _ | IL.NTodo _ ->
            None)
      cfg_nodes
  in

  (* Reaching definitions (forward may-analysis, worklist fixpoint):
     IN(n) = union of OUT(p); OUT(n) = IN(n) with n's def overriding its sid.
     Params are definitions live at Enter. This is what makes the DFG
     SSA-like: a use binds to its *reaching* definitions, and a use reached
     by several gets a phi merge node, instead of last-write-wins binding. *)
  let in_map : (int, IntSet.t IntMap.t) Hashtbl.t = Hashtbl.create 64 in
  let get_in ni =
    Option.value (Hashtbl.find_opt in_map ni) ~default:IntMap.empty
  in
  let out_of ni m =
    match Hashtbl.find_opt gen ni with
    | Some (sid, idx) -> IntMap.add sid (IntSet.singleton idx) m
    | None -> m
  in
  Hashtbl.replace in_map cfg.CFG.entry !param_defs;
  let queue = Queue.create () in
  Queue.push cfg.CFG.entry queue;
  while not (Queue.is_empty queue) do
    let ni = Queue.pop queue in
    let out = out_of ni (get_in ni) in
    CFG.successors cfg ni
    |> List.iter (fun (si, _) ->
           let cur = get_in si in
           let merged =
             IntMap.union (fun _ a b -> Some (IntSet.union a b)) cur out
           in
           if not (IntMap.equal IntSet.equal merged cur) then (
             Hashtbl.replace in_map si merged;
             Queue.push si queue))
  done;

  (* Phi merge nodes, memoized per def-set (v1's Op::Phi: commutative,
     non-emitting, seed = arity only). Created on demand — no dominance
     frontiers needed. *)
  let phi_memo : (int list, int) Hashtbl.t = Hashtbl.create 8 in
  let phi_node (defs : int list) : int =
    match Hashtbl.find_opt phi_memo defs with
    | Some i -> i
    | None ->
        let d = fresh_dnode () in
        let k = List.length defs in
        let h = leaf 0x05 [ string_of_int k ] in
        d.hash <- h;
        d.prev <- h;
        d.commutative <- true;
        d.dlabel <- Printf.sprintf "φ%d" k;
        d.dkind <- KPhi;
        d.preds <- Array.of_list (List.mapi (fun pos i -> (i, pos)) defs);
        let idx = push d in
        Hashtbl.add phi_memo defs idx;
        idx
  in
  (* Resolve a use of [sid] at CFG node [ni] to a DFG node. *)
  let resolve ni sid : int option =
    match IntMap.find_opt sid (get_in ni) with
    | None -> None
    | Some set -> (
        match IntSet.elements set with
        | [] -> None
        | [ i ] -> Some i
        | many -> Some (phi_node many))
  in

  (* Pass 2: seed hashes + predecessor wiring. *)
  pending
  |> List.iter (fun (idx, ni, nk) ->
         let line_of nk =
           match IL_helpers.orig_of_node nk with
           | Some orig -> (
               match
                 AST_generic_helpers.range_of_any_opt (IL.any_of_orig orig)
               with
               | Some (loc, _) -> loc.Tok.pos.Pos.line
               | None -> 0)
           | None -> 0
         in
         (* N7: closure graft *)
         let grafted =
           match nk with
           | IL.NInstr
               {
                 i =
                   IL.AssignAnon
                     ( { IL.base = IL.Var n; rev_offset = [] },
                       IL.Lambda _ );
                 _;
               } -> (
               match IL.NameMap.find_opt n fcfg.IL.lambdas with
               | Some sub ->
                   let rets = build sub (get_in ni) in
                   let d = Dynarray.get nodes idx in
                   let h = leaf 0x03 [] in
                   d.hash <- h;
                   d.prev <- h;
                   d.commutative <- true;
                   d.emit <- false;
                   d.dtag <- Construct;
                   d.dlabel <- "λ";
                   d.dline <- line_of nk;
                   d.preds <-
                     Array.of_list (List.mapi (fun pos i -> (i, pos)) rets);
                   true
               | None -> false)
           | _ -> false
         in
         if not grafted then (
           let l =
             match nk with
             | IL.NInstr i -> local_of_instr ~fc i
             | IL.NCond (_, e) ->
                 local_of_control ~fc 0x04 ~descr:"cond" ~emit_trivial:false e
             | IL.NReturn (_, e) ->
                 local_of_control ~fc 0x05 ~descr:"return" ~emit_trivial:false
                   e
             | IL.NThrow (_, e) ->
                 local_of_control ~fc 0x06 ~descr:"throw" ~emit_trivial:true e
             | _ -> assert false
           in
           let d = Dynarray.get nodes idx in
           d.hash <- l.seed;
           d.prev <- l.seed;
           d.commutative <- l.commutative;
           d.emit <- l.emit;
           d.dtag <- l.ltag;
           d.dlabel <- l.descr;
           d.dline <- line_of nk;
           let preds =
             l.pnames
             |> List.mapi (fun pos pn -> (pos, pn))
             |> List.filter_map (fun (pos, pn) ->
                    match pn with
                    | Edge n -> (
                        match resolve ni (G.SId.to_int n.IL.sid) with
                        | Some i -> Some (i, pos)
                        | None -> Some (sentinel_idx, pos))
                    | OptEdge n -> (
                        match resolve ni (G.SId.to_int n.IL.sid) with
                        | Some i -> Some (i, pos)
                        | None -> None))
           in
           d.preds <- Array.of_list preds));

  (* this unit's return nodes (graft preds for an enclosing AssignAnon) *)
  pending
  |> List.filter_map (fun (idx, _, nk) ->
         match nk with IL.NReturn _ -> Some idx | _ -> None)
  in

  let _ = build fcfg IntMap.empty in
  let arr = Dynarray.to_array nodes in

  (* Iterative propagation, snapshotting each round. *)
  let snapshot () = Array.map (fun d -> d.hash) arr in
  let rounds = ref [ snapshot () ] in
  for _round = 1 to !iterations do
    Array.iter (fun d -> d.prev <- d.hash) arr;
    Array.iter
      (fun d ->
        if Array.length d.preds > 0 then
          let cur = d.prev in
          if d.commutative then (
            let accum = ref 0 in
            Array.iter
              (fun (pi, _pos) ->
                accum := (!accum + Fhash.mix cur arr.(pi).prev) land mask62)
              d.preds;
            d.hash <- Fhash.mix cur !accum)
          else (
            let h = ref cur in
            Array.iter
              (fun (pi, pos) ->
                h := Fhash.mix !h (Fhash.mix_pos arr.(pi).prev pos))
              d.preds;
            d.hash <- !h))
      arr;
    rounds := snapshot () :: !rounds
  done;
  { dnodes = arr; rounds = Array.of_list (List.rev !rounds) }

(* Extract tagged DFG features for one function unit: round 0 emits the local
   seeds (v1's most rename-invariant features); later rounds emit only nodes
   whose hash changed (v1 extract_tagged_features). *)
let extract ?(fc : cfg = !base_cfg) (fcfg : IL.fun_cfg) : feature list =
  let g = graph_of ~fc fcfg in
  let n = Array.length g.dnodes in
  let features = ref [] in
  for i = 0 to n - 1 do
    let d = g.dnodes.(i) in
    if d.emit then features := { hash = g.rounds.(0).(i); tag = d.dtag } :: !features
  done;
  for r = 1 to Array.length g.rounds - 1 do
    for i = 0 to n - 1 do
      let d = g.dnodes.(i) in
      if d.emit && g.rounds.(r).(i) <> g.rounds.(r - 1).(i) then
        features := { hash = g.rounds.(r).(i); tag = d.dtag } :: !features
    done
  done;
  List.rev !features

(* Delta channel: features hashed under a RICHER cfg, emitted ONLY where the
   richer signal actually changed the hash vs the structural base — nodes
   whose ancestry contains names / values / types. Purely-structural nodes are
   already covered by their base feature; re-emitting them would double the
   mismatch mass of structural edits (measured: it costs evolved recall).
   Salted into a disjoint hash domain. *)
let delta_salt = leaf 0x5A [ "delta" ]

let extract_delta ?(base : cfg = !base_cfg) ~(rich : cfg)
    (fcfg : IL.fun_cfg) : feature list =
  let gb = graph_of ~fc:base fcfg in
  let gr = graph_of ~fc:rich fcfg in
  let n = Array.length gr.dnodes in
  let features = ref [] in
  if Array.length gb.dnodes = n then begin
    let emit_if_delta r i =
      let hr = gr.rounds.(r).(i) in
      if hr <> gb.rounds.(r).(i) then
        features :=
          { hash = Fhash.mix delta_salt hr; tag = gr.dnodes.(i).dtag }
          :: !features
    in
    for i = 0 to n - 1 do
      if gr.dnodes.(i).emit then emit_if_delta 0 i
    done;
    for r = 1 to Array.length gr.rounds - 1 do
      for i = 0 to n - 1 do
        if gr.dnodes.(i).emit && gr.rounds.(r).(i) <> gr.rounds.(r - 1).(i)
        then emit_if_delta r i
      done
    done
  end;
  List.rev !features
