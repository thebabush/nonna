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
let iterations_override : int option ref = ref None

(* Per-language depth (kernel rank sweep, 2026-06-11): C wants 0 — depth 1
   is flat on evolved MRR but doubles candidate misses (0.4%->0.8%) and
   costs exact/renamed perfection (1.000->0.999/0.989). Macro-heavy code
   makes neighborhoods noisier, so propagated hashes diverge faster. *)
let iters_for (lang : Lang.t option) : int =
  match !iterations_override with
  | Some n -> n
  | None -> (
      match lang with
      | Some Lang.C -> 0
      (* C++ behaves like Rust/Python, not C: exp-node features at depth 2 win.
         Tuned via LLM-judge correlation on a Chrome (net/) corpus, 2026-06-15
         (n=62 sonnet-judged pairs): spearman(jaccard, judge overall)
         0.859 (depth0/no-exp) -> 0.879 (+exp) -> 0.890 (+exp, depth2). *)
      | Some (Lang.Rust | Lang.Python | Lang.Python2 | Lang.Python3 | Lang.Cpp) ->
          2 (* exp-node features specialize per round *)
      | _ -> 1)

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
  exp_nodes : bool;
      (* sub-expressions become DFG nodes (operands flow in as edges)
         instead of collapsing into the consumer's seed: an edit then
         poisons only its k-hop neighborhood, not the whole tree, and
         depth means the same thing inside and across instructions *)
  thru_copies : bool;
      (* copy-like definitions (bare copy, casts, x = s.field) are
         transparent to dataflow: consumers bind through them to the
         underlying def. Kernel 6.10->6.16 is full of one-line
         type-laundering refactors (u32 *x param -> x = state->x) that
         are invisible at depth 0 but poison every consumer's depth-1
         hash; this pushes through them. Edges only — round-0 features
         are unchanged. *)
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
    exp_nodes = false;
    thru_copies = false;
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
    exp_nodes = false; (* structural choice, not a signal channel *)
    thru_copies = false; (* ditto *)
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

(* Per-language base (N8 sweeps, issue #2): channel optima differ by
   language. Python additionally wants string_values + field_names
   (stringly-typed: literals and attribute names are API identity there;
   ALL MRR 0.955→0.969, evolved 0.862→0.929, samefile FPR halved) — the
   same channels COST recall on Rust. Composed on top of the global
   base_cfg so --with ablations still apply everywhere. *)
let base_cfg_for (lang : Lang.t) : cfg =
  let b = !base_cfg in
  match lang with
  | Lang.Python | Lang.Python2 | Lang.Python3 ->
      { b with string_values = true; field_names = true; exp_nodes = true }
  (* kernel-tuned (v6.10<->v6.16 rank sweep): struct fields and format/log
     strings are API identity in C, same as Python; the other channels are
     flat and leak identifiers into renamed pairs *)
  | Lang.C -> { b with string_values = true; field_names = true }
  (* C++ keeps C's string/field channels AND adds exp_nodes (decomposed
     expression graphs), like Rust/Python. Tuned via LLM-judge correlation on a
     Chrome net/ corpus (2026-06-15, n=62): +exp_nodes lifted spearman 0.859->0.879
     (0.890 with depth-2 iters). call_names measured flat (0.859) — left off. *)
  | Lang.Cpp -> { b with string_values = true; field_names = true; exp_nodes = true }
  (* OCaml lands on the C config, NOT the Rust/Python one: record/module field
     access (Dot offsets) and format/label strings are API identity, while
     exp_nodes — decomposed expression graphs that win for Rust/Python/Cpp —
     HURT here. Tuned via LLM-judge correlation on a 5-lib corpus (base,
     containers, dune, re, yojson; 2026-06-30, n=37 sonnet-judged pairs):
     spearman(jaccard, overall) 0.873 (base) -> 0.884 (+string/field);
     +exp_nodes measured 0.844, iters-2 neutral. Corroborated on 732 mined
     pairs: samefile FPR halved (0.027 -> 0.014), positive recall unchanged.
     (OCaml IL itself required fixing opengrep's AST_to_IL — without it every
     OCaml body collapsed to NTodo; see vendor/opengrep patches.) *)
  | Lang.Ocaml -> { b with string_values = true; field_names = true }
  (* exp-nodes sweeps (2026-06): decomposed expression graphs at depth 2
     win the evolved kind on Rust (MRR 0.723->0.766, r@5 0.812->0.859) AND
     Python (0.929->0.950, r@5 0.964->0.986), everything else flat — an
     edit poisons k hops, not a whole collapsed tree. C measured a tie vs
     collapsed depth-0 (0.917 vs 0.918, better tail) and keeps collapsed. *)
  | Lang.Rust -> { b with exp_nodes = true }
  | _ -> b

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
  lor (if c.exp_nodes then 256 else 0)
  lor (if c.thru_copies then 512 else 0)

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
  lthru : bool; (* transparent to dataflow (thru_copies) *)
}

let mk ?(comm = false) ?(emit = true) ~tag ~descr seed pnames =
  { seed; pnames; commutative = comm; emit; ltag = tag; descr; lthru = false }

let edges ns = List.map (fun n -> Edge n) ns

let truncate (n : int) (s : string) =
  if String.length s <= n then s else String.sub s 0 (n - 1) ^ "…"

let const_label lit =
  let kind, value = Il_util.const_descr lit in
  match value with
  | Some v ->
      if kind = "string" then "\"" ^ truncate 10 v ^ "\"" else truncate 12 v
  | None -> kind

(* Copy-like RHS: the value is some def passed through unchanged-ish —
   a bare read, a field read (Dot offsets only; Index/Mem add real
   inputs), or casts of those. Exactly one underlying source. *)
let rec copy_like (e : IL.exp) : bool =
  match e.IL.e with
  | IL.Fetch { IL.base; rev_offset } ->
      List.for_all
        (fun (o : IL.offset) ->
          match o.IL.o with IL.Dot _ -> true | _ -> false)
        rev_offset
      && (match base with
         | IL.Var _ -> true
         | IL.Mem e1 -> copy_like e1 (* p->f deref: Mem base *)
         | IL.VarSpecial _ -> false)
  | IL.Cast (_, e1) -> copy_like e1
  | _ -> false

(* Classify + locally hash one IL instruction. *)
let local_of_instr ~(fc : cfg) (i : IL.instr) : local =
  let hash_exp = hash_exp ~fc in
  let hash_lval = hash_lval ~fc in
  match i.IL.i with
  | IL.Assign (lval, exp) -> (
      let hexp, rhs_names = hash_exp exp in
      match (lval.IL.base, lval.IL.rev_offset) with
      | IL.Var _, [] ->
          let seed = Fhash.mix (leaf 0x01 []) hexp in
          let preds = edges rhs_names in
          let l =
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
                  seed preds
          in
          { l with lthru = fc.thru_copies && copy_like exp }
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
  mutable dthrough : bool; (* transparent to dataflow (thru_copies) *)
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
    dthrough = false;
  }

let mask62 = 0x3FFFFFFFFFFFFFFF

(* The assembled DFG plus one hash snapshot per propagation round.
   rounds.(0) = local seeds; rounds.(r) = state after round r. *)
type graph = { dnodes : dnode array; rounds : int array array }

module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)

let graph_of ?(fc : cfg = !base_cfg) ?(iters = 1) (fcfg : IL.fun_cfg) :
    graph =
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

  (* ── exp-nodes mode (fc.exp_nodes): sub-expressions are DFG nodes ────
     Each returns the index of the node representing the value. Bare
     variable reads create NO node — they resolve straight to the reaching
     definition (or the extern sentinel), so a use is one edge, not a hop.
     Seeds carry only the node's own kind; operand context arrives via
     propagation rounds, exactly like def-use context. *)
  let mk_enode ~line ?(comm = false) ?(emit = true) ~tag ~descr seed preds =
    let d = fresh_dnode () in
    d.hash <- seed;
    d.prev <- seed;
    d.commutative <- comm;
    d.emit <- emit;
    d.dtag <- tag;
    d.dlabel <- descr;
    d.dline <- line;
    d.preds <- Array.of_list (List.mapi (fun pos i -> (i, pos)) preds);
    push d
  in
  let rec enode_of_exp ~ni ~line (e : IL.exp) : int =
    let enode_of_exp = enode_of_exp ~ni ~line in
    let mk_enode ?comm ?emit = mk_enode ~line ?comm ?emit in
    match e.IL.e with
    | IL.Fetch { IL.base = IL.Var n; rev_offset = [] } -> (
        match resolve ni (G.SId.to_int n.IL.sid) with
        | Some i -> i
        | None -> sentinel_idx)
    | IL.Fetch lval -> enode_of_lval ~ni ~line lval
    | IL.Literal lit ->
        let kind, value = Il_util.const_descr lit in
        let parts =
          if keep_value fc kind then kind :: Option.to_list value
          else [ kind ]
        in
        let tag =
          if kind = "string" then ConstString else ConstOther
        in
        mk_enode ~tag ~descr:(const_label lit) (leaf 0x13 parts) []
    | IL.Cast (ty, e1) ->
        let parts = if fc.ty_descrs then [ Il_util.ty_descr ty ] else [] in
        mk_enode ~emit:false ~tag:BinOp
          ~descr:("as " ^ Il_util.ty_descr ty)
          (leaf 0x1A parts)
          [ enode_of_exp e1 ]
    | IL.Operator ((op, _), args) ->
        let preds =
          List.map
            (fun a -> enode_of_exp (IL_helpers.exp_of_arg a))
            args
        in
        let t = if List.length args >= 2 then BinOp else UnOp in
        mk_enode
          ~comm:(op_commutative op)
          ~tag:t ~descr:(G.show_operator op)
          (leaf 0x10 [ G.show_operator op; string_of_int (List.length args) ])
          preds
    | IL.Composite (ck, (_, xs, _)) ->
        let ck_descr =
          match ck with
          | IL.Constructor _ when not fc.call_names -> "ctor"
          | _ -> composite_descr ck
        in
        mk_enode ~emit:(xs <> []) ~tag:Construct ~descr:ck_descr
          (leaf 0x17 [ ck_descr; string_of_int (List.length xs) ])
          (List.map enode_of_exp xs)
    | IL.RecordOrDict fes ->
        (* one node per field: the parked per-field-record-features lever —
           editing one field no longer kills the whole construction *)
        let preds =
          List.map
            (function
              | IL.Field (n, e1) ->
                  let fparts =
                    if fc.field_names then [ fst n.IL.ident ] else []
                  in
                  mk_enode ~tag:Field
                    ~descr:("." ^ fst n.IL.ident ^ "=")
                    (leaf 0x18 fparts)
                    [ enode_of_exp e1 ]
              | IL.Entry (k, v) ->
                  mk_enode ~tag:Construct ~descr:"entry" (leaf 0x18 [])
                    [ enode_of_exp k; enode_of_exp v ]
              | IL.Spread e1 ->
                  mk_enode ~emit:false ~tag:Construct ~descr:"spread"
                    (leaf 0x19 []) [ enode_of_exp e1 ])
            fes
        in
        mk_enode ~tag:Construct ~descr:"record"
          (leaf 0x1C [ string_of_int (List.length fes) ])
          preds
    | IL.FixmeExp (_, any, eopt) ->
        let toks =
          if fc.macro_tokens then Il_util.token_strings_of_any any else []
        in
        mk_enode ~tag:MacroBag ~descr:"macro" (leaf 0x1F toks)
          (match eopt with Some e1 -> [ enode_of_exp e1 ] | None -> [])

  and enode_of_lval ~ni ~line (lval : IL.lval) : int =
    (* one node per lval read; chain SHAPE is the seed, base + index
       values are edges *)
    let base_seed, base_preds =
      match lval.IL.base with
      | IL.Var n -> (
          ( var_leaf,
            match resolve ni (G.SId.to_int n.IL.sid) with
            | Some i -> [ i ]
            | None -> [ sentinel_idx ] ))
      | IL.VarSpecial (sp, _) ->
          (leaf 0x21 [ IL.show_var_special sp ], [])
      | IL.Mem e -> (leaf 0x22 [], [ enode_of_exp ~ni ~line e ])
    in
    let offs = List.rev lval.IL.rev_offset in
    let seed, preds =
      List.fold_left
        (fun (h, ps) (o : IL.offset) ->
          match o.IL.o with
          | IL.Dot n ->
              let fparts =
                if fc.field_names then [ fst n.IL.ident ] else []
              in
              (Fhash.mix h (leaf 0x15 fparts), ps)
          | IL.Index e ->
              ( Fhash.mix h (leaf 0x16 []),
                ps @ [ enode_of_exp ~ni ~line e ] )
          | IL.Slice i ->
              (Fhash.mix h (leaf 0x23 [ string_of_int i ]), ps))
        (base_seed, base_preds) offs
    in
    let tag, descr =
      match offs with
      | { IL.o = IL.Dot f; _ } :: _ -> (Field, "." ^ fst f.IL.ident)
      | _ -> (Index, "[..]")
    in
    mk_enode ~line ~tag ~descr seed preds
  in

  (* exp-nodes mode: wire one INSTRUCTION/CONTROL node (the reserved idx,
     which reaching defs point at) on top of the exp nodes. *)
  let exp_wire (idx : int) (ni : int) (line : int) (nk : IL.node_kind) :
      unit =
    let set ?(comm = false) ?(emit = true) ~tag ~descr seed preds =
      let d = Dynarray.get nodes idx in
      d.hash <- seed;
      d.prev <- seed;
      d.commutative <- comm;
      d.emit <- emit;
      d.dtag <- tag;
      d.dlabel <- descr;
      d.dline <- line;
      d.preds <- Array.of_list (List.mapi (fun pos i -> (i, pos)) preds)
    in
    let enode_of_exp = enode_of_exp ~ni ~line in
    let rec underlying_def (e : IL.exp) : int =
      match e.IL.e with
      | IL.Cast (_, e1) -> underlying_def e1
      | IL.Fetch { IL.base = IL.Var n; _ } -> (
          match resolve ni (G.SId.to_int n.IL.sid) with
          | Some i2 -> i2
          | None -> sentinel_idx)
      | IL.Fetch { IL.base = IL.Mem e1; _ } -> underlying_def e1
      | _ -> sentinel_idx
    in
    match nk with
    | IL.NInstr i -> (
        match i.IL.i with
        | IL.Assign (lval, exp) -> (
            match (lval.IL.base, lval.IL.rev_offset) with
            | IL.Var _, [] ->
                (* def anchor: pass-through to the RHS value node (covers
                   bare copies too — Fetch-bare returns the def itself).
                   thru_copies: copy-like RHS binds the anchor to the
                   UNDERLYING def; the rhs node is still built so its
                   round-0 feature emits, it's just off the use chain. *)
                let thru = fc.thru_copies && copy_like exp in
                let rhs = enode_of_exp exp in
                let preds = if thru then [ underlying_def exp ] else [ rhs ] in
                (Dynarray.get nodes idx).dthrough <- thru;
                set ~emit:false ~tag:BinOp ~descr:"=" (leaf 0x01 []) preds
            | _ ->
                let chain =
                  List.fold_left
                    (fun h (o : IL.offset) ->
                      match o.IL.o with
                      | IL.Dot n ->
                          let fparts =
                            if fc.field_names then [ fst n.IL.ident ]
                            else []
                          in
                          Fhash.mix h (leaf 0x15 fparts)
                      | IL.Index _ -> Fhash.mix h (leaf 0x16 [])
                      | IL.Slice s ->
                          Fhash.mix h (leaf 0x23 [ string_of_int s ]))
                    (leaf 0x02 [])
                    (List.rev lval.IL.rev_offset)
                in
                let base_preds =
                  match lval.IL.base with
                  | IL.Var n -> (
                      match resolve ni (G.SId.to_int n.IL.sid) with
                      | Some i2 -> [ i2 ]
                      | None -> [ sentinel_idx ])
                  | IL.VarSpecial _ -> []
                  | IL.Mem e -> [ enode_of_exp e ]
                in
                let idx_preds =
                  lval.IL.rev_offset |> List.rev
                  |> List.filter_map (fun (o : IL.offset) ->
                         match o.IL.o with
                         | IL.Index e -> Some (enode_of_exp e)
                         | _ -> None)
                in
                let t, dsc =
                  match lval.IL.rev_offset with
                  | { o = IL.Dot f; _ } :: _ ->
                      (Field, "store ." ^ fst f.IL.ident)
                  | _ -> (Index, "store [..]")
                in
                set ~tag:t ~descr:dsc chain
                  (base_preds @ idx_preds @ [ enode_of_exp exp ]))
        | IL.AssignAnon (_, _) ->
            set ~emit:false ~tag:Construct ~descr:"lambda" (leaf 0x03 []) []
        | IL.Call (_, fexp, args) ->
            let ckind, cname, callee_preds =
              match fexp.IL.e with
              | IL.Fetch { IL.base = IL.Var fn; rev_offset = [] } -> (
                  ( "named",
                    fst fn.IL.ident,
                    (* OptEdge: a global callee name is not dataflow *)
                    match resolve ni (G.SId.to_int fn.IL.sid) with
                    | Some i2 -> [ i2 ]
                    | None -> [] ))
              | IL.Fetch ({ IL.rev_offset = { o = IL.Dot m; _ } :: _; _ } as
                          lv) ->
                  ("method", fst m.IL.ident, [ enode_of_lval ~ni ~line lv ])
              | _ -> ("dynamic", "", [ enode_of_exp fexp ])
            in
            let target_parts =
              ckind
              :: (if fc.call_names && cname <> "" then [ cname ] else [])
            in
            let arg_preds =
              List.map
                (fun a -> enode_of_exp (IL_helpers.exp_of_arg a))
                args
            in
            let short = if cname = "" then ckind else cname in
            set ~tag:Call
              ~descr:
                (Printf.sprintf "%s(%d)" (truncate 14 short)
                   (List.length args))
              (leaf 0x12 (target_parts @ [ string_of_int (List.length args) ]))
              (callee_preds @ arg_preds)
        | IL.CallSpecial (_, (sp, _), args) ->
            set ~tag:Call
              ~descr:(truncate 16 (IL.show_call_special sp))
              (leaf 0x12
                 [
                   "special:" ^ IL.show_call_special sp;
                   string_of_int (List.length args);
                 ])
              (List.map
                 (fun a -> enode_of_exp (IL_helpers.exp_of_arg a))
                 args)
        | IL.New (_, ty, ctor, args) ->
            let ty_part = if fc.ty_descrs then [ Il_util.ty_descr ty ] else [] in
            set ~tag:Construct
              ~descr:("new " ^ truncate 12 (Il_util.ty_descr ty))
              (leaf 0x1B (ty_part @ [ string_of_int (List.length args) ]))
              ((match ctor with Some e -> [ enode_of_exp e ] | None -> [])
              @ List.map
                  (fun a -> enode_of_exp (IL_helpers.exp_of_arg a))
                  args)
        | IL.FixmeInstr (_, any) ->
            let toks =
              if fc.macro_tokens then Il_util.token_strings_of_any any
              else []
            in
            set ~tag:MacroBag ~descr:"macro!" (leaf 0x1F toks) [])
    | IL.NCond (_, e) ->
        set
          ~emit:(not (Il_util.exp_is_trivial e))
          ~tag:Control ~descr:"cond" (leaf 0x04 []) [ enode_of_exp e ]
    | IL.NReturn (_, e) ->
        set
          ~emit:(not (Il_util.exp_is_trivial e))
          ~tag:Control ~descr:"return" (leaf 0x05 []) [ enode_of_exp e ]
    | IL.NThrow (_, e) ->
        set ~tag:Control ~descr:"throw" (leaf 0x06 []) [ enode_of_exp e ]
    | _ -> assert false
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
         if not grafted then
           if fc.exp_nodes then exp_wire idx ni (line_of nk) nk
           else (
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
           d.dthrough <- l.lthru;
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

  (* thru_copies: every edge into a transparent single-pred node is
     rewritten to its terminal def (transitively; cycle-guarded for
     x = x.next loops). Transparent nodes keep their own preds and
     round-0 emission — they just stop being a propagation hop. *)
  if fc.thru_copies then begin
    let rec chase i seen =
      let d = arr.(i) in
      if (not d.dthrough) || Array.length d.preds <> 1 || List.mem i seen
      then i
      else chase (fst d.preds.(0)) (i :: seen)
    in
    Array.iter
      (fun d ->
        d.preds <- Array.map (fun (pi, pos) -> (chase pi [], pos)) d.preds)
      arr
  end;

  (* Iterative propagation, snapshotting each round. *)
  let snapshot () = Array.map (fun d -> d.hash) arr in
  let rounds = ref [ snapshot () ] in
  for _round = 1 to iters do
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
let extract ?(fc : cfg = !base_cfg) ?(iters = 1) (fcfg : IL.fun_cfg) :
    feature list =
  let g = graph_of ~fc ~iters fcfg in
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

let extract_delta ?(base : cfg = !base_cfg) ?(iters = 1) ~(rich : cfg)
    (fcfg : IL.fun_cfg) : feature list =
  (* exp_nodes changes the node STRUCTURE, not the signal: rich must agree
     with base or the per-index hash comparison below is meaningless *)
  let rich = { rich with exp_nodes = base.exp_nodes } in
  let gb = graph_of ~fc:base ~iters fcfg in
  let gr = graph_of ~fc:rich ~iters fcfg in
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
