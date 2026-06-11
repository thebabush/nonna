(* Shared helpers for walking the Opengrep IL. *)

module G = AST_generic

(* Reachable CFG nodes in deterministic (nodei-ascending) order.
   Does NOT descend into lambdas: per D6, lambdas are indexed as their own
   flat units (Visit_function_defs already visits them separately). *)
let nodes_in_order (cfg : IL.cfg) : (int * IL.node) list =
  CFG.NodeiSet.elements cfg.CFG.reachable
  |> List.map (fun ni -> (ni, cfg.CFG.graph#nodes#assoc ni))

let succs (cfg : IL.cfg) (ni : int) : int list =
  CFG.successors cfg ni |> List.map fst

(* Sub-expressions reachable from an lval (Mem bases, Index offsets). *)
let exps_of_lval (lval : IL.lval) : IL.exp list =
  let base = match lval.IL.base with IL.Mem e -> [ e ] | _ -> [] in
  let offs =
    List.concat_map
      (fun (o : IL.offset) ->
        match o.IL.o with IL.Index e -> [ e ] | IL.Dot _ | IL.Slice _ -> [])
      lval.IL.rev_offset
  in
  base @ offs

(* The expressions appearing in a CFG node, in deterministic order. *)
let exps_of_node (n : IL.node_kind) : IL.exp list =
  match n with
  | IL.NInstr i -> (
      match i.IL.i with
      | IL.Assign (lval, e) -> exps_of_lval lval @ [ e ]
      | IL.AssignAnon (lval, _) -> exps_of_lval lval
      | IL.Call (lv, f, args) ->
          (match lv with Some l -> exps_of_lval l | None -> [])
          @ (f :: List.map IL_helpers.exp_of_arg args)
      | IL.CallSpecial (lv, _, args) ->
          (match lv with Some l -> exps_of_lval l | None -> [])
          @ List.map IL_helpers.exp_of_arg args
      | IL.New (lval, _, ctor, args) ->
          exps_of_lval lval
          @ (match ctor with Some e -> [ e ] | None -> [])
          @ List.map IL_helpers.exp_of_arg args
      | IL.FixmeInstr _ -> [])
  | IL.NCond (_, e)
  | IL.NReturn (_, e)
  | IL.NThrow (_, e)
  | IL.TrueNode e
  | IL.FalseNode e ->
      [ e ]
  | IL.Enter | IL.Exit | IL.Join | IL.NGoto _ | IL.NOther _ | IL.NTodo _ -> []

(* Pre-order iteration over an exp tree, descending into nested lvals. *)
let rec iter_exp (f : IL.exp -> unit) (e : IL.exp) : unit =
  f e;
  match e.IL.e with
  | IL.Fetch lval -> List.iter (iter_exp f) (exps_of_lval lval)
  | IL.Literal _ -> ()
  | IL.Cast (_, e1) -> iter_exp f e1
  | IL.Composite (_, (_, xs, _)) -> List.iter (iter_exp f) xs
  | IL.Operator (_, args) ->
      List.iter (fun a -> iter_exp f (IL_helpers.exp_of_arg a)) args
  | IL.RecordOrDict fes ->
      List.iter
        (function
          | IL.Field (_, e1) | IL.Spread e1 -> iter_exp f e1
          | IL.Entry (k, v) ->
              iter_exp f k;
              iter_exp f v)
        fes
  | IL.FixmeExp (_, _, Some e1) -> iter_exp f e1
  | IL.FixmeExp (_, _, None) -> ()

(* Last segment of a generic name, e.g. `std::cmp::max` -> "max". *)
let name_last_str (n : G.name) : string =
  match n with
  | G.Id ((s, _), _) -> s
  | G.IdQualified { G.name_last = (s, _), _; _ } -> s

(* Compact, position-free descriptor of a declared type. *)
let rec ty_descr (t : G.type_) : string =
  match t.G.t with
  | G.TyN n -> name_last_str n
  | G.TyApply (t1, _) -> ty_descr t1 ^ "<>"
  | G.TyArray (_, t1) -> "[" ^ ty_descr t1 ^ "]"
  | G.TyTuple _ -> "tuple"
  | G.TyFun _ -> "fn"
  | G.TyPointer (_, t1) -> "*" ^ ty_descr t1
  | G.TyRef (_, t1) -> "&" ^ ty_descr t1
  | G.TyVar (s, _) -> "'" ^ s
  | G.TyAny _ -> "_"
  | _ -> "ty"

(* (kind, value) descriptor of a literal, position-free. *)
let const_descr (lit : G.literal) : string * string option =
  match lit with
  | G.Bool (b, _) -> ("bool", Some (string_of_bool b))
  | G.Int pi -> ("int", Parsed_int.to_string_opt pi)
  | G.Float (fopt, tok) ->
      ( "float",
        match fopt with
        | Some f -> Some (string_of_float f)
        | None -> ( try Some (Tok.content_of_tok tok) with _ -> None) )
  | G.Char (s, _) -> ("char", Some s)
  | G.String (_, (s, _), _) -> ("string", Some s)
  | G.Regexp ((_, (s, _), _), _) -> ("regexp", Some s)
  | G.Atom (_, (s, _)) -> ("atom", Some s)
  | G.Unit _ -> ("unit", None)
  | G.Null _ -> ("null", None)
  | G.Undefined _ -> ("undefined", None)
  | G.Imag (s, _) -> ("imag", Some s)
  | G.Ratio (s, _) -> ("ratio", Some s)

(* Token contents of an arbitrary generic-AST fragment (used to hash macro
   bags from FixmeExp: name + inner tokens survive macro lowering). *)
let token_strings_of_any (any : G.any) : string list =
  AST_generic_helpers.ii_of_any any
  |> List.filter_map (fun tok ->
         match Tok.content_of_tok tok with
         | s when s <> "" -> Some s
         | _ -> None
         | exception _ -> None)

(* Identifiers bound by a parameter pattern: (resolved sid, declared type).
   Rust (and other pattern-param languages) lower fn params to ParamPattern
   whose IL pname is a synthetic `!!_implicit_param!_N` with its own sid —
   the body references the PATTERN-bound names, so those sids must also map
   to the parameter's DFG node. *)
let rec pat_bound_sids (p : G.pattern) : (int * G.type_ option) list =
  let sid_of (info : G.id_info) : int option =
    match !(info.G.id_resolved) with
    | Some (_, sid) -> Some (G.SId.to_int sid)
    | None -> None
  in
  match p with
  | G.PatId (_, info) -> (
      match sid_of info with
      | Some sid -> [ (sid, !(info.G.id_type)) ]
      | None -> [])
  | G.PatTyped (p1, ty) ->
      pat_bound_sids p1
      |> List.map (fun (sid, t) ->
             (sid, match t with Some _ -> t | None -> Some ty))
  | G.PatAs (p1, (_, info)) -> (
      pat_bound_sids p1
      @ match sid_of info with Some sid -> [ (sid, None) ] | None -> [])
  | G.PatTuple (_, ps, _) | G.PatList (_, ps, _) | G.PatConstructor (_, ps) ->
      List.concat_map pat_bound_sids ps
  | G.PatRecord (_, fields, _) ->
      List.concat_map (fun (_, p1) -> pat_bound_sids p1) fields
  | G.PatKeyVal (p1, p2) | G.PatDisj (p1, p2) ->
      pat_bound_sids p1 @ pat_bound_sids p2
  | G.PatWhen (p1, _) -> pat_bound_sids p1
  | _ -> []

(* Is an exp trivial (unit literal or bare var read)? *)
let exp_is_trivial (e : IL.exp) : bool =
  match e.IL.e with
  | IL.Literal (G.Unit _) -> true
  | IL.Fetch { IL.base = IL.Var _; rev_offset = [] } -> true
  | _ -> false
