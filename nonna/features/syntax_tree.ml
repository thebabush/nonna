(* Syntax-subtree hashes (D16): one hash per statement subtree of a function
   body, at every depth, from the generic AST after naming. They back the
   [Tree] signature family and the exact block-clone listing (`dupes
   --blocks`), the scb-check style of clone detection with one difference:
   call targets keep their names (D14: they are API identity).

   A statement's hash mixes its kind with its normalized token stream, in
   which every direct child statement stands as one token: the child's own
   hash. So hashes compose bottom-up and equal subtrees hash equally wherever
   they sit. Normalization, per token:
   - local variables and parameters (resolved by Naming_AST, binding sites
     included) are one constant, "$v". Not first-occurrence numbering: that
     numbers variables relative to the subtree being hashed, so a child's
     hash could not be reused inside its parent's; with call, field and
     literal-kind tokens kept, a constant loses little;
   - call targets keep their name; a qualified callee keeps its last
     segment only (`Self::f(x)` = `f(x)`); a locally bound callee (a
     closure) is a variable;
   - field and method names keep their text, as do keywords, operators,
     punctuation, types and non-local names;
   - a literal is its kind (int, string, ...): one token whatever its value
     or delimiters;
   - comments carry no AST tokens, so they are invisible; formatting too.

   That is name mode 0, the only one [nodes] (and so `dupes --blocks`)
   uses. The [Tree] family's [features] take a [config] (the `tree.*`
   --weights knobs, read in Signature): name mode 1 rewrites names to their
   entity kind, mode 2 hashes AST constructor kinds only, and flake-style
   extras (statement n-grams, parent-child edges, shapes at small radii)
   add partial-credit features on top of the subtree hashes. *)

module G = AST_generic

type node = {
  stmt : G.stmt;
  hash : Fhash.t;
  stmts : int; (* statement nodes in the subtree, itself included *)
  body_stmts : int;
      (* compound statements: statements in their largest body (scb's
         count); 0 for every other kind *)
  first : Tok.location; (* first and last code token *)
  last : Tok.location;
}

(* One small int per statement kind; tokens tell the rest apart. *)
let kind_of (st : G.stmt) : int =
  match st.G.s with
  | G.ExprStmt _ -> 1
  | G.Block _ -> 2
  | G.If _ -> 3
  | G.While _ -> 4
  | G.Return _ -> 5
  | G.DoWhile _ -> 6
  | G.For _ -> 7
  | G.Switch _ -> 8
  | G.Continue _ -> 9
  | G.Break _ -> 10
  | G.Label _ -> 11
  | G.Goto _ -> 12
  | G.Throw _ -> 13
  | G.Try _ -> 14
  | G.WithUsingResource _ -> 15
  | G.Assert _ -> 16
  | G.DefStmt _ -> 17
  | G.DirectiveStmt _ -> 18
  | G.DisjStmt _ -> 19
  | G.OtherStmtWithStmt _ -> 20
  | G.OtherStmt _ -> 21
  | G.RawStmt _ -> 22

let literal_kind (l : G.literal) : int =
  match l with
  | G.Bool _ -> 1
  | G.Int _ -> 2
  | G.Float _ -> 3
  | G.Char _ -> 4
  | G.String _ -> 5
  | G.Regexp _ -> 6
  | G.Atom _ -> 7
  | G.Unit _ -> 8
  | G.Null _ -> 9
  | G.Undefined _ -> 10
  | G.Imag _ -> 11
  | G.Ratio _ -> 12

(* Statements in one body of a compound statement. *)
let body_count (s : G.stmt) : int =
  match s.G.s with G.Block (_, l, _) -> List.length l | _ -> 1

(* scb counts the largest body; an `else if` is not a body of its own. *)
let body_stmts_of (st : G.stmt) : int =
  let else_count = function
    | Some ({ G.s = G.Block _; _ } as e) -> body_count e
    | _ -> 0
  in
  match st.G.s with
  | G.If (_, _, th, el) -> max (body_count th) (else_count el)
  | G.While (_, _, b) | G.DoWhile (_, b, _) | G.For (_, _, b) -> body_count b
  | G.WithUsingResource (_, _, b) -> body_count b
  | G.Switch (_, _, cases) -> List.length cases (* match arms *)
  | G.Try (_, b, catches, else_, fin) ->
      List.fold_left max (body_count b)
        (List.map (fun (_, _, h) -> body_count h) catches
        @ (match else_ with Some (_, s) -> [ body_count s ] | None -> [])
        @ match fin with Some (_, s) -> [ body_count s ] | None -> [])
  | G.Block (_, l, _) -> List.length l
  | _ -> 0

(* Ent: a name rewritten to its entity kind (name mode 1) *)
type tclass = Var | Lit of int | Skip | Ent of string

let is_local (info : G.id_info) : bool =
  match !(info.G.id_resolved) with
  | Some ((G.LocalVar | G.Parameter | G.EnclosedVar), _) -> true
  | _ -> false

let origin_toks (any : G.any) : Tok.t list =
  AST_generic_helpers.ii_of_any any |> List.filter Tok.is_origintok

(* Class of every token the normalization rewrites, keyed by byte offset.
   Macro token bags the reparse left opaque (Units.reparse_rust_macro_args:
   `{}` macros, qualified ones like `tracing::info!`, unparseable trees) have
   no resolved names; in them, a token spelled like a variable or parameter
   bound in this body counts as that variable, and literal-shaped tokens
   (quoted, numeric, true/false) as their kind.

   Name mode 1 (entity kinds) also rewrites every other name: a call
   target to "call/<arity>", a field or method name to "field", a whole
   type to one "type" token, any other non-local name (consts, enum
   variants, paths, nested-definition names) to one "ident" token, and in
   macro bags every identifier-shaped token left to "ident". *)
let classify ?(names = 0) (body : G.stmt) : (int, tclass) Hashtbl.t =
  let cls = Hashtbl.create 64 in
  let set tok c =
    if Tok.is_origintok tok then Hashtbl.replace cls (Tok.bytepos_of_tok tok) c
  in
  let locals = Hashtbl.create 16 and bags = ref [] in
  let var ((s, tok) : G.ident) =
    Hashtbl.replace locals s ();
    set tok Var
  in
  (* the first token of [any] stands for all of it *)
  let collapse any c =
    match
      List.sort_uniq
        (fun a b -> compare (Tok.bytepos_of_tok a) (Tok.bytepos_of_tok b))
        (origin_toks any)
    with
    | t :: rest ->
        set t c;
        List.iter (fun t -> set t Skip) rest
    | [] -> ()
  in
  let lit any l = collapse any (Lit (literal_kind l)) in
  let ent = names = 1 in
  let v =
    object (self)
      inherit [_] G.iter_no_id_info as super

      method! visit_expr env e =
        match e.G.e with
        | G.N (G.Id (id, info)) when is_local info -> var id
        | G.N n when ent -> collapse (G.Name n) (Ent "ident")
        | G.L l -> lit (G.E e) l
        | G.Call ({ G.e = G.N n; _ }, ((_, l, _) as args)) ->
            let call = Ent (Printf.sprintf "call/%d" (List.length l)) in
            (match n with
            | G.Id (id, info) ->
                if is_local info then var id else if ent then set (snd id) call
            | G.IdQualified { G.name_last = (_, last), _; _ } ->
                List.iter (fun t -> set t Skip) (origin_toks (G.Name n));
                if ent then set last call
                else if Tok.is_origintok last then
                  Hashtbl.remove cls (Tok.bytepos_of_tok last));
            super#visit_arguments env args
        | G.DotAccess (obj, _, G.FN n) when ent ->
            (* a turbofish method name is qualified with its type args *)
            collapse (G.Name n) (Ent "field");
            self#visit_expr env obj
        | G.Constructor (n, (_, l, _)) when ent ->
            collapse (G.Name n) (Ent "ident");
            List.iter (self#visit_expr env) l
        | _ -> super#visit_expr env e

      method! visit_type_ env t =
        if ent then collapse (G.T t) (Ent "type") else super#visit_type_ env t

      method! visit_directive env d =
        if ent then collapse (G.Dir d) (Ent "directive")
        else super#visit_directive env d

      method! visit_pattern env p =
        match p with
        | G.PatId (id, _) -> var id
        | G.PatLiteral l -> lit (G.P p) l
        | G.PatConstructor (n, ps) when ent ->
            collapse (G.Name n) (Ent "ident");
            List.iter (self#visit_pattern env) ps
        | _ -> super#visit_pattern env p

      method! visit_parameter_classic env p =
        Option.iter var p.G.pname;
        super#visit_parameter_classic env p

      method! visit_argument env a =
        match a with
        | G.OtherArg (("ArgMacro", _), anys) ->
            bags := List.concat_map origin_toks anys :: !bags
        | _ -> super#visit_argument env a

      method! visit_definition env ((entity, kind) as def) =
        (match (entity.G.name, kind) with
        | G.EN (G.Id (id, _)), G.VarDef _ -> var id
        | G.EN (G.Id ((_, tok), _)), G.FieldDefColon _ when ent ->
            set tok (Ent "field")
        | G.EN (G.Id ((_, tok), _)), _ when ent -> set tok (Ent "ident")
        | _ -> ());
        super#visit_definition env def
    end
  in
  v#visit_stmt () body;
  let bag_class (c : string) : tclass option =
    if Hashtbl.mem locals c then Some Var
    else if c = "true" || c = "false" then Some (Lit 1)
    else
      match c.[0] with
      | '0' .. '9' -> Some (Lit (if String.contains c '.' then 3 else 2))
      | 'a' .. 'z' | 'A' .. 'Z' | '_' when ent -> Some (Ent "ident")
      | _ -> None
  in
  (* a bag string arrives as quote, content, quote tokens (or as one) *)
  let classify_bag toks =
    let in_string = ref false in
    List.sort_uniq
      (fun a b -> compare (Tok.bytepos_of_tok a) (Tok.bytepos_of_tok b))
      toks
    |> List.iter (fun t ->
           let c = Tok.content_of_tok t in
           let quoted = String.contains c '"' in
           if Hashtbl.mem cls (Tok.bytepos_of_tok t) || c = "" then ()
           else if !in_string then begin
             set t Skip;
             if quoted then in_string := false
           end
           else if quoted && (c.[0] = '"' || c.[0] = 'b' || c.[0] = 'r') then begin
             set t (Lit 5);
             (* one quote only: content and the closing quote follow *)
             in_string := List.length (String.split_on_char '"' c) < 3
           end
           else Option.iter (set t) (bag_class c))
  in
  List.iter classify_bag !bags;
  cls

(* Every statement subtree of [body] (itself included), children before
   parents. Statements without code tokens are left out. *)
let nodes ?(names = 0) (body : G.stmt) : node list =
  let cls = classify ~names body in
  let toks =
    origin_toks (G.S body)
    |> List.sort_uniq (fun a b ->
           compare (Tok.bytepos_of_tok a) (Tok.bytepos_of_tok b))
    |> Array.of_list
  in
  let index = Hashtbl.create (Array.length toks) in
  Array.iteri (fun i t -> Hashtbl.replace index (Tok.bytepos_of_tok t) i) toks;
  let var_h = Fhash.hash_str "$v" and lit_h = Fhash.hash_str "$lit" in
  (* normalized token hashes; None = dropped *)
  let th =
    Array.map
      (fun t ->
        match Hashtbl.find_opt cls (Tok.bytepos_of_tok t) with
        | Some Var -> Some var_h
        | Some (Lit k) -> Some (Fhash.mix lit_h k)
        | Some Skip -> None
        | Some (Ent k) -> Some (Fhash.hash_str ("$" ^ k))
        | None -> Some (Fhash.hash_str (Tok.content_of_tok t)))
      toks
  in
  let out = ref [] in
  (* token index range [lo, hi] of a statement, from its own tokens *)
  let range st =
    List.fold_left
      (fun acc t ->
        match Hashtbl.find_opt index (Tok.bytepos_of_tok t) with
        | None -> acc
        | Some i -> (
            match acc with
            | None -> Some (i, i)
            | Some (lo, hi) -> Some (min lo i, max hi i)))
      None
      (origin_toks (G.S st))
  in
  let children st =
    let acc = ref [] in
    let v =
      object
        inherit [_] G.iter_no_id_info
        method! visit_stmt _ s = acc := s :: !acc
      end
    in
    v#visit_stmt_kind () st.G.s;
    List.rev !acc
  in
  (* (node, lo, hi) of a statement, recording every subtree on the way *)
  let rec walk (st : G.stmt) : (node * int * int) option =
    match range st with
    | None -> None
    | Some (lo, hi) ->
        let kids =
          List.filter_map walk (children st)
          |> List.sort (fun (_, a, _) (_, b, _) -> compare a b)
        in
        let f = Fhash.Feed.create () in
        Fhash.Feed.tag f 0xD0;
        Fhash.Feed.int f (kind_of st);
        let rec feed i kids =
          if i <= hi then
            match kids with
            | (k, klo, khi) :: rest when klo <= i ->
                if klo = i then begin
                  Fhash.Feed.tag f 0x01;
                  Fhash.Feed.int f k.hash
                end;
                feed (max i (khi + 1)) rest
            | _ ->
                Option.iter
                  (fun h ->
                    Fhash.Feed.tag f 0x02;
                    Fhash.Feed.int f h)
                  th.(i);
                feed (i + 1) kids
        in
        feed lo kids;
        (* origin tokens always carry a location *)
        let loc i = Tok.unsafe_loc_of_tok toks.(i) in
        let n =
          {
            stmt = st;
            hash = Fhash.Feed.finish f;
            stmts = List.fold_left (fun n (k, _, _) -> n + k.stmts) 1 kids;
            body_stmts = body_stmts_of st;
            first = loc lo;
            last = loc hi;
          }
        in
        out := n :: !out;
        Some (n, lo, hi)
  in
  ignore (walk body);
  List.rev !out

(* Tree-family weight of a subtree: 1 + ln(statements in it). A `return x;`
   weighs 1, a matched 15-statement block 3.7. *)
let weight (n : node) : float = 1. +. log (float_of_int n.stmts)

(* The [Tree] family's knobs (`tree.*` via --weights, read in Signature).
   [default_config] gives today's features exactly: mode-0 subtree hashes,
   no extras. *)
type config = {
  names : int;
      (* 0: names verbatim; 1: entity kinds (classify); 2: kinds only, the
         subtree hashes taken from the kind tree below *)
  subtrees : bool; (* the full-subtree hashes *)
  ngrams : int; (* statement n-grams, n = 1..ngrams; 0 = off *)
  edges : bool; (* parent -> child label pairs *)
  radius : int; (* shapes at radius 1..radius; 0 = off *)
  log_counts : bool; (* extras: one more feature per 2nd, 4th, 8th... copy *)
  w_ngram : float; (* an n-gram weighs w_ngram * n *)
  w_edge : float;
  w_radius : float; (* a radius-r shape weighs w_radius * r *)
}

let default_config =
  {
    names = 0;
    subtrees = true;
    ngrams = 0;
    edges = false;
    radius = 0;
    log_counts = true;
    w_ngram = 0.25;
    w_edge = 0.25;
    w_radius = 0.25;
  }

(* The kind tree, for name mode 2 and the extras: one node per statement,
   expression and pattern, labelled by its AST constructor. The label takes
   names per the mode: a call folds its callee into its label ("call:f",
   "call/2", or plain "Call" with the callee as a child in mode 2); a field
   access is "field:x", "field" or "DotAccess"; another non-local name its
   text, "ident" or "N"; a constructor pattern likewise. In every mode
   locals are "$v", literals their kind, operators and other specials
   their symbol; types and macro token bags carry no node. *)
type knode = {
  label : Fhash.t;
  kids : knode list;
  is_stmt : bool;
  is_block : bool;
  kstmts : int; (* statement nodes in the subtree *)
  height : int; (* 0 for a leaf *)
}

type kitem = KE of G.expr | KS of G.stmt | KP of G.pattern

(* The statements, expressions and patterns directly under [k], in
   visiting (source) order, through any intermediate node kind. *)
let direct_kids (k : kitem) : kitem list =
  let acc = ref [] and root = ref true in
  let enter f x c = if !root then (root := false; f x) else acc := c :: !acc in
  let v =
    object
      inherit [_] G.iter_no_id_info as super
      method! visit_expr env e = enter (super#visit_expr env) e (KE e)
      method! visit_stmt env s = enter (super#visit_stmt env) s (KS s)
      method! visit_pattern env p = enter (super#visit_pattern env) p (KP p)
    end
  in
  (match k with
  | KE e -> v#visit_expr () e
  | KS s -> v#visit_stmt () s
  | KP p -> v#visit_pattern () p);
  List.rev !acc

let expr_kind_name (e : G.expr_kind) : string =
  match e with
  | G.L _ -> "L"
  | G.Container _ -> "Container"
  | G.Comprehension _ -> "Comprehension"
  | G.Record _ -> "Record"
  | G.Constructor _ -> "Constructor"
  | G.N _ -> "N"
  | G.IdSpecial _ -> "IdSpecial"
  | G.Call _ -> "Call"
  | G.RegexpTemplate _ -> "RegexpTemplate"
  | G.New _ -> "New"
  | G.Xml _ -> "Xml"
  | G.Assign _ -> "Assign"
  | G.AssignOp _ -> "AssignOp"
  | G.LetPattern _ -> "LetPattern"
  | G.DotAccess _ -> "DotAccess"
  | G.ArrayAccess _ -> "ArrayAccess"
  | G.SliceAccess _ -> "SliceAccess"
  | G.Lambda _ -> "Lambda"
  | G.AnonClass _ -> "AnonClass"
  | G.Conditional _ -> "Conditional"
  | G.Yield _ -> "Yield"
  | G.Await _ -> "Await"
  | G.Cast _ -> "Cast"
  | G.Seq _ -> "Seq"
  | G.Ref _ -> "Ref"
  | G.DeRef _ -> "DeRef"
  | G.Alias _ -> "Alias"
  | G.LocalImportAll _ -> "LocalImportAll"
  | G.Ellipsis _ -> "Ellipsis"
  | G.DeepEllipsis _ -> "DeepEllipsis"
  | G.DisjExpr _ -> "DisjExpr"
  | G.TypedMetavar _ -> "TypedMetavar"
  | G.DotAccessEllipsis _ -> "DotAccessEllipsis"
  | G.StmtExpr _ -> "StmtExpr"
  | G.OtherExpr ((k, _), _) -> "OtherExpr:" ^ k
  | G.RawExpr _ -> "RawExpr"

let pattern_kind_name (p : G.pattern) : string =
  match p with
  | G.PatLiteral _ -> "PatLiteral"
  | G.PatConstructor _ -> "PatConstructor"
  | G.PatRecord _ -> "PatRecord"
  | G.PatId _ -> "PatId"
  | G.PatTuple _ -> "PatTuple"
  | G.PatList _ -> "PatList"
  | G.PatKeyVal _ -> "PatKeyVal"
  | G.PatWildcard _ -> "PatWildcard"
  | G.PatDisj _ -> "PatDisj"
  | G.PatTyped _ -> "PatTyped"
  | G.PatWhen _ -> "PatWhen"
  | G.PatAs _ -> "PatAs"
  | G.PatType _ -> "PatType"
  | G.PatEllipsis _ -> "PatEllipsis"
  | G.DisjPat _ -> "DisjPat"
  | G.OtherPat ((k, _), _) -> "OtherPat:" ^ k

let name_text (n : G.name) : string =
  origin_toks (G.Name n)
  |> List.sort_uniq (fun a b ->
         compare (Tok.bytepos_of_tok a) (Tok.bytepos_of_tok b))
  |> List.map Tok.content_of_tok |> String.concat ""

let last_name (n : G.name) : string =
  match n with
  | G.Id ((s, _), _) -> s
  | G.IdQualified { G.name_last = (s, _), _; _ } -> s

(* A non-local name under [names]: its text, "ident" or "N". *)
let other_name ~names (text : string) (kind : string) : string =
  match names with 0 -> "n:" ^ text | 1 -> "ident" | _ -> kind

(* Label of [k] and whether its first direct kid (a call's callee) is
   folded into it. *)
let label_of ~names (k : kitem) : string * bool =
  match k with
  | KS st -> (Printf.sprintf "s%d" (kind_of st), false)
  | KP p -> (
      match p with
      | G.PatId _ -> ("$v", false)
      | G.PatLiteral l -> (Printf.sprintf "lit%d" (literal_kind l), false)
      | G.PatConstructor (n, _) ->
          (other_name ~names (name_text n) "PatConstructor", false)
      | _ -> (pattern_kind_name p, false))
  | KE e -> (
      match e.G.e with
      | G.L l -> (Printf.sprintf "lit%d" (literal_kind l), false)
      | G.N (G.Id (_, info)) when is_local info -> ("$v", false)
      | G.N n -> (other_name ~names (name_text n) "N", false)
      | G.IdSpecial (sp, _) -> ("sp:" ^ G.show_special sp, false)
      | G.Call ({ G.e = G.IdSpecial (sp, _); _ }, _) ->
          ("op:" ^ G.show_special sp, true)
      | G.Call ({ G.e = G.N n; _ }, (_, l, _)) when names < 2 ->
          let local =
            match n with G.Id (_, info) -> is_local info | _ -> false
          in
          ( (match (names, local) with
            | 0, false -> "call:" ^ last_name n
            | 0, true -> "call:$v"
            | _, false -> Printf.sprintf "call/%d" (List.length l)
            | _, true -> Printf.sprintf "call$v/%d" (List.length l)),
            true )
      | G.DotAccess (_, _, G.FN n) when names < 2 ->
          ((if names = 0 then "field:" ^ name_text n else "field"), false)
      | G.Constructor (n, _) ->
          (other_name ~names (name_text n) "Constructor", false)
      | G.AssignOp (_, (op, _), _) -> ("AssignOp:" ^ G.show_operator op, false)
      | k -> (expr_kind_name k, false))

let rec kind_tree ~names (k : kitem) : knode =
  let label, fold = label_of ~names k in
  let kids =
    match direct_kids k with _ :: rest when fold -> rest | l -> l
  in
  let kids = List.map (kind_tree ~names) kids in
  let is_stmt = match k with KS _ -> true | _ -> false in
  {
    label = Fhash.hash_str label;
    kids;
    is_stmt;
    is_block =
      (match k with KS { G.s = G.Block _; _ } -> true | _ -> false);
    kstmts =
      List.fold_left (fun n c -> n + c.kstmts) (if is_stmt then 1 else 0) kids;
    height = List.fold_left (fun h c -> max h (c.height + 1)) 0 kids;
  }

(* Shape of a kind subtree truncated at [r] levels below its root, for r =
   0..rmax, and the full shape (no truncation): the label mixed with the
   kids' shapes one level shallower. A shape at r >= height is the full
   shape. *)
let shape_feed (label : Fhash.t) (kids : Fhash.t list) : Fhash.t =
  let f = Fhash.Feed.create () in
  Fhash.Feed.tag f 0xD1;
  Fhash.Feed.int f label;
  List.iter
    (fun h ->
      Fhash.Feed.tag f 0x01;
      Fhash.Feed.int f h)
    kids;
  Fhash.Feed.finish f


(* flake's log_counts: a hash seen c times also yields (hash, 2^i) for
   every 2^i <= c, i >= 1, so repeats count without becoming a bag. *)
let log_counted (fs : (Fhash.t * float) list) : (Fhash.t * float) list =
  let counts = Hashtbl.create 64 in
  List.iter
    (fun (h, w) ->
      let c, w0 = Option.value (Hashtbl.find_opt counts h) ~default:(0, w) in
      Hashtbl.replace counts h (c + 1, Float.max w w0))
    fs;
  Hashtbl.fold
    (fun h (c, w) acc ->
      let rec more k acc =
        if k > c then acc else more (2 * k) ((Fhash.mix h k, w) :: acc)
      in
      more 2 ((h, w) :: acc))
    counts []

(* Mode-2 subtree hashes and the extras, from the kind tree of [body]. *)
let kind_features (cfg : config) (body : G.stmt) : (Fhash.t * float) list =
  let root = kind_tree ~names:cfg.names (KS body) in
  let rmax = max cfg.radius (if cfg.ngrams > 0 then 1 else 0) in
  let subtrees = ref [] and extras = ref [] in
  let extra h w = extras := (h, w) :: !extras in
  (* (shapes at 0..rmax, full shape) of [n], emitting on the way *)
  let rec go (n : knode) : Fhash.t array * Fhash.t =
    let kids = List.map go n.kids in
    let shapes =
      Array.init (rmax + 1) (fun r ->
          if r = 0 then shape_feed n.label []
          else shape_feed n.label (List.map (fun (s, _) -> s.(r - 1)) kids))
    in
    let full = shape_feed n.label (List.map snd kids) in
    for r = 1 to min cfg.radius n.height do
      extra shapes.(r) (cfg.w_radius *. float_of_int r)
    done;
    if cfg.edges then
      List.iter
        (fun c -> extra (Fhash.of_ints 0xD2 [ n.label; c.label ]) cfg.w_edge)
        n.kids;
    if cfg.ngrams > 0 && n.is_block then begin
      let toks = Array.of_list (List.map (fun (s, _) -> s.(1)) kids) in
      let len = Array.length toks in
      for k = 1 to cfg.ngrams do
        for i = 0 to len - k do
          extra
            (Fhash.of_ints 0xD3 (k :: Array.to_list (Array.sub toks i k)))
            (cfg.w_ngram *. float_of_int k)
        done
      done
    end;
    if cfg.names = 2 && cfg.subtrees && n.is_stmt && n != root then
      subtrees := (full, 1. +. log (float_of_int n.kstmts)) :: !subtrees;
    (shapes, full)
  in
  ignore (go root);
  !subtrees @ if cfg.log_counts then log_counted !extras else !extras

(* The [Tree] family's features: every statement subtree in the body, not
   the body itself, plus the extras [cfg] switches on. A whole-body hash
   adds nothing the Base family does not already score for whole
   functions, and it is the one feature any top-level edit or reordering
   always breaks, with the largest weight. *)
let features ?(cfg = default_config) (body : G.stmt) : (Fhash.t * float) list =
  let token_subtrees =
    if cfg.subtrees && cfg.names < 2 then
      nodes ~names:cfg.names body
      |> List.filter_map (fun n ->
             if n.stmt == body then None else Some (n.hash, weight n))
    else []
  in
  let kinds =
    if cfg.names = 2 && cfg.subtrees || cfg.ngrams > 0 || cfg.edges
       || cfg.radius > 0
    then kind_features cfg body
    else []
  in
  token_subtrees @ kinds
