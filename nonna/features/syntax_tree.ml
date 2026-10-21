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
   - comments carry no AST tokens, so they are invisible; formatting too. *)

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

type tclass = Var | Lit of int | Skip

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
   (quoted, numeric, true/false) as their kind. *)
let classify (body : G.stmt) : (int, tclass) Hashtbl.t =
  let cls = Hashtbl.create 64 in
  let set tok c =
    if Tok.is_origintok tok then Hashtbl.replace cls (Tok.bytepos_of_tok tok) c
  in
  let locals = Hashtbl.create 16 and bags = ref [] in
  let var ((s, tok) : G.ident) =
    Hashtbl.replace locals s ();
    set tok Var
  in
  let lit any l =
    match
      List.sort_uniq
        (fun a b -> compare (Tok.bytepos_of_tok a) (Tok.bytepos_of_tok b))
        (origin_toks any)
    with
    | t :: rest ->
        set t (Lit (literal_kind l));
        List.iter (fun t -> set t Skip) rest
    | [] -> ()
  in
  let v =
    object
      inherit [_] G.iter_no_id_info as super

      method! visit_expr env e =
        match e.G.e with
        | G.N (G.Id (id, info)) when is_local info -> var id
        | G.L l -> lit (G.E e) l
        | G.Call ({ G.e = G.N n; _ }, args) ->
            (match n with
            | G.Id (id, info) -> if is_local info then var id
            | G.IdQualified { G.name_last = (_, last), _; _ } ->
                List.iter (fun t -> set t Skip) (origin_toks (G.Name n));
                if Tok.is_origintok last then
                  Hashtbl.remove cls (Tok.bytepos_of_tok last));
            super#visit_arguments env args
        | _ -> super#visit_expr env e

      method! visit_pattern env p =
        match p with
        | G.PatId (id, _) -> var id
        | G.PatLiteral l -> lit (G.P p) l
        | _ -> super#visit_pattern env p

      method! visit_parameter_classic env p =
        Option.iter var p.G.pname;
        super#visit_parameter_classic env p

      method! visit_argument env a =
        match a with
        | G.OtherArg (("ArgMacro", _), anys) ->
            bags := List.concat_map origin_toks anys :: !bags
        | _ -> super#visit_argument env a

      method! visit_definition env ((ent, kind) as def) =
        (match (ent.G.name, kind) with
        | G.EN (G.Id (id, _)), G.VarDef _ -> var id
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
let nodes (body : G.stmt) : node list =
  let cls = classify body in
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

(* The [Tree] family's features: every statement subtree in the body, not
   the body itself. A whole-body hash adds nothing the Base family does not
   already score for whole functions, and it is the one feature any
   top-level edit or reordering always breaks, with the largest weight. *)
let features (body : G.stmt) : (Fhash.t * float) list =
  nodes body
  |> List.filter_map (fun n ->
         if n.stmt == body then None else Some (n.hash, weight n))
