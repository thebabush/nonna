(* Semantic features: call surface, constants, params/returns.
   Port of v1's semantic.rs over the IL CFG. Each category is deduplicated:
   these are set-membership features ("calls parse_header", "mentions 4096"),
   the DFG features carry the structure. *)

module G = AST_generic

let feed_of (tagv : int) : Fhash.Feed.t =
  let f = Fhash.Feed.create () in
  Fhash.Feed.tag f tagv;
  f

let dedup (l : Fhash.t list) : Fhash.t list = List.sort_uniq compare l

let trivial_number (s : string) =
  match s with
  | "0" | "1" | "-1" | "2" | "0." | "1." | "0.0" | "1.0" -> true
  | _ -> false

(* Channelized: calls carry NAMES; str/int/float consts carry VALUES (three
   separate channels — they behave differently: a magic string is a strong
   identity signal, a `2` is noise); misc (param count, return pattern) is
   name-free. *)
type parts = {
  calls : Fhash.t list;
  str_consts : Fhash.t list;
  int_consts : Fhash.t list;
  float_consts : Fhash.t list;
  misc : Fhash.t list;
}

let extract_parts (fcfg : IL.fun_cfg) : parts =
  let nodes = Il_util.nodes_in_order fcfg.IL.cfg in
  let calls = ref [] in
  let strings = ref [] in
  let ints = ref [] in
  let floats = ref [] in
  let returns = ref 0 in
  let returns_value = ref false in

  nodes
  |> List.iter (fun (_, (node : IL.node)) ->
         (* call surface: one feature per unique (target, arity) *)
         (match node.IL.n with
         | IL.NInstr { i = IL.Call (_, fexp, args); _ } ->
             let f = feed_of 0xB0 in
             Fhash.Feed.int f (List.length args);
             (match fexp.IL.e with
             | IL.Fetch { IL.base = IL.Var fn; rev_offset = [] } ->
                 Fhash.Feed.tag f 0x01;
                 Fhash.Feed.str f (fst fn.IL.ident)
             | IL.Fetch { IL.rev_offset = { o = IL.Dot m; _ } :: _; _ } ->
                 Fhash.Feed.tag f 0x02;
                 Fhash.Feed.str f (fst m.IL.ident)
             | _ -> Fhash.Feed.tag f 0x03);
             calls := Fhash.Feed.finish f :: !calls
         | IL.NInstr { i = IL.CallSpecial (_, (sp, _), args); _ } ->
             let f = feed_of 0xB0 in
             Fhash.Feed.int f (List.length args);
             Fhash.Feed.tag f 0x04;
             Fhash.Feed.str f (IL.show_call_special sp);
             calls := Fhash.Feed.finish f :: !calls
         | IL.NInstr { i = IL.New (_, ty, _, args); _ } ->
             let f = feed_of 0xB0 in
             Fhash.Feed.int f (List.length args);
             Fhash.Feed.tag f 0x05;
             Fhash.Feed.str f (Il_util.ty_descr ty);
             calls := Fhash.Feed.finish f :: !calls
         | IL.NReturn (_, e) ->
             incr returns;
             if not (Il_util.exp_is_trivial e) then returns_value := true
             else if
               match e.IL.e with IL.Literal (G.Unit _) -> false | _ -> true
             then returns_value := true
         | _ -> ());
         (* constants anywhere in the node's exps *)
         Il_util.exps_of_node node.IL.n
         |> List.iter
              (Il_util.iter_exp (fun e ->
                   match e.IL.e with
                   | IL.Literal lit -> (
                       match Il_util.const_descr lit with
                       | ("string" | "char"), Some v ->
                           let f = feed_of 0xC0 in
                           Fhash.Feed.str f v;
                           strings := Fhash.Feed.finish f :: !strings
                       | "int", Some v when not (trivial_number v) ->
                           let f = feed_of 0xC1 in
                           Fhash.Feed.str f v;
                           ints := Fhash.Feed.finish f :: !ints
                       | "float", Some v when not (trivial_number v) ->
                           let f = feed_of 0xC2 in
                           Fhash.Feed.str f v;
                           floats := Fhash.Feed.finish f :: !floats
                       | _ -> ())
                   | _ -> ())));

  let param_count =
    let f = feed_of 0xC3 in
    Fhash.Feed.int f (List.length fcfg.IL.params);
    Fhash.Feed.finish f
  in
  let return_pattern =
    let f = feed_of 0xC4 in
    Fhash.Feed.int f (min !returns 4);
    Fhash.Feed.tag f (if !returns_value then 1 else 0);
    Fhash.Feed.finish f
  in
  {
    calls = dedup !calls;
    str_consts = dedup !strings;
    int_consts = dedup !ints;
    float_consts = dedup !floats;
    misc = [ param_count; return_pattern ];
  }
