(* D16: Tree features are rename-invariant (variables and parameters) but
   keep call targets: renamed copies hash equally, a copy calling a
   different function does not. Name modes 1 and 2 (entity kinds, kinds
   only) also merge copies that differ in a callee or a type name, in every
   variant; mode 0 keeps today's hashes exactly (golden digests taken from
   the code before the modes existed). *)

module G = AST_generic
module Syntax_tree = Nonna_features.Syntax_tree
module Hset = Set.Make (Int)

(* Tree features of the first function in [src], parsed and named the way
   Units.units_of_file does. *)
let features ?cfg ~(ext : string) (src : string) : (int * float) list =
  let tmp = Filename.temp_file "nonna_tree" ext in
  Fun.protect
    ~finally:(fun () -> Sys.remove tmp)
    (fun () ->
      Out_channel.with_open_bin tmp (fun oc -> output_string oc src);
      let path = Fpath.v tmp in
      let ast = Parse_target.parse_program path in
      let lang = Lang.lang_of_filename_exn path in
      Naming_AST.resolve lang ast;
      Implicit_return.mark_implicit_return lang ast;
      let found = ref None in
      Visit_function_defs.visit
        (fun _ fdef ->
          if !found = None then
            found :=
              Some
                (Syntax_tree.features ?cfg
                   (AST_generic_helpers.funcbody_to_stmt fdef.G.fbody)))
        ast;
      match !found with
      | Some fs -> fs
      | None -> failwith ("no function in " ^ tmp))

let hashes ?cfg ~ext src : Hset.t =
  Hset.of_list (List.map fst (features ?cfg ~ext src))

(* (count, digest) of the sorted (hash, weight) list *)
let digest ?cfg ~ext src : int * string =
  let fs = List.sort compare (features ?cfg ~ext src) in
  ( List.length fs,
    Digest.to_hex
      (Digest.string
         (String.concat ","
            (List.map (fun (h, w) -> Printf.sprintf "%d:%.6f" h w) fs))) )

let fails = ref 0

let check name ok =
  if not ok then (
    Printf.printf "FAIL %s\n" name;
    incr fails)

let rust_a =
  {|fn total(xs: &[i32], k: i32) -> i32 {
    let mut acc = 0;
    for x in xs {
        if *x > k {
            acc += x * 2;
            log(acc, "big");
        }
    }
    acc
}|}

let rust_renamed =
  {|fn sum_big(items: &[i32], limit: i32) -> i32 {
    // a comment the original lacks
    let mut s = 0;
    for it in items {
        if *it > limit { s += it * 7; log(s, "other"); }
    }
    s
}|}

let rust_other_call =
  {|fn total(xs: &[i32], k: i32) -> i32 {
    let mut acc = 0;
    for x in xs {
        if *x > k {
            acc += x * 2;
            trace(acc, "big");
        }
    }
    acc
}|}

(* differs from [rust_typed] in a type name only (the qualifier of
   `Foo::new` is skipped in every mode) *)
let rust_typed =
  {|fn wrap(xs: &[i32]) -> usize {
    let w: Foo = Foo::new(xs);
    w.len() as usize
}|}

let rust_other_type =
  {|fn wrap(xs: &[i32]) -> usize {
    let w: Bar = Bar::new(xs);
    w.len() as usize
}|}

let py_a =
  {|def total(xs, k):
    acc = 0
    for x in xs:
        if x > k:
            acc += x * 2
            log(acc, "big")
    return acc
|}

let py_renamed =
  {|def sum_big(items, limit):
    s = 0
    for it in items:
        if it > limit:
            s += it * 3
            log(s, "x")
    return s
|}

let py_other_call =
  {|def total(xs, k):
    acc = 0
    for x in xs:
        if x > k:
            acc += x * 2
            trace(acc, "big")
    return acc
|}

let cfg names = { Syntax_tree.default_config with names }

(* every extra on, so their features are checked for invariance too *)
let cfg_extras names =
  { (cfg names) with ngrams = 3; edges = true; radius = 2 }

let () =
  Parsing_init.init ();
  List.iter
    (fun (ext, src, want) ->
      check ("mode 0 = pre-modes golden " ^ ext) (digest ~ext src = want);
      check ("explicit mode 0 = default " ^ ext)
        (digest ~cfg:(cfg 0) ~ext src = want))
    [
      (".rs", rust_a, (8, "fb9a7df469ed446f7f5a14a0453c3320"));
      (".py", py_a, (8, "28ad0951b90474707de49939ef238131"));
    ];
  List.iter
    (fun (what, ext, a, b) ->
      (* renamed copies are equal in mode 0 too (checked below) *)
      if not (String.ends_with ~suffix:"renamed" what) then
        check (what ^ ": mode 0 keeps them apart")
          (not (Hset.equal (hashes ~ext a) (hashes ~ext b)));
      List.iter
        (fun (label, cfg) ->
          let ha = hashes ~cfg ~ext a in
          check (what ^ ": " ^ label ^ " non-empty") (not (Hset.is_empty ha));
          check (what ^ ": " ^ label ^ " merges them")
            (Hset.equal ha (hashes ~cfg ~ext b)))
        [
          ("mode 1", cfg 1);
          ("mode 2", cfg 2);
          ("mode 1 + extras", cfg_extras 1);
          ("mode 2 + extras", cfg_extras 2);
          ("mode 2 extras only", { (cfg_extras 2) with subtrees = false });
        ])
    [
      ("rust callee", ".rs", rust_a, rust_other_call);
      ("python callee", ".py", py_a, py_other_call);
      ("rust type", ".rs", rust_typed, rust_other_type);
      ("rust renamed", ".rs", rust_a, rust_renamed);
      ("python renamed", ".py", py_a, py_renamed);
    ];
  (* the extras add features; extras off and mode 0 is just the subtrees *)
  check "mode 0 extras add features"
    (Hset.cardinal (hashes ~cfg:(cfg_extras 0) ~ext:".rs" rust_a)
    > Hset.cardinal (hashes ~ext:".rs" rust_a));
  check "subtrees off, no extras: empty"
    (Hset.is_empty
       (hashes ~cfg:{ (cfg 1) with subtrees = false } ~ext:".rs" rust_a));
  List.iter
    (fun (lang, ext, a, renamed, other) ->
      let ha = hashes ~ext a in
      check (lang ^ ": non-empty") (not (Hset.is_empty ha));
      check (lang ^ ": renamed copy has equal Tree features")
        (Hset.equal ha (hashes ~ext renamed));
      check (lang ^ ": a different callee changes Tree features")
        (not (Hset.equal ha (hashes ~ext other))))
    [
      ("rust", ".rs", rust_a, rust_renamed, rust_other_call);
      ("python", ".py", py_a, py_renamed, py_other_call);
    ];
  if !fails > 0 then exit 1
