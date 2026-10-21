(* D16: Tree features are rename-invariant (variables and parameters) but
   keep call targets: renamed copies hash equally, a copy calling a
   different function does not. *)

module G = AST_generic
module Syntax_tree = Nonna_features.Syntax_tree
module Hset = Set.Make (Int)

(* Tree feature hashes of the first function in [src], parsed and named the
   way Units.units_of_file does. *)
let hashes ~(ext : string) (src : string) : Hset.t =
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
                (Syntax_tree.features
                   (AST_generic_helpers.funcbody_to_stmt fdef.G.fbody)))
        ast;
      match !found with
      | Some fs -> Hset.of_list (List.map fst fs)
      | None -> failwith ("no function in " ^ tmp))

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

let () =
  Parsing_init.init ();
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
