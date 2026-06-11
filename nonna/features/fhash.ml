(* Feature hashing.
 *
 * v1 used xxh3; any well-distributed 64-bit hash works since only the
 * collision rate matters. We use FNV-1a 64 over a byte buffer for the
 * feed-style hasher, and a splitmix64 finalizer for hash mixing, all in
 * pure OCaml (no xxhash bindings in the switch). Hashes are truncated to
 * 62 bits so they fit in a non-negative OCaml int.
 *)

type t = int

let fnv_offset = 0xcbf29ce484222325L
let fnv_prime = 0x100000001b3L
let to62 (h : int64) : int = Int64.to_int (Int64.logand h 0x3FFFFFFFFFFFFFFFL)

let fnv_byte (h : int64) (c : int) : int64 =
  Int64.mul (Int64.logxor h (Int64.of_int c)) fnv_prime

let fnv_string (h : int64) (s : string) : int64 =
  let h = ref h in
  String.iter (fun c -> h := fnv_byte !h (Char.code c)) s;
  !h

let fnv_int64 (h : int64) (v : int64) : int64 =
  let h = ref h in
  for i = 0 to 7 do
    h :=
      fnv_byte !h
        (Int64.to_int (Int64.logand (Int64.shift_right_logical v (i * 8)) 0xFFL))
  done;
  !h

(* splitmix64 finalizer *)
let mix64 (x : int64) : int64 =
  let x = Int64.logxor x (Int64.shift_right_logical x 30) in
  let x = Int64.mul x 0xbf58476d1ce4e5b9L in
  let x = Int64.logxor x (Int64.shift_right_logical x 27) in
  let x = Int64.mul x 0x94d049bb133111ebL in
  Int64.logxor x (Int64.shift_right_logical x 31)

(* Order-dependent pair mix: mix a b <> mix b a. *)
let mix (a : t) (b : t) : t =
  to62
    (mix64
       (Int64.add
          (Int64.mul (Int64.of_int a) 0x9E3779B97F4A7C15L)
          (Int64.of_int b)))

(* Mix an operand position into a hash so the input slot matters. *)
let mix_pos (h : t) (pos : int) : t =
  to62 (mix64 (Int64.add (Int64.of_int h) (Int64.mul (Int64.of_int (pos + 1)) 0x165667B19E3779F9L)))

let hash_str (s : string) : t = to62 (fnv_string fnv_offset s)

(* Feed-style incremental hasher (port of v1's FeatureHasher). *)
module Feed = struct
  type t = { buf : Buffer.t }

  let create () = { buf = Buffer.create 64 }
  let tag t (b : int) = Buffer.add_char t.buf (Char.chr (b land 0xFF))

  let int t (v : int) =
    Buffer.add_int64_le t.buf (Int64.of_int v)

  let str t (s : string) =
    int t (String.length s);
    Buffer.add_string t.buf s

  let sep t = Buffer.add_char t.buf '\xFF'

  let finish t : int =
    to62 (fnv_string fnv_offset (Buffer.contents t.buf))
end

(* Hash with one of K seeded "permutations" (for MinHash). *)
let permute ~(seed : int64) (x : t) : t =
  to62 (mix64 (Int64.logxor (Int64.of_int x) seed))
