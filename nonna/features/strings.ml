(* Pieces of string literals that still match after small edits to the
   string (port of ida-similarity's strings.py).

   Content-defined chunks cut wherever the hash of the last [window] bytes has
   its low [bits] bits clear, so cuts depend only on nearby bytes: after an
   edit, the cuts (and the chunks) away from it are the same as before. *)

let window = 3
let min_len = 3
let max_len = 16

(* only the start of long strings: the rest is rarely what identifies them *)
let limit = 256

let chunks ?(bits = 2) (s : string) : string list =
  let s = if String.length s > limit then String.sub s 0 limit else s in
  let mask = (1 lsl bits) - 1 in
  let out = ref [] in
  let start = ref 0 in
  for i = 0 to String.length s - 1 do
    let len = i + 1 - !start in
    if len >= min_len then begin
      let w = ref 0L in
      for j = max 0 (i + 1 - window) to i do
        w := Int64.logor (Int64.shift_left !w 8) (Int64.of_int (Char.code s.[j]))
      done;
      if len >= max_len || Int64.to_int (Fhash.mix64 !w) land mask = 0 then begin
        out := String.sub s !start len :: !out;
        start := i + 1
      end
    end
  done;
  if !start < String.length s then
    out := String.sub s !start (String.length s - !start) :: !out;
  List.rev !out
