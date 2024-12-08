open Base

type 'a lazylist = Cons of 'a * (unit -> 'a lazylist)

let rec lseq n = Cons (n, fun () -> lseq (n + 1))
let lhd (Cons (n, _)) = n
let ltl (Cons (_, tf)) = tf ()

let rec ltake (Cons (h, tf)) n =
  match n with 0 -> [] | _ -> h :: ltake (tf ()) (n - 1)

let rec ldrop (Cons (_, tf) as ll) n =
  match n with 0 -> ll | _ -> ldrop (tf ()) (n - 1)

let rec lmap f (Cons (h, tf)) = Cons (f h, fun () -> lmap f (tf ()))

let lwith_index ll =
  let rec go_lwith_index (Cons (h, tf)) n =
    Cons ((n, h), fun () -> go_lwith_index (tf ()) @@ (n + 1))
  in
  go_lwith_index ll 0

let rec lfilter f (Cons (h, tf)) =
  if f h then Cons (h, fun () -> lfilter f (tf ())) else lfilter f (tf ())

let rec interleave (Cons (h, tf)) l = Cons (h, fun () -> interleave l (tf ()))
let rec ldbl n = Cons (n, fun () -> ldbl (n * 2))
let rec lnth (Cons (h, tf)) n = match n with 0 -> h | _ -> lnth (tf ()) (n - 1)

let rec lrepeat l =
  let go l n = List.nth l n in
  let rec lgo l n = Cons (go l n, fun () -> lgo l ((n + 1) % List.length l)) in
  lgo l 0

let lfab =
  let rec lfab_go x y = Cons (x, fun () -> lfab_go y (x + y)) in
  lfab_go 0 1

let unleave ll =
  ( lmap snd @@ lfilter (fun (i, _) -> equal (i % 2) 0) @@ lwith_index ll,
    lmap snd @@ lfilter (fun (i, _) -> equal (i % 2) 1) @@ lwith_index ll )

let as_char x = String.make 1 @@ Char.of_int_exn @@ (65 + x)

let rec char_helper x =
  if x < 26 then as_char x
  else  char_helper ((x / 26) - 1) ^ as_char @@ x % 26

let cell_column = lmap char_helper @@ lseq 0
