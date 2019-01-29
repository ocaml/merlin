(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2019  Merlin contributors

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  The Software is provided "as is", without warranty of any kind, express or
  implied, including but not limited to the warranties of merchantability,
  fitness for a particular purpose and noninfringement. In no event shall
  the authors or copyright holders be liable for any claim, damages or other
  liability, whether in an action of contract, tort or otherwise, arising
  from, out of or in connection with the software or the use or other dealings
  in the Software.

)* }}} *)

open Std

type ref_and_reset = F : 'a ref * (unit -> 'a) -> ref_and_reset
type bindings = { mutable refs: ref_and_reset list }

let new_bindings () =
  { refs = [] }

let ref t f =
  let result = ref (f ()) in
  t.refs <- (F (result, f)) :: t.refs;
  result

type 'a slot = { ref : 'a ref; mutable value : 'a }
type a_slot = Slot : 'a slot -> a_slot
type scope = a_slot list

let fresh t =
  List.map ~f:(fun (F(ref,f)) -> Slot {ref; value = f ()}) t.refs

let merge = (@)

type ref_and_value = V : 'a ref * 'a -> ref_and_value
let restore l = List.iter ~f:(fun (V(r,v)) -> r := v) l

let with_scope scope f =
  let backup = List.rev_map ~f:(fun (Slot {ref;_}) -> V (ref,!ref)) scope in
  List.iter ~f:(fun (Slot {ref;value}) -> ref := value) scope;
  match f () with
  | x ->
    List.iter ~f:(fun (Slot s) -> s.value <- !(s.ref)) scope;
    restore backup;
    x
  | exception exn ->
    List.iter ~f:(fun (Slot s) -> s.value <- !(s.ref)) scope;
    restore backup;
    reraise exn
