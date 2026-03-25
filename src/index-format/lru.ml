(* This file includes code inspired by or derived from work
by Clément Pascutto (MIT License, 2020).

--------------------------------------------------------------------------------

The MIT License

Copyright (c) 2020 Clément Pascutto

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE. *)

module Make (K : sig
  type t

  val equal : t -> t -> bool
  val hash : t -> int
end) =
struct
  module H = Hashtbl.Make (K)
  type key = K.t

  type 'a t = {
    htbl : (K.t * 'a) Dbllist.cell H.t;
    dbllst : (K.t * 'a) Dbllist.t;
    cap: int;
    mutable size : int;
  }

  let unsafe_create cap = {
    htbl = H.create 0;
    dbllst = Dbllist.create();
    cap;
    size = 0;
  }

  let create cap =
    if cap <= 0 then invalid_arg "capacity must be strictly positive";
    unsafe_create cap

  let find t k =
    let c = H.find t.htbl k in
    Dbllist.promote t.dbllst c;
    let _, found_value = Dbllist.get c in
    found_value

  let find_opt t k =
    try Some (find t k)
    with Not_found -> None

  let add t k v =
    match H.find_opt t.htbl k with
    | Some c ->
      Dbllist.promote_update t.dbllst c (k, v);
    | None ->
      if t.size + 1 > t.cap then (
        let discarded_key, _ = Dbllist.discard t.dbllst in
        H.remove t.htbl discarded_key;
        t.size <- t.size - 1
      );
      let new_c = Dbllist.add_front t.dbllst (k, v) in
      H.add t.htbl k new_c;
      t.size <- t.size + 1
end