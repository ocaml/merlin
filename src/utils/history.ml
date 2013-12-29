(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013  Frédéric Bour  <frederic.bour(_)lakaban.net>
                      Thomas Refis  <refis.thomas(_)gmail.com>
                      Simon Castellan  <simon.castellan(_)iuwt.fr>

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

type 'a non_empty =
  | One of 'a
  | More of 'a * 'a non_empty

let nhd = function
  | One x | More (x,_) -> x

(** {0 History}
  * A sort of zipper: maintains and synchronizes a list of different
  * versions of an object (see ocamlmerlin.ml top comment).
  *)
type 'a t = {head: 'a non_empty; position: int; tail: 'a list}

(* New history *)
let initial x = {head = One x; position = 0; tail = []}

let head x = x.head
let tail x = x.tail
let position x = x.position

(** Element to the left of the cursor
  * (if last operation was an insertion, the inserted value is returned)
  *)
let focused' (More (x,_) | One x) = x
let focused t = focused' t.head

(** Move forward while item under cursor satisfy predicate *)
let rec seek_forward pred head position = function
  | x :: tail when pred (focused' head) ->
    seek_forward pred (More (x,head)) (position + 1) tail
  | tail -> {head; position; tail}
let seek_forward pred = function
  | {head; position; tail = x :: tail} when pred (focused' head) ->
    seek_forward pred (More (x,head)) (position + 1) tail
  | t -> t

(** Move backward while item under cursor satisfy predicate *)
let rec seek_backward pred position tail = function
  | More (x,head) when pred x ->
    seek_backward pred (position - 1) (x :: tail) head
  | head -> {head; position; tail}
let seek_backward pred = function
  | {head = More (x,head); position; tail} when pred x ->
    seek_backward pred (position - 1) (x :: tail) head
  | t -> t

(** Moves an arbitrary number of steps.
  *
  * May stop earlier if it reaches an end of history.
 *)
let rec move n head position tail =
  match head, tail with
  | head, (x :: tail) when n > 0 ->
    move (n - 1) (More (x,head)) (position + 1) tail
  | (More (x,head)), tail when n < 0 ->
    move (n + 1) head (position - 1) (x :: tail)
  | head, tail -> {head; position; tail}
let move n = function
  | {head; position; tail = x :: tail} when n > 0 ->
    move (n - 1) (More (x,head)) (position + 1) tail
  | {head = More (x,head); position; tail} when n < 0 ->
    move (n + 1) head (position - 1) (x :: tail)
  | t -> t

(** Adds an element to the left of the cursor:
  * insert w [..zyx|abc..] = [..zyxw|abc..] *)
let insert x h =
  {head = More (x, h.head); position = h.position + 1; tail = []}

(** Modifies focused element. *)
let modify f = function
  | {head = One x; position; _} ->
    {head = One (f x); position; tail = []}
  | {head = More (x,head); position; _} ->
    {head = More (f x, head); position; tail = []}

let append f {head = (One x | More (x,_) as head); position; _} =
  {head = More (f x, head); position = position + 1; tail = []}

let reconstruct ~init ~fold h =
  let f b x' = More (fold x' (nhd b), b) in
  let rec past tail = function
    | More (x,head) -> past (x :: tail) head
    | One x -> List.fold_left ~f ~init:(One (init x)) tail
  in
  let head = past [] h.head in
  let f (b,l) a =
    let b' = fold a b in
    (b', (b' :: l))
  in
  let tail = List.rev (snd (List.fold_left ~f ~init:(nhd head, []) h.tail)) in
  let position = h.position in
  {head; position; tail}

let sync ~check ~init ~fold a = function
  | Some b ->
    let b = move (a.position - b.position) b in
    assert (b.position <= a.position);
    if b.position = a.position && check (focused a) (focused b) then b
    else
      let rec aux worklist ha hb =
        match ha, hb with
        | One a, One b | More (a,_), More (b,_) when check a b ->
          worklist, hb
        | One a, One b ->
          worklist, One (init a)
        | More (a,ha), More (_,hb) ->
          aux (a :: worklist) ha hb
        | _, _ -> assert false
      in
      let rec rewind worklist ha count =
        if count > 0
        then match ha with
          | More (a,ha) -> rewind (a :: worklist) ha (count - 1)
          | _ -> assert false
        else aux worklist ha b.head
      in
      let worklist, hb = rewind [] a.head (a.position - b.position) in
      let hb = List.fold_left' worklist ~init:hb
          ~f:(fun a hb -> More (fold a (focused' hb), hb))
      in
      { head = hb; tail = []; position = a.position }
  | None ->
    reconstruct ~init ~fold {a with tail = []}
