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

type 'a non_empty =
  | One of 'a
  | More of 'a * 'a non_empty 

(** {0 Historique}
  * A sort of zipper: maintains and synchronizes a list of different
  * versions of an object (see ocamlmerlin.ml top comment).
  *)
type 'a t = {head: 'a non_empty; tail: 'a list}

(* New history *)
let initial x = {head = One x; tail = []}

(** Element to the left of the cursor
  * (if last operation was an insertion, the inserted value is returned)
  *)
let focused {head = (More (x,_) | One x); _} = x

(** Move forward while item under cursor satisfy predicate *)
let rec seek_forward pred = function
  | {head; tail = x :: tail'} as t when pred (focused t) ->
    seek_forward pred {head = More (x,head); tail = tail'}
  | t -> t

(** Move backward while item under cursor satisfy predicate *)
let rec seek_backward pred = function
  | {head = More (x,head'); tail} when pred x ->
    seek_backward pred {head = head'; tail = x :: tail}
  | t -> t

(** Moves an arbitrary number of steps.
  *
  * May stop earlier if it reaches an end of history.
 *)
let rec move n = function
  | {head; tail = x :: tail} when n > 0 -> 
    move (pred n) {head = More (x,head); tail}
  | {head = More (x,head); tail = tail} when n < 0 -> 
    move (succ n) {head; tail = x :: tail}
  | t -> t

(** Adds an element to the left of the cursor:
  * insert w [..zyx|abc..] = [..zyxw|abc..] *)
let insert x h = {head = More (x, h.head); tail = []}

(** Modifies focused element. *)
let modify f = function
  | {head = One x; _} -> {head = One (f x); tail = []}
  | {head = More (x,head); _} -> {head = More (f x, head); tail = []}

let append f {head = (One x | More (x,_) as head); _} =
  {head = More (f x, head); tail = []}
