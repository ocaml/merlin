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
type 'a t

(* New history *)
val initial : 'a -> 'a t

(** Element to the left of the cursor
  * (if last operation was an insertion, the inserted value is returned)
  *)
val focused : 'a t -> 'a

val head : 'a t -> 'a non_empty

(** Move forward while item under cursor satisfy predicate *)
val seek_forward  : ('a -> bool) -> 'a t -> 'a t
(** Move backward while item under cursor satisfy predicate *)
val seek_backward : ('a -> bool) -> 'a t -> 'a t

(** Moves an arbitrary number of steps.
  *
  * May stop earlier if it reaches an end of history.
 *)
val move : int -> 'a t -> 'a t

(** Adds an element to the left of the cursor and drop tail:
  * insert w [..zyx|abc..] = [..zyxw|] *)
val insert : 'a -> 'a t -> 'a t

(** Modifies focused element. *)
val modify : ('a -> 'a) -> 'a t -> 'a t

val append : ('a -> 'a) -> 'a t -> 'a t

val reconstruct : 'a t -> ('a -> 'b) -> ('b -> 'a -> 'b) -> 'b t
