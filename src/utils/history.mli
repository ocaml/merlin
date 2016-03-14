(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2015  Frédéric Bour  <frederic.bour(_)lakaban.net>
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

(** {0 History}
  * A sort of zipper: maintains and synchronizes a list of different
  * versions of an object (see ocamlmerlin.ml top comment).
  *)
type 'a t

(* New history *)
val initial : 'a -> 'a t

val of_list : 'a list -> 'a t
val to_list : 'a t -> 'a list

(** Element to the left of the cursor
  * (if last operation was an insertion, the inserted value is returned)
  *)
val focused : 'a t -> 'a

val head : 'a t -> 'a List.non_empty
val tail : 'a t -> 'a list
val position : 'a t -> int

(** Move forward while item under cursor satisfy predicate *)
val seek_forward  : ?last:bool -> ('a -> bool) -> 'a t -> 'a t
(** Move backward while item under cursor satisfy predicate *)
val seek_backward : ?last:bool -> ('a -> bool) -> 'a t -> 'a t

(** Moves an arbitrary number of steps.
  *
  * May stop earlier if it reaches an end of history.
 *)
val move : int -> 'a t -> 'a t

(** Seek a precise position.
  *
  * May stop earlier if it reaches an end of history.
 *)
val seek : int -> 'a t -> 'a t

(** Adds an element to the left of the cursor and drops the tail:
  * insert w [..zyx|abc..] = [..zyxw|] *)
val insert : 'a -> 'a t -> 'a t

(** Adds an element to the left of the cursor and keep the tail:
  * insert w [..zyx|abc..] = [..zyxw|abc] *)
val fake_insert : 'a -> 'a t -> 'a t

(** Modifies focused element. *)
val modify : ('a -> 'a) -> 'a t -> 'a t

val append : ('a -> 'a) -> 'a t -> 'a t

(* Drop tail of history *)
val drop_tail : 'a t -> 'a t

(* [hb' = reconstruct ~init ~fold ha]
   where
     [ha = (a_0, a_1, a_2, ...)]
   computes
     [hb' = (b_0 = init a_0, b_1 = fold a_1 b_0, b_2 = fold a_2 b_1, ...)]
   for all items in [ha] (including those after cursor) *)
val reconstruct : init:('a -> 'b) -> fold:('a -> 'b -> 'b) -> 'a t -> 'b t

(* [hb' = sync ~check ~init ~fold ha (Some hb)] synchronize [ha] and [hb]
   up to current position in [ha].
   [hb] is supposed to be a previous result of [sync].
   Let [ha] be:
     (a_0, a_1, a_2, ...)
   After synchronization, [hb'] is equal to:
     (b_0 = init a_0, b_1 = fold a_1 b_0, b_2 = fold a_2 b_1, ...)
   [check] function is called to check if two items are still valid, in order
   to minimize work and update [hb] incrementally.

   If second argument is None, then this function behave like
   [reconstruct ~init ~fold ha] with an empty tail.
*)
val sync
  :  strong_check:('a -> 'b -> bool)
  -> weak_check:('a -> 'b -> bool)
  -> init:('a -> 'b)
  -> strong_fold:('a -> 'b -> 'b)
  -> weak_update:('a -> 'b -> 'b)
  -> 'a t -> 'b t option -> 'b t * [`Nothing_done | `Updated]
