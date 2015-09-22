(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2014  Frédéric Bour  <frederic.bour(_)lakaban.net>
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
exception Warning of Location.t * string

val warnings: exn list ref option fluid
val raise_warning: exn -> unit
val prerr_warning: Location.t -> Warnings.t -> unit
val catch_warnings: exn list ref -> (unit -> 'a) -> 'a

val compare_pos: Lexing.position -> Location.t -> int

(** Return the smallest location covered by both arguments,
    ghost if both are ghosts *)
val location_union : Location.t -> Location.t -> Location.t

(** Like location_union, but keep loc_ghost'ness of first argument *)
val location_extend : Location.t -> Location.t -> Location.t
