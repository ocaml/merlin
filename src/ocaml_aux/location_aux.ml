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

type t
  = Location.t
  = { loc_start: Lexing.position; loc_end: Lexing.position; loc_ghost: bool }

let compare_pos pos loc =
  if Lexing.compare_pos pos loc.Location.loc_start < 0 then
    -1
  else if Lexing.compare_pos pos loc.Location.loc_end > 0 then
    1
  else
    0

let union l1 l2 =
  if l1 = Location.none then l2
  else if l2 = Location.none then l1
  else {
    Location.
    loc_start = Lexing.min_pos l1.Location.loc_start l2.Location.loc_start;
    loc_end   = Lexing.max_pos l1.Location.loc_end l2.Location.loc_end;
    loc_ghost = l1.Location.loc_ghost && l2.Location.loc_ghost;
  }

let extend l1 l2 =
  if l1 = Location.none then l2
  else if l2 = Location.none then l1
  else {
    Location.
    loc_start = Lexing.min_pos l1.Location.loc_start l2.Location.loc_start;
    loc_end   = Lexing.max_pos l1.Location.loc_end l2.Location.loc_end;
    loc_ghost = l1.Location.loc_ghost;
  }
