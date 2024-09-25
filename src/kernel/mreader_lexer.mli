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

type keywords = Lexer_raw.keywords

type triple = Parser_raw.token * Lexing.position * Lexing.position

type t

val make : Warnings.state -> keywords -> Mconfig.t -> Msource.t -> t

val for_completion :
  t -> Lexing.position -> bool (* complete labels or not *) * t

val initial_position : t -> Lexing.position

val tokens : t -> triple list
val keywords : t -> string list
val errors : t -> exn list
val comments : t -> (string * Location.t) list

val reconstruct_identifier :
  Mconfig.t -> Msource.t -> Lexing.position -> string Location.loc list

val identifier_suffix : string Location.loc list -> string Location.loc list
