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

open Sturgeon_stub

type spec =
  | Normal of Extension.set * Merlin_parser.kind
  | PP of string * Merlin_parser.kind
  | External of string * string list * Merlin_parser.kind

type t

val make : spec -> Merlin_source.t -> t
val update : Merlin_source.t -> t -> t

val result : t -> Merlin_parser.tree

val source : t -> Merlin_source.t
val compare : t -> t -> int

val is_normal  : t -> Merlin_parser.t option
val find_lexer : t -> Merlin_lexer.t option

val comments : t -> (string * Location.t) list
val errors : t -> exn list

val reconstruct_identifier: t -> Lexing.position -> string Location.loc list

val trace : t -> flag Widget.Nav.frame -> unit
val for_completion: t -> Lexing.position -> [`No_labels of bool] * t

val with_reader : t -> (unit -> 'a) -> 'a
val pprint : Extend_protocol.Reader.pretty_parsetree -> string
val oprint_list : Extend_protocol.Reader.outcometree list -> string list

val has_extend_support : string -> bool
