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

type t

val create: ?dot_merlins:string list -> ?path:string -> Merlin_parser.state -> t

val unit_name : t -> string
val project: t -> Merlin_project.t

val lexer: t -> (exn list * Merlin_lexer.item) History.t
val update: t -> (exn list * Merlin_lexer.item) History.t -> [`Nothing_done | `Updated]
val start_lexing: ?pos:Lexing.position -> t -> Merlin_lexer.t
val lexer_errors: t -> exn list

val comments: t -> (string * Location.t) list

val parser: t -> Merlin_parser.t
val parser_errors: t -> exn list

val recover: t -> Merlin_recover.t
val recover_history : t -> (Merlin_lexer.item * Merlin_recover.t) History.t

val typer: t -> Merlin_typer.t

val get_mark: t -> Merlin_parser.frame option
val has_mark: t -> Merlin_parser.frame option -> bool

val is_implementation : t -> bool

(* All top modules of current project, with current module removed *)
val global_modules: t -> string list

(* Try to do a background job, return false if nothing has to be done *)
val idle_job : t -> bool
