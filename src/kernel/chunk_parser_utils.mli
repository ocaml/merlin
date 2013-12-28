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

type token = Raw_parser.token
(* String representation of [token]s, for debug purpose *)
val token_to_string : token -> string
(* Wrap lexing function to optionally dump [token] stream, for debug purpose *)
val dump_lexer : ?who:string -> ('a -> token) -> 'a -> token

(* Heuristic applied before submitting [token] stream containing syntax errors
 * to [Raw_parser].
 * When the [Outline_parser] encounters an error, parsing is interrupted and
 * [re_sync] is called. [re_sync] consumes tokens until it find a potential
 * point for resuming parser (generally at the beginning of a definition).
 *
 * [re_sync] returns the number of phrases the [Outline_parser] must succeed
 * to parse for the resynchronization to be considered successful.
 * (0 for EOF or explicit synchronization, 1 or more when it's only
 * approximative) *)
val re_sync : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> int
