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

(** {0 Outline parser}
 * Auxiliary definitions used by the outline parser *)

type offset = History.offset
type position = Lexing.position

(** Source code constructs are split into "chunks" of different "kinds". *)
type kind =
  | Enter_module (* module _ = struct *)
  | Leave_module (* end *)
  | Definition   (* let / type / … *)
  | Done         (* EOF found after a syntactically correct construction *)
  | Unterminated (* Unfinished syntactic construction *)
  | Syntax_error of Location.t

val kind_to_string : kind -> string

(** The outline parser proceeds by side-effects:
  * - most productions have no attached semantic action
  * - when the parser finds a syntactic construct that it knows how to chunk
  *   (above: module or definition), it raises the [Chunk] exception.
  * The associated position is the position of the last token of the
  * chunk, so that, if the parser consumed a lookahead token, it can
  * be recognized and added back to the lexing buffer.
  * EX: let a = 5 type t = ...
  *                  ^ |CHUNK: let a = 5|
  *   The parser raises [Chunk] only after [type], so we must add the
  *   [type] token back to the input stream.
  *)
exception Chunk of kind * position

(** Called to (re)initialize the parser. (nesting := 0) *)
val reset : unit -> unit

(** Used to ignore first-class modules.
  * The construct "let module = … in " allows to define a module
  * locally inside a definition, but our outline parser cannot work
  * inside a definition (it is either correct as a whole,
  * or incorrect).
  * After call to [enter_sub], such constructions are not reported
  * until a call to its dual [leave_sub] is made.
  *)
val enter_sub : unit -> unit
(** Decrements [nesting] *)
val leave_sub : unit -> unit
(** Sends [Chunk] only when outside of any enter_sub/leave_sub *)
val emit_top : kind -> position -> unit
