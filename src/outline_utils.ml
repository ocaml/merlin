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

type offset = History.offset
type position = Lexing.position

type kind =
  | Enter_module
  | Leave_module
  | Definition
  | Done
  | Unterminated
  | Syntax_error of Location.t

exception Chunk of kind * position

let kind_to_string = function
  | Enter_module   -> "Enter_module"
  | Leave_module   -> "Leave_module"
  | Definition     -> "Definition"
  | Done           -> "Done"
  | Unterminated   -> "Unterminated"
  | Syntax_error _ -> "Syntax_error"

(** Used to ignore first-class modules.
  * The construct "let module = … in " allows to define a module
  * locally inside a definition, but our outline parser cannot work
  * inside a definition (it is either correct as a whole,
  * or incorrect).
  * [nesting] is incremented at the beginning of such constructions
  * (here, [let module]) and decremented at its end (here, after the
  * module expression is parsed).No module definition is reported
  * while [!nesting > 0].
  *)
let nesting = ref 0

let reset () = nesting := 0

let enter_sub () =
  incr nesting

let leave_sub () =
  (if !nesting <= 0
   then failwith "Outline_utils.leave: invalid nesting");
  decr nesting

let emit_top c pos =
  (*prerr_endline "emit";*)
  if !nesting = 0 then
    raise (Chunk (c,pos))
