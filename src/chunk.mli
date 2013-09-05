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

type item_desc =
  | Definitions of Parsetree.structure_item Location.loc list
  | Module_opening of Location.t * string Location.loc * Parsetree.module_expr
  (* When a module is closed, you have to rewind some number
   * of definitions in the history (those of the module and
   * its submodules); the offset indicates the last definition before
   * the corresponding Module_opening.
   *)
  | Module_closing of Parsetree.structure_item Location.loc * History.offset

and step = (Outline_utils.kind, item_desc) Misc.sum
and item = Outline.sync * (exn list * step) * (string * Location.t) list
and sync = item History.sync
and t = item History.t

exception Malformed_module of Location.t
exception Invalid_chunk

val sync_step : Outline_utils.kind -> Outline.token list -> t -> step
val sync : Outline.t -> t -> t
val exns : t -> exn list

val dump_chunk : t -> (string * int) list
