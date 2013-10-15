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

(* Maintains a typing environment synchronized with a chunk history *)

open Std

module Context : sig
  type state = exn list * Env.t * Typedtree.structure Location.loc list * Btype.snapshot option

  type sig_item = Types.signature Location.loc list or_exn
  type str_item = Typedtree.structure Location.loc list or_exn
  type sig_in_sig_modtype = unit
  type sig_in_sig_module  = unit
  type sig_in_str_modtype = unit
  type str_in_module      = unit
end
module Spine : Spine.S with module Context := Context
type t = Spine.t
val update : Chunk.t -> t option -> t

val exns : t -> exn list
val env : t -> Env.t
val trees : t -> Typedtree.structure Location.loc list
val snapshot : t -> Btype.snapshot option
