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

open Std

type 'a binding = string Location.loc * 'a Location.loc

type step_state = {
  global_exns: exn list;
  exns: exn list;
  modules: string Location.loc list; (* Local modules *)
}

module Context : sig
  type state = step_state

  type sig_item = Parsetree.signature_item Location.loc list or_exn
  type str_item = Parsetree.structure_item Location.loc list or_exn
  type sig_in_sig_modtype = Parsetree.modtype_declaration binding or_exn
  type sig_in_sig_module  = Parsetree.module_type binding or_exn
  type sig_in_str_modtype = Parsetree.module_type binding or_exn
  type str_in_module      = Parsetree.module_expr binding or_exn
end
module Spine : Spine.S with module Context = Context
type t = Spine.t
val update : Outline.t -> t option -> t

val exns : t -> exn list
val local_modules : t -> string Location.loc list
