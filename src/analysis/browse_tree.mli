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

type t =
  { t_node : Mbrowse.node;
    t_loc : Location.t;
    t_env : Env.t;
    t_children : t list lazy_t
  }

val default_loc : Location.t
val default_env : Env.t

(** [of_node ?loc ?env node] produces a tree from [node], using [loc] and [env]
  * as default annotation when nothing can be inferred from the [node].
  * If they are not specified, annotations from child are used for approximation.
  *)
val of_node : ?env:Env.t -> Mbrowse.node -> t

val of_browse : Mbrowse.t -> t

val dummy : t

val all_occurrences : Path.t -> t -> (t * Path.t Location.loc list) list
val all_constructor_occurrences :
  t
  * [ `Description of Types.constructor_description
    | `Declaration of Typedtree.constructor_declaration
    | `Extension_constructor of Typedtree.extension_constructor ] ->
  t ->
  t Location.loc list

val all_occurrences_of_prefix :
  Path.t -> Browse_raw.node -> (Path.t Location.loc * Longident.t) list
