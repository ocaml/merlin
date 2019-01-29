(* {{{ COPYING *(

   This file is part of Merlin, an helper for ocaml editors

   Copyright (C) 2013 - 2019  Merlin contributors

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

module Namespace : sig
  type t = [
    | `Vals
    | `Type
    | `Constr
    | `Mod
    | `Modtype
    | `Functor
    | `Labels
    | `Unknown
    | `Apply
  ]

  val to_string : t -> string
end

module Id : sig
  type t = private
    | Id of Ident.t
    | String of string

  val name : t -> string
end

type t (* = private elt list *)
and elt = private
  | Ident of Id.t * Namespace.t
  | Applied_to of t

val to_string : t -> string
val to_unique_string : t -> string

val head : t -> elt option
val head_exn : t -> elt

val peal_head : t -> t option
val peal_head_exn : t -> t

val equal : t -> t -> bool

val rewrite_head : new_prefix:t -> t -> t

val strip_stamps : t -> t

val of_path : namespace:Namespace.t -> Path.t -> t

val empty : t

val subst_prefix : old_prefix:t -> new_prefix:t -> t -> t option
