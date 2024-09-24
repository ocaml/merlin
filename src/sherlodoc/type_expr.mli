(* {{{ COPYING *(

     This file is part of Merlin, an helper for ocaml editors

     Copyright (C) 2013 - 2024  Xavier Van de Woestyne <xaviervdw(_)gmail.com>
                                Arthur Wendling <arthur(_)tarides.com>


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

(** A representation of internal types, with superfluous information removed to
    make it easier to compare them and calculate their distance. *)

(** Type variables are indexed by integers calculated according to their
    positions. For example, in the expression of type ['a -> 'b -> 'c],
    respectively ['a] will have the value [1], ['b] will have the value [2] and
    [’c] will have the value [3].

    This makes ['a -> 'b -> 'c] isomorphic to [’foo -> 'bar -> 'baz]. *)
type t =
  | Arrow of t * t
  | Tycon of string * t list
  | Tuple of t list
  | Tyvar of int
  | Wildcard
  | Unhandled

(** [normalize_type_parameters ty] replace string based type variables to
    integer based type variables. *)
val normalize_type_parameters : Type_parsed.t -> t

(** Try deserializing a string into a typed expression. *)
val from_string : string -> t option

(** Render a type to a string. *)
val to_string : t -> string

(** Equality between types *)
val equal : t -> t -> bool
