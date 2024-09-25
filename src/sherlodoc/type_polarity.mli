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

(** Describes the polarity sign of a type [negative] for contravariant
    parameters and [positive] for covariant parameters (the return of the
    function). *)

type t

val positive : t
val negative : t

(** [negate x] returns [positive] if [x] is [negative] and [negative] if [x] is
    [positive]. *)
val negate : t -> t

(** Equality between polarity sign. *)
val equal : t -> t -> bool

(** A comparison that act that [negative < positive]. *)
val compare : t -> t -> int

(** Simple printer for polarity sign. *)
val to_string : t -> string
