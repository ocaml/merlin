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

(** Utilities for calculating distances between names. *)

(** [distance ?cutoff a b] returns the
    {{:https://en.wikipedia.org/wiki/Damerau%E2%80%93Levenshtein_distance}
      Damerau-Levenshtein} between [a] and [b]. *)
val distance : ?cutoff:int -> string -> string -> int option

(** [distance_of_substring ?cutoff a b] compute the distance by extracting
    relevant substring from [b] *)
val distance_of_substring : ?cutoff:int -> string -> string -> int option

(** [best_distance ?cutoff words entry] compute the best distance of a list of
    string according to a given string. *)
val best_distance : ?cutoff:int -> string list -> string -> int
