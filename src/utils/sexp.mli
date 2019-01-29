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

open Std

type t =
    Cons of t * t
  | Sym of string
  | String of string
  | Int of int
  | Float of float

val nil : t
val of_list : t list -> t

val tell_sexp : (string -> unit) -> t -> unit
val tell_cons : (string -> unit) -> t -> unit

val to_buf : t -> Buffer.t -> unit

val to_string : t -> string

val of_string : string -> t

val of_file_descr :
  ?on_read:(Unix.file_descr -> unit) -> Unix.file_descr -> unit -> t option
val of_channel :
  ?on_read:(Unix.file_descr -> unit) -> in_channel -> unit -> t option

val of_json : json -> t
val to_json : t -> json
