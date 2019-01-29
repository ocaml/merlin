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

(** {0 Merlin representation of a textual source code}

    It bundles filename and a content, and offers functions for computing
    positions in the source.
*)
type t

(** Making a content from name and contents. *)
val make : string -> t

(** {1 Position management} *)

type position = [
  | `Start
  | `Offset of int
  | `Logical of int * int
  | `End
]

val get_offset     : t -> [< position] -> [> `Offset of int]

val get_logical    : t -> [< position] -> [> `Logical of int * int]

val get_lexing_pos : t -> filename:string -> [< position] -> Lexing.position

(** {1 Managing content} *)

(** Updating content *)
val substitute : t -> [< position] -> [< position | `Length of int] -> string -> t

(** Source code of the file *)
val text : t -> string

val dump : t -> Std.json

val print_position : unit -> [< position] -> string
