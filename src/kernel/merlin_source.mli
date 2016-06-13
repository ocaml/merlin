(* Merlin representation of a textual source code *)

type t

val empty : filename:string -> t

val compare : t -> t -> int

(* Position management *)

type position = [
  | `Start
  | `Offset of int
  | `Logical of int * int
  | `End
]

val get_offset     : t -> [< position] -> [> `Offset of int]
val get_logical    : t -> [< position] -> [> `Logical of int * int]
val get_lexing_pos : t -> [< position] -> Lexing.position

(* Updating content *)
val substitute : t -> [< position] -> [< position | `Length of int] -> string -> t

(* Accessing content *)

val filename : t -> string
val unitname : t -> string
val text : t -> string
