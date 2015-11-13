(* Merlin representation of a textual source code *)

type t

val empty : name:string -> t

(* Position management *)

type position = [
  | `Offset of int
  | `Logical of int * int
]

val get_offset       : t -> [< position | `End] -> [> `Offset of int]
val get_logical      : t -> [< position | `End] -> [> `Logical of int * int]

(* Accessing content *)

val name : t -> string
val text : t -> string
val substitute : t -> [< position] -> [< position | `Length of int | `End] -> string -> t
