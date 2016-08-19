(** {0 Merlin representation of a textual source code}

    It bundles filename and a content, and offers functions for computing
    positions in the source.
*)
type t

(** Making a content from name and contents. *)
val make : filename:string -> text:string -> t

val dump : t -> Std.json

(** {1 Position management} *)

type position = [
  | `Start
  | `Offset of int
  | `Logical of int * int
  | `End
]

val get_offset     : t -> [< position] -> [> `Offset of int]

val get_logical    : t -> [< position] -> [> `Logical of int * int]

val get_lexing_pos : t -> [< position] -> Lexing.position

(** Updating content *)
val substitute : t -> [< position] -> [< position | `Length of int] -> string -> t

(** {1 Accessing contents} *)

(** Raw filename *)
val filename : t -> string

(** Unit (ML module) name *)
val unitname : t -> string

(** Source code of the file *)
val text : t -> string
