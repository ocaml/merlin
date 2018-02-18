(** {0 Merlin representation of a textual source code}

    It bundles filename and a content, and offers functions for computing
    positions in the source.
*)
type t

(** Making a content from name and contents. *)
val make : Trace.t -> string -> t

(** {1 Position management} *)

type position = [
  | `Start
  | `Offset of int
  | `Logical of int * int
  | `End
]

val get_offset     : Trace.t -> t -> [< position] -> [> `Offset of int]

val get_logical    : Trace.t -> t -> [< position] -> [> `Logical of int * int]

val get_lexing_pos : Trace.t -> t -> filename:string -> [< position] -> Lexing.position

(** {1 Managing content} *)

(** Updating content *)
val substitute : Trace.t -> t -> [< position] -> [< position | `Length of int] -> string -> t

(** Source code of the file *)
val text : t -> string

val dump : t -> Std.json

val print_position : unit -> [< position] -> string
