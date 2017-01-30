(** {0 Merlin representation of a textual source code}

    It bundles filename and a content, and offers functions for computing
    positions in the source.
*)
type t

(** Making a content from name and contents. *)
val make : Trace.t -> Mconfig.t -> string -> t

(** {1 Position management} *)

type position = [
  | `Start
  | `Offset of int
  | `Logical of int * int
  | `End
]

val get_offset     : Trace.t -> t -> [< position] -> [> `Offset of int]

val get_logical    : Trace.t -> t -> [< position] -> [> `Logical of int * int]

val get_lexing_pos : Trace.t -> t -> [< position] -> Lexing.position

(** Updating content *)
val substitute : Trace.t -> t -> [< position] -> [< position | `Length of int] -> string -> t

(** {1 Accessing contents} *)

(** Raw filename *)
val filename : t -> string

(** Unit (ML module) name *)
val unitname : t -> string

(** Source code of the file *)
val text : t -> string

(** {1 Dump} *)

val dump : t -> Std.json

val print_position : unit -> [< position] -> string
