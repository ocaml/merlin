type kind =
  | Enter_module
  | Leave_module
  | Definition
  | Rollback
  | Done
  | Unterminated

exception Chunk of kind * Lexing.position

val nesting : int ref
val reset : unit -> unit

val enter : unit -> unit
val leave : unit -> unit
val emit_top : kind -> Lexing.position -> unit
