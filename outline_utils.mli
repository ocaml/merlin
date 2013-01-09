type kind =
  | Enter_module
  | Leave_module
  | Definition
  | Rollback
  | Done
  | Unterminated

exception Chunk of kind * Lexing.position

val filter_first : int ref
val nesting : int ref
val reset : rollback:int -> unit -> unit

val enter : unit -> unit
val leave : unit -> unit
val emit_top : kind -> Lexing.position -> unit
