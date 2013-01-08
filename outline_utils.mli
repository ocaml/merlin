(*type context_delimiter =
  | End
  | Rparen
  | Rbracket
  | Rbracketbar
  | Rbrace
  | Rbracegreater
  | In_value*)

type chunk =
  | Enter_module
  | Leave_module
  | Definition
  | Rollback
  | Done
  | Unterminated
(* | Partial_expr of context_delimiter list
   | Partial_module of context_delimiter list*)

exception Chunk of chunk * Lexing.position

val nesting : int ref
val reset : unit -> unit

val enter : unit -> unit
val leave : unit -> unit
val emit_top : chunk -> Lexing.position -> unit
