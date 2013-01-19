type kind =
  | Enter_module
  | Leave_module
  | Definition
  | Rollback
  | Done
  | Unterminated
  | Exception of exn
 
exception Chunk of kind * Lexing.position

val filter_first : int ref
val nesting : int ref
val reset : rollback:int -> unit -> unit

val enter : unit -> unit
val leave : unit -> unit
val emit_top : kind -> Lexing.position -> unit

val pos_to_json : Lexing.position -> Json.json
val pos_of_json : Json.json -> [ `Line of (int * int) ]

val ppf_to_string : unit -> Format.formatter * (unit -> string)
