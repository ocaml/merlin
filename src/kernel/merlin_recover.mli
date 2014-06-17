type t

val fresh : Merlin_parser.t -> t

val fold : Merlin_lexer.item -> t -> t

val parser : t -> Merlin_parser.t

val exns : t -> exn list

val dump : Format.formatter -> t ->  unit
val dump_recoverable : Format.formatter -> t -> unit
