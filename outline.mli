type token = Chunk_parser.token History.loc

type item = int * Outline_utils.kind * token list * exn list
type sync = item History.sync
type t = item History.t 

val last_position : t -> Lexing.position option
val seek_line : int * int -> t -> t
val seek_offset : int -> t -> t

val parse_step : ?rollback:int -> ?bufpos:Lexing.position ref -> ?exns:exn list -> goteof:bool ref ->
  token History.t -> Lexing.lexbuf -> token History.t * item option

val parse : ?rollback:int -> ?bufpos:Lexing.position ref -> goteof:bool ref ->
  token History.t -> t -> Lexing.lexbuf -> token History.t * t

