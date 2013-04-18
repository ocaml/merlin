type token = Chunk_parser.token History.loc

type item = {
  kind       : Outline_utils.kind;
  loc        : Location.t;
  tokens     : token list;
  exns       : exn list;
}

type sync = item History.sync
type t = item History.t

val item_loc : item -> Location.t
val location : t -> Location.t

val parse_step : ?bufpos:Lexing.position ref -> ?exns:exn list ->
  token History.t -> Lexing.lexbuf -> token History.t * item option

val parse : ?bufpos:Lexing.position ref ->
  token list -> t -> Lexing.lexbuf -> token list * t

val exns : t -> exn list
val append_exns : exn list -> t -> t
