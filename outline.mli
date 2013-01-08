type token = Chunk_parser.token 
type 't result = 't History.t * Outline_utils.chunk * 't History.t

val token_to_string : token -> string

val parse_with : 't History.t ->
  parser:((Lexing.lexbuf -> 't) -> Lexing.lexbuf -> unit) ->
    lexer:(Lexing.lexbuf -> 't) ->
     Lexing.lexbuf -> 't result

val parse : token History.t -> Lexing.lexbuf -> token result
