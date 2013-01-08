val token_to_string : Chunk_parser.token -> string

val parse_with : 't History.loc History.t ->
  parser:((Lexing.lexbuf -> 't) -> Lexing.lexbuf -> unit) ->
    lexer:(Lexing.lexbuf -> 't) ->
      Lexing.lexbuf -> 
  't History.loc History.t * Outline_utils.chunk * 't History.loc list

type token = Chunk_parser.token History.loc  

val parse_step : token History.t -> Lexing.lexbuf ->
  token History.t * Outline_utils.chunk * token list

val parse : token History.t * (Outline_utils.chunk * token list) History.t ->
  Lexing.lexbuf -> token History.t * (Outline_utils.chunk * token list) History.t
