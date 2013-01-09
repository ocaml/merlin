module Utils : module type of Outline_utils

module Raw :
sig
  type token = Chunk_parser.token
  type item = token History.loc  
  type sync = item History.sync
  type t = item History.t 

  val token_to_string : token -> string
end

val parse_with : 't History.loc History.t ->
  parser:((Lexing.lexbuf -> 't) -> Lexing.lexbuf -> unit) ->
    lexer:(Lexing.lexbuf -> 't) ->
      Lexing.lexbuf -> 
  't History.loc History.t * Outline_utils.kind * 't History.loc list

module Chunked :
sig
  type item = Raw.sync * Outline_utils.kind * Raw.item list
  type sync = item History.sync
  type t = item History.t 
end

val parse_step : Raw.t -> Lexing.lexbuf ->
  Raw.t * Chunked.item

val parse : Raw.t * Chunked.t ->
  Lexing.lexbuf -> Raw.t * Chunked.t
