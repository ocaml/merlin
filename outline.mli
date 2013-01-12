module Raw :
sig
  type token = Chunk_parser.token
  type item = token History.loc  
  type sync = item History.sync
  type t = item History.t 
end

module Chunked :
sig
  type item = Raw.sync * (int * Outline_utils.kind * Raw.item list * exn list)
  type sync = item History.sync
  type t = item History.t 

  val last_position : t -> Lexing.position option
  val seek_line : int * int -> t -> t
  val seek_offset : int -> t -> t
end

val parse_step : ?rollback:int -> ?bufpos:Lexing.position ref -> ?exns:exn list -> goteof:bool ref ->
  Raw.t -> Lexing.lexbuf -> Raw.t * Chunked.item option

val parse : ?rollback:int -> ?bufpos:Lexing.position ref -> goteof:bool ref ->
  Raw.t * Chunked.t -> Lexing.lexbuf -> Raw.t * Chunked.t

