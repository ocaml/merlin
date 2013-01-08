type item =
  | Root
  | Definition of Parsetree.structure_item * item
  | Module_opening of Location.t * string Location.loc * Parsetree.module_expr * item

type token = Chunk_parser.token History.loc
type t = (Outline_utils.chunk * token list) History.sync * item

exception Malformed_module
exception Invalid_chunk

val empty : item
val append_step : Outline_utils.chunk -> token list -> item -> item

val append : (Outline_utils.chunk * token list) History.t -> t History.t -> t History.t
