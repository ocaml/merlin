type t =
  | Root
  | Definition of Parsetree.structure_item * t
  | Module_opening of Location.t * string Location.loc * Parsetree.module_expr * t

exception Malformed_module
exception Invalid_chunk

val empty : t
val append : Outline_utils.chunk -> Chunk_parser.token History.t -> t -> t

val append_history : Outline_utils.chunk -> Chunk_parser.token History.t -> t History.t -> t History.t 
