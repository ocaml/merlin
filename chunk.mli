type item_desc =
  | Definition of Parsetree.structure_item Location.loc
  | Module_opening of Location.t * string Location.loc * Parsetree.module_expr

type item = Outline.sync * item_desc
type sync = item History.sync
type t = item History.t

exception Malformed_module
exception Invalid_chunk

val sync_step : Outline_utils.kind -> Outline.token list -> Outline.sync -> t -> t

val sync : Outline.t -> t -> t

val dump_chunk : item_desc list -> (string * int) list
