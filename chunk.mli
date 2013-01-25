type item_desc =
  | Root
  | Definition of Parsetree.structure_item Location.loc * item_desc
  | Module_opening of Location.t * string Location.loc * Parsetree.module_expr * item_desc
  | Module_closing of Parsetree.structure_item Location.loc * item_desc

type item = Outline.sync * item_desc
type sync = item History.sync
type t = item History.t

exception Malformed_module
exception Invalid_chunk

val empty : item_desc
val sync_step : Outline_utils.kind -> Outline.token list -> item_desc -> item_desc

val sync : Outline.t -> t -> t
val item : t -> item_desc

val dump_chunk : item_desc -> (string * int) list
