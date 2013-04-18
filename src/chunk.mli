type item_desc =
  | Definitions of Parsetree.structure_item Location.loc list
  | Module_opening of Location.t * string Location.loc * Parsetree.module_expr
  (* When a module is closed, you have to rewind some number
   * of definitions in the history (those of the module and
   * its submodules); the offset indicates the last definition before
   * the corresponding Module_opening.
   *)
  | Module_closing of Parsetree.structure_item Location.loc * History.offset

and step = (Outline_utils.kind, item_desc) Misc.sum
and item = Outline.sync * (exn list * step)
and sync = item History.sync
and t = item History.t

exception Malformed_module of Location.t
exception Invalid_chunk

val sync_step : Outline_utils.kind -> Outline.token list -> t -> step
val sync : Outline.t -> t -> t
val exns : t -> exn list

val dump_chunk : t -> (string * int) list
