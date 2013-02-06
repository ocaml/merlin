type item_desc =
  | Definitions of Parsetree.structure_item Location.loc list
  | Module_opening of Location.t * string Location.loc * Parsetree.module_expr
    (* Quand un module est refermé, il faut remonter en certain nombre de
     * définitions dans l'historique (celle du module et des sous modules) :
     * l'offset indique la dernière définition avant le Module_opening
     * correspondant *)
  | Module_closing of Parsetree.structure_item Location.loc * History.offset

and item = Outline.sync * (item_desc, exn) Misc.sum
and sync = item History.sync
and t = item History.t

exception Malformed_module of Location.t
exception Invalid_chunk

val sync_step : Outline_utils.kind -> Outline.token list -> t -> item_desc option
val sync : Outline.t -> t -> t

val dump_chunk : t -> (string * int) list
