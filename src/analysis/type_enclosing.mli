val from_nodes
  : (Env.t * Browse_raw.node * Query_protocol.is_tail_position) list
  -> (Location.t *
      [> `Modtype of Env.t * Types.module_type
      | `Type of Env.t * Types.type_expr
      | `Type_decl of Env.t * Ident.t * Types.type_declaration ] *
      Query_protocol.is_tail_position
     ) list

val from_reconstructed
  : int -> string Location.loc list -> Env.t -> Mbrowse.node
  -> (Location.t *
      [> `String of string | `Type of Env.t * Types.type_expr ] *
      Query_protocol.is_tail_position
     ) list
