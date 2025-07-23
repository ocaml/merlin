module Type_tree : sig
  type node_data =
    | Arrow
    | Tuple
    | Poly_variant
    | Object
    | Type_ref of { path : Path.t; ty : Types.type_expr }

  type t = { data : node_data; children : t list }
end

(** Convert a type into a simplified tree representation. *)
val create_type_tree : Types.type_expr -> Type_tree.t option
