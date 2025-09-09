open Std

module Type_tree = struct
  type node_data =
    | Arrow
    | Tuple
    | Poly_variant
    | Object
    | Type_ref of { path : Path.t; ty : Types.type_expr }

  type t = { data : node_data; children : t list }
end

let rec flatten_arrow ret_ty =
  match Types.get_desc ret_ty with
  | Tarrow (_, ty1, ty2, _) -> ty1 :: flatten_arrow ty2
  | _ -> [ ret_ty ]

let rec create_type_tree ty : Type_tree.t option =
  match Types.get_desc ty with
  | Tarrow (_, ty1, ty2, _) ->
    let tys = ty1 :: flatten_arrow ty2 in
    let children = List.filter_map tys ~f:create_type_tree in
    Some { data = Arrow; children }
  | Ttuple tys ->
    let children = List.filter_map tys ~f:create_type_tree in
    Some { data = Tuple; children }
  | Tconstr (path, arg_tys, abbrev_memo) ->
    let ty_without_args =
      Btype.newty2 ~level:Ident.highest_scope (Tconstr (path, [], abbrev_memo))
    in
    let children = List.filter_map arg_tys ~f:create_type_tree in
    Some { data = Type_ref { path; ty = ty_without_args }; children }
  | Tlink ty | Tpoly (ty, _) -> create_type_tree ty
  | Tobject (fields_type, _) ->
    let rec extract_field_types (ty : Types.type_expr) =
      match Types.get_desc ty with
      | Tfield (_, _, ty, rest) -> ty :: extract_field_types rest
      | _ -> []
    in
    let field_types = List.rev (extract_field_types fields_type) in
    let children = List.filter_map field_types ~f:create_type_tree in
    Some { data = Object; children }
  | Tvariant row_desc ->
    let fields = Types.row_fields row_desc in
    let children =
      List.filter_map fields ~f:(fun (_, row_field) ->
          match Types.row_field_repr row_field with
          | Rpresent (Some ty) -> create_type_tree ty
          | Reither (_, tys, _) ->
            (* If there are multiple types in [tys], they are types that are meant to
               unify with each other (it'd be a type error if not, see
               [Ctype.collapse_conj]). So just using the head of the list seems fine
               (using the entire list results in types being duplicated). *)
            List.hd_opt tys |> Option.bind ~f:create_type_tree
          | Rpresent None | Rabsent -> None)
    in
    Some { data = Poly_variant; children }
  | Tnil | Tvar _ | Tsubst _ | Tunivar _ | Tpackage _ | Tfield _ -> None
