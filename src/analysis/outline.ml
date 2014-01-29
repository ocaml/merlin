open Std

let rec summarize node =
  let open Typedtree in
  let pos = node.str_loc.Location.loc_start in
  match node.str_desc with
  | Tstr_module (id, _, mod_expr) ->
    let name = Ident.name id in
    let children = traverse_mod_expr mod_expr in
    [{ Protocol. name ; pos ; children ; kind = `Module }]
  | Tstr_value (_, bindings) ->
    let children = [] in
    List.filter_map bindings ~f:(fun ({ pat_desc ; _ }, _) ->
      match pat_desc with
      | Tpat_var (id, loc) ->
        let pos = loc.Location.loc.Location.loc_start in
        Some { Protocol. name = Ident.name id ; kind = `Value ; pos ; children }
      | _ -> (* TODO: handle more cases *)
        None
    )
  | Tstr_type definitions ->
    List.map definitions ~f:(fun (id, loc, type_decl) ->
      let pos = loc.Location.loc.Location.loc_start in
      let children = traverse_type_decl type_decl in
      { Protocol. name = Ident.name id ; kind = `Type ; pos ; children }
    )
  | _ ->
    []

and traverse_mod_expr { Typedtree. mod_desc ; _ } =
  match mod_desc with
  | Typedtree.Tmod_structure str -> handle_struct str
  | Typedtree.Tmod_functor (_param, _param_loc, _, me) ->
    (* Should we add [_param] to the outline too? *)
    traverse_mod_expr me
  | Typedtree.Tmod_constraint (me, _, _, _) -> traverse_mod_expr me
  | _ -> []

and traverse_type_decl t_decl =
  match t_decl.Typedtree.typ_kind with
  | Typedtree.Ttype_abstract -> []
    (* TODO: add support for polymorphic variants and objects using the manifest *)
  | Typedtree.Ttype_variant cstrs ->
    let children = [] in
    List.map cstrs ~f:(fun (id, loc, _, _) ->
      let pos = loc.Location.loc.Location.loc_start in
      { Protocol. name = Ident.name id ; kind = `Constructor ; pos ; children }
    )
  | Typedtree.Ttype_record fields ->
    List.map fields ~f:(fun (id, loc, _, _, _) ->
      let pos = loc.Location.loc.Location.loc_start in
      { Protocol. name = Ident.name id ; kind = `Label ; pos ; children = [] }
    )

and handle_struct str =
  List.concat_map str.Typedtree.str_items ~f:summarize

let get structures = List.concat (List.rev_map structures ~f:handle_struct)
