type ppx_kind =
  | Expr of Parsetree.expression
  | Sig_item of Parsetree.signature_item
  | Str_item of Parsetree.structure_item

let check_at_pos pos loc = Location_aux.compare_pos pos loc = 0

let check_extension_node pos (expression : Parsetree.expression) =
  match expression.pexp_desc with
  | Pexp_extension (loc, _) ->
    if check_at_pos pos loc.loc then Some expression.pexp_loc else None
  | _ -> None

let check_deriving_attr pos (attrs : Parsetree.attributes) =
  let found_attr =
    List.find_opt
      (fun (attribute : Parsetree.attribute) ->
        attribute.attr_name.txt = "deriving"
        && check_at_pos pos attribute.attr_loc)
      attrs
  in
  match found_attr with
  | Some attribute -> Some attribute.attr_loc
  | None -> None

let check_structures pos (item : Parsetree.structure_item_desc) =
  match item with
  | Pstr_type (_, ty) ->
    List.find_map
      (fun (t : Parsetree.type_declaration) ->
        check_deriving_attr pos t.ptype_attributes)
      ty
  | Pstr_exception tc -> check_deriving_attr pos tc.ptyexn_attributes
  | Pstr_modtype mt -> check_deriving_attr pos mt.pmtd_attributes
  | Pstr_typext tex -> check_deriving_attr pos tex.ptyext_attributes
  | _ -> None

let check_signatures pos (item : Parsetree.signature_item_desc) =
  match item with
  | Psig_type (_, ty) ->
    List.find_map
      (fun (t : Parsetree.type_declaration) ->
        check_deriving_attr pos t.ptype_attributes)
      ty
  | Psig_exception tc -> check_deriving_attr pos tc.ptyexn_attributes
  | Psig_modtype mt -> check_deriving_attr pos mt.pmtd_attributes
  | Psig_typext tex -> check_deriving_attr pos tex.ptyext_attributes
  | _ -> None

let check_extension ~parsetree ~pos =
  let kind = ref None in
  let expr (self : Ast_iterator.iterator) (expr : Parsetree.expression) =
    match check_extension_node pos expr with
    | Some ext_loc -> kind := Some (Expr expr, ext_loc)
    | None -> Ast_iterator.default_iterator.expr self expr
  in
  let signature_item (self : Ast_iterator.iterator)
      (original_sg : Parsetree.signature_item) =
    match check_signatures pos original_sg.psig_desc with
    | Some attr_loc -> kind := Some (Sig_item original_sg, attr_loc)
    | None -> Ast_iterator.default_iterator.signature_item self original_sg
  in
  let structure_item (self : Ast_iterator.iterator)
      (original_str : Parsetree.structure_item) =
    match check_structures pos original_str.pstr_desc with
    | Some attr_loc -> kind := Some (Str_item original_str, attr_loc)
    | None -> Ast_iterator.default_iterator.structure_item self original_str
  in
  let iterator =
    { Ast_iterator.default_iterator with signature_item; structure_item; expr }
  in
  let () =
    match parsetree with
    | `Interface si -> iterator.signature iterator si
    | `Implementation str -> iterator.structure iterator str
  in
  !kind

let get_ppxed_source ~ppxed_parsetree ~pos ppx_kind_with_attr :
    Query_protocol.ppxed_source =
  let expression = ref None in
  let signature = ref [] in
  let structure = ref [] in
  let () =
    match ppx_kind_with_attr with
    | Expr original_expr, _ -> (
      let expr (self : Ast_iterator.iterator) (new_expr : Parsetree.expression)
          =
        match
          Location_aux.included ~into:original_expr.pexp_loc new_expr.pexp_loc
        with
        | true -> expression := Some new_expr
        | false -> Ast_iterator.default_iterator.expr self new_expr
      in
      let iterator = { Ast_iterator.default_iterator with expr } in
      match ppxed_parsetree with
      | `Interface si -> iterator.signature iterator si
      | `Implementation str -> iterator.structure iterator str)
    | Sig_item original_sg, _ -> (
      let signature_item (self : Ast_iterator.iterator)
          (new_sg : Parsetree.signature_item) =
        let included =
          Location_aux.included new_sg.psig_loc ~into:original_sg.psig_loc
        in
        match
          (included && original_sg <> new_sg, new_sg.psig_loc.loc_ghost)
        with
        | true, _ -> signature := new_sg :: !signature
        | false, false ->
          Ast_iterator.default_iterator.signature_item self new_sg
        | false, true -> () (* We don't enter nested ppxes *)
      in
      let iterator = { Ast_iterator.default_iterator with signature_item } in
      match ppxed_parsetree with
      | `Interface si -> iterator.signature iterator si
      | `Implementation str -> iterator.structure iterator str)
    | Str_item original_str, _ -> (
      let structure_item (self : Ast_iterator.iterator)
          (new_str : Parsetree.structure_item) =
        let included =
          Location_aux.included new_str.pstr_loc ~into:original_str.pstr_loc
        in
        match (included, new_str.pstr_loc.loc_ghost) with
        | true, _ -> (
          match check_structures pos new_str.pstr_desc with
          | None -> structure := new_str :: !structure
          | Some _ -> ())
        | false, false ->
          Ast_iterator.default_iterator.structure_item self new_str
        | false, true -> ()
      in
      let iterator = { Ast_iterator.default_iterator with structure_item } in
      match ppxed_parsetree with
      | `Interface si -> iterator.signature iterator si
      | `Implementation str -> iterator.structure iterator str)
  in
  match (ppx_kind_with_attr : ppx_kind * Warnings.loc) with
  | Expr _, ext_loc ->
    { code = Pprintast.string_of_expression (Option.get !expression);
      attr_start = ext_loc.loc_start;
      attr_end = ext_loc.loc_end
    }
  | Sig_item _, attr_loc ->
    let exp =
      Pprintast.signature Format.str_formatter (List.rev !signature);
      Format.flush_str_formatter ()
    in
    { code = exp; attr_start = attr_loc.loc_start; attr_end = attr_loc.loc_end }
  | Str_item _, attr_loc ->
    let exp =
      Pprintast.structure Format.str_formatter (List.rev !structure);
      Format.flush_str_formatter ()
    in
    { code = exp; attr_start = attr_loc.loc_start; attr_end = attr_loc.loc_end }
