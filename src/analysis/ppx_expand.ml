let ppx_expansion ~ppx ~attr_start ~attr_end : Query_protocol.ppx_expand_result
    =
  { code = ppx; attr_start; attr_end }

let expand ~parsetree ~ppxed_parsetree ~pos =
  let check_at_pos loc = Location_aux.compare_pos pos loc = 0 in
  let atr = ref None in
  let expression = ref [] in
  let signature = ref [] in
  let structure = ref [] in
  let check_deriving_attr (attrs : Parsetree.attributes) =
    List.exists
      (fun (attr : Parsetree.attribute) ->
        atr := Some attr.attr_loc;
        attr.attr_name.txt = "deriving" && check_at_pos attr.attr_loc)
      attrs
  in
  let check_structures (item : Parsetree.structure_item_desc) =
    match item with
    | Pstr_type (_, ty) ->
        List.exists
          (fun (t : Parsetree.type_declaration) ->
            check_deriving_attr t.ptype_attributes)
          ty
    | Pstr_exception tc -> check_deriving_attr tc.ptyexn_attributes
    | Pstr_modtype mt -> check_deriving_attr mt.pmtd_attributes
    | Pstr_extension (_, attrs) -> check_deriving_attr attrs
    | _ -> false
  in
  let check_signatures (item : Parsetree.signature_item_desc) =
    match item with
    | Psig_type (_, ty) ->
        List.exists
          (fun (t : Parsetree.type_declaration) ->
            check_deriving_attr t.ptype_attributes)
          ty
    | Psig_exception tc -> check_deriving_attr tc.ptyexn_attributes
    | Psig_modtype mt -> check_deriving_attr mt.pmtd_attributes
    | Psig_extension (_, attrs) -> check_deriving_attr attrs
    | _ -> false
  in
  let check_extension_node (expression : Parsetree.expression) =
    match expression.pexp_desc with
    | Pexp_extension (loc, _) ->
        atr := Some expression.pexp_loc;
        check_at_pos loc.loc
    | _ -> false
  in
  let expr (self : Ast_iterator.iterator) (expr : Parsetree.expression) =
    match check_extension_node expr with
    | true -> (
        let expr (self : Ast_iterator.iterator) (exp : Parsetree.expression) =
          match exp.pexp_loc = expr.pexp_loc && check_at_pos exp.pexp_loc with
          | true -> expression := exp :: !expression
          | false -> Ast_iterator.default_iterator.expr self exp
        in
        let iterator = { Ast_iterator.default_iterator with expr } in
        match ppxed_parsetree with
        | `Interface si -> iterator.signature iterator si
        | `Implementation str -> iterator.structure iterator str)
    | false -> Ast_iterator.default_iterator.expr self expr
  in
  let signature_item (self : Ast_iterator.iterator)
      (item_1 : Parsetree.signature_item) =
    match check_signatures item_1.psig_desc with
    | true -> (
        let signature_item (self : Ast_iterator.iterator)
            (item_2 : Parsetree.signature_item) =
          match check_at_pos item_2.psig_loc && item_1 <> item_2 with
          | true -> signature := item_2 :: !signature
          | false -> Ast_iterator.default_iterator.signature_item self item_2
        in
        let iterator = { Ast_iterator.default_iterator with signature_item } in
        match ppxed_parsetree with
        | `Interface si -> iterator.signature iterator si
        | `Implementation str -> iterator.structure iterator str)
    | false -> Ast_iterator.default_iterator.signature_item self item_1
  in
  let structure_item (self : Ast_iterator.iterator)
      (item_1 : Parsetree.structure_item) =
    match check_structures item_1.pstr_desc with
    | true -> (
        let structure_item (self : Ast_iterator.iterator)
            (item_2 : Parsetree.structure_item) =
          match check_at_pos item_2.pstr_loc && item_1 <> item_2 with
          | true -> structure := item_2 :: !structure
          | false -> Ast_iterator.default_iterator.structure_item self item_2
        in
        let iterator = { Ast_iterator.default_iterator with structure_item } in
        match ppxed_parsetree with
        | `Interface si -> iterator.signature iterator si
        | `Implementation str -> iterator.structure iterator str)
    | false -> Ast_iterator.default_iterator.structure_item self item_1
  in
  let iterator =
    { Ast_iterator.default_iterator with signature_item; structure_item; expr }
  in
  let _ =
    match parsetree with
    | `Interface si -> iterator.signature iterator si
    | `Implementation str -> iterator.structure iterator str
  in
  match (!signature, !structure, !expression) with
  | [], [], [] -> None
  | signature, [], [] ->
      let exp =
        Pprintast.signature Format.str_formatter (List.rev signature);
        Format.flush_str_formatter ()
      in
      Some
        (ppx_expansion ~ppx:exp ~attr_start:(Option.get !atr).loc_start
           ~attr_end:(Option.get !atr).loc_end)
  | [], structure, [] ->
      let exp =
        Pprintast.structure Format.str_formatter (List.rev structure);
        Format.flush_str_formatter ()
      in
      Some
        (ppx_expansion ~ppx:exp ~attr_start:(Option.get !atr).loc_start
           ~attr_end:(Option.get !atr).loc_end)
  | [], [], expression ->
      let exp =
        List.iter
          (fun exp -> Pprintast.expression Format.str_formatter exp)
          (List.rev expression);
        Format.flush_str_formatter ()
      in
      Some
        (ppx_expansion ~ppx:exp ~attr_start:(Option.get !atr).loc_start
           ~attr_end:(Option.get !atr).loc_end)
  | _ -> None
