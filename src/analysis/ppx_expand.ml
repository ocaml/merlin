type ppx_expand_result = Query_protocol.ppx_expand_result option

type ppx_kind =
  | Expr of Parsetree.expression
  | Sig_item of Parsetree.signature_item
  | Str_item of Parsetree.structure_item

let attr = ref None
let expression = ref []
let signature = ref []
let structure = ref []
let kind = ref None
let check_at_pos pos loc = Location_aux.compare_pos pos loc = 0

let check_extension_node pos (expression : Parsetree.expression) =
  match expression.pexp_desc with
  | Pexp_extension (loc, _) ->
      attr := Some expression.pexp_loc;
      check_at_pos pos loc.loc
  | _ -> false

let check_deriving_attr pos (attrs : Parsetree.attributes) =
  List.exists
    (fun (attribute : Parsetree.attribute) ->
      attr := Some attribute.attr_loc;
      attribute.attr_name.txt = "deriving"
      && check_at_pos pos attribute.attr_loc)
    attrs

let check_structures pos (item : Parsetree.structure_item_desc) =
  match item with
  | Pstr_type (_, ty) ->
      List.exists
        (fun (t : Parsetree.type_declaration) ->
          check_deriving_attr pos t.ptype_attributes)
        ty
  | Pstr_exception tc -> check_deriving_attr pos tc.ptyexn_attributes
  | Pstr_modtype mt -> check_deriving_attr pos mt.pmtd_attributes
  | Pstr_extension (_, attrs) -> check_deriving_attr pos attrs
  | _ -> false

let check_signatures pos (item : Parsetree.signature_item_desc) =
  match item with
  | Psig_type (_, ty) ->
      List.exists
        (fun (t : Parsetree.type_declaration) ->
          check_deriving_attr pos t.ptype_attributes)
        ty
  | Psig_exception tc -> check_deriving_attr pos tc.ptyexn_attributes
  | Psig_modtype mt -> check_deriving_attr pos mt.pmtd_attributes
  | Psig_extension (_, attrs) -> check_deriving_attr pos attrs
  | _ -> false

let get_ppx_items ppxed_parsetree pos ppx_kind =
  match ppx_kind with
  | Expr original_expr -> (
      let expr (self : Ast_iterator.iterator) (new_expr : Parsetree.expression)
          =
        match
          original_expr.pexp_loc.loc_start.pos_cnum
          <= new_expr.pexp_loc.loc_start.pos_cnum
          && new_expr.pexp_loc.loc_end.pos_cnum
             <= original_expr.pexp_loc.loc_end.pos_cnum
        with
        | true -> 
          kind := Some (Expr new_expr);
          expression := new_expr :: !expression
        | false -> Ast_iterator.default_iterator.expr self new_expr
      in
      let iterator = { Ast_iterator.default_iterator with expr } in
      match ppxed_parsetree with
      | `Interface si -> iterator.signature iterator si
      | `Implementation str -> iterator.structure iterator str)
  | Sig_item original_sg -> (
      let signature_item (self : Ast_iterator.iterator)
          (new_sg : Parsetree.signature_item) =
        match check_at_pos pos new_sg.psig_loc && original_sg <> new_sg with
        | true -> 
          kind := Some (Sig_item new_sg);
          signature := new_sg :: !signature
        | false -> Ast_iterator.default_iterator.signature_item self new_sg
      in
      let iterator = { Ast_iterator.default_iterator with signature_item } in
      match ppxed_parsetree with
      | `Interface si -> iterator.signature iterator si
      | `Implementation str -> iterator.structure iterator str)
  | Str_item original_str -> (
      let structure_item (self : Ast_iterator.iterator)
          (new_str : Parsetree.structure_item) =
        match check_at_pos pos new_str.pstr_loc && original_str <> new_str with
        | true -> 
          kind := Some (Str_item new_str);
          structure := new_str :: !structure
        | false -> Ast_iterator.default_iterator.structure_item self new_str
      in
      let iterator = { Ast_iterator.default_iterator with structure_item } in
      match ppxed_parsetree with
      | `Interface si -> iterator.signature iterator si
      | `Implementation str -> iterator.structure iterator str)

let expand ~parsetree ~ppxed_parsetree ~pos : ppx_expand_result =
  let expr (self : Ast_iterator.iterator) (expr : Parsetree.expression) =
    match check_extension_node pos expr with
    | true -> get_ppx_items ppxed_parsetree pos (Expr expr)
    | false -> Ast_iterator.default_iterator.expr self expr
  in
  let signature_item (self : Ast_iterator.iterator)
      (original_sg : Parsetree.signature_item) =
    match check_signatures pos original_sg.psig_desc with
    | true -> get_ppx_items ppxed_parsetree pos (Sig_item original_sg)
    | false -> Ast_iterator.default_iterator.signature_item self original_sg
  in
  let structure_item (self : Ast_iterator.iterator)
      (original_str : Parsetree.structure_item) =
    match check_structures pos original_str.pstr_desc with
    | true -> get_ppx_items ppxed_parsetree pos (Str_item original_str)
    | false -> Ast_iterator.default_iterator.structure_item self original_str
  in
  let iterator =
    { Ast_iterator.default_iterator with signature_item; structure_item; expr }
  in
  let () =
    match parsetree with
    | `Interface si -> iterator.signature iterator si
    | `Implementation str -> iterator.structure iterator str
  in
  match !kind with
  | Some Expr _ ->
      let exp =
        List.iter
          (fun exp -> Pprintast.expression Format.str_formatter exp)
          (List.rev !expression);
        Format.flush_str_formatter ()
      in
      Some
        {
          code = exp;
          attr_start = (Option.get !attr).loc_start;
          attr_end = (Option.get !attr).loc_end;
        }
  | Some Sig_item _ ->
      let exp =
        Pprintast.signature Format.str_formatter (List.rev !signature);
        Format.flush_str_formatter ()
      in
      Some
        {
          code = exp;
          attr_start = (Option.get !attr).loc_start;
          attr_end = (Option.get !attr).loc_end;
        }
  | Some Str_item _ ->
      let exp =
        Pprintast.structure Format.str_formatter (List.rev !structure);
        Format.flush_str_formatter ()
      in
      Some
        {
          code = exp;
          attr_start = (Option.get !attr).loc_start;
          attr_end = (Option.get !attr).loc_end;
        }
  | _ -> None
