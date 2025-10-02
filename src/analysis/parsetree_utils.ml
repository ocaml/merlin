open Std

open Parsetree

type nonrec constant_desc = constant_desc

let constant_desc c = c.pconst_desc

let filter_merlin_attr =
  let default = Ast_mapper.default_mapper in
  let keep attr =
    let { Location.txt; _ }, _ = Ast_helper.Attr.as_tuple attr in
    not (Std.String.is_prefixed ~by:"merlin." txt)
  in
  let attributes mapper attrs =
    default.Ast_mapper.attributes mapper (List.filter ~f:keep attrs)
  in
  { default with Ast_mapper.attributes }

let expr_remove_merlin_attributes expr =
  filter_merlin_attr.Ast_mapper.expr filter_merlin_attr expr
