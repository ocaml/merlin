type ppx_kind =
  | Expr of Parsetree.expression
  | Sig_item of Parsetree.signature_item
  | Str_item of Parsetree.structure_item

val check_extension :
  parsetree:
    [ `Implementation of Parsetree.structure
    | `Interface of Parsetree.signature ] ->
  pos:Lexing.position ->
  (ppx_kind * Warnings.loc) option

val get_ppxed_source :
  ppxed_parsetree:
    [ `Implementation of Parsetree.structure
    | `Interface of Parsetree.signature ] ->
  pos:Lexing.position ->
  ppx_kind * Warnings.loc ->
  Query_protocol.ppxed_source
