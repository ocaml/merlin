val expand :
  parsetree:
    [ `Implementation of Parsetree.structure
    | `Interface of Parsetree.signature ] ->
  ppxed_parsetree:
    [ `Implementation of Parsetree.structure
    | `Interface of Parsetree.signature ] ->
  pos:Lexing.position ->
  Query_protocol.ppx_expand_result option
