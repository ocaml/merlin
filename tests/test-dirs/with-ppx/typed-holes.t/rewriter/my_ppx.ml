open Ppxlib

let expand ~ctxt payload =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  Ast_builder.Default.eint ~loc payload

let my_extension =
  Extension.V3.declare "get_int" Extension.Context.expression
    Ast_pattern.(single_expr_payload (eint __))
    expand

let rule = Ppxlib.Context_free.Rule.extension my_extension
let () = Driver.register_transformation ~rules:[ rule ] "get_int"
