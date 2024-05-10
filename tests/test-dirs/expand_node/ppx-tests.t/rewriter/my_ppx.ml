open Ppxlib

let expand ~ctxt payload =
  let _p = payload in
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  Ast_builder.Default.estring ~loc "OCaml is so cool"

let my_extension =
  Extension.V3.declare "tell_me" Extension.Context.expression
    Ast_pattern.(__)
    expand

let rule = Ppxlib.Context_free.Rule.extension my_extension
let () = Driver.register_transformation ~rules:[ rule ] "tell_me"
