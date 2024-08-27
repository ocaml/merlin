open Std

let extract_toplevel_identifier item =
  match item.Typedtree.sig_desc with
  | Typedtree.Tsig_value { val_id; _ } ->  [val_id]
  | Typedtree.Tsig_modsubst { ms_id; _ } -> [ms_id]
  | Typedtree.Tsig_modtype { mtd_id; _ }
  | Typedtree.Tsig_modtypesubst { mtd_id; _ } ->  [mtd_id]
  | Typedtree.Tsig_module { md_id; _ } -> Option.to_list md_id
  | Typedtree.Tsig_recmodule mods ->
      List.filter_map ~f:(fun Typedtree.{md_id; _} -> md_id) mods
  | Typedtree.Tsig_class cls ->
      List.map ~f:(fun Typedtree.{ ci_id_class; _} -> ci_id_class) cls
  | Typedtree.Tsig_class_type cls ->
      List.map ~f:(fun Typedtree.{ ci_id_class_type; _} -> ci_id_class_type) cls
  | Typedtree.Tsig_type _
  | Typedtree.Tsig_typesubst _
  | Typedtree.Tsig_typext _
  | Typedtree.Tsig_exception _
  | Typedtree.Tsig_open _
  | Typedtree.Tsig_include _
  | Typedtree.Tsig_attribute _ -> []

let let_bound_vars bindings =
  List.filter_map ~f:(fun value_binding ->
    match value_binding.Typedtree.vb_pat.pat_desc with
    | Tpat_var (id, loc, _) -> Some (id, loc)
    | _ -> None
  ) bindings
