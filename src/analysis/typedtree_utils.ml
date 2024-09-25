open Std

let extract_toplevel_identifier item =
  match item.Typedtree.sig_desc with
  | Typedtree.Tsig_value { val_id; _ } -> [ val_id ]
  | Typedtree.Tsig_modsubst { ms_id; _ } -> [ ms_id ]
  | Typedtree.Tsig_modtype { mtd_id; _ }
  | Typedtree.Tsig_modtypesubst { mtd_id; _ } -> [ mtd_id ]
  | Typedtree.Tsig_module { md_id; _ } -> Option.to_list md_id
  | Typedtree.Tsig_recmodule mods ->
    List.filter_map ~f:(fun Typedtree.{ md_id; _ } -> md_id) mods
  | Typedtree.Tsig_class cls ->
    List.map ~f:(fun Typedtree.{ ci_id_class; _ } -> ci_id_class) cls
  | Typedtree.Tsig_class_type cls ->
    List.map ~f:(fun Typedtree.{ ci_id_class_type; _ } -> ci_id_class_type) cls
  | Typedtree.Tsig_type _
  | Typedtree.Tsig_typesubst _
  | Typedtree.Tsig_typext _
  | Typedtree.Tsig_exception _
  | Typedtree.Tsig_open _
  | Typedtree.Tsig_include _
  | Typedtree.Tsig_attribute _ -> []

let let_bound_vars bindings =
  List.filter_map
    ~f:(fun value_binding ->
      match value_binding.Typedtree.vb_pat.pat_desc with
      | Tpat_var (id, loc) -> Some (id, loc)
      | Typedtree.Tpat_any
      | Typedtree.Tpat_alias (_, _, _)
      | Typedtree.Tpat_constant _ | Typedtree.Tpat_tuple _
      | Typedtree.Tpat_construct (_, _, _, _)
      | Typedtree.Tpat_variant (_, _, _)
      | Typedtree.Tpat_record (_, _)
      | Typedtree.Tpat_array _ | Typedtree.Tpat_lazy _
      | Typedtree.Tpat_or (_, _, _) -> None)
    bindings

let pat_var_id_and_loc = function
  | Typedtree.{ pat_desc = Tpat_var (id, loc); _ } -> Some (id, loc)
  | _ -> None

let pat_alias_pat_id_and_loc = function
  | Typedtree.{ pat_desc = Tpat_alias (pat, id, loc); _ } -> Some (pat, id, loc)
  | _ -> None
