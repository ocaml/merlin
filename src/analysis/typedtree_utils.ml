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
      | Tpat_var (id, loc, _) -> Some (id, loc)
      | Typedtree.Tpat_any
      | Typedtree.Tpat_alias (_, _, _, _)
      | Typedtree.Tpat_constant _ | Typedtree.Tpat_tuple _
      | Typedtree.Tpat_construct (_, _, _, _)
      | Typedtree.Tpat_variant (_, _, _)
      | Typedtree.Tpat_record (_, _)
      | Typedtree.Tpat_array _ | Typedtree.Tpat_lazy _
      | Typedtree.Tpat_or (_, _, _) -> None)
    bindings

let location_of_declaration ~uid =
  let of_option name =
    match name.Location.txt with
    | Some txt -> Some { name with txt }
    | None -> None
  in
  let of_value_binding vb =
    let bound_idents = Typedtree.let_bound_idents_full [ vb ] in
    ListLabels.find_map
      ~f:(fun (_, loc, _, uid') -> if uid = uid' then Some loc else None)
      bound_idents
  in
  function
  | Typedtree.Value vd -> Some vd.val_name
  | Value_binding vb -> of_value_binding vb
  | Type td -> Some td.typ_name
  | Constructor cd -> Some cd.cd_name
  | Extension_constructor ec -> Some ec.ext_name
  | Label ld -> Some ld.ld_name
  | Module md -> of_option md.md_name
  | Module_binding mb -> of_option mb.mb_name
  | Module_type mtd -> Some mtd.mtd_name
  | Module_substitution msd -> Some msd.ms_name
  | Class cd -> Some cd.ci_id_name
  | Class_type ctd -> Some ctd.ci_id_name

let pat_var_id_and_loc = function
  | Typedtree.{ pat_desc = Tpat_var (id, loc, _); _ } -> Some (id, loc)
  | _ -> None

let pat_alias_pat_id_and_loc = function
  | Typedtree.{ pat_desc = Tpat_alias (pat, id, loc, _); _ } ->
    Some (pat, id, loc)
  | _ -> None
