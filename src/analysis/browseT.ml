open Typedtree

type node =
  | Pattern                  of pattern
  | Expression               of expression
  | Case                     of case
  | Class_expr               of class_expr
  | Class_structure          of class_structure
  | Class_field              of class_field
  | Class_field_kind         of class_field_kind
  | Module_expr              of module_expr
  | Module_type_constraint   of module_type_constraint
  | Structure                of structure
  | Structure_item           of structure_item
  | Module_binding           of module_binding
  | Value_binding            of value_binding
  | Module_type              of module_type
  | Signature                of signature
  | Signature_item           of signature_item
  | Module_declaration       of module_declaration
  | Module_type_declaration  of module_type_declaration
  | With_constraint          of with_constraint
  | Core_type                of core_type
  | Package_type             of package_type
  | Row_field                of row_field
  | Value_description        of value_description
  | Type_declaration         of type_declaration
  | Type_kind                of type_kind
  | Label_declaration        of label_declaration
  | Constructor_declaration  of constructor_declaration
  | Class_type               of class_type
  | Class_signature          of class_signature
  | Class_type_field         of class_type_field
  | Class_declaration        of class_declaration
  | Class_description        of class_description
  | Class_type_declaration   of class_type_declaration

and t = {
  node: node;
  children: t list lazy_t;
}

let rec of_list f l1 l2 = match l1 with
  | [] -> l2
  | x :: xs -> f x :: of_list f xs l2

let rec of_option_list f l1 l2 = match l1 with
  | [] -> l2
  | x :: xs -> match f x with
    | Some x' -> x' :: of_option_list f xs l2
    | None -> of_option_list f xs l2

let rec of_node node =
  let children () =
    match node with
    | Pattern { pat_desc; pat_loc; pat_extra } ->
      of_pattern_desc pat_desc (List.fold_right of_pat_extra pat_extra [])
    | Expression { exp_desc; exp_loc; exp_extra } ->
      of_expression_desc exp_desc (List.fold_right of_exp_extra exp_extra [])
    | Case { c_lhs; c_guard; c_rhs } ->
      of_pattern c_lhs :: of_expression c_rhs ::
      of_option of_expression c_guard []
    | Class_expr { cl_desc; cl_loc } ->
      of_class_expr_desc cl_desc []
    | Class_structure { cstr_self; cstr_fields } ->
      of_pattern cstr_self ::
      List.map (fun f -> of_node (Class_field f)) cstr_fields
    | Class_field { cf_desc; cf_loc } ->
      of_class_field_desc cf_desc []
    | Class_field_kind (Tcfk_virtual ct) ->
      [of_core_type ct]
    | Class_field_kind (Tcfk_concrete (_,e)) ->
      [of_expression e]
    | Module_expr { mod_desc; mod_loc } ->
      of_module_expr_desc mod_desc []
    | Module_type_constraint Tmodtype_implicit ->
      []
    | Module_type_constraint (Tmodtype_explicit mt) ->
      [of_module_type mt]
    | Structure { str_items } ->
      List.map (fun item -> of_node (Structure_item item)) str_items
    | Structure_item { str_desc; str_loc; str_env } ->
      of_structure_item_desc str_desc []
    | Module_binding { mb_expr; mb_loc } ->
      [of_node (Module_expr mb_expr)]
    | Value_binding { vb_pat; vb_expr } ->
      [of_pattern vb_pat; of_expression vb_expr]
    | Module_type { mty_desc; mty_env; mty_loc } ->
      of_module_type_desc mty_desc []
    | Signature { sig_items; sig_final_env } ->
      List.map (fun item -> of_node (Signature_item item)) sig_items
    | Signature_item { sig_desc; sig_env; sig_loc } ->
      of_signature_item_desc sig_desc []
    | Module_declaration { md_type; md_loc } ->
      [of_module_type md_type]
    | Module_type_declaration { mtd_type } ->
      of_option of_module_type mtd_type []
    | With_constraint (Twith_type td | Twith_typesubst td) ->
      [of_node (Type_declaration td)]
    | With_constraint (Twith_module _ | Twith_modsubst _) ->
      []
    | Core_type { ctyp_desc; ctyp_env; ctyp_loc } ->
      of_core_type_desc ctyp_desc []
    | Package_type { pack_fields } ->
      List.map (fun (_,ct) -> of_core_type ct) pack_fields
    | Row_field (Ttag (_,_,cts)) ->
      List.map of_core_type cts
    | Row_field (Tinherit ct) ->
      [of_core_type ct]
    | Value_description { val_desc } ->
      [of_core_type val_desc]
    | Type_declaration { typ_cstrs; typ_kind; typ_manifest } ->
      let of_typ_cstrs (ct1,ct2,_) acc =
        of_core_type ct1 :: of_core_type ct2 :: acc
      in
      of_option of_core_type typ_manifest @@
      of_node (Type_kind typ_kind) ::
      List.fold_right of_typ_cstrs typ_cstrs []
    | Type_kind Ttype_abstract ->
      []
    | Type_kind (Ttype_variant cds) ->
      List.map (fun cd -> of_node (Constructor_declaration cd)) cds
    | Type_kind (Ttype_record lds) ->
      List.map (fun ld -> of_node (Label_declaration ld)) lds
    | Label_declaration { ld_type } ->
      [of_core_type ld_type]
    | Constructor_declaration { cd_args; cd_res } ->
      let args = match cd_res with
        | None -> cd_args
        | Some res -> res :: cd_args
      in
      List.map of_core_type args
    | Class_type { cltyp_desc } ->
      of_class_type_desc cltyp_desc []
    | Class_signature { csig_self; csig_fields } ->
      of_core_type csig_self ::
      List.map (fun x -> of_node (Class_type_field x)) csig_fields
    | Class_type_field { ctf_desc } ->
      of_class_type_field_desc ctf_desc []
    | Class_declaration { ci_expr } ->
      [of_node (Class_expr ci_expr)]
    | Class_description { ci_expr } ->
      [of_node (Class_type ci_expr)]
    | Class_type_declaration { ci_expr } ->
      [of_node (Class_type ci_expr)]
  in
  { node; children = Lazy.from_fun children }

and of_option : 'a. ('a -> 'b) -> 'a option -> 'b list -> 'b list = fun f o acc ->
  match o with
  | None -> acc
  | Some x -> f x :: acc

and of_pattern_desc pat acc = match pat with
  | Tpat_any | Tpat_var _ | Tpat_constant _ | Tpat_variant (_,None,_) ->
    acc
  | Tpat_alias (p,_,_) | Tpat_variant (_,Some p,_) | Tpat_lazy p ->
    of_pattern p :: acc
  | Tpat_tuple ps | Tpat_construct (_,_,ps) | Tpat_array ps ->
    of_list of_pattern ps acc
  | Tpat_record (ls,_) ->
    of_list (fun (_,_,p) -> of_pattern p) ls acc
  | Tpat_or (p1,p2,_) ->
    of_pattern p1 :: of_pattern p2 :: acc

and of_pat_extra (pat,_,_) acc = match pat with
  | Tpat_constraint ct ->
    of_core_type ct :: acc
  | Tpat_type _ | Tpat_unpack ->
    acc

and of_expression e = of_node (Expression e)
and of_case c = of_node (Case c)
and of_pattern p = of_node (Pattern p)
and of_core_type ct : t = of_node (Core_type ct)
and of_value_binding vb = of_node (Value_binding vb)
and of_module_type mt = of_node (Module_type mt)
and of_module_expr me = of_node (Module_expr me)

and of_expression_desc desc acc = match desc with
  | Texp_ident _ | Texp_constant _ | Texp_instvar _
  | Texp_variant (_,None) | Texp_new _ ->
    acc
  | Texp_let (_,vbs,e) ->
    of_expression e ::
    of_list of_value_binding vbs acc
  | Texp_function (_,cs,_) ->
    of_list of_case cs acc
  | Texp_apply (e,ls) ->
    of_expression e ::
    of_option_list (function
        | (_,None,_) -> None
        | (_,Some e,_) -> Some (of_expression e))
      ls
      acc
  | Texp_match (e,cs,_) | Texp_try (e,cs) ->
    of_expression e ::
    of_list of_case cs acc
  | Texp_tuple es | Texp_construct (_,_,es) | Texp_array es ->
    of_list of_expression es acc
  | Texp_variant (_,Some e) | Texp_field (e,_,_)
  | Texp_assert e | Texp_lazy e | Texp_setinstvar (_,_,_,e) ->
    of_expression e :: acc
  | Texp_record (ls,e) ->
    of_option of_expression e @@
    of_list (fun (_,_,e) -> of_expression e) ls acc
  | Texp_setfield (e1,_,_,e2) | Texp_ifthenelse (e1,e2,None)
  | Texp_sequence (e1,e2) | Texp_while (e1,e2) ->
    of_expression e1 :: of_expression e2 :: acc
  | Texp_ifthenelse (e1,e2,Some e3) ->
    of_expression e1 :: of_expression e2 :: of_expression e3 :: acc
  | Texp_for (_,_,e1,e2,_,e3) ->
    of_expression e1 :: of_expression e2 :: of_expression e3 :: acc
  | Texp_send (e,_,eo) ->
    of_expression e :: of_option of_expression eo acc
  | Texp_override (_,ls) ->
    of_list (fun (_,_,e) -> of_expression e) ls acc
  | Texp_letmodule (_,_,me,e) ->
    of_module_expr me :: of_expression e :: acc
  | Texp_object (cs,_) ->
    of_node (Class_structure cs) :: acc
  | Texp_pack me ->
    of_module_expr me :: acc

and of_exp_extra (exp,_,_) acc = match exp with
  | Texp_constraint ct ->
    of_core_type ct :: acc
  | Texp_coerce (cto,ct) ->
    of_core_type ct :: of_option of_core_type cto acc
  | Texp_poly cto ->
    of_option of_core_type cto acc
  | Texp_open _ | Texp_newtype _ -> acc

and of_class_expr_desc desc acc = match desc with
  | Tcl_ident (_,_,cts) ->
    of_list of_core_type cts acc
  | Tcl_structure cs ->
    of_node (Class_structure cs) :: acc
  | Tcl_fun (_,p,es,ce,_) ->
    of_list (fun (_,_,e) -> of_expression e) es @@
    of_pattern p ::
    of_node (Class_expr ce) :: acc
  | Tcl_apply (ce,es) ->
    of_option_list (function
        | (_,None,_) -> None
        | (_,Some e,_) -> Some (of_expression e))
      es @@
    of_node (Class_expr ce) ::
    acc
  | Tcl_let (_,vbs,es,ce) ->
    of_list of_value_binding vbs @@
    of_list (fun (_,_,e) -> of_expression e) es @@
    of_node (Class_expr ce) ::
    acc
  | Tcl_constraint (ce,cto,_,_,_) ->
    of_option (fun ct -> of_node (Class_type ct)) cto @@
    of_node (Class_expr ce) ::
    acc

and of_class_field_desc desc acc = match desc with
  | Tcf_inherit (_,ce,_,_,_) ->
    of_node (Class_expr ce) :: acc
  | Tcf_val (_,_,_,cfk,_) | Tcf_method (_,_,cfk) ->
    of_node (Class_field_kind cfk) :: acc
  | Tcf_constraint (ct1,ct2) ->
    of_core_type ct1 :: of_core_type ct2 :: acc
  | Tcf_initializer e ->
    of_expression e :: acc
and of_module_expr_desc desc acc = match desc with
  | Tmod_ident _ -> acc
  | Tmod_structure str ->
    of_node (Structure str) :: acc
  | Tmod_functor (_,_,mto,me) ->
    of_option of_module_type mto @@
    of_module_expr me ::
    acc
  | Tmod_apply (me1,me2,_) ->
    of_module_expr me1 ::
    of_module_expr me2 ::
    acc
  | Tmod_constraint (me,_,mtc,_) ->
    of_module_expr me ::
    of_node (Module_type_constraint mtc) ::
    acc
  | Tmod_unpack (e,_) ->
    of_expression e ::
    acc

and of_structure_item_desc desc acc = match desc with
  | Tstr_eval (e,_) ->
    of_expression e :: acc
  | Tstr_value (_,vbs) ->
    of_list of_value_binding vbs acc
  | Tstr_primitive vd ->
    of_node (Value_description vd) :: acc
  | Tstr_type tds ->
    of_list (fun td -> of_node (Type_declaration td)) tds acc
  | Tstr_exception cd ->
    of_node (Constructor_declaration cd) :: acc
  | Tstr_module mb ->
    of_node (Module_binding mb) :: acc
  | Tstr_recmodule mbs ->
    of_list (fun x -> of_node (Module_binding x)) mbs acc
  | Tstr_modtype mtd ->
    of_node (Module_type_declaration mtd) :: acc
  | Tstr_class cds ->
    of_list (fun (cd,_,_) -> of_node (Class_declaration cd)) cds acc
  | Tstr_class_type ctds ->
    of_list (fun (_,_,ctd) -> of_node (Class_type_declaration ctd)) ctds acc
  | Tstr_include (me,_,_) ->
    of_module_expr me :: acc
  | Tstr_exn_rebind _ | Tstr_open _ | Tstr_attribute _ ->
    acc

and of_module_type_desc desc acc = match desc with
  | Tmty_ident _ | Tmty_alias _ -> acc
  | Tmty_signature sg ->
    of_node (Signature sg) :: acc
  | Tmty_functor (_,_,mto,mt) ->
    of_option of_module_type mto @@
    of_module_type mt ::
    acc
  | Tmty_with (mt,wcs) ->
    of_list (fun (_,_,wc) -> of_node (With_constraint wc)) wcs @@
    of_module_type mt ::
    acc
  | Tmty_typeof me ->
    of_module_expr me :: acc

and of_signature_item_desc desc acc = match desc with
  | Tsig_open _ | Tsig_attribute _ ->
    acc
  | Tsig_value vd ->
    of_node (Value_description vd) :: acc
  | Tsig_type tds ->
    of_list (fun td -> of_node (Type_declaration td)) tds acc
  | Tsig_exception cd ->
    of_node (Constructor_declaration cd) :: acc
  | Tsig_module md ->
    of_node (Module_declaration md) :: acc
  | Tsig_recmodule mds ->
    of_list (fun md -> of_node (Module_declaration md)) mds acc
  | Tsig_modtype mtd ->
    of_node (Module_type_declaration mtd) :: acc
  | Tsig_include (mt,_,_) ->
    of_module_type mt :: acc
  | Tsig_class cds ->
    of_list (fun cd -> of_node (Class_description cd)) cds acc
  | Tsig_class_type ctds ->
    of_list (fun ctd -> of_node (Class_type_declaration ctd)) ctds acc

and of_core_type_desc desc acc = match desc with
  | Ttyp_any | Ttyp_var _ -> acc
  | Ttyp_arrow (_,ct1,ct2) ->
    of_core_type ct1 :: of_core_type ct2 :: acc
  | Ttyp_tuple cts | Ttyp_constr (_,_,cts) | Ttyp_class (_,_,cts) ->
    of_list of_core_type cts acc
  | Ttyp_object (cts,_) ->
    of_list (fun (_,ct) -> of_core_type ct) cts acc
  | Ttyp_poly (_,ct) | Ttyp_alias (ct,_) ->
    of_core_type ct :: acc
  | Ttyp_variant (rfs,_,_) ->
    of_list (fun rf -> of_node (Row_field rf)) rfs acc
  | Ttyp_package pt ->
    of_node (Package_type pt) :: acc

and of_class_type_desc desc acc = match desc with
  | Tcty_constr (_,_,cts) ->
    of_list of_core_type cts acc
  | Tcty_signature cs ->
    of_node (Class_signature cs) :: acc
  | Tcty_arrow (_,ct,clt) ->
    of_core_type ct :: of_node (Class_type clt) :: acc

and of_class_type_field_desc desc acc = match desc with
  | Tctf_inherit ct ->
    of_node (Class_type ct) :: acc
  | Tctf_val (_,_,_,ct) | Tctf_method (_,_,_,ct) ->
    of_core_type ct :: acc
  | Tctf_constraint (ct1,ct2) ->
    of_core_type ct1 :: of_core_type ct2 :: acc

