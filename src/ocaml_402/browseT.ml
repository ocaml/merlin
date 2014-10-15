(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2014  Frédéric Bour  <frederic.bour(_)lakaban.net>
                             Thomas Refis  <refis.thomas(_)gmail.com>
                             Simon Castellan  <simon.castellan(_)iuwt.fr>

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  The Software is provided "as is", without warranty of any kind, express or
  implied, including but not limited to the warranties of merchantability,
  fitness for a particular purpose and noninfringement. In no event shall
  the authors or copyright holders be liable for any claim, damages or other
  liability, whether in an action of contract, tort or otherwise, arising
  from, out of or in connection with the software or the use or other dealings
  in the Software.

)* }}} *)

open Std

type constructor_declaration = Typedtree.constructor_declaration

open Typedtree

type node =
  | Dummy
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
  | Type_extension           of type_extension
  | Extension_constructor    of extension_constructor
  | Label_declaration        of label_declaration
  | Constructor_declaration  of constructor_declaration
  | Class_type               of class_type
  | Class_signature          of class_signature
  | Class_type_field         of class_type_field
  | Class_declaration        of class_declaration
  | Class_description        of class_description
  | Class_type_declaration   of class_type_declaration

  | Method_call              of expression * meth
  | Module_binding_name      of module_binding
  | Module_declaration_name  of module_declaration
  | Module_type_declaration_name of module_type_declaration


let default_loc = Location.none
let default_env = Env.empty

type t = {
  t_node: node;
  t_loc : Location.t;
  t_env : Env.t;
  t_children: t list lazy_t;
}

let dummy =
  { t_node = Dummy;
    t_loc = default_loc;
    t_env = default_env;
    t_children = lazy [];
  }

let rec of_list f l1 l2 = match l1 with
  | [] -> l2
  | x :: xs -> f x :: of_list f xs l2

let rec of_option_list f l1 l2 = match l1 with
  | [] -> l2
  | x :: xs -> match f x with
    | Some x' -> x' :: of_option_list f xs l2
    | None -> of_option_list f xs l2

let of_option f o acc =
  match o with
  | None -> acc
  | Some x -> f x :: acc

let rec of_node t_node =
  let t_loc, t_env =
    match t_node with
    | Pattern        {pat_loc = loc; pat_env = env}
    | Expression     {exp_loc = loc; exp_env = env}
    | Method_call    ({exp_loc = loc; exp_env = env}, _)
    | Class_expr     {cl_loc = loc; cl_env = env}
    | Module_expr    {mod_loc = loc; mod_env = env}
    | Structure_item {str_loc = loc; str_env = env}
    | Signature_item {sig_env = env; sig_loc = loc}
    | Module_type    {mty_env = env; mty_loc = loc}
    | Core_type      {ctyp_env = env; ctyp_loc = loc}
    | Class_type     {cltyp_env = env; cltyp_loc = loc}
      -> Some loc, Some env
    | Class_field             {cf_loc = loc}
    | Module_binding          {mb_loc = loc}
    | Module_declaration      {md_loc = loc}
    | Module_type_declaration {mtd_loc = loc}
    | Value_description       {val_loc = loc}
    | Type_declaration        {typ_loc = loc}
    | Label_declaration       {ld_loc = loc}
    | Constructor_declaration {cd_loc = loc}
    | Class_type_field        {ctf_loc = loc}
    | Class_declaration       {ci_loc = loc}
    | Class_description       {ci_loc = loc}
    | Class_type_declaration  {ci_loc = loc}
    | Extension_constructor   {ext_loc = loc}
      -> Some loc, None
    | Module_binding_name          {mb_name = loc}
    | Module_declaration_name      {md_name = loc}
    | Module_type_declaration_name {mtd_name = loc}
      -> Some loc.Location.loc, None
    | Structure {str_final_env = env} | Signature {sig_final_env = env}
      -> None, Some env
    | Case _ | Class_structure _ | Value_binding _ | Type_extension _
    | Class_field_kind _ | Module_type_constraint _ | With_constraint _
    | Row_field _ | Type_kind _ | Class_signature _ | Package_type _
    | Dummy
      -> None, None
  in
  let t_loc = Option.value ~default:default_loc t_loc in
  let t_env = Option.value ~default:default_env t_env in
  let children () =
    match t_node with
    | Pattern { pat_desc; pat_loc; pat_extra } ->
      of_pattern_desc pat_desc (List.fold_right ~f:of_pat_extra pat_extra ~init:[])
    | Expression ({ exp_desc; exp_loc; exp_extra } as exp) ->
      of_expression_desc exp exp_desc (List.fold_right ~f:of_exp_extra exp_extra ~init:[])
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
    | Module_binding mb ->
      [of_node (Module_expr mb.mb_expr); of_node (Module_binding_name mb)]
    | Value_binding { vb_pat; vb_expr } ->
      [of_pattern vb_pat; of_expression vb_expr]
    | Module_type { mty_desc; mty_env; mty_loc } ->
      of_module_type_desc mty_desc []
    | Signature { sig_items; sig_final_env } ->
      List.map (fun item -> of_node (Signature_item item)) sig_items
    | Signature_item { sig_desc; sig_env; sig_loc } ->
      of_signature_item_desc sig_desc []
    | Module_declaration md ->
      [of_module_type md.md_type; of_node (Module_declaration_name md)]
    | Module_type_declaration mtd ->
      of_option of_module_type mtd.mtd_type [of_node (Module_type_declaration_name mtd)]
    | With_constraint (Twith_type td | Twith_typesubst td) ->
      [of_node (Type_declaration td)]
    | With_constraint (Twith_module _ | Twith_modsubst _) ->
      []
    | Core_type { ctyp_desc; ctyp_env; ctyp_loc } ->
      of_core_type_desc ctyp_desc []
    | Package_type { pack_fields } ->
      List.map (fun (_,ct) -> of_core_type ct) pack_fields
    | Row_field (Ttag (_,_,_,cts)) ->
      List.map of_core_type cts
    | Row_field (Tinherit ct) ->
      [of_core_type ct]
    | Value_description { val_desc } ->
      [of_core_type val_desc]
    | Type_declaration { typ_params; typ_cstrs; typ_kind; typ_manifest } ->
      let of_typ_cstrs (ct1,ct2,_) acc =
        of_core_type ct1 :: of_core_type ct2 :: acc
      in
      of_option of_core_type typ_manifest @@
      of_list of_typ_param typ_params @@
      of_node (Type_kind typ_kind) ::
      List.fold_right ~f:of_typ_cstrs typ_cstrs ~init:[]
    | Type_kind (Ttype_abstract | Ttype_open) ->
      []
    | Type_kind (Ttype_variant cds) ->
      List.map (fun cd -> of_node (Constructor_declaration cd)) cds
    | Type_kind (Ttype_record lds) ->
      List.map (fun ld -> of_node (Label_declaration ld)) lds
    | Type_extension { tyext_params; tyext_constructors } ->
      let of_constructors ec acc =
        of_node (Extension_constructor ec) :: acc
      in
      of_list of_typ_param tyext_params @@
      List.fold_right ~f:of_constructors tyext_constructors ~init:[]
    | Extension_constructor { ext_kind = Text_decl (cts,cto) } ->
      of_option of_core_type cto @@
      of_list of_core_type cts []
    | Extension_constructor { ext_kind = Text_rebind _ } ->
      []
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
    | Class_declaration { ci_params; ci_expr } ->
      of_node (Class_expr ci_expr) ::
      List.map of_typ_param ci_params
    | Class_description { ci_params; ci_expr } ->
      of_node (Class_type ci_expr) ::
      List.map of_typ_param ci_params
    | Class_type_declaration { ci_params; ci_expr } ->
      of_node (Class_type ci_expr) ::
      List.map of_typ_param ci_params
    | Dummy -> []
    | Method_call _ -> []
    | Module_binding_name _ -> []
    | Module_declaration_name _ -> []
    | Module_type_declaration_name _ -> []
  in
  { t_node; t_loc; t_env;
    t_children = Lazy.from_fun children }

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
and of_typ_param (ct,_) = of_core_type ct

and of_expression_desc exp desc acc = match desc with
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
  | Texp_match (e,cs1,cs2,_) ->
    of_expression e ::
    of_list of_case (cs1 @ cs2) acc
  | Texp_try (e,cs) ->
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
  | Texp_ifthenelse (e1,e2,Some e3) | Texp_for (_,_,e1,e2,_,e3) ->
    of_expression e1 :: of_expression e2 :: of_expression e3 :: acc
  | Texp_send (e,meth,eo) ->
    let lhs = of_expression e in
    let meth =
      let loc_start = lhs.t_loc.Location.loc_end in
      let loc_end = match eo with
        | None -> exp.exp_loc.Location.loc_end
        | Some e -> e.exp_loc.Location.loc_start
      in
      { t_node = Method_call (e,meth);
        t_loc = {Location. loc_ghost = true; loc_start; loc_end};
        t_env = default_env; t_children = lazy [];
      }
    in
    lhs :: meth :: of_option of_expression eo acc
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
  | Tcf_attribute _ ->
    assert false (*TODO*)
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
  | Tstr_typext text ->
    of_node (Type_extension text) :: acc
  | Tstr_exception ec ->
    of_node (Extension_constructor ec) :: acc
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
  | Tstr_include { incl_mod = me } ->
    of_module_expr me :: acc
  | Tstr_open _ | Tstr_attribute _ ->
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
  | Tsig_typext text ->
    of_node (Type_extension text) :: acc
  | Tsig_exception ec ->
    of_node (Extension_constructor ec) :: acc
  | Tsig_module md ->
    of_node (Module_declaration md) :: acc
  | Tsig_recmodule mds ->
    of_list (fun md -> of_node (Module_declaration md)) mds acc
  | Tsig_modtype mtd ->
    of_node (Module_type_declaration mtd) :: acc
  | Tsig_include { incl_mod = mt } ->
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
    of_list (fun (_,_,ct) -> of_core_type ct) cts acc
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
  | Tctf_attribute _ ->
    acc

let rec annot loc env t =
  let t_loc = if t.t_loc == default_loc then loc else t.t_loc in
  let t_env = if t.t_env == default_env then env else t.t_env in
  if (t_loc != t.t_loc ) || (t_env != t.t_env) then
    { t_node = t.t_node; t_loc; t_env;
      t_children = lazy (List.map (annot loc env) (Lazy.force t.t_children)) }
  else
    t

let of_node ?(loc=default_loc) ?(env=default_env) node =
  let t = of_node node in
  if loc != default_loc || env != default_env then
    annot loc env t
  else
    t


(** Accessors for information specific to a node *)

let string_of_node = function
  | Dummy                     -> "dummy"
  | Pattern                 _ -> "pattern"
  | Expression              _ -> "expression"
  | Case                    _ -> "case"
  | Class_expr              _ -> "class_expr"
  | Class_structure         _ -> "class_structure"
  | Class_field             _ -> "class_field"
  | Class_field_kind        _ -> "class_field_kind"
  | Module_expr             _ -> "module_expr"
  | Module_type_constraint  _ -> "module_type_constraint"
  | Structure               _ -> "structure"
  | Structure_item          _ -> "structure_item"
  | Module_binding          _ -> "module_binding"
  | Value_binding           _ -> "value_binding"
  | Module_type             _ -> "module_type"
  | Signature               _ -> "signature"
  | Signature_item          _ -> "signature_item"
  | Module_declaration      _ -> "module_declaration"
  | Module_type_declaration _ -> "module_type_declaration"
  | With_constraint         _ -> "with_constraint"
  | Core_type               _ -> "core_type"
  | Package_type            _ -> "package_type"
  | Row_field               _ -> "row_field"
  | Value_description       _ -> "value_description"
  | Type_declaration        _ -> "type_declaration"
  | Type_kind               _ -> "type_kind"
  | Type_extension          _ -> "type_extension"
  | Extension_constructor   _ -> "extension_constructor"
  | Label_declaration       _ -> "label_declaration"
  | Constructor_declaration _ -> "constructor_declaration"
  | Class_type              _ -> "class_type"
  | Class_signature         _ -> "class_signature"
  | Class_type_field        _ -> "class_type_field"
  | Class_declaration       _ -> "class_declaration"
  | Class_description       _ -> "class_description"
  | Class_type_declaration  _ -> "class_type_declaration"
  | Method_call             _ -> "method_call"
  | Module_binding_name     _ -> "module_binding_name"
  | Module_declaration_name _ -> "module_declaration_name"
  | Module_type_declaration_name _ -> "module_type_declaration_name"

let pattern_paths { Typedtree. pat_desc; pat_extra; pat_loc } =
  let init =
    match pat_desc with
    | Tpat_var (id,_) -> [Location.mkloc (Path.Pident id) pat_loc]
    | Tpat_alias (_,id,loc) -> [{loc with Location.txt = Path.Pident id}]
    | _ -> []
  in
  List.fold_left ~init pat_extra
    ~f:(fun acc (extra,_,_) ->
      match extra with
      | Tpat_type (path,loc) -> {loc with Location.txt = path} :: acc
      | _ -> acc)

let expression_paths { Typedtree. exp_desc; exp_extra } =
  match exp_desc with
  | Texp_ident (path,loc,_) -> [{loc with Location.txt = path}]
  | Texp_new (path,loc,_) -> [{loc with Location.txt = path}]
  | Texp_instvar (_,path,loc)  -> [{loc with Location.txt = path}]
  | Texp_setinstvar (_,path,loc,_) -> [{loc with Location.txt = path}]
  | Texp_override (_,ps) ->
    List.map (fun (path,loc,_) -> {loc with Location.txt = path}) ps
  | Texp_letmodule (id,loc,_,_) -> [{loc with Location.txt = Path.Pident id}]
  | _ -> []

let is_constructor t =
  match t.t_node with
  | Constructor_declaration decl ->
    Some {decl.cd_name with Location.txt = `Declaration decl}
  | Expression {exp_desc = Texp_construct (loc, desc, _)} ->
    Some {loc with Location.txt = `Description desc}
  | Pattern {pat_desc = Tpat_construct (loc, desc, _)} ->
    Some {loc with Location.txt = `Description desc}
  | _ -> None
