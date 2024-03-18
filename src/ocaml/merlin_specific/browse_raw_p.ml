(* {{{ COPYING *(
  Same as Browse_raw module but for the Parstree.
  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 -  Pizie Dust  <playersrebirth(_)gmail.com>

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

[@@@ocaml.warning "-9"]

open Std

type constructor_declaration = Parsetree.constructor_declaration

open Parsetree

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
  | Structure                of structure
  | Signature                of signature
  | Structure_item           of structure_item 
  | Signature_item           of signature_item
  | Module_binding           of module_binding
  | Value_binding            of value_binding
  | Module_type              of module_type
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
  | Binding_op               of binding_op

  | Include_description      of include_description
  | Include_declaration      of include_declaration
  | Open_description         of open_description
  | Open_declaration         of open_declaration
  (* | Method_call              of expression * meth * Location.t 
  | Record_field             of [`Expression of expression | `Pattern of pattern]
                                * Types.label_description
                                * Longident.t Location.loc *)
  | Module_binding_name      of module_binding
  | Module_declaration_name  of module_declaration
  | Module_type_declaration_name of module_type_declaration

let node_update_env env0 = function
  | _ -> env0
  
let node_real_loc loc0 = function
  | Expression              {pexp_loc = loc}
  | Pattern                 {ppat_loc = loc}
  (* | Method_call             (_, _, loc) *)
  (* | Record_field            (_, _, {loc}) *)
  | Class_expr              {pcl_loc = loc}
  | Module_expr             {pmod_loc = loc}
  | Structure_item          {pstr_loc = loc}
  | Signature_item          {psig_loc = loc}
  | Module_type             {pmty_loc = loc}
  | Core_type               {ptyp_loc = loc}
  | Class_type              {pcty_loc = loc}
  | Class_field             {pcf_loc = loc}
  | Module_binding          {pmb_loc = loc}
  | Module_declaration      {pmd_loc = loc}
  | Module_type_declaration {pmtd_loc = loc}
  | Value_description       {pval_loc = loc}
  | Value_binding           {pvb_loc = loc}
  | Type_declaration        {ptype_loc = loc}
  | Label_declaration       {pld_loc = loc}
  | Constructor_declaration {pcd_loc = loc}
  | Class_type_field        {pctf_loc = loc}
  | Class_declaration       {pci_loc = loc}
  | Class_description       {pci_loc = loc}
  | Class_type_declaration  {pci_loc = loc}
  | Extension_constructor   {pext_loc = loc}
  | Include_description     {pincl_loc = loc}
  | Include_declaration     {pincl_loc = loc}
  | Open_description        {popen_loc = loc}
  | Open_declaration        {popen_loc = loc}
  | Binding_op              {pbop_loc = loc}
    -> loc
  | Module_type_declaration_name {pmtd_name = loc}
    -> loc.Location.loc
  | Module_declaration_name      {pmd_name = loc}
  | Module_binding_name          {pmb_name = loc}
    -> loc.Location.loc
  | Structure _ | Signature _ | Case _ | Class_structure _ | Type_extension _
  | Class_field_kind _ | With_constraint _
  | Row_field _ | Type_kind _ | Class_signature _ | Package_type _
  | Dummy
    -> loc0

let node_attributes = function
  | Expression exp        -> exp.pexp_attributes
  | Pattern pat           -> pat.ppat_attributes
  | Class_expr cl         -> cl.pcl_attributes
  | Class_field cf        -> cf.pcf_attributes
  | Module_expr me        -> me.pmod_attributes
  | Structure_item ({pstr_desc = Pstr_eval (_,attr)}) -> attr
  | Structure_item ({pstr_desc = Pstr_attribute a}) -> [a]
  | Structure_item ({pstr_desc = Pstr_extension (_,a)}) -> a
  | Signature_item ({psig_desc = Psig_attribute a}) -> [a]
  | Module_binding mb     -> mb.pmb_attributes
  | Value_binding vb      -> vb.pvb_attributes
  | Module_type mt        -> mt.pmty_attributes
  | Module_declaration md -> md.pmd_attributes
  | Module_type_declaration mtd -> mtd.pmtd_attributes
  | Open_description o    -> o.popen_attributes
  | Include_declaration i -> i.pincl_attributes
  | Include_description i -> i.pincl_attributes
  | Core_type ct          -> ct.ptyp_attributes
  | Row_field rf          -> rf.prf_attributes
  | Value_description vd  -> vd.pval_attributes
  | Type_declaration td   -> td.ptype_attributes
  | Label_declaration ld  -> ld.pld_attributes
  | Constructor_declaration cd -> cd.pcd_attributes
  | Type_extension te     -> te.ptyext_attributes
  | Extension_constructor ec -> ec.pext_attributes
  | Class_type ct         -> ct.pcty_attributes
  | Class_type_field ctf  -> ctf.pctf_attributes
  | Class_declaration ci -> ci.pci_attributes
  | Class_description ci -> ci.pci_attributes
  | Class_type_declaration ci -> ci.pci_attributes
  (* | Method_call (obj,_,_) -> obj.pexp_attributes *)
  (* | Record_field (`Expression obj,_,_) -> obj.pexp_attributes
  | Record_field (`Pattern obj,_,_) -> obj.ppat_attributes *)
  | _ -> []

let has_attr ~name node =
  let attrs = node_attributes node in
  List.exists ~f:(fun a ->
    let (str,_) = Ast_helper.Attr.as_tuple a in
    str.Location.txt = name
  ) attrs

let node_merlin_loc loc0 node =
  let attributes = node_attributes node in
  let loc =
    let open Parsetree in
    let pred { attr_name = loc; _ } = Location_aux.is_relaxed_location loc in
    match List.find attributes ~f:pred with
    | { attr_name; _ } -> attr_name.Location.loc
    | exception Not_found -> node_real_loc loc0 node
  in loc

let app node f acc =
  f node acc

type 'a f0 = node -> 'a -> 'a
type ('b,'a) f1 = 'b -> 'a f0 -> 'a -> 'a

let id_fold (_f : _ f0) acc = acc

let ( ** ) f1 f2 (f : _ f0) acc =
  f2 f (f1 f acc)

let rec list_fold (f' : _ f1) xs f acc = match xs with
  | x :: xs -> list_fold f' xs f (f' x f acc)
  | [] -> acc

let _array_fold (f' : _ f1) arr env f acc =
  let acc = ref acc in
  for i = 0 to Array.length arr - 1 do
    acc := f' arr.(i) env f !acc
  done;
  !acc

let rec _list_fold_with_next (f' : _ -> _ f1) xs f acc = match xs with
  | x :: (y :: _ as xs) -> _list_fold_with_next f' xs f (f' (Some y) x f acc)
  | [x] -> f' None x f acc
  | [] -> acc

let option_fold f' o (f : _ f0) acc = match o with
  | None -> acc
  | Some x -> f' x f acc

let of_core_type ct = app (Core_type ct)

let of_expression e = app (Expression e)

let of_case c = app (Case c)
let of_label_declaration ct = app (Label_declaration ct)
let of_value_binding vb = app (Value_binding vb)
let of_module_type mt = app (Module_type mt)
let of_module_expr me = app (Module_expr me)
let of_typ_param (ct,_) = of_core_type ct
let of_constructor_arguments = function
  | Pcstr_tuple cts -> list_fold of_core_type cts
  | Pcstr_record lbls -> list_fold of_label_declaration lbls

let of_bop ({ pbop_exp; _ } as bop) =
  app (Binding_op bop) ** of_expression pbop_exp

let of_pattern p =
  app (Pattern p)

(* let of_record_field obj loc lbl =
  fun env (f : _ f0) acc ->
  app (Record_field (obj,lbl,loc)) env f acc *)
(* 
let of_exp_record_field obj lid_loc lbl =
  of_record_field (`Expression obj) lid_loc lbl

let of_pat_record_field obj loc lbl =
  of_record_field (`Pattern obj) loc lbl *)

let of_pattern_desc desc =
  match desc with
  | Ppat_any | Ppat_var _ | Ppat_constant _
  | Ppat_interval _ | Ppat_type _ | Ppat_unpack _ 
    -> id_fold
  | Ppat_alias (p,_) | Ppat_lazy p
  | Ppat_exception p -> of_pattern p
  | Ppat_variant (_,p) -> 
    (match p with 
    | Some p -> of_pattern p 
    | None -> id_fold)
  | Ppat_tuple ps | Ppat_array ps ->
    list_fold of_pattern ps
  | Ppat_construct (_,ps) ->
    (match ps with 
    | Some (_, ps) -> of_pattern ps
    | None -> id_fold)
  | Ppat_record (ps,_) ->
    list_fold (fun (_, p) -> 
      of_pattern p) ps
  | Ppat_or (p1,p2) ->
    of_pattern p1 ** of_pattern p2
  | Ppat_extension _ -> 
    id_fold
  | Ppat_constraint (p,ct) -> 
    of_pattern p ** 
    of_core_type ct
  | Ppat_open (_,p) -> 
    of_pattern p

(* let of_method_call obj meth loc =
  fun env (f : _ f0) acc ->
  let loc_start = obj.exp_loc.Location.loc_end in
  let loc_end = loc.Location.loc_end in
  let loc = {loc with Location. loc_start; loc_end} in
  app (Method_call (obj,meth,loc)) env f acc *)

let of_expression_desc = function
  | Pexp_ident _ | Pexp_constant _
  | Pexp_variant (_,None) | Pexp_new _ -> id_fold
  | Pexp_let (_,vbs,e) ->
    of_expression e ** list_fold of_value_binding vbs
  | Pexp_function cases  ->
    list_fold of_case cases
  | Pexp_apply (e,ls) ->
    of_expression e **
    list_fold (fun (_,e) ->of_expression e) ls
  | Pexp_match (e,cs) ->
    of_expression e **
    list_fold of_case cs
  | Pexp_try (e,cs) ->
    of_expression e **
    list_fold of_case cs
  | Pexp_tuple es | Pexp_array es ->
    list_fold of_expression es
  | Pexp_construct (_,es) ->
    (match es with 
    | None -> id_fold
    | Some es -> of_expression es)
  | Pexp_variant (_,Some e)
  | Pexp_assert e | Pexp_lazy e | Pexp_setinstvar (_,e) ->
    of_expression e
  | Pexp_record (ess, es) ->
    option_fold of_expression es **
    list_fold (fun (_, e) -> of_expression e) ess
  | Pexp_field (e1, _) ->
    of_expression e1
  | Pexp_setfield (e1,_,e2) ->
    of_expression e1 ** of_expression e2
  | Pexp_ifthenelse (e1,e2,None)
  | Pexp_sequence (e1,e2) | Pexp_while (e1,e2) ->
    of_expression e1 ** of_expression e2
  | Pexp_ifthenelse (e1,e2,Some e3) | Pexp_for (_,e1,e2,_,e3) ->
    of_expression e1 ** of_expression e2 ** of_expression e3
  | Pexp_send (e,_) ->
    of_expression e
  | Pexp_override ls ->
    list_fold (fun (_,e) -> of_expression e) ls
  | Pexp_letmodule (_,me, e) ->
    app (Module_expr me) ** of_expression e
  | Pexp_letexception (ec,e) ->
    app (Extension_constructor ec) ** of_expression e
  | Pexp_object cs ->
    app (Class_structure cs)
  | Pexp_pack me ->
    of_module_expr me
  | Pexp_unreachable | Pexp_extension _ ->
    id_fold
  | Pexp_letop { let_; ands; body;} ->
    let bindops = let_ :: ands in
    list_fold of_bop bindops **
    of_expression body
  | Pexp_open (od, e) ->
    app (Module_expr od.popen_expr) ** of_expression e
  | Pexp_fun (_, e1, p, e2) -> 
    (match e1 with
    | None -> id_fold
    | Some e -> of_expression e) 
    ** of_pattern p 
    ** of_expression e2
  | Pexp_constraint (e, ct) -> 
    of_expression e ** of_core_type ct
  | Pexp_newtype (_, e) -> 
    of_expression e
  | Pexp_coerce (e, cto, ct) -> 
    of_expression e ** 
    (match cto with 
    | None -> id_fold
    | Some c -> of_core_type c) **
    of_core_type ct
  | Pexp_poly (e, cto) -> 
    of_expression e ** 
    match cto with
    | None -> id_fold 
    | Some ct -> of_core_type ct

and of_class_expr_desc = function
  | Pcl_structure cs ->
    app (Class_structure cs)
  | Pcl_fun (_,es,p,ce) ->
      (match es with
      | None -> id_fold
      | Some es -> of_expression es) **
    of_pattern p **
    app (Class_expr ce)
  | Pcl_apply (ce,es) ->
    list_fold (fun (_,e) -> of_expression e)
      es **
    app (Class_expr ce)
  | Pcl_let (_,vbs,ce) ->
    list_fold of_value_binding vbs **
    app (Class_expr ce)
  | Pcl_constraint (ce,ct) ->
    app (Class_type ct) **
    app (Class_expr ce)
  | Pcl_open (_,ce) ->
    app (Class_expr ce)
  | Pcl_constr (_, ct) ->
    list_fold of_core_type ct
  | Pcl_extension _ ->
    id_fold

and of_class_field_desc = function
  | Pcf_inherit (_,ce,_) ->
    app (Class_expr ce)
  | Pcf_val (_,_,cfk) | Pcf_method (_,_,cfk) ->
    app (Class_field_kind cfk)
  | Pcf_constraint (ct1,ct2) ->
    of_core_type ct1 ** of_core_type ct2
  | Pcf_initializer e ->
    of_expression e
  | Pcf_attribute _ ->
    id_fold 
  | Pcf_extension _ ->
    id_fold

and of_module_expr_desc = function
  | Pmod_ident _ -> id_fold
  | Pmod_structure str ->
    app (Structure str)
  | Pmod_functor (Unit,me) -> of_module_expr me
  | Pmod_functor (Named (_, mt),me) ->
    of_module_type mt ** of_module_expr me
  | Pmod_apply (me1,me2) ->
    of_module_expr me1 **
    of_module_expr me2
  | Pmod_constraint (me,mt) ->
    of_module_expr me **
    app (Module_type mt)
  | Pmod_unpack e ->
    of_expression e
  | Pmod_extension _ ->
    id_fold


and of_structure_item_desc = function
  | Pstr_eval (e,_) ->
    of_expression e
  | Pstr_value (_,vbs) ->
    list_fold of_value_binding vbs
  | Pstr_primitive vd ->
    app (Value_description vd)
  | Pstr_type (_,tds) ->
    list_fold (fun td -> app (Type_declaration td)) tds
  | Pstr_typext text ->
    app (Type_extension text)
  | Pstr_exception texn ->
    app (Extension_constructor texn.ptyexn_constructor)
  | Pstr_module mb ->
    app (Module_binding mb)
  | Pstr_recmodule mbs ->
    list_fold (fun x -> app (Module_binding x)) mbs
  | Pstr_modtype mtd ->
    app (Module_type_declaration mtd)
  | Pstr_class cds ->
    list_fold (fun (cd) -> app (Class_declaration cd)) cds
  | Pstr_class_type ctds ->
    list_fold (fun (ctd) -> app (Class_type_declaration ctd)) ctds
  | Pstr_include i ->
    app (Include_declaration i)
  | Pstr_open d ->
    app (Open_declaration d)
  | Pstr_attribute _ ->
    id_fold
  | Pstr_extension _ ->
    id_fold

and of_module_type_desc = function
  | Pmty_ident _ | Pmty_alias _ -> id_fold
  | Pmty_signature sg ->
    app (Signature sg)
  | Pmty_functor (Named (_,mt1),mt2) ->
    of_module_type mt1 ** of_module_type mt2
  | Pmty_functor (Unit,mt) -> of_module_type mt
  | Pmty_with (mt,wcs) ->
    list_fold (fun (wc) -> app (With_constraint wc)) wcs **
    of_module_type mt
  | Pmty_typeof me ->
    of_module_expr me
  | Pmty_extension _ ->
    id_fold

and of_signature_item_desc = function
  | Psig_attribute _ ->
    id_fold
  | Psig_open d ->
    app (Open_description d)
  | Psig_value vd ->
    app (Value_description vd)
  | Psig_type (_,tds) ->
    list_fold (fun td -> app (Type_declaration td)) tds
  | Psig_typext text ->
    app (Type_extension text)
  | Psig_exception texn ->
    app (Extension_constructor texn.ptyexn_constructor)
  | Psig_module md ->
    app (Module_declaration md)
  | Psig_recmodule mds ->
    list_fold (fun md -> app (Module_declaration md)) mds
  | Psig_modtype mtd ->
    app (Module_type_declaration mtd)
  | Psig_include i ->
    app (Include_description i)
  | Psig_class cds ->
    list_fold (fun cd -> app (Class_description cd)) cds
  | Psig_class_type ctds ->
    list_fold (fun ctd -> app (Class_type_declaration ctd)) ctds
  | Psig_typesubst tds ->
    list_fold (fun td -> app (Type_declaration td)) tds
  | Psig_modsubst _ms ->
    id_fold
  | Psig_modtypesubst _mts ->
    id_fold
  | Psig_extension _ ->
    id_fold

and of_core_type_desc = function
  | Ptyp_any | Ptyp_var _ -> id_fold
  | Ptyp_arrow (_,ct1,ct2) ->
    of_core_type ct1 ** of_core_type ct2
  | Ptyp_tuple cts | Ptyp_constr (_,cts) | Ptyp_class (_,cts) ->
    list_fold of_core_type cts
  | Ptyp_object (cts,_) ->
    list_fold (fun of_ ->
      match of_.pof_desc with
      | Otag (_,ct)
      | Oinherit ct -> of_core_type ct
    ) cts
  | Ptyp_poly (_,ct) | Ptyp_alias (ct,_) ->
    of_core_type ct
  | Ptyp_variant (rfs,_,_) ->
    list_fold (fun rf -> app (Row_field rf)) rfs
  | Ptyp_package pt ->
    app (Package_type pt)
  | Ptyp_extension _ ->
    id_fold

and of_class_type_desc = function
  | Pcty_constr (_,cts) ->
    list_fold of_core_type cts
  | Pcty_signature cs ->
    app (Class_signature cs)
  | Pcty_arrow (_,ct,clt) ->
    of_core_type ct ** app (Class_type clt)
  | Pcty_open (_,ct) ->
    app (Class_type ct)
  | Pcty_extension _ -> id_fold

and of_class_type_field_desc = function
  | Pctf_inherit ct ->
    app (Class_type ct)
  | Pctf_val (_,_,_,ct) | Pctf_method (_,_,_,ct) ->
    of_core_type ct
  | Pctf_constraint (ct1,ct2) ->
    of_core_type ct1 ** of_core_type ct2
  | Pctf_attribute _ ->
    id_fold
  | Pctf_extension _ ->
    id_fold

let of_node = function
  | Dummy -> id_fold
  | Pattern { ppat_desc } ->
    of_pattern_desc ppat_desc
  | Expression { pexp_desc; } ->
    of_expression_desc pexp_desc
  | Case { pc_lhs; pc_guard; pc_rhs } ->
    of_pattern pc_lhs ** of_expression pc_rhs **
    option_fold of_expression pc_guard
  | Class_expr { pcl_desc } ->
    of_class_expr_desc pcl_desc
  | Class_structure { pcstr_self; pcstr_fields } ->
    of_pattern pcstr_self **
    list_fold (fun f -> app (Class_field f)) pcstr_fields
  | Class_field { pcf_desc } ->
    of_class_field_desc pcf_desc
  | Class_field_kind (Cfk_virtual ct) ->
    of_core_type ct
  | Class_field_kind (Cfk_concrete (_,e)) ->
    of_expression e
  | Module_expr { pmod_desc } ->
    of_module_expr_desc pmod_desc
  | Structure str ->
      list_fold (fun st -> app (Structure_item(st))) str
  | Structure_item ({ pstr_desc }) ->
    of_structure_item_desc pstr_desc
  | Module_binding pmb ->
    app (Module_expr pmb.pmb_expr) **
    app (Module_binding_name pmb)
  | Value_binding { pvb_pat; pvb_expr } ->
    of_pattern pvb_pat **
    of_expression pvb_expr
  | Module_type { pmty_desc } ->
    of_module_type_desc pmty_desc
  | Signature sgs ->
    list_fold (fun sg -> app(Signature_item(sg))) sgs
  | Signature_item ({ psig_desc }) ->
    of_signature_item_desc psig_desc
  | Module_declaration pmd ->
    of_module_type pmd.pmd_type **
    app (Module_declaration_name pmd)
  | Module_type_declaration pmtd ->
    option_fold of_module_type pmtd.pmtd_type **
    app (Module_type_declaration_name pmtd)
  | With_constraint (Pwith_type (_,td) | Pwith_typesubst (_,td)) ->
    app (Type_declaration td)
  | With_constraint (Pwith_module _ | Pwith_modsubst _) ->
    id_fold
  | With_constraint (Pwith_modtype (_, mt) | Pwith_modtypesubst (_,mt)) ->
    of_module_type mt
  | Core_type { ptyp_desc } ->
    of_core_type_desc ptyp_desc
  | Package_type (_, pack_fields) ->
    list_fold (fun (_,ct) -> of_core_type ct) pack_fields
  | Row_field prf -> begin
      match prf.prf_desc with
      | Rtag (_,_,cts) -> list_fold of_core_type cts
      | Rinherit ct -> of_core_type ct
    end
  | Value_description { pval_type } ->
    of_core_type pval_type
  | Type_declaration { ptype_params; ptype_cstrs; ptype_kind; ptype_manifest } ->
    let of_typ_cstrs (ct1,ct2,_) = of_core_type ct1 ** of_core_type ct2 in
    option_fold of_core_type ptype_manifest **
    list_fold of_typ_param ptype_params **
    app (Type_kind ptype_kind) **
    list_fold of_typ_cstrs ptype_cstrs
  | Type_kind (Ptype_abstract | Ptype_open) ->
    id_fold
  | Type_kind (Ptype_variant cds) ->
    list_fold (fun cd -> app (Constructor_declaration cd)) cds
  | Type_kind (Ptype_record lds) ->
    list_fold (fun ld -> app (Label_declaration ld)) lds
  | Type_extension { ptyext_params; ptyext_constructors } ->
    list_fold of_typ_param ptyext_params **
    list_fold (fun ec -> app (Extension_constructor ec)) ptyext_constructors
  | Extension_constructor { pext_kind = Pext_decl (_, carg,cto) } ->
    option_fold of_core_type cto **
    of_constructor_arguments carg
  | Extension_constructor { pext_kind = Pext_rebind _ } ->
    id_fold
  | Label_declaration { pld_type } ->
    of_core_type pld_type
  | Constructor_declaration { pcd_args; pcd_res } ->
    option_fold of_core_type pcd_res **
    of_constructor_arguments pcd_args
  | Class_type { pcty_desc } ->
    of_class_type_desc pcty_desc
  | Class_signature { pcsig_self; pcsig_fields } ->
    of_core_type pcsig_self **
    list_fold (fun x -> app (Class_type_field x)) pcsig_fields
  | Class_type_field { pctf_desc } ->
    of_class_type_field_desc pctf_desc
  | Class_declaration { pci_params; pci_expr } ->
    app (Class_expr pci_expr) **
    list_fold of_typ_param pci_params
  | Class_description { pci_params; pci_expr } ->
    app (Class_type pci_expr) **
    list_fold of_typ_param pci_params
  | Class_type_declaration { pci_params; pci_expr } ->
    app (Class_type pci_expr) **
    list_fold of_typ_param pci_params
  (* | Method_call _ -> id_fold
  | Record_field _ -> id_fold *)
  | Module_binding_name _ -> id_fold
  | Module_declaration_name _ -> id_fold
  | Module_type_declaration_name _ -> id_fold
  | Open_description _ -> id_fold
  | Open_declaration pod ->
    app (Module_expr pod.popen_expr)
  | Include_declaration pi ->
    of_module_expr pi.pincl_mod
  | Include_description pi ->
    of_module_type pi.pincl_mod
  | Binding_op { pbop_pat; pbop_exp; } ->
    app (Pattern pbop_pat) ** 
    app (Expression pbop_exp)

let fold_node f node acc =
  of_node node f acc

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
  (* | Module_type_constraint  _ -> "module_type_constraint" *)
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
  | Binding_op              _ -> "binding_op"
  (* | Method_call             _ -> "method_call"
  | Record_field            _ -> "record_field" *)
  | Module_binding_name     _ -> "module_binding_name"
  | Module_declaration_name _ -> "module_declaration_name"
  | Module_type_declaration_name _ -> "module_type_declaration_name"
  | Open_description        _ -> "open_description"
  | Open_declaration        _ -> "open_declaration"
  | Include_description     _ -> "include_description"
  | Include_declaration     _ -> "include_declaration"

let node_is_constructor = function
  | Constructor_declaration decl ->
    Some {decl.pcd_name with Location.txt = `Declaration decl}
  | Expression {pexp_desc = Pexp_construct (loc, desc)} ->
    Some {loc with Location.txt = `Description desc}
  | Extension_constructor ext_cons ->
    Some { Location.loc = ext_cons.pext_loc;
           txt = `Extension_constructor ext_cons}
  | _ -> None
  
