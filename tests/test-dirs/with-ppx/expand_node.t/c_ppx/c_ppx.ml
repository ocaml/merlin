open Ppxlib
open Ast_builder.Default

(* Type declarations in structure *)
let generate_impl ~ctxt (rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map (fun ty ->
    pstr_type ~loc rec_flag
    [{
      ptype_loc = {loc with loc_ghost = true};
      ptype_params = ty.ptype_params;
      ptype_cstrs = ty.ptype_cstrs;
      ptype_kind = ty.ptype_kind;
      ptype_manifest = ty.ptype_manifest;
      ptype_private = ty.ptype_private;
      ptype_attributes = [];
      ptype_name = {txt = ty.ptype_name.txt ^ "_renamed"; loc = {loc with loc_ghost = true}} 
    }]
  ) type_declarations

(* Type declarations in signature *)
let generate_intf ~ctxt (rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map (fun ty -> 
    psig_type ~loc rec_flag
    [{
      ptype_loc = {loc with loc_ghost = true};
      ptype_params = ty.ptype_params;
      ptype_cstrs = ty.ptype_cstrs;
      ptype_kind = ty.ptype_kind;
      ptype_manifest = ty.ptype_manifest;
      ptype_private = ty.ptype_private;
      ptype_attributes = [];
      ptype_name = {txt = ty.ptype_name.txt ^ "_renamed"; loc = {loc with loc_ghost = true}}    
    }]
  ) type_declarations

(* Type_extensions in structure *)
let generate_ext_impl ~ctxt type_extension =
  let new_path = Longident.parse ((Longident.name type_extension.ptyext_path.txt) ^ "_renamed") in
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  [
    pstr_typext ~loc
    {
      ptyext_path = {txt = new_path; loc = ({loc with loc_ghost = true})};
      ptyext_params = type_extension.ptyext_params;
      ptyext_constructors = type_extension.ptyext_constructors;
      ptyext_private = type_extension.ptyext_private;
      ptyext_loc = {type_extension.ptyext_loc with loc_ghost = true};
      ptyext_attributes = [];
    }
  ]

(* Type_extensions in signature *)
let generate_ext_intf ~ctxt type_extension =
  let new_path = Longident.parse ((Longident.name type_extension.ptyext_path.txt) ^ "_renamed") in
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  [
    psig_typext ~loc
    {
      ptyext_path = {txt = new_path; loc = ({loc with loc_ghost = true})};
      ptyext_params = type_extension.ptyext_params;
      ptyext_constructors = type_extension.ptyext_constructors;
      ptyext_private = type_extension.ptyext_private;
      ptyext_loc = {type_extension.ptyext_loc with loc_ghost = true};
      ptyext_attributes = [];
    }
  ]

(* Type_exceptions in structure *)
let generate_exn_impl ~ctxt type_exception =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  [
    pstr_exception ~loc
    {
      ptyexn_constructor = {
        pext_name = {txt = type_exception.ptyexn_constructor.pext_name.txt ^ "_renamed"; loc = {loc with loc_ghost = true}};
        pext_kind = type_exception.ptyexn_constructor.pext_kind;
        pext_loc = {loc with loc_ghost = true};
        pext_attributes = [];
      };
      ptyexn_loc = {loc with loc_ghost = true};
      ptyexn_attributes = [];
    }
  ]

(* Type_exceptions in signature *)
let generate_exn_intf ~ctxt type_exception =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  [
    psig_exception ~loc
    {
      ptyexn_constructor = {
        pext_name = {txt = type_exception.ptyexn_constructor.pext_name.txt ^ "_renamed"; loc = {loc with loc_ghost = true}};
        pext_kind = type_exception.ptyexn_constructor.pext_kind;
        pext_loc = {loc with loc_ghost = true};
        pext_attributes = [];
      };
      ptyexn_loc = {loc with loc_ghost = true};
      ptyexn_attributes = [];
    }
  ]

(* Module_type_declarations in structure *)
let generate_mt_impl ~ctxt module_type_declaration =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  [
    pstr_modtype ~loc 
    {
      pmtd_name = {txt = module_type_declaration.pmtd_name.txt ^ "_renamed"; loc = {loc with loc_ghost = true};};
      pmtd_type = module_type_declaration.pmtd_type;
      pmtd_attributes = [];
      pmtd_loc = {loc with loc_ghost = true};
    }
  ]

let generate_mt_intf ~ctxt module_type_declaration =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  [
    psig_modtype ~loc 
    {
      pmtd_name = {txt = module_type_declaration.pmtd_name.txt ^ "_renamed"; loc = {loc with loc_ghost = true};};
      pmtd_type = module_type_declaration.pmtd_type;
      pmtd_attributes = [];
      pmtd_loc = {loc with loc_ghost = true};
    }
  ]


(* Driver for type declarations in structures*)  
let ty_impl_generator = Deriving.Generator.V2.make_noarg generate_impl

(* Driver for type declarations in signatures*)  
let ty_intf_generator = Deriving.Generator.V2.make_noarg generate_intf

(* Driver for type_extensions in structures*)  
let ext_impl_generator = Deriving.Generator.V2.make_noarg generate_ext_impl

(* Driver for type_extensions in signatures*)  
let ext_intf_generator = Deriving.Generator.V2.make_noarg generate_ext_intf

(* Driver for type_exceptions in structures*)  
let exn_impl_generator = Deriving.Generator.V2.make_noarg generate_exn_impl

(* Driver for type_exceptions in signatures*)  
let exn_intf_generator = Deriving.Generator.V2.make_noarg generate_exn_intf

(* Driver for module_type_declarations in structures*)  
let mdt_impl_generator = Deriving.Generator.V2.make_noarg generate_mt_impl

(* Driver for module_type_declarations in signatures*)  
let mdt_intf_generator = Deriving.Generator.V2.make_noarg generate_mt_intf
let my_deriver =
  Deriving.add "rename"
    ~str_type_decl:ty_impl_generator 
    ~sig_type_decl:ty_intf_generator
    ~str_type_ext:ext_impl_generator
    ~sig_type_ext:ext_intf_generator
    ~str_exception:exn_impl_generator
    ~sig_exception:exn_intf_generator
    ~str_module_type_decl:mdt_impl_generator 
    ~sig_module_type_decl:mdt_intf_generator
  |> Deriving.ignore
