open Std
open Typedtree

let { Logger.log } = Logger.for_section "iterators"

(* Sometimes we do not want to iterate on nodes that do not correspond to actual
    syntax such as the ones introduced by PPXes with the `merlin.hide`
    attribute. *)
let iter_only_visible iter =
  let has_attribute ~name attrs =
    List.exists
      ~f:(fun a ->
        let str, _ = Ast_helper.Attr.as_tuple a in
        str.Location.txt = name)
      attrs
  in
  let not_hidden attrs = not (has_attribute ~name:"merlin.hide" attrs) in
  let not_hidden_node node =
    not (Browse_raw.has_attr ~name:"merlin.hide" node)
  in
  Tast_iterator.
    { iter with
      class_declaration =
        (fun sub ({ ci_attributes; _ } as cl) ->
          if not_hidden ci_attributes then iter.class_declaration sub cl);
      class_description =
        (fun sub ({ ci_attributes; _ } as cl) ->
          if not_hidden ci_attributes then iter.class_description sub cl);
      class_expr =
        (fun sub ({ cl_attributes; _ } as cl) ->
          if not_hidden cl_attributes then iter.class_expr sub cl);
      class_field =
        (fun sub ({ cf_attributes; _ } as cl) ->
          if not_hidden cf_attributes then iter.class_field sub cl);
      class_type =
        (fun sub ({ cltyp_attributes; _ } as cl) ->
          if not_hidden cltyp_attributes then iter.class_type sub cl);
      class_type_declaration =
        (fun sub ({ ci_attributes; _ } as cl) ->
          if not_hidden ci_attributes then iter.class_type_declaration sub cl);
      class_type_field =
        (fun sub ({ ctf_attributes; _ } as cl) ->
          if not_hidden ctf_attributes then iter.class_type_field sub cl);
      expr =
        (fun sub ({ exp_attributes; _ } as e) ->
          if not_hidden exp_attributes then iter.expr sub e);
      extension_constructor =
        (fun sub ({ ext_attributes; _ } as e) ->
          if not_hidden ext_attributes then iter.extension_constructor sub e);
      include_description =
        (fun sub ({ incl_attributes; _ } as incl) ->
          if not_hidden incl_attributes then iter.include_description sub incl);
      include_declaration =
        (fun sub ({ incl_attributes; _ } as incl) ->
          if not_hidden incl_attributes then iter.include_declaration sub incl);
      module_binding =
        (fun sub ({ mb_attributes; _ } as mb) ->
          if not_hidden mb_attributes then iter.module_binding sub mb);
      module_declaration =
        (fun sub ({ md_attributes; _ } as m) ->
          if not_hidden md_attributes then iter.module_declaration sub m);
      module_substitution =
        (fun sub ({ ms_attributes; _ } as m) ->
          if not_hidden ms_attributes then iter.module_substitution sub m);
      module_expr =
        (fun sub ({ mod_attributes; _ } as m) ->
          if not_hidden mod_attributes then iter.module_expr sub m);
      module_type =
        (fun sub ({ mty_attributes; _ } as m) ->
          if not_hidden mty_attributes then iter.module_type sub m);
      module_type_declaration =
        (fun sub ({ mtd_attributes; _ } as m) ->
          if not_hidden mtd_attributes then iter.module_type_declaration sub m);
      pat =
        (fun sub ({ pat_attributes; _ } as p) ->
          if not_hidden pat_attributes then iter.pat sub p);
      row_field =
        (fun sub ({ rf_attributes; _ } as p) ->
          if not_hidden rf_attributes then iter.row_field sub p);
      object_field =
        (fun sub ({ of_attributes; _ } as p) ->
          if not_hidden of_attributes then iter.object_field sub p);
      open_declaration =
        (fun sub ({ open_attributes; _ } as p) ->
          if not_hidden open_attributes then iter.open_declaration sub p);
      open_description =
        (fun sub ({ open_attributes; _ } as p) ->
          if not_hidden open_attributes then iter.open_description sub p);
      signature_item =
        (fun sub si ->
          if not_hidden_node (Signature_item (si, Env.empty)) then
            iter.signature_item sub si);
      structure_item =
        (fun sub si ->
          if not_hidden_node (Structure_item (si, Env.empty)) then
            iter.structure_item sub si);
      typ =
        (fun sub ({ ctyp_attributes; _ } as t) ->
          if not_hidden ctyp_attributes then iter.typ sub t);
      type_declaration =
        (fun sub ({ typ_attributes; _ } as t) ->
          if not_hidden typ_attributes then iter.type_declaration sub t);
      type_extension =
        (fun sub ({ tyext_attributes; _ } as t) ->
          if not_hidden tyext_attributes then iter.type_extension sub t);
      type_exception =
        (fun sub ({ tyexn_attributes; _ } as t) ->
          if not_hidden tyexn_attributes then iter.type_exception sub t);
      value_binding =
        (fun sub ({ vb_attributes; _ } as vb) ->
          if not_hidden vb_attributes then iter.value_binding sub vb);
      value_description =
        (fun sub ({ val_attributes; _ } as vb) ->
          if not_hidden val_attributes then iter.value_description sub vb)
    }

(* The compiler contains an iterator that aims to gather definitions but
   ignores local values like let-in expressions and local type definition. To
   provide occurrences in the active buffer we extend the compiler's iterator with
   these cases. *)
let iter_on_defs ~uid_to_locs_tbl =
  let log = log ~title:"iter_on_defs" in
  let register_uid uid fragment =
    let loc = Typedtree_utils.location_of_declaration ~uid fragment in
    Option.iter loc ~f:(fun loc -> Types.Uid.Tbl.add uid_to_locs_tbl uid loc)
  in
  let iter_decl = Cmt_format.iter_on_declarations ~f:register_uid in
  let register_uid uid loc = Types.Uid.Tbl.add uid_to_locs_tbl uid loc in
  { iter_decl with
    expr =
      (fun sub ({ exp_extra; _ } as expr) ->
        List.iter exp_extra ~f:(fun (exp_extra, _loc, _attr) ->
            match exp_extra with
            | Texp_newtype' (typ_id, typ_name, uid) ->
              log "Found newtype %s wit id %a (%a)\n%!" typ_name.txt Logger.fmt
                (Fun.flip Ident.print_with_scope typ_id) Logger.fmt (fun fmt ->
                  Location.print_loc fmt typ_name.loc);
              register_uid uid typ_name;
              ()
            | _ -> ());
        iter_decl.expr sub expr)
  }

let build_uid_to_locs_tbl ~(local_defs : Mtyper.typedtree) () =
  let uid_to_locs_tbl : string Location.loc Types.Uid.Tbl.t =
    Types.Uid.Tbl.create 64
  in
  let iter = iter_on_defs ~uid_to_locs_tbl in
  begin
    match local_defs with
    | `Interface sign -> iter.signature iter sign
    | `Implementation str -> iter.structure iter str
  end;
  uid_to_locs_tbl

let iter_on_usages ~f (local_defs : Mtyper.typedtree) =
  let occ_iter = Cmt_format.iter_on_occurrences ~f in
  let iter = iter_only_visible occ_iter in
  begin
    match local_defs with
    | `Interface signature -> iter.signature iter signature
    | `Implementation structure -> iter.structure iter structure
  end

let iterator_on_usages ~f =
  let occ_iter = Cmt_format.iter_on_occurrences ~f in
  iter_only_visible occ_iter
