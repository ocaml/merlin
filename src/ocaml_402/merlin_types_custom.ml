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
open Misc

let extract_const_string = function
  | Asttypes.Const_string (str, _) -> str
  | _ -> assert false

module Parsetree = struct
  open Parsetree

  let format_params ~f params =
    let format_param (param,_variance) =
      match param.ptyp_desc with
      | Ptyp_any -> f "_"
      | Ptyp_var v -> f v
      | _ -> assert false (*TODO*)
    in
    List.map format_param params

  let extract_specific_parsing_info = function
    | { pexp_desc = Pexp_ident longident } -> `Ident longident
    | { pexp_desc = Pexp_construct (longident, _) } -> `Constr longident
    | _ -> `Other

  let map_constructors ~f lst =
    List.map lst ~f:(fun { pcd_name ; pcd_args ; pcd_res ; pcd_loc ; _ } ->
      f pcd_name.Location.txt pcd_args pcd_res pcd_loc
    )

  let args_of_constructor c = c.pcd_args

  let inspect_label { pld_name ; pld_mutable ; pld_type ; pld_loc ; _ } =
    pld_name, pld_mutable, pld_type, pld_loc
end

let signature_item_ident =
  let open Types in function
  | Sig_value (id, _)
  | Sig_type (id, _, _)
  | Sig_typext (id, _, _)
  | Sig_module (id, _, _)
  | Sig_modtype (id, _)
  | Sig_class (id, _, _)
  | Sig_class_type (id, _, _) -> id

let include_idents l = List.map signature_item_ident l

let lookup_constructor = Env.lookup_constructor
let lookup_label       = Env.lookup_label

let fold_types f id env acc =
  Env.fold_types (fun s p (decl,descr) acc -> f s p decl acc) id env acc

let fold_constructors f id env acc =
  Env.fold_constructors
    (fun constr acc -> f constr.Types.cstr_name constr acc)
    id env acc
let fold_labels = Env.fold_labels

let extract_subpatterns =
  let open Typedtree in function
  | Tpat_any | Tpat_var _ | Tpat_constant _ | Tpat_variant (_,None,_) -> []
  | Tpat_alias (p,_,_) | Tpat_lazy p | Tpat_variant (_,Some p,_) -> [p]
  | Tpat_array ps | Tpat_tuple ps | Tpat_construct (_,_,ps) -> ps
  | Tpat_or (p1,p2,_) -> [p1;p2]
  | Tpat_record (r,_) -> List.map ~f:thd3 r

let extract_specific_subexpressions =
  let open Typedtree in function
  | Texp_construct (_,_,es)  -> es
  | Texp_record (pldes,Some e) -> e :: List.map ~f:thd3 pldes
  | Texp_record (pldes,None)   -> List.map ~f:thd3 pldes
  | Texp_field (ea,_,_)        -> [ea]
  | Texp_setfield (ea,_,_,eb)  -> [ea;eb]
  | _ -> assert false

let exp_open_env = function
  | Typedtree.Texp_open (_,_,_,env) -> env
  | _ -> assert false

let extract_functor_arg m = m

let extract_modtype_declaration m = m.Types.mtd_type
let extract_module_declaration m = m.Types.md_type

let lookup_module name env =
  let path = Env.lookup_module ~load:true name env in
  let md = Env.find_module path env in
  path, extract_module_declaration md

let lookup_modtype name env =
  let path, mdtype = Env.lookup_modtype name env in
  path, mdtype.Types.mtd_type

let summary_prev =
  let open Env in
  function
  | Env_empty -> None
  | Env_open (s,_) | Env_value (s,_,_)
  | Env_type (s,_,_) | Env_extension (s,_,_)
  | Env_module (s,_,_) | Env_modtype (s,_,_)
  | Env_class (s,_,_) | Env_cltype (s,_,_)
  | Env_functor_arg (s,_) ->
    Some s

let signature_of_summary =
  let open Env in
  let open Types in
  function
  | Env_value (_,i,v)      -> Some (Sig_value (i,v))
  (* Trec_not == bluff, FIXME *)
  | Env_type (_,i,t)       -> Some (Sig_type (i,t,Trec_not))
  (* Texp_first == bluff, FIXME *)
  | Env_extension (_,i,e)  ->
    begin match e.ext_type_path with
    | Path.Pident id when Ident.name id = "exn" ->
      Some (Sig_typext (i,e, Text_exception))
    | _ ->
      Some (Sig_typext (i,e, Text_first))
    end
  | Env_module (_,i,m)     -> Some (Sig_module (i,m,Trec_not))
  | Env_modtype (_,i,m)    -> Some (Sig_modtype (i,m))
  | Env_class (_,i,c)      -> Some (Sig_class (i,c,Trec_not))
  | Env_cltype (_,i,c)     -> Some (Sig_class_type (i,c,Trec_not))
  | Env_open _ | Env_empty | Env_functor_arg _ -> None

let id_of_constr_decl c = c.Types.cd_id

let add_hidden_signature env sign =
  let add_item env comp =
    match comp with
    | Types.Sig_value(id, decl)     -> Env.add_value (Ident.hide id) decl env
    | Types.Sig_type(id, decl, _)   -> Env.add_type ~check:false (Ident.hide id) decl env
    | Types.Sig_typext(id, decl, _) -> Env.add_extension ~check:false (Ident.hide id) decl env
    | Types.Sig_module(id, mty, _)  -> Env.add_module (Ident.hide id) mty.Types.md_type env
    | Types.Sig_modtype(id, decl)   -> Env.add_modtype (Ident.hide id) decl env
    | Types.Sig_class(id, decl, _)  -> Env.add_class (Ident.hide id) decl env
    | Types.Sig_class_type(id, decl, _) -> Env.add_cltype (Ident.hide id) decl env
  in
  List.fold_left ~f:add_item ~init:env sign

let signature_ident =
  let open Types in function
  | Sig_value (i,_)
  | Sig_type (i,_,_)
  | Sig_typext (i,_,_)
  | Sig_modtype (i,_)
  | Sig_module (i,_,_)
  | Sig_class (i,_,_)
  | Sig_class_type (i,_,_) -> i

let union_loc_opt a b = match a,b with
  | None, None -> None
  | (Some _ as l), None | None, (Some _ as l) -> l
  | Some a, Some b -> Some (Parsing_aux.location_union a b)

let rec signature_loc =
  let open Types in
  let rec mod_loc = function
    | Mty_ident _ -> None
    | Mty_functor (_,m1,m2) ->
      begin match extract_functor_arg m1 with
      | Some m1 -> union_loc_opt (mod_loc m1) (mod_loc m2)
      | None -> mod_loc m2
      end
    | Mty_signature (lazy s) ->
        let rec find_first = function
          | x :: xs -> (match signature_loc x with
                        | (Some _ as v) -> v
                        | None -> find_first xs)
          | [] -> None
        in
        let a = find_first s and b = find_first (List.rev s) in
        union_loc_opt a b
    | _ -> None
  in
  function
  | Sig_value (_,v)    -> Some v.val_loc
  | Sig_type (_,t,_)   -> Some t.type_loc
  | Sig_typext (_,e,_) -> Some e.ext_loc
  | Sig_module (_,m,_) -> mod_loc (extract_module_declaration m)
  | Sig_modtype (_,m) ->
    begin match extract_modtype_declaration m with
    | Some m -> mod_loc m
    | None -> None
    end
  | Sig_class (_,_,_)
  | Sig_class_type (_,_,_) -> None

let str_ident_locs item =
  let open Typedtree in
  match item.str_desc with
  | Tstr_value (_, binding_lst) ->
    let rec inspect_pattern pat =
      match pat.pat_desc with
      | Tpat_var (id, _) -> [ Ident.name id , pat.pat_loc ]
      | Tpat_tuple patts
      | Tpat_array patts
      | Tpat_construct (_, _, patts) ->
        List.concat_map patts ~f:inspect_pattern
      | Tpat_record (lst, _) ->
        List.map lst ~f:(fun (lid_loc, _, _pattern) ->
          Longident.last lid_loc.Asttypes.txt, lid_loc.Asttypes.loc
        ) (* TODO: handle rhs, i.e. [_pattern] *)
      | Tpat_variant (_, Some pat, _) -> inspect_pattern pat
      | _ -> []
    in
    List.concat_map binding_lst ~f:(fun b -> inspect_pattern b.vb_pat)
  | Tstr_module mb -> [ Ident.name mb.mb_id , mb.mb_loc ]
  | Tstr_recmodule mbs ->
    List.map mbs ~f:(fun mb -> Ident.name mb.mb_id , mb.mb_loc)
  | Tstr_modtype mtd -> [ Ident.name mtd.mtd_id , mtd.mtd_loc ]
  | Tstr_type td_list ->
    List.map td_list ~f:(fun { typ_id ; typ_loc } ->
      Ident.name typ_id, typ_loc
    )
  | Tstr_exception ec -> [ Ident.name ec.ext_id , ec.ext_loc ]
  | _ -> []

let get_mod_expr_if_included ~name item =
  match item.Typedtree.str_desc with
  | Typedtree.Tstr_include { Typedtree. incl_type ; incl_mod } when
    List.exists (include_idents incl_type) ~f:(fun x -> Ident.name x = name) ->
    `Mod_expr incl_mod
  | _ -> `Not_included

let sig_ident_locs item =
  let open Typedtree in
  match item.sig_desc with
  | Tsig_value vd -> [ Ident.name vd.val_id , vd.val_loc ]
  | Tsig_type td_list ->
    List.map td_list ~f:(fun { typ_id ; typ_loc } ->
      Ident.name typ_id, typ_loc
    )
  | Tsig_typext te ->
    (* N.B. we don't want to stop on "type M.t += ..." when looking for "M.t", but we do
     * want to stop here if we are looking for a constructor added at this particular
     * point. That's why we only get information about the constructors. *)
    List.map te.tyext_constructors ~f:(fun ec ->
      Ident.name ec.ext_id , ec.ext_loc
    )
  | Tsig_exception ec -> [ Ident.name ec.ext_id , ec.ext_loc ]
  | Tsig_module md -> [ Ident.name md.md_id , md.md_loc ]
  | Tsig_recmodule mds ->
    List.map mds ~f:(fun md -> Ident.name md.md_id , md.md_loc)
  | Tsig_modtype mtd -> [ Ident.name mtd.mtd_id , mtd.mtd_loc ]
  | _ -> []

let get_mod_type_if_included ~name item =
  match item.Typedtree.sig_desc with
  | Typedtree.Tsig_include { Typedtree. incl_type ; incl_mod } when
    List.exists (include_idents incl_type) ~f:(fun x -> Ident.name x = name) ->
    `Mod_type incl_mod
  | _ -> `Not_included

let expose_module_binding item =
  match item.Typedtree.str_desc with
  | Typedtree.Tstr_module mb -> [mb]
  | Typedtree.Tstr_recmodule mbs -> mbs
  | _ -> []

let expose_module_declaration item =
  match item.Typedtree.sig_desc with
  | Typedtree.Tsig_module md -> [md]
  | Typedtree.Tsig_recmodule mds -> mds
  | _ -> []

let remove_indir_me me =
  match me.Typedtree.mod_desc with
  | Typedtree.Tmod_ident (path, _) -> `Alias path
  | Typedtree.Tmod_structure str -> `Str str
  | Typedtree.Tmod_functor _ -> `Functor ""
  | Typedtree.Tmod_apply (_,_,_) -> `Functor " instantiation"
  | Typedtree.Tmod_constraint (me, _, _, _) -> `Mod_expr me
  | Typedtree.Tmod_unpack _ -> `Unpack

let remove_indir_mty mty =
  match mty.Typedtree.mty_desc with
  | Typedtree.Tmty_alias (path, _)
  | Typedtree.Tmty_ident (path, _) -> `Alias path
  | Typedtree.Tmty_signature sg -> `Sg sg
  | Typedtree.Tmty_functor _ -> `Functor " signature"
  | Typedtree.Tmty_with (mty, _) -> `Mod_type mty
  | Typedtree.Tmty_typeof me -> `Mod_expr me

let path_and_loc_of_cstr desc env =
  let open Types in
  match desc.cstr_tag with
  | Cstr_extension (path, loc) -> path, desc.cstr_loc
  | _ ->
    match desc.cstr_res.desc with
    | Tconstr (path, _, _) ->
      let typ_decl = Env.find_type path env in
      path, typ_decl.Types.type_loc
    | _ -> assert false

(* TODO: remove *)
let dest_tstr_eval str =
  let open Typedtree in
  match str.str_items with
  | [ { str_desc = Tstr_eval (exp,[]) }] -> exp
  | _ -> failwith "unhandled expression"

let full_scrape = Env.scrape_alias
