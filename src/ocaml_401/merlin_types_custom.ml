open Std
open Misc

let extract_const_string = function
  | Asttypes.Const_string str -> str
  | _ -> assert false

module Parsetree = struct
  open Parsetree

  let format_params ~f =
    List.map ~f:(function None -> f "_" | Some id -> f id.Location.txt)

  let extract_specific_parsing_info = function
    | { pexp_desc = Pexp_ident longident } -> `Ident longident
    | { pexp_desc = Pexp_construct (longident, _, _) } -> `Constr longident
    | _ -> `Other

  let map_constructors ~f lst =
    List.map lst ~f:(fun ({ Location. txt = pcd_name }, pcd_args, pcd_res, pcd_loc) ->
      f pcd_name pcd_args pcd_res pcd_loc
    )

  let args_of_constructor (_, args, _, _) = args

  let inspect_label lbl = lbl
end

let signature_item_ident =
  let open Types in function
  | Sig_value (id, _)
  | Sig_type (id, _, _)
  | Sig_exception (id, _)
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
  | Tpat_array ps | Tpat_tuple ps | Tpat_construct (_,_,ps,_) -> ps
  | Tpat_or (p1,p2,_) -> [p1;p2]
  | Tpat_record (r,_) -> List.map ~f:thd3 r

let extract_specific_subexpressions =
  let open Typedtree in function
  | Texp_construct (_,_,es,_)  -> es
  | Texp_record (pldes,Some e) -> e :: List.map ~f:thd3 pldes
  | Texp_record (pldes,None)   -> List.map ~f:thd3 pldes
  | Texp_field (ea,_,_)        -> [ea]
  | Texp_setfield (ea,_,_,eb)  -> [ea;eb]
  | _ -> assert false

let exp_open_env = function
  | Typedtree.Texp_open (_,_,_,env) -> env
  | _ -> assert false

let extract_functor_arg m = Some m

let extract_modtype_declaration = function
  | Types.Modtype_abstract -> None
  | Types.Modtype_manifest mt -> Some mt

let extract_module_declaration m = m

let lookup_module = Env.lookup_module

let lookup_modtype name env =
  match Env.lookup_modtype name env with
  | path, Types.Modtype_abstract -> path, None
  | path, Types.Modtype_manifest mty -> path, Some mty

let tstr_eval_expression = function
  | Typedtree.Tstr_eval e -> e
  | _ -> assert false

let summary_prev =
  let open Env in
  function
  | Env_empty -> None
  | Env_open (s,_) | Env_value (s,_,_)
  | Env_type (s,_,_) | Env_exception (s,_,_)
  | Env_module (s,_,_) | Env_modtype (s,_,_)
  | Env_class (s,_,_) | Env_cltype (s,_,_) ->
    Some s

let signature_of_summary =
  let open Env in
  let open Types in
  function
  | Env_value (_,i,v)      -> Some (Sig_value (i,v))
  (* Trec_not == bluff, FIXME *)
  | Env_type (_,i,t)       -> Some (Sig_type (i,t,Trec_not))
  (* Texp_first == bluff, FIXME *)
  | Env_exception (_,i,e)  -> Some (Sig_exception (i, e))
  | Env_module (_,i,m)     -> Some (Sig_module (i,m,Trec_not))
  | Env_modtype (_,i,m)    -> Some (Sig_modtype (i,m))
  | Env_class (_,i,c)      -> Some (Sig_class (i,c,Trec_not))
  | Env_cltype (_,i,c)     -> Some (Sig_class_type (i,c,Trec_not))
  | Env_open _ | Env_empty -> None

let id_of_constr_decl (id, _, _) = id

let add_hidden_signature env sign =
  let add_item env comp =
    match comp with
    | Types.Sig_value(id, decl)     -> Env.add_value (Ident.hide id) decl env
    | Types.Sig_type(id, decl, _)   -> Env.add_type (Ident.hide id) decl env
    | Types.Sig_exception(id, decl) -> Env.add_exception (Ident.hide id) decl env
    | Types.Sig_module(id, mt, _)   -> Env.add_module (Ident.hide id) mt env
    | Types.Sig_modtype(id, decl)   -> Env.add_modtype (Ident.hide id) decl env
    | Types.Sig_class(id, decl, _)  -> Env.add_class (Ident.hide id) decl env
    | Types.Sig_class_type(id, decl, _) -> Env.add_cltype (Ident.hide id) decl env
  in
  List.fold_left ~f:add_item ~init:env sign

let signature_ident =
  let open Types in function
  | Sig_value (i,_)
  | Sig_type (i,_,_)
  | Sig_exception (i,_)
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
    | Mty_functor (_,m1,m2) -> union_loc_opt (mod_loc m1) (mod_loc m2)
    | Mty_signature (lazy s) ->
        let rec find_first = function
          | x :: xs -> (match signature_loc x with
                        | (Some _ as v) -> v
                        | None -> find_first xs)
          | [] -> None
        in
        let a = find_first s and b = find_first (List.rev s) in
        union_loc_opt a b
  in
  function
  | Sig_value (_,v)     -> Some v.val_loc
  | Sig_type (_,t,_)    -> Some t.type_loc
  | Sig_exception (_,e) -> Some e.exn_loc
  | Sig_module (_,m,_)  -> mod_loc m
  | Sig_modtype (_,m)   ->
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
      | Tpat_construct (_, _, patts, _) ->
        List.concat_map patts ~f:inspect_pattern
      | Tpat_record (lst, _) ->
        List.map lst ~f:(fun (lid_loc, _, _pattern) ->
          Longident.last lid_loc.Asttypes.txt, lid_loc.Asttypes.loc
        ) (* TODO: handle rhs, i.e. [_pattern] *)
      | Tpat_variant (_, Some pat, _) -> inspect_pattern pat
      | _ -> []
    in
    List.concat_map binding_lst ~f:(fun (pat, _) -> inspect_pattern pat)
  | Tstr_module (id, name, _) -> [ Ident.name id , name.Asttypes.loc ]
  | Tstr_recmodule mods ->
    List.map mods ~f:(fun (id,name,_,_) -> Ident.name id, name.Asttypes.loc)
  | Tstr_type td_list ->
    List.map td_list ~f:(fun (id, name, _) ->
      Ident.name id, name.Asttypes.loc
    )
  | Tstr_exception (id, name, _) -> [ Ident.name id , name.Asttypes.loc ]
  | _ -> []

let get_mod_expr_if_included ~name item =
  match item.Typedtree.str_desc with
  | Typedtree.Tstr_include (mod_expr, sign) when
    List.exists (include_idents sign) ~f:(fun x -> Ident.name x = name) ->
    `Mod_expr mod_expr
  | _ -> `Not_included

let sig_ident_locs item =
  let open Typedtree in
  match item.sig_desc with
  | Tsig_value (id, _, vd) -> [ Ident.name id , vd.val_loc ]
  | Tsig_type td_list ->
    List.map td_list ~f:(fun (id, _, td) -> Ident.name id, td.typ_loc)
  | Tsig_exception (id, _, ed) -> [ Ident.name id , ed.exn_loc ]
  | Tsig_module (id, _, mt) -> [ Ident.name id , mt.mty_loc ]
  | Tsig_recmodule mds ->
    List.map mds ~f:(fun (id, _, mt) -> Ident.name id , mt.mty_loc)
  | Tsig_modtype (_, str_loc, _) -> [ str_loc.Asttypes.txt , str_loc.Asttypes.loc ]
  | _ -> []

let get_mod_type_if_included ~name item =
  match item.Typedtree.sig_desc with
  | Typedtree.Tsig_include (mty, sign) when
    List.exists (include_idents sign) ~f:(fun x -> Ident.name x = name) ->
    `Mod_type mty
  | _ -> `Not_included

let expose_module_binding item =
  let open Typedtree in
  match item.str_desc with
  | Tstr_module (mb_id, mb_name, mb_expr) ->
    [{ mb_id ; mb_name ; mb_expr ; mb_loc = mb_name.Asttypes.loc }]
  | Tstr_recmodule mods ->
    List.map mods
      ~f:(fun (mb_id, mb_name, _, mb_expr) ->
          { mb_id ; mb_name ; mb_expr ; mb_loc = mb_name.Asttypes.loc })
  | _ -> []

let expose_module_declaration item =
  let open Typedtree in
  match item.sig_desc with
  | Tsig_module (md_id, md_name, md_type) ->
    [{ md_id ; md_name ; md_type ; md_loc = md_name.Asttypes.loc }]
  | Tsig_recmodule mds ->
    List.map mds ~f:(fun (md_id, md_name, md_type) ->
      { md_id ; md_name ; md_type ; md_loc = md_name.Asttypes.loc }
    )
  | _ -> []

let remove_indir_mty mty =
  match mty.Typedtree.mty_desc with
  | Typedtree.Tmty_ident (path, _) -> `Alias path
  | Typedtree.Tmty_signature sg -> `Sg sg
  | Typedtree.Tmty_functor _ -> `Functor
  | Typedtree.Tmty_with (mty, _) -> `Mod_type mty
  | Typedtree.Tmty_typeof me -> `Mod_expr me

let path_and_loc_of_cstr desc env =
  let open Types in
  match desc.cstr_tag with
  | Cstr_exception (path, loc) -> path, loc
  | _ ->
    match desc.cstr_res.desc with
    | Tconstr (path, _, _) ->
      let typ_decl = Env.find_type path env in
      path, typ_decl.Types.type_loc
    | _ -> assert false

let dest_tstr_eval str =
  let open Typedtree in
  match str.str_items with
  | [ { str_desc = Tstr_eval exp }] -> exp
  | _ -> failwith "unhandled expression"

let full_scrape = Mtype.scrape
