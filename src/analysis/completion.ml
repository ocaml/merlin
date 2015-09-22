(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2015  Frédéric Bour  <frederic.bour(_)lakaban.net>
                             Thomas Refis  <refis.thomas(_)gmail.com>
                             Simon Castellan  <simon.castellan(_)iuwt.fr>
                             Jeremie Dimino  <jeremie(_)dimino.org>

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
open Merlin_lib

open BrowseT
open Browse_node

(* List methods of an object.
   Code taken from [uTop](https://github.com/diml/utop
   with permission from Jeremie Dimino. *)
let lookup_env f x env =
  try Some (f x env)
  with Not_found | Env.Error _ -> None

let rec methods_of_type env ?(acc=[]) type_expr =
  let open Types in
  match type_expr.desc with
  | Tlink type_expr | Tobject (type_expr, _) | Tpoly (type_expr, _) ->
    methods_of_type env ~acc type_expr
  | Tfield (name, _, ty, rest) ->
    methods_of_type env ~acc:((name,ty) :: acc) rest
  | Tconstr (path, _, _) -> begin
      match lookup_env Env.find_type path env with
      | None | Some { type_manifest = None } -> acc
      | Some { type_manifest = Some type_expr } ->
        methods_of_type env ~acc type_expr
    end
  | _ -> acc

let classify_node = function
  | Dummy                      -> `Expression
  | Pattern                  _ -> `Pattern
  | Expression               _ -> `Expression
  | Case                     _ -> `Pattern
  | Class_expr               _ -> `Expression
  | Class_structure          _ -> `Expression
  | Class_field              _ -> `Expression
  | Class_field_kind         _ -> `Expression
  | Module_expr              _ -> `Module
  | Module_type_constraint   _ -> `Module_type
  | Structure                _ -> `Structure
  | Structure_item           _ -> `Structure
  | Module_binding           _ -> `Module
  | Value_binding            _ -> `Type
  | Module_type              _ -> `Module_type
  | Signature                _ -> `Signature
  | Signature_item           _ -> `Signature
  | Module_declaration       _ -> `Module
  | Module_type_declaration  _ -> `Module_type
  | With_constraint          _ -> `Type
  | Core_type                _ -> `Type
  | Package_type             _ -> `Module_type
  | Row_field                _ -> `Expression
  | Value_description        _ -> `Type
  | Type_declaration         _ -> `Type
  | Type_kind                _ -> `Type
  | Type_extension           _ -> `Type
  | Extension_constructor    _ -> `Type
  | Label_declaration        _ -> `Type
  | Constructor_declaration  _ -> `Type
  | Class_type               _ -> `Type
  | Class_signature          _ -> `Type
  | Class_type_field         _ -> `Type
  | Class_declaration        _ -> `Expression
  | Class_description        _ -> `Type
  | Class_type_declaration   _ -> `Type
  | Method_call              _ -> `Expression
  | Module_binding_name      _ -> `Module
  | Module_declaration_name  _ -> `Module
  | Module_type_declaration_name _ -> `Module_type


open Protocol.Compl

let make_candidate ?get_doc ~attrs ~exact name ?loc ?path ty =
  let ident = match path with
    | Some path -> Ident.create (Path.last path)
    | None -> Extension.ident
  in
  let ppf, to_string = Format.to_string () in
  let kind =
    match ty with
    | `Value v ->
      Printtyp.type_scheme ppf v.Types.val_type;
      `Value
    | `Cons c  ->
      Browse_misc.print_constructor ppf c;
      `Constructor
    | `Label label_descr ->
      let desc =
        Types.(Tarrow (Raw_compat.Parsetree.arg_label_of_str "",
                       label_descr.lbl_res, label_descr.lbl_arg, Cok))
      in
      Printtyp.type_scheme ppf (Btype.newgenty desc);
      `Label
    | `Mod m   ->
      if exact then
        begin
          let verbosity = Fluid.get Type_utils.verbosity in
          match Type_utils.mod_smallerthan (1000 * verbosity) m with
          | None -> ()
          | Some _ -> Printtyp.modtype ppf m
        end;
      `Module
    | `ModType m ->
      if exact then
        Printtyp.modtype_declaration ident ppf ((*verbose_sig env*) m);
      `Modtype
    | `Typ t ->
      Printtyp.type_declaration ident ppf
        (*if exact then verbose_type_decl env t else*)( t);
      `Type
    | `Variant (label,arg) ->
      Format.pp_print_string ppf label;
      Option.iter ~f:(fun t ->
          Format.pp_print_string ppf " of ";
          Printtyp.type_scheme ppf t)
        arg;
      `Variant
  in
  (* FIXME: When suggesting variants (and constructors) with parameters,
     it could be nice to check precedence and add or not parenthesis.
  let name = match ty with
    | `Variant (_, Some _) -> "(" ^ name ^ " )"
    | _ -> name
  in*)
  let desc =
    match kind with
    | `Module | `Modtype -> ""
    | _ -> to_string ()
  in
  let info = match Raw_compat.read_doc_attributes attrs, get_doc, kind with
    | Some (str, _), _, _ -> str
    | None, _, (`Module | `Modtype) -> to_string ()
    | None, None, _ -> ""
    | None, Some get_doc, kind ->
      match path, loc with
      | Some p, Some loc ->
        let namespace = (* FIXME: that's just terrible *)
          match kind with
          | `Value -> `Vals
          | `Type -> `Type
          | _ -> assert false
        in
        begin match get_doc (`Completion_entry (namespace, p, loc)) with
          | `Found str -> str
          | _ -> ""
        end
      | _, _ -> ""
  in
  {name; kind; desc; info}

let item_for_global_module name = {name; kind = `Module; desc = ""; info = ""}

let fold_variant_constructors ~env ~init ~f =
  let rec aux acc t =
    let t = Ctype.repr t in
    match t.Types.desc with
    | Types.Tvariant { Types. row_fields; row_more; row_name } ->
      let acc =
        let keep_if_present acc (lbl, row_field) =
          match row_field with
          | Types.Rpresent arg when lbl <> "" -> f ("`" ^ lbl) arg acc
          | Types.Reither (_, lst, _, _) when lbl <> "" ->
            let arg =
              match lst with
              | [ well_typed ] -> Some well_typed
              | _ -> None
            in
            f ("`" ^ lbl) arg acc
          | _ -> acc
        in
        List.fold_left ~init:acc row_fields ~f:keep_if_present
      in
      let acc =
        match row_name with
        | None -> acc
        | Some (path,te) ->
          match (Env.find_type path env).Types.type_manifest with
          | None -> acc
          | Some te -> aux acc te
      in
      aux acc row_more
    | Types.Tconstr _ ->
      let t' = try Ctype.full_expand env t with _ -> t in
      if Types.TypeOps.equal t t' then
        acc
      else
        aux acc t'
    | _ -> acc
  in
  aux init

let get_candidates ?get_doc ?target_type prefix path kind ~validate env =
  let make_weighted_candidate ?(priority=0) ~attrs ~exact name ?loc ?path ty =
    (* Just like [make_candidate] but associates some metadata to the candidate.
       The candidates are later sorted using these metadata.

       The ordering works as follow:
       - first we compare the priority of the candidates
       - if they are equal, then we compare their "binding time": things
         introduced more recently will come before older bindings (i.e. we
         prioritize the local context)
       - if these are also equal, then we just use classic string ordering on
         the candidate name. *)
    let time =
      try Ident.binding_time (Path.head (Option.get path))
      with _ -> 0
    in
    let item = make_candidate ?get_doc ~attrs ~exact name ?loc ?path ty in
    (- priority, - time, name), item
  in
  let is_internal name = name = "" || name.[0] = '_' in
  let items =
    let snap = Btype.snapshot () in
    let rec arrow_arity n t =
      match (Ctype.repr t).Types.desc with
      | Types.Tarrow (_,_,rhs,_) -> arrow_arity (n + 1) rhs
      | _ -> n
    in
    let rec nth_arrow n t =
      if n <= 0 then t else
      match (Ctype.repr t).Types.desc with
      | Types.Tarrow (_,_,rhs,_) -> nth_arrow (n - 1) rhs
      | _ -> t
    in
    let type_check =
      (* Defines the priority of a candidate.
         Given the type of a candidate it will return:
         - 2 if the type can be unified with the expected one
         - 1 if a value of that type applied to some parameters returns a value
             of the expected type, i.e. if
                 target_type =                    a -> b -> c
                 type        = p1 -> ... -> pN -> a -> b -> c
         - 0 otherwise

         Note that if no type is expected (i.e. if we're not in an application),
         then 1 will be returned. *)
      match target_type with
      | None -> fun scheme -> 1
      | Some ty ->
        let arity = arrow_arity 0 ty in
        fun scheme ->
        let result =
          try Ctype.unify_var env ty (Ctype.instance env scheme); 2
          with _ ->
            let ty' = Ctype.instance env scheme in
            let arity = arrow_arity (-arity) ty' in
            let ty' = nth_arrow arity ty' in
            try Ctype.unify_var env ty ty'; 1
            with _ -> 0
        in
        Btype.backtrack snap;
        result
    in
    let rec of_kind = function
      | `Variants ->
        begin match target_type with
        | None -> []
        | Some t ->
          fold_variant_constructors t ~init:[] ~f:(fun name param candidates ->
            if not @@ validate `Variant `Variant name then candidates else
            make_weighted_candidate name ~exact:false ~priority:2 ~attrs:[]
                (`Variant (name, param))
            :: candidates
          ) ~env
        end

      | `Values ->
        let type_check {Types. val_type} = type_check val_type in
        Env.fold_values (fun name path v candidates ->
          if not (validate `Lident `Value name) then candidates else
          let priority = if is_internal name then 0 else type_check v in
          make_weighted_candidate ~exact:(name = prefix) name ~priority ~path
            ~attrs:(Raw_compat.val_attributes v)
            (`Value v) ~loc:v.Types.val_loc
          :: candidates
        ) path env []

      | `Constructor ->
        let type_check {Types. cstr_res} = type_check cstr_res in
        Raw_compat.fold_constructors (fun name v candidates ->
          if not @@ validate `Lident `Cons name then candidates else
          let priority = if is_internal name then 0 else type_check v in
          make_weighted_candidate ~exact:(name=prefix) name (`Cons v) ~priority
            ~attrs:(Raw_compat.cstr_attributes v)
          :: candidates
        ) path env []

      | `Types ->
        Raw_compat.fold_types (fun name path decl candidates ->
          if not @@ validate `Lident `Typ name then candidates else
          make_weighted_candidate ~exact:(name = prefix) name ~path (`Typ decl)
            ~loc:decl.Types.type_loc ~attrs:(Raw_compat.type_attributes decl)
          :: candidates
        ) path env []

      | `Modules ->
        Env.fold_modules (fun name path v candidates ->
          let v, attrs = Raw_compat.extract_module_declaration v in
          if not @@ validate `Uident `Mod name then candidates else
          make_weighted_candidate ~exact:(name = prefix) name ~path (`Mod v) ~attrs
          :: candidates
        ) path env []

      | `Modules_type ->
        Env.fold_modtypes (fun name path v candidates ->
          if not @@ validate `Uident `Mod name then candidates else
            make_weighted_candidate ~exact:(name=prefix) name ~path (`ModType v)
              ~attrs:(Raw_compat.mtd_attributes v)
            :: candidates
        ) path env []

      | `Labels ->
        Raw_compat.fold_labels (fun l candidates ->
          let {Types.lbl_name = name} = l in
          if not (validate `Lident `Label name) then candidates else
            make_weighted_candidate ~exact:(name = prefix) name (`Label l)
              ~attrs:(Raw_compat.lbl_attributes l)
            :: candidates
        ) path env []

      | `Group (kinds) -> List.concat_map ~f:of_kind kinds
    in
    of_kind kind
  in
  let items = List.sort items ~cmp:(fun (a,_) (b,_) -> compare a b) in
  let items = List.rev_map ~f:snd items in
  items

let gen_values = `Group [`Values; `Constructor]

let default_kinds = [`Variants; gen_values; `Types; `Modules; `Modules_type]

let completion_order = function
  | `Expression  -> [`Variants; gen_values; `Types; `Modules; `Modules_type]
  | `Structure   -> [gen_values; `Types; `Modules; `Modules_type]
  | `Pattern     -> [`Variants; `Constructor; `Modules; `Labels; `Values; `Types; `Modules_type]
  | `Module      -> [`Modules; `Modules_type; `Types; gen_values]
  | `Module_type -> [`Modules_type; `Modules; `Types; gen_values]
  | `Signature   -> [`Types; `Modules; `Modules_type; gen_values]
  | `Type        -> [`Types; `Modules; `Modules_type; gen_values]

let complete_methods ~env ~prefix obj =
  let t = obj.Typedtree.exp_type in
  let has_prefix (name,_) =
    String.is_prefixed ~by:prefix name &&
    (* Prevent identifiers introduced by type checker to leak *)
    try ignore (String.index name ' ' : int); false
    with Not_found -> true
  in
  let methods = List.filter has_prefix (methods_of_type env t) in
  List.map methods ~f:(fun (name,ty) ->
    let info = "" (* TODO: get documentation. *) in
    let ppf, to_string = Format.to_string () in
    Printtyp.type_scheme ppf ty;
    { name; kind = `MethodCall; desc = to_string (); info }
  )

let complete_prefix ?get_doc ?target_type ~env ~prefix ~is_label buffer node =
  let seen = Hashtbl.create 7 in
  let uniq n = if Hashtbl.mem seen n
    then false
    else (Hashtbl.add seen n (); true)
  in
  let find ?path ~is_label prefix =
    let valid tag name =
      try
        (* Prevent identifiers introduced by type checker to leak *)
        ignore (String.index name '-' : int);
        false
      with Not_found ->
        String.is_prefixed ~by:prefix name && uniq (tag,name)
    in
    (* Hack to prevent extensions namespace to leak *)
    let validate ident tag name =
      (if ident = `Uident
       then name <> "" && name.[0] <> '_'
       else name <> "_")
      && valid tag name
    in
    if not is_label then
      let kind = classify_node node.t_node in
      let order = completion_order kind in
      let add_completions acc kind =
        get_candidates ?get_doc ?target_type prefix path kind ~validate env @ acc
      in
      List.fold_left ~f:add_completions order ~init:[]
    else
      Raw_compat.fold_labels (fun ({Types.lbl_name = name} as l) candidates ->
        if not (valid `Label name) then candidates else
          make_candidate ?get_doc ~exact:(name = prefix) name (`Label l) ~attrs:[]
          :: candidates
      ) path env []
  in
  try
    match prefix with
    | Longident.Ldot (path, prefix) -> find ~path ~is_label prefix
    | Longident.Lident prefix ->
      let compl = find ~is_label prefix in
      (* Add modules on path but not loaded *)
      List.fold_left (Buffer.global_modules buffer) ~init:compl ~f:(
        fun candidates name ->
          let default = { name; kind = `Module; desc = ""; info = "" } in
          if name = prefix && uniq (`Mod, name) then
            try
              let path, (md, attrs) = Raw_compat.lookup_module (Longident.Lident name) env in
              make_candidate ?get_doc ~exact:true name ~path (`Mod md) ~attrs
              :: candidates
            with Not_found ->
              default :: candidates
          else if String.is_prefixed ~by:prefix name && uniq (`Mod,name) then
            default :: candidates
          else
            candidates
      )
    | _ -> find ~is_label (String.concat ~sep:"." @@ Longident.flatten prefix)
  with Not_found -> []

(* Propose completion from a particular node *)
let node_complete buffer ?get_doc ?target_type node prefix =
  let env = node.t_env in
  Printtyp.wrap_printing_env env @@ fun () ->
  match node.t_node with
  | Method_call (obj,_) -> complete_methods ~env ~prefix obj
  | Pattern    { Typedtree.pat_desc = Typedtree.Tpat_record (_, _) ; _ }
  | Expression { Typedtree.exp_desc = Typedtree.Texp_record (_, _) ; _ } ->
    let prefix, _is_label = Longident.(keep_suffix @@ parse prefix) in
    complete_prefix ?get_doc ?target_type ~env ~prefix ~is_label:true buffer node
  | x ->
    let prefix, is_label = Longident.(keep_suffix @@ parse prefix) in
    complete_prefix ?get_doc ?target_type ~env ~prefix ~is_label buffer node

let expand_prefix ~global_modules env prefix =
  let lidents, last =
    let ts = Expansion.explore ~global_modules env in
    Expansion.get_lidents ts prefix
  in
  let validate' =
    let last = Str.regexp (Expansion.regex_of_path_prefix last) in
    fun s -> Str.string_match last s 0
  in
  let validate _ _ s = validate' s in
  let process_lident lident =
    let candidates =
      let aux compl kind = get_candidates "" lident kind ~validate env @ compl in
      List.fold_left ~f:aux default_kinds ~init:[]
    in
    match lident with
    | None ->
      let f name =
        if not (validate' name) then None else
        Some (item_for_global_module name)
      in
      candidates @ List.filter_map global_modules ~f
    | Some lident ->
      let lident = Longident.flatten lident in
      let lident = String.concat ~sep:"." lident ^ "." in
      List.map candidates ~f:(fun c -> { c with name = lident ^ c.name })
  in
  List.concat_map ~f:process_lident lidents

open Typedtree

let labels_of_application ~env ~prefix f args =
  let rec labels t =
    let t = Ctype.repr t in
    match t.Types.desc with
    | Types.Tarrow (label, lhs, rhs, _) ->
      (Raw_compat.arg_label_to_str label, lhs) :: labels rhs
    | _ ->
      let t' = Ctype.full_expand env t in
      if Types.TypeOps.equal t t' then
        []
      else
        labels t'
  in
  let labels = labels f.exp_type in
  let is_application_of label (label',expr,_) =
    let label' = Raw_compat.arg_label_to_str label' in
    match expr with
    | Some {exp_loc = {Location. loc_ghost; loc_start; loc_end}} ->
      label = label'
      && label <> prefix
      && not loc_ghost
      && not (loc_start = loc_end)
    | None -> false
  in
  let unapplied_label (label,_) =
    label <> "" && not (List.exists (is_application_of label) args)
  in
  List.map (List.filter labels ~f:unapplied_label) ~f:(fun (label, ty) ->
    if label.[0] <> '?' then
      "~" ^ label, ty
    else
      match (Ctype.repr ty).Types.desc with
      | Types.Tconstr (path, [ty], _) when Path.same path Predef.path_option ->
        label, ty
      | _ -> label, ty
  )


let labels_of_application ?(prefix="") node =
  let prefix =
    if prefix <> "" && prefix.[0] = '~' then
      String.sub prefix ~pos:1 ~len:(String.length prefix - 1)
    else
      prefix
  in
  match node.exp_desc with
  | Texp_apply (f, args) -> labels_of_application ~prefix ~env:node.exp_env f args
  | _ -> []

let application_context ~verbosity ~prefix node ancestors =
  let module Printtyp = Type_utils.Printtyp in
  let target_type = ref (
    match node with
    | { t_node = Expression { exp_type = ty } }
    | { t_node = Pattern { pat_type = ty } } -> Some ty
    | _ -> None
  )
  in
  let context =
    match node, ancestors with
    | { t_node = Expression earg },
      { t_node = Expression ({ exp_desc = Texp_apply (efun, _);
                               exp_type = app_type; exp_env } as app) } :: _
      when earg != efun ->
      Printtyp.wrap_printing_env exp_env ~verbosity @@ fun () ->
      (* Type variables shared accross arguments should all be
         printed with the same name.
         [Printtyp.type_scheme] ensure that a name is unique within a given
         type, but not accross different invocations.
         [reset] followed by calls to [mark_loops] and [type_sch] provide
         that *)
      Printtyp.reset ();
      let pr t =
        let ppf, to_string = Format.to_string () in
        Printtyp.mark_loops t;
        Printtyp.type_sch ppf t;
        to_string ()
      in
      (* Special case for optional arguments applied with ~,
         get the argument wrapped inside Some _ *)
      let earg =
        match Raw_compat.optional_label_sugar earg.exp_desc with
        | None -> earg
        | Some earg ->
          target_type := Some earg.exp_type;
          earg
      in
      let labels = labels_of_application ~prefix app in
      `Application { Protocol.Compl.
                     argument_type = pr earg.exp_type;
                     labels = List.map (fun (lbl,ty) -> lbl, pr ty) labels;
                   }
    | _ -> `Unknown
  in
  !target_type, context
