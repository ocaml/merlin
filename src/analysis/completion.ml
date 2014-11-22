(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2014  Frédéric Bour  <frederic.bour(_)lakaban.net>
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

(*module Verbose_print = struct
  let type_scheme ppf t =
    let env = Printtyp.curr_printing_env () in
    Printtyp.type_scheme ppf (verbose_type env t)
  let type_declaration id ppf t =
    let env = Printtyp.curr_printing_env () in
    Printtyp.type_declaration id ppf (verbose_type_decl env t)
  let modtype_declaration id ppf t =
    let env = Printtyp.curr_printing_env () in
    Printtyp.modtype_declaration id ppf (verbose_sig env t)
end*)

(** Heuristic to find suitable environment to complete / type at given position.
 *  1. Try to find environment near given cursor.
 *  2. Check if there is an invalid construct between found env and cursor :
 *    Case a.
 *      > let x = valid_expr ||
 *      The env found is the right most env from valid_expr, it's a correct
 *      answer.
 *    Case b.
 *      > let x = valid_expr
 *      > let y = invalid_construction||
 *      In this case, the env found is the same as in case a, however it is
 *      preferable to use env from enclosing module rather than an env from
 *      inside x definition.
 *)
let node_at typer pos_cursor =
  let structures = Typer.contents typer in
  let structures = Browse.of_typer_contents structures in
  try
    let node, _pos_node =
      match Browse.deepest_before pos_cursor structures with
      | Some ({BrowseT. t_loc} as node) -> node, t_loc.Location.loc_end
      | None -> raise Not_found
    in
    node
  with Not_found ->
    {BrowseT.dummy with BrowseT.t_env = Typer.env typer}

(* List methods of an object.
 * Code taken from [uTop](https://github.com/diml/utop
 * with permission from Jeremie Dimino. *)
let lookup_env f x env =
  try Some (f x env)
  with Not_found | Env.Error _ -> None

let rec find_method env meth type_expr =
  let open Types in
  match type_expr.desc with
  | Tfield (name, _, ty, _) when name = meth -> Some ty
  | Tobject (type_expr, _) | Tpoly (type_expr, _)
  | Tlink type_expr | Tfield (_, _, _, type_expr) ->
    find_method env meth type_expr
  | Tconstr (path, _, _) -> begin
      match lookup_env Env.find_type path env with
      | None | Some { type_manifest = None } -> None
      | Some { type_manifest = Some type_expr } ->
        find_method env meth type_expr
    end
  | _ -> None

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

let classify_node =
  let open BrowseT in function
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


let completion_format ~exact name ?path ty =
  let ident = match path with
    | Some path -> Ident.create (Path.last path)
    | None -> Extension.ident
  in
  let ppf, to_string = Format.to_string () in
  let kind =
    match ty with
    | `Value v ->
      let v = (*if exact
                 then Types.({v with val_type = verbose_type env v.val_type})
                 else*) v
      in
      Printtyp.value_description ident ppf v;
      `Value
    | `Cons c  ->
      Format.pp_print_string ppf name;
      Format.pp_print_string ppf " : ";
      Browse_misc.print_constructor ppf c;
      `Constructor
    | `Label label_descr ->
      let desc =
        Types.(Tarrow ("", label_descr.lbl_res, label_descr.lbl_arg, Cok))
      in
      Format.pp_print_string ppf name;
      Format.pp_print_string ppf " : ";
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
  in
  let desc, info =
    match kind with
    | `Module|`Modtype -> "", to_string ()
    | _ -> to_string (), "" in
  {Protocol. name; kind; desc; info}

let completion_fold prefix path kind ~validate env compl =
  let fmt ~exact name ?path ty =
    let time =
      try Ident.binding_time (Path.head (Option.get path))
      with _ -> 0 in
    let item = completion_format ~exact name ?path ty in
    (- time, name), item in
  let items =
    match kind with
    | `Values ->
      Env.fold_values
        (fun name path v compl ->
           if validate `Lident `Value name
           then (fmt ~exact:(name = prefix) name ~path (`Value v)) :: compl
           else compl)
        path env []
    | `Constructor ->
      Merlin_types_custom.fold_constructors
        (fun name v compl ->
           if validate `Lident `Cons name
           then (fmt ~exact:(name = prefix) name (`Cons v)) :: compl
           else compl)
        path env []
    | `Types ->
      Merlin_types_custom.fold_types
        (fun name path decl compl ->
           if validate `Lident `Typ name
           then (fmt ~exact:(name = prefix) name ~path (`Typ decl)) :: compl
           else compl)
        path env []
    | `Modules ->
      Env.fold_modules
        (fun name path v compl ->
           let v = Merlin_types_custom.extract_module_declaration v in
           if validate `Uident `Mod name
           then (fmt ~exact:(name = prefix) name ~path (`Mod v)) :: compl
           else compl)
        path env []
    | `Modules_type ->
      Env.fold_modtypes
        (fun name path v compl ->
           if validate `Uident `Mod name
           then (fmt ~exact:(name = prefix) name ~path (`ModType v)) :: compl
           else compl)
        path env []
  in
  let items = List.sort items ~cmp:(fun (a,_) (b,_) -> compare a b) in
  let items = List.rev_map ~f:snd items in
  items @ compl

let completion_order = function
  | `Expression  -> [`Values; `Constructor; `Types; `Modules; `Modules_type]
  | `Structure   -> [`Values; `Constructor; `Types; `Modules; `Modules_type]
  | `Pattern     -> [`Constructor; `Modules; `Values; `Types; `Modules_type]
  | `Module      -> [`Modules; `Modules_type; `Types; `Constructor; `Values]
  | `Module_type -> [`Modules_type; `Modules; `Types; `Constructor; `Values]
  | `Signature   -> [`Types; `Modules; `Modules_type; `Constructor; `Values]
  | `Type        -> [`Types; `Modules; `Modules_type; `Constructor; `Values]

(* Propose completion from a particular node *)
let node_complete buffer node prefix =
  let prefix =
    let li = Longident.parse prefix in
    let suffix = Longident.last li in
    if suffix <> ""
      && Char.uppercase prefix.[0] <> prefix.[0]
      && Char.uppercase suffix.[0] = suffix.[0]
    then
      suffix
    else
      prefix
  in
  let env = node.BrowseT.t_env in
  let seen = Hashtbl.create 7 in
  let uniq n = if Hashtbl.mem seen n
    then false
    else (Hashtbl.add seen n (); true)
  in
  let find ?path prefix =
    let valid tag name =
      String.is_prefixed ~by:prefix name && uniq (tag,name)
    in
    (* Prevent identifiers introduced by type checker to leak *)
    let valid tag name =
      try
        ignore (String.index name '-' : int);
        false
      with Not_found -> valid tag name
    in
    (* Hack to prevent extensions namespace to leak *)
    let validate ident tag name =
      (if ident = `UIdent
       then name <> "" && name.[0] <> '_'
       else name <> "_")
      && valid tag name
    in
    try
      let kind = classify_node node.BrowseT.t_node in
      let order = completion_order kind in
      let add_completions kind compl =
        completion_fold prefix path kind ~validate env compl in
      List.fold_left' ~f:add_completions order ~init:[]
    with
    | exn ->
      (* Our path might be of the form [Some_path.record.Real_path.prefix] which
       * would explain why the previous cases failed.
       * We only keep [Real_path] for our path. *)
      let is_lowercase c = c = Char.lowercase c in
      let rec keep_until_lowercase li =
        let open Longident in
        match li with
        | Lident id when id <> "" && not (is_lowercase id.[0]) -> Some li
        | Ldot (path, id) when id <> "" && not (is_lowercase id.[0]) ->
          begin match keep_until_lowercase path with
          | None -> Some (Lident id)
          | Some path -> Some (Ldot (path, id))
          end
        | _ -> None
      in
      begin match path with
      | None -> raise exn (* clearly the hypothesis is wrong here *)
      | Some long_ident ->
        let path = keep_until_lowercase long_ident in
       Merlin_types_custom.fold_labels
          (fun ({Types.lbl_name = name} as l) compl ->
            if valid `Label name then
              (completion_format ~exact:(name = prefix) name (`Label l)) :: compl
            else compl)
          path env []
      end
  in
  Printtyp.wrap_printing_env env
  begin fun () ->
  match node.BrowseT.t_node with
  | BrowseT.Method_call (exp',_) ->
    let t = exp'.Typedtree.exp_type in
    let has_prefix (name,_) =
      String.is_prefixed ~by:prefix name &&
      (* Prevent identifiers introduced by type checker to leak *)
      try ignore (String.index name ' ' : int); false
      with Not_found -> true
    in
    let methods = List.filter has_prefix (methods_of_type env t) in
    List.map (fun (name,ty) ->
      let ppf, to_string = Format.to_string () in
      Printtyp.type_scheme ppf ty;
      {Protocol.
        name;
        kind = `MethodCall;
        desc = to_string ();
        info = "";
      })
      methods
  | _ ->
    try
      match Longident.parse prefix with
      | Longident.Ldot (path,prefix) -> find ~path prefix
      | Longident.Lident prefix ->
        (* Add modules on path but not loaded *)
        let compl = find prefix in
        List.fold_left (Buffer.global_modules buffer) ~init:compl
          ~f:begin fun compl modname ->
          let default = { Protocol.
            name = modname;
            kind = `Module;
            desc = "";
            info = "";
          } in
          match modname with
          | modname when modname = prefix && uniq (`Mod,modname) ->
            (try let path, md =
              Merlin_types_custom.lookup_module (Longident.Lident modname) env in
               completion_format ~exact:true modname ~path (`Mod md) :: compl
             with Not_found -> default :: compl)
          | modname when String.is_prefixed ~by:prefix modname && uniq (`Mod,modname) ->
            default :: compl
          | _ -> compl
        end
      | _ -> find prefix
    with Not_found -> []
  end

