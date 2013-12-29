(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013  Frédéric Bour  <frederic.bour(_)lakaban.net>
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
open Global_state

type fixme = { fixme: unit }

type step = fixme
type t = fixme

let verbosity =
  let counter = ref 0 in
  fun cmd ->
    begin match cmd with
    | `Query -> ()
    | `Incr  -> incr counter
    | `Clear -> counter := 0
    end;
    !counter

let verbose_type env ty =
  if verbosity `Query > 0
  then (Ctype.full_expand env ty)
  else ty

let verbose_type_decl env ty =
  match ty.Types.type_manifest with
  | Some m -> {ty with Types.type_manifest = Some (verbose_type env m)}
  | None -> ty

let verbose_sig env m =
  let open Types in
  let rec expand verbosity = function
    | Modtype_manifest (Mty_ident p) when verbosity > 0 ->
      expand (pred verbosity)
             (Modtype_manifest (Env.find_modtype_expansion p env))
    | m -> m
  in
  expand (verbosity `Query) m

module Verbose_print = struct
  let type_scheme ppf t =
    let env = Printtyp.curr_printing_env () in
    Printtyp.type_scheme ppf (verbose_type env t)
  let type_declaration id ppf t =
    let env = Printtyp.curr_printing_env () in
    Printtyp.type_declaration id ppf (verbose_type_decl env t)
  let modtype_declaration id ppf t =
    let env = Printtyp.curr_printing_env () in
    Printtyp.modtype_declaration id ppf (verbose_sig env t)
end

let retype state =
  failwith "TODO"
  (*let init x = x
  and fold old prev =
    let outlines = old.outlines in
    let chunks = Chunk.update outlines (Some prev.chunks) in
    let types = Typer.update chunks (Some prev.types) in
    {outlines; chunks; types}
  in
  { state with steps = History.reconstruct state.steps ~init ~fold }*)

(** Heuristic to speed-up reloading of CMI files that has changed *)
let quick_refresh_modules state =
  if Env.check_cache_consistency ()
  then state, false
  else (Env.reset_cache (); retype state, true)

let browse step =
  failwith "TODO"
  (*List.concat_map
    ~f:(fun {Location.txt} -> Browse.structure txt)
    Typer.trees step.types*)

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
let node_at state pos_cursor =
  failwith "TODO"
  (* let step = History.focused state.steps in
  let structures = browse step in
  let cmp o = Merlin_parsing.compare_pos pos_cursor (Outline.location o) in
  let outlines =
    let rec aux o =
      match Outline.Spine.previous o with
      | Some o' when cmp o < 0 -> aux o'
      | Some _ | None -> o
    in
    aux step.outlines
  in
  try
    let node, pos_node =
      match Browse.nearest_before pos_cursor structures with
      | Some ({ Browse.loc } as node) -> node, loc.Location.loc_end
      | None -> raise Not_found
    in
    match Outline.location outlines with
    | { Location.loc_start } when
        Lexing.(compare_pos loc_start pos_node > 0 &&
                  compare_pos pos_cursor loc_start > 0) ->
      raise Not_found
    | _ -> node
  with Not_found ->
    let chunks = Chunk.update outlines (Some step.chunks) in
    let types  = Typer.update chunks   (Some step.types)  in
    Browse.({ dummy with env = Typer.env types })*)

let local_modules_at state pos_cursor =
  failwith "TODO"
  (*let step = History.focused state.steps in
  let cmp o = Merlin_parsing.compare_pos pos_cursor (Outline.location o) in
  let outlines =
    let rec aux o =
      match Outline.Spine.previous o with
      | Some o' when cmp o < 0 -> aux o'
      | Some _ | None -> o
    in
    aux step.outlines
  in
  let chunks = Chunk.update outlines (Some step.chunks) in
  Chunk.local_modules chunks*)

let str_items_before state pos_cursor =
  failwith "TODO"
  (*let step = History.focused state.steps in
  let cmp o = Merlin_parsing.compare_pos pos_cursor (Outline.location o) in
  let outlines =
    let rec aux o =
      match Outline.Spine.previous o with
      | Some o' when cmp o < 0 -> aux o'
      | Some _ | None -> o
    in
    aux step.outlines
  in
  let chunks = Chunk.update outlines (Some step.chunks) in
  let types  = Typer.update chunks (Some step.types) in
  Typer.trees types*)

(* Gather all exceptions in state (warnings, syntax, env, typer, ...) *)
let exns state =
  failwith "TODO"
  (*let step = History.focused state.steps in
  Outline.exns step.outlines
  @ Chunk.exns step.chunks
  @ Typer.exns step.types*)

(* Check if module is smaller (= has less definition, counting nested ones)
 * than a particular threshold. Return (Some n) if module has size n, or None
 * otherwise (module is bigger than threshold).
 * Used to skip printing big modules in completion. *)
let rec mod_smallerthan n m =
  if n < 0 then None
  else
  let open Types in
  match m with
  | Mty_ident _ -> Some 1
  | Mty_signature (lazy s) ->
    begin match List.length_lessthan n s with
    | None -> None
    | Some _ ->
      List.fold_left s ~init:(Some 0)
      ~f:begin fun acc item ->
        match acc, item with
        | None, _ -> None
        | Some n', _ when n' > n -> None
        | Some n1, Sig_modtype (_,Modtype_manifest m)
        | Some n1, Sig_module (_,m,_) ->
          (match mod_smallerthan (n - n1) m with
           | Some n2 -> Some (n1 + n2)
           | None -> None)
        | Some n', _ -> Some (succ n')
      end
    end
  | Mty_functor (_,m1,m2) ->
    begin
      match mod_smallerthan n m1 with
      | None -> None
      | Some n1 ->
      match mod_smallerthan (n - n1) m2 with
      | None -> None
      | Some n2 -> Some (n1 + n2)
    end

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

(* Propose completion from a particular node *)
let node_complete node prefix =
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
  let {Browse.env} = node in
  let fmt ~exact name ?path ty =
    let ident = match path with
      | Some path -> Ident.create (Path.last path)
      | None -> Extension.ident
    in
    let ppf, to_string = Format.to_string () in
    let kind =
      match ty with
      | `Value v ->
        let v = if exact
          then Types.({v with val_type = verbose_type env v.val_type})
          else v
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
         begin match mod_smallerthan (2000 * verbosity `Query) m with
           | None -> ()
           | Some _ -> Printtyp.modtype ppf m
         end;
         `Module
      | `ModType m ->
        if exact then
          Printtyp.modtype_declaration ident ppf (verbose_sig env m);
        `Modtype
      | `Typ t ->
        Printtyp.type_declaration ident ppf
          (if exact then verbose_type_decl env t else t);
        `Type
    in
    let desc, info = match kind with `Module|`Modtype -> "", to_string () | _ -> to_string (), "" in
    {Protocol. name; kind; desc; info}
  in
  let seen = Hashtbl.create 7 in
  let uniq n = if Hashtbl.mem seen n
    then false
    else (Hashtbl.add seen n (); true)
  in
  let find ?path prefix =
    let valid tag n = String.is_prefixed ~by:prefix n && uniq (tag,n) in
    (* Hack to prevent extensions namespace to leak *)
    let valid ?(uident=false) tag name =
      (if uident
       then name <> "" && name.[0] <> '_'
       else name <> "_")
      && valid tag name
    in
    let compl = [] in
    try
      let compl = Env.fold_values
        (fun name path v compl ->
          if valid `Value name
          then (fmt ~exact:(name = prefix) name ~path (`Value v)) :: compl
          else compl)
        path env compl
      in
      let compl = Merlin_types.fold_constructors
        (fun name v compl ->
           if valid `Cons name
           then (fmt ~exact:(name = prefix) name (`Cons v)) :: compl
           else compl)
        path env compl
      in
      let compl = Merlin_types.fold_types
        (fun name path decl compl ->
          if valid `Typ name
          then (fmt ~exact:(name = prefix) name ~path (`Typ decl)) :: compl
          else compl)
        path env compl
      in
      let compl = Env.fold_modules
        (fun name path v compl ->
          if valid ~uident:true `Mod name
          then (fmt ~exact:(name = prefix) name ~path (`Mod v)) :: compl
          else compl)
        path env compl
      in
      let compl = Env.fold_modtypes
        (fun name path v compl ->
          if valid ~uident:true `Mod name
          then (fmt ~exact:(name = prefix) name ~path (`ModType v)) :: compl
          else compl)
        path env compl
      in
      compl
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
        Merlin_types.fold_labels
          (fun ({Types.lbl_name = name} as l) compl ->
            if valid `Label name then (fmt ~exact:(name = prefix) name (`Label l)) :: compl else compl)
          path env compl
      end
  in
  Printtyp.wrap_printing_env env
  begin fun () ->
  match node.Browse.context with
  | Browse.MethodCall (t,_) ->
    let has_prefix (name,_) = String.is_prefixed ~by:prefix name in
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
        begin match List.length_lessthan 30 compl with
        | Some _ -> List.fold_left (Project.global_modules ()) ~init:compl
          ~f:begin fun compl modname ->
          let default = { Protocol.
            name = modname;
            kind = `Module;
            desc = "";
            info = "";
          } in
          match modname with
          | modname when modname = prefix && uniq (`Mod,modname) ->
              (try let path, md = Env.lookup_module (Longident.Lident modname) env in
                fmt ~exact:true modname ~path (`Mod md) :: compl
              with Not_found -> default :: compl)
          | modname when String.is_prefixed ~by:prefix modname && uniq (`Mod,modname) ->
            default :: compl
          | _ -> compl
          end
        | None -> compl
        end
      | _ -> find prefix
    with Not_found -> []
  end

let validate_parser t =
  failwith "TODO"
  (*if !(t.parser_validity)
  then t
  else
    let steps = History.seek_backward (fun _ -> true) t.steps in
    let step = History.focused steps in
    initial step*)

