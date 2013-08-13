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

type t = {
  pos      : Lexing.position;
  tokens   : Outline.token list;
  comments : Lexer.comment list;
  outlines : Outline.t;
  chunks   : Chunk.t;
  types    : Typer.t;
}

let initial = {
  pos      = Lexing.({pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 0});
  tokens   = [];
  comments = [];
  outlines = History.empty;
  chunks   = History.empty;
  types    = History.empty;
}

(* FIXME: 
 * Pathes are global, but once support for different pathes has been added to 
 * typer, this should be made a [state] wide property.
 * *)
let source_path : string list ref = ref []
let global_modules = ref (lazy [])

let reset_global_modules () =
  let paths = !Config.load_path in
  global_modules := lazy (Misc.modules_in_path ~ext:".cmi" paths)

(** Heuristic to speed-up reloading of CMI files that has changed *)
let quick_refresh_modules state =
  if Env.quick_reset_cache () then
  begin
    let types = Typer.sync state.chunks History.empty in
    {state with types}, true
  end
  else state, false

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
  let structures = Misc.list_concat_map
    (fun (str,sg) -> Browse.structure str)
    (Typer.trees state.types)
  in
  let cmp o = Location.compare_pos pos_cursor (Outline.item_loc o) in
  let outlines = History.seek_backward (fun o -> cmp o < 0) state.outlines in
  try
    let node, pos_node =
      match Browse.nearest_before pos_cursor structures with
      | Some ({ Browse.loc } as node) -> node, loc.Location.loc_end
      | None -> raise Not_found
    in
    match Outline.location outlines with
    | { Location.loc_start } when
        Misc.(compare_pos loc_start pos_node > 0 &&
              compare_pos pos_cursor loc_start > 0) ->
      raise Not_found
    | _ -> node
  with Not_found ->
    let _, chunks = History.Sync.rewind fst outlines state.chunks in
    let _, types = History.Sync.rewind fst chunks state.types in
    Browse.({ dummy with env = Typer.env types })

(* Gather all exceptions in state (warnings, syntax, env, typer, ...) *)
let exns state =
  Outline.exns state.outlines
  @ Chunk.exns state.chunks
  @ Typer.exns state.types

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
  | Mty_signature s ->
    begin match Misc.length_lessthan n s with
    | None -> None
    | Some n' ->
      List.fold_left
      begin fun acc item ->
        match acc, item with
        | None, _ -> None
        | Some n', _ when n' > n -> None
        | Some n1, Sig_modtype (_,Modtype_manifest m)
        | Some n1, Sig_module (_,m,_) ->
          (match mod_smallerthan (n - n1) m with
           | Some n2 -> Some (n1 + n2)
           | None -> None)
        | Some n', _ -> Some (succ n')
      end (Some 0) s
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
  let {Browse.env} = node in
  let fmt ~exact name path ty =
    let ident = Ident.create (Path.last path) in
    let ppf, to_string = Misc.ppf_to_string () in
    let kind =
      match ty with
      | `Value v -> Printtyp.value_description ident ppf v; "Value"
      | `Cons c  ->
          Format.pp_print_string ppf name;
          Format.pp_print_string ppf " : ";
          Browse_misc.print_constructor ppf c;
          "Constructor"
      | `Label label_descr ->
          let desc =
            Types.(Tarrow ("", label_descr.lbl_res, label_descr.lbl_arg, Cok))
          in
          Format.pp_print_string ppf name;
          Format.pp_print_string ppf " : ";
          Printtyp.type_scheme ppf (Btype.newgenty desc);
          "Label"
      | `Mod m   ->
          (if exact then
             match mod_smallerthan 200 m with
               | None -> ()
               | Some _ -> Printtyp.modtype ppf m
          ); "Module"
      | `Typ t ->
          Printtyp.type_declaration ident ppf t; "Type"
    in
    let desc, info = match kind with "Module" -> "", to_string () | _ -> to_string (), "" in
    `Assoc ["name", `String name ; "kind", `String kind ; "desc", `String desc ; "info", `String info]
  in
  let seen = Hashtbl.create 7 in
  let uniq n = if Hashtbl.mem seen n
    then false
    else (Hashtbl.add seen n (); true)
  in
  let find ?path prefix compl =
    let valid tag n = Misc.has_prefix prefix n && uniq (tag,n) in
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
          then (fmt ~exact:(name = prefix) name path (`Value v)) :: compl 
          else compl)
        path env compl
      in
      let compl = Env.fold_constructors
        (fun name path v compl ->
          if valid `Cons name 
          then (fmt ~exact:(name = prefix) name path (`Cons v)) :: compl 
          else compl)
        path env compl
      in
      let compl = Env.fold_types
        (fun name path v compl ->
          if valid `Typ name 
          then (fmt ~exact:(name = prefix)  name path (`Typ v)) :: compl 
          else compl)
        path env compl
      in
      let compl = Env.fold_modules
        (fun name path v compl ->
          if valid ~uident:true `Mod name 
          then (fmt ~exact:(name = prefix)  name path (`Mod v)) :: compl 
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
        Env.fold_labels
          (fun name path l compl ->
            if valid `Label name then (fmt ~exact:(name = prefix) name path (`Label l)) :: compl else compl)
          path env compl
      end
  in
  Printtyp.wrap_printing_env env
  begin fun () ->
  match node.Browse.context with
  | Browse.MethodCall (t,_) ->
    let has_prefix (name,_) = Misc.has_prefix prefix name in
    let methods = List.filter has_prefix (methods_of_type env t) in
    List.map (fun (name,ty) ->
      let ppf, to_string = Misc.ppf_to_string () in
      Printtyp.type_scheme ppf ty;
      `Assoc ["name", `String name ; "kind", `String "#" ;
              "desc", `String (to_string ()) ;
              "info", `String ""])
      methods
  | _ ->
    try
      match Longident.parse prefix with
      | Longident.Ldot (path,prefix) -> find ~path prefix []
      | Longident.Lident prefix ->
        (* Add modules on path but not loaded *)
        let compl = find prefix [] in
        begin match Misc.length_lessthan 30 compl with
        | Some _ -> List.fold_left
          begin fun compl modname ->
          let default = `Assoc [
              "name", `String modname;
              "kind", `String "module";
              "desc", `String "" ;
              "info", `String ""
            ] in 
          match modname with
          | modname when modname = prefix && uniq (`Mod,modname) ->
              (try let p, md = Env.lookup_module (Longident.Lident modname) env in
                fmt ~exact:true modname p (`Mod md) :: compl
              with Not_found -> default :: compl)
          | modname when Misc.has_prefix prefix modname && uniq (`Mod,modname) ->
            default :: compl
          | _ -> compl
          end compl (Lazy.force !global_modules)
        | None -> compl
        end
      | _ -> find prefix []
    with Not_found -> []
  end

and locate node path_str =
  Track_definition.from_string
    ~sources:(!source_path)
    ~env:(node.Browse.env)
    path_str
