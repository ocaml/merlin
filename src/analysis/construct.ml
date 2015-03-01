(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2015  Frédéric Bour  <frederic.bour(_)lakaban.net>
                             Thomas Refis  <refis.thomas(_)gmail.com>
                             Simon Castellan  <simon.castellan(_)iuwt.fr>
                             Arthur Wendling  <art.wendling(_)gmail.com>

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
open BrowseT

let section = Logger.Section.of_string "construct"

exception Not_allowed of string

let () =
  Location.register_error_of_exn (function
    | Not_allowed s  -> Some (Location.error ("Construct not allowed on " ^ s))
    | _ -> None
  )

let mk_id s  = Location.mknoloc (Longident.Lident s)
let mk_var s = Location.mknoloc s

module Predef_types = struct
  let unit_ () =
    Ast_helper.Exp.construct (mk_var (Longident.Lident "()")) None

  let char_ () =
    Ast_helper.Exp.constant (Asttypes.Const_char 'c')

  let int_ () =
    Ast_helper.Exp.constant (Asttypes.Const_int 0)

  let string_ () =
    Ast_helper.Exp.constant (Ast_helper.const_string "")

  let list_ () =
    Ast_helper.Exp.construct (mk_var (Longident.Lident "[]")) None

  let array_ () =
    Ast_helper.Exp.array []

  let tbl = Hashtbl.create 6

  let () =
    List.iter ~f:(fun (k, v) -> Hashtbl.add tbl k v) [
      Predef.path_unit, unit_ ;
      Predef.path_char, char_ ;
      Predef.path_int, int_ ;
      Predef.path_string, string_ ;
      Predef.path_list, list_ ;
      Predef.path_array, array_ ;
    ]
end

let bind name t env =
  let vd = {
    Types.val_type = t ;
    val_kind = Types.Val_unbound ;
    val_loc = Location.none ;
    val_attributes = []
  } in
  Env.add_value (Ident.create name) vd env

let already_defined name env =
  try let _ = Env.lookup_value (Longident.Lident name) env in true
  with Not_found -> false

let freevar ?(prefix = "") t env =
  let rec to_string i =
    let n, m = i mod 26, i / 26 in
    let s = String.make 1 (Char.chr (Char.code 'a' + n)) in
    if m = 0
    then s
    else to_string (m - 1) ^ s in
  let rec go i =
    let name = prefix ^ to_string i in
    if already_defined name env
    then go (i + 1)
    else name in
  let name = go 0 in
  name, bind name t env

let hole t env =
  let name, env' = freevar ~prefix:"_" t env in
  let hole = Ast_helper.Exp.ident (mk_id name) in
  hole, env'

let prefix env path =
  let path = Printtyp.shorten_path ~env path in
  match Path.to_string_list path with
  | [] -> assert false
  | p :: ps ->
    fun name ->
      let open Longident in
      match
        List.fold_left ps ~init:(Lident p) ~f:(fun lid p -> Ldot (lid, p))
      with
      | Lident _ -> Lident name
      | Ldot (lid, _) -> Ldot (lid, name)
      | _ -> assert false

let rec gen_expr env type_expr =
  let open Types in
  let type_expr = Btype.repr type_expr in
  match type_expr.desc with
  | Tlink _    -> assert false (* impossible after [Btype.repr] *)
  | Tvar _     -> raise (Not_allowed "non-immediate type")
  | Tobject _  -> raise (Not_allowed "object type")
  | Tconstr (path, params, _) ->
    begin try Hashtbl.find Predef_types.tbl path (), env
    with Not_found ->
      match Env.find_type_descrs path env with
      | [], labels when labels <> [] ->
          let fields, env' =
            List.fold_left labels
              ~init:([], env)
              ~f:(fun (fields, env) lbl ->
                 Ctype.unify env lbl.lbl_res type_expr ;
                 let expr, env' = gen_expr env lbl.lbl_arg in
                 let field =
                   match fields with
                   | [] -> mk_var (prefix env path lbl.lbl_name)
                   | _ -> mk_id lbl.lbl_name in
                 (field, expr) :: fields, env') in
          let fields = List.rev fields in
          Ast_helper.Exp.record fields None, env'
      | constrs, [] ->
        let are_types_unifiable typ =
          let snap = Btype.snapshot () in
          let res =
            try Ctype.unify_gadt ~newtype_level:0 (ref env) type_expr typ ; true
            with Ctype.Unify _trace -> false in
          Btype.backtrack snap ;
          res in
        let constrs =
          List.filter_map constrs ~f:(fun cstr_descr ->
            if cstr_descr.cstr_generalized
               && not (are_types_unifiable cstr_descr.cstr_res)
            then None
            else Some cstr_descr) in
        begin match constrs with
          | [] -> raise (Not_allowed "no constructor")
          | cstr_descr :: _ ->
            let args, env' =
              if cstr_descr.cstr_arity <= 0
              then None, env
              else let t, env' = gen_tuple env cstr_descr.cstr_args in
                   Some t, env' in
            let lidl = mk_var (prefix env path cstr_descr.cstr_name) in
            Ast_helper.Exp.construct lidl args, env
        end
      | _ -> raise (Not_allowed "constr")
      end
  | Tpackage (path, ids, args) -> raise (Not_allowed "modules")
  | Tvariant row_desc -> raise (Not_allowed "variant type")

  | Ttuple ts ->
    gen_tuple env ts

  | Tarrow (label, t0, t1, _) ->
    let lbl =
      if label <> "" && label.[0] = '?'
      then String.sub label 1 (String.length label - 1)
      else label in
    let name, env' =
      if lbl = "" || already_defined lbl env
      then freevar t0 env
      else lbl, bind lbl t0 env in
    let out, env'' = try gen_expr env' t1 with Not_allowed _ -> hole t1 env' in
    let ast =
      Ast_helper.Exp.fun_ label None
        (Ast_helper.Pat.var (mk_var name))
        out in
    ast, env''

  | _ ->
    let fmt, to_string = Format.to_string () in
    Printtyp.type_expr fmt type_expr ;
    raise (Not_allowed (to_string ()))

and gen_tuple env types =
  let rec go acc env = function
    | [] -> List.rev acc, env
    | t::ts ->
      let h, env' = try gen_expr env t with Not_allowed _ -> hole t env in
      go (h::acc) env' ts in
  let holes, env' = go [] env types in
  Ast_helper.Exp.tuple holes, env'


let needs_parentheses e = match e.Parsetree.pexp_desc with
  | Parsetree.Pexp_fun _ -> true
  | _ -> false

let node ~loc ~env parents node =
  match node.t_node with
  | Expression expr ->
    let ty = expr.Typedtree.exp_type in
    let result, _ = gen_expr env ty in
    let fmt, to_string = Format.to_string () in
    Pprintast.expression fmt result ;
    let str = to_string () in
    let str = if needs_parentheses result then "(" ^ str ^ ")" else str in
    loc, str
  | node ->
    raise (Not_allowed (BrowseT.string_of_node node))
