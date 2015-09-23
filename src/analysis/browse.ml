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
open Typedtree
open BrowseT
open Browse_node

type t = Env.t * Location.t * Browse_node.t List.Non_empty.t

let select_leafs pos env loc node =
  let rec aux acc env loc path =
    let select env loc node' acc =
      if Parsing_aux.compare_pos pos loc = 0
      then aux acc env loc (List.More (node', path))
      else acc
    in
    let acc' = Browse_node.fold_node select env loc (List.Non_empty.hd path) acc in
    if acc == acc'
    then (env, loc, path) :: acc
    else acc'
  in
  aux [] env loc node

let t2_first = 1
let t1_first = -1

let compare_locations pos l1 l2 =
  match
    Parsing_aux.compare_pos pos l1,
    Parsing_aux.compare_pos pos l2
  with
  | 0, 0 ->
    (* Cursor inside both locations: favor non ghost and closer to the end *)
    let g1 = l1.Location.loc_ghost and g2 = l2.Location.loc_ghost in
    if g1 && not g2 then
      t2_first
    else if g2 && not g1 then
      t1_first
    else
      Lexing.compare_pos l1.Location.loc_end l2.Location.loc_end
  (* Cursor inside one location: it has priority *)
  | 0, _ -> t1_first
  | _, 0 -> t2_first
  (* Cursor outside locations: favor after *)
  | n, m when n > 0 && m < 0 -> t1_first
  | n, m when m > 0 && n < 0 -> t2_first
  (* Cursor is after both, select the closest one *)
  | _, _ ->
      Lexing.compare_pos l2.Location.loc_end l1.Location.loc_end

let best_node pos = function
  | [] -> None
  | init :: xs ->
    let prj (_env,loc,_node) = loc in
    let f acc x =
      if compare_locations pos (prj acc) (prj x) <= 0
      then acc
      else x
    in
    Some (List.fold_left ~f ~init xs)

let deepest_before pos roots =
  Option.map (best_node pos roots) ~f:(fun (env,loc,path) ->
      let rec aux env0 loc0 path =
        let candidate = Browse_node.fold_node
            (fun env loc node acc ->
               match acc with
               | Some (_,loc',_) when compare_locations pos loc' loc <= 0 -> acc
               | Some _ | None -> Some (env,loc,node)
            )
            env0 loc0 (List.Non_empty.hd path) None
        in
        match candidate with
        | None -> (env0,loc0,path)
        | Some (env,loc,node) ->
          aux env loc (List.More (node,path))
      in
      aux env loc path
    )

let nearest_before pos roots =
  Option.bind (best_node pos roots) ~f:(fun (env,loc,node) ->
      best_node pos (select_leafs pos env loc node)
    )

let enclosing pos roots =
  Option.bind (best_node pos roots) ~f:(fun (env,loc,node) ->
      best_node pos (select_leafs pos env loc node)
    )

let all_occurrences path =
  let rec aux acc t =
    let acc =
      let paths = node_paths t.t_node in
      let same l = Path.same path l.Location.txt in
      match List.filter ~f:same paths with
      | [] -> acc
      | paths -> (t, paths) :: acc
    in
    List.fold_left ~f:aux ~init:acc (Lazy.force t.t_children)
  in
  aux []

let fix_loc node =
  let _, loc = Browse_node.node_update_env
    BrowseT.default_env Location.none node in
  if loc != Location.none then loc else
    let rec aux env loc node acc =
      if loc != Location.none then
        Parsing_aux.location_union loc acc
  else Browse_node.fold_node aux env loc node acc
  in
  aux BrowseT.default_env Location.none node Location.none

let of_structure str =
  let env  = str.str_final_env in
  let node = Browse_node.Structure str in
  let loc  = fix_loc node in
  (env, loc, List.One node)

let of_signature sg =
  let env  = sg.sig_final_env in
  let node = Browse_node.Signature sg in
  let loc  = fix_loc node in
  (env, loc, List.One node)

let of_typer_contents contents =
  let of_content (content,_) = match content with
    | `Str (_, `Ok str) -> Some (of_structure str)
    | `Sg (_, `Ok sg) -> Some (of_signature sg)
    | `Str (_, `Fail (env, loc)) | `Sg (_, `Fail (env, loc)) ->
      None
  in
  List.filter_map ~f:of_content contents

let rec normalize_type_expr env = function
  | {Types.desc = Types.Tconstr (path,_,_)} ->
    normalize_type_decl env (Env.find_type path env)
  | _ -> raise Not_found

and normalize_type_decl env decl = match decl.Types.type_manifest with
  | Some expr -> normalize_type_expr env expr
  | None -> decl

let same_constructor env a b =
  let name = function
    | `Description d -> d.Types.cstr_name
    | `Declaration d -> Ident.name d.Typedtree.cd_id
  in
  if name a <> name b then false
  else begin
    let get_decls = function
      | `Description d ->
        let ty = normalize_type_expr env d.Types.cstr_res in
        begin match ty.Types.type_kind with
        | Types.Type_variant decls ->
          List.map decls ~f:Raw_compat.id_of_constr_decl
        | _ -> assert false
        end
      | `Declaration d ->
        [d.Typedtree.cd_id]
    in
    let a = get_decls a in
    let b = get_decls b in
    List.exists a ~f:(fun id -> List.exists b ~f:(Ident.same id))
  end

let all_constructor_occurrences ({t_env = env},d) t =
  let rec aux acc t =
    let acc =
      match node_is_constructor t.t_node with
      | Some d' when same_constructor env d d'.Location.txt ->
        {d' with Location.txt = t} :: acc
      | _ -> acc
    in
    List.fold_left ~f:aux ~init:acc (Lazy.force t.t_children)
  in
  aux [] t

let annotate_tail_calls ts : (Browse_node.t * Protocol.is_tail_position) list =
  let is_one_of candidates node = List.mem node ~set:candidates in
  let find_entry_points candidates node =
    Tail_analysis.entry_points node,
    (node, is_one_of candidates node) in
  let _, entry_points = List.fold_n_map ts ~f:find_entry_points ~init:[] in
  let propagate candidates (node,entry) =
    let is_in_tail = entry || is_one_of candidates node in
    (if is_in_tail
     then Tail_analysis.tail_positions node
     else []),
    (node, is_in_tail) in
  let _, tail_positions = List.fold_n_map entry_points ~f:propagate ~init:[] in
  List.map ~f:(fun (node,tail) ->
      node,
      if not tail then
        `No
      else if Tail_analysis.is_call node then
        `Tail_call
      else
        `Tail_position)
    tail_positions

let annotate_tail_calls_from_leaf ts =
  let ts = List.Non_empty.to_list ts in
  List.rev (annotate_tail_calls (List.rev ts))

let rec is_recovered_expression = function
  | (* Recovery on arbitrary expressions *)
    { Typedtree.exp_desc = Typedtree.Texp_tuple [_] } ->
    true
  | (* Recovery on unbound identifier *)
    { Typedtree.exp_desc = Typedtree.Texp_ident (Path.Pident id, _, _) }
    when Ident.name id = "*type-error*" ->
    true
  | (* Recovery on desugared optional label application *)
    { Typedtree.exp_desc = (Typedtree.Texp_construct _ as cstr) }
    when is_recovered_Texp_construct cstr ->
    true
  | _ -> false

and is_recovered_Texp_construct cstr =
  match Raw_compat.optional_label_sugar cstr with
  | Some e -> is_recovered_expression e
  | _ -> false

let is_recovered = function
  | Expression e -> is_recovered_expression e
  | _ -> false

(** Heuristic to find suitable environment to complete / type at given position.
    1. Try to find environment near given cursor.
    2. Check if there is an invalid construct between found env and cursor :
      Case a.
        > let x = valid_expr ||
        The env found is the right most env from valid_expr, it's a correct
        answer.
      Case b.
        > let x = valid_expr
        > let y = invalid_construction||
        In this case, the env found is the same as in case a, however it is
        preferable to use env from enclosing module rather than an env from
        inside x definition.
 *)
let node_at ?(skip_recovered=false) typer pos_cursor =
  let open Merlin_lib in
  let structures = Typer.contents typer in
  let structures = of_typer_contents structures in
  let rec select = function
    (* If recovery happens, the incorrect node is kept and a recovery node
       is introduced, so the node to check for recovery is the second one. *)
    | List.More (node, (List.More (node', _) as ancestors))
      when skip_recovered && is_recovered node' -> select ancestors
    | l -> l
  in
  match deepest_before pos_cursor structures with
  | Some (env, loc, path) -> (env, loc, select path)
  | None -> (Typer.env typer , Location.none, List.One Browse_node.Dummy)
