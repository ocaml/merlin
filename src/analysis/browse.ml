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
open Browse_node

type node = Env.t * Browse_node.t
type t = node List.Non_empty.t

let node_loc node =
  let loc = Browse_node.node_update_loc Location.none node in
  if loc != Location.none then
    loc
  else
    let rec aux env loc node acc =
      if loc != Location.none then
        Parsing_aux.location_union loc acc
      else Browse_node.fold_node aux env loc node acc
    in
    aux Env.empty Location.none node Location.none

let leaf_node = List.Non_empty.hd
let leaf_loc t = node_loc (snd (leaf_node t))

let select_leafs pos root =
  let rec aux acc loc path =
    let select env loc node' acc =
      if Parsing_aux.compare_pos pos loc = 0
      then aux acc loc (List.More ((env, node'), path))
      else acc
    in
    let env, node = leaf_node path in
    let acc' = Browse_node.fold_node select env loc node acc in
    if acc == acc'
    then path :: acc
    else acc'
  in
  aux [] (leaf_loc root) root

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
    let f acc x =
      if compare_locations pos (leaf_loc acc) (leaf_loc x) <= 0
      then acc
      else x
    in
    Some (List.fold_left ~f ~init xs)

let deepest_before pos roots =
  Option.map (best_node pos roots) ~f:(fun root ->
      let rec aux loc0 path =
        let env0, node0 = leaf_node path in
        let candidate = Browse_node.fold_node
            (fun env loc node acc -> match acc with
             | Some (_,loc',_) when compare_locations pos loc' loc <= 0 -> acc
             | Some _ | None -> Some (env,loc,node)
            )
            env0 loc0 node0 None
        in
        match candidate with
        | None -> path
        | Some (env,loc,node) ->
          aux loc (List.More ((env,node),path))
      in
      aux (leaf_loc root) root
    )

let nearest_before pos roots =
  (* FIXME: wrong :) *)
  Option.bind (best_node pos roots) ~f:(fun root ->
      best_node pos (select_leafs pos root)
    )

let enclosing pos roots =
  Option.bind (best_node pos roots) ~f:(fun root ->
      best_node pos (select_leafs pos root)
    )

let of_structure str =
  List.One (str.str_final_env, Browse_node.Structure str)

let of_signature sg =
  List.One (sg.sig_final_env, Browse_node.Signature sg)

let of_typer_contents contents =
  let of_content (content,_) = match content with
    | `Str (_, `Ok str) -> Some (of_structure str)
    | `Sg (_, `Ok sg) -> Some (of_signature sg)
    | `Str (_, `Fail (env, loc)) | `Sg (_, `Fail (env, loc)) ->
      None
  in
  List.filter_map ~f:of_content contents

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
    | List.More ((_,node), (List.More ((_,node'), _) as ancestors))
      when is_recovered node' -> select ancestors
    | l -> l
  in
  match deepest_before pos_cursor structures with
  | Some path when skip_recovered -> select path
  | Some path -> path
  | None -> List.One (Typer.env typer, Browse_node.Dummy)
