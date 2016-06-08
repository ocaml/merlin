(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2015  Frédéric Bour  <frederic.bour(_)lakaban.net>
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

type node = Browse_node.t
type t = (Env.t * node) List.Non_empty.t

let node_of_binary_part env part =
  let open Cmt_format in
  match part with
  | Partial_structure x ->
    Browse_node.Structure x
  | Partial_structure_item x ->
    Browse_node.Structure_item (x, env)
  | Partial_expression x ->
    Browse_node.Expression x
  | Partial_pattern x ->
    Browse_node.Pattern x
  | Partial_class_expr x ->
    Browse_node.Class_expr x
  | Partial_signature x ->
    Browse_node.Signature x
  | Partial_signature_item x ->
    Browse_node.Signature_item (x, env)
  | Partial_module_type x ->
    Browse_node.Module_type x

let fold_node f env t acc =
  let acc = match
      Cmt_format.saved_types_from_attributes
        (Browse_node.node_attributes t)
    with
    | [] -> acc
    | parts ->
      let rec aux acc = function
        | [] -> acc
        | part :: parts ->
          let t = node_of_binary_part env part in
          aux (f (Browse_node.node_update_env env t) t acc) parts
      in
      aux acc parts
  in
  Browse_node.fold_node f env t acc

let approximate_loc get_loc node =
  let loc = get_loc Location.none node in
  if loc == Location.none then
    let rec aux env node acc =
      let loc = get_loc Location.none node in
      if loc != Location.none then
        Location_aux.union loc acc
      else fold_node aux env node acc
    in
    aux Env.empty node Location.none
  else
    loc

let node_loc node = approximate_loc Browse_node.node_real_loc node

(* Fuzzy locations, more likely to locate the appropriate node *)
let node_merlin_loc node = approximate_loc Browse_node.node_merlin_loc node

let leaf_node = List.Non_empty.hd
let leaf_loc t = node_loc (snd (leaf_node t))

exception Merlin_only of t list

let has_attr attr attrs =
 List.exists ~f:(fun (str,_) -> str.Location.txt = attr) attrs

let select_leafs pos root =
  let rec aux acc path =
    let select env node acc =
      let loc = node_merlin_loc node in
      if Location_aux.compare_pos pos loc = 0
      then aux acc (List.More ((env, node), path))
      else acc
    in
    let env, node = leaf_node path in
    let attrs = Browse_node.node_attributes node in
    if has_attr "merlin.ignore" attrs then
      acc
    else if has_attr "merlin.teresting" attrs then
      let acc' = fold_node select env node [] in
      raise (Merlin_only (if [] == acc' then [path] else acc'))
    else
      let acc' = fold_node select env node acc in
      if acc == acc'
      then path :: acc
      else acc'
  in
  try aux [] root
  with Merlin_only t -> t

let compare_locations pos l1 l2 =
  let t2_first = +1 in
  let t1_first = -1 in
  match
    Location_aux.compare_pos pos l1,
    Location_aux.compare_pos pos l2
  with
  | 0, 0 ->
    (* Cursor inside both locations: favor closer to the end *)
    Lexing.compare_pos l1.Location.loc_end l2.Location.loc_end
  (* Cursor inside one location: it has priority *)
  | 0, _ -> t1_first
  | _, 0 -> t2_first
  (* Cursor outside locations: favor before *)
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

let enclosing pos roots =
  Option.bind (best_node pos roots)
    ~f:(fun root -> best_node pos (select_leafs pos root))

let deepest_before pos roots =
  match enclosing pos roots with
  | None -> None
  | Some root ->
    let rec aux loc0 path =
      let env0, node0 = leaf_node path in
      let candidate = fold_node
          (fun env node acc ->
             let loc = node_merlin_loc node in
             if path == root ||
                Location_aux.compare_pos pos loc = 0 ||
                Lexing.compare_pos loc.Location.loc_end loc0.Location.loc_end = 0
             then match acc with
               | Some (_,loc',_) when compare_locations pos loc' loc <= 0 -> acc
               | Some _ | None -> Some (env,loc,node)
             else acc
          )
          env0 node0 None
      in
      match candidate with
      | None -> path
      | Some (env,loc,node) ->
        aux loc (List.More ((env,node),path))
    in
    Some (aux (leaf_loc root) root)

let nearest_before pos roots =
  match enclosing pos roots with
  | None -> None
  | Some root ->
    let rec aux prev = function
      | List.More ((_, node), next) as prev
        when Location_aux.compare_pos pos (node_merlin_loc node) = 0
        -> aux prev next
      | _ -> prev
    in
    Some (aux root root)

let of_structure str =
  List.One (str.str_final_env, Browse_node.Structure str)

let of_signature sg =
  List.One (sg.sig_final_env, Browse_node.Signature sg)

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
