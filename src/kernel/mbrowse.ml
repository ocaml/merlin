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
open Browse_raw

type node = Browse_raw.node
type t = (Env.t * node) list

let node_of_binary_part = Browse_raw.node_of_binary_part

let fold_node f env t acc =
  let acc =
    match
      Msupport.get_saved_types_from_attributes (Browse_raw.node_attributes t)
    with
    | [] -> acc
    | parts ->
      let rec aux acc = function
        | [] -> acc
        | part :: parts ->
          let t = node_of_binary_part env part in
          aux (f (Browse_raw.node_update_env env t) t acc) parts
      in
      aux acc parts
  in
  Browse_raw.fold_node f env t acc

let approximate_loc get_loc node =
  let loc = get_loc Location.none node in
  if loc == Location.none then
    let rec aux env node acc =
      let loc = get_loc Location.none node in
      if loc != Location.none then Location_aux.union loc acc
      else fold_node aux env node acc
    in
    aux Env.empty node Location.none
  else loc

let node_loc node = approximate_loc Browse_raw.node_real_loc node

(* Fuzzy locations, more likely to locate the appropriate node *)
let node_merlin_loc node = approximate_loc Browse_raw.node_merlin_loc node

let leaf_node = List.hd
let leaf_loc t = node_loc (snd (leaf_node t))

let drop_leaf t =
  match t with
  | [] | [ _ ] -> None
  | _leaf :: parents -> Some parents

let is_hidden node = Browse_raw.has_attr ~name:"merlin.hide" node

let is_focus node = Browse_raw.has_attr ~name:"merlin.focus" node

let select_leafs pos root =
  let branches = ref [] in
  let rec select_child branch env node has_selected =
    let loc = node_merlin_loc node in
    if Location_aux.compare_pos pos loc = 0 && not (is_hidden node) then (
      traverse ((env, node) :: branch);
      true)
    else has_selected
  and traverse branch =
    let env, node = leaf_node branch in
    if is_focus node then (
      branches := [];
      let has_leaves = fold_node (select_child branch) env node false in
      if not has_leaves then branches := [ branch ];
      raise Exit)
    else if not (is_hidden node) then
      let has_leaves = fold_node (select_child branch) env node false in
      if not has_leaves then branches := branch :: !branches
  in
  (try traverse root with Exit -> ());
  !branches

let compare_locations pos l1 l2 =
  let t2_first = 1 in
  let t1_first = -1 in
  match (Location_aux.compare_pos pos l1, Location_aux.compare_pos pos l2) with
  (* Cursor inside both locations: favor non-ghost closer to the end *)
  | 0, 0 -> begin
    match (l1.Location.loc_ghost, l2.Location.loc_ghost) with
    | true, false -> 1
    | false, true -> -1
    | _ -> Lexing.compare_pos l1.Location.loc_end l2.Location.loc_end
  end
  (* Cursor inside one location: it has priority *)
  | 0, _ -> t1_first
  | _, 0 -> t2_first
  (* Cursor outside locations: favor before *)
  | n, m when n > 0 && m < 0 -> t1_first
  | n, m when m > 0 && n < 0 -> t2_first
  (* Cursor is after both, select the closest one *)
  | _, _ -> Lexing.compare_pos l2.Location.loc_end l1.Location.loc_end

let best_node pos = function
  | [] -> []
  | init :: xs ->
    let f acc x =
      if compare_locations pos (leaf_loc acc) (leaf_loc x) <= 0 then acc else x
    in
    List.fold_left ~f ~init xs

let enclosing pos roots =
  match best_node pos roots with
  | [] -> []
  | root -> best_node pos (select_leafs pos root)

let deepest_before pos roots =
  match enclosing pos roots with
  | [] -> []
  | root ->
    let rec aux path =
      let env0, node0 = leaf_node path in
      let loc0 = node_merlin_loc node0 in
      let select_candidate env node acc =
        let loc = node_merlin_loc node in
        if
          path == root
          || Location_aux.compare_pos pos loc = 0
          || Lexing.compare_pos loc.Location.loc_end loc0.Location.loc_end = 0
        then
          match acc with
          | Some (_, loc', _) when compare_locations pos loc' loc <= 0 -> acc
          | Some _ | None -> Some (env, loc, node)
        else acc
      in
      match fold_node select_candidate env0 node0 None with
      | None -> path
      | Some (env, _, node) -> aux ((env, node) :: path)
    in
    aux root

(* Select open nodes *)

let rec select_open_node = function[@warning "-9"]
  | ( _,
      Structure_item
        ( { str_desc =
              Tstr_open
                { open_expr = { mod_desc = Tmod_ident (p, { txt = longident }) }
                }
          },
          _ ) )
    :: ancestors -> Some (p, longident, ancestors)
  | (_, Signature_item ({ sig_desc = Tsig_open op }, _)) :: ancestors ->
    let p, { Asttypes.txt = longident } = op.open_expr in
    Some (p, longident, ancestors)
  | ( _,
      Expression
        { exp_desc =
            Texp_open
              ( { open_expr = { mod_desc = Tmod_ident (p, { txt = longident }) }
                },
                _ );
          _
        } )
    :: _ as ancestors -> Some (p, longident, ancestors)
  | (_, Pattern { pat_extra; _ }) :: ancestors
    when List.exists pat_extra ~f:(function
           | Tpat_open _, _, _ -> true
           | _ -> false) ->
    let p, longident =
      List.find_map pat_extra ~f:(function
        | Tpat_open (p, { txt = longident }, _), _, _ -> Some (p, longident)
        | _ -> None)
    in
    Some (p, longident, ancestors)
  | [] -> None
  | _ :: ancestors -> select_open_node ancestors

let of_structure str =
  let env =
    match str.str_items with
    | [] -> str.str_final_env
    | item :: _ -> item.str_env
  in
  [ (env, Browse_raw.Structure str) ]

let of_signature sg =
  let env =
    match sg.sig_items with
    | [] -> sg.sig_final_env
    | item :: _ -> item.sig_env
  in
  [ (env, Browse_raw.Signature sg) ]

let of_typedtree = function
  | `Implementation str -> of_structure str
  | `Interface sg -> of_signature sg

let optional_label_sugar = function
  | Typedtree.Texp_construct (id, _, [ e ])
    when id.Location.loc.Location.loc_ghost
         && id.Location.txt = Longident.Lident "Some" -> Some e
  | _ -> None

let rec is_recovered_expression e =
  match e.Typedtree.exp_desc with
  (* Recovery on arbitrary expressions *)
  | Texp_tuple [ _ ] -> true
  (* Recovery on unbound identifier *)
  | Texp_ident (Path.Pident id, _, _) when Ident.name id = "*type-error*" ->
    true
  (* Recovery on desugared optional label application *)
  | Texp_construct _ as cstr when is_recovered_Texp_construct cstr -> true
  | _ -> false

and is_recovered_Texp_construct cstr =
  match optional_label_sugar cstr with
  | Some e -> is_recovered_expression e
  | _ -> false

let is_recovered = function
  | Expression e -> is_recovered_expression e
  | _ -> false

let print_node () node = Browse_raw.string_of_node node

let print () t = List.print (fun () (_, node) -> print_node () node) () t
