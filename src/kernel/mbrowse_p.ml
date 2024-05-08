(* {{{ COPYING *(
  Same as Mbrowse module but for the Parstree.
  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 -  Pizie Dust  <playersrebirth(_)gmail.com>

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
open Parsetree
open Browse_raw_p

type node = Browse_raw_p.node
type t = node list

let print_node () node =
  Browse_raw_p.string_of_node node

let print () t =
  List.print (fun () node -> print_node () node) () t

let fold_node f t acc =
  let acc =
    match
      Msupport.get_saved_types_from_attributes (Browse_raw_p.node_attributes t)
    with
    | [] -> acc
    | parts ->
      let rec aux acc = function
        | [] -> acc
        | parts ->
          aux (f t acc) parts
      in
      aux acc parts
  in
  Browse_raw_p.fold_node f t acc

let approximate_loc get_loc node =
  let loc = get_loc Location.none node in
  if loc == Location.none then
    let rec aux node acc =
      let loc = get_loc Location.none node in
      if loc != Location.none then
        Location_aux.union loc acc
      else fold_node aux node acc
    in
    aux node Location.none
  else
    loc

let node_loc node = approximate_loc Browse_raw_p.node_real_loc node

(* Fuzzy locations, more likely to locate the appropriate node *)
let node_merlin_loc node = approximate_loc Browse_raw_p.node_merlin_loc node

let leaf_node lst:node = List.hd lst
let leaf_loc t = node_loc ((leaf_node t))

let drop_leaf t =
  match t with
  | [] | [ _ ] -> None
  | _leaf :: parents -> Some parents

let is_hidden node =
  Browse_raw_p.has_attr ~name:"merlin.hide" node

let is_focus node =
  Browse_raw_p.has_attr ~name:"merlin.focus" node

let select_leafs pos root =
  let branches = ref [] in
  let rec select_child branch node has_selected =
    let loc = node_merlin_loc node in
    if Location_aux.compare_pos pos loc = 0 &&
       not (is_hidden node)
    then
      (traverse (node :: branch); true)
    else
      has_selected
  and traverse branch =
    let node = leaf_node branch in
    if (is_focus node) then (
      branches := [];
      let has_leaves = fold_node (select_child branch) node false in
      if not has_leaves then
        branches := [branch];
      raise Exit
    )
    else if not (is_hidden node) then (
      let has_leaves = fold_node (select_child branch) node false in
      if not has_leaves then
        branches := branch :: !branches
    )
  in
  (try traverse root with Exit -> ());
  !branches

let compare_locations pos l1 l2 =
  let t2_first = +1 in
  let t1_first = -1 in
  match
    Location_aux.compare_pos pos l1,
    Location_aux.compare_pos pos l2
  with
  | 0, 0 ->
    (* Cursor inside both locations: favor non-ghost closer to the end *)
    begin match l1.Location.loc_ghost, l2.Location.loc_ghost with
    | true, false -> 1
    | false, true -> -1
    | _ ->
        Lexing.compare_pos l1.Location.loc_end l2.Location.loc_end
    end
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
  | [] -> []
  | init :: xs ->
    let f acc x =
      if compare_locations pos (leaf_loc acc) (leaf_loc x) <= 0
      then acc
      else x
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
      let node0 = leaf_node path in
      let loc0 = node_merlin_loc node0 in
      let select_candidate node acc =
        let loc = node_merlin_loc node in
        if path == root ||
           Location_aux.compare_pos pos loc = 0 ||
           Lexing.compare_pos loc.Location.loc_end loc0.Location.loc_end = 0
        then match acc with
          | Some (loc',_) when compare_locations pos loc' loc <= 0 -> acc
          | Some _ | None -> Some (loc,node)
        else acc
      in
      match fold_node select_candidate node0 None with
      | None -> path
      | Some (_,node) ->
        aux (node :: path)
    in
    (aux root)

(* Select open nodes *)

let rec select_open_node =
  function[@warning "-9"]
  | (Structure_item {pstr_desc =
                             Pstr_open { popen_expr =
                                           { pmod_desc = Pmod_ident ({txt = longident}) }}})
    :: ancestors ->
    Some (longident, ancestors)
  | (Signature_item {psig_desc = Psig_open op}) :: ancestors ->
    let ({ Asttypes.txt = longident; }) = op.popen_expr in
    Some (longident, ancestors)
  | (Expression { pexp_desc =
          Pexp_open ({ popen_expr =
                        { pmod_desc = Pmod_ident ({txt = longident})}}, _); _})
    :: _ as ancestors ->
      Some (longident, ancestors)
  | [] -> None
  | _ :: ancestors -> select_open_node ancestors

let of_structure str = [Browse_raw_p.Structure str]

let of_structure_items lst =
  List.map ~f:(fun head -> Browse_raw_p.Structure_item head) lst
    
let of_signature_items lst =
  List.map ~f:(fun head -> Browse_raw_p.Signature_item head) lst
  
let of_signature sg = [Browse_raw_p.Signature sg]

let of_parsetree = function
  | `Implementation str -> of_structure str
  | `Interface sg -> of_signature sg

let to_parsetree node =
  let n = (List.hd (List.rev node)) in
  match n with 
  | Browse_raw_p.Structure str ->
    `Implementation str
  | Browse_raw_p.Signature sg -> 
    `Interface sg
  | _ -> raise (Invalid_argument "Invalid input")
  
let optional_label_sugar = function
  | Parsetree.Pexp_construct (id, e)
    when id.Location.loc.Location.loc_ghost
      && id.Location.txt = Longident.Lident "Some" ->
    e
  | _ -> None

let rec is_recovered_expression e =
  match e.Parsetree.pexp_desc with
  | (* Recovery on arbitrary expressions *)
    Pexp_tuple [_] ->
    true
  | (* Recovery on desugared optional label application *)
    Pexp_construct _ as cstr
    when is_recovered_Pexp_construct cstr ->
    true
  | _ -> false

and is_recovered_Pexp_construct cstr =
  match optional_label_sugar cstr with
  | Some e -> is_recovered_expression e
  | _ -> false

let is_recovered = function
  | Expression e -> is_recovered_expression e
  | _ -> false

let check_node pos node =
  let loc = node_merlin_loc node in
  if Location_aux.compare_pos pos loc = 0 && loc.loc_ghost then true else false 

let get_children pos root = 
  let children =
    List.map ~f:(fun x -> 
      match x with
      | Structure str ->
          of_structure_items (List.filter ~f:(fun x ->
              check_node pos (Structure_item(x))
          ) str)
      | Signature str ->
        of_signature_items (List.filter ~f:(fun x ->
          check_node pos (Signature_item(x))
        ) str)
      | _ -> []) root 
  in children |> List.concat |> List.hd


let pprint_deriver_node () node = 
  let ppf, to_string = Format.to_string () in
    begin match node with
      | Browse_raw_p.Structure_item n -> Pprintast.structure ppf [n]
      | Browse_raw_p.Signature_item n -> Pprintast.signature ppf [n]
      | _ -> raise (Invalid_argument "Wrong nodes")
    end;
  Format.pp_print_newline ppf ();
  to_string ()

let pprint_deriver_nodes = 
  List.print pprint_deriver_node 
  