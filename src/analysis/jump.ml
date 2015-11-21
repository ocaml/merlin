(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2015  Frédéric Bour  <frederic.bour(_)lakaban.net>
                             Thomas Refis  <refis.thomas(_)gmail.com>
                             Simon Castellan  <simon.castellan(_)iuwt.fr>
                             Tomasz Kołodziejski  <tkolodziejski(_)gmail.com>

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

let is_node_fun = function
  | Expression { exp_desc = Texp_function _; _ } -> true
  | _ -> false
;;

let is_node_let = function
  | Value_binding _ -> true
  | _ -> false
;;

let is_node_pattern = function
  | Case _ -> true
  | _ -> false
;;

let fun_pred = fun all ->
  (* For:
     `let f x y z = ...` jump to f
     For
     `let f = fun x -> fun y -> fun z -> ...` jump to f
     For
     `List.map l ~f:(fun x -> ...)` jump to fun

     Every fun is immediately followed by pattern in the typed tree.
     Invariant: head is a fun.
  *)
  let rec normalize_fun = function
    (* fun pat fun something *)
    | node1 :: node2 :: node3 :: tail when is_node_fun node3 ->
      assert (is_node_fun node1);
      assert (is_node_pattern node2);
      normalize_fun (node3 :: tail)
    (* fun let something *)
    | node1 :: node2 :: tail when is_node_let node2 ->
      assert (is_node_fun node1);
      node2
    | node :: tail ->
      assert (is_node_fun node);
      node
    | _ ->
      assert false
  in
  match all with
  | node :: tail when is_node_fun node -> Some (normalize_fun all)
  | _ -> None
;;

let let_pred = function
  | node :: tail when is_node_let node -> Some node
  | _ -> None
;;

let module_pred = function
  | (Module_binding _ as node) :: tail -> Some node
  | _ -> None
;;

let match_pred = function
  | (Expression { exp_desc = Texp_match _ } as node) :: tail -> Some node
  | _ -> None
;;

let rec find_map ~f = function
  | [] -> None
  | head :: tail ->
    match f head with
    | Some v -> Some v
    | None -> find_map tail ~f
;;

exception No_matching_target
exception No_predicate of string

(* Returns first node on the list matching a predicate *)
let rec find_node preds nodes =
  match nodes with
  | [] -> raise No_matching_target
  | _ :: tail ->
    match find_map preds ~f:(fun pred -> pred nodes) with
    | Some node -> node
    | None -> find_node preds tail
;;

(* Skip all nodes that won't advance cursor's position *)
let rec skip_non_moving pos = function
  | (node :: tail) as all ->
    let node_loc = Browse_node.node_real_loc Location.none node in
    let loc_start = node_loc.Location.loc_start in
    if pos.Lexing.pos_lnum = loc_start.Lexing.pos_lnum then
      skip_non_moving pos tail
    else
      all
  | [] -> []
;;

let get typed_tree pos target =
  let roots = Merlin_typer.to_browse typed_tree in
  let enclosings =
    match Merlin_browse.enclosing pos [roots] with
    | None -> []
    | Some l -> List.map ~f:snd (List.Non_empty.to_list l)
  in

  let all_preds = [
    "fun", fun_pred;
    "let", let_pred;
    "module", module_pred;
    "match", match_pred;
  ] in
  let targets = Str.split (Str.regexp "[, ]") target in
  try
    let preds =
      List.map targets ~f:(fun target ->
        match List.find_some all_preds ~f:(fun (name, _) -> name = target) with
        | Some (name, f) -> f
        | None -> raise (No_predicate target)
      )
    in
    if String.length target = 0 then
      `Error "Specify target"
    else begin
      let nodes = skip_non_moving pos enclosings in
      let node = find_node preds nodes in
      let node_loc = Browse_node.node_real_loc Location.none node in
      `Found node_loc.Location.loc_start
    end
  with
  | No_predicate target ->
    `Error ("No predicate for " ^ target)
  | No_matching_target ->
    `Error "No matching target"

