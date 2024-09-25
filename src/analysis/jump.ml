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
open Browse_raw

type direction = Prev | Next

let is_node_fun = function
  | Expression { exp_desc = Texp_function _; _ } -> true
  | _ -> false

let is_node_let = function
  | Value_binding _ -> true
  | _ -> false

let is_node_pattern = function
  | Case _ -> true
  | _ -> false

let fun_pred all =
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
    | node1 :: node2 :: _ when is_node_let node2 ->
      assert (is_node_fun node1);
      node2
    | node :: _ ->
      assert (is_node_fun node);
      node
    | _ -> assert false
  in
  match all with
  | node :: _ when is_node_fun node -> Some (normalize_fun all)
  | _ -> None

let let_pred = function
  | node :: _ when is_node_let node -> Some node
  | _ -> None

let module_pred = function
  | (Module_binding _ as node) :: _ -> Some node
  | _ -> None

let module_type_pred = function
  | (Module_type_declaration _ as node) :: _ -> Some node
  | _ -> None

let match_pred = function
  | (Expression { exp_desc = Texp_match _; _ } as node) :: _ -> Some node
  | _ -> None

let rec find_map ~f = function
  | [] -> None
  | head :: tail -> (
    match f head with
    | Some v -> Some v
    | None -> find_map tail ~f)

exception No_matching_target
exception No_predicate of string
exception No_next_match_case
exception No_prev_match_case

(* Returns first node on the list matching a predicate *)
let rec find_node preds nodes =
  match nodes with
  | [] -> raise No_matching_target
  | _ :: tail -> (
    match find_map preds ~f:(fun pred -> pred nodes) with
    | Some node -> node
    | None -> find_node preds tail)

(* Skip all nodes that won't advance cursor's position *)
let rec skip_non_moving pos = function
  | node :: tail as all ->
    let node_loc = Browse_raw.node_real_loc Location.none node in
    let loc_start = node_loc.Location.loc_start in
    if pos.Lexing.pos_lnum = loc_start.Lexing.pos_lnum then
      skip_non_moving pos tail
    else all
  | [] -> []

let get_cases_from_match node =
  match node with
  | Expression { exp_desc = Texp_match (_, cases, _); _ } -> cases
  | _ -> []

let find_case_pos cases pos direction =
  let rec find_pos pos cases direction =
    match cases with
    | [] -> None
    | { c_lhs = { pat_loc; _ }; _ } :: tail ->
      let check =
        match direction with
        | Prev -> pos.Lexing.pos_cnum > pat_loc.loc_start.pos_cnum
        | Next -> pos.Lexing.pos_cnum < pat_loc.loc_start.pos_cnum
      in
      if check then Some pat_loc.loc_start else find_pos pos tail direction
  in
  let case = find_pos pos cases direction in
  match case with
  | Some location -> `Found location
  | None -> (
    match direction with
    | Next -> raise No_next_match_case
    | Prev -> raise No_prev_match_case)

let get typed_tree pos target =
  let roots = Mbrowse.of_typedtree typed_tree in
  let enclosings =
    match Mbrowse.enclosing pos [ roots ] with
    | [] -> []
    | l -> List.map ~f:snd l
  in
  let all_preds =
    [ ("fun", fun_pred);
      ("let", let_pred);
      ("module", module_pred);
      ("module-type", module_type_pred);
      ("match", match_pred);
      ("match-next-case", match_pred);
      ("match-prev-case", match_pred)
    ]
  in
  let targets = Str.split (Str.regexp "[, ]") target in
  try
    let preds =
      List.map targets ~f:(fun target ->
          match
            List.find_some all_preds ~f:(fun (name, _) -> name = target)
          with
          | Some (_, f) -> f
          | None -> raise (No_predicate target))
    in
    if String.length target = 0 then `Error "Specify target"
    else
      let nodes = skip_non_moving pos enclosings in
      let node = find_node preds nodes in
      match target with
      | "match-next-case" -> find_case_pos (get_cases_from_match node) pos Next
      | "match-prev-case" ->
        find_case_pos (List.rev (get_cases_from_match node)) pos Prev
      | _ ->
        let node_loc = Browse_raw.node_real_loc Location.none node in
        `Found node_loc.Location.loc_start
  with
  | No_predicate target -> `Error ("No predicate for " ^ target)
  | No_matching_target -> `Error "No matching target"
  | No_next_match_case -> `Error "No next case found"
  | No_prev_match_case -> `Error "No previous case found"

let phrase typed_tree pos target =
  let roots = Mbrowse.of_typedtree typed_tree in
  (* Select nodes around cursor.
     If the cursor is around a module expression, also search inside it. *)
  let enclosing =
    match Mbrowse.enclosing pos [ roots ] with
    | (env, (Browse_raw.Module_expr _ as node)) :: enclosing ->
      Browse_raw.fold_node
        (fun env node enclosing -> (env, node) :: enclosing)
        env node enclosing
    | enclosing -> enclosing
  in
  (* Drop environment, they are of no use here *)
  let enclosing = List.map ~f:snd enclosing in
  let find_item x xs =
    match target with
    | `Prev -> List.rev (List.take_while ~f:(( != ) x) xs)
    | `Next -> (
      match List.drop_while ~f:(( != ) x) xs with
      | _ :: xs -> xs
      | [] -> [])
  in
  let find_pos prj xs =
    match target with
    | `Prev ->
      let f x = Location_aux.compare_pos pos (prj x) > 0 in
      List.rev (List.take_while ~f xs)
    | `Next ->
      let f x = Location_aux.compare_pos pos (prj x) >= 0 in
      List.drop_while ~f xs
  in
  let rec seek_item = function
    | [] -> None
    | Browse_raw.Signature xs :: tail -> begin
      match find_pos (fun x -> x.Typedtree.sig_loc) xs.Typedtree.sig_items with
      | [] -> seek_item tail
      | y :: _ -> Some y.Typedtree.sig_loc
    end
    | Browse_raw.Structure xs :: tail -> begin
      match find_pos (fun x -> x.Typedtree.str_loc) xs.Typedtree.str_items with
      | [] -> seek_item tail
      | y :: _ -> Some y.Typedtree.str_loc
    end
    | Browse_raw.Signature_item (x, _) :: Browse_raw.Signature xs :: tail ->
    begin
      match find_item x xs.Typedtree.sig_items with
      | [] -> seek_item tail
      | y :: _ -> Some y.Typedtree.sig_loc
    end
    | Browse_raw.Structure_item (x, _) :: Browse_raw.Structure xs :: tail ->
    begin
      match find_item x xs.Typedtree.str_items with
      | [] -> seek_item tail
      | y :: _ -> Some y.Typedtree.str_loc
    end
    | _ :: xs -> seek_item xs
  in
  match (seek_item enclosing, target) with
  | Some loc, _ -> `Logical (Lexing.split_pos loc.Location.loc_start)
  | None, `Prev -> `Start
  | None, `Next -> `End
