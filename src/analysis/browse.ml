(* {{{ COPYING *(
  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013  Frédéric Bour  <frederic.bour(_)lakaban.net>
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

let local_near pos nodes =
  let cmp = Parsing_aux.compare_pos pos in
  let best_of ({ t_loc = l1 } as t1) ({ t_loc = l2 } as t2) =
    match cmp l1, cmp l2 with
    | 0, 0 ->
      (* Cursor is inside locations: select larger one... not sure why :-) *)
      if Location.(Lexing.compare_pos l1.loc_end l2.loc_end) < 0
      then t2
      else t1
      (* Cursor inside one location, prefer it *)
    | 0, _ -> t1
    | _, 0 -> t2
    | _, _ ->
      (* Cursor outside locations, select the rightmost one *)
      if Location.(Lexing.compare_pos l1.loc_end l2.loc_end) < 0
      then t2
      else t1
  in
  List.fold_left nodes ~init:None ~f:(fun best t ->
    match cmp t.t_loc, best with
    | n, _ when n < 0 -> best
    | _, None -> Some t
    | _, Some t' -> Some (best_of t t')
  )

let is_enclosing pos { t_loc } =
  (Parsing_aux.compare_pos pos t_loc = 0)

let traverse_branch pos tree =
  let rec traverse { t_children = lazy children } acc =
    match local_near pos children with
    | Some t' -> traverse t' (t' :: acc)
    | None -> acc
  in
  traverse tree [tree]

let deepest_before pos envs =
  Option.map (local_near pos envs) ~f:(fun t -> List.hd (traverse_branch pos t))

let nearest_before pos envs =
  Option.bind (local_near pos envs) ~f:(fun t ->
    let branch = traverse_branch pos t in
    let rec aux = function
      | a :: b :: _tail when is_enclosing pos b -> Some a
      (* No node matched: fallback to deepest before behavior *)
      | [_] -> Some (List.hd branch)
      | [] -> None
      | _ :: tail -> aux tail
    in
    aux branch
  )

let enclosing pos envs =
  let not_enclosing l = not (is_enclosing pos l) in
  match local_near pos envs with
  | None -> []
  | Some t ->
    let results = traverse_branch pos t in
    List.drop_while ~f:not_enclosing results

let all_occurences id =
  let rec aux acc t =
    let acc =
      let paths =
        match t.t_node with
        | Pattern p -> pattern_paths p
        | Expression e -> expression_paths e
        | _ -> []
      in
      if List.exists paths ~f:(fun path -> Ident.same id (Path.head path)) then
        t :: acc
      else
        acc
    in
    List.fold_left ~f:aux ~init:acc (Lazy.force t.t_children)
  in
  aux []

let of_structures strs =
  let of_structure str =
    Lazy.force (BrowseT.of_node (BrowseT.Structure str)).t_children
  in
  List.concat_map ~f:of_structure strs
