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

let local_near pos nodes =
  let cmp = Parsing_aux.compare_pos pos in
  let best_of ({ t_loc = l1 } as t1) ({ t_loc = l2 } as t2) =
    match cmp l1, cmp l2 with
    | 0, 0 ->
      (* Cursor is inside locations: non ghost and select largest one... not
         sure why :-) *)
      let g1 = l1.Location.loc_ghost and g2 = l2.Location.loc_ghost in
      if (g1 = g2 && Location.(Lexing.compare_pos l1.loc_end l2.loc_end) < 0)
         || not g2
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
  match local_near pos envs with
  | None -> []
  | Some t -> traverse_branch pos t

let nearest_before pos envs =
  match local_near pos envs with
  | None -> []
  | Some t ->
    let branch = traverse_branch pos t in
    let rec aux l = match l with
      | a :: b :: _ when is_enclosing pos b -> l
      (* No node matched: fallback to deepest before behavior *)
      | [_] -> branch
      | [] -> []
      | _ :: tail -> aux tail
    in
    aux branch

let enclosing pos envs =
  let not_enclosing l = not (is_enclosing pos l) in
  match local_near pos envs with
  | None -> []
  | Some t ->
    let results = traverse_branch pos t in
    List.drop_while ~f:not_enclosing results

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

let rec fix_loc env t =
  let t_children = t.t_children in
  let t_env =
    if t.t_env == BrowseT.default_env
    then env
    else t.t_env
  in
  if t.t_loc == BrowseT.default_loc then
    let t_children = List.map (fix_loc t_env) (Lazy.force t_children) in
    {t with
     t_env;
     t_loc = List.fold_left ~init:BrowseT.default_loc t_children
         ~f:(fun l t ->
             if t.t_loc == BrowseT.default_loc then
               l
             else if l == BrowseT.default_loc then
               t.t_loc
             else
               Parsing_aux.location_union t.t_loc l);
     t_children = lazy t_children}
  else
    {t with t_env; t_children = lazy (List.map (fix_loc t_env) (Lazy.force t_children))}

let of_structure str =
  fix_loc str.str_final_env (BrowseT.of_node (Structure str))

let of_signature sg =
  fix_loc sg.sig_final_env (BrowseT.of_node (Signature sg))

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

let annotate_tail_calls ts : (BrowseT.t * Protocol.is_tail_position) list =
  let open BrowseT in
  let is_one_of candidates t = List.mem t.t_node ~set:candidates in
  let find_entry_points candidates t =
    Tail_analysis.entry_points t.t_node,
    (t, is_one_of candidates t) in
  let _, entry_points = List.fold_n_map ts ~f:find_entry_points ~init:[] in
  let propagate candidates (t,entry) =
    let is_in_tail = entry || is_one_of candidates t in
    (if is_in_tail
     then Tail_analysis.tail_positions t.t_node
     else []),
    (t, is_in_tail) in
  let _, tail_positions = List.fold_n_map entry_points ~f:propagate ~init:[] in
  List.map ~f:(fun (t,tail) ->
      t,
      if not tail then
        `No
      else if Tail_analysis.is_call t.t_node then
        `Tail_call
      else
        `Tail_position)
    tail_positions

let annotate_tail_calls_from_leaf ts =
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
  | {t_node = Expression e } -> is_recovered_expression e
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
    | node :: (node' :: _ as ancestors)
      when skip_recovered && is_recovered node' -> select ancestors
    | node :: ancestors -> node, ancestors
    | [] -> {BrowseT.dummy with BrowseT.t_env = Typer.env typer}, []
  in
  select (deepest_before pos_cursor structures)
