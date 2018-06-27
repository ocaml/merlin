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
open Option.Infix

let print_constructor c =
  let open Types in
  match c.cstr_args with
  | [] ->
    Printtyp.tree_of_type_scheme
      (Raw_compat.dummy_type_scheme c.cstr_res.desc)
  | args ->
    let desc = Tarrow (Ast_helper.no_label,
                       Raw_compat.dummy_type_scheme (Ttuple args),
                       c.cstr_res, Cok)
    in
    Printtyp.tree_of_type_scheme (Raw_compat.dummy_type_scheme desc)

let summary_at pos sum =
  let rec signature_loc =
    let open Types in
    let union_loc_opt a b = match a,b with
      | None, None -> None
      | l, None | None, l -> l
      | Some a, Some b -> Some (Location_aux.union a b)
    in
    let rec mod_loc = function
      | Mty_ident _ -> None
      | Mty_functor (_,m1,m2) ->
        begin match m1 with
          | Some m1 -> union_loc_opt (mod_loc m1) (mod_loc m2)
          | None -> mod_loc m2
        end
      | Mty_signature s ->
        let rec find_first = function
          | x :: xs -> (match signature_loc x with
              | (Some _ as v) -> v
              | None -> find_first xs)
          | [] -> None
        in
        let a = find_first s and b = find_first (List.rev s) in
        union_loc_opt a b
      | _ -> None
    in function
      | Sig_value (_,v)    -> Some v.val_loc
      | Sig_type (_,t,_)   -> Some t.type_loc
      | Sig_typext (_,e,_) -> Some e.ext_loc
      | Sig_module (_,m,_) -> mod_loc m.Types.md_type
      | Sig_modtype (_,m) ->
        begin match m.Types.mtd_type with
          | Some m -> mod_loc m
          | None -> None
        end
      | Sig_class (_,_,_)
      | Sig_class_type (_,_,_) -> None
  in
  let cmp = Location_aux.compare_pos pos in
  let rec aux sum =
    match Raw_compat.signature_of_summary sum >>= signature_loc with
    | None -> Raw_compat.summary_prev sum >>= aux
    | Some loc ->
      match cmp loc with
      | x when x < 0 -> None
      | 0 -> Some sum
      | _ -> Raw_compat.summary_prev sum >>= aux
  in
  aux sum

let signature_of_env ?(ignore_extensions=true) env =
  let sg = ref [] in
  let append item = sg := item :: !sg in
  let rec aux = function
    | Env.Env_module (_,i,_)
      when ignore_extensions && i = Extension.ident -> ()
    | summary ->
      let open Raw_compat in
      Option.iter ~f:append (signature_of_summary summary);
      Option.iter ~f:aux (summary_prev summary)
  in
  aux (Env.summary env);
  Typemod.simplify_signature (!sg)

let dump_browse node =
  let attr ({Location . txt; loc},payload) =
    `Assoc [
      "start"    , Lexing.json_of_position loc.Location.loc_start;
      "end"      , Lexing.json_of_position loc.Location.loc_end;
      "name"     , `String (txt ^ if payload = Parsetree.PStr [] then "" else " _")
    ]
  in
  let rec append env node acc =
    let loc = Mbrowse.node_loc node in
    `Assoc [
      "filename" , `String loc.Location.loc_start.Lexing.pos_fname;
      "start"    , Lexing.json_of_position loc.Location.loc_start;
      "end"      , Lexing.json_of_position loc.Location.loc_end;
      "ghost"    , `Bool loc.Location.loc_ghost;
      "attrs"    , `List (List.map ~f:attr (Browse_raw.node_attributes node));
      "kind"     , `String (Browse_raw.string_of_node node);
      "children" , dump_list env node
    ] :: acc
  and dump_list env node =
    `List (List.sort ~cmp:compare @@
           Mbrowse.fold_node append env node [])
  in
  `List (append Env.empty node [])

let annotate_tail_calls (ts : Mbrowse.t) :
  (Env.t * Browse_raw.node * Query_protocol.is_tail_position) list =
  let is_one_of candidates node = List.mem node ~set:candidates in
  let find_entry_points candidates (env, node) =
    Tail_analysis.entry_points node,
    (env, node, is_one_of candidates node) in
  let _, entry_points = List.fold_n_map ts ~f:find_entry_points ~init:[] in
  let propagate candidates (env, node, entry) =
    let is_in_tail = entry || is_one_of candidates node in
    (if is_in_tail
     then Tail_analysis.tail_positions node
     else []),
    (env, node, is_in_tail) in
  let _, tail_positions = List.fold_n_map entry_points ~f:propagate ~init:[] in
  List.map ~f:(fun (env, node, tail) ->
      env, node,
      if not tail then
        `No
      else if Tail_analysis.is_call node then
        `Tail_call
      else
        `Tail_position)
    tail_positions
