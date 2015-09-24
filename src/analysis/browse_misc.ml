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
open Option.Infix
open BrowseT

let print_constructor ppf c =
  let open Types in
  match c.cstr_args with
  | [] ->
    Printtyp.type_scheme ppf ({ level = 0 ; id = 0 ; desc = c.cstr_res.desc })
  | args ->
    let desc = Tarrow (Raw_compat.Parsetree.arg_label_of_str "",
                       { level = 0; id = 0; desc = Ttuple args}, c.cstr_res,Cok)
    in
    Printtyp.type_scheme ppf ({ level = 0 ; id = 0 ; desc  })

let summary_at pos sum =
  let cmp = Parsing_aux.compare_pos pos in
  let rec aux sum =
    let open Raw_compat in
    match signature_of_summary sum >>= signature_loc with
    | None -> summary_prev sum >>= aux
    | Some loc ->
      match cmp loc with
      | x when x < 0 -> None
      | 0 -> Some sum
      | _ -> summary_prev sum >>= aux
  in
  aux sum

let signature_of_env ?(ignore_extensions=true) env =
  let open Types in
  let sg = ref [] in
  let append item = sg := item :: !sg in
  let rec aux = function
    | Env.Env_module (_,i,_)
      when ignore_extensions && i = Extension.ident -> ()
    | summary ->
      let open Raw_compat in
      Option.iter ~f:append (signature_of_summary summary);
      Option.iter aux (summary_prev summary)
  in
  aux (Env.summary env);
  Typemod.simplify_signature (!sg)

let dump_ts ts =
  let rec append env loc node acc =
    `Assoc [
      "start", Lexing.json_of_position loc.Location.loc_start;
      "end",   Lexing.json_of_position loc.Location.loc_end;
      "ghost", `Bool loc.Location.loc_ghost;
      "kind", `String (Browse_node.string_of_node node);
      "children", dump_list env loc node
    ] :: acc
  and dump_list env loc node =
    `List (List.sort ~cmp:compare @@
           Browse_node.fold_node append env loc node [])
  in
  `List (List.fold_left ts ~init:[] ~f:(fun acc node ->
      append Env.empty (Browse.node_loc node) node acc))

let annotate_tail_calls (ts : Browse_node.t list) : (Browse_node.t * Protocol.is_tail_position) list =
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
  let ts = List.map ~f:snd @@ List.Non_empty.to_list ts in
  List.rev (annotate_tail_calls (List.rev ts))
