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
open Option.Infix
open BrowseT

let union_loc_opt a b = match a,b with
  | None, None -> None
  | (Some _ as l), None | None, (Some _ as l) -> l
  | Some a, Some b -> Some (Parsing_aux.location_union a b)

let rec signature_loc =
  let open Types in
  let rec mod_loc = function
    | Mty_ident _ -> None
    | Mty_functor (_,m1,m2) ->
      begin match Merlin_types_custom.extract_functor_arg m1 with
        | Some m1 -> union_loc_opt (mod_loc m1) (mod_loc m2)
        | None -> mod_loc m2
      end
    | Mty_signature (lazy s) ->
        let rec find_first = function
          | x :: xs -> (match signature_loc x with
                        | (Some _ as v) -> v
                        | None -> find_first xs)
          | [] -> None
        in
        let a = find_first s and b = find_first (List.rev s) in
        union_loc_opt a b
    | _ -> None
  in
  function
  | Sig_value (_,v)     -> Some v.val_loc
  | Sig_type (_,t,_)      -> Some t.type_loc
  | Sig_typext (_,e,_) -> Some e.ext_loc
  | Sig_module (_,m,_)    -> mod_loc (Merlin_types_custom.extract_module_declaration m)
  | Sig_modtype (_,m) ->
    begin match Merlin_types_custom.extract_modtype_declaration m with
      | Some m -> mod_loc m
      | None -> None
    end
  | Sig_class (_,_,_)
  | Sig_class_type (_,_,_) -> None

let signature_ident =
  let open Types in function
  | Sig_value (i,_)
  | Sig_type (i,_,_)
  | Sig_typext (i,_,_)
  | Sig_modtype (i,_)
  | Sig_module (i,_,_)
  | Sig_class (i,_,_)
  | Sig_class_type (i,_,_) -> i

let print_constructor ppf c =
  let open Types in
  match c.cstr_args with
  | [] ->
    Printtyp.type_expr ppf ({ level = 0 ; id = 0 ; desc = c.cstr_res.desc })
  | args ->
    let desc = Tarrow ("",{ level = 0; id = 0; desc = Ttuple args}, c.cstr_res,Cok) in
    Printtyp.type_expr ppf ({ level = 0 ; id = 0 ; desc  })

let summary_prev =
  Merlin_types_custom.summary_prev

let signature_of_summary =
  Merlin_types_custom.signature_of_summary

let summary_at pos sum =
  let cmp = Parsing_aux.compare_pos pos in
  let rec aux sum =
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
      Option.iter ~f:append (signature_of_summary summary);
      Option.iter aux (summary_prev summary)
  in
  aux (Env.summary env);
  Typemod.simplify_signature (!sg)

let rec dump_ts ts =
  let dump_t { t_loc ; t_node ; t_children = lazy children } =
    IO.with_location t_loc
    [
      "kind", `String (string_of_node t_node);
      "children", dump_ts children
    ]
  in
  let cmp_start { t_loc = l1 } { t_loc = l2 } =
    Lexing.compare_pos l1.Location.loc_start l2.Location.loc_end
  in
  let ts = List.sort cmp_start ts in
  `List (List.map dump_t ts)
