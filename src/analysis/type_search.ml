(* {{{ COPYING *(

   This file is part of Merlin, an helper for ocaml editors

   Copyright (C) 2013 - 2024  Frédéric Bour  <frederic.bour(_)lakaban.net>
                              Thomas Refis  <refis.thomas(_)gmail.com>
                              Simon Castellan  <simon.castellan(_)iuwt.fr>
                              Arthur Wendling <arthur(_)tarides.com>
                              Xavier Van de Woestyne <xaviervdw(_)gmail.com>

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

let sherlodoc_type_of env typ =
  let open Merlin_sherlodoc in
  let rec aux typ =
    match Types.get_desc typ with
    | Types.Tvar None -> Type_parsed.Wildcard
    | Types.Tvar (Some ty) -> Type_parsed.Tyvar ty
    | Types.Ttuple elts -> Type_parsed.tuple @@ List.map ~f:aux elts
    | Types.Tarrow (_, a, b, _) -> Type_parsed.Arrow (aux a, aux b)
    | Types.Tconstr (p, args, _) ->
      let p = Printtyp.rewrite_double_underscore_paths env p in
      let name = Format.asprintf "%a" Printtyp.path p in
      Type_parsed.Tycon (name, List.map ~f:aux args)
    | _ -> Type_parsed.Unhandled
  in
  typ |> aux |> Type_expr.normalize_type_parameters

let make_constructible path desc =
  let holes =
    match Types.get_desc desc with
    | Types.Tarrow (l, _, b, _) ->
      let rec aux acc t =
        match Types.get_desc t with
        | Types.Tarrow (l, _, b, _) -> aux (acc ^ with_label l) b
        | _ -> acc
      and with_label l =
        match l with
        | Ocaml_parsing.Asttypes.Nolabel -> " _"
        | Labelled s -> " ~" ^ s ^ ":_"
        | Optional _ -> ""
      in
      aux (with_label l) b
    | _ -> ""
  in
  path ^ holes

let doc_to_option = function
  | `Builtin doc | `Found doc -> Some doc
  | _ -> None

let get_doc ~config ~env ~local_defs ~comments ~pos name =
  Locate.get_doc ~config ~env ~local_defs ~comments ~pos (`User_input name)
  |> doc_to_option

let compare_result Query_protocol.{ cost = cost_a; name = a; doc = doc_a; _ }
    Query_protocol.{ cost = cost_b; name = b; doc = doc_b; _ } =
  let c = Int.compare cost_a cost_b in
  if Int.equal c 0 then
    let c = Int.compare (String.length a) (String.length b) in
    match (c, doc_a, doc_b) with
    | 0, Some _, None -> 1
    | 0, None, Some _ -> -1
    | 0, Some doc_a, Some doc_b ->
      let c = Int.compare (String.length doc_a) (String.length doc_b) in
      (* Make default insertion determinist *)
      if Int.equal 0 c then String.compare a b else c
    | 0, None, None -> String.compare a b
    | _ -> c
  else c

let compute_value query env _ path desc acc =
  let open Merlin_sherlodoc in
  let d = desc.Types.val_type in
  let typ = sherlodoc_type_of env d in
  let name =
    Printtyp.wrap_printing_env env @@ fun () ->
    let path = Printtyp.rewrite_double_underscore_paths env path in
    Format.asprintf "%a" Printtyp.path path
  in
  let cost = Query.distance_for query ~path:name typ in
  if cost >= 1000 then acc
  else
    let doc = None in
    let loc = desc.Types.val_loc in
    let typ = desc.Types.val_type in
    let constructible = make_constructible name d in
    Query_protocol.{ cost; name; typ; loc; doc; constructible } :: acc

let compute_values query env lident acc =
  Env.fold_values (compute_value query env) lident env acc

let values_from_module query env lident acc =
  let rec aux acc lident =
    match Env.find_module_by_name lident env with
    | exception _ -> acc
    | _ ->
      let acc = compute_values query env (Some lident) acc in
      Env.fold_modules
        (fun name _ mdl acc ->
          match mdl.Types.md_type with
          | Types.Mty_alias _ -> acc
          | _ ->
            let lident = Longident.Ldot (lident, name) in
            aux acc lident)
        (Some lident) env acc
  in
  aux acc lident

let run ?(limit = 100) ~env ~query ~modules () =
  let init = compute_values query env None [] in
  modules
  |> List.fold_left ~init ~f:(fun acc name ->
         let lident = Longident.Lident name in
         values_from_module query env lident acc)
  |> List.sort ~cmp:compare_result
  |> List.take_n limit

let classify_query query =
  let query = String.trim query in
  match query.[0] with
  | '+' | '-' -> `Polarity query
  | _ -> `By_type query
  | exception Invalid_argument _ -> `Polarity query
