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

type trie =
  | T of string * Longident.t * t Lazy.t
and t = trie list

let type_of typ =
      let open Merlin_sherlodoc in
  let rec aux typ =
    match Types.get_desc typ with
    | Types.Tvar None -> Type_parsed.Wildcard
    | Types.Tvar (Some ty) -> Type_parsed.Tyvar ty
    | Types.Ttuple elts -> Type_parsed.tuple @@ List.map ~f:aux elts
    | Types.Tarrow (_, a, b, _) -> Type_parsed.Arrow (aux a, aux b)
    | Types.Tconstr (p, args, _) ->
      let name = Format.asprintf "%a" Printtyp.path p in
      Type_parsed.Tycon (name, List.map ~f:aux args)
    | _ -> Type_parsed.Unhandled
    in typ |> aux |> Type_expr.normalize_type_parameters

let make_trie env modules =
  let rec walk env lident =
    Env.fold_modules (fun name _ mdl acc ->
        match mdl.Types.md_type with
        | Types.Mty_alias _ -> acc
        | _ ->
          let lident = Longident.Ldot (lident, name) in
          T (name, lident, lazy (walk env lident)) :: acc
      ) (Some lident) env []
  in
  List.fold_left
    ~init:[]
    ~f:(fun acc name ->
        let lident = Longident.Lident name in
        match Env.find_module_by_name lident env with
        | exception _ -> acc
        | _ -> T (name, lident, lazy (walk env lident)) :: acc
      )
    modules

let doc_to_option = function
  | `Builtin doc
  | `Found doc -> Some doc
  | _ -> None

let run ?(limit = 100) config local_defs comments pos env query trie =
  let fold_values dir acc =
    Env.fold_values (fun _ path desc acc ->
        let open Merlin_sherlodoc in
        let typ = type_of desc.Types.val_type in
        let path = Format.asprintf "%a" Printtyp.path path in
        let cost = Query_parser.distance_for query ~path typ in
        if cost >= 1000 then acc
        else
          let doc =
            Locate.get_doc
              ~config
              ~env
              ~local_defs
              ~comments
              ~pos
              (`User_input path)
            |> doc_to_option
          in
          (cost, path, desc, doc) :: acc
      ) dir env acc
  in
  let rec walk acc (T (_, dir, children)) =
    let force () =
      let _ = Env.find_module_by_name dir env in
      Lazy.force children
    in
    match force () with
    | computed_children ->
      let init = fold_values (Some dir) acc in
      List.fold_left ~init ~f:walk computed_children
    | exception _ -> acc
  in
  let init = fold_values None [] in
  trie
  |> List.fold_left ~init ~f:walk
  |> List.sort ~cmp:(fun (cost_a, a, _, doc_a) (cost_b, b, _, doc_b) ->
      let c = Int.compare cost_a cost_b in
      if Int.equal c 0 then
        let c = Int.compare (String.length a) (String.length b) in
        match c, doc_a, doc_b with
        | 0, Some _, None -> 1
        | 0, None, Some _ -> -1
        | 0, Some a, Some b ->
          Int.compare (String.length a) (String.length b)
        | _ -> c
      else c
    )
  |> List.take_n limit
    
  
