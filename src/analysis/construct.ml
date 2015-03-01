(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2015  Frédéric Bour  <frederic.bour(_)lakaban.net>
                             Thomas Refis  <refis.thomas(_)gmail.com>
                             Simon Castellan  <simon.castellan(_)iuwt.fr>
                             Arthur Wendling  <art.wendling(_)gmail.com>

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
open BrowseT

let section = Logger.Section.of_string "construct"

exception Not_allowed of string

let () =
  Location.register_error_of_exn (function
    | Not_allowed s  -> Some (Location.error ("Construct not allowed on " ^ s))
    | _ -> None
  )

let rec gen_expr env type_expr =
  let open Types in
  let type_expr = Btype.repr type_expr in
  match type_expr.desc with
  | Tlink _    -> assert false (* impossible after [Btype.repr] *)
  | Tvar _     -> raise (Not_allowed "non-immediate type")
  | Tobject _  -> raise (Not_allowed "object type")
  | Ttuple ts -> raise (Not_allowed "tuple")
  | Tarrow (label, t0, t1, commut) -> raise (Not_allowed "arrow")
  | Tconstr (path, _params, _) -> raise (Not_allowed "constr")
  | Tpackage (path, ids, args) -> raise (Not_allowed "modules")
  | Tvariant row_desc -> raise (Not_allowed "variant type")
  | _ ->
    let fmt, to_string = Format.to_string () in
    Printtyp.type_expr fmt type_expr ;
    raise (Not_allowed (to_string ()))

let node ~loc ~env parents node =
  match node.t_node with
  | Expression expr ->
    let ty = expr.Typedtree.exp_type in
    let result, _ = gen_expr env ty in
    let fmt, to_string = Format.to_string () in
    Pprintast.expression fmt result ;
    let str = to_string () in
    loc, str
  | node ->
    raise (Not_allowed (BrowseT.string_of_node node))
